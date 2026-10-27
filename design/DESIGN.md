# Design: update as a monad, commands that close over their own bookkeeping

## Problem

The library had the right instincts (inert commands, a pure `Html` functor,
accessible locators) buried under accidental surface. Four root causes drove
most of the reviewer's ten problems:

1. `Definition.lift :: result -> message` could not see the instance's
   arguments, so library-owned state (a cache generation) had to travel in the
   result. Tests had to know it: `Query.CompletedFetch "charmander" 1 (Right x)`.
2. `Update` was a record, so composition needed a hand-written
   `{read, write, toParentMessage}` lens per child, and the out-message needed
   its own field on every update, `Void` at the top.
3. Two test monads (Story, Scene) over one shared state, each re-exporting the
   same fifteen wrappers, plus command expectations that duplicate what the
   pending-command rule already enforces.
4. `Html` carried `Fragment` and `Empty`, and the runtime paid for them with a
   full re-render of any sibling list containing one.

Constraints kept: commands are inert and resolved by value in tests; a new
message cannot arrive while a command is pending; unresolved commands fail the
test; `Html` is a pure functor with typed events; locators are accessible;
server rendering is a pure fold; the Pokédex works in a browser (verified
below with the Playwright smoke test against live PokéAPI).

## Usage (caller's view)

### Quickstart: a counter

```purescript
module Counter where

import Prelude

import Control.Monad.State.Class (modify_)
import Foldkit (Program, Update)
import Foldkit.Html (Html, button, div, onClick, p, text)

type Model = { count :: Int }

data Message = Increment | Decrement

update :: Message -> Update Model Message Unit
update = case _ of
  Increment -> modify_ \model -> model { count = model.count + 1 }
  Decrement -> modify_ \model -> model { count = model.count - 1 }

view :: Model -> Html Message
view model = div []
  [ button [ onClick Decrement ] [ text "-" ]
  , p [] [ text (show model.count) ]
  , button [ onClick Increment ] [ text "+" ]
  ]

program :: Program Model Message
program = { model: { count: 0 }, init: pure unit, update, view }

-- browser: Foldkit.Browser.mount "root" program
-- server:  Foldkit.Render.renderToString (view program.model)
```

### The real Pokédex (`examples/src/Example/Pokedex.purs`)

```purescript
type Model =
  { list :: Pagination.Model Int HttpError PokemonSummary
  , search :: String
  , selected :: Maybe String
  , details :: Query.Model String HttpError Pokemon
  }

data Message
  = UpdatedSearch String
  | SelectedPokemon String
  | ReachedListEnd
  | GotPage (Pagination.Message Int HttpError PokemonSummary)
  | GotDetail (Query.Message String HttpError Pokemon)

program :: Program Model Message
program =
  { model: { list: Pagination.init, search: "", selected: Nothing, details: Query.init }
  , init: loadMore
  , update
  , view
  }

update :: Message -> Update Model Message Unit
update = case _ of
  UpdatedSearch search -> modify_ _ { search = search }
  SelectedPokemon name -> do
    modify_ _ { selected = Just name }
    zoom @"details" GotDetail (Query.ensure fetchPokemonDetail name)
  ReachedListEnd -> loadMore
  GotPage message -> zoom @"list" GotPage (Pagination.update message)
  GotDetail message -> zoom @"details" GotDetail (Query.update message)

loadMore :: Update Model Message Unit
loadMore = zoom @"list" GotPage (Pagination.loadMore fetchPokemonPage)

fetchPokemonPage :: Definition (Maybe Int) (Either HttpError (Page Int PokemonSummary))
fetchPokemonPage = define "FetchPokemonPage" \cursor -> do
  let
    offset = fromMaybe 0 cursor
    limit = min pageSize (maxPokemon - offset)
    next = if offset + limit >= maxPokemon then Nothing else Just (offset + limit)
  result <- Http.sendJson decodeList (Http.get (listUrl limit offset))
  pure (map { items: _, next } result)

fetchPokemonDetail :: Definition String (Either HttpError Pokemon)
fetchPokemonDetail = define "FetchPokemonDetail" \name ->
  Http.sendJson decodePokemon (Http.get ("https://pokeapi.co/api/v2/pokemon/" <> name))
```

The view's list end is one exhaustive `case` on `Pagination.Status`:

```purescript
viewListEnd status loaded = case status of
  HasMore -> div [ class_ "load-more", onVisible ReachedListEnd ] [ text "Scroll for more" ]
  LoadingMore -> div [ class_ "load-more" ] [ span [ class_ "skeleton skeleton-load" ] [] ]
  LoadFailed error -> ... button [ onClick ReachedListEnd ] [ text "Try again" ]
  AllLoaded -> p [ class_ "status" ] [ text ("All " <> show loaded <> " Pokémon loaded.") ]
```

### Parent and child (`examples/src/Example/Parent.purs`)

The child's out-message is its `update`'s return value. No config record, no
`OutMessage` field, no `Void` anywhere.

```purescript
-- Counter
update :: Message -> Update Model Message (Maybe OutMessage)
update = case _ of
  ClickedIncrement -> do
    { count } <- modify \model -> model { count = model.count + 1 }
    pure if count == 10 then Just ReachedTen else Nothing
  ClickedDecrement -> modify_ (\model -> model { count = model.count - 1 }) $> Nothing
  ClickedReset -> put initialModel $> Nothing

-- Parent
update :: Message -> Update Model Message Unit
update (GotCounter message) = do
  out <- zoom @"counter" GotCounter (Counter.update message)
  for_ out \ReachedTen -> modify_ _ { reachedTen = true }

view :: Model -> Html Message
view model = map GotCounter (Counter.view model.counter)
```

### Tests, exactly as written (`examples/test/Test/*.purs`)

Model-level. A pure `update` needs no DSL; call it:

```purescript
it "reports ReachedTen from the tenth increment" do
  (runUpdate (Counter.update ClickedIncrement) { count: 9 }).value `shouldEqual` Just ReachedTen
```

Model-level through the program, where commands matter:

```purescript
it "caches a detail and retries a failure" do
  Test.run program do
    resolve fetchPokemonPage (Right { items: [ charmanderSummary ], next: Nothing })
    send (SelectedPokemon "charmander")
    resolve fetchPokemonDetail (Left (BadStatus 500 ""))
    expect (role Alert) (ContainsText "HTTP 500")
    send (SelectedPokemon "charmander")
    resolve fetchPokemonDetail (Right charmander)
    expectModel (Query.lookup "charmander" <<< _.details) (Success charmander)
    send (SelectedPokemon "charmander")   -- cached: no command, or the scenario fails
```

View-level:

```purescript
it "filters the grid and shows the selected detail" do
  Test.run program do
    resolve fetchPokemonPage (Right { items: [ bulbasaur, charmanderSummary ], next: Nothing })
    fill (label "Search Pokémon") "char"
    expectAbsent (named Button "Bulbasaur")
    click (named Button "Charmander")
    expectPresent (named Status "Loading Charmander")
    resolve fetchPokemonDetail (Right charmander)
    expectPresent (named Heading "Charmander")
    expect (role Article) (ContainsText "Fire")
```

No generation numbers, no `liftedCommand`, no `assertRight` wrapper, no
`expectCommands` before every `resolve`, no `Locator.text "X"` followed by `"X"`.

## Shape

### Module map

```text
foldkit/src/Foldkit.purs              Program, Update (monad), Step, perform, zoom, runUpdate
foldkit/src/Foldkit/Command.purs      Definition, Command, define (+ kernel ops for runtime/test)
foldkit/src/Foldkit/Html.purs         Html (Element | Text), Attribute, Role, builders
foldkit/src/Foldkit/AsyncData.purs    AsyncData error value (Functor), toMaybe, isLoading
foldkit/src/Foldkit/Query.purs        keyed cache child: Model, Message, init, lookup, ensure, refresh, update
foldkit/src/Foldkit/Pagination.purs   cursor list child: Model, Message, Page, Status, init, items, status, loadMore, update
foldkit/src/Foldkit/Http.purs         get, post, send, sendJson, readJson, HttpError
foldkit/src/Foldkit/Render.purs       renderToString
foldkit/src/Foldkit/Browser.purs(.js) mount: keyed diff, in-place onVisible, focus restore
foldkit/src/Foldkit/Locator.purs      accessible locators and queries, typed roles
foldkit/src/Foldkit/Test.purs         Scenario: one monad for model, view, and commands
foldkit/test/                         library specs (scenario rules, caches, view, http)
examples/src/Example/                 Counter, Parent, Pokedex (+ .Browser, .Server entries)
examples/test/                        example specs
```

Two workspace packages. `foldkit` depends on nothing in `examples`; the examples
and their tests compile against the library as a consumer would. The test
runner is the ecosystem's `spec` + `spec-node`, not a library module.

### Update and command emission (position)

`newtype Update model message a = Update (model -> { model, commands, value :: a })`,
with `Functor`/`Apply`/`Applicative`/`Bind`/`Monad` and `MonadState model`. It is
State plus an append-only command list. An app's `update` is
`message -> Update model message Unit`; a child that reports upward returns its
out-message as `a`. That one choice removes the `outMessage` field, the
`Void` type argument from every signature, `continue`/`withCommands`/
`withOutMessage`/`mapCommands`, and the `onOutMessage` callback: the parent
binds the value and handles it with ordinary code. OutMessage therefore lives
nowhere in the library. It is a convention, `Update Model Message (Maybe Out)`.

Commands are emitted with exactly one function, `perform definition args toMessage`.
There is no public way to build a `Command` value and then emit it, so there is
one way to issue a command.

### Definition vs instance, and how tests resolve without bookkeeping (position)

`Definition args result` is a name, a runner, and a captured `Show args` for
failure messages. It has no message type. The message function is supplied
per instance (`perform def args toMessage`), so it can close over anything the
caller knows. `Query.ensure` performs `perform def key (Completed key generation)`;
the generation is inside the closure. A test calls `resolve fetchPokemonDetail (Right x)`:
the scenario finds the pending command from that definition and applies the
instance's own closure to `x`. Generation, cursor, and wrapping constructor
never appear in a test.

`unsafeCoerce` is needed, once, inside `Foldkit.Command`. A `Command message`
erases `args` and `result`. To apply its `toMessage` to the test's `result`,
the library must know the test's `Definition args result` is the one the
command came from. The check is JavaScript reference equality on the
definition value (`unsafeRefEq`). Soundness argument: a definition is a
top-level, monomorphic CAF, evaluated once, so one reference has one type; two
references with the same name are two definitions, and the scenario reports
"a pending Command has that name but comes from a different Definition value"
instead of coercing. The residual hole is a definition polymorphic in an
unconstrained `result` (`forall a. Definition Unit (Array a)`), which is one
CAF at many types. Such a definition can only return `[]`-like values, and the
design documents it rather than paying a type-level tag on every definition.

### Composition (position)

`zoom @"label" Wrap childUpdate` runs a child transition against a record field
(visible type application on the label, `Record.get`/`Record.set` underneath)
and maps the child's command messages with `Wrap`. It returns the child's
value. That covers all three cases with one function:

- a sub-program: `zoom @"counter" GotCounter (Counter.update message)`;
- a keyed cache: `zoom @"details" GotDetail (Query.ensure fetchPokemonDetail name)`;
- a paginated list: `zoom @"list" GotPage (Pagination.loadMore fetchPokemonPage)`.

`Query` and `Pagination` are ordinary child components with opaque `Model` and
`Message`, so they need no `Config`, `receive`, or `liftedCommand`. The view
side is `map Wrap (Child.view model.child)`; `Submodel.mapView = map` is gone.

### Caches

`Query.Model key error value` holds `Map key { data, generation }`. `ensure`
fetches unless cached or loading (a failure refetches); `refresh` always
fetches, superseding any request in flight, and keeps the old value visible as
`Loading (Just v)`. `update` settles only the matching generation.

`Pagination.Model cursor error item` holds the items and one
`Phase = Ready (Maybe cursor) | Fetching (Maybe cursor) | Failed (Maybe cursor) error | Exhausted`.
The next request lives in the phase, so "no more pages but a next cursor" and
"loading with no cursor to retry" cannot be built. At most one page is in
flight, so the generation counter is deleted. The view reads
`Status error = HasMore | LoadingMore | LoadFailed error | AllLoaded`, replacing
`hasMore :: Boolean` plus `status :: AsyncData Unit error`.

`AsyncData error value = NotAsked | Loading (Maybe value) | Failure error (Maybe value) | Success value`.
Four constructors instead of six; the "previous value" is uniform, `Functor`
is derived, and the misleading `isSuccess` is gone (`toMaybe` says what it means).
The error comes first so `map` reaches the value, matching Elm's `RemoteData`.

### Test DSL (position)

One monad, `Scenario model message a`, over
`ReaderT { update, view } (ExceptT String (State { model, pending }))`.
Story and Scene merge; there was never a reason a model-level test should be
unable to look at the view. Failure is `Either String` inside, thrown as an
`Error` at the edge: `Test.run :: MonadThrow Error m => Program m msg -> Scenario ... -> m Unit`
plugs into any `spec` body. `Test.evaluate` returns the `Either` for tests of
the test library.

Every scenario starts from `program.model` after `program.init`, with init's
commands pending. There is no `runFrom model`: tests reach a state through the
program, so opaque cache models never need test-only constructors
(`Pagination.fromItems` is deleted).

Command expectations are deleted (`expectCommands`, `expectHasCommands`,
`expectNone`, `byDefinition`, `byInstance`, `Matcher`). The pending rule already
proves them: `resolve` fails if the command is not pending, a new message fails
if anything is pending, and the end fails if anything is left. `resolveFor def args result`
picks one of several instances and doubles as the argument assertion.

Model assertions collapse to `expectModel :: (Eq a, Show a) => (model -> a) -> a -> Scenario`,
so the model itself needs no `Show`. View assertions collapse to
`expectPresent`, `expectAbsent`, `expectCount`, and `expect locator check`, where
`Check = HasText | ContainsText | HasValue | HasAttribute | HasName | HasDescription | IsDisabled | IsEnabled | IsChecked`.
Interactions: `click`, `fill`, `submit`, `press`, `toggle`. Scoping is a
locator (`within parent child`), not reader state.

Roles are a `Role` ADT in `Foldkit.Html`, used by both `role_` in views and
`role`/`named` in locators. `named "buton" ...` no longer compiles. The
implicit-role table maps tags to `Maybe Role`.

### Html and runtime (position)

`Html message = Element { tag, attributes, children } | Text String`. No
fragment and no empty node, as in Elm: conditional content is an array
operation. The runtime diff then compares only elements and text, and the
`isComplex` full re-render path is deleted.

`Attribute` gains `Key String` and `AriaRole Role`. When any sibling is keyed,
children are reconciled by key (reuse same key and tag, move with
`insertBefore`, fresh nodes for unkeyed or duplicate keys); otherwise by
position. Pokédex cards are keyed by name, so filtering moves nodes instead of
rewriting every card.

`onVisible` stays an element attribute, not a subscription, because it is tied
to a node's lifetime. Its semantics are now defined and patched like any
event: attaching it starts an observer that fires on each entry into view,
including immediately if already visible; removing it disconnects; keeping it
swaps the handler without re-observing. The old rule "never reuse a node that
carries OnVisible" is deleted. Pagination needs no special case: the sentinel
drops `onVisible` while loading and re-attaches it after, which re-fires if the
sentinel is still in view.

Subscriptions (timers, global keys, resize) are deliberately not added. No
example needs one, and `Program` is a record, so a `subscriptions` field is a
single additive change when one does.

`Main.purs` and its `isBrowser` branch are deleted. `Example.Pokedex.Browser`
mounts; `Example.Pokedex.Server` renders. `Program.name` and the `input`/flags
parameter are deleted; a program that needs flags is a function returning a
`Program`.

### Interface depth

App authors import `Foldkit (Program, Update, perform, zoom)`,
`Foldkit.Command (Definition, define)`, `Foldkit.Html`, and the two cache
modules' five or six functions each. Behind that: generations, cursors,
message wrapping, pending-command accounting, and keyed DOM reconciliation.
Old `Command` exported 23 names; the new one exports 10, of which an app uses 2.

### What is deleted

`Foldkit.Core`, `Foldkit.Submodel`, `Foldkit.Simulation`, `Foldkit.Story`,
`Foldkit.Scene`, the library's own test runner (`Foldkit.Test` runner and its
JS), `Main.purs`, `Example.Weather` and its tests, `CommandName`, `define0`,
`command0`, `Matcher` and its six helpers, `liftResult`, `mapDefinition`,
`Query.Config`, `Query.receive`, `Query.reset`, `Query.liftedCommand`,
`Query.revalidate*`, `Pagination.Config`, `Pagination.receive`,
`Pagination.fromItems`, `Pagination.hasMore`, the pagination generation,
`AsyncData.map`/`isIdle`/`isSuccess`/`isFailure`/`getError`/`Refreshing`/`Stale`,
`Http.Client`, `pureClient`, `browserClient`, `defaultRequest`, `withHeader(s)`,
`withBody`, `postJson`, `guardStatus`, `Render.Document`,
`renderDocumentToString`, `Fragment`, `Empty`, `testId`, `Locator.first`/`last`/
`roleWith`/`title`/`testId`, and the port-history docs
(`foldkit-to-iris.md`, `decision-trail.tsv`, `scene-design.md`, `query-cache.md`,
`http.md`). Library source went from about 3,000 lines to 2,087; the whole
repo's PureScript from 4,625 to 2,995.

## Synthesis decision

Filled in by arena.

## Tradeoffs accepted

- We accept one `unsafeCoerce`, guarded by reference identity, in exchange for
  tests that resolve every command, including library-owned ones, with a plain
  result. The cost is the rule "define commands at the top level", which the
  scenario enforces with a named error rather than silent misbehavior.
- We accept that `zoom`'s label and wrapper are not tied together, so two
  children of identical types could be zoomed with the wrong constructor, in
  exchange for zero per-child configuration.
- We accept no `expectCommands` in exchange for a smaller DSL whose single
  pending rule already proves which commands ran. A test that wants to assert
  a command's arguments without resolving it cannot; it resolves with
  `resolveFor` instead.
- We accept no fragments in `Html` in exchange for a diff with no special cases.
  A view that wants "nothing" writes `[]` in the parent's children.
- We accept that `Html(..)` and `Attribute(..)` constructors are public, because
  the tree is plain data folded by four interpreters (render, browser, locator,
  test). The lowercase builders are the documented way to construct it.
- We accept no injectable HTTP client in exchange for deleting a seam no test
  used: commands never run in tests, and `readJson` is pure.
- We accept that a scenario cannot start from an arbitrary model in exchange for
  opaque cache models with no test backdoors.

## Alternatives considered

- Keep `Update` as a record and add a `Writer`-free tuple
  `Tuple model (Array (Command message))`. Smaller type, but composition needs
  either the lens records back or a `zoom` over tuples that cannot carry a
  child's out-message without a third component. The monad hides more: state,
  commands, and the return channel in one bind.
- Definitions carrying `DecodeJson args` so tests recover args from JSON with
  no coercion. Safe, but it couples every command to codecs, and still cannot
  recover the message wrapper applied by `zoom`, so `liftedCommand` returns.
- Type-level command tags (`class CommandTag tag args result | tag -> args result`)
  for coercion-free identity. Sound, but every command costs a data type and an
  instance, which is more surface than the hole it closes.
- Subscriptions now, with `onVisible` as a subscription. Rejected: visibility is
  a property of a node, not of the model, and nothing else needs subscriptions yet.

## Implementation reconciliation

Every module is implemented, not stubbed, so no `unsafeCrashWith` and no
`partial` dependency. Deviations
from the first sketch, each accepted by this runner:

- `Query.get` became `Query.lookup` because `get` collides with `MonadState`'s
  `get`, which every update module imports.
- `Test.within` (reader scope) was replaced by `Locator.within`, removing a
  second way to scope and the reader field.
- `Test.startingAt` was dropped once the Pokédex tests read better starting
  from `program.init`.

Verified beyond compiling: `iris test` runs 23 specs, all passing; a mutation
(make `Query.ensure` refetch a cached entry) fails "caches a detail and retries
a failure" with "The scenario ended with unresolved Commands"; `iris run -p examples`
prints the SSR shell; and `scripts/smoke-pokedex.mjs` against Vite and live
PokéAPI passed ("list, focus, filter, detail, pagination, stable detail panel").

`iris build` tail:

```text
Reading Spago workspace configuration...

✓ Selecting 2 packages to build:
    examples
    foldkit

Downloading dependencies...
```

`iris test` tail:

```text
Counter
  ✓︎ reports ReachedTen from the tenth increment
  ✓︎ clicks through every control
Parent
  ✓︎ records the child's ReachedTen through the mapped view
Pokédex
  ✓︎ loads pages from the start until the last cursor
  ✓︎ caches a detail and retries a failure
  ✓︎ filters the grid and shows the selected detail
  ✓︎ says when no Pokémon match

Summary
7/7 tests passed
...
Http.readJson
  ✓︎ decodes a 2xx body
  ✓︎ turns a non-2xx status into BadStatus
  ✓︎ reports malformed JSON as BadBody

Summary
16/16 tests passed
```

## Open questions and risks

- Is the namespace `Foldkit` still right once the README stops calling this a
  port? This candidate kept it to avoid churn with no consumer benefit.
- Should the scenario relax the pending rule for interactions while a command
  is in flight (typing in search while a detail loads is realistic), or is the
  strictness the point?
- Is `onVisible`'s 240px root margin a library policy or an argument?
- Does anyone need fire-and-forget commands (focus an element, log)? Today a
  command must produce a message.

## Next implementation step

Add a runtime-level test for keyed reconciliation (move, insert, remove with
duplicate keys) under a DOM shim, since the smoke test exercises it only
through the Pokédex filter.
