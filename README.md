# iris-foldkit

FoldKit's Elm architecture and Story/Scene testing for PureScript, compiled with
Iris. The demo is a working Pokédex that fetches the first 151 Pokémon from
[PokéAPI](https://pokeapi.co).

This is an implementation, not a wrapper around the npm package. The program
model, commands, pure view tree, Submodel fold, AsyncData, typed HTTP client,
Story, Scene, accessible queries, server rendering, browser runtime, and Vite
development setup are all here. The test suite is the part that matters most.

```text
41 passed, 0 failed.
```

## Quick start

Requirements: Iris 0.1.13, Node 22 or newer, Spago 1.0.4.

```sh
npm install
npm run dev
```

Open <http://localhost:5173>. `npm run dev` runs `iris watch` and Vite together.
A PureScript edit rebuilds the output; Vite updates the page. The search input
keeps focus across re-renders.

Other commands:

```sh
./scripts/check.sh   # format check, build, test, and run
npm run build        # production build into dist/
npm run smoke        # headless browser: list, search focus, filter, detail
```

Or run the PureScript steps directly:

```sh
iris format --check
iris build
iris test
iris run
```

`iris run` prints the server-rendered Pokédex shell. In a browser bundle,
`Main.main` starts the Pokédex in `#root` instead.

## The Pokédex

The example is a real app, not a fixture:

- `Example.Pokedex.start` loads the first page of
  `https://pokeapi.co/api/v2/pokemon` through a paginated query.
- A sentinel at the bottom of the grid loads the next 24 when it enters view.
  Scrolling reaches all 151.
- The list is filtered client-side by name.
- Selecting a card runs `FetchPokemonDetail` against the PokéAPI detail endpoint.
- Details are cached by name. Selecting a cached card is synchronous and sends
  no command.
- The detail panel shows sprite, types, height, weight, and base stats.
- Loading uses fixed-size skeletons; the detail panel keeps the same height
  before, during, and after a load.
- The browser smoke test verifies the live page, not just the pure interpreter.

## Typed HTTP

`Foldkit.Http` is a small, typed client over
[`ezfetch`](https://github.com/purescript-web/purescript-ezfetch), which is a
PureScript `Aff` API over the platform fetch implementation. Application code
does not call a raw JS `fetch` foreign import.

```purescript
type Request =
  { method :: Method
  , url :: String
  , headers :: Array (Tuple String String)
  , body :: Maybe String
  }

type Response =
  { status :: Int
  , statusText :: String
  , headers :: Map String String
  , body :: String
  }

data HttpError
  = InvalidUrl String
  | NetworkError String
  | StatusError Int String String
  | DecodeError String

type Client =
  { send :: Request -> Aff (Either HttpError Response)
  }
```

`Client` is a record, so tests inject `pureClient` and commands resolve without a
network. `browserClient` maps `ezfetch` responses into the same error ADT.
`sendJson` parses and decodes with Argonaut. `guardStatus` is shared by both
clients, so a fake 404 behaves like a real 404.

See [docs/http.md](docs/http.md).

## The architecture

`Foldkit.Core` is the whole program shape:

```purescript
type Update model message out =
  { model :: model
  , commands :: Array (Command message)
  , outMessage :: Maybe out
  }

type Program model message input out =
  { name :: String
  , start :: input -> Update model message out
  , update :: model -> message -> Update model message out
  , view :: model -> Html message
  }
```

`out` is the OutMessage type. A top-level app uses `Void`; a child uses its own
ADT and the parent handles it. Messages are ordinary ADTs, so `update` is checked
for exhaustiveness. There is no runtime Schema for the model or the messages.

A command is inert data with a name, a structurally comparable JSON payload,
and an `Aff` runner. A definition owns the result-to-message lift:

```purescript
data Definition args result message = Definition
  { name :: CommandName
  , encodeArgs :: args -> Maybe Json
  , run :: args -> Aff result
  , lift :: result -> message
  }
```

Tests resolve the raw result; they never run the command. `start` is also
effectful: the runtime renders the initial model and then runs the commands it
returned.

## Story and Scene

A Story test drives `update` and asserts on the model:

```purescript
Story.runStory Pokedex.update Pokedex.initialModel do
  Story.send (Pokedex.LoadedList summaries)
  Story.expectModel "the list is loaded" \model ->
    case model.list of
      Success values -> Array.length values == 2
      _ -> false
  Story.send (Pokedex.SelectedPokemon "charmander")
  Story.expectCommands
    [ Command.byDefinition Pokedex.fetchPokemonDetail ]
  Story.resolve Pokedex.fetchPokemonDetail (Right samplePokemon)
  Story.expectModel "the detail is loaded" \model ->
    case model.detail of
      Success pokemon -> pokemon.name == "charmander"
      _ -> false
  Story.expectNone
```

A Scene test drives the rendered `Html` tree:

```purescript
Scene.runScene Pokedex.update Pokedex.view loadedModel do
  Scene.expectText (Locator.text "Bulbasaur") "Bulbasaur"
  Scene.typeText (Locator.label "Search Pokémon") "char"
  Scene.expectText (Locator.text "Charmander") "Charmander"
  Scene.expectAbsent (Locator.text "Bulbasaur")
  Scene.click (Locator.namedRole "button" "Charmander")
  Scene.expectCommands
    [ Command.byDefinition Pokedex.fetchPokemonDetail ]
  Scene.resolve Pokedex.fetchPokemonDetail (Right samplePokemon)
  Scene.expectText (Locator.namedRole "heading" "Charmander") "Charmander"
  Scene.expectTextContaining (Locator.role "article") "Fire"
  Scene.expectNone
```

There is no DOM and no happy-dom. `Html message` is a pure functor; an `OnClick`
attribute carries its message. Clicking calls the handler, runs `update`, and
asks `view` for the next tree. The command queue is checked before every external
message and again at the end of the run.

## What PureScript buys

- **No schema layer.** Models and messages are records and ADTs. `case` is
  exhaustive. FoldKit's `Schema`, `defineMessageUnion`, callable Proxy
  constructors, and `Match` helpers have no counterpart because the language
  already has the feature.
- **No phantom typing.** `StoryM model message out` and `SceneM model message out`
  carry the types through the `do` block. No `_phantomModel`, `_phantomMessage`,
  `NoInfer`, or overload pairs.
- **No stringly events.** `OnClick`, `OnInput`, `OnSubmit`, and `OnKeyDown` are
  constructors with typed payloads, not entries in a string-keyed handler table.
- **No invented "ignored" state.** An event handler is total, so "the handler ran
  but produced no message" cannot happen. FoldKit's `expectIgnored` and
  `expectHandled` disappear.
- **One state shape for both runners.** Scene keeps only its locator scope in the
  reader; commands, model, and OutMessage live in the same `Simulation.State` as
  Story. The command and assertion rules cannot drift.
- **Typed effects at the edge.** Commands are `Aff` values with typed results.
  HTTP is a `Client` record with a typed error ADT, not an untyped JS callback.
- **Pure server rendering.** `renderToString` is a fold over the same view tree.

## Layout

```text
src/Foldkit/Core.purs          Update, Program, constructors
src/Foldkit/Command.purs       Command, Definition, Matcher
src/Foldkit/Html.purs          pure Html/Attribute/KeyEvent tree
src/Foldkit/Locator.purs       accessible locators and queries
src/Foldkit/Query.purs         keyed AsyncData cache with generations
src/Foldkit/Pagination.purs    cursor pages with append and retry
src/Foldkit/Simulation.purs    shared pending-command state and rules
src/Foldkit/Story.purs         model-level interpreter
src/Foldkit/Scene.purs         view-level interpreter
src/Foldkit/Submodel.purs      parent/child fold and OutMessage
src/Foldkit/AsyncData.purs     remote data ADT
src/Foldkit/Http.purs          typed Client over ezfetch
src/Foldkit/Render.purs        Html -> String
src/Foldkit/Runtime.purs       browser runtime (same-tag patch + focus restore)
src/Foldkit/Test.purs          small Aff test runner
src/Example/Pokedex.purs       the real PokéAPI app
src/Example/Pokedex/Browser.purs
src/Example/Counter.purs       counter with an OutMessage at ten
src/Example/Parent.purs        Submodel composition
src/Example/Weather.purs       async form with a typed command result
web/main.js                    Vite entry
web/styles.css                 Pokédex styles
test/Test/**                   Story, Scene, HTTP, command, and Pokedex tests
```

## Status

Implemented:

- Program/Update/Command with result-typed definitions and initial commands.
- Pure `Html` with typed events and a `Functor` instance.
- Story and Scene interpreters with pending-command discipline.
- Definition and instance matchers, exact multiset and subset assertions,
  ambiguity errors, `resolveExact`, and `Scene.send`.
- Accessible locators and assertions, including name and description.
- Click, type, submit, keydown, check; submit-button fallback to the form.
- Submodel fold and OutMessage with a parent example.
- Keyed query cache, cursor pagination, and viewport sentinel (`onVisible`).
- AsyncData, typed HTTP with a testable `Client`, `renderToString`, and a small
  same-tag DOM patch with focus restoration.
- Pokédex app, Vite dev server, hot reload, and a headless browser smoke test.
- A small test runner run by `iris test`.

Deferred with a design in
[docs/foldkit-to-iris.md](docs/foldkit-to-iris.md): VDOM diffing/hydration,
subscriptions, mounts, managed resources, routing/navigation, ports, custom
elements, canvas, field validation, calendar, and DevTools.

## Documents

- [FoldKit → Iris module map](docs/foldkit-to-iris.md)
- [Story and Scene design](docs/scene-design.md)
- [Typed HTTP](docs/http.md)
- [Query caches and pagination](docs/query-cache.md)
- [Idiomatic PureScript standard](docs/idiomatic-purescript.md)
- [Iris setup and workflow](docs/iris-setup.md)
- [Decision trail](docs/decision-trail.tsv)
