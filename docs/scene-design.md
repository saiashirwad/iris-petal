# Story and Scene: the test story

FoldKit's testing story is the reason this project exists. A Story test drives
`update` and asserts on the model. A Scene test drives a rendered view, asserts
through accessible locators, and resolves commands. Neither runs an effect.

The PureScript version keeps both guarantees and removes the type-level
scaffolding that FoldKit needs in TypeScript.

## The shape

Both runners are pure interpreters. Scene keeps its locator scope in the reader
environment, so both interpreters share exactly one state record:

```purescript
type StoryM model message out =
  ReaderT
    (model -> message -> Update model message out)
    (ExceptT String (State (Simulation.State model message out)))

type SceneM model message out =
  ReaderT
    ( { update :: model -> message -> Update model message out
      , view :: model -> Html message
      , scope :: Maybe Locator
      }
    )
    (ExceptT String (State (Simulation.State model message out)))
```

`model`, `message`, and `out` are the monad's parameters. They stay fixed for the
whole block, so `send`, `resolve`, `expectModel`, and locator actions all infer
without annotations. There are no `_phantomModel`, `_phantomMessage`, `NoInfer`,
or overload pairs.

A test is a `do` block:

```purescript
Story.runStory Weather.update Weather.initialModel do
  Story.send (Weather.UpdatedZipCode "90210")
  Story.send Weather.SubmittedWeatherForm
  Story.expectModel "weather is loading" \model ->
    isPending model.weather
  Story.expectCommands
    [ Command.byDefinition Weather.fetchWeather ]
  Story.resolve
    Weather.fetchWeather
    (Weather.FetchWeatherSucceeded (Weather.sampleWeather "90210"))
  Story.expectModel "weather is a success" \model ->
    case model.weather of
      Success weather -> weather.temperature == 72
      _ -> false
  Story.expectNone
```

`given` is not a step. The initial model is an argument to the runner. This is
one less concept, one less ordering mistake, and it makes the model available to
the type checker before the first action.

## Why a monad

Three candidates were prototyped against the same requirements.

**Flat step list (closest to FoldKit).** A test is an
`Array (StoryStep model message out)` interpreted left to right. It reads like a
screenplay, has an obvious step index for errors, and needs no monad. The costs
are real: conditional steps and data-driven steps become array concatenation,
reusable groups are arrays rather than functions, and a step cannot return a
value without an existential or a tuple. It also needs a `given` step because the
array carries the model only at runtime.

**Indexed/free DSL.** The idea is to make illegal orderings unrepresentable:
resolution tied to a command's result, view assertions only in a Scene, command
actions only in a scope. PureScript has no GADTs, so a typed `Resolve`
instruction needs an existential plus a cast. More importantly, `update` emits a
data-dependent number of commands. A type-level pending set would force every
test to spell its command trace in types and would turn a readable command diff
into a row-unification error. That is anti-test. The useful part of this
candidate is a **result-typed `Definition`**, and that is adopted.

**Monadic DSL (chosen).** A `do` block is the native way to express a sequence
in PureScript. Reusable groups are ordinary functions. Conditional and
data-driven steps use `when`, `for_`, and `traverse` without inventing step
combinators. The interpreter stays pure and fail-fast. The cost is that a step is
not inspectable data; for a hand-written screenplay, that is not a cost that
matters. If step introspection is needed later, a `tap` action can expose the
model, commands, and rendered tree at that point.

The cross-model reviewer's verdict was **adopt with changes**: keep the monadic
interpreter, add definition-level command matchers, expose `Scene.send`, and
share the command/assertion actions. Those changes are in the code.

## Commands are inert data

A command carries a name, canonical argument text, and an `Aff` runner. The test
runner never calls the runner.

```purescript
newtype Command message = Command
  { name :: CommandName
  , arguments :: String
  , run :: Aff message
  }
```

A definition is a data type, not a callable object with a brand:

```purescript
data Definition args result message = Definition
  { name :: CommandName
  , run :: args -> Aff result
  , lift :: result -> message
  }
```

`resolve` takes the raw result, so the test cannot resolve a command with an
unrelated message:

```purescript
Story.resolve
  Weather.fetchWeather
  (Weather.FetchWeatherFailed "Location not found")
```

The definition lifts `FetchWeatherFailed` to `FailedFetchWeather`. If the test
passes `ClickedIncrement`, the definition does not type-check. FoldKit builds the
same guarantee out of conditional types and a `ResultMessageForMatcher`; here it
is one type parameter.

Matching has exactly two modes, copied from FoldKit because both are useful:

```purescript
data Matcher message
  = MatchDefinition CommandName
  | MatchInstance (Command message)

byDefinition :: Definition args result message -> Matcher message
byInstance :: Command message -> Matcher message
```

- **Definition match:** name only. `expectCommands [ byDefinition fetchWeather ]`
  means "one `FetchWeather`, whatever its arguments".
- **Instance match:** name plus exact canonical arguments.
  `expectCommands [ byInstance (Command.command fetchWeather { zipCode: "90210" }) ]`
  means "that invocation".
- `resolve definition result` uses the definition match. It is an error if two
  pending commands match, because the test did not say which invocation it meant.
- `resolveExact command result` uses the instance match. It removes one matching
  invocation, so duplicate identical commands can be resolved one at a time.
- `expectCommands` is order-independent multiset equality. `expectHasCommands`
  is a subset. `expectNone` is the empty multiset.

`expectCommands` checks the whole pending queue, not just one entry. A test that
forgets to resolve a command cannot reach the next external message, and cannot
end successfully.

## Pending discipline

`send`, `Scene.send`, and every Scene interaction check the pending queue first.
If a non-empty queue exists, the action fails with the pending commands printed:

```text
I found unresolved Commands when you sent a new Message:

    "FetchWeather" { zipCode: "90210" }

Resolve all Commands before sending the next Message.
```

`resolve` removes the matching command, runs `update` with the lifted result,
and appends whatever commands that update produced. Resolution can cascade. The
runner checks the queue again at the end.

There is no keyed/interruptible exemption yet. When interruption lands, keyed
commands will be allowed to stay pending across messages, exactly as FoldKit
does.

## Scene is a pure view interpreter

`Html message` is a data structure. Events are typed constructors:

```purescript
data Attribute message
  = Attribute String String
  | BooleanAttribute String Boolean
  | OnClick message
  | OnInput (String -> message)
  | OnChange (String -> message)
  | OnCheck (Boolean -> message)
  | OnSubmit message
  | OnKeyDown (KeyEvent -> message)
  | OnFocus message
  | OnBlur message
```

Clicking is therefore:

1. Resolve the locator against `view model`.
2. Read the `OnClick` attribute from the target or its nearest ancestor.
3. Apply the handler to a synthetic event value.
4. Run `update` on the resulting message.
5. Re-render by asking the same pure `view` function for the new model.

There is no DOM, no happy-dom, and no dispatch singleton. `typeText` applies the
`OnInput` handler to `value`; `submit` applies `OnSubmit`; `press` applies
`OnKeyDown` with a `KeyEvent`; `check` applies `OnCheck` with the negated checked
state. Clicking a submit button with no click handler falls through to the
nearest ancestor form's submit handler, which is why both of these tests work:

```purescript
Scene.click (Query.namedRole "button" "Get Weather")
Scene.submit (Query.role "form")
```

Event propagation is deliberately simpler than the browser: the nearest handler
wins, and there is no `stopPropagation`/`preventDefault` model yet. FoldKit
models both because it emulates browser semantics. The first slice only needs
the common case; a `ClickOptions` record can be added when a real component
needs it.

Because the handler type is total, an interaction cannot "run but produce
nothing". FoldKit needs `expectIgnored` and `expectHandled` to acknowledge that
state. Here the state does not exist, so neither action exists.

## Locators and assertions

Locators are an ADT, not a callable object with a `.description` property:

```purescript
role :: String -> Locator
namedRole :: String -> String -> Locator
label :: String -> Locator
text :: String -> Locator
textContaining :: String -> Locator
placeholder, altText, title, testId, displayValue :: String -> Locator
within :: Locator -> Locator -> Locator
first, last, nth :: Locator -> Locator
```

Scene assertions are actions:

```purescript
Scene.expectExists locator
Scene.expectAbsent locator
Scene.expectText locator "exact text"
Scene.expectTextContaining locator "substring"
Scene.expectValue locator "90210"
Scene.expectDisabled locator
Scene.expectEnabled locator
Scene.expectChecked locator
Scene.expectAttribute locator "aria-busy" "true"
Scene.expectAccessibleName locator "Zip code"
Scene.expectAccessibleDescription locator "We never share it."
Scene.expectCount 2 locator
```

`within` changes the reader environment with `local`. Nested scopes compose as
`within parent (within child assertion)`, so the article test reads exactly like
the rendered structure:

```purescript
Scene.within (Query.role "article") do
  Scene.expectExists (Query.namedRole "heading" "90210")
  Scene.expectText
    (Query.text "Beverly Hills, California")
    "Beverly Hills, California"
  Scene.expectText (Query.text "72°F") "72°F"
```

Accessible names resolve `aria-labelledby`, `aria-label`, `<label for>`, a
wrapping `<label>`, native names such as `img[alt]` and submit-button `value`,
name-from-content for buttons/links/headings/options, and `title`. Role
resolution covers explicit `role` plus the common implicit roles. This is a
subset of AccName 1.2. Regex locators, `.not` matchers, and CSS selectors are not
implemented. The failure text always names the locator and the actual value.

`Query.find` returns the first match, matching FoldKit's locator semantics.
`expectCount` uses `Query.findAll`. `Query.text` keeps the deepest matching
element so a text assertion does not silently match a wrapper that also contains
the text.

## Failure behavior

The interpreter fails fast. After a failed step, the model is not what production
would have, so continuing produces noise. The exception is the queue assertions,
which report the full expected and actual sets in one message.

Every failure is a `Left String` from the pure runner. The FFI-free boundary
means tests can inspect a failure without throwing:

```purescript
case
  Scene.runScene Counter.update Counter.view Counter.initialModel do
    Scene.click (Query.namedRole "button" "Nope")
  of
  Left errorMessage -> assertTrue "names the locator" (contains "Nope" errorMessage)
  Right _ -> assertTrue "expected failure" false
```

`Foldkit.Test` turns `Left` into a failed test with the message in the report.
`iris test` runs `Test.Main`; there is no Vitest dependency.

## Type inference

A test body normally needs no annotations. The runner fixes `model`, `message`,
and `out`; the actions are polymorphic in exactly those parameters. The places a
signature appears are all at the boundary of reusable code:

- A reusable action that is polymorphic over Story and Scene needs a rank-2
  signature. A concrete `Story ... Unit` or `Scene ... Unit` action needs none.
- `expectModel` needs `Show model`, because the failure prints the model.
  `expectModelWith` takes a renderer instead, for models that contain functions.
- `expectOutMessage` needs `Eq out` and `Show out`.
- A helper that returns `Update model message out` may need its model/message
  types named if it is used at two different instantiations.

Those constraints are honest: they exist because the assertion prints something,
not because the runner needs a phantom.

## Extending to mounts and managed resources

The state record is the extension point. A mount is another pending collection;
a managed resource is another live map. Actions such as `expectMount`,
`resolveMount`, `acquire`, and `release` can be added without changing the
monad's parameters or the existing actions. The pending discipline becomes: an
external message is blocked by unresolved commands and unresolved mounts (and
unacknowledged unmounts), which is exactly FoldKit's rule.

Subscriptions enter as `Scene.send` (or `Subscription.emit`). That is an external
message and obeys the same guard. Managed resources enter as model-gated
`acquire`/`release` actions with a requirements check.

The important constraint is that all of these remain data and pure actions in
the test interpreter. None of them should require a real DOM, a real timer, or a
real socket.
