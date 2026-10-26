# iris-foldkit

FoldKit's Elm architecture and Story/Scene testing for PureScript, compiled with
Iris.

This is a working vertical slice, not a wrapper around the npm package. The
program model, commands, pure view tree, Submodel fold, AsyncData, Story, Scene,
accessible queries, server rendering, and a minimal browser runtime are all
implemented in PureScript. The test suite is the part that matters most.

```text
26 passed, 0 failed.
```

## Quick start

Requirements: Iris 0.1.13, Node 22 or newer, Spago 1.0.4.

```sh
./scripts/check.sh   # format check, build, test, run
```

Or run the steps directly:

```sh
iris format --check
iris build
iris test
iris run
```

`iris run` prints the counter and weather views as HTML. In a browser bundle,
`Main.main` starts the weather app in `#root` instead.

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

A command is inert data with a name, canonical argument text, and an `Aff`
runner. A definition owns the result-to-message lift:

```purescript
data Definition args result message = Definition
  { name :: CommandName
  , run :: args -> Aff result
  , lift :: result -> message
  }
```

Tests resolve the raw result; they never run the command.

## Story and Scene

A Story test drives `update` and asserts on the model:

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

A Scene test drives the rendered `Html` tree:

```purescript
Scene.runScene Weather.update Weather.view Weather.initialModel do
  Scene.expectExists (Query.label "Zip code")
  Scene.typeText (Query.label "Zip code") "90210"
  Scene.submit (Query.role "form")
  Scene.expectExists (Query.namedRole "button" "Loading...")
  Scene.expectCommands
    [ Command.byInstance
        (Command.command Weather.fetchWeather { zipCode: "90210" })
    ]
  Scene.resolve
    Weather.fetchWeather
    (Weather.FetchWeatherSucceeded (Weather.sampleWeather "90210"))
  Scene.within (Query.role "article") do
    Scene.expectExists (Query.namedRole "heading" "90210")
    Scene.expectText
      (Query.text "Beverly Hills, California")
      "Beverly Hills, California"
    Scene.expectText (Query.text "72°F") "72°F"
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
- **Pure server rendering.** `renderToString` is a fold over the same view tree.
- **Effects only at the edge.** `Aff` is in command runners and the browser
  runtime. The test interpreter and the renderer are pure.

## Layout

```text
src/Foldkit/Core.purs          Update, Program, constructors
src/Foldkit/Command.purs       Command, Definition, Matcher
src/Foldkit/Html.purs          pure Html/Attribute/KeyEvent tree
src/Foldkit/Query.purs         accessible locators and queries
src/Foldkit/Simulation.purs    shared pending-command state and rules
src/Foldkit/Story.purs         model-level interpreter
src/Foldkit/Scene.purs         view-level interpreter
src/Foldkit/Submodel.purs      parent/child fold and OutMessage
src/Foldkit/AsyncData.purs     remote data ADT
src/Foldkit/Render.purs        Html -> String
src/Foldkit/Runtime.purs       minimal browser runtime (full re-render)
src/Foldkit/Test.purs          small Aff test runner
src/Example/Counter.purs       counter with an OutMessage at ten
src/Example/Parent.purs        Submodel composition
src/Example/Weather.purs       async form with a typed command result
src/Example/Browser.purs       browser entry
test/Test/**                   Story, Scene, command, and render tests
```

## Status

Implemented:

- Program/Update/Command with result-typed definitions.
- Pure `Html` with typed events and a `Functor` instance.
- Story and Scene interpreters with pending-command discipline.
- Definition and instance matchers, exact multiset and subset assertions,
  ambiguity errors, `resolveExact`.
- Accessible locators for role/name, label, text, placeholder, alt text, title,
  test id, display value; `within`, `first`, `last`, `nth`; text/value/disabled/
  enabled/checked/attribute/accessible-name/count assertions.
- Click, type, submit, keydown, check; submit-button fallback to the form.
- Submodel fold and OutMessage with a parent example.
- AsyncData, `renderToString`, and a minimal full-re-render browser runtime.
- A small test runner run by `iris test`.

Deferred with a design in
[docs/foldkit-to-iris.md](docs/foldkit-to-iris.md): VDOM diffing/hydration,
subscriptions, mounts, managed resources, routing/navigation, ports, custom
elements, canvas, field validation, calendar, and DevTools.

## Documents

- [FoldKit → Iris module map](docs/foldkit-to-iris.md)
- [Story and Scene design](docs/scene-design.md)
- [Idiomatic PureScript standard](docs/idiomatic-purescript.md)
- [Iris setup and workflow](docs/iris-setup.md)
- [Decision trail](docs/decision-trail.tsv)
