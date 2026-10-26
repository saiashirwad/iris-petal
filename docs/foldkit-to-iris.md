# FoldKit → Iris/PureScript: a module-by-module map

This document is the conceptual map behind `iris-foldkit`. FoldKit 0.167.0 is the
reference. The question for every module is not "how do we port this file?" but
"what problem does this module solve, and what is the smallest PureScript shape
that solves it better?"

The test library is the part that matters most. Everything else is here so the
test library has a real architecture to test.

## At a glance

| FoldKit area | FoldKit role | Status here | Iris module |
| --- | --- | --- | --- |
| `Program`, `Update`, `start`, `update`, `view` | application architecture | implemented | `Foldkit.Core` |
| `Command` | one-shot effects as data | implemented, result-typed | `Foldkit.Command` |
| Story | model-level simulation | implemented as a monadic interpreter | `Foldkit.Story` |
| Scene | view-level simulation | implemented as a monadic interpreter | `Foldkit.Scene` |
| `test/query`, `test/matchers` | accessible locators and assertions | implemented subset (name and description) | `Foldkit.Locator`, `Foldkit.Scene` |
| `Html`, attributes, events | view DSL | implemented as a pure functor | `Foldkit.Html` |
| Snabbdom, VDOM patch | DOM rendering | minimal full-replace runtime | `Foldkit.Runtime` |
| SSR serializer | HTML string rendering | implemented | `Foldkit.Render` |
| `Submodel`, `OutMessage` | parent/child composition | implemented with a plain record fold | `Foldkit.Submodel` |
| `AsyncData` | remote data ADT | implemented | `Foldkit.AsyncData` |
| `http/` | preconfigured HTTP layer | implemented as a testable Client over `ezfetch` | `Foldkit.Http` |
| experimental `Query` | keyed async cache | implemented with per-entry generations | `Foldkit.Query` |
| pagination | paged lists | implemented as a general cursor query | `Foldkit.Pagination` |
| `effect/Schema`, `defineMessageUnion` | runtime schemas and callable unions | deleted; plain ADTs | — |
| `Struct.modifyFields` | record updates | replaced by record update syntax | — |
| Subscriptions | model-gated streams | designed, deferred | — |
| Mounts | element lifecycle | designed, deferred | — |
| ManagedResources | model-gated acquire/release | designed, deferred | — |
| Routing, Navigation, Url | typed routes and history | designed, deferred | — |
| SSR hydration, document metadata | hydration and head | deferred | — |
| Ports, CustomElement, Canvas | host integration | designed, deferred | — |
| FieldValidation, Calendar | pure domain libraries | designed, deferred | — |
| DevTools | history and inspection | designed, deferred | — |

## 1. The core program

FoldKit needs a Schema for the Model because it preserves models across hot
reload, decodes hydration payloads, and exposes them to DevTools. PureScript
models are ordinary data. There is no reason to make the model a codec.

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

`out` is the OutMessage type. A top-level app instantiates it with `Void`; a
child uses its own ADT and a parent handles it. There is no `outMessage?: never`
branch and no overload pair. `Maybe out` is the whole story.

Messages are data constructors:

```purescript
data Message
  = UpdatedZipCode String
  | SubmittedWeatherForm
  | SucceededFetchWeather Weather
  | FailedFetchWeather String
```

`update` is a total function. The compiler rejects a missing case. FoldKit's
`defineMessageUnion`, `Message.match`, `Match.tagsExhaustive`, and the callable
Proxy constructors do not exist here because the language already has the thing
they emulate.

`Update.withOutMessage`, `Update.combine`, `Refreshable`, and `Update.refresh`
are convenience layers. `continue`, `withCommands`, and `withOutMessage` cover
the same ground in a few lines. `combine` can be added when a real app needs it;
it is not architecture.

## 2. Commands

FoldKit's Command is an Effect with a name and optional args, plus a message
mapping chain so tests can resolve a mapped child command without restating the
parent wrapper. In PureScript the mapping is a function, so the chain is not
needed. The command itself is inert data:

```purescript
newtype Command message = Command
  { name :: CommandName
  , arguments :: Maybe Json
  , run :: Aff message
  }
```

A `Definition` is a named constructor whose result is typed separately from the
message it produces:

```purescript
data Definition args result message = Definition
  { name :: CommandName
  , encodeArgs :: args -> Maybe Json
  , run :: args -> Aff result
  , lift :: result -> message
  }

define
  :: EncodeJson args
  => CommandName
  -> (args -> Aff result)
  -> (result -> message)
  -> Definition args result message
```

The result type is load-bearing. `Story.resolve` and `Scene.resolve` accept the
raw result, not a message:

```purescript
data FetchWeatherResult
  = FetchWeatherSucceeded Weather
  | FetchWeatherFailed String

fetchWeather :: Definition { zipCode :: String } FetchWeatherResult Message
fetchWeather =
  define
    (CommandName "FetchWeather")
    (\args -> pure (FetchWeatherSucceeded (sampleWeather args.zipCode)))
    case _ of
      FetchWeatherSucceeded weather -> SucceededFetchWeather weather
      FetchWeatherFailed error -> FailedFetchWeather error
```

A test writes `Scene.resolve fetchWeather (FetchWeatherFailed "not found")`.
Passing `ClickedIncrement` where a `FetchWeatherResult` belongs is a compile
error. FoldKit reaches the same place with `ResultMessageForMatcher` and a
conditional type; PureScript just has a type parameter.

The pending-command representation is a name plus a `Maybe Json` payload. A
definition captures an `EncodeJson args` dictionary and encodes its typed
arguments once, at construction. Matching compares the JSON structurally, so two
commands with the same data are the same instance even if a `Show` instance
would have rendered them differently. A definition matches by name; an instance
matches by name and payload. That is exactly FoldKit's definition-vs-instance
distinction, without a unique symbol brand.

This is the best available answer without existentials. A command list is
heterogeneous, so the arguments cannot all share a type; encoding them to the
canonical JSON value keeps construction typed and matching structural. The
constraint is that command arguments must be JSON data, which is what a command
should carry anyway.

Interruption, `mapEffect`, and keyed in-flight commands are deferred. `Aff`
fibers and `bracket` are the eventual implementation; they do not belong in the
first vertical slice.

## 3. Submodels and OutMessage

FoldKit encodes child composition as four `ChildFold` shapes with type-level
policy switches (`toParentOutMessage`, `foldOutMessage`, `never`). PureScript
needs one record:

```purescript
type Config parentModel parentMessage childModel childMessage out =
  { read :: parentModel -> childModel
  , write :: parentModel -> childModel -> parentModel
  , toParentMessage :: childMessage -> parentMessage
  , update :: childModel -> childMessage -> Update childModel childMessage out
  , onOutMessage :: out -> parentModel -> Update parentModel parentMessage Void
  }

foldChild
  :: Config parentModel parentMessage childModel childMessage out
  -> childMessage
  -> parentModel
  -> Update parentModel parentMessage Void
```

The parent handles the child message:

```purescript
update model = case _ of
  GotCounter childMessage ->
    Submodel.foldChild config childMessage model
  where
  config =
    { read: _.counter
    , write: \parent child -> parent { counter = child }
    , toParentMessage: GotCounter
    , update: Counter.update
    , onOutMessage: \ReachedTen parent ->
        continue parent { reachedTen = true }
    }
```

The view side is `map`:

```purescript
view model = Submodel.mapView GotCounter (Counter.view model.counter)
```

FoldKit needs `Submodel.defineView` branding and a `h.submodel` slot because its
view builder dispatches through a runtime stack. In PureScript, `Html` is a
functor and a child event already carries a child message. `map` is the boundary.
`Submodel.foldChild` maps child commands with `Command.mapMessage` and merges the
parent update produced by `onOutMessage`.

`foldChildInit` and `foldChildInits` are init-time equivalents. They are small
additions when a real app needs them; the data shape above is the foundation.

## 4. HTML, VDOM, and the runtime

FoldKit's `HtmlBuilder` has roughly 187 element constructors and 311 attribute
tags. It also has a module-global dispatch stack, boundary registries, slot IDs,
and mount bookkeeping because the view builder must attach effects while it
builds. The PureScript view is a data structure:

```purescript
data Html message
  = Element
      { tag :: String
      , attributes :: Array (Attribute message)
      , children :: Array (Html message)
      }
  | Text String
  | Fragment (Array (Html message))
  | Empty

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

`Html` is a `Functor`. An event attribute carries its message or a function from
a typed payload, so there is no dispatch stack and no runtime singleton. This is the single biggest reason the
Scene tests are pure: rendering is `view model`, and clicking is calling the
handler stored on the target.

The element and attribute set is a deliberate subset: semantic HTML, forms,
lists, headings, tables, and the ARIA attributes the test suite needs. Missing
attributes can be added with `attr "name"`. Missing elements can be added with
`element "tag"`.

`Foldkit.Runtime.runApplication` is a real browser runtime, but intentionally a
first slice: it renders the whole tree on every message. It does not diff. Focus,
selection, and scroll are therefore not preserved across updates. A Snabbdom-style
differ, keyed children, and hydration are the next runtime work; they are not
needed to prove the architecture or the test story.

FoldKit's `OnMount`, `OnUnmount`, hooks, lazy slots, and boundary dispatch are
deferred with the runtime. Their PureScript design is smaller because the view is
already pure: a mount is data attached to an element, and the runtime interprets
it.

## 5. Server rendering

`Foldkit.Render.renderToString` folds the same pure tree to escaped HTML.
`renderDocumentToString` wraps it in a document. There is no serializer stack
machine and no hydration marker protocol yet.

This is the highest-value non-test module: it runs in Node, has no DOM
dependency, and makes view snapshots trivial. FoldKit's serializer is
`experimental/server/serialize.ts`; its value is the same.

Hydration, document metadata, flags scripts, and root attributes are deferred.
The design would be: render HTML plus a root marker, then a browser entry walks
the DOM and the view tree in parallel to attach event listeners, then resumes the
normal runtime.

## 6. Routing, navigation, and URLs

Deferred, but the shape is clear. FoldKit's route parser is a bidirectional
codec wrapped in `Effect` and protected by `TerminalParser` /
`ExtendableBiparser` phantom flags. Neither needs to exist in PureScript:

```purescript
newtype Biparser value = Biparser
  { parse :: Array String -> Either ParseError (Tuple value (Array String))
  , print :: value -> PrintState -> Either ParseError PrintState
  }
```

A terminal parser is a different type from an extendable one, so the illegal
`slash` after `query` is rejected by the type checker rather than a phantom
field. `PrintState` is an array of query pairs, not `URLSearchParams`.
`Route.Transition` is a small ADT. Navigation is a handful of `Effect` FFI calls
over `window.history`.

## 7. Subscriptions

Deferred. FoldKit's subscription is a model-dependent stream that the runtime
switches when dependencies change. The PureScript sketch:

```purescript
type Subscription model message =
  { dependencies :: model -> Array String
  , subscribe :: (message -> Effect Unit) -> Effect (Effect Unit)
  }
```

The runtime keeps the current dependency snapshot, compares it with `Eq`, and
stops/starts the subscription when it changes. This replaces `Stream`,
`PubSub`, `switchMap`, and `keepAliveEquivalence` with a plain callback and an
explicit equality function. If a real app needs streams, `purescript-event` or
an `AVar` loop can live inside `subscribe`; the runtime does not need to know.

## 8. HTTP

Implemented. FoldKit's `http/` is one preconfigured Effect `HttpClient` layer.
PureScript `Aff` does not need a layer; a `Client` record is enough.

```purescript
type Client =
  { send :: Request -> Aff (Either HttpError Response)
  }

data HttpError
  = InvalidUrl String
  | NetworkError String
  | StatusError Int String String
  | DecodeError String
```

`Request` is data: a closed `Method` ADT, a URL string, an array of headers, and
an optional text body. `Response` is status, status text, headers, and body.
`browserClient` wraps `ezfetch`; `pureClient` is the test seam; `guardStatus` is
shared by both, so a fake 404 and a real 404 behave the same. `sendJson` keeps
Argonaut in charge of decoding and maps parse/decode failures to `DecodeError`.
See [`http.md`](./http.md).

The Pokedex uses this client for the list and detail endpoints. Its commands
return `Either HttpError value`, so `update` only sees page and detail messages.

### Query caches and pagination

The Pokedex does not keep one `AsyncData` for details. It keeps a
`Foldkit.Query` keyed by Pokémon name. Each entry has its own `AsyncData` and
generation, so a stale response cannot overwrite a newer one. `loadIfMissing`
starts a fetch only for `Idle`/`Failure`; a cached `Success` is a synchronous
read with no command.

The list is a `Foldkit.Pagination` model: items, next cursor, status, and a
generation. A page appends; a failure keeps the loaded items; `hasMore` is false
when the cursor is exhausted. The view puts `onVisible LoadMorePokemon` on a
sentinel at the bottom, and the runtime's `IntersectionObserver` dispatches the
message when it enters view.

Both state machines are in the library, not the Pokedex. See
[`query-cache.md`](./query-cache.md).

## 9. Mounts, managed resources, ports, and host integration

These are deferred but the data shapes are not mysterious.

A mount is an element lifecycle effect:

```purescript
data Mount message
  = Mount
      { name :: String
      , onMount :: Element -> Aff message
      , onUnmount :: Aff message
      }
```

The runtime runs `onMount` after insertion and `onUnmount` before removal. Scene
can expose pending mounts as data, the same way commands are pending data.

A managed resource is model-gated acquire/release:

```purescript
type ManagedResource key value =
  { acquire :: key -> Aff value
  , release :: key -> value -> Aff Unit
  }
```

The model stores `Map key (Maybe value)`. A runtime watcher compares the model's
requirements with the live map and uses `bracket` for the gap. FoldKit's
`Context.Service` and `Ref` machinery becomes ordinary state and `Aff`.

Ports are typed host channels:

```purescript
newtype Port value = Port String
```

with `EncodeJson`/`DecodeJson` at the boundary. Custom elements are FFI with a
typed property/event table; a PureScript record of codecs does the job that
FoldKit's generated picker object does. Canvas is a pure display list
(`Shape -> Array CanvasCommand`) plus a thin FFI painter; the current FoldKit
code already separates geometry from the `CanvasRenderingContext2D` calls.

## 10. Pure domain libraries

- `AsyncData` is implemented in `Foldkit.AsyncData` with the six states
  (`Idle`, `Loading`, `Success`, `Failure`, `Refreshing`, `Stale`). `map`,
  `match`, `getData`, `getError`, `isPending`, and `revalidate` are included.
- `FieldValidation` is deferred. The PureScript shape is
  `data Field value = NotValidated value | Validating value | Valid value |
  Invalid value (NonEmptyArray String)`, with `Rule value` as a record of
  predicate and message. Adding a request token to `Validating` fixes FoldKit's
  stale-async-result hole.
- `Calendar` is deferred; the `datetime` package gives the date arithmetic, and
  a `CalendarDate` newtype gives the invariant year/month/day. Locale formatting
  is a pure function over a locale record.
- `DevTools` is deferred. `Show`/`Generic` replace Schema summarization. A
  history store is a `Ref (Array (Tuple Message model))` plus a pure diff.

## 11. Story and Scene

This is the point of the project, so it gets its own document:
[`scene-design.md`](./scene-design.md). The short version:

- Both Story and Scene are `do` blocks in a pure interpreter.
- `Story` sees the model. `Scene` sees the rendered `Html` tree.
- Commands never run. They are resolved with a raw result value; the definition
  maps it to a message.
- A new external message is rejected while a command is pending.
- The runner rejects unresolved commands at the end.
- `Matcher` has two forms: `byDefinition` matches by name only, `byInstance`
  matches by name and exact arguments. Ambiguity is an error; exact assertions
  are order-independent multisets. `resolveExact` removes one instance at a time.
- `Scene.send` injects an external message (a subscription, mount, or host
  event) and obeys the same pending-command guard as an interaction.
- Locators live in `Foldkit.Locator`; the data query cache lives in
  `Foldkit.Query`. The module names now match the two different meanings of
  "query".
- Scene interactions call the handler stored on the target element. Clicking a
  submit button falls through to the form's submit handler when no click handler
  exists. There is no DOM and no happy-dom.
- An event handler is total (`message` or `payload -> message`), so "the handler
  ran but produced no message" is unrepresentable. FoldKit's `expectIgnored` and
  `expectHandled` have no counterpart because the state cannot occur.

## 12. TypeScript workarounds deleted

| FoldKit / TypeScript | Iris / PureScript |
| --- | --- |
| `Schema.Struct` model, `Schema` message variants | ordinary records and ADTs |
| `defineMessageUnion` plus callable Proxy constructors | `data Message = ...` |
| `Message.match` and `Match.tagsExhaustive` | `case` with compiler exhaustiveness |
| `NoInfer`, phantom `_phantom*` fields, overload pairs | monad type parameters |
| `CommandDefinitionTypeId` brand | `Definition` record and a name |
| `messageMappers` runtime chain | typed `mapMessage` / `mapDefinition` |
| `Object.assign` callable locators with `.description` | `Locator` ADT |
| runtime dispatch stack and `__setRuntime` | events carry messages |
| `happy-dom` fake target chain for pointer events | plain `Event` record |
| `expectIgnored` / `expectHandled` state machine | total typed handler, impossible |
| `deepFreeze` and model preservation | immutable data |
| `Effect` `Context.Service` plumbing | `Aff`, `ReaderT`, explicit records |
| `Function.dual` data-first/data-last overloads | one argument order |
| `Schema`-based DevTools summaries | `Show` / `Generic` |
| `Struct.modifyFields` | record update syntax |

## 13. What is intentionally not here

- A keyed VDOM differ, focus/scroll preservation, or hydration.
- Command interruption and keyed in-flight work.
- Subscriptions, mounts, managed resources, ports, custom elements, canvas.
- Routing, navigation, URL parsing, and history integration.
- Field validation, calendar, and DevTools.
- A full AccName 1.2 implementation, regex locators, or `.not` matchers.
- `expectIgnored` / `expectHandled`; the total handler type makes them
  meaningless.
- `resolveAll` resolver queues; repeated `resolve` in a `do` block is the
  direct expression of the same idea. A `resolveAll` combinator can be added if
  a data-driven test needs ordered consumption.

The next slice should add the runtime differ and mounts, because those are the
two places where the pure view tree meets real browser state. The test library
is already the part of FoldKit that most repays a rewrite, and it is complete
enough to drive those additions.
