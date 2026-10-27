# Grounding: redesign iris-foldkit into the cleanest TEA library in PureScript

Source repo (read-only reference): `/Users/texoport/code/iris-foldkit` at commit `add912e` (branch `redesign`).
Read every file under `src/`, `test/`, `web/`, plus `README.md` and `docs/idiomatic-purescript.md` before designing.

## Mandate from the owner

- Greenfield. Breaking changes are *required*, not merely accepted. No compatibility shims, no deprecated aliases.
- Goal: the cleanest, most ergonomic Elm-architecture (TEA) frontend library in PureScript, compiled with Iris.
- The consumer is (a) an app author writing `Model`/`Message`/`update`/`view` and tests, and (b) the next maintainer of the library.

## Toolchain facts (verified)

- Iris 0.1.13 is the compiler (`iris build`, `iris test`, `iris run`, `iris format`). A full build+test is ~1.5s.
- Package set registry 81.6.0. `mtl` is NOT in the set; `transformers` is (Control.Monad.Reader/State/Except/Error.Class work).
- Visible type applications work: `forall @label. ... => Proxy @label`, `over @"x" f rec` compiled and ran under Iris.
- `record` package is available (Record.get/set/modify with Proxy).
- Known Iris quirk: a nested `case` in `Story.runStory` once failed with `UnreportedUnknownType`; rewriting with `maybe` fixed it.
- The browser demo is served by Vite from `output/` (`web/main.js` imports `output/Example.Pokedex.Browser/index.js`). `npm run smoke` drives it with Playwright against the live PokéAPI.

## What exists (~4.5k lines of PureScript, ~200 exported non-Html names)

| Module | Lines | Role |
| --- | --- | --- |
| Foldkit.Core | 32 | `Update model message out = {model, commands, outMessage :: Maybe out}`; `Program {name, start, update, view}`; continue/withCommands/withOutMessage/mapCommands |
| Foldkit.Command | 140 | `Definition args result message` {name, encodeArgs, run, lift}; `Command message` {name, arguments :: Maybe Json, run :: Aff message}; Matcher; 23 exports |
| Foldkit.Html | 412 | pure `Html message` = Element/Text/Fragment/Empty; `Attribute` ADT with typed events (OnClick, OnInput, ..., OnVisible) |
| Foldkit.Runtime (+ .js) | 272 | browser runtime: Refs, positional same-tag patch, full re-render when any child is Fragment/Empty, refuses reuse when OnVisible present, focus capture/restore by id |
| Foldkit.Render | 81 | Html -> String |
| Foldkit.Submodel | 41 | `foldChild` config record {read, write, toParentMessage, update, onOutMessage}; `mapView = map` |
| Foldkit.AsyncData | 117 | Idle/Loading/Success/Failure/Refreshing/Stale; exports its own `map` (no Functor instance) |
| Foldkit.Query | 194 | keyed AsyncData cache with per-entry generation; Config {read, write, toParentMessage}; liftedCommand |
| Foldkit.Pagination | 161 | cursor pages; Config {read, write, toParentMessage}; generation guard; hasMore Boolean + status AsyncData Unit |
| Foldkit.Http | 161 | Client record over ezfetch; HttpError ADT |
| Foldkit.Simulation | 308 | shared pending-command state + rules for Story/Scene |
| Foldkit.Story | 96 | ReaderT update (ExceptT String (State ...)) model-level test DSL |
| Foldkit.Scene | 405 | same + view + locator scope; click/typeText/submit/press/check + ~12 expect* |
| Foldkit.Locator | 554 | accessible queries over the pure Html tree (roles as strings) |
| Foldkit.Test (+ .js) | 62 | tiny Aff test runner living in the *library* src |
| Example.* | ~650 | Pokedex (real PokéAPI app), Counter, Parent, Weather |

## Problems the owner's reviewer (me) already sees — confirm, refute, or go further

1. **Tests leak library internals.** Pokedex tests resolve `Query.CompletedFetch "charmander" 1 (Right x)` and `Pagination.CompletedFetch 2 (...)`: the author must know the generation counter. The README advertises `Story.resolve Pokedex.fetchPokemonDetail (Right samplePokemon)` but that does not compile against the real types. Root cause: `Definition.lift :: result -> message` cannot see the instance's args, so the generation has to live in the result.
2. **Config-record boilerplate.** `listConfig`, `detailConfig`, Submodel `config` are each `{read, write, toParentMessage}` lenses written by hand. Tests need `Query.liftedCommand detailConfig def` to even name the command.
3. **OutMessage pollutes every signature.** `Update Model Message Void` everywhere; `outMessage` field on every update even at the top level.
4. **Too many ways / too much surface.** Command exports 23 names (name, arguments, execute, matches, matchesDefinition, matcherMatches, describe, describeDefinition, describeMatcher, ...). `define` vs `define0`, `command` vs `command0`. `CommandName (CommandName "X")` boilerplate. `Http.get` == `Http.defaultRequest`; `Http.send client r = client.send r`. `Submodel.mapView = map`. `Query.reset = init`.
5. **AsyncData** exports a `map` that shadows Functor instead of instances; `isSuccess` is true for Refreshing/Stale.
6. **Story vs Scene** duplicate ~15 wrappers each. Every test wraps in `assertRight "label" (Story.runStory ...)`. `Scene.expectText (Locator.text "Bulbasaur") "Bulbasaur"` is redundant. Roles are strings (`namedRole "buton" ...` compiles).
7. **Runtime** special-cases: `isComplex` children (Fragment/Empty) force a full child re-render; `reusable` refuses to patch any element carrying OnVisible; `createNode` turns Fragment into an empty text node. No keyed children. `Program.name` is unused. `Main.purs` branches on `isBrowser` to pick SSR vs browser.
8. **App code ignores the library's seams.** Pokedex hardcodes `Http.browserClient` inside commands, re-implements JSON field decoding by hand (`decodeField` re-decodes the object per field), and never uses `Http.sendJson`.
9. **Layout.** The test runner lives in the library. Examples compile as part of the library package. Docs are written as "a port of FoldKit" (`docs/foldkit-to-iris.md`, a decision trail of port choices), not as a library.
10. **Missing TEA core.** No subscriptions (timers, keyboard, window events). `onVisible` is a one-off hack standing in for what a subscription/mount would do. Decide whether the clean core needs a subscription concept now, or whether to leave room for it.

## Keep (do not regress)

- Commands are inert data; tests resolve them with values and never run effects. Pending-command discipline (cannot send a new message while commands are unresolved; unresolved commands at the end fail the test).
- Pure `Html message` functor with typed event constructors; no DOM in tests.
- Accessible locators (role/name/label/text) and readable failure messages.
- Pure server rendering.
- The Pokédex demo works in the browser: paginated infinite scroll to 151, client-side search keeping input focus, cached detail panel with skeletons and no layout shift.
