# Petal

Petal is a PureScript library for building and testing user interfaces. An application is a model, an update function, and a view. Updates change the model and describe work to perform; the browser runtime runs that work and sends its results back as messages. The same program can be exercised in memory without a browser or network.

```text
model ──view──> HTML ──interaction──> message
  ↑                                    │
  └────────────── update ──────────────┘
                    │
                 commands ──result──> message
```

## A small program

This counter has no effects: clicking the button sends `Increment`, `update` changes the model, and `view` renders the new count.

```purescript
module Main where

import Prelude

import Control.Monad.State.Class (modify_)
import Effect (Effect)
import Petal (Program, Update)
import Petal.Browser as Browser
import Petal.Html (Html, button, div, onClick, p, text)

type Model = { count :: Int }

data Message = Increment

update :: Message -> Update Model Message Unit
update Increment = modify_ \model -> model { count = model.count + 1 }

view :: Model -> Html Message
view model = div []
  [ p [] [ text (show model.count) ]
  , button [ onClick Increment ] [ text "+" ]
  ]

program :: Program Model Message
program =
  { model: { count: 0 }
  , init: pure unit
  , update
  , view
  }

main :: Effect Unit
main = Browser.mount "root" program
```

The host page needs an element with `id="root"` and a JavaScript entry point that calls the compiled `Main.main`. `init` is an update run once at startup; use it to issue initial commands or change the initial model. The browser runtime renders the view, patches the DOM after updates, and runs commands asynchronously.

## Commands

A command definition names an effect and provides an `Aff` runner. Define it at the top level, then issue it from an update with `perform`:

```purescript
import Data.Either (Either)
import Petal (perform)
import Petal.Command (Definition, define)
import Petal.Http (HttpError)
import Petal.Http as Http

fetchPokemonDetail :: Definition String (Either HttpError Pokemon)
fetchPokemonDetail = define "FetchPokemonDetail" \name ->
  Http.sendJson decodePokemon
    (Http.get ("https://pokeapi.co/api/v2/pokemon/" <> name))

-- Inside an update branch:
perform fetchPokemonDetail name GotDetail
```

Here `GotDetail` turns the result into a message handled by the program. `Pokemon`, `decodePokemon`, and that message constructor belong to the application; a complete example is in [Example.Pokedex](examples/src/Example/Pokedex.purs). Keep definitions at the top level: tests identify commands by the definition value, not its name.

`Update model message a` is a state transition that also collects commands. It implements `MonadState`, so updates can use operations such as `get`, `modify_`, and `put`. `runUpdate` exposes the resulting model, commands, and return value for direct testing. For nested models, `zoom` runs a child update on a record field and maps its messages into the parent.

## Testing a program

`Petal.Test` runs scenarios against the program's pure HTML tree. Locate controls by their accessible role or label, interact with them, and check what the view shows. This test uses the [counter example](examples/src/Example/Counter.purs):

```purescript
import Petal.Html (Role(..))
import Petal.Locator (named, role)
import Petal.Test (Check(..), click, expect)
import Petal.Test as Test

-- In a Test.Spec test:
Test.run Counter.program do
  click (named Button "+")
  click (named Button "+")
  expect (role Paragraph) (HasText "2")
```

### A story with a request

The Pokédex starts by requesting a page. This story supplies that page, searches the visible list, opens a result, and supplies its details:

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

The first `resolve` answers the command issued by `init`. Clicking Charmander issues another command; the test can inspect the loading view before answering it. The named Pokémon values are fixtures defined in the [complete test](examples/test/Test/Pokedex.purs), along with the imports.

### A story about failure and retry

The same scenario API can send messages directly and inspect model state when that makes a behavior easier to express:

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
    send (SelectedPokemon "charmander")
```

The last selection uses the cached result. If it issued another command, the scenario would fail because that command was left unresolved.

Commands are not executed in a scenario. Instead, supply their results with `resolve definition result`, or `resolveFor definition args result` when the same definition has multiple pending commands. Scenarios start after `init`, must resolve pending commands before sending another message or interaction, and fail if commands remain unresolved at the end. Locators used for an interaction or a single-element expectation must match exactly one element; use `within` or `nth` to choose one. The [Pokédex tests](examples/test/Test/Pokedex.purs) also show pagination and model assertions; the [parent test](examples/test/Test/Parent.purs) exercises a composed component.

### Explore a program step by step

`Test.start` creates an in-memory session. `step` applies a scenario action and returns the new session, or an error that leaves the input session usable. `observe` summarizes the view and lists pending commands in order, and `finish` checks that none remain. Unlike `Test.run`, a session accepts interactions while commands are pending; `runConcurrent` runs a complete scenario the same way. `resolveAt` resolves the pending command at a zero-based index in the latest observation, which tells apart requests with identical arguments.

The Pokédex has a fixture-backed JSON-lines driver for people and agents to try this:

```sh
iris run -q -p examples --main Example.Pokedex.Explore
```

It prints an observation, then reads one JSON object per line. These lines reproduce an out-of-order response:

```json
{"op":"resolve","index":0,"fixture":"first-page"}
{"op":"click","name":"Bulbasaur"}
{"op":"click","name":"Charmander"}
{"op":"resolve","index":1,"fixture":"charmander"}
{"op":"resolve","index":0,"fixture":"bulbasaur"}
{"op":"finish"}
{"op":"trace"}
```

Each successful action prints the new observation. `trace` prints the successful actions so far; feed them back one per line to replay them in a fresh session. The other operations are `observe`, `reset`, and `fill` with a `value`; `detail-error` is a fixture for a failed detail request. The [regression test](examples/test/Test/Pokedex.purs) records the intended behavior as assertions; a trace is only an interaction log.

The composed counter runs on the same driver with `--main Example.Parent.Explore`; send `{"op":"click","name":"+"}`. It has no fixtures or text input, and its hidden `reachedTen` flag is not part of the observation; the [parent test](examples/test/Test/Parent.purs) checks it directly.

The view summary follows Petal's locator semantics and is not a browser accessibility tree.

The test runner interprets Petal's HTML and event handlers in memory. It is useful for testing application behavior, but it does not run a browser or replace browser tests for DOM, layout, or integration behavior.

## Included modules

| Module | Purpose |
| --- | --- |
| `Petal`, `Petal.Command` | Programs, state transitions, commands, and nested updates |
| `Petal.Html`, `Petal.Browser` | Typed HTML and events; browser mounting and DOM updates |
| `Petal.Render` | Render an HTML tree to a string |
| `Petal.Test`, `Petal.Locator` | In-memory scenarios, interactions, and accessible queries |
| `Petal.Http` | HTTP requests and JSON decoding for command runners |
| `Petal.AsyncData`, `Petal.Query` | Loading states and a keyed cache with stale-response protection |
| `Petal.Pagination` | Cursor-based page loading, retry, and accumulated items |

## Run this repository

The workspace contains the `petal` library and an `examples` package. It uses [Iris](https://iris-lang.com/) and Node.js; the checked-in setup notes list Iris 0.1.13 and Node 22 or newer. See [Iris setup](docs/iris-setup.md) for installation and tooling details.

```sh
iris build              # compile both packages
iris test               # run library and example tests
iris run -p examples    # print the server-rendered Pokédex shell
./scripts/check.sh      # format check, build, tests, and server example
```

To run the browser Pokédex locally:

```sh
npm install
npm run dev
```

Open <http://localhost:5173/>. The example fetches data from PokéAPI. For the browser smoke test, install Chromium with `npx playwright install chromium`, leave the dev server running, and run `npm run smoke` in another terminal.
