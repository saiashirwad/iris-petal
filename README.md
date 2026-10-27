# foldkit

The Elm architecture for PureScript, compiled with Iris. A program is a model,
a message type, an `update` written as a `do` block, and a pure `view`.
Commands are inert data, so a test drives the real program, resolves each
command with a value, and never runs an effect or touches a DOM.

The demo is a Pokédex over [PokéAPI](https://pokeapi.co): infinite scroll to
151, client-side search that keeps input focus, and a cached detail panel.

## A counter

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
```

Mount it in the browser with `Foldkit.Browser.mount "root" program`, or render
it on a server with `Foldkit.Render.renderToString (view program.model)`.

## Commands

Define a command once, at the top level. Issue it from `update` with
`perform`, naming the message its result becomes:

```purescript
fetchPokemon :: Definition String (Either HttpError Pokemon)
fetchPokemon = define "FetchPokemon" \name ->
  Http.sendJson decodePokemon (Http.get ("https://pokeapi.co/api/v2/pokemon/" <> name))

update = case _ of
  Selected name -> perform fetchPokemon name GotPokemon
  GotPokemon result -> modify_ _ { pokemon = result }
```

## Testing

A scenario starts where the program starts and drives it the way a user would:

```purescript
it "shows the selected Pokémon" do
  Test.run Pokedex.program do
    resolve fetchPokemonPage (Right { items: [ bulbasaur, charmander ], next: Nothing })
    fill (label "Search Pokémon") "char"
    click (named Button "Charmander")
    resolve fetchPokemonDetail (Right charmanderDetail)
    expect (role Article) (ContainsText "Fire")
```

A new message cannot arrive while a command is pending, and a scenario that
ends with a pending command fails. `resolve` names the command by its
definition and supplies its result; nothing else about the command leaks into
the test.

## Layout

```text
foldkit/     the library package (src/Foldkit*, test/ with its own specs)
examples/    Counter, Parent, and the Pokédex, with their specs
web/         Vite entry and styles for the Pokédex
design/      the design rationale
```

## Scripts

```sh
./scripts/check.sh      # format check, build, test, server render
npm run dev             # iris watch + Vite on http://localhost:5173
npm run smoke           # Playwright against the live Pokédex
```

Requirements: Iris 0.1.13 and Node 22 or newer.
