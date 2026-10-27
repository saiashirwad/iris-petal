# Petal

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
import Petal (Program, Update)
import Petal.Html (Html, button, div, onClick, p, text)

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

Mount it in the browser with `Petal.Browser.mount "root" program`, or render
it on a server with `Petal.Render.renderToString (view program.model)`.

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

## Composition

A child is a model, a message type, and an `update`. The parent keeps the
child's model in a record field and runs the child there with `zoom`, naming
the field with a type application and wrapping the child's messages:

```purescript
update = case _ of
  GotCounter message -> do
    out <- zoom @"counter" GotCounter (Counter.update message)
    for_ out \ReachedTen -> modify_ _ { reachedTen = true }

view model = map GotCounter (Counter.view model.counter)
```

A child reports to its parent through `update`'s return value, here
`Update Counter.Model Counter.Message (Maybe OutMessage)`. There is no
configuration record and no out-message field.

The library's two stateful children work the same way. `Petal.Query` is a
keyed cache of `AsyncData` and `Petal.Pagination` is a cursor-paged list:

```purescript
SelectedPokemon name -> zoom @"details" GotDetail (Query.ensure fetchPokemonDetail name)
GotDetail message -> zoom @"details" GotDetail (Query.update message)
ReachedListEnd -> zoom @"list" GotPage (Pagination.loadMore fetchPokemonPage)
GotPage message -> zoom @"list" GotPage (Pagination.update message)
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
petal/     the library package (src/Petal*, test/ with its own specs)
examples/    Counter, Parent, and the Pokédex, with their specs
web/         Vite entry and styles for the Pokédex
docs/        design rationale, style standard, Iris setup
```

## Scripts

```sh
./scripts/check.sh      # format check, build, test, server render
npm run dev             # iris watch + Vite on http://localhost:5173
npm run smoke           # Playwright against the live Pokédex
```

Requirements: Iris 0.1.13 and Node 22 or newer.

## Documents

- [Design rationale](docs/design.md)
- [Idiomatic PureScript standard](docs/idiomatic-purescript.md)
- [Iris setup and workflow](docs/iris-setup.md)
