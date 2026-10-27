module Test.Pokedex (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Example.Pokedex
  (Message(..), Pokemon, PokemonSummary, fetchPokemonDetail, fetchPokemonPage, program)
import Foldkit.AsyncData (AsyncData(..))
import Foldkit.Html (Role(..))
import Foldkit.Http (HttpError(..))
import Foldkit.Locator (label, named, role)
import Foldkit.Pagination (Status(..))
import Foldkit.Pagination as Pagination
import Foldkit.Query as Query
import Foldkit.Test
  ( Check(..)
  , click
  , expect
  , expectAbsent
  , expectModel
  , expectPresent
  , fill
  , resolve
  , resolveFor
  , send
  )
import Foldkit.Test as Test
import Test.Spec (Spec, describe, it)

bulbasaur :: PokemonSummary
bulbasaur = { name: "bulbasaur", url: "https://pokeapi.co/api/v2/pokemon/1/" }

charmanderSummary :: PokemonSummary
charmanderSummary = { name: "charmander", url: "https://pokeapi.co/api/v2/pokemon/4/" }

squirtle :: PokemonSummary
squirtle = { name: "squirtle", url: "https://pokeapi.co/api/v2/pokemon/7/" }

charmander :: Pokemon
charmander =
  { id: 4
  , name: "charmander"
  , height: 6
  , weight: 85
  , types: [ "fire" ]
  , sprite: Just "https://example.test/charmander.png"
  , stats: [ { name: "hp", value: 39 }, { name: "attack", value: 52 } ]
  }

spec :: Spec Unit
spec = describe "Pokédex" do
  it "loads pages from the start until the last cursor" do
    Test.run program do
      resolve fetchPokemonPage (Right { items: [ bulbasaur, charmanderSummary ], next: Just 24 })
      expectModel (Pagination.items <<< _.list) [ bulbasaur, charmanderSummary ]
      send ReachedListEnd
      resolveFor fetchPokemonPage (Just 24) (Right { items: [ squirtle ], next: Nothing })
      expectModel (Pagination.status <<< _.list) AllLoaded
      expectPresent (named Button "Squirtle")

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

  it "says when no Pokémon match" do
    Test.run program do
      resolve fetchPokemonPage (Right { items: [ bulbasaur ], next: Nothing })
      fill (label "Search Pokémon") "zzz"
      expect (role Status) (HasText "No Pokémon match that search.")
