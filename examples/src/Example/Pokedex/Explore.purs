-- | Explore the Pokédex through the JSON-lines driver, with fixture responses.
module Example.Pokedex.Explore (main) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Example.Explore.Session as Session
import Example.Pokedex (Model, Message, Pokemon, fetchPokemonDetail, fetchPokemonPage, program)
import Petal.Http (HttpError(..))
import Petal.Test (Scenario)
import Petal.Test as Test

main :: Effect Unit
main = Session.run program (Just "Search Pokémon") fixture

fixture :: Int -> String -> Either String (Scenario Model Message Unit)
fixture index name = case name of
  "first-page" -> Right
    ( Test.resolveAt index fetchPokemonPage
        ( Right
            { items:
                [ { name: "bulbasaur", url: "https://pokeapi.co/api/v2/pokemon/1/" }
                , { name: "charmander", url: "https://pokeapi.co/api/v2/pokemon/4/" }
                ]
            , next: Nothing
            }
        )
    )
  "bulbasaur" -> Right (Test.resolveAt index fetchPokemonDetail (Right bulbasaur))
  "charmander" -> Right (Test.resolveAt index fetchPokemonDetail (Right charmander))
  "detail-error" -> Right (Test.resolveAt index fetchPokemonDetail (Left (BadStatus 500 "")))
  _ -> Left ("Unknown fixture: " <> name)

bulbasaur :: Pokemon
bulbasaur =
  { id: 1
  , name: "bulbasaur"
  , height: 7
  , weight: 69
  , types: [ "grass" ]
  , sprite: Nothing
  , stats: []
  }

charmander :: Pokemon
charmander =
  { id: 4
  , name: "charmander"
  , height: 6
  , weight: 85
  , types: [ "fire" ]
  , sprite: Nothing
  , stats: []
  }
