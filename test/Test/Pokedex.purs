module Test.Pokedex where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Example.Pokedex as Pokedex
import Foldkit.AsyncData (AsyncData(..))
import Foldkit.Command as Command
import Foldkit.Http as Http
import Foldkit.Query as Query
import Foldkit.Scene as Scene
import Foldkit.Story as Story
import Foldkit.Test (Test, assertRight, test)

summaries :: Array Pokedex.PokemonSummary
summaries =
  [ { name: "bulbasaur", url: "https://pokeapi.co/api/v2/pokemon/1/" }
  , { name: "charmander", url: "https://pokeapi.co/api/v2/pokemon/4/" }
  ]

samplePokemon :: Pokedex.Pokemon
samplePokemon =
  { id: 4
  , name: "charmander"
  , height: 6
  , weight: 85
  , types: [ "fire" ]
  , sprite: Just "https://example.test/charmander.png"
  , stats: [ { name: "hp", value: 39 }, { name: "attack", value: 52 } ]
  }

loadedModel :: Pokedex.Model
loadedModel = { list: Success summaries, search: "", selected: Nothing, detail: Idle }

storyTests :: Array Test
storyTests =
  [ test "loading the list, filtering it, and loading a detail" do
      assertRight "pokedex story"
        ( Story.runStory Pokedex.update Pokedex.initialModel do
            Story.send (Pokedex.LoadedList summaries)
            Story.expectModel "the list is loaded" \model -> case model.list of
              Success values -> Array.length values == 2
              _ -> false
            Story.send (Pokedex.UpdatedSearch "char")
            Story.expectModel "the search is stored" \model -> model.search == "char"
            Story.send (Pokedex.SelectedPokemon "charmander")
            Story.expectCommands [ Command.byDefinition Pokedex.fetchPokemonDetail ]
            Story.resolve Pokedex.fetchPokemonDetail (Right samplePokemon)
            Story.expectModel "the detail is loaded" \model -> case model.detail of
              Success pokemon -> pokemon.name == "charmander"
              _ -> false
            Story.expectNone
        )
  , test "a failed list load reaches Failure" do
      assertRight "pokedex failure story"
        ( Story.runStory Pokedex.update Pokedex.initialModel do
            Story.send (Pokedex.FailedList "offline")
            Story.expectModel "the list carries the error" \model -> case model.list of
              Failure error -> error == "offline"
              _ -> false
            Story.expectNone
        )
  ]

sceneTests :: Array Test
sceneTests =
  [ test "the grid filters and a card loads the detail panel" do
      assertRight "pokedex scene"
        ( Scene.runScene Pokedex.update Pokedex.view loadedModel do
            Scene.expectText (Query.text "Bulbasaur") "Bulbasaur"
            Scene.typeText (Query.label "Search Pokémon") "char"
            Scene.expectText (Query.text "Charmander") "Charmander"
            Scene.expectAbsent (Query.text "Bulbasaur")
            Scene.click (Query.namedRole "button" "Charmander")
            Scene.expectCommands [ Command.byDefinition Pokedex.fetchPokemonDetail ]
            Scene.resolve Pokedex.fetchPokemonDetail (Right samplePokemon)
            Scene.expectText (Query.namedRole "heading" "Charmander") "Charmander"
            Scene.expectTextContaining (Query.role "article") "Fire"
            Scene.expectTextContaining (Query.role "article") "39"
            Scene.expectNone
        )
  , test "an empty search shows the status message" do
      assertRight "pokedex empty search"
        ( Scene.runScene Pokedex.update Pokedex.view loadedModel do
            Scene.typeText (Query.label "Search Pokémon") "zzz"
            Scene.expectTextContaining (Query.role "status") "No Pokémon match"
            Scene.expectNone
        )
  , test "a failed detail request renders an alert" do
      assertRight "pokedex detail failure"
        ( Scene.runScene Pokedex.update Pokedex.view loadedModel do
            Scene.click (Query.namedRole "button" "Bulbasaur")
            Scene.resolve Pokedex.fetchPokemonDetail (Left (Http.StatusError 500 "Server Error" ""))
            Scene.expectTextContaining (Query.role "alert") "HTTP 500"
            Scene.expectNone
        )
  ]
