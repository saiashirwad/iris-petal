module Test.Pokedex where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Example.Pokedex as Pokedex
import Foldkit.AsyncData (AsyncData(..))
import Foldkit.Command as Command
import Foldkit.Http as Http
import Foldkit.Locator as Locator
import Foldkit.Pagination as Pagination
import Foldkit.Query as Query
import Foldkit.Scene as Scene
import Foldkit.Story as Story
import Foldkit.Test (Test, assertRight, test)

summaries :: Array Pokedex.PokemonSummary
summaries =
  [ { name: "bulbasaur", url: "https://pokeapi.co/api/v2/pokemon/1/" }
  , { name: "charmander", url: "https://pokeapi.co/api/v2/pokemon/4/" }
  ]

moreSummaries :: Array Pokedex.PokemonSummary
moreSummaries =
  [ { name: "squirtle", url: "https://pokeapi.co/api/v2/pokemon/7/" }
  , { name: "pikachu", url: "https://pokeapi.co/api/v2/pokemon/25/" }
  ]

sampleCharmander :: Pokedex.Pokemon
sampleCharmander =
  { id: 4
  , name: "charmander"
  , height: 6
  , weight: 85
  , types: [ "fire" ]
  , sprite: Just "https://example.test/charmander.png"
  , stats: [ { name: "hp", value: 39 }, { name: "attack", value: 52 } ]
  }

sampleBulbasaur :: Pokedex.Pokemon
sampleBulbasaur =
  { id: 1
  , name: "bulbasaur"
  , height: 7
  , weight: 69
  , types: [ "grass", "poison" ]
  , sprite: Just "https://example.test/bulbasaur.png"
  , stats: [ { name: "hp", value: 45 } ]
  }

pageCommand ::
  Command.Definition
    { cursor :: Maybe Int, generation :: Int }
    (Pagination.Message Pokedex.PokemonSummary Int Http.HttpError)
    Pokedex.Message
pageCommand = Pagination.liftedCommand Pokedex.listConfig Pokedex.fetchPokemonPage

queryCommand ::
  Command.Definition
    { key :: String, generation :: Int }
    (Query.Message String Pokedex.Pokemon Http.HttpError)
    Pokedex.Message
queryCommand = Query.liftedCommand Pokedex.detailConfig Pokedex.fetchPokemonDetail

hasCharmander :: Pokedex.Model -> Boolean
hasCharmander model = case Query.read model.details "charmander" of
  Success pokemon -> pokemon.name == "charmander"
  _ -> false

hasCharmanderFailure :: Pokedex.Model -> Boolean
hasCharmanderFailure model = case Query.read model.details "charmander" of
  Failure _ -> true
  _ -> false

hasPageFailure :: Pokedex.Model -> Boolean
hasPageFailure model = case Pagination.status model.list of
  Failure _ -> true
  _ -> false

loadedModel :: Pokedex.Model
loadedModel =
  { list: Pagination.fromItems summaries, search: "", selected: Nothing, details: Query.init }

storyTests :: Array Test
storyTests =
  [ test "a page appends and the next cursor loads more" do
      assertRight "pokedex pagination story"
        ( Story.runStory Pokedex.update Pokedex.initialModel do
            Story.send Pokedex.LoadMorePokemon
            Story.expectModel "the generation advances" \model -> model.list.generation == 1
            Story.expectCommands [ Command.byDefinition pageCommand ]
            Story.resolve pageCommand
              (Pagination.CompletedFetch 1 (Right { items: summaries, next: Just 24 }))
            Story.expectModel "the first page is loaded" \model ->
              Array.length (Pagination.items model.list) == 2
            Story.send Pokedex.LoadMorePokemon
            Story.expectCommands [ Command.byDefinition pageCommand ]
            Story.resolve pageCommand
              (Pagination.CompletedFetch 2 (Right { items: moreSummaries, next: Nothing }))
            Story.expectModel "the second page is appended" \model ->
              Array.length (Pagination.items model.list) == 4
            Story.expectNone
        )
  , test "loading the list, filtering it, and loading a detail" do
      assertRight "pokedex story"
        ( Story.runStory Pokedex.update Pokedex.initialModel do
            Story.send Pokedex.LoadMorePokemon
            Story.resolve pageCommand
              (Pagination.CompletedFetch 1 (Right { items: summaries, next: Nothing }))
            Story.expectModel "the list is loaded" \model ->
              Array.length (Pagination.items model.list) == 2
            Story.send (Pokedex.UpdatedSearch "char")
            Story.expectModel "the search is stored" \model -> model.search == "char"
            Story.send (Pokedex.SelectedPokemon "charmander")
            Story.expectCommands [ Command.byDefinition queryCommand ]
            Story.resolve queryCommand
              (Query.CompletedFetch "charmander" 1 (Right sampleCharmander))
            Story.expectModel "the detail is cached" hasCharmander
            Story.expectNone
        )
  , test "selecting a cached detail does not fetch again" do
      assertRight "pokedex cache story"
        ( Story.runStory Pokedex.update Pokedex.initialModel do
            Story.send (Pokedex.SelectedPokemon "charmander")
            Story.resolve queryCommand
              (Query.CompletedFetch "charmander" 1 (Right sampleCharmander))
            Story.send (Pokedex.SelectedPokemon "charmander")
            Story.expectNone
            Story.expectModel "the detail is still cached" hasCharmander
        )
  , test "a failed entry retries on the next selection" do
      assertRight "pokedex retry story"
        ( Story.runStory Pokedex.update Pokedex.initialModel do
            Story.send (Pokedex.SelectedPokemon "charmander")
            Story.resolve queryCommand
              (Query.CompletedFetch "charmander" 1 (Left (Http.StatusError 500 "Server Error" "")))
            Story.expectModel "the failure is cached" hasCharmanderFailure
            Story.send (Pokedex.SelectedPokemon "charmander")
            Story.expectCommands [ Command.byDefinition queryCommand ]
            Story.resolve queryCommand
              (Query.CompletedFetch "charmander" 2 (Right sampleCharmander))
            Story.expectModel "the retry succeeds" hasCharmander
            Story.expectNone
        )
  , test "a failed page keeps the error and can retry" do
      assertRight "pokedex page failure"
        ( Story.runStory Pokedex.update Pokedex.initialModel do
            Story.send Pokedex.LoadMorePokemon
            Story.resolve pageCommand
              (Pagination.CompletedFetch 1 (Left (Http.NetworkError "offline")))
            Story.expectModel "the page error is stored" hasPageFailure
            Story.send Pokedex.LoadMorePokemon
            Story.expectCommands [ Command.byDefinition pageCommand ]
            Story.resolve pageCommand
              (Pagination.CompletedFetch 2 (Right { items: summaries, next: Nothing }))
            Story.expectModel "the retry loads items" \model ->
              Array.length (Pagination.items model.list) == 2
            Story.expectNone
        )
  ]

sceneTests :: Array Test
sceneTests =
  [ test "the grid filters and a card loads the detail panel" do
      assertRight "pokedex scene"
        ( Scene.runScene Pokedex.update Pokedex.view loadedModel do
            Scene.expectText (Locator.text "Bulbasaur") "Bulbasaur"
            Scene.typeText (Locator.label "Search Pokémon") "char"
            Scene.expectText (Locator.text "Charmander") "Charmander"
            Scene.expectAbsent (Locator.text "Bulbasaur")
            Scene.click (Locator.namedRole "button" "Charmander")
            Scene.expectCommands [ Command.byDefinition queryCommand ]
            Scene.resolve queryCommand
              (Query.CompletedFetch "charmander" 1 (Right sampleCharmander))
            Scene.expectText (Locator.namedRole "heading" "Charmander") "Charmander"
            Scene.expectTextContaining (Locator.role "article") "Fire"
            Scene.expectTextContaining (Locator.role "article") "39"
            Scene.expectNone
        )
  , test "re-selecting a cached card does not fetch or lose the detail" do
      assertRight "pokedex cached scene"
        ( Scene.runScene Pokedex.update Pokedex.view loadedModel do
            Scene.click (Locator.namedRole "button" "Charmander")
            Scene.resolve queryCommand
              (Query.CompletedFetch "charmander" 1 (Right sampleCharmander))
            Scene.expectText (Locator.namedRole "heading" "Charmander") "Charmander"
            Scene.click (Locator.namedRole "button" "Charmander")
            Scene.expectNone
            Scene.expectText (Locator.namedRole "heading" "Charmander") "Charmander"
            Scene.expectTextContaining (Locator.role "article") "Fire"
        )
  , test "an empty search shows the status message" do
      assertRight "pokedex empty search"
        ( Scene.runScene Pokedex.update Pokedex.view loadedModel do
            Scene.typeText (Locator.label "Search Pokémon") "zzz"
            Scene.expectTextContaining (Locator.role "status") "No Pokémon match"
            Scene.expectNone
        )
  , test "a failed detail request renders an alert and retries" do
      assertRight "pokedex detail failure"
        ( Scene.runScene Pokedex.update Pokedex.view loadedModel do
            Scene.click (Locator.namedRole "button" "Bulbasaur")
            Scene.resolve queryCommand
              (Query.CompletedFetch "bulbasaur" 1 (Left (Http.StatusError 500 "Server Error" "")))
            Scene.expectTextContaining (Locator.role "alert") "HTTP 500"
            Scene.click (Locator.namedRole "button" "Bulbasaur")
            Scene.expectCommands [ Command.byDefinition queryCommand ]
            Scene.resolve queryCommand (Query.CompletedFetch "bulbasaur" 2 (Right sampleBulbasaur))
            Scene.expectText (Locator.namedRole "heading" "Bulbasaur") "Bulbasaur"
            Scene.expectTextContaining (Locator.role "article") "Grass"
            Scene.expectNone
        )
  ]
