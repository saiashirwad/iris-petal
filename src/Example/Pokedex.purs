module Example.Pokedex
  ( Model
  , Message(..)
  , Pokemon
  , PokemonSummary
  , Stat
  , initialModel
  , start
  , update
  , view
  , program
  , fetchPokemonList
  , fetchPokemonDetail
  , fetchList
  , fetchPokemon
  )
  where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?), printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as Str
import Data.String.CodeUnits as StringCode
import Data.Traversable (traverse)
import Data.Void (Void)
import Effect.Aff (Aff)
import Foldkit.AsyncData (AsyncData(..), getData, isPending)
import Foldkit.Command (CommandName(..), Definition, command, command0, define, define0)
import Foldkit.Core (Program, Update, continue)
import Foldkit.Html
  ( Html
  , ariaBusy
  , ariaLabel
  , article
  , aside
  , button
  , class_
  , dd
  , div
  , dl
  , dt
  , h1
  , h2
  , h3
  , header
  , id_
  , img
  , input
  , li
  , main
  , onClick
  , onInput
  , p
  , placeholder_
  , role_
  , section
  , span
  , src_
  , text
  , type_
  , ul
  , value_
  , alt_
  )
import Foldkit.Http as Http

type PokemonSummary = { name :: String, url :: String }

type Stat = { name :: String, value :: Int }

type Pokemon =
  { id :: Int
  , name :: String
  , height :: Int
  , weight :: Int
  , types :: Array String
  , sprite :: Maybe String
  , stats :: Array Stat
  }

data Message
  = LoadedList (Array PokemonSummary)
  | FailedList String
  | UpdatedSearch String
  | SelectedPokemon String
  | LoadedPokemon Pokemon
  | FailedPokemon String

type Model =
  { list :: AsyncData (Array PokemonSummary) String
  , search :: String
  , selected :: Maybe String
  , detail :: AsyncData Pokemon String
  }

initialModel :: Model
initialModel = { list: Loading, search: "", selected: Nothing, detail: Idle }

listUrl :: String
listUrl = "https://pokeapi.co/api/v2/pokemon?limit=151"

detailUrl :: String -> String
detailUrl name = "https://pokeapi.co/api/v2/pokemon/" <> name

decodeField :: forall value. DecodeJson value => Json -> String -> Either String value
decodeField json key = do
  object <- lmap printJsonDecodeError (decodeJson json)
  lmap printJsonDecodeError (object .: key)

decodeOptionalField ::
  forall value. DecodeJson value => Json -> String -> Either String (Maybe value)
decodeOptionalField json key = do
  object <- lmap printJsonDecodeError (decodeJson json)
  lmap printJsonDecodeError (object .:? key)

decodeSummary :: Json -> Either String PokemonSummary
decodeSummary json = do
  name <- decodeField json "name"
  url <- decodeField json "url"
  pure { name, url }

decodeList :: Json -> Either String (Array PokemonSummary)
decodeList json = do
  results :: Array Json <- decodeField json "results"
  traverse decodeSummary results

decodeType :: Json -> Either String String
decodeType json = do
  typeJson <- decodeField json "type"
  decodeField typeJson "name"

decodeStat :: Json -> Either String Stat
decodeStat json = do
  value <- decodeField json "base_stat"
  statJson <- decodeField json "stat"
  name <- decodeField statJson "name"
  pure { name, value }

decodePokemon :: Json -> Either String Pokemon
decodePokemon json = do
  id <- decodeField json "id"
  name <- decodeField json "name"
  height <- decodeField json "height"
  weight <- decodeField json "weight"
  typeJson :: Array Json <- decodeField json "types"
  types <- traverse decodeType typeJson
  statJson :: Array Json <- decodeField json "stats"
  stats <- traverse decodeStat statJson
  sprites <- decodeField json "sprites"
  sprite :: Maybe String <- decodeOptionalField sprites "front_default"
  pure { id, name, height, weight, types, sprite, stats }

fetchJson :: String -> Aff (Either Http.HttpError Json)
fetchJson url = do
  result <- Http.send Http.browserClient (Http.get url)
  pure do
    response <- result
    lmap Http.DecodeError (jsonParser response.body)

fetchList :: Aff (Either Http.HttpError (Array PokemonSummary))
fetchList = do
  result <- fetchJson listUrl
  pure do
    json <- result
    lmap Http.DecodeError (decodeList json)

fetchPokemon :: String -> Aff (Either Http.HttpError Pokemon)
fetchPokemon name = do
  result <- fetchJson (detailUrl name)
  pure do
    json <- result
    lmap Http.DecodeError (decodePokemon json)

fetchPokemonList :: Definition Unit (Either Http.HttpError (Array PokemonSummary)) Message
fetchPokemonList = define0 (CommandName "FetchPokemonList") fetchList case _ of
  Left error -> FailedList (Http.describeError error)
  Right summaries -> LoadedList summaries

fetchPokemonDetail :: Definition String (Either Http.HttpError Pokemon) Message
fetchPokemonDetail = define (CommandName "FetchPokemonDetail") fetchPokemon case _ of
  Left error -> FailedPokemon (Http.describeError error)
  Right pokemon -> LoadedPokemon pokemon

start :: Unit -> Update Model Message Void
start _ = { model: initialModel, commands: [ command0 fetchPokemonList ], outMessage: Nothing }

update :: Model -> Message -> Update Model Message Void
update model = case _ of
  LoadedList summaries -> continue model { list = Success summaries }
  FailedList error -> continue model { list = Failure error }
  UpdatedSearch value -> continue model { search = value }
  SelectedPokemon name ->
    { model: model { selected = Just name, detail = Loading }
    , commands: [ command fetchPokemonDetail name ]
    , outMessage: Nothing
    }
  LoadedPokemon pokemon -> continue model { detail = Success pokemon }
  FailedPokemon error -> continue model { detail = Failure error }

program :: Program Model Message Unit Void
program = { name: "Pokédex", start, update, view }

view :: Model -> Html Message
view model = div [ class_ "pokedex" ]
  [ header [ class_ "masthead" ]
      [ h1 [] [ text "Iris Pokédex" ]
      , p [] [ text "The first 151 Pokémon, fetched from PokéAPI." ]
      , input
          [ id_ "search"
          , type_ "search"
          , value_ model.search
          , placeholder_ "Search by name"
          , ariaLabel "Search Pokémon"
          , onInput UpdatedSearch
          ]
      ]
  , main [ class_ "layout" ]
      [ section [ class_ "list-panel", ariaBusy (isPending model.list) ]
          [ viewList model.list model.search model.selected ]
      , detailPanel model
      ]
  ]

viewList :: AsyncData (Array PokemonSummary) String -> String -> Maybe String -> Html Message
viewList list search selected = case list of
  Failure error ->
    p [ role_ "alert", class_ "error" ] [ text ("Could not load Pokédex: " <> error) ]
  _ -> case getData list of
    Nothing -> p [ role_ "status", class_ "status" ] [ text "Loading Pokédex…" ]
    Just summaries ->
      let
        visible = filterSummaries search summaries
      in
        if Array.null visible
        then p [ role_ "status", class_ "status" ] [ text "No Pokémon match that search." ]
        else div [ class_ "grid" ] (map (viewCard selected) visible)

filterSummaries :: String -> Array PokemonSummary -> Array PokemonSummary
filterSummaries search summaries =
  let
    query = Str.toLower (Str.trim search)
  in
    if Str.null query
    then summaries
    else Array.filter (\summary -> Str.contains (Str.Pattern query) (Str.toLower summary.name))
      summaries

viewCard :: Maybe String -> PokemonSummary -> Html Message
viewCard selected summary = button
  [ class_ (if selected == Just summary.name then "card selected" else "card")
  , type_ "button"
  , onClick (SelectedPokemon summary.name)
  ]
  [ img [ src_ (spriteUrl (pokemonIdFromUrl summary.url)), alt_ summary.name, class_ "sprite" ]
  , span [ class_ "card-name" ] [ text (capitalize summary.name) ]
  ]

detailPanel :: Model -> Html Message
detailPanel model = aside [ class_ "detail-panel" ] case model.selected of
  Nothing -> [ p [ class_ "hint" ] [ text "Select a Pokémon to see its details." ] ]
  Just name -> [ viewDetail name model.detail ]

viewDetail :: String -> AsyncData Pokemon String -> Html Message
viewDetail name detail = case detail of
  Failure error -> p [ role_ "alert", class_ "error" ]
    [ text ("Could not load " <> capitalize name <> ": " <> error) ]
  _ -> case getData detail of
    Nothing -> p [ role_ "status", class_ "status" ] [ text ("Loading " <> capitalize name <> "…") ]
    Just pokemon -> viewPokemon pokemon

viewPokemon :: Pokemon -> Html Message
viewPokemon pokemon = article [ class_ "pokemon" ]
  [ header [ class_ "pokemon-header" ]
      [ h2 [] [ text (capitalize pokemon.name) ]
      , span [ class_ "dex-number" ] [ text ("#" <> show pokemon.id) ]
      ]
  , img
      [ src_ (fromMaybe (spriteUrl pokemon.id) pokemon.sprite)
      , alt_ pokemon.name
      , class_ "detail-sprite"
      ]
  , div [ class_ "types" ] (map typeBadge pokemon.types)
  , dl [ class_ "facts" ]
      [ dt [] [ text "Height" ]
      , dd [] [ text (show (pokemon.height * 10) <> " cm") ]
      , dt [] [ text "Weight" ]
      , dd [] [ text (show (pokemon.weight * 100) <> " g") ]
      ]
  , h3 [] [ text "Base stats" ]
  , ul [ class_ "stats" ] (map statRow pokemon.stats)
  ]

typeBadge :: String -> Html Message
typeBadge value = span [ class_ ("type " <> value) ] [ text (capitalize value) ]

statRow :: Stat -> Html Message
statRow stat = li []
  [ span [] [ text (statLabel stat.name) ]
  , span [ class_ "stat-value" ] [ text (show stat.value) ]
  ]

statLabel :: String -> String
statLabel = case _ of
  "hp" -> "HP"
  "special-attack" -> "Sp. Atk"
  "special-defense" -> "Sp. Def"
  value -> capitalize (Str.replaceAll (Str.Pattern "-") (Str.Replacement " ") value)

capitalize :: String -> String
capitalize value = case StringCode.uncons value of
  Nothing -> value
  Just { head, tail } -> Str.toUpper (StringCode.singleton head) <> tail

pokemonIdFromUrl :: String -> Int
pokemonIdFromUrl url =
  case Array.last (Array.filter (not <<< Str.null) (Str.split (Str.Pattern "/") url)) of
    Just segment -> fromMaybe 0 (Int.fromString segment)
    Nothing -> 0

spriteUrl :: Int -> String
spriteUrl id =
  "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/" <> show id <> ".png"
