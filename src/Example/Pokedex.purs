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
  , listConfig
  , detailConfig
  , fetchPokemonPage
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
import Foldkit.Command (CommandName(..))
import Foldkit.Core (Program, Update, continue)
import Foldkit.Html
  ( Html
  , alt_
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
  , loading_
  , main
  , onClick
  , onInput
  , onVisible
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
  )
import Foldkit.Http as Http
import Foldkit.Pagination as Pagination
import Foldkit.Query as Query

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
  = UpdatedSearch String
  | SelectedPokemon String
  | LoadMorePokemon
  | GotPokemonPage (Pagination.Message PokemonSummary Int Http.HttpError)
  | GotPokemonQuery (Query.Message String Pokemon Http.HttpError)

type Model =
  { list :: Pagination.Model PokemonSummary Int Http.HttpError
  , search :: String
  , selected :: Maybe String
  , details :: Query.Model String Pokemon Http.HttpError
  }

initialModel :: Model
initialModel = { list: Pagination.init, search: "", selected: Nothing, details: Query.init }

pageSize :: Int
pageSize = 24

maxPokemon :: Int
maxPokemon = 151

listUrl :: Int -> Int -> String
listUrl limit offset =
  "https://pokeapi.co/api/v2/pokemon?limit=" <> show limit <> "&offset=" <> show offset

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
  result <- fetchJson (listUrl maxPokemon 0)
  pure do
    json <- result
    lmap Http.DecodeError (decodeList json)

fetchPokemon :: String -> Aff (Either Http.HttpError Pokemon)
fetchPokemon name = do
  result <- fetchJson (detailUrl name)
  pure do
    json <- result
    lmap Http.DecodeError (decodePokemon json)

fetchPokemonPage :: Pagination.Definition Int PokemonSummary Http.HttpError
fetchPokemonPage = Pagination.define (CommandName "FetchPokemonPage") \maybeOffset -> do
  let
    offset = fromMaybe 0 maybeOffset
    remaining = maxPokemon - offset
    limit = min pageSize remaining
  if remaining <= 0
  then pure (Right { items: [], next: Nothing })
  else do
    result <- fetchJson (listUrl limit offset)
    pure do
      json <- result
      summaries <- lmap Http.DecodeError (decodeList json)
      let
        next = if offset + pageSize >= maxPokemon then Nothing else Just (offset + pageSize)
      pure { items: summaries, next }

fetchPokemonDetail :: Query.Definition String Pokemon Http.HttpError
fetchPokemonDetail = Query.define (CommandName "FetchPokemonDetail") fetchPokemon

listConfig :: Pagination.Config Model Message PokemonSummary Int Http.HttpError
listConfig =
  { read: _.list, write: \model list -> model { list = list }, toParentMessage: GotPokemonPage }

detailConfig :: Query.Config Model Message String Pokemon Http.HttpError
detailConfig =
  { read: _.details
  , write: \model details -> model { details = details }
  , toParentMessage: GotPokemonQuery
  }

start :: Unit -> Update Model Message Void
start _ = Pagination.loadMore listConfig fetchPokemonPage initialModel

update :: Model -> Message -> Update Model Message Void
update model = case _ of
  UpdatedSearch value -> continue model { search = value }
  SelectedPokemon name ->
    Query.loadIfMissing detailConfig fetchPokemonDetail name model { selected = Just name }
  LoadMorePokemon -> Pagination.loadMore listConfig fetchPokemonPage model
  GotPokemonPage pageMessage -> Pagination.receive listConfig pageMessage model
  GotPokemonQuery queryMessage -> Query.receive detailConfig queryMessage model

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
      [ section [ class_ "list-panel", ariaBusy (isPending (Pagination.status model.list)) ]
          [ viewList model.list model.search model.selected ]
      , detailPanel model
      ]
  ]

viewList ::
  Pagination.Model PokemonSummary Int Http.HttpError -> String -> Maybe String -> Html Message
viewList list search selected = case Pagination.status list of
  Failure error -> p [ role_ "alert", class_ "error" ]
    [ text ("Could not load Pokédex: " <> Http.describeError error) ]
  _ -> case Array.uncons (Pagination.items list) of
    Nothing -> listSkeleton
    Just _ ->
      let
        visible = filterSummaries search (Pagination.items list)
      in
        div [ class_ "list-stack" ]
          [ if Array.null visible
            then p [ role_ "status", class_ "status" ] [ text "No Pokémon match that search." ]
            else div [ class_ "grid" ] (map (viewCard selected) visible)
          , viewLoadMore list
          ]

viewLoadMore :: Pagination.Model PokemonSummary Int Http.HttpError -> Html Message
viewLoadMore list =
  if not (Pagination.hasMore list)
  then p [ class_ "status" ]
    [ text ("All " <> show (Array.length (Pagination.items list)) <> " Pokémon loaded.") ]
  else if isPending (Pagination.status list)
  then div [ class_ "load-more" ] [ span [ class_ "skeleton skeleton-load" ] [] ]
  else div [ class_ "load-more", onVisible LoadMorePokemon ] [ text "Scroll for more" ]

listSkeleton :: Html Message
listSkeleton = div [ class_ "grid", role_ "status", ariaLabel "Loading Pokédex" ]
  ( Array.replicate 24
      ( div [ class_ "card skeleton-card" ]
          [ div [ class_ "skeleton skeleton-sprite" ] []
          , div [ class_ "skeleton skeleton-name" ] []
          ]
      )
  )

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
  [ img
      [ src_ (spriteUrl (pokemonIdFromUrl summary.url))
      , alt_ summary.name
      , loading_ "lazy"
      , class_ "sprite"
      ]
  , span [ class_ "card-name" ] [ text (capitalize summary.name) ]
  ]

detailPanel :: Model -> Html Message
detailPanel model =
  aside [ class_ "detail-panel", ariaBusy (isPending selectedDetail) ] case model.selected of
    Nothing -> [ p [ class_ "hint" ] [ text "Select a Pokémon to see its details." ] ]
    Just name -> [ viewDetail name selectedDetail ]
  where
  selectedDetail = case model.selected of
    Nothing -> Idle
    Just name -> Query.read model.details name

viewDetail :: String -> AsyncData Pokemon Http.HttpError -> Html Message
viewDetail name detail = case detail of
  Failure error -> p [ role_ "alert", class_ "error" ]
    [ text ("Could not load " <> capitalize name <> ": " <> Http.describeError error) ]
  _ -> case getData detail of
    Nothing -> detailSkeleton name
    Just pokemon -> viewPokemon pokemon

detailSkeleton :: String -> Html Message
detailSkeleton name = article
  [ class_ "pokemon skeleton-card", role_ "status", ariaLabel ("Loading " <> capitalize name) ]
  [ header [ class_ "pokemon-header" ]
      [ div [ class_ "skeleton skeleton-title" ] [], div [ class_ "skeleton skeleton-number" ] [] ]
  , div [ class_ "skeleton skeleton-image" ] []
  , div [ class_ "types" ]
      [ span [ class_ "skeleton skeleton-badge" ] [], span [ class_ "skeleton skeleton-badge" ] [] ]
  , dl [ class_ "facts" ]
      [ dt [] [ text "Height" ]
      , dd [] [ div [ class_ "skeleton skeleton-line" ] [] ]
      , dt [] [ text "Weight" ]
      , dd [] [ div [ class_ "skeleton skeleton-line" ] [] ]
      ]
  , h3 [] [ text "Base stats" ]
  , ul [ class_ "stats" ] (Array.replicate 4 (li [] [ div [ class_ "skeleton skeleton-stat" ] [] ]))
  ]

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
