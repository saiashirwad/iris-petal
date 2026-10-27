module Example.Pokedex
  ( Model
  , Message(..)
  , Pokemon
  , PokemonSummary
  , Stat
  , program
  , update
  , view
  , fetchPokemonPage
  , fetchPokemonDetail
  )
  where

import Prelude

import Control.Monad.State.Class (modify_)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, (.:), (.:?))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as Str
import Data.String.CodeUnits as StringCode
import Data.Traversable (traverse)
import Petal (Program, Update, zoom)
import Petal.AsyncData (AsyncData(..), isLoading, toMaybe)
import Petal.Command (Definition, define)
import Petal.Html
  ( Html
  , Role(..)
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
  , key
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
import Petal.Http (HttpError)
import Petal.Http as Http
import Petal.Pagination (Page, Status(..))
import Petal.Pagination as Pagination
import Petal.Query as Query

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

type Model =
  { list :: Pagination.Model Int HttpError PokemonSummary
  , search :: String
  , selected :: Maybe String
  , details :: Query.Model String HttpError Pokemon
  }

data Message
  = UpdatedSearch String
  | SelectedPokemon String
  | ReachedListEnd
  | GotPage (Pagination.Message Int HttpError PokemonSummary)
  | GotDetail (Query.Message String HttpError Pokemon)

program :: Program Model Message
program =
  { model: { list: Pagination.init, search: "", selected: Nothing, details: Query.init }
  , init: loadMore
  , update
  , view
  }

update :: Message -> Update Model Message Unit
update = case _ of
  UpdatedSearch search -> modify_ _ { search = search }
  SelectedPokemon name -> do
    modify_ _ { selected = Just name }
    zoom @"details" GotDetail (Query.ensure fetchPokemonDetail name)
  ReachedListEnd -> loadMore
  GotPage message -> zoom @"list" GotPage (Pagination.update message)
  GotDetail message -> zoom @"details" GotDetail (Query.update message)

loadMore :: Update Model Message Unit
loadMore = zoom @"list" GotPage (Pagination.loadMore fetchPokemonPage)

fetchPokemonPage :: Definition (Maybe Int) (Either HttpError (Page Int PokemonSummary))
fetchPokemonPage = define "FetchPokemonPage" \cursor -> do
  let
    offset = fromMaybe 0 cursor
    limit = min pageSize (maxPokemon - offset)
    next = if offset + limit >= maxPokemon then Nothing else Just (offset + limit)
  result <- Http.sendJson decodeList (Http.get (listUrl limit offset))
  pure (map { items: _, next } result)

fetchPokemonDetail :: Definition String (Either HttpError Pokemon)
fetchPokemonDetail = define "FetchPokemonDetail" \name ->
  Http.sendJson decodePokemon (Http.get ("https://pokeapi.co/api/v2/pokemon/" <> name))

pageSize :: Int
pageSize = 24

maxPokemon :: Int
maxPokemon = 151

listUrl :: Int -> Int -> String
listUrl limit offset =
  "https://pokeapi.co/api/v2/pokemon?limit=" <> show limit <> "&offset=" <> show offset

decodeList :: Json -> Either JsonDecodeError (Array PokemonSummary)
decodeList json = do
  page <- decodeJson json
  page .: "results"

decodePokemon :: Json -> Either JsonDecodeError Pokemon
decodePokemon json = do
  object <- decodeJson json
  id <- object .: "id"
  name <- object .: "name"
  height <- object .: "height"
  weight <- object .: "weight"
  typeSlots :: Array { type :: { name :: String } } <- object .: "types"
  statSlots :: Array { base_stat :: Int, stat :: { name :: String } } <- object .: "stats"
  sprites <- object .: "sprites"
  sprite <- sprites .:? "front_default"
  pure
    { id
    , name
    , height
    , weight
    , types: map _.type.name typeSlots
    , sprite
    , stats: map (\slot -> { name: slot.stat.name, value: slot.base_stat }) statSlots
    }

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
      [ section [ class_ "list-panel", ariaBusy (Pagination.status model.list == LoadingMore) ]
          [ viewList model ]
      , detailPanel model
      ]
  ]

viewList :: Model -> Html Message
viewList model = case Pagination.status model.list, Pagination.items model.list of
  LoadFailed error, [] -> p [ role_ Alert, class_ "error" ]
    [ text ("Could not load Pokédex: " <> Http.describeError error) ]
  _, [] -> listSkeleton
  status, summaries ->
    let
      visible = filterSummaries model.search summaries
    in
      div [ class_ "list-stack" ]
        [ if Array.null visible
          then p [ role_ Status, class_ "status" ] [ text "No Pokémon match that search." ]
          else div [ class_ "grid" ] (map (viewCard model.selected) visible)
        , viewListEnd status (Array.length summaries)
        ]

viewListEnd :: Status HttpError -> Int -> Html Message
viewListEnd status loaded = case status of
  HasMore -> div [ class_ "load-more", onVisible ReachedListEnd ] [ text "Scroll for more" ]
  LoadingMore -> div [ class_ "load-more" ] [ span [ class_ "skeleton skeleton-load" ] [] ]
  LoadFailed error -> div [ class_ "load-more" ]
    [ p [ role_ Alert, class_ "error" ]
        [ text ("Could not load more: " <> Http.describeError error) ]
    , button [ type_ "button", onClick ReachedListEnd ] [ text "Try again" ]
    ]
  AllLoaded -> p [ class_ "status" ] [ text ("All " <> show loaded <> " Pokémon loaded.") ]

listSkeleton :: Html Message
listSkeleton = div [ class_ "grid", role_ Status, ariaLabel "Loading Pokédex" ]
  ( Array.replicate pageSize
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
  [ key summary.name
  , class_ (if selected == Just summary.name then "card selected" else "card")
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
  aside [ class_ "detail-panel", ariaBusy (isLoading detail) ] case model.selected of
    Nothing -> [ p [ class_ "hint" ] [ text "Select a Pokémon to see its details." ] ]
    Just name -> [ viewDetail name detail ]
  where
  detail = case model.selected of
    Nothing -> NotAsked
    Just name -> Query.lookup name model.details

viewDetail :: String -> AsyncData HttpError Pokemon -> Html Message
viewDetail name detail = case detail of
  Failure error _ -> p [ role_ Alert, class_ "error" ]
    [ text ("Could not load " <> capitalize name <> ": " <> Http.describeError error) ]
  _ -> case toMaybe detail of
    Nothing -> detailSkeleton name
    Just pokemon -> viewPokemon pokemon

detailSkeleton :: String -> Html Message
detailSkeleton name = article
  [ class_ "pokemon skeleton-card", role_ Status, ariaLabel ("Loading " <> capitalize name) ]
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
