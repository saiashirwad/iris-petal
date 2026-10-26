module Example.Weather where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Foldkit.AsyncData (AsyncData(..), isPending)
import Foldkit.Command (CommandName(..), Definition, command, define)
import Foldkit.Core (Update, continue, withCommands)
import Foldkit.Html
  ( Html
  , article
  , button
  , class_
  , disabled_
  , div
  , empty
  , form
  , h1
  , h2
  , id_
  , input
  , label
  , onInput
  , onSubmit
  , p
  , placeholder_
  , role_
  , text
  , type_
  , value_
  , for_
  )

type Weather =
  { zipCode :: String
  , locationName :: String
  , region :: String
  , temperature :: Int
  , description :: String
  , humidity :: Int
  , windSpeed :: Int
  }

data Message
  = UpdatedZipCode String
  | SubmittedWeatherForm
  | SucceededFetchWeather Weather
  | FailedFetchWeather String

derive instance eqMessage :: Eq Message

instance showMessage :: Show Message where
  show = case _ of
    UpdatedZipCode value -> "UpdatedZipCode " <> show value
    SubmittedWeatherForm -> "SubmittedWeatherForm"
    SucceededFetchWeather weather -> "SucceededFetchWeather " <> show weather
    FailedFetchWeather error -> "FailedFetchWeather " <> show error

type Model = { zipCode :: String, weather :: AsyncData Weather String }

initialModel :: Model
initialModel = { zipCode: "", weather: Idle }

sampleWeather :: String -> Weather
sampleWeather zipCode =
  { zipCode
  , locationName: "Beverly Hills"
  , region: "California"
  , temperature: 72
  , description: "Clear sky"
  , humidity: 45
  , windSpeed: 10
  }

type FetchWeatherArgs = { zipCode :: String }

data FetchWeatherResult = FetchWeatherSucceeded Weather | FetchWeatherFailed String

derive instance eqFetchWeatherResult :: Eq FetchWeatherResult

instance showFetchWeatherResult :: Show FetchWeatherResult where
  show = case _ of
    FetchWeatherSucceeded weather -> "FetchWeatherSucceeded " <> show weather
    FetchWeatherFailed error -> "FetchWeatherFailed " <> show error

fetchWeather :: Definition FetchWeatherArgs FetchWeatherResult Message
fetchWeather = define (CommandName "FetchWeather")
  (\args -> pure (FetchWeatherSucceeded (sampleWeather args.zipCode)))
  ( case _ of
      FetchWeatherSucceeded weather -> SucceededFetchWeather weather
      FetchWeatherFailed error -> FailedFetchWeather error
  )

update :: Model -> Message -> Update Model Message Void
update model = case _ of
  UpdatedZipCode value -> continue model { zipCode = value }
  SubmittedWeatherForm ->
    if isPending model.weather
    then continue model
    else
      withCommands (model { weather = Loading }) [ command fetchWeather { zipCode: model.zipCode } ]
  SucceededFetchWeather weather -> continue model { weather = Success weather }
  FailedFetchWeather error -> continue model { weather = Failure error }

view :: Model -> Html Message
view model = div [ class_ "weather" ]
  [ h1 [] [ text "Weather" ]
  , form [ onSubmit SubmittedWeatherForm ]
      [ label [ for_ "zip" ] [ text "Zip code" ]
      , input
          [ id_ "zip"
          , type_ "text"
          , value_ model.zipCode
          , placeholder_ "Enter a zip code"
          , onInput UpdatedZipCode
          ]
      , button [ type_ "submit", disabled_ (isPending model.weather) ]
          [ text (if isPending model.weather then "Loading..." else "Get Weather") ]
      ]
  , weatherBody model.weather
  ]

weatherBody :: AsyncData Weather String -> Html Message
weatherBody = case _ of
  Idle -> empty
  Loading -> p [] [ text "Fetching weather..." ]
  Success weather -> weatherCard weather
  Failure error -> p [ role_ "alert" ] [ text error ]
  Refreshing _ -> p [] [ text "Refreshing..." ]
  Stale weather _ -> weatherCard weather

weatherCard :: Weather -> Html Message
weatherCard weather = article [ class_ "weather-card" ]
  [ h2 [] [ text weather.zipCode ]
  , p [] [ text (weather.locationName <> ", " <> weather.region) ]
  , p [] [ text (show weather.temperature <> "°F") ]
  , p [] [ text weather.description ]
  , p [] [ text (show weather.humidity <> "%") ]
  , p [] [ text (show weather.windSpeed <> " mph") ]
  ]
