module Test.Weather.Story where

import Prelude

import Data.Either (Either(..))
import Data.String as Str
import Effect.Aff (Aff)
import Example.Weather as Weather
import Foldkit.AsyncData (AsyncData(..), isPending)
import Foldkit.Command as Command
import Foldkit.Story as Story
import Foldkit.Test (Test, assertRight, assertTrue, test)

storyTests :: Array Test
storyTests =
  [ test "submitting fetches weather and reaches Success" do
      assertRight "weather story"
        ( Story.runStory Weather.update Weather.initialModel do
            Story.send (Weather.UpdatedZipCode "90210")
            Story.expectModel "zip code is in the model" \model ->
              model.zipCode == "90210"
            Story.send Weather.SubmittedWeatherForm
            Story.expectModel "weather is loading" \model ->
              isPending model.weather
            Story.expectCommands
              [ Command.command Weather.fetchWeather { zipCode: "90210" } ]
            Story.resolve
              Weather.fetchWeather
              (Weather.FetchWeatherSucceeded (Weather.sampleWeather "90210"))
            Story.expectModel "weather is a success with the fetched data" \model ->
              case model.weather of
                Success weather ->
                  weather.temperature == 72
                    && weather.locationName == "Beverly Hills"
                    && weather.description == "Clear sky"
                _ -> false
            Story.expectNone
            Story.expectNoOutMessage
        )
  , test "a failed fetch reaches Failure" do
      assertRight "weather failure story"
        ( Story.runStory Weather.update Weather.initialModel do
            Story.send (Weather.UpdatedZipCode "00000")
            Story.send Weather.SubmittedWeatherForm
            Story.resolve Weather.fetchWeather (Weather.FetchWeatherFailed "Location not found")
            Story.expectModel "weather is a failure" \model ->
              case model.weather of
                Failure error -> error == "Location not found"
                _ -> false
            Story.expectNone
        )
  , test "sending a new Message with a pending Command is rejected" do
      case
        Story.runStory Weather.update Weather.initialModel do
          Story.send Weather.SubmittedWeatherForm
          Story.send (Weather.UpdatedZipCode "10001")
        of
        Left errorMessage ->
          assertTrue
            "message explains pending commands"
            ( contains "unresolved Commands" errorMessage
                && contains "FetchWeather" errorMessage
            )
        Right _ ->
          assertTrue "expected the story to fail" false
    ]

contains :: String -> String -> Boolean
contains needle haystack =
  Str.contains (Str.Pattern needle) haystack
