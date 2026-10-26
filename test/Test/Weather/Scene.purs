module Test.Weather.Scene where

import Prelude

import Data.Either (Either(..))
import Data.String as Str
import Effect.Aff (Aff)
import Example.Weather as Weather
import Foldkit.Command as Command
import Foldkit.Query as Query
import Foldkit.Scene as Scene
import Foldkit.Test (Test, assertRight, assertTrue, test)

sceneTests :: Array Test
sceneTests =
  [ test "the initial view shows an empty form" do
      assertRight "initial weather scene"
        ( Scene.runScene Weather.update Weather.view Weather.initialModel do
            Scene.expectExists (Query.label "Zip code")
            Scene.expectExists (Query.namedRole "button" "Get Weather")
            Scene.expectAbsent (Query.role "article")
            Scene.expectNone
        )
  , test "typing updates the controlled input value" do
      assertRight "typing scene"
        ( Scene.runScene Weather.update Weather.view Weather.initialModel do
            Scene.typeText (Query.label "Zip code") "90210"
            Scene.expectValue (Query.label "Zip code") "90210"
            Scene.expectNone
        )
  , test "submitting shows the loading state and exact Command" do
      assertRight "submitting scene"
        ( Scene.runScene Weather.update Weather.view Weather.initialModel do
            Scene.typeText (Query.label "Zip code") "90210"
            Scene.submit (Query.role "form")
            Scene.expectExists (Query.namedRole "button" "Loading...")
            Scene.expectCommands
              [ Command.command Weather.fetchWeather { zipCode: "90210" } ]
            Scene.resolve
              Weather.fetchWeather
              (Weather.FetchWeatherSucceeded (Weather.sampleWeather "90210"))
        )
  , test "resolving the fetch renders the weather card" do
      assertRight "weather success scene"
        ( Scene.runScene Weather.update Weather.view Weather.initialModel do
            Scene.typeText (Query.label "Zip code") "90210"
            Scene.submit (Query.role "form")
            Scene.resolve
              Weather.fetchWeather
              (Weather.FetchWeatherSucceeded (Weather.sampleWeather "90210"))
            Scene.expectExists (Query.role "article")
            Scene.within (Query.role "article") do
              Scene.expectExists (Query.namedRole "heading" "90210")
              Scene.expectText
                (Query.text "Beverly Hills, California")
                "Beverly Hills, California"
              Scene.expectText (Query.text "72°F") "72°F"
              Scene.expectTextContaining (Query.role "article") "Clear sky"
              Scene.expectTextContaining (Query.role "article") "45%"
              Scene.expectTextContaining (Query.role "article") "10 mph"
            Scene.expectNone
        )
  , test "clicking a submit button follows the form submit handler" do
      assertRight "click submit scene"
        ( Scene.runScene Weather.update Weather.view Weather.initialModel do
            Scene.typeText (Query.label "Zip code") "90210"
            Scene.click (Query.namedRole "button" "Get Weather")
            Scene.expectExists (Query.namedRole "button" "Loading...")
            Scene.expectCommands
              [ Command.command Weather.fetchWeather { zipCode: "90210" } ]
            Scene.resolve
              Weather.fetchWeather
              (Weather.FetchWeatherSucceeded (Weather.sampleWeather "90210"))
            Scene.expectExists (Query.role "article")
        )
  , test "a failed fetch renders an alert instead of the card" do
      assertRight "weather failure scene"
        ( Scene.runScene Weather.update Weather.view Weather.initialModel do
            Scene.typeText (Query.label "Zip code") "00000"
            Scene.submit (Query.role "form")
            Scene.resolve
              Weather.fetchWeather
              (Weather.FetchWeatherFailed "Location not found")
            Scene.expectAbsent (Query.role "article")
            Scene.expectTextContaining (Query.role "alert") "Location not found"
            Scene.expectNone
        )
  , test "a missing element explains which locator failed" do
      case
        Scene.runScene Weather.update Weather.view Weather.initialModel do
          Scene.expectExists (Query.namedRole "button" "Nope")
        of
        Left errorMessage ->
          assertTrue
            "message names the locator"
            (Str.contains (Str.Pattern "Nope") errorMessage)
        Right _ ->
          assertTrue "expected the scene to fail" false
    ]
