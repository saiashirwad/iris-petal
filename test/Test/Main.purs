module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Foldkit.Test (runTests, suite)
import Test.Counter as Counter
import Test.Parent as Parent
import Test.Render as Render
import Test.Weather.Scene as WeatherScene
import Test.Weather.Story as WeatherStory

main :: Effect Unit
main =
  launchAff_
    ( runTests
        ( suite "Counter" (Counter.storyTests <> Counter.sceneTests)
            <> suite "Parent" (Parent.storyTests <> Parent.sceneTests)
            <> suite "Weather story" WeatherStory.storyTests
            <> suite "Weather scene" WeatherScene.sceneTests
            <> suite "Render" Render.renderTests
        )
    )
