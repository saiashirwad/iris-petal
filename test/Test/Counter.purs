module Test.Counter where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.String as Str
import Example.Counter as Counter
import Foldkit.Query as Query
import Foldkit.Scene as Scene
import Foldkit.Story as Story
import Foldkit.Test (Test, assertRight, assertTrue, test)

storyTests :: Array Test
storyTests =
  [ test "increments, decrements, and resets the model" do
      assertRight "counter story"
        ( Story.runStory Counter.update Counter.initialModel do
            Story.send Counter.ClickedIncrement
            Story.send Counter.ClickedIncrement
            Story.expectModel "count is 2" \model -> model.count == 2
            Story.send Counter.ClickedDecrement
            Story.expectModel "count is 1" \model -> model.count == 1
            Story.send Counter.ClickedReset
            Story.expectModel "count is 0" \model -> model.count == 0
            Story.expectNone
        )
  , test "emits ReachedTen when the count reaches ten" do
      assertRight "counter out message"
        ( Story.runStory Counter.update Counter.initialModel do
            for_ (Array.range 1 10) \_ -> Story.send Counter.ClickedIncrement
            Story.expectOutMessage Counter.ReachedTen
            Story.expectModel "count is ten" \model -> model.count == 10
            Story.expectNone
        )
  ]

sceneTests :: Array Test
sceneTests =
  [ test "renders the counter and clicks through every control" do
      assertRight "counter scene"
        ( Scene.runScene Counter.update Counter.view Counter.initialModel do
            Scene.expectExists (Query.namedRole "heading" "Counter")
            Scene.expectText (Query.text "0") "0"
            Scene.click (Query.namedRole "button" "+")
            Scene.expectText (Query.text "1") "1"
            Scene.click (Query.namedRole "button" "+")
            Scene.expectText (Query.text "2") "2"
            Scene.click (Query.namedRole "button" "-")
            Scene.expectText (Query.text "1") "1"
            Scene.click (Query.namedRole "button" "Reset")
            Scene.expectText (Query.text "0") "0"
        )
  , test "a missing locator fails with a readable message" do
      case
        Scene.runScene Counter.update Counter.view Counter.initialModel do
          Scene.click (Query.namedRole "button" "Nope")
      of
        Left errorMessage ->
          assertTrue "message names the locator" (Str.contains (Str.Pattern "Nope") errorMessage)
        Right _ -> assertTrue "expected the scene to fail" false
  ]
