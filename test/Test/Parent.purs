module Test.Parent where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Effect.Aff (Aff)
import Example.Counter as Counter
import Example.Parent as Parent
import Foldkit.Locator as Locator
import Foldkit.Scene as Scene
import Foldkit.Story as Story
import Foldkit.Test (Test, assertRight, test)

storyTests :: Array Test
storyTests =
  [ test "a child OutMessage is handled by the parent" do
      assertRight "parent story"
        ( Story.runStory Parent.update Parent.initialModel do
            for_ (Array.range 1 10) \_ -> Story.send (Parent.GotCounter Counter.ClickedIncrement)
            Story.expectModel "parent recorded ReachedTen" \model -> model.reachedTen
            Story.expectModel "child count is ten" \model -> model.counter.count == 10
            Story.expectNoOutMessage
        )
  ]

sceneTests :: Array Test
sceneTests =
  [ test "the parent view maps child messages back to the parent" do
      assertRight "parent scene"
        ( Scene.runScene Parent.update Parent.view Parent.initialModel do
            Scene.expectExists (Locator.namedRole "heading" "Counter")
            Scene.click (Locator.namedRole "button" "+")
            Scene.expectText (Locator.text "1") "1"
        )
  ]
