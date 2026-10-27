module Test.Counter (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Example.Counter (Message(..), OutMessage(..))
import Example.Counter as Counter
import Petal (runUpdate)
import Petal.Html (Role(..))
import Petal.Locator (named, role)
import Petal.Test (Check(..), click, expect)
import Petal.Test as Test
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Counter" do
  it "reports ReachedTen from the tenth increment" do
    (runUpdate (Counter.update ClickedIncrement) { count: 9 }).value `shouldEqual` Just ReachedTen
    (runUpdate (Counter.update ClickedIncrement) { count: 3 }).value `shouldEqual` Nothing

  it "clicks through every control" do
    Test.run Counter.program do
      click (named Button "+")
      click (named Button "+")
      click (named Button "-")
      expect (role Paragraph) (HasText "1")
      click (named Button "Reset")
      expect (role Paragraph) (HasText "0")
