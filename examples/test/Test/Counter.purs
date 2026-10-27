module Test.Counter (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Example.Counter (Message(..), OutMessage(..))
import Example.Counter as Counter
import Foldkit (runUpdate)
import Foldkit.Html (Role(..))
import Foldkit.Locator (named, role)
import Foldkit.Test (Check(..), click, expect)
import Foldkit.Test as Test
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
