module Test.Parent (spec) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Example.Parent as Parent
import Foldkit.Html (Role(..))
import Foldkit.Locator (named)
import Foldkit.Test (click, expectModel)
import Foldkit.Test as Test
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = describe "Parent" do
  it "records the child's ReachedTen through the mapped view" do
    Test.run Parent.program do
      for_ (Array.range 1 9) \_ -> click (named Button "+")
      expectModel _.reachedTen false
      click (named Button "+")
      expectModel _.reachedTen true
      expectModel _.counter.count 10
