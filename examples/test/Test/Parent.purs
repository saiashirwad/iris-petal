module Test.Parent (spec) where

import Prelude

import Data.Array as Array
import Data.Either (either)
import Data.Foldable (for_)
import Example.Parent as Parent
import Petal.Html (Role(..))
import Petal.Locator (named, role)
import Petal.Test (Check(..), click, expect, expectModel)
import Petal.Test as Test
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

spec :: Spec Unit
spec = describe "Parent" do
  it "records the child's ReachedTen through the mapped view" do
    Test.run Parent.program do
      for_ (Array.range 1 9) \_ -> click (named Button "+")
      expectModel _.reachedTen false
      click (named Button "+")
      expectModel _.reachedTen true
      expectModel _.counter.count 10

  it "explores the composed counter through successive session steps" do
    either fail pure do
      first <- Test.step (click (named Button "+")) (Test.start Parent.program)
      second <- Test.step
        ( do
            click (named Button "+")
            expect (role Paragraph) (HasText "2")
            expectModel _.counter.count 2
        )
        first
      Test.finish second
