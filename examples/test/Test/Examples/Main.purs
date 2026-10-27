module Test.Examples.Main where

import Prelude

import Effect (Effect)
import Test.Counter as Counter
import Test.Parent as Parent
import Test.Pokedex as Pokedex
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Counter.spec
  Parent.spec
  Pokedex.spec
