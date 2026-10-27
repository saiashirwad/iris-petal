module Test.Petal.Main where

import Prelude

import Effect (Effect)
import Test.Petal.Cache as Cache
import Test.Petal.Http as Http
import Test.Petal.Locator as Locator
import Test.Petal.Scenario as Scenario
import Test.Petal.View as View
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Scenario.spec
  Cache.spec
  View.spec
  Http.spec
  Locator.spec
