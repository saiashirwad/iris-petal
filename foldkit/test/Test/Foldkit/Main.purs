module Test.Foldkit.Main where

import Prelude

import Effect (Effect)
import Test.Foldkit.Cache as Cache
import Test.Foldkit.Http as Http
import Test.Foldkit.Locator as Locator
import Test.Foldkit.Scenario as Scenario
import Test.Foldkit.View as View
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Scenario.spec
  Cache.spec
  View.spec
  Http.spec
  Locator.spec
