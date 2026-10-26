module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Example.Browser as Browser
import Example.Counter as Counter
import Example.Weather as Weather
import Foldkit.Render (renderToString)
import Foldkit.Runtime as Runtime

main :: Effect Unit
main = do
  browser <- Runtime.isBrowser
  if browser then Browser.main
  else do
    log (renderToString (Counter.view Counter.initialModel))
    log (renderToString (Weather.view Weather.initialModel))
