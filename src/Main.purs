module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Example.Pokedex as Pokedex
import Example.Pokedex.Browser as PokedexBrowser
import Foldkit.Render (renderToString)
import Foldkit.Runtime as Runtime

main :: Effect Unit
main = do
  browser <- Runtime.isBrowser
  if browser then PokedexBrowser.main else log (renderToString (Pokedex.view Pokedex.initialModel))
