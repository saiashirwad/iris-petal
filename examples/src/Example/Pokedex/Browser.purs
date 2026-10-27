module Example.Pokedex.Browser (main) where

import Prelude

import Effect (Effect)
import Example.Pokedex as Pokedex
import Petal.Browser as Browser

main :: Effect Unit
main = Browser.mount "root" Pokedex.program
