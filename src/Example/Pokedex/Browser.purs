module Example.Pokedex.Browser where

import Prelude

import Effect (Effect)
import Example.Pokedex as Pokedex
import Foldkit.Runtime as Runtime

main :: Effect Unit
main = Runtime.runApplication { program: Pokedex.program, input: unit, containerId: "root" }
