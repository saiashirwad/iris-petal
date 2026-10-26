module Example.Browser where

import Prelude

import Effect (Effect)
import Example.Weather as Weather
import Foldkit.Core (Program, continue)
import Foldkit.Runtime as Runtime

weatherProgram :: Program Weather.Model Weather.Message Unit Void
weatherProgram =
  { name: "Weather"
  , start: \_ -> continue Weather.initialModel
  , update: Weather.update
  , view: Weather.view
  }

main :: Effect Unit
main = Runtime.runApplication { program: weatherProgram, input: unit, containerId: "root" }
