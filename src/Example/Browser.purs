module Example.Browser where

import Prelude

import Effect (Effect)
import Example.Weather as Weather
import Foldkit.Runtime as Runtime

main :: Effect Unit
main =
  Runtime.runApplication
    { initialModel: Weather.initialModel
    , update: Weather.update
    , view: Weather.view
    , containerId: "root"
    }
