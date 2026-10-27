-- | Prints the server-rendered shell: the initial model's view, before the
-- | first page loads.
module Example.Pokedex.Server (main) where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Example.Pokedex as Pokedex
import Foldkit.Render (renderToString)

main :: Effect Unit
main = log (renderToString (Pokedex.view Pokedex.program.model))
