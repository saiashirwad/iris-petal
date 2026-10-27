module Example.Parent.Explore (main) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Example.Explore.Session as Session
import Example.Parent as Parent

main :: Effect Unit
main = Session.run Parent.program Nothing \_ name ->
  Left ("No fixtures in the counter example: " <> name)
