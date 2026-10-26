module Foldkit.Submodel
  ( Config
  , foldChild
  , mapView
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Foldkit.Command as Command
import Foldkit.Core (Update, continue)
import Foldkit.Html (Html)

type Config parentModel parentMessage childModel childMessage out =
  { read :: parentModel -> childModel
  , write :: parentModel -> childModel -> parentModel
  , toParentMessage :: childMessage -> parentMessage
  , update :: childModel -> childMessage -> Update childModel childMessage out
  , onOutMessage :: out -> parentModel -> Update parentModel parentMessage Void
  }

foldChild
  :: forall parentModel parentMessage childModel childMessage out
   . Config parentModel parentMessage childModel childMessage out
  -> childMessage
  -> parentModel
  -> Update parentModel parentMessage Void
foldChild config childMessage parentModel =
  let
    childUpdate = config.update (config.read parentModel) childMessage
    parentAfterChild = config.write parentModel childUpdate.model
    outUpdate = case childUpdate.outMessage of
      Nothing -> continue parentAfterChild
      Just outMessage -> config.onOutMessage outMessage parentAfterChild
  in
    outUpdate
      { commands =
          map (Command.mapMessage config.toParentMessage) childUpdate.commands
            <> outUpdate.commands
      , outMessage = Nothing
      }

mapView
  :: forall childMessage parentMessage
   . (childMessage -> parentMessage)
  -> Html childMessage
  -> Html parentMessage
mapView = map
