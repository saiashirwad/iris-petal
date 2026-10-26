module Foldkit.Core (Update, Program, continue, withCommands, withOutMessage, mapCommands) where

import Prelude

import Data.Maybe (Maybe(..))
import Foldkit.Command (Command)
import Foldkit.Html (Html)

type Update model message out =
  { model :: model, commands :: Array (Command message), outMessage :: Maybe out }

type Program model message input out =
  { name :: String
  , start :: input -> Update model message out
  , update :: model -> message -> Update model message out
  , view :: model -> Html message
  }

continue :: forall model message out. model -> Update model message out
continue model = { model, commands: [], outMessage: Nothing }

withCommands ::
  forall model message out. model -> Array (Command message) -> Update model message out
withCommands model commands = { model, commands, outMessage: Nothing }

withOutMessage :: forall model message out. model -> out -> Update model message out
withOutMessage model outMessage = { model, commands: [], outMessage: Just outMessage }

mapCommands ::
  forall model message out.
  (Command message -> Command message) -> Update model message out -> Update model message out
mapCommands f update = update { commands = map f update.commands }
