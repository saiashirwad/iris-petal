module Example.Parent where

import Prelude

import Data.Void (Void)
import Example.Counter (OutMessage(..))
import Example.Counter as Counter
import Foldkit.Core (Update, continue)
import Foldkit.Html (Html)
import Foldkit.Submodel as Submodel

data Message
  = GotCounter Counter.Message

derive instance eqMessage :: Eq Message

instance showMessage :: Show Message where
  show = case _ of
    GotCounter childMessage -> "GotCounter (" <> show childMessage <> ")"

type Model =
  { counter :: Counter.Model
  , reachedTen :: Boolean
  }

initialModel :: Model
initialModel =
  { counter: Counter.initialModel
  , reachedTen: false
  }

update :: Model -> Message -> Update Model Message Void
update model = case _ of
  GotCounter childMessage ->
    Submodel.foldChild config childMessage model
  where
  config =
    { read: _.counter
    , write: \parent child -> parent { counter = child }
    , toParentMessage: GotCounter
    , update: Counter.update
    , onOutMessage: \ReachedTen parent -> continue parent { reachedTen = true }
    }

view :: Model -> Html Message
view model =
  Submodel.mapView GotCounter (Counter.view model.counter)
