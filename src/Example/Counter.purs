module Example.Counter where

import Prelude

import Data.Maybe (Maybe(..))
import Foldkit.Core (Update, continue)
import Foldkit.Html (Html, button, div, h1, onClick, p, text)

data Message = ClickedIncrement | ClickedDecrement | ClickedReset

derive instance eqMessage :: Eq Message

instance showMessage :: Show Message where
  show = case _ of
    ClickedIncrement -> "ClickedIncrement"
    ClickedDecrement -> "ClickedDecrement"
    ClickedReset -> "ClickedReset"

data OutMessage = ReachedTen

derive instance eqOutMessage :: Eq OutMessage
derive instance ordOutMessage :: Ord OutMessage

instance showOutMessage :: Show OutMessage where
  show = case _ of
    ReachedTen -> "ReachedTen"

type Model = { count :: Int }

initialModel :: Model
initialModel = { count: 0 }

update :: Model -> Message -> Update Model Message OutMessage
update model = case _ of
  ClickedIncrement ->
    let
      next = model { count = model.count + 1 }
    in
      if next.count == 10
      then { model: next, commands: [], outMessage: Just ReachedTen }
      else continue next
  ClickedDecrement -> continue model { count = model.count - 1 }
  ClickedReset -> continue model { count = 0 }

view :: Model -> Html Message
view model = div []
  [ h1 [] [ text "Counter" ]
  , p [] [ text (show model.count) ]
  , button [ onClick ClickedDecrement ] [ text "-" ]
  , button [ onClick ClickedReset ] [ text "Reset" ]
  , button [ onClick ClickedIncrement ] [ text "+" ]
  ]
