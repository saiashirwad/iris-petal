module Example.Counter
  ( Model
  , Message(..)
  , OutMessage(..)
  , initialModel
  , update
  , view
  , program
  )
  where

import Prelude

import Control.Monad.State.Class (modify, modify_, put)
import Data.Maybe (Maybe(..))
import Petal (Program, Update)
import Petal.Html (Html, button, div, h1, onClick, p, text)

type Model = { count :: Int }

data Message = ClickedIncrement | ClickedDecrement | ClickedReset

data OutMessage = ReachedTen

derive instance eqOutMessage :: Eq OutMessage

instance showOutMessage :: Show OutMessage where
  show ReachedTen = "ReachedTen"

initialModel :: Model
initialModel = { count: 0 }

update :: Message -> Update Model Message (Maybe OutMessage)
update = case _ of
  ClickedIncrement -> do
    { count } <- modify \model -> model { count = model.count + 1 }
    pure if count == 10 then Just ReachedTen else Nothing
  ClickedDecrement -> modify_ (\model -> model { count = model.count - 1 }) $> Nothing
  ClickedReset -> put initialModel $> Nothing

view :: Model -> Html Message
view model = div []
  [ h1 [] [ text "Counter" ]
  , p [] [ text (show model.count) ]
  , button [ onClick ClickedDecrement ] [ text "-" ]
  , button [ onClick ClickedReset ] [ text "Reset" ]
  , button [ onClick ClickedIncrement ] [ text "+" ]
  ]

program :: Program Model Message
program = { model: initialModel, init: pure unit, update: void <<< update, view }
