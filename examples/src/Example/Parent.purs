module Example.Parent (Model, Message(..), update, view, program) where

import Prelude

import Control.Monad.State.Class (modify_)
import Data.Foldable (for_)
import Example.Counter (OutMessage(..))
import Example.Counter as Counter
import Petal (Program, Update, zoom)
import Petal.Html (Html)

type Model = { counter :: Counter.Model, reachedTen :: Boolean }

data Message = GotCounter Counter.Message

update :: Message -> Update Model Message Unit
update (GotCounter message) = do
  out <- zoom @"counter" GotCounter (Counter.update message)
  for_ out \ReachedTen -> modify_ _ { reachedTen = true }

view :: Model -> Html Message
view model = map GotCounter (Counter.view model.counter)

program :: Program Model Message
program =
  { model: { counter: Counter.initialModel, reachedTen: false }, init: pure unit, update, view }
