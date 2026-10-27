-- | The whole program shape. An app is a `Program`: an initial model, a
-- | startup transition, `update`, and `view`.
-- |
-- | `Update model message a` is a pure state transition over `model` that may
-- | also issue commands producing `message`. It is a monad, so `update` reads
-- | as a `do` block with `modify_`, `gets`, and `perform`. The `a` is whatever
-- | the transition returns: `Unit` for an app, and a child's out-message for a
-- | component that reports to its parent.
module Foldkit (Program, Update, Step, perform, zoom, runUpdate) where

import Prelude

import Control.Monad.State.Class (class MonadState)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Foldkit.Command (Command, Definition)
import Foldkit.Command as Command
import Foldkit.Html (Html)
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))

type Program model message =
  { model :: model
  , init :: Update model message Unit
  , update :: message -> Update model message Unit
  , view :: model -> Html message
  }

newtype Update model message a = Update (model -> Step model message a)

type Step model message a = { model :: model, commands :: Array (Command message), value :: a }

instance functorUpdate :: Functor (Update model message) where
  map f (Update transition) = Update \model ->
    let
      step = transition model
    in
      step { value = f step.value }

instance applyUpdate :: Apply (Update model message) where
  apply = ap

instance applicativeUpdate :: Applicative (Update model message) where
  pure value = Update \model -> { model, commands: [], value }

instance bindUpdate :: Bind (Update model message) where
  bind (Update first) next = Update \model ->
    let
      before = first model
      Update second = next before.value
      after = second before.model
    in
      after { commands = before.commands <> after.commands }

instance monadUpdate :: Monad (Update model message)

instance monadStateUpdate :: MonadState model (Update model message) where
  state f = Update \model ->
    let
      Tuple value next = f model
    in
      { model: next, commands: [], value }

-- | Issue a command. The runtime runs it after this update; a test resolves it
-- | with a `result` value.
perform ::
  forall model message args result.
  Definition args result -> args -> (result -> message) -> Update model message Unit
perform definition args toMessage = Update \model ->
  { model, commands: [ Command.make definition args toMessage ], value: unit }

-- | Run a child's transition against the record field `label` and wrap its
-- | commands' messages for the parent. The child's return value passes through.
-- |
-- | ```purescript
-- | GotDetail message -> zoom @"details" GotDetail (Query.update message)
-- | ```
zoom ::
  forall @label childModel childMessage parentMessage rest parentRow a.
  IsSymbol label =>
  Row.Cons label childModel rest parentRow =>
  (childMessage -> parentMessage) ->
  Update childModel childMessage a ->
  Update { | parentRow } parentMessage a
zoom wrap (Update child) = Update \parent ->
  let
    step = child (Record.get (Proxy :: Proxy label) parent)
  in
    { model: Record.set (Proxy :: Proxy label) step.model parent
    , commands: map (map wrap) step.commands
    , value: step.value
    }

runUpdate :: forall model message a. Update model message a -> model -> Step model message a
runUpdate (Update transition) = transition
