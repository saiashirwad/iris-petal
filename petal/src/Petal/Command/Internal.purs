module Petal.Command.Internal
  ( Definition
  , Command
  , define
  , make
  , definitionName
  , describe
  , run
  , isFrom
  , argumentsFor
  , resolveWith
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

newtype Definition args result =
  Definition { name :: String, run :: args -> Aff result, showArgs :: args -> String }

newtype Command message =
  Command { definition :: Definition Erased Erased, args :: Erased, toMessage :: Erased -> message }

foreign import data Erased :: Type

instance functorCommand :: Functor Command where
  map f (Command command) = Command command { toMessage = f <<< command.toMessage }

instance showCommand :: Show (Command message) where
  show = describe

-- | Define a command at the top level of a module. Identity is the definition
-- | value itself, so a definition built inside a function is a new definition
-- | on every call and tests cannot resolve it.
define :: forall args result. Show args => String -> (args -> Aff result) -> Definition args result
define commandName runner = Definition { name: commandName, run: runner, showArgs: show }

make ::
  forall args result message.
  Definition args result -> args -> (result -> message) -> Command message
make definition args toMessage = Command
  { definition: erase definition, args: unsafeCoerce args, toMessage: toMessage <<< unsafeCoerce }

describe :: forall message. Command message -> String
describe (Command command) =
  let
    Definition definition = command.definition
  in
    definition.name <> " " <> definition.showArgs command.args

run :: forall message. Command message -> Aff message
run (Command command) =
  let
    Definition definition = command.definition
  in
    command.toMessage <$> definition.run command.args

isFrom :: forall args result message. Definition args result -> Command message -> Boolean
isFrom definition (Command command) = unsafeRefEq (erase definition) command.definition

argumentsFor :: forall args result message. Definition args result -> Command message -> Maybe args
argumentsFor definition commandValue@(Command command) =
  if isFrom definition commandValue then Just (unsafeCoerce command.args) else Nothing

resolveWith ::
  forall args result message. Definition args result -> result -> Command message -> Maybe message
resolveWith definition result commandValue@(Command command) =
  if isFrom definition commandValue then Just (command.toMessage (unsafeCoerce result)) else Nothing

erase :: forall args result. Definition args result -> Definition Erased Erased
erase = unsafeCoerce

definitionName :: forall args result. Definition args result -> String
definitionName (Definition definition) = definition.name
