module Foldkit.Command
  ( CommandName(..)
  , Command
  , Definition
  , define
  , define0
  , command
  , command0
  , definitionName
  , name
  , arguments
  , execute
  , mapMessage
  , mapDefinition
  , matches
  , matchesDefinition
  , describe
  , describeDefinition
  ) where

import Prelude

import Effect.Aff (Aff)

newtype CommandName = CommandName String

derive newtype instance eqCommandName :: Eq CommandName
derive newtype instance ordCommandName :: Ord CommandName
derive newtype instance showCommandName :: Show CommandName

newtype Command message = Command
  { name :: CommandName
  , arguments :: String
  , run :: Aff message
  }

type Definition args result message =
  { name :: CommandName
  , lift :: result -> message
  , build :: args -> Command message
  }

define
  :: forall args result message
   . Show args
  => CommandName
  -> (args -> Aff result)
  -> (result -> message)
  -> Definition args result message
define commandName run lift =
  { name: commandName
  , lift
  , build: \args ->
      Command
        { name: commandName
        , arguments: show args
        , run: map lift (run args)
        }
  }

define0
  :: forall result message
   . CommandName
  -> Aff result
  -> (result -> message)
  -> Definition Unit result message
define0 commandName run lift =
  { name: commandName
  , lift
  , build: \_ ->
      Command
        { name: commandName
        , arguments: ""
        , run: map lift run
        }
  }

command
  :: forall args result message
   . Definition args result message
  -> args
  -> Command message
command definition = definition.build

command0
  :: forall result message
   . Definition Unit result message
  -> Command message
command0 definition = definition.build unit

definitionName
  :: forall args result message
   . Definition args result message
  -> CommandName
definitionName definition = definition.name

name :: forall message. Command message -> CommandName
name (Command value) = value.name

arguments :: forall message. Command message -> String
arguments (Command value) = value.arguments

execute :: forall message. Command message -> Aff message
execute (Command value) = value.run

mapMessage :: forall a b. (a -> b) -> Command a -> Command b
mapMessage f (Command value) =
  Command value { run = map f value.run }

mapDefinition
  :: forall args result message parentMessage
   . (message -> parentMessage)
  -> Definition args result message
  -> Definition args result parentMessage
mapDefinition f definition =
  definition
    { lift = f <<< definition.lift
    , build = mapMessage f <<< definition.build
    }

matches :: forall a b. Command a -> Command b -> Boolean
matches (Command left) (Command right) =
  left.name == right.name && left.arguments == right.arguments

matchesDefinition
  :: forall args result message
   . Definition args result message
  -> Command message
  -> Boolean
matchesDefinition definition commandValue =
  definition.name == name commandValue

describe :: forall message. Command message -> String
describe commandValue =
  case arguments commandValue of
    "" -> show (name commandValue)
    commandArguments -> show (name commandValue) <> " " <> commandArguments

describeDefinition
  :: forall args result message
   . Definition args result message
  -> String
describeDefinition definition = show definition.name

instance showCommand :: Show (Command message) where
  show = describe
