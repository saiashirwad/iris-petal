module Foldkit.Command
  ( CommandName(..)
  , Command
  , Definition
  , Matcher
  , define
  , define0
  , command
  , command0
  , definitionName
  , liftResult
  , name
  , arguments
  , execute
  , mapMessage
  , mapDefinition
  , matches
  , matchesDefinition
  , byDefinition
  , byInstance
  , matcherMatches
  , describe
  , describeDefinition
  , describeMatcher
  )
  where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)

newtype CommandName = CommandName String

derive newtype instance eqCommandName :: Eq CommandName
derive newtype instance ordCommandName :: Ord CommandName

instance showCommandName :: Show CommandName where
  show (CommandName value) = value

newtype Command message =
  Command { name :: CommandName, arguments :: Maybe Json, run :: Aff message }

data Definition args result message
  = Definition
      { name :: CommandName
      , encodeArgs :: args -> Maybe Json
      , run :: args -> Aff result
      , lift :: result -> message
      }

data Matcher message = MatchDefinition CommandName | MatchInstance (Command message)

define ::
  forall args result message.
  EncodeJson args =>
  CommandName ->
  (args -> Aff result) ->
  (result -> message) ->
  Definition args result message
define commandName run lift =
  Definition { name: commandName, encodeArgs: Just <<< encodeJson, run, lift }

define0 ::
  forall result message.
  CommandName -> Aff result -> (result -> message) -> Definition Unit result message
define0 commandName run lift =
  Definition { name: commandName, encodeArgs: const Nothing, run: \_ -> run, lift }

command :: forall args result message. Definition args result message -> args -> Command message
command (Definition definition) args = Command
  { name: definition.name
  , arguments: definition.encodeArgs args
  , run: map definition.lift (definition.run args)
  }

command0 :: forall result message. Definition Unit result message -> Command message
command0 definition = command definition unit

definitionName :: forall args result message. Definition args result message -> CommandName
definitionName (Definition definition) = definition.name

liftResult :: forall args result message. Definition args result message -> result -> message
liftResult (Definition definition) = definition.lift

name :: forall message. Command message -> CommandName
name (Command value) = value.name

arguments :: forall message. Command message -> Maybe Json
arguments (Command value) = value.arguments

execute :: forall message. Command message -> Aff message
execute (Command value) = value.run

mapMessage :: forall a b. (a -> b) -> Command a -> Command b
mapMessage f (Command value) = Command value { run = map f value.run }

mapDefinition ::
  forall args result message parentMessage.
  (message -> parentMessage) ->
  Definition args result message ->
  Definition args result parentMessage
mapDefinition f (Definition definition) = Definition definition { lift = f <<< definition.lift }

matches :: forall a b. Command a -> Command b -> Boolean
matches (Command left) (Command right) =
  left.name == right.name && left.arguments == right.arguments

matchesDefinition ::
  forall args result message. Definition args result message -> Command message -> Boolean
matchesDefinition definition commandValue = definitionName definition == name commandValue

byDefinition :: forall args result message. Definition args result message -> Matcher message
byDefinition definition = MatchDefinition (definitionName definition)

byInstance :: forall message. Command message -> Matcher message
byInstance = MatchInstance

matcherMatches :: forall message. Matcher message -> Command message -> Boolean
matcherMatches matcher commandValue = case matcher of
  MatchDefinition commandName -> commandName == name commandValue
  MatchInstance expected -> matches expected commandValue

describe :: forall message. Command message -> String
describe commandValue = case arguments commandValue of
  Nothing -> show (name commandValue)
  Just commandArguments -> show (name commandValue) <> " " <> stringify commandArguments

describeDefinition :: forall args result message. Definition args result message -> String
describeDefinition definition = show (definitionName definition)

describeMatcher :: forall message. Matcher message -> String
describeMatcher = case _ of
  MatchDefinition commandName -> show commandName
  MatchInstance commandValue -> describe commandValue

instance showCommand :: Show (Command message) where
  show = describe
