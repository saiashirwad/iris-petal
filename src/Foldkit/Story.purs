module Foldkit.Story
  ( Story
  , StoryState
  , runStory
  , given
  , send
  , resolve
  , resolveExact
  , expectCommands
  , expectHasCommands
  , expectNone
  , expectModel
  , expectOutMessage
  , expectNoOutMessage
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (State, get, modify_, put, runState)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foldkit.Command (Command, Definition)
import Foldkit.Command as Command
import Foldkit.Core (Update)
import Foldkit.Simulation as Simulation

type StoryState model message out = Simulation.State model message out

type StoryM model message out =
  ReaderT
    (model -> message -> Update model message out)
    (ExceptT String (State (StoryState model message out)))

type Story model message out = StoryM model message out Unit

runStory
  :: forall model message out
   . (model -> message -> Update model message out)
  -> model
  -> Story model message out
  -> Either String (StoryState model message out)
runStory updateFn initial story =
  let
    Tuple result finalState =
      runState
        (runExceptT (runReaderT story updateFn))
        (Simulation.initial initial)
  in
    case result of
      Left errorMessage -> Left errorMessage
      Right _ ->
        case Simulation.unresolvedError finalState.commands of
          Nothing -> Right finalState
          Just errorMessage -> Left errorMessage

given :: forall model message out. model -> Story model message out
given model =
  modify_ \state -> state { model = model }

send :: forall model message out. message -> Story model message out
send message = do
  state <- get
  case Simulation.pendingError "when you sent a new Message" state.commands of
    Just errorMessage -> throwError errorMessage
    Nothing -> applyMessage message

resolve
  :: forall args result model message out
   . Definition args result message
  -> result
  -> Story model message out
resolve definition result = do
  updateFn <- ask
  state <- get
  case
    Simulation.resolveBy
      (Command.describeDefinition definition)
      (Command.matchesDefinition definition)
      updateFn
      (definition.lift result)
      state
    of
    Left errorMessage -> throwError errorMessage
    Right next -> put next

resolveExact
  :: forall model message out
   . Command message
  -> message
  -> Story model message out
resolveExact commandValue resultMessage = do
  updateFn <- ask
  state <- get
  case
    Simulation.resolveBy
      (Command.describe commandValue)
      (Command.matches commandValue)
      updateFn
      resultMessage
      state
    of
    Left errorMessage -> throwError errorMessage
    Right next -> put next

expectCommands
  :: forall model message out
   . Array (Command message)
  -> Story model message out
expectCommands expected = do
  state <- get
  case Simulation.exactError expected state.commands of
    Nothing -> pure unit
    Just errorMessage -> throwError errorMessage

expectHasCommands
  :: forall model message out
   . Array (Command message)
  -> Story model message out
expectHasCommands expected = do
  state <- get
  case Simulation.subsetError expected state.commands of
    Nothing -> pure unit
    Just errorMessage -> throwError errorMessage

expectNone :: forall model message out. Story model message out
expectNone = expectCommands []

expectModel
  :: forall model message out
   . Show model
  => String
  -> (model -> Boolean)
  -> Story model message out
expectModel description predicate = do
  state <- get
  if predicate state.model then pure unit
  else
    throwError
      ( "Expected model to satisfy: "
          <> description
          <> "\n\nActual model:\n"
          <> show state.model
      )

expectOutMessage
  :: forall model message out
   . Eq out
  => Show out
  => out
  -> Story model message out
expectOutMessage expected = do
  state <- get
  case state.outMessage of
    Just actual
      | actual == expected -> pure unit
      | otherwise ->
          throwError
            ( "Expected OutMessage "
                <> show expected
                <> " but the last update emitted "
                <> show actual
                <> "."
            )
    Nothing ->
      throwError
        ( "Expected OutMessage "
            <> show expected
            <> " but the last update emitted none."
        )

expectNoOutMessage :: forall model message out. Story model message out
expectNoOutMessage = do
  state <- get
  case state.outMessage of
    Nothing -> pure unit
    Just _ ->
      throwError "Expected no OutMessage but the last update emitted one."

applyMessage
  :: forall model message out
   . message
  -> StoryM model message out Unit
applyMessage message = do
  updateFn <- ask
  state <- get
  put (Simulation.apply updateFn message state)
