module Foldkit.Story
  ( Story
  , StoryState
  , runStory
  , send
  , resolve
  , resolveExact
  , expectCommands
  , expectHasCommands
  , expectNone
  , expectModel
  , expectModelWith
  , expectOutMessage
  , expectNoOutMessage
  )
  where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (State, runState)
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Foldkit.Command (Command, Definition, Matcher)
import Foldkit.Core (Update)
import Foldkit.Simulation as Simulation

type StoryState model message out = Simulation.State model message out

type StoryM model message out =
  ReaderT
    (model -> message -> Update model message out)
    (ExceptT String (State (StoryState model message out)))

type Story model message out = StoryM model message out Unit

runStory ::
  forall model message out.
  (model -> message -> Update model message out) ->
  model ->
  Story model message out ->
  Either String (StoryState model message out)
runStory updateFn initial story =
  let
    Tuple result finalState =
      runState (runExceptT (runReaderT story updateFn)) (Simulation.initial initial)
  in
    case result of
      Left errorMessage -> Left errorMessage
      Right _ -> finish finalState
  where
  finish :: StoryState model message out -> Either String (StoryState model message out)
  finish finalState = maybe (Right finalState) Left (Simulation.unresolvedError finalState.commands)

send :: forall model message out. message -> Story model message out
send message = do
  updateFn <- ask
  Simulation.send "when you sent a new Message" updateFn message

resolve ::
  forall args result model message out.
  Definition args result message -> result -> Story model message out
resolve definition result = do
  updateFn <- ask
  Simulation.resolve updateFn definition result

resolveExact :: forall model message out. Command message -> message -> Story model message out
resolveExact commandValue resultMessage = do
  updateFn <- ask
  Simulation.resolveExact updateFn commandValue resultMessage

expectCommands :: forall model message out. Array (Matcher message) -> Story model message out
expectCommands = Simulation.expectCommands

expectHasCommands :: forall model message out. Array (Matcher message) -> Story model message out
expectHasCommands = Simulation.expectHasCommands

expectNone :: forall model message out. Story model message out
expectNone = Simulation.expectNone

expectModel ::
  forall model message out. Show model => String -> (model -> Boolean) -> Story model message out
expectModel = Simulation.expectModelWith show

expectModelWith ::
  forall model message out.
  (model -> String) -> String -> (model -> Boolean) -> Story model message out
expectModelWith = Simulation.expectModelWith

expectOutMessage :: forall model message out. Eq out => Show out => out -> Story model message out
expectOutMessage = Simulation.expectOutMessage

expectNoOutMessage :: forall model message out. Story model message out
expectNoOutMessage = Simulation.expectNoOutMessage
