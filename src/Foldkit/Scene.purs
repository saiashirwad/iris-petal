module Foldkit.Scene
  ( Scene
  , SceneState
  , runScene
  , click
  , typeText
  , submit
  , press
  , check
  , within
  , expectExists
  , expectAbsent
  , expectText
  , expectTextContaining
  , expectValue
  , expectDisabled
  , expectEnabled
  , expectChecked
  , expectAttribute
  , expectAccessibleName
  , expectCount
  , resolve
  , resolveExact
  , expectCommands
  , expectHasCommands
  , expectNone
  , expectOutMessage
  , expectNoOutMessage
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (State, get, gets, modify_, put, runState)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.Tuple (Tuple(..))
import Foldkit.Command (Command, Definition)
import Foldkit.Command as Command
import Foldkit.Core (Update)
import Foldkit.Html (Event, Html, defaultEvent)
import Foldkit.Query as Query
import Foldkit.Simulation as Simulation

type SceneState model message out =
  { simulation :: Simulation.State model message out
  , scope :: Maybe Query.Locator
  }

type SceneEnv model message out =
  { update :: model -> message -> Update model message out
  , view :: model -> Html message
  }

type SceneM model message out =
  ReaderT
    (SceneEnv model message out)
    (ExceptT String (State (SceneState model message out)))

type Scene model message out = SceneM model message out Unit

runScene
  :: forall model message out
   . (model -> message -> Update model message out)
  -> (model -> Html message)
  -> model
  -> Scene model message out
  -> Either String (SceneState model message out)
runScene updateFn viewFn initial scene =
  let
    env = { update: updateFn, view: viewFn }
    Tuple result finalState =
      runState
        (runExceptT (runReaderT scene env))
        { simulation: Simulation.initial initial, scope: Nothing }
  in
    case result of
      Left errorMessage -> Left errorMessage
      Right _ ->
        case Simulation.unresolvedError finalState.simulation.commands of
          Nothing -> Right finalState
          Just errorMessage -> Left errorMessage

click :: forall model message out. Query.Locator -> Scene model message out
click locator = do
  ref <- resolveRef locator
  case Query.findHandler "click" ref of
    Just handler -> dispatch handler defaultEvent
    Nothing ->
      if Query.isSubmitButton ref then
        case Query.findSubmitHandler ref of
          Just handler -> dispatch handler defaultEvent
          Nothing ->
            throwError
              ( "I found an element matching "
                  <> Query.describe locator
                  <> " but it has no click handler and no submit handler on its form."
              )
      else
        throwError
          ( "I found an element matching "
              <> Query.describe locator
              <> " but neither it nor an ancestor has a click handler."
          )

typeText
  :: forall model message out
   . Query.Locator
  -> String
  -> Scene model message out
typeText locator value = do
  ref <- resolveRef locator
  case Query.findHandler "input" ref of
    Nothing ->
      throwError
        ( "I found an element matching "
            <> Query.describe locator
            <> " but it has no input handler."
        )
    Just handler -> dispatch handler defaultEvent { value = value }

submit :: forall model message out. Query.Locator -> Scene model message out
submit locator = do
  ref <- resolveRef locator
  case Query.eventHandler "submit" (Query.targetHtml ref) of
    Nothing ->
      throwError
        ( "I found an element matching "
            <> Query.describe locator
            <> " but it has no submit handler."
        )
    Just handler -> dispatch handler defaultEvent

press
  :: forall model message out
   . Query.Locator
  -> String
  -> Scene model message out
press locator key = do
  ref <- resolveRef locator
  case Query.findHandler "keydown" ref of
    Nothing ->
      throwError
        ( "I found an element matching "
            <> Query.describe locator
            <> " but it has no keydown handler."
        )
    Just handler -> dispatch handler defaultEvent { key = key }

check :: forall model message out. Query.Locator -> Scene model message out
check locator = do
  ref <- resolveRef locator
  let
    currentlyChecked =
      Query.booleanAttribute "checked" (Query.targetHtml ref) == Just true
  case Query.findHandler "change" ref of
    Nothing ->
      throwError
        ( "I found an element matching "
            <> Query.describe locator
            <> " but it has no change handler."
        )
    Just handler -> dispatch handler defaultEvent { checked = not currentlyChecked }

within
  :: forall model message out
   . Query.Locator
  -> Scene model message out
  -> Scene model message out
within locator action = do
  previous <- gets _.scope
  scopedParent <- scopedLocator locator
  modify_ \state -> state { scope = Just scopedParent }
  action
  modify_ \state -> state { scope = previous }

expectExists :: forall model message out. Query.Locator -> Scene model message out
expectExists locator = void (resolveRef locator)

expectAbsent :: forall model message out. Query.Locator -> Scene model message out
expectAbsent locator = do
  root <- currentRoot
  scoped <- scopedLocator locator
  case Query.find scoped root of
    Nothing -> pure unit
    Just _ ->
      throwError
        ( "Expected no element matching "
            <> Query.describe scoped
            <> " but one exists."
        )

expectText
  :: forall model message out
   . Query.Locator
  -> String
  -> Scene model message out
expectText locator expected = do
  ref <- resolveRef locator
  let
    actual =
      Query.normalizeWhitespace
        (Query.textContent (Query.targetHtml ref))
    wanted = Query.normalizeWhitespace expected
  if actual == wanted then pure unit
  else
    throwError
      ( "Expected "
          <> Query.describe locator
          <> " to have text \""
          <> wanted
          <> "\" but received \""
          <> actual
          <> "\"."
      )

expectTextContaining
  :: forall model message out
   . Query.Locator
  -> String
  -> Scene model message out
expectTextContaining locator expected = do
  ref <- resolveRef locator
  let
    actual =
      Query.normalizeWhitespace
        (Query.textContent (Query.targetHtml ref))
    wanted = Query.normalizeWhitespace expected
  if actual == wanted || Str.contains (Str.Pattern wanted) actual then pure unit
  else
    throwError
      ( "Expected "
          <> Query.describe locator
          <> " to contain text \""
          <> wanted
          <> "\" but received \""
          <> actual
          <> "\"."
      )

expectValue
  :: forall model message out
   . Query.Locator
  -> String
  -> Scene model message out
expectValue locator expected = do
  ref <- resolveRef locator
  case Query.attributeValue "value" (Query.targetHtml ref) of
    Just actual
      | actual == expected -> pure unit
      | otherwise ->
          throwError
            ( "Expected "
                <> Query.describe locator
                <> " to have value \""
                <> expected
                <> "\" but received \""
                <> actual
                <> "\"."
            )
    Nothing ->
      throwError
        ( "Expected "
            <> Query.describe locator
            <> " to have value \""
            <> expected
            <> "\" but it has no value."
        )

expectDisabled :: forall model message out. Query.Locator -> Scene model message out
expectDisabled locator = do
  ref <- resolveRef locator
  if isDisabled ref then pure unit
  else
    throwError
      ( "Expected "
          <> Query.describe locator
          <> " to be disabled but it is enabled."
      )

expectEnabled :: forall model message out. Query.Locator -> Scene model message out
expectEnabled locator = do
  ref <- resolveRef locator
  if isDisabled ref then
    throwError
      ( "Expected "
          <> Query.describe locator
          <> " to be enabled but it is disabled."
      )
  else pure unit

expectChecked :: forall model message out. Query.Locator -> Scene model message out
expectChecked locator = do
  ref <- resolveRef locator
  if Query.booleanAttribute "checked" (Query.targetHtml ref) == Just true then pure unit
  else
    throwError
      ( "Expected "
          <> Query.describe locator
          <> " to be checked but it is not."
      )

expectAttribute
  :: forall model message out
   . Query.Locator
  -> String
  -> String
  -> Scene model message out
expectAttribute locator attribute expected = do
  ref <- resolveRef locator
  case Query.attributeValue attribute (Query.targetHtml ref) of
    Just actual
      | actual == expected -> pure unit
      | otherwise ->
          throwError
            ( "Expected "
                <> Query.describe locator
                <> " to have attribute "
                <> attribute
                <> "=\""
                <> expected
                <> "\" but received \""
                <> actual
                <> "\"."
            )
    Nothing ->
      throwError
        ( "Expected "
            <> Query.describe locator
            <> " to have attribute "
            <> attribute
            <> "=\""
            <> expected
            <> "\" but the attribute is absent."
        )

expectAccessibleName
  :: forall model message out
   . Query.Locator
  -> String
  -> Scene model message out
expectAccessibleName locator expected = do
  root <- currentRoot
  ref <- resolveRef locator
  let
    actual =
      Query.normalizeWhitespace
        (Query.accessibleName root (Query.targetHtml ref))
    wanted = Query.normalizeWhitespace expected
  if actual == wanted then pure unit
  else
    throwError
      ( "Expected "
          <> Query.describe locator
          <> " to have accessible name \""
          <> wanted
          <> "\" but received \""
          <> actual
          <> "\"."
      )

expectCount
  :: forall model message out
   . Int
  -> Query.Locator
  -> Scene model message out
expectCount expected locator = do
  root <- currentRoot
  scoped <- scopedLocator locator
  let actual = Array.length (Query.findAll scoped root)
  if actual == expected then pure unit
  else
    throwError
      ( "Expected "
          <> show expected
          <> " elements matching "
          <> Query.describe scoped
          <> " but found "
          <> show actual
          <> "."
      )

resolve
  :: forall args result model message out
   . Definition args result message
  -> result
  -> Scene model message out
resolve definition result = do
  env <- ask
  state <- get
  case
    Simulation.resolveBy
      (Command.describeDefinition definition)
      (Command.matchesDefinition definition)
      env.update
      (definition.lift result)
      state.simulation
    of
    Left errorMessage -> throwError errorMessage
    Right next -> put state { simulation = next }

resolveExact
  :: forall model message out
   . Command message
  -> message
  -> Scene model message out
resolveExact commandValue resultMessage = do
  env <- ask
  state <- get
  case
    Simulation.resolveBy
      (Command.describe commandValue)
      (Command.matches commandValue)
      env.update
      resultMessage
      state.simulation
    of
    Left errorMessage -> throwError errorMessage
    Right next -> put state { simulation = next }

expectCommands
  :: forall model message out
   . Array (Command message)
  -> Scene model message out
expectCommands expected = do
  state <- get
  case Simulation.exactError expected state.simulation.commands of
    Nothing -> pure unit
    Just errorMessage -> throwError errorMessage

expectHasCommands
  :: forall model message out
   . Array (Command message)
  -> Scene model message out
expectHasCommands expected = do
  state <- get
  case Simulation.subsetError expected state.simulation.commands of
    Nothing -> pure unit
    Just errorMessage -> throwError errorMessage

expectNone :: forall model message out. Scene model message out
expectNone = expectCommands []

expectOutMessage
  :: forall model message out
   . Eq out
  => Show out
  => out
  -> Scene model message out
expectOutMessage expected = do
  state <- get
  case state.simulation.outMessage of
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

expectNoOutMessage :: forall model message out. Scene model message out
expectNoOutMessage = do
  state <- get
  case state.simulation.outMessage of
    Nothing -> pure unit
    Just _ ->
      throwError "Expected no OutMessage but the last update emitted one."

dispatch
  :: forall model message out
   . (Event -> message)
  -> Event
  -> SceneM model message out Unit
dispatch handler event =
  applyExternalMessage (handler event)

applyExternalMessage
  :: forall model message out
   . message
  -> SceneM model message out Unit
applyExternalMessage message = do
  state <- get
  case
    Simulation.pendingError
      "when an interaction dispatched a new Message"
      state.simulation.commands
    of
    Just errorMessage -> throwError errorMessage
    Nothing -> do
      env <- ask
      put state { simulation = Simulation.apply env.update message state.simulation }

currentRoot :: forall model message out. SceneM model message out (Html message)
currentRoot = do
  env <- ask
  state <- get
  pure (env.view state.simulation.model)

scopedLocator
  :: forall model message out
   . Query.Locator
  -> SceneM model message out Query.Locator
scopedLocator locator = do
  scope <- gets _.scope
  pure case scope of
    Nothing -> locator
    Just parent -> Query.within parent locator

resolveRef
  :: forall model message out
   . Query.Locator
  -> SceneM model message out (Query.NodeRef message)
resolveRef locator = do
  root <- currentRoot
  scoped <- scopedLocator locator
  case Query.findRef scoped root of
    Just ref -> pure ref
    Nothing ->
      throwError
        ( "I could not find an element matching "
            <> Query.describe scoped
            <> ".\n\nCheck that the view renders it in the current state."
        )

isDisabled :: forall message. Query.NodeRef message -> Boolean
isDisabled ref =
  Query.booleanAttribute "disabled" (Query.targetHtml ref) == Just true
    || Query.attributeValue "aria-disabled" (Query.targetHtml ref) == Just "true"
