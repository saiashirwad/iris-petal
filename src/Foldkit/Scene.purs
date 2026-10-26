module Foldkit.Scene
  ( Scene
  , SceneState
  , runScene
  , send
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
  )
  where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, ask, asks, local, runReaderT)
import Control.Monad.State (State, get, runState)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.Tuple (Tuple(..))
import Foldkit.Command (Command, Definition, Matcher)
import Foldkit.Core (Update)
import Foldkit.Html (Html, defaultKeyEvent)
import Foldkit.Query as Query
import Foldkit.Simulation as Simulation

type SceneState model message out = Simulation.State model message out

type SceneEnv model message out =
  { update :: model -> message -> Update model message out
  , view :: model -> Html message
  , scope :: Maybe Query.Locator
  }

type SceneM model message out =
  ReaderT (SceneEnv model message out) (ExceptT String (State (SceneState model message out)))

type Scene model message out = SceneM model message out Unit

runScene ::
  forall model message out.
  (model -> message -> Update model message out) ->
  (model -> Html message) ->
  model ->
  Scene model message out ->
  Either String (SceneState model message out)
runScene updateFn viewFn initial scene =
  let
    env = { update: updateFn, view: viewFn, scope: Nothing }
    Tuple result finalState =
      runState (runExceptT (runReaderT scene env)) (Simulation.initial initial)
  in
    case result of
      Left errorMessage -> Left errorMessage
      Right _ -> case Simulation.unresolvedError finalState.commands of
        Nothing -> Right finalState
        Just errorMessage -> Left errorMessage

send :: forall model message out. message -> Scene model message out
send message = do
  updateFn <- asks _.update
  Simulation.send "when you sent a new Message" updateFn message

click :: forall model message out. Query.Locator -> Scene model message out
click locator = do
  ref <- resolveRef locator
  case Query.findClickHandler ref of
    Just message -> dispatch message
    Nothing ->
      if Query.isSubmitButton ref
      then case Query.findSubmitHandler ref of
        Just message -> dispatch message
        Nothing -> throwError
          ( "I found an element matching "
              <> Query.describe locator
              <> " but it has no click handler and no submit handler on its form."
          )
      else throwError
        ( "I found an element matching "
            <> Query.describe locator
            <> " but neither it nor an ancestor has a click handler."
        )

typeText :: forall model message out. Query.Locator -> String -> Scene model message out
typeText locator value = do
  ref <- resolveRef locator
  case Query.findInputHandler ref of
    Nothing -> throwError
      ("I found an element matching " <> Query.describe locator <> " but it has no input handler.")
    Just handler -> dispatch (handler value)

submit :: forall model message out. Query.Locator -> Scene model message out
submit locator = do
  ref <- resolveRef locator
  case Query.findSubmitHandler ref of
    Nothing -> throwError
      ("I found an element matching " <> Query.describe locator <> " but it has no submit handler.")
    Just message -> dispatch message

press :: forall model message out. Query.Locator -> String -> Scene model message out
press locator key = do
  ref <- resolveRef locator
  case Query.findKeyDownHandler ref of
    Nothing -> throwError
      ( "I found an element matching "
          <> Query.describe locator
          <> " but it has no keydown handler."
      )
    Just handler -> dispatch (handler defaultKeyEvent { key = key })

check :: forall model message out. Query.Locator -> Scene model message out
check locator = do
  ref <- resolveRef locator
  case Query.findCheckHandler ref of
    Nothing -> throwError
      ("I found an element matching " <> Query.describe locator <> " but it has no change handler.")
    Just handler -> dispatch (handler (not (Query.isChecked ref.element)))

within ::
  forall model message out. Query.Locator -> Scene model message out -> Scene model message out
within locator action = do
  env <- ask
  let
    scoped = case env.scope of
      Nothing -> locator
      Just parent -> Query.within parent locator
  local (\current -> current { scope = Just scoped }) action

expectExists :: forall model message out. Query.Locator -> Scene model message out
expectExists locator = void (resolveRef locator)

expectAbsent :: forall model message out. Query.Locator -> Scene model message out
expectAbsent locator = do
  root <- currentRoot
  scoped <- scopedLocator locator
  case Query.find scoped root of
    Nothing -> pure unit
    Just _ ->
      throwError ("Expected no element matching " <> Query.describe scoped <> " but one exists.")

expectElement ::
  forall model message out.
  Query.Locator -> (Query.NodeRef message -> Maybe String) -> Scene model message out
expectElement locator check = do
  ref <- resolveRef locator
  case check ref of
    Nothing -> pure unit
    Just errorMessage -> throwError errorMessage

expectText :: forall model message out. Query.Locator -> String -> Scene model message out
expectText locator expected = expectElement locator \ref ->
  let
    actual = Query.normalizeWhitespace (Query.textContent (Query.targetHtml ref))
    wanted = Query.normalizeWhitespace expected
  in
    if actual == wanted
    then Nothing
    else Just
      ( "Expected "
          <> Query.describe locator
          <> " to have text \""
          <> wanted
          <> "\" but received \""
          <> actual
          <> "\"."
      )

expectTextContaining :: forall model message out. Query.Locator -> String -> Scene model message out
expectTextContaining locator expected = expectElement locator \ref ->
  let
    actual = Query.normalizeWhitespace (Query.textContent (Query.targetHtml ref))
    wanted = Query.normalizeWhitespace expected
  in
    if Str.contains (Str.Pattern wanted) actual
    then Nothing
    else Just
      ( "Expected "
          <> Query.describe locator
          <> " to contain text \""
          <> wanted
          <> "\" but received \""
          <> actual
          <> "\"."
      )

expectValue :: forall model message out. Query.Locator -> String -> Scene model message out
expectValue locator expected = expectElement locator \ref ->
  case Query.attributeValue "value" (Query.targetHtml ref) of
    Just actual
      | actual == expected -> Nothing
      | otherwise -> Just
        ( "Expected "
            <> Query.describe locator
            <> " to have value \""
            <> expected
            <> "\" but received \""
            <> actual
            <> "\"."
        )
    Nothing -> Just
      ( "Expected "
          <> Query.describe locator
          <> " to have value \""
          <> expected
          <> "\" but it has no value."
      )

expectDisabled :: forall model message out. Query.Locator -> Scene model message out
expectDisabled locator = expectElement locator \ref ->
  if Query.isDisabled ref.element
  then Nothing
  else Just ("Expected " <> Query.describe locator <> " to be disabled but it is enabled.")

expectEnabled :: forall model message out. Query.Locator -> Scene model message out
expectEnabled locator = expectElement locator \ref ->
  if Query.isDisabled ref.element
  then Just ("Expected " <> Query.describe locator <> " to be enabled but it is disabled.")
  else Nothing

expectChecked :: forall model message out. Query.Locator -> Scene model message out
expectChecked locator = expectElement locator \ref ->
  if Query.isChecked ref.element
  then Nothing
  else Just ("Expected " <> Query.describe locator <> " to be checked but it is not.")

expectAttribute ::
  forall model message out. Query.Locator -> String -> String -> Scene model message out
expectAttribute locator attribute expected = expectElement locator \ref ->
  case Query.attributeValue attribute (Query.targetHtml ref) of
    Just actual
      | actual == expected -> Nothing
      | otherwise -> Just
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
    Nothing -> Just
      ( "Expected "
          <> Query.describe locator
          <> " to have attribute "
          <> attribute
          <> "=\""
          <> expected
          <> "\" but the attribute is absent."
      )

expectAccessibleName :: forall model message out. Query.Locator -> String -> Scene model message out
expectAccessibleName locator expected = do
  root <- currentRoot
  expectElement locator \ref ->
    let
      actual = Query.normalizeWhitespace (Query.accessibleName root (Query.targetHtml ref))
      wanted = Query.normalizeWhitespace expected
    in
      if actual == wanted
      then Nothing
      else Just
        ( "Expected "
            <> Query.describe locator
            <> " to have accessible name \""
            <> wanted
            <> "\" but received \""
            <> actual
            <> "\"."
        )

expectCount :: forall model message out. Int -> Query.Locator -> Scene model message out
expectCount expected locator = do
  root <- currentRoot
  scoped <- scopedLocator locator
  let
    actual = Array.length (Query.findAll scoped root)
  if actual == expected
  then pure unit
  else throwError
    ( "Expected "
        <> show expected
        <> " elements matching "
        <> Query.describe scoped
        <> " but found "
        <> show actual
        <> "."
    )

resolve ::
  forall args result model message out.
  Definition args result message -> result -> Scene model message out
resolve definition result = do
  updateFn <- asks _.update
  Simulation.resolve updateFn definition result

resolveExact :: forall model message out. Command message -> message -> Scene model message out
resolveExact commandValue resultMessage = do
  updateFn <- asks _.update
  Simulation.resolveExact updateFn commandValue resultMessage

expectCommands :: forall model message out. Array (Matcher message) -> Scene model message out
expectCommands = Simulation.expectCommands

expectHasCommands :: forall model message out. Array (Matcher message) -> Scene model message out
expectHasCommands = Simulation.expectHasCommands

expectNone :: forall model message out. Scene model message out
expectNone = Simulation.expectNone

expectOutMessage :: forall model message out. Eq out => Show out => out -> Scene model message out
expectOutMessage = Simulation.expectOutMessage

expectNoOutMessage :: forall model message out. Scene model message out
expectNoOutMessage = Simulation.expectNoOutMessage

dispatch :: forall model message out. message -> Scene model message out
dispatch message = do
  updateFn <- asks _.update
  Simulation.send "when an interaction dispatched a new Message" updateFn message

currentRoot :: forall model message out. SceneM model message out (Html message)
currentRoot = do
  env <- ask
  state <- get
  pure (env.view state.model)

scopedLocator :: forall model message out. Query.Locator -> SceneM model message out Query.Locator
scopedLocator locator = do
  scope <- asks _.scope
  pure case scope of
    Nothing -> locator
    Just parent -> Query.within parent locator

resolveRef ::
  forall model message out. Query.Locator -> SceneM model message out (Query.NodeRef message)
resolveRef locator = do
  root <- currentRoot
  scoped <- scopedLocator locator
  case Query.findRef scoped root of
    Just ref -> pure ref
    Nothing -> throwError
      ( "I could not find an element matching "
          <> Query.describe scoped
          <> ".\n\nCheck that the view renders it in the current state."
      )
