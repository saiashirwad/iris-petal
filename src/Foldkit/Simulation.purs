module Foldkit.Simulation
  ( State
  , initial
  , apply
  , send
  , resolve
  , resolveExact
  , expectCommands
  , expectHasCommands
  , expectNone
  , expectModelWith
  , expectOutMessage
  , expectNoOutMessage
  , formatCommands
  , pendingError
  , unresolvedError
  , exactError
  , subsetError
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.State (class MonadState, get, put)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Ord (comparing)
import Foldkit.Command (Command, Definition, Matcher)
import Foldkit.Command as Command
import Foldkit.Core (Update)

type State model message out =
  { model :: model, commands :: Array (Command message), outMessage :: Maybe out }

initial :: forall model message out. model -> State model message out
initial model = { model, commands: [], outMessage: Nothing }

apply ::
  forall model message out.
  (model -> message -> Update model message out) ->
  message ->
  State model message out ->
  State model message out
apply updateFn message state =
  let
    result = updateFn state.model message
  in
    state
      { model = result.model
      , commands = state.commands <> result.commands
      , outMessage = result.outMessage
      }

send ::
  forall m model message out.
  MonadError String m =>
  MonadState (State model message out) m =>
  String ->
  (model -> message -> Update model message out) ->
  message ->
  m Unit
send context updateFn message = do
  state <- get
  case pendingError context state.commands of
    Just errorMessage -> throwError errorMessage
    Nothing -> put (apply updateFn message state)

resolve ::
  forall m model message out args result.
  MonadError String m =>
  MonadState (State model message out) m =>
  (model -> message -> Update model message out) ->
  Definition args result message ->
  result ->
  m Unit
resolve updateFn definition result = do
  state <- get
  case
    resolveBy (Command.describeDefinition definition) (Command.matchesDefinition definition)
      updateFn (Command.liftResult definition result) state
  of
    Left errorMessage -> throwError errorMessage
    Right next -> put next

resolveExact ::
  forall m model message out.
  MonadError String m =>
  MonadState (State model message out) m =>
  (model -> message -> Update model message out) ->
  Command message ->
  message ->
  m Unit
resolveExact updateFn commandValue resultMessage = do
  state <- get
  case
    resolveFirstBy (Command.describe commandValue) (Command.matches commandValue) updateFn
      resultMessage state
  of
    Left errorMessage -> throwError errorMessage
    Right next -> put next

expectCommands ::
  forall m model message out.
  MonadError String m => MonadState (State model message out) m => Array (Matcher message) -> m Unit
expectCommands expected = do
  state <- get
  case exactError expected state.commands of
    Nothing -> pure unit
    Just errorMessage -> throwError errorMessage

expectHasCommands ::
  forall m model message out.
  MonadError String m => MonadState (State model message out) m => Array (Matcher message) -> m Unit
expectHasCommands expected = do
  state <- get
  case subsetError expected state.commands of
    Nothing -> pure unit
    Just errorMessage -> throwError errorMessage

expectNone ::
  forall m model message out.
  MonadError String m => MonadState (State model message out) m => m Unit
expectNone = expectCommands []

expectModelWith ::
  forall m model message out.
  MonadError String m =>
  MonadState (State model message out) m =>
  (model -> String) ->
  String ->
  (model -> Boolean) ->
  m Unit
expectModelWith render description predicate = do
  state <- get
  if predicate state.model
  then pure unit
  else throwError
    ("Expected model to satisfy: " <> description <> "\n\nActual model:\n" <> render state.model)

expectOutMessage ::
  forall m model message out.
  MonadError String m =>
  MonadState (State model message out) m =>
  Eq out =>
  Show out =>
  out ->
  m Unit
expectOutMessage expected = do
  state <- get
  case state.outMessage of
    Just actual
      | actual == expected -> pure unit
      | otherwise -> throwError
        ( "Expected OutMessage "
            <> show expected
            <> " but the last update emitted "
            <> show actual
            <> "."
        )
    Nothing ->
      throwError ("Expected OutMessage " <> show expected <> " but the last update emitted none.")

expectNoOutMessage ::
  forall m model message out.
  MonadError String m => MonadState (State model message out) m => m Unit
expectNoOutMessage = do
  state <- get
  case state.outMessage of
    Nothing -> pure unit
    Just _ -> throwError "Expected no OutMessage but the last update emitted one."

formatCommands :: forall message. Array (Command message) -> String
formatCommands commands = case Array.uncons commands of
  Nothing -> "    (none)"
  Just _ ->
    intercalate "\n" (map (\commandValue -> "    " <> Command.describe commandValue) commands)

pendingError :: forall message. String -> Array (Command message) -> Maybe String
pendingError context commands =
  if Array.null commands
  then Nothing
  else Just
    ( "I found unresolved Commands "
        <> context
        <> ":\n\n"
        <> formatCommands commands
        <> "\n\nResolve all Commands before sending the next Message."
    )

unresolvedError :: forall message. Array (Command message) -> Maybe String
unresolvedError commands =
  if Array.null commands
  then Nothing
  else Just
    ( "I found Commands without resolvers:\n\n"
        <> formatCommands commands
        <> "\n\nEvery Command produced by update needs to be resolved."
    )

resolveBy ::
  forall model message out.
  String ->
  (Command message -> Boolean) ->
  (model -> message -> Update model message out) ->
  message ->
  State model message out ->
  Either String (State model message out)
resolveBy description predicate updateFn message state = case Array.length matching of
  0 -> Left
    ( "I tried to resolve \""
        <> description
        <> "\" but no matching pending Command was found.\n\nPending Commands:\n"
        <> formatCommands state.commands
        <> "\n\nMake sure the previous Message produced this Command."
    )
  1 -> case removeFirstBy predicate state.commands of
    Nothing -> Left "I found a matching Command but could not remove it."
    Just remaining -> Right (apply updateFn message state { commands = remaining })
  _ -> Left
    ( "I tried to resolve \""
        <> description
        <> "\" but multiple pending Commands match.\n\nMatches:\n"
        <> formatCommands matching
        <> "\n\nPass a Command instance with specific arguments to disambiguate."
    )
  where
  matching = Array.filter predicate state.commands

resolveFirstBy ::
  forall model message out.
  String ->
  (Command message -> Boolean) ->
  (model -> message -> Update model message out) ->
  message ->
  State model message out ->
  Either String (State model message out)
resolveFirstBy description predicate updateFn message state =
  case removeFirstBy predicate state.commands of
    Nothing -> Left (missingCommandError description state.commands)
    Just remaining -> Right (apply updateFn message state { commands = remaining })

missingCommandError :: forall message. String -> Array (Command message) -> String
missingCommandError description commands = "I tried to resolve \""
  <> description
  <> "\" but no matching pending Command was found.\n\nPending Commands:\n"
  <> formatCommands commands
  <> "\n\nMake sure the previous Message produced this Command."

exactError :: forall message. Array (Matcher message) -> Array (Command message) -> Maybe String
exactError expected actual =
  if matchesExact expected actual
  then Nothing
  else Just
    ( "Expected exactly these Commands:\n\n"
        <> formatMatchers expected
        <> "\n\nBut found:\n\n"
        <> formatSorted actual
    )

subsetError :: forall message. Array (Matcher message) -> Array (Command message) -> Maybe String
subsetError expected actual =
  if matchesSubset expected actual
  then Nothing
  else Just
    ( "Expected to find Commands:\n\n"
        <> formatMatchers expected
        <> "\n\nBut the pending Commands are:\n\n"
        <> formatSorted actual
    )

matchesExact :: forall message. Array (Matcher message) -> Array (Command message) -> Boolean
matchesExact expected actual = case Array.uncons expected of
  Nothing -> Array.null actual
  Just { head: matcher, tail } -> case removeFirstBy (Command.matcherMatches matcher) actual of
    Nothing -> false
    Just remaining -> matchesExact tail remaining

matchesSubset :: forall message. Array (Matcher message) -> Array (Command message) -> Boolean
matchesSubset expected actual = case Array.uncons expected of
  Nothing -> true
  Just { head: matcher, tail } -> case removeFirstBy (Command.matcherMatches matcher) actual of
    Nothing -> false
    Just remaining -> matchesSubset tail remaining

removeFirstBy :: forall value. (value -> Boolean) -> Array value -> Maybe (Array value)
removeFirstBy predicate values = case Array.uncons values of
  Nothing -> Nothing
  Just { head, tail } ->
    if predicate head then Just tail else Array.cons head <$> removeFirstBy predicate tail

formatMatchers :: forall message. Array (Matcher message) -> String
formatMatchers matchers = case Array.uncons matchers of
  Nothing -> "    (none)"
  Just _ -> intercalate "\n"
    ( map (\matcher -> "    " <> Command.describeMatcher matcher)
        (Array.sortBy (comparing Command.describeMatcher) matchers)
    )

formatSorted :: forall message. Array (Command message) -> String
formatSorted commands = case Array.uncons commands of
  Nothing -> "    (none)"
  Just _ -> intercalate "\n"
    ( map (\commandValue -> "    " <> Command.describe commandValue)
        (Array.sortBy (comparing Command.describe) commands)
    )
