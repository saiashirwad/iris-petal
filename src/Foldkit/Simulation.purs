module Foldkit.Simulation
  ( State
  , initial
  , apply
  , formatCommands
  , pendingError
  , unresolvedError
  , resolveBy
  , exactError
  , subsetError
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Ord (comparing)
import Foldkit.Command (Command)
import Foldkit.Command as Command
import Foldkit.Core (Update)

type State model message out =
  { model :: model
  , commands :: Array (Command message)
  , outMessage :: Maybe out
  }

initial :: forall model message out. model -> State model message out
initial model =
  { model
  , commands: []
  , outMessage: Nothing
  }

apply
  :: forall model message out
   . (model -> message -> Update model message out)
  -> message
  -> State model message out
  -> State model message out
apply updateFn message state =
  let
    result = updateFn state.model message
  in
    state
      { model = result.model
      , commands = state.commands <> result.commands
      , outMessage = result.outMessage
      }

formatCommands :: forall message. Array (Command message) -> String
formatCommands commands =
  case Array.uncons commands of
    Nothing -> "    (none)"
    Just _ ->
      intercalate "\n"
        (map (\commandValue -> "    " <> Command.describe commandValue) commands)

pendingError :: forall message. String -> Array (Command message) -> Maybe String
pendingError context commands =
  if Array.null commands then Nothing
  else
    Just
      ( "I found unresolved Commands "
          <> context
          <> ":\n\n"
          <> formatCommands commands
          <> "\n\nResolve all Commands before sending the next Message."
      )

unresolvedError :: forall message. Array (Command message) -> Maybe String
unresolvedError commands =
  if Array.null commands then Nothing
  else
    Just
      ( "I found Commands without resolvers:\n\n"
          <> formatCommands commands
          <> "\n\nEvery Command produced by update needs to be resolved."
      )

resolveBy
  :: forall model message out
   . String
  -> (Command message -> Boolean)
  -> (model -> message -> Update model message out)
  -> message
  -> State model message out
  -> Either String (State model message out)
resolveBy description predicate updateFn message state =
  case Array.length matching of
    0 ->
      Left
        ( "I tried to resolve \""
            <> description
            <> "\" but no matching pending Command was found.\n\nPending Commands:\n"
            <> formatCommands state.commands
            <> "\n\nMake sure the previous Message produced this Command."
        )
    1 ->
      case removeFirstBy predicate state.commands of
        Nothing ->
          Left "I found a matching Command but could not remove it."
        Just remaining ->
          Right (apply updateFn message state { commands = remaining })
    _ ->
      Left
        ( "I tried to resolve \""
            <> description
            <> "\" but multiple pending Commands match.\n\nMatches:\n"
            <> formatCommands matching
            <> "\n\nPass a Command instance with specific arguments to disambiguate."
        )
  where
  matching = Array.filter predicate state.commands

exactError
  :: forall message
   . Array (Command message)
  -> Array (Command message)
  -> Maybe String
exactError expected actual =
  if matchesExact expected actual then Nothing
  else
    Just
      ( "Expected exactly these Commands:\n\n"
          <> formatSorted expected
          <> "\n\nBut found:\n\n"
          <> formatSorted actual
      )

subsetError
  :: forall message
   . Array (Command message)
  -> Array (Command message)
  -> Maybe String
subsetError expected actual =
  if matchesSubset expected actual then Nothing
  else
    Just
      ( "Expected to find Commands:\n\n"
          <> formatSorted expected
          <> "\n\nBut the pending Commands are:\n\n"
          <> formatSorted actual
      )

matchesExact :: forall message. Array (Command message) -> Array (Command message) -> Boolean
matchesExact expected actual =
  case Array.uncons expected of
    Nothing -> Array.null actual
    Just { head: commandValue, tail } ->
      case removeFirstBy (Command.matches commandValue) actual of
        Nothing -> false
        Just remaining -> matchesExact tail remaining

matchesSubset :: forall message. Array (Command message) -> Array (Command message) -> Boolean
matchesSubset expected actual =
  case Array.uncons expected of
    Nothing -> true
    Just { head: commandValue, tail } ->
      case removeFirstBy (Command.matches commandValue) actual of
        Nothing -> false
        Just remaining -> matchesSubset tail remaining

removeFirstBy :: forall value. (value -> Boolean) -> Array value -> Maybe (Array value)
removeFirstBy predicate values =
  case Array.uncons values of
    Nothing -> Nothing
    Just { head, tail } ->
      if predicate head then Just tail
      else Array.cons head <$> removeFirstBy predicate tail

formatSorted :: forall message. Array (Command message) -> String
formatSorted commands =
  case Array.uncons commands of
    Nothing -> "    (none)"
    Just _ ->
      intercalate "\n"
        ( map
            (\commandValue -> "    " <> Command.describe commandValue)
            (Array.sortBy (comparing Command.describe) commands)
        )
