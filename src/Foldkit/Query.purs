module Foldkit.Query
  ( Model
  , Message(..)
  , Definition
  , Config
  , init
  , read
  , reset
  , define
  , command
  , liftedCommand
  , update
  , receive
  , loadIfMissing
  , revalidate
  , revalidateOrLoad
  )
  where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Foldkit.AsyncData (AsyncData(..), isPending, settle)
import Foldkit.AsyncData as AsyncData
import Foldkit.Command (Command, CommandName, Definition)
import Foldkit.Command as Command
import Foldkit.Core (Update, continue)

type Entry value error = { data :: AsyncData value error, generation :: Int }

type Model key value error = { entries :: Map key (Entry value error) }

data Message key value error = CompletedFetch key Int (Either error value)

type Definition key value error =
  { name :: CommandName
  , fetch :: key -> Aff (Either error value)
  , fetchCommand ::
      Command.Definition
        { key :: key, generation :: Int }
        (Message key value error)
        (Message key value error)
  }

type Config parentModel parentMessage key value error =
  { read :: parentModel -> Model key value error
  , write :: parentModel -> Model key value error -> parentModel
  , toParentMessage :: Message key value error -> parentMessage
  }

init :: forall key value error. Model key value error
init = { entries: Map.empty }

reset :: forall key value error. Model key value error
reset = init

read :: forall key value error. Ord key => Model key value error -> key -> AsyncData value error
read model key = maybe Idle _.data (Map.lookup key model.entries)

define ::
  forall key value error.
  EncodeJson key => CommandName -> (key -> Aff (Either error value)) -> Definition key value error
define commandName fetch =
  { name: commandName
  , fetch
  , fetchCommand: Command.define commandName
      ( \args -> do
          result <- fetch args.key
          pure (CompletedFetch args.key args.generation result)
      )
      identity
  }

command ::
  forall key value error.
  Definition key value error ->
  Command.Definition
    { key :: key, generation :: Int }
    (Message key value error)
    (Message key value error)
command definition = definition.fetchCommand

liftedCommand ::
  forall parentModel parentMessage key value error.
  Config parentModel parentMessage key value error ->
  Definition key value error ->
  Command.Definition { key :: key, generation :: Int } (Message key value error) parentMessage
liftedCommand config definition =
  Command.mapDefinition config.toParentMessage definition.fetchCommand

update ::
  forall key value error.
  Ord key => Message key value error -> Model key value error -> Model key value error
update message model = case message of
  CompletedFetch key generation result -> case Map.lookup key model.entries of
    Just entry
      | entry.generation == generation && isPending entry.data ->
        model { entries = Map.insert key entry { data = settle entry.data result } model.entries }
    _ -> model

receive ::
  forall parentModel parentMessage key value error.
  Ord key =>
  Config parentModel parentMessage key value error ->
  Message key value error ->
  parentModel ->
  Update parentModel parentMessage Void
receive config message parent = continue (config.write parent (update message (config.read parent)))

loadIfMissing ::
  forall parentModel parentMessage key value error.
  Ord key =>
  EncodeJson key =>
  Config parentModel parentMessage key value error ->
  Definition key value error ->
  key ->
  parentModel ->
  Update parentModel parentMessage Void
loadIfMissing config definition key parent =
  case AsyncData.loadIfMissing (read (config.read parent) key) of
    Nothing -> continue parent
    Just nextData -> startUpdate config definition key nextData parent

revalidate ::
  forall parentModel parentMessage key value error.
  Ord key =>
  EncodeJson key =>
  Config parentModel parentMessage key value error ->
  Definition key value error ->
  key ->
  parentModel ->
  Update parentModel parentMessage Void
revalidate config definition key parent =
  case AsyncData.revalidate (read (config.read parent) key) of
    Nothing -> continue parent
    Just nextData -> startUpdate config definition key nextData parent

revalidateOrLoad ::
  forall parentModel parentMessage key value error.
  Ord key =>
  EncodeJson key =>
  Config parentModel parentMessage key value error ->
  Definition key value error ->
  key ->
  parentModel ->
  Update parentModel parentMessage Void
revalidateOrLoad config definition key parent =
  case AsyncData.revalidateOrLoad (read (config.read parent) key) of
    Nothing -> continue parent
    Just nextData -> startUpdate config definition key nextData parent

startUpdate ::
  forall parentModel parentMessage key value error.
  Ord key =>
  EncodeJson key =>
  Config parentModel parentMessage key value error ->
  Definition key value error ->
  key ->
  AsyncData value error ->
  parentModel ->
  Update parentModel parentMessage Void
startUpdate config definition key nextData parent =
  let
    started = start definition key nextData (config.read parent)
  in
    { model: config.write parent started.model
    , commands: [ Command.mapMessage config.toParentMessage started.command ]
    , outMessage: Nothing
    }

start ::
  forall key value error.
  Ord key =>
  EncodeJson key =>
  Definition key value error ->
  key ->
  AsyncData value error ->
  Model key value error ->
  { model :: Model key value error, command :: Command (Message key value error) }
start definition key nextData model =
  let
    generation = case Map.lookup key model.entries of
      Just entry -> entry.generation + 1
      Nothing -> 1
    entry = { data: nextData, generation }
  in
    { model: model { entries = Map.insert key entry model.entries }
    , command: Command.command definition.fetchCommand { key, generation }
    }
