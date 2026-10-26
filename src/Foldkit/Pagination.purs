module Foldkit.Pagination
  ( Model
  , Message(..)
  , Definition
  , Config
  , Page
  , init
  , fromItems
  , items
  , status
  , hasMore
  , define
  , command
  , liftedCommand
  , update
  , receive
  , loadMore
  )
  where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff (Aff)
import Foldkit.AsyncData (AsyncData(..), isPending)
import Foldkit.Command (Command, CommandName)
import Foldkit.Command as Command
import Foldkit.Core (Update, continue)

type Page item cursor = { items :: Array item, next :: Maybe cursor }

type Model item cursor error =
  { items :: Array item
  , next :: Maybe cursor
  , hasMore :: Boolean
  , status :: AsyncData Unit error
  , generation :: Int
  }

data Message item cursor error = CompletedFetch Int (Either error (Page item cursor))

type Definition cursor item error =
  { name :: CommandName
  , fetch :: Maybe cursor -> Aff (Either error (Page item cursor))
  , fetchCommand ::
      Command.Definition
        { cursor :: Maybe cursor, generation :: Int }
        (Message item cursor error)
        (Message item cursor error)
  }

type Config parentModel parentMessage item cursor error =
  { read :: parentModel -> Model item cursor error
  , write :: parentModel -> Model item cursor error -> parentModel
  , toParentMessage :: Message item cursor error -> parentMessage
  }

init :: forall item cursor error. Model item cursor error
init = { items: [], next: Nothing, hasMore: true, status: Idle, generation: 0 }

fromItems :: forall item cursor error. Array item -> Model item cursor error
fromItems values =
  { items: values, next: Nothing, hasMore: false, status: Success unit, generation: 0 }

items :: forall item cursor error. Model item cursor error -> Array item
items model = model.items

status :: forall item cursor error. Model item cursor error -> AsyncData Unit error
status model = model.status

hasMore :: forall item cursor error. Model item cursor error -> Boolean
hasMore model = model.hasMore

define ::
  forall cursor item error.
  EncodeJson cursor =>
  CommandName ->
  (Maybe cursor -> Aff (Either error (Page item cursor))) ->
  Definition cursor item error
define commandName fetch =
  { name: commandName
  , fetch
  , fetchCommand: Command.define commandName
      ( \args -> do
          result <- fetch args.cursor
          pure (CompletedFetch args.generation result)
      )
      identity
  }

command ::
  forall cursor item error.
  Definition cursor item error ->
  Command.Definition
    { cursor :: Maybe cursor, generation :: Int }
    (Message item cursor error)
    (Message item cursor error)
command definition = definition.fetchCommand

liftedCommand ::
  forall parentModel parentMessage item cursor error.
  Config parentModel parentMessage item cursor error ->
  Definition cursor item error ->
  Command.Definition
    { cursor :: Maybe cursor, generation :: Int }
    (Message item cursor error)
    parentMessage
liftedCommand config definition =
  Command.mapDefinition config.toParentMessage definition.fetchCommand

update ::
  forall item cursor error.
  Message item cursor error -> Model item cursor error -> Model item cursor error
update message model = case message of
  CompletedFetch generation result ->
    if generation /= model.generation || not (isPending model.status)
    then model
    else case result of
      Left error -> model { status = Failure error }
      Right page ->
        model
          { items = model.items <> page.items
          , next = page.next
          , hasMore = isJust page.next && not (Array.null page.items)
          , status = Success unit
          }

receive ::
  forall parentModel parentMessage item cursor error.
  Config parentModel parentMessage item cursor error ->
  Message item cursor error ->
  parentModel ->
  Update parentModel parentMessage Void
receive config message parent = continue (config.write parent (update message (config.read parent)))

loadMore ::
  forall parentModel parentMessage item cursor error.
  EncodeJson cursor =>
  Config parentModel parentMessage item cursor error ->
  Definition cursor item error ->
  parentModel ->
  Update parentModel parentMessage Void
loadMore config definition parent =
  let
    model = config.read parent
  in
    if not model.hasMore || isPending model.status
    then continue parent
    else
      let
        generation = model.generation + 1
        pending = model { status = Loading, generation = generation }
        commandValue = Command.command definition.fetchCommand { cursor: model.next, generation }
      in
        { model: config.write parent pending
        , commands: [ Command.mapMessage config.toParentMessage commandValue ]
        , outMessage: Nothing
        }
