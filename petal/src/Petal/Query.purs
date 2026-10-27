-- | A keyed cache of remote values. Embed `Model` in a parent record and drive
-- | it with `zoom`:
-- |
-- | ```purescript
-- | SelectedPokemon name -> zoom @"details" GotDetail (Query.ensure fetchPokemonDetail name)
-- | GotDetail message -> zoom @"details" GotDetail (Query.update message)
-- | ```
-- |
-- | Each entry carries a generation so a response from a superseded request
-- | is dropped. The generation is captured by the command instance, so a test
-- | resolves the fetch with a plain `Either error value`.
module Petal.Query (Model, Message, init, lookup, ensure, refresh, update) where

import Prelude

import Control.Monad.State.Class (get, gets, modify_, put)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Petal (Update, perform)
import Petal.AsyncData (AsyncData(..), toMaybe)
import Petal.Command (Definition)

newtype Model key error value = Model (Map key (Entry error value))

type Entry error value = { data :: AsyncData error value, generation :: Int }

data Message key error value = Completed key Int (Either error value)

init :: forall key error value. Model key error value
init = Model Map.empty

lookup :: forall key error value. Ord key => key -> Model key error value -> AsyncData error value
lookup key (Model entries) = maybe NotAsked _.data (Map.lookup key entries)

-- | Fetch `key` unless it is cached or already loading. A failed entry fetches
-- | again.
ensure ::
  forall key error value.
  Ord key =>
  Definition key (Either error value) ->
  key ->
  Update (Model key error value) (Message key error value) Unit
ensure definition key = do
  current <- gets (lookup key)
  case current of
    NotAsked -> start definition key
    Failure _ _ -> start definition key
    Loading _ -> pure unit
    Success _ -> pure unit

-- | Fetch `key` again, keeping the cached value visible while it loads. A
-- | request already in flight is superseded.
refresh ::
  forall key error value.
  Ord key =>
  Definition key (Either error value) ->
  key ->
  Update (Model key error value) (Message key error value) Unit
refresh = start

update ::
  forall key error value.
  Ord key =>
  Message key error value ->
  Update (Model key error value) (Message key error value) Unit
update (Completed key generation result) = modify_ \(Model entries) ->
  case Map.lookup key entries of
    Just entry
      | entry.generation == generation ->
        Model (Map.insert key entry { data = settle entry.data result } entries)
    _ -> Model entries

start ::
  forall key error value.
  Ord key =>
  Definition key (Either error value) ->
  key ->
  Update (Model key error value) (Message key error value) Unit
start definition key = do
  Model entries <- get
  let
    previous = Map.lookup key entries
    generation = maybe 1 (add 1 <<< _.generation) previous
    data_ = Loading (previous >>= toMaybe <<< _.data)
  put (Model (Map.insert key { data: data_, generation } entries))
  perform definition key (Completed key generation)

settle :: forall error value. AsyncData error value -> Either error value -> AsyncData error value
settle previous = case _ of
  Right value -> Success value
  Left error -> Failure error (toMaybe previous)
