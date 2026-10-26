module Foldkit.AsyncData
  ( AsyncData(..)
  , isIdle
  , isLoading
  , isPending
  , isSuccess
  , isFailure
  , hasData
  , getData
  , getError
  , map
  , match
  , revalidate
  ) where

import Prelude

import Data.Maybe (Maybe(..))

data AsyncData value error
  = Idle
  | Loading
  | Success value
  | Failure error
  | Refreshing value
  | Stale value error

derive instance eqAsyncData :: (Eq value, Eq error) => Eq (AsyncData value error)
derive instance ordAsyncData :: (Ord value, Ord error) => Ord (AsyncData value error)
instance showAsyncData :: (Show value, Show error) => Show (AsyncData value error) where
  show = case _ of
    Idle -> "Idle"
    Loading -> "Loading"
    Success value -> "Success (" <> show value <> ")"
    Failure error -> "Failure (" <> show error <> ")"
    Refreshing value -> "Refreshing (" <> show value <> ")"
    Stale value error -> "Stale (" <> show value <> ") (" <> show error <> ")"

isIdle :: forall value error. AsyncData value error -> Boolean
isIdle = case _ of
  Idle -> true
  _ -> false

isLoading :: forall value error. AsyncData value error -> Boolean
isLoading = case _ of
  Loading -> true
  _ -> false

isPending :: forall value error. AsyncData value error -> Boolean
isPending = case _ of
  Loading -> true
  Refreshing _ -> true
  _ -> false

isSuccess :: forall value error. AsyncData value error -> Boolean
isSuccess = case _ of
  Success _ -> true
  Refreshing _ -> true
  Stale _ _ -> true
  _ -> false

isFailure :: forall value error. AsyncData value error -> Boolean
isFailure = case _ of
  Failure _ -> true
  _ -> false

hasData :: forall value error. AsyncData value error -> Boolean
hasData = case _ of
  Success _ -> true
  Refreshing _ -> true
  Stale _ _ -> true
  _ -> false

getData :: forall value error. AsyncData value error -> Maybe value
getData = case _ of
  Success value -> Just value
  Refreshing value -> Just value
  Stale value _ -> Just value
  _ -> Nothing

getError :: forall value error. AsyncData value error -> Maybe error
getError = case _ of
  Failure error -> Just error
  Stale _ error -> Just error
  _ -> Nothing

map :: forall a b error. (a -> b) -> AsyncData a error -> AsyncData b error
map f = case _ of
  Idle -> Idle
  Loading -> Loading
  Success value -> Success (f value)
  Failure error -> Failure error
  Refreshing value -> Refreshing (f value)
  Stale value error -> Stale (f value) error

match
  :: forall value error result
   . { idle :: Unit -> result
     , loading :: Unit -> result
     , success :: value -> result
     , failure :: error -> result
     , refreshing :: value -> result
     , stale :: value -> error -> result
     }
  -> AsyncData value error
  -> result
match handlers = case _ of
  Idle -> handlers.idle unit
  Loading -> handlers.loading unit
  Success value -> handlers.success value
  Failure error -> handlers.failure error
  Refreshing value -> handlers.refreshing value
  Stale value error -> handlers.stale value error

revalidate :: forall value error. AsyncData value error -> AsyncData value error
revalidate = case _ of
  Success value -> Refreshing value
  Stale value error -> Refreshing value
  other -> other
