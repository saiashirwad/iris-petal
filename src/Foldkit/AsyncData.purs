module Foldkit.AsyncData
  ( AsyncData(..)
  , isIdle
  , isLoading
  , isPending
  , isSuccess
  , isFailure
  , getData
  , getError
  , map
  , settle
  , revalidate
  , revalidateOrLoad
  , loadIfMissing
  )
  where

import Prelude

import Data.Either (Either(..))
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

settle :: forall value error. AsyncData value error -> Either error value -> AsyncData value error
settle previous result = case result of
  Right value -> Success value
  Left error -> case getData previous of
    Just value -> Stale value error
    Nothing -> Failure error

revalidate :: forall value error. AsyncData value error -> Maybe (AsyncData value error)
revalidate = case _ of
  Success value -> Just (Refreshing value)
  Stale value _ -> Just (Refreshing value)
  _ -> Nothing

revalidateOrLoad :: forall value error. AsyncData value error -> Maybe (AsyncData value error)
revalidateOrLoad = case _ of
  Idle -> Just Loading
  Failure _ -> Just Loading
  Success value -> Just (Refreshing value)
  Stale value _ -> Just (Refreshing value)
  _ -> Nothing

loadIfMissing :: forall value error. AsyncData value error -> Maybe (AsyncData value error)
loadIfMissing = case _ of
  Idle -> Just Loading
  Failure _ -> Just Loading
  _ -> Nothing
