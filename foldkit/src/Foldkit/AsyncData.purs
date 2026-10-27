-- | Remote data. `Loading` and `Failure` keep the last good value, if any, so
-- | a refresh or a failed refresh never drops what the view already shows.
module Foldkit.AsyncData (AsyncData(..), toMaybe, isLoading) where

import Prelude

import Data.Maybe (Maybe(..))

data AsyncData error value
  = NotAsked
  | Loading (Maybe value)
  | Failure error (Maybe value)
  | Success value

derive instance eqAsyncData :: (Eq error, Eq value) => Eq (AsyncData error value)
derive instance functorAsyncData :: Functor (AsyncData error)

instance showAsyncData :: (Show error, Show value) => Show (AsyncData error value) where
  show = case _ of
    NotAsked -> "NotAsked"
    Loading previous -> "(Loading " <> show previous <> ")"
    Failure error previous -> "(Failure " <> show error <> " " <> show previous <> ")"
    Success value -> "(Success " <> show value <> ")"

toMaybe :: forall error value. AsyncData error value -> Maybe value
toMaybe = case _ of
  NotAsked -> Nothing
  Loading previous -> previous
  Failure _ previous -> previous
  Success value -> Just value

isLoading :: forall error value. AsyncData error value -> Boolean
isLoading = case _ of
  Loading _ -> true
  _ -> false
