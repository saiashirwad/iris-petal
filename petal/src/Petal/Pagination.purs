-- | A cursor-paginated list. Embed `Model` in a parent record and drive it
-- | with `zoom`:
-- |
-- | ```purescript
-- | ReachedListEnd -> zoom @"list" GotPage (Pagination.loadMore fetchPokemonPage)
-- | GotPage message -> zoom @"list" GotPage (Pagination.update message)
-- | ```
-- |
-- | At most one page is in flight, so a response can only answer the current
-- | request. A response that arrives while nothing is loading is ignored.
module Petal.Pagination
  ( Model
  , Message
  , Page
  , Status(..)
  , init
  , items
  , status
  , loadMore
  , update
  )
  where

import Prelude

import Control.Monad.State.Class (get, modify_)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Petal (Update, perform)
import Petal.Command (Definition)

type Page cursor item = { items :: Array item, next :: Maybe cursor }

newtype Model cursor error item = Model { items :: Array item, phase :: Phase cursor error }

-- | `Maybe cursor` is the request for the next page: `Nothing` asks for the
-- | first page.
data Phase cursor error
  = Ready (Maybe cursor)
  | Fetching (Maybe cursor)
  | Failed (Maybe cursor) error
  | Exhausted

data Message cursor error item = Completed (Either error (Page cursor item))

data Status error = HasMore | LoadingMore | LoadFailed error | AllLoaded

derive instance eqStatus :: Eq error => Eq (Status error)

instance showStatus :: Show error => Show (Status error) where
  show = case _ of
    HasMore -> "HasMore"
    LoadingMore -> "LoadingMore"
    LoadFailed error -> "(LoadFailed " <> show error <> ")"
    AllLoaded -> "AllLoaded"

init :: forall cursor error item. Model cursor error item
init = Model { items: [], phase: Ready Nothing }

items :: forall cursor error item. Model cursor error item -> Array item
items (Model model) = model.items

status :: forall cursor error item. Model cursor error item -> Status error
status (Model model) = case model.phase of
  Ready _ -> HasMore
  Fetching _ -> LoadingMore
  Failed _ error -> LoadFailed error
  Exhausted -> AllLoaded

-- | Request the next page. A no-op while a page is loading or after the last
-- | page. After a failure it retries the same cursor.
loadMore ::
  forall cursor error item.
  Definition (Maybe cursor) (Either error (Page cursor item)) ->
  Update (Model cursor error item) (Message cursor error item) Unit
loadMore definition = do
  Model model <- get
  case model.phase of
    Ready cursor -> fetch cursor
    Failed cursor _ -> fetch cursor
    Fetching _ -> pure unit
    Exhausted -> pure unit
  where
  fetch cursor = do
    modify_ \(Model model) -> Model model { phase = Fetching cursor }
    perform definition cursor Completed

update ::
  forall cursor error item.
  Message cursor error item -> Update (Model cursor error item) (Message cursor error item) Unit
update (Completed result) = modify_ \(Model model) -> case model.phase of
  Fetching cursor -> Model case result of
    Left error -> model { phase = Failed cursor error }
    Right page ->
      model
        { items = model.items <> page.items
        , phase = case page.next of
            Just next
              | not (Array.null page.items) -> Ready (Just next)
            _ -> Exhausted
        }
  _ -> Model model
