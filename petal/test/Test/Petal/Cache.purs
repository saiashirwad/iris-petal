module Test.Petal.Cache (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Petal (Program, zoom)
import Petal.AsyncData (AsyncData(..))
import Petal.Command (Definition, define)
import Petal.Html (div)
import Petal.Pagination (Page, Status(..))
import Petal.Pagination as Pagination
import Petal.Query as Query
import Petal.Test (expectModel, resolve, resolveFor, send)
import Petal.Test as Test
import Test.Petal.Scenario (failsWith)
import Test.Spec (Spec, describe, it)

data Message
  = Load String
  | Refresh String
  | More
  | GotEntry (Query.Message String String Int)
  | GotPage (Pagination.Message Int String String)

type Model =
  { entries :: Query.Model String String Int, pages :: Pagination.Model Int String String }

fetchEntry :: Definition String (Either String Int)
fetchEntry = define "FetchEntry" \_ -> pure (Right 0)

fetchPage :: Definition (Maybe Int) (Either String (Page Int String))
fetchPage = define "FetchPage" \_ -> pure (Right { items: [], next: Nothing })

program :: Program Model Message
program =
  { model: { entries: Query.init, pages: Pagination.init }
  , init: pure unit
  , update: case _ of
      Load key -> zoom @"entries" GotEntry (Query.ensure fetchEntry key)
      Refresh key -> zoom @"entries" GotEntry (Query.refresh fetchEntry key)
      More -> zoom @"pages" GotPage (Pagination.loadMore fetchPage)
      GotEntry message -> zoom @"entries" GotEntry (Query.update message)
      GotPage message -> zoom @"pages" GotPage (Pagination.update message)
  , view: \_ -> div [] []
  }

entry :: String -> Model -> AsyncData String Int
entry key model = Query.lookup key model.entries

spec :: Spec Unit
spec = describe "Query and Pagination" do
  it "caches an entry and skips a second fetch" do
    Test.run program do
      send (Load "a")
      expectModel (entry "a") (Loading Nothing)
      resolve fetchEntry (Right 1)
      expectModel (entry "a") (Success 1)
      send (Load "a")

  it "keeps the value through a failed refresh and retries on ensure" do
    Test.run program do
      send (Load "a")
      resolve fetchEntry (Right 1)
      send (Refresh "a")
      expectModel (entry "a") (Loading (Just 1))
      resolve fetchEntry (Left "offline")
      expectModel (entry "a") (Failure "offline" (Just 1))
      send (Load "a")
      resolve fetchEntry (Right 2)
      expectModel (entry "a") (Success 2)

  it "appends pages and stops after the last one" do
    Test.run program do
      send More
      resolveFor fetchPage Nothing (Right { items: [ "a", "b" ], next: Just 2 })
      expectModel (Pagination.status <<< _.pages) HasMore
      send More
      resolveFor fetchPage (Just 2) (Right { items: [ "c" ], next: Nothing })
      expectModel (Pagination.items <<< _.pages) [ "a", "b", "c" ]
      expectModel (Pagination.status <<< _.pages) AllLoaded
      send More

  it "retries the failed cursor" do
    Test.run program do
      send More
      resolve fetchPage (Right { items: [ "a" ], next: Just 1 })
      send More
      resolve fetchPage (Left "offline")
      expectModel (Pagination.status <<< _.pages) (LoadFailed "offline")
      send More
      resolveFor fetchPage (Just 1) (Right { items: [ "b" ], next: Nothing })
      expectModel (Pagination.items <<< _.pages) [ "a", "b" ]

  it "issues one request while a page is loading" do
    failsWith "unresolved Commands" $ Test.evaluate program do
      send More
      send More
