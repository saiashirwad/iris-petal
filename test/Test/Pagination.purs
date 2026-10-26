module Test.Pagination where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Foldkit.AsyncData (AsyncData(..))
import Foldkit.Pagination as Pagination
import Foldkit.Test (Test, assertEqual, assertTrue, test)

type Item = String

loaded :: Pagination.Model Item Int String
loaded = Pagination.init { status = Loading, generation = 1 }

pageOne :: Pagination.Page Item Int
pageOne = { items: [ "a", "b" ], next: Just 1 }

pageTwo :: Pagination.Page Item Int
pageTwo = { items: [ "c" ], next: Nothing }

tests :: Array Test
tests =
  [ test "a matching page appends and advances the cursor" do
      let
        result = Pagination.update (Pagination.CompletedFetch 1 (Right pageOne)) loaded
      assertEqual "items" [ "a", "b" ] result.items
      assertEqual "next" (Just 1) result.next
      assertTrue "has more" result.hasMore
  , test "a duplicate response for a settled page is ignored" do
      let
        once = Pagination.update (Pagination.CompletedFetch 1 (Right pageOne)) loaded
        twice = Pagination.update (Pagination.CompletedFetch 1 (Right pageTwo)) once
      assertEqual "items" [ "a", "b" ] twice.items
      assertEqual "next" (Just 1) twice.next
  , test "a stale generation is ignored" do
      let
        result = Pagination.update (Pagination.CompletedFetch 0 (Right pageTwo)) loaded
      assertEqual "items" [] result.items
      assertEqual "status" Loading result.status
  , test "a failure keeps the loaded items and retry is allowed" do
      let
        withItems = Pagination.update (Pagination.CompletedFetch 1 (Right pageOne)) loaded
        pendingAgain = withItems { status = Loading, generation = 2 }
        failed = Pagination.update (Pagination.CompletedFetch 2 (Left "offline")) pendingAgain
      assertEqual "items" [ "a", "b" ] failed.items
      assertEqual "status" (Failure "offline") failed.status
      assertTrue "has more" failed.hasMore
  ]
