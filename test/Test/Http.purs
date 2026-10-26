module Test.Http where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Effect.Aff (Aff)
import Foldkit.Http as Http
import Foldkit.Test (Test, assertEqual, assertTrue, test)

okClient :: Http.Client
okClient = Http.pureClient \_ ->
  Right { status: 200, statusText: "OK", headers: Map.empty, body: "{\"hello\":\"world\"}" }

missingClient :: Http.Client
missingClient = Http.pureClient \_ ->
  Right { status: 404, statusText: "Not Found", headers: Map.empty, body: "missing" }

tests :: Array Test
tests =
  [ test "a client returns the response body" do
      result <- Http.send okClient (Http.get "https://example.test")
      case result of
        Right response -> assertEqual "body" "{\"hello\":\"world\"}" response.body
        Left error -> assertTrue ("unexpected " <> Http.describeError error) false
  , test "non-2xx responses become StatusError" do
      result <- Http.send missingClient (Http.get "https://example.test")
      case result of
        Right _ -> assertTrue "expected a status error" false
        Left error -> case error of
          Http.StatusError 404 _ "missing" -> pure unit
          _ -> assertTrue ("unexpected " <> Http.describeError error) false
  ]
