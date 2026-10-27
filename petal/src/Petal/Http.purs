-- | HTTP for command runners. Commands are inert in tests, so nothing here is
-- | injectable: a test resolves the command with a value and never reaches
-- | `send`. The status and decoding rules are the pure `readJson`, which tests
-- | call directly.
module Petal.Http
  ( Method(..)
  , Request
  , Response
  , HttpError(..)
  , get
  , post
  , send
  , sendJson
  , readJson
  , describeError
  )
  where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String as Str
import Data.Tuple (Tuple(..))
import Data.URL as URL
import Effect.Aff (Aff, try)
import Effect.Aff.HTTP as Fetch
import Effect.Aff.HTTP.Header as FetchHeader
import Effect.Aff.HTTP.Request as FetchRequest
import Effect.Aff.HTTP.Response as FetchResponse
import Effect.Exception as Exception

data Method = GET | POST | PUT | PATCH | DELETE

derive instance eqMethod :: Eq Method

type Request =
  { method :: Method, url :: String, headers :: Array (Tuple String String), body :: Maybe String }

type Response = { status :: Int, body :: String }

data HttpError = BadUrl String | NetworkError String | BadStatus Int String | BadBody String

derive instance eqHttpError :: Eq HttpError

instance showHttpError :: Show HttpError where
  show = describeError

get :: String -> Request
get url = { method: GET, url, headers: [], body: Nothing }

post :: String -> Json -> Request
post url json =
  { method: POST
  , url
  , headers: [ Tuple "Content-Type" "application/json" ]
  , body: Just (stringify json)
  }

-- | A non-2xx status is a `BadStatus`, so callers never inspect `status`.
send :: Request -> Aff (Either HttpError Response)
send request = case URL.fromString request.url of
  Nothing -> pure (Left (BadUrl request.url))
  Just url -> do
    result <- try
      ( Fetch.fetch (toMethod request.method) url
          { headers: FetchHeader.Headers (Map.fromFoldable request.headers)
          , body: maybe FetchRequest.BodyEmpty (\body -> FetchRequest.BodyString body Nothing)
              request.body
          }
      )
    case result of
      Left error -> pure (Left (NetworkError (Exception.message error)))
      Right response -> do
        status <- FetchResponse.status response
        body <- FetchResponse.text response
        pure (checkStatus { status, body })

sendJson ::
  forall value. (Json -> Either JsonDecodeError value) -> Request -> Aff (Either HttpError value)
sendJson decode request = do
  result <- send request
  pure (result >>= readJson decode)

readJson ::
  forall value. (Json -> Either JsonDecodeError value) -> Response -> Either HttpError value
readJson decode response = do
  checked <- checkStatus response
  json <- lmap BadBody (jsonParser checked.body)
  lmap (BadBody <<< printJsonDecodeError) (decode json)

checkStatus :: Response -> Either HttpError Response
checkStatus response =
  if response.status >= 200 && response.status < 300
  then Right response
  else Left (BadStatus response.status response.body)

toMethod :: Method -> FetchRequest.Method
toMethod = case _ of
  GET -> FetchRequest.GET
  POST -> FetchRequest.POST
  PUT -> FetchRequest.PUT
  PATCH -> FetchRequest.PATCH
  DELETE -> FetchRequest.DELETE

describeError :: HttpError -> String
describeError = case _ of
  BadUrl url -> "Invalid URL: " <> url
  NetworkError message -> "Network error: " <> message
  BadStatus status body -> "HTTP " <> show status <> if Str.null body then "" else ": " <> body
  BadBody message -> "Could not decode response: " <> message
