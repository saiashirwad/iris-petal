module Foldkit.Http
  ( Method(..)
  , Request
  , Response
  , HttpError(..)
  , Client
  , defaultRequest
  , get
  , post
  , postJson
  , withHeader
  , withHeaders
  , withBody
  , send
  , sendJson
  , guardStatus
  , pureClient
  , browserClient
  , describeError
  )
  where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.Tuple (Tuple(..))
import Data.URL as URL
import Effect.Aff (Aff, try)
import Effect.Aff.HTTP as Http
import Effect.Aff.HTTP.Header as HttpHeader
import Effect.Aff.HTTP.Request as HttpRequest
import Effect.Aff.HTTP.Response as HttpResponse
import Effect.Exception as Exception

data Method = GET | POST | PUT | PATCH | DELETE | HEAD

derive instance eqMethod :: Eq Method
derive instance ordMethod :: Ord Method

instance showMethod :: Show Method where
  show = case _ of
    GET -> "GET"
    POST -> "POST"
    PUT -> "PUT"
    PATCH -> "PATCH"
    DELETE -> "DELETE"
    HEAD -> "HEAD"

type Request =
  { method :: Method, url :: String, headers :: Array (Tuple String String), body :: Maybe String }

type Response =
  { status :: Int, statusText :: String, headers :: Map String String, body :: String }

data HttpError
  = InvalidUrl String
  | NetworkError String
  | StatusError Int String String
  | DecodeError String

derive instance eqHttpError :: Eq HttpError

instance showHttpError :: Show HttpError where
  show = describeError

type Client = { send :: Request -> Aff (Either HttpError Response) }

defaultRequest :: String -> Request
defaultRequest url = { method: GET, url, headers: [], body: Nothing }

get :: String -> Request
get = defaultRequest

post :: String -> String -> Request
post url body =
  { method: POST, url, headers: [ Tuple "Content-Type" "application/json" ], body: Just body }

postJson :: String -> Json -> Request
postJson url = post url <<< stringify

withHeader :: String -> String -> Request -> Request
withHeader key value request = request { headers = Array.snoc request.headers (Tuple key value) }

withHeaders :: Array (Tuple String String) -> Request -> Request
withHeaders values request = request { headers = request.headers <> values }

withBody :: String -> Request -> Request
withBody body request = request { body = Just body }

send :: Client -> Request -> Aff (Either HttpError Response)
send client request = client.send request

sendJson :: forall value. DecodeJson value => Client -> Request -> Aff (Either HttpError value)
sendJson client request = do
  result <- client.send request
  pure do
    response <- result
    json <- lmap DecodeError (jsonParser response.body)
    lmap (DecodeError <<< printJsonDecodeError) (decodeJson json)

guardStatus :: Response -> Either HttpError Response
guardStatus response =
  if response.status >= 200 && response.status < 300
  then Right response
  else Left (StatusError response.status response.statusText response.body)

pureClient :: (Request -> Either HttpError Response) -> Client
pureClient handler = { send: pure <<< (\request -> handler request >>= guardStatus) }

browserClient :: Client
browserClient =
  { send: \request -> case URL.fromString request.url of
      Nothing -> pure (Left (InvalidUrl request.url))
      Just url -> do
        result <- try
          ( Http.fetch (toMethod request.method) url
              { headers: toHeaders request.headers, body: toBody request.body }
          )
        case result of
          Left error -> pure (Left (NetworkError (Exception.message error)))
          Right response -> do
            status <- HttpResponse.status response
            statusText <- HttpResponse.statusText response
            headers <- HttpResponse.headers response
            body <- HttpResponse.text response
            pure (guardStatus { status, statusText, headers, body })
  }

toMethod :: Method -> HttpRequest.Method
toMethod = case _ of
  GET -> HttpRequest.GET
  POST -> HttpRequest.POST
  PUT -> HttpRequest.PUT
  PATCH -> HttpRequest.PATCH
  DELETE -> HttpRequest.DELETE
  HEAD -> HttpRequest.HEAD

toHeaders :: Array (Tuple String String) -> HttpHeader.Headers
toHeaders values = HttpHeader.Headers (Map.fromFoldable values)

toBody :: Maybe String -> HttpRequest.Body
toBody = case _ of
  Nothing -> HttpRequest.BodyEmpty
  Just body -> HttpRequest.BodyString body Nothing

describeError :: HttpError -> String
describeError = case _ of
  InvalidUrl url -> "Invalid URL: " <> url
  NetworkError message -> "Network error: " <> message
  StatusError status statusText body ->
    "HTTP " <> show status <> " " <> statusText <> if Str.null body then "" else "\n" <> body
  DecodeError message -> "Could not decode response: " <> message
