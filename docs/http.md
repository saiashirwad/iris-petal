# Typed HTTP

The Pokédex needs network effects. Application code should not open a raw JS
`fetch` foreign import, and it should not decode JSON through an untyped
callback. `Foldkit.Http` is the boundary.

## Why this shape

The PureScript package set already has several HTTP clients:

- `affjax` is mature but XMLHttpRequest-based and its `Request`/`Response` types
  are larger than this slice needs.
- `fetch` is a high-level binding over Fetch, but its request headers are a
  homogeneous record, which fits a fixed set of headers better than an
  application-level header list.
- `ezfetch` exposes `Method`, `URL`, `Headers`, `Body`, and `Response` as
  PureScript data and runs in `Aff`. It supports dynamic headers, typed bodies,
  status, text, JSON, blobs, and forms. It is the closest thing to the API this
  project wants.

`Foldkit.Http` wraps `ezfetch` in a smaller application-facing type. The wrap is
not ceremony: it gives a single error ADT, a canonical `Client` capability, and
a `pureClient` that tests can inject.

No application module imports `Effect.Aff.HTTP` directly. Only
`Foldkit.Http.browserClient` does.

## The API

```purescript
type Request =
  { method :: Method
  , url :: String
  , headers :: Array (Tuple String String)
  , body :: Maybe String
  }

type Response =
  { status :: Int
  , statusText :: String
  , headers :: Map String String
  , body :: String
  }

data HttpError
  = InvalidUrl String
  | NetworkError String
  | StatusError Int String String
  | DecodeError String

type Client =
  { send :: Request -> Aff (Either HttpError Response)
  }
```

`Method` is a closed ADT: `GET`, `POST`, `PUT`, `PATCH`, `DELETE`, `HEAD`.
`Request` is data; it has no hidden effects. `Response` is already read to text
because this client is for JSON and text APIs. A streaming response is a
different capability and is not part of this slice.

`Client` is a record with one function. That is the test seam:

```purescript
pureClient :: (Request -> Either HttpError Response) -> Client
```

`pureClient` applies `guardStatus`, so a fake 404 is a `StatusError` exactly like
a real 404. The browser client uses the same guard.

```purescript
browserClient :: Client
```

`browserClient` parses the URL with `Data.URL`, calls `ezfetch`, catches its
rejection as `NetworkError`, reads status/statusText/headers/body, and then
applies `guardStatus`.

## JSON

`sendJson` keeps Argonaut in charge of decoding:

```purescript
sendJson
  :: DecodeJson value
  => Client
  -> Request
  -> Aff (Either HttpError value)
```

A JSON parse failure or a decode failure becomes `DecodeError String`. The
Pokedex uses `send` plus its own small decoders because its payloads are large
and it wants domain-specific messages. Both paths use the same error ADT.

## Pokedex usage

```purescript
fetchJson :: String -> Aff (Either Http.HttpError Json)
fetchJson url = do
  result <- Http.send Http.browserClient (Http.get url)
  pure do
    response <- result
    lmap Http.DecodeError (jsonParser response.body)

fetchList :: Aff (Either Http.HttpError (Array PokemonSummary))
fetchList = do
  result <- fetchJson listUrl
  pure do
    json <- result
    lmap Http.DecodeError (decodeList json)
```

The command definition lifts the result into a message:

```purescript
fetchPokemonList :: Definition Unit (Either Http.HttpError (Array PokemonSummary)) Message
fetchPokemonList =
  define0 (CommandName "FetchPokemonList") fetchList case _ of
    Left error -> FailedList (Http.describeError error)
    Right summaries -> LoadedList summaries
```

The update function never sees an `Aff`. It sees `LoadedList` or `FailedList`.

## Testing

`Test.Http` covers the transport contract without a network:

```purescript
okClient :: Http.Client
okClient =
  Http.pureClient \_ ->
    Right
      { status: 200
      , statusText: "OK"
      , headers: Map.empty
      , body: "{\"hello\":\"world\"}"
      }
```

`Test.Pokedex` resolves the detail command with a `Right samplePokemon` or a
`Left (Http.StatusError 500 "Server Error" "")`. The update and view are tested
without a browser. The browser smoke test is separate: `npm run smoke` starts
Chromium against the Vite page and exercises the real network path.

## Design notes

- `Request` stores headers as an array, not a row-record. Header names are data,
  and a client can add or filter them at runtime.
- `Body` is `Maybe String`. JSON is the common case; `postJson` stringifies a
  `Json` value and sets `Content-Type: application/json`.
- `guardStatus` is a pure function, so the success/failure rule is testable and
  shared.
- The client is a record, not a typeclass or a service layer. Passing one
  function is simpler than installing an effect environment.
- `HttpError` is closed. A new transport case is a new constructor, and every
  `describeError` case is checked by the compiler.

## What comes next

Abort/cancellation, timeouts, retries, and middleware are the natural next layer.
They should stay above `Client` or inside a new `Policy` record, not leak into
`Request`. Streaming and multipart are separate capabilities; they should not be
bolted onto the text response type.
