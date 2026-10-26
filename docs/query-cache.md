# Query caches and pagination

The Pokédex caches two different things:

- The **list** is a sequence of pages. It loads 24 Pokémon at a time, appends
  each page, and stops at 151. A sentinel at the bottom of the list asks for the
  next page when it enters the viewport.
- The **detail** is keyed by Pokémon name. Clicking a Pokémon loads it once;
  clicking it again reads the cached value and starts no command.

These are ports of FoldKit's experimental `Query` and keyed-query idea, adapted
to a plain PureScript model.

## Why a query cache at all

An `AsyncData` field is enough for one value. A cache needs more:

- a key, so each detail has its own state
- a pending status per entry, so one request does not block another
- a generation per entry, so a slow response cannot overwrite a newer one
- an operation that starts a fetch only when the value is missing

`Foldkit.Query` owns those four things. The app still owns the data and the
message that carries it.

## Keyed query

```purescript
type Model key value error =
  { entries :: Map key (Entry value error)
  }

type Entry value error =
  { data :: AsyncData value error
  , generation :: Int
  }

data Message key value error
  = CompletedFetch key Int (Either error value)
```

A definition captures the fetch and the command that runs it:

```purescript
type Definition key value error =
  { name :: CommandName
  , fetch :: key -> Aff (Either error value)
  , fetchCommand :: Command.Definition
      { key :: key, generation :: Int }
      (Message key value error)
      (Message key value error)
  }
```

`define` requires `EncodeJson key`; the command payload is typed at the
definition and structurally encoded for matching.

The parent wires the cache in with a config record:

```purescript
type Config parentModel parentMessage key value error =
  { read :: parentModel -> Model key value error
  , write :: parentModel -> Model key value error -> parentModel
  , toParentMessage :: Message key value error -> parentMessage
  }
```

Operations:

- `loadIfMissing config definition key parent` — starts a fetch only for
  `Idle` or `Failure`; loaded and in-flight entries are untouched.
- `revalidate config definition key parent` — refreshes only `Success`/`Stale`.
- `revalidateOrLoad config definition key parent` — refreshes loaded entries
  and loads cold entries.
- `receive config message parent` — folds a completion into the right entry.
- `read model key` — returns the entry's `AsyncData`, or `Idle`.
- `liftedCommand config definition` — the command definition a Story/Scene test
  resolves. It lifts `Message key value error` to the parent message.

The completion applies only when the entry is still pending and its generation
matches. A stale response is ignored, not written over newer data.

### Pokedex usage

```purescript
fetchPokemonDetail :: Query.Definition String Pokemon Http.HttpError
fetchPokemonDetail =
  Query.define (CommandName "FetchPokemonDetail") fetchPokemon

detailConfig :: Query.Config Model Message String Pokemon Http.HttpError
detailConfig =
  { read: _.details
  , write: \model details -> model { details = details }
  , toParentMessage: GotPokemonQuery
  }
```

```purescript
SelectedPokemon name ->
  Query.loadIfMissing detailConfig fetchPokemonDetail name
    model { selected = Just name }
```

The view reads the selected entry:

```purescript
Query.read model.details name
```

A cache hit is synchronous. There is no loading state and no command. A cache
miss becomes `Loading` and renders a fixed-size skeleton.

## Pagination

```purescript
type Page item cursor =
  { items :: Array item
  , next :: Maybe cursor
  }

type Model item cursor error =
  { items :: Array item
  , next :: Maybe cursor
  , hasMore :: Boolean
  , status :: AsyncData Unit error
  , generation :: Int
  }
```

`Pagination.define` takes `Maybe cursor -> Aff (Either error (Page item cursor))`.
`loadMore` starts a page only when `hasMore` is true and nothing is pending.
`receive` appends the page, stores `next`, and sets `hasMore`. A stale
generation is ignored. A failure keeps the items already loaded and sets
`status = Failure`, so retrying does not discard the cache.

`fromItems` builds a complete, non-paged model for tests or static data.

### Pokedex usage

```purescript
fetchPokemonPage :: Pagination.Definition Int PokemonSummary Http.HttpError
fetchPokemonPage =
  Pagination.define (CommandName "FetchPokemonPage") \maybeOffset -> do
    let offset = fromMaybe 0 maybeOffset
        remaining = maxPokemon - offset
        limit = min pageSize remaining
    ...
```

```purescript
LoadMorePokemon ->
  Pagination.loadMore listConfig fetchPokemonPage model
```

The view renders a sentinel after the grid:

```purescript
div [ class_ "load-more", onVisible LoadMorePokemon ]
  [ text "Scroll for more" ]
```

`onVisible` is a view attribute. The browser runtime attaches an
`IntersectionObserver` with a small root margin, dispatches the message once
when the sentinel enters view, and disconnects. The next render creates a new
sentinel, so the next page can load. No timer, no scroll listener, no JS in the
app.

## No layout shift

The detail panel has a stable minimum height and a skeleton with the same
structure as the real card. The list starts with 24 skeleton cards, matching the
first page. The browser smoke test checks the actual heights:

```text
before=600 loading=600 after=600
```

It also verifies that clicking a cached card starts no request and keeps the
card visible, and that scrolling reaches "All 151 Pokémon loaded."

## General-purpose pieces

- `Foldkit.Http.Client` — a typed request/response capability with a test client.
- `Foldkit.Query` — keyed async cache with generation guards.
- `Foldkit.Pagination` — cursor-based pages with append, retry, and generation guards.
- `Html.onVisible` — a general viewport sentinel, not a Pokédex special case.

The app-specific code is the fetch and the view. The state machines live in the
library and are tested without a browser.
