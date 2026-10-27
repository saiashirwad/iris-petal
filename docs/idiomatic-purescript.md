# Idiomatic PureScript in this repository

This is the style standard for `petal`. It exists because a PureScript
port of a TypeScript library can compile while still reading like TypeScript
with different syntax. The goal is code that a PureScript or Haskell programmer
would have designed from the start.

## 1. Data first

Model the domain with ADTs and records before writing logic.

- Sum types for alternatives: `data AsyncData error value = NotAsked | Loading (Maybe value) | ...`
- Records for product data: `type Model = { count :: Int }`
- Newtypes for opaque state: `newtype Model key error value = Model (Map key ...)`
- Separate a definition from an instance when they have different lifetimes:
  `Definition args result` and `Command message`. The message function belongs
  to the instance, so it can close over what the caller knew.
- A definition's identity is the top-level value itself, not its name.

Do not encode a variant as a string plus optional fields. Do not use a record
with `Maybe` fields where a sum type says which combinations are legal.

## 2. Constructors and smart constructors

Export the type and the values users need. Keep helper constructors total and
small.

```purescript
data Attribute message
  = OnClick message
  | OnInput (String -> message)

onClick :: forall message. message -> Attribute message
onClick = OnClick
```

An event is not a string key. `OnClick` cannot be looked up as `"input"` by
mistake, and the handler payload type is part of the constructor.

## 3. Exhaustive matching

Every `case` on an ADT is exhaustive. Do not add a wildcard to silence a new
constructor. The compiler is the checklist.

```purescript
update = case _ of
  UpdatedSearch search -> modify_ _ { search = search }
  SelectedPokemon name -> ...
  ReachedListEnd -> ...
  GotPage message -> ...
  GotDetail message -> ...
```

If a wildcard is genuinely total, it should be a named function with a clear
reason, not a habit.

## 4. One monad per interpreter

`Update model message a` is State over the model plus an append-only list of
commands. `Petal.Test.Scenario` is the standard stack:

```purescript
ReaderT program (ExceptT String (State world))
```

- `ReaderT` carries the program's `update` and `view`.
- `State` carries the model and the pending commands.
- `ExceptT String` carries the first failure.

Scoping is a locator (`within parent child`), not reader state. Do not introduce
a capability typeclass unless a real helper needs it.

## 5. Standard classes, not bespoke ones

Use `Functor`, `Eq`, `Ord`, `Show`, `MonadState`, `MonadError`, `MonadReader`.
Do not invent a `MonadTest` class to avoid passing `update`; pass `update` or use
`ask`.

`Show` belongs at assertion boundaries where the failure prints a value.
`expectModel` compares a projection of the model, so only the projected value
needs `Eq` and `Show`.

## 6. Pure core, effects at the edge

- `Html`, `Locator`, `Command`, and `Update` are pure data or pure functions.
- `Aff` appears only in command runners and the browser runtime.
- The test interpreter resolves commands with values; it never runs them.
- The renderer is a pure fold from `Html` to `String`.
- HTTP is plain `Aff` functions. Its status and decoding rules are the pure
  `readJson`, which is what tests call.
- Caches are opaque child models driven with `zoom`. Their bookkeeping
  (generations, cursors) is captured by command instances and never reaches a
  test.
- FFI is a thin edge: DOM operations in `Petal.Browser.js`.
- `unsafeCoerce` appears only in `Petal.Command.Internal`, behind a
  reference identity check.

No hidden mutation, no module-global dispatch stack, no `unsafePerformEffect`.

## 7. No TypeScript workarounds

Do not port these patterns:

- Runtime `Schema` for models and messages; use ADTs.
- Callable union constructors and `Match.tagsExhaustive`; use `data` and `case`.
- Phantom fields (`_phantomModel`, `_phantomMessage`); the monad parameters carry
  the types.
- `NoInfer` and overload stacks; write one polymorphic signature.
- Unique-symbol brands for definitions; use a data type or a name.
- `Object.assign` callable locators with a `.description` property; use a
  `Locator` ADT.
- String-keyed event handler tables; use typed `Attribute` constructors.
- `expectIgnored`/`expectHandled`; a total typed handler makes the state
  unrepresentable.
- `deepFreeze`; PureScript data is immutable.
- A free monad or indexed monad for impossible command orderings; `update` emits
  a data-dependent number of commands, so the type-level version is not honest.

## 8. Names and signatures

- Full words: `model`, `message`, `commandValue`, `resultMessage`, not `m`, `msg`,
  `cmd`.
- The type of a function is its documentation. Avoid a comment that repeats it.
- One argument order. Do not provide data-first and data-last overloads through
  `Function.dual`.
- Prefer a named helper over a clever inline expression, but do not introduce an
  intermediate binding for every subexpression.

## 9. Formatting and layout

`iris format` is the formatter. Run it before every commit. Keep the build green
before committing.

The project uses the formatter's output even when it differs from a hand-written
layout. Do not fight it.

## 10. Verification

- `iris build` compiles both workspace packages.
- `iris test` runs the `petal` and `examples` specs and exits non-zero on failure.
- `iris run -p examples` prints the server-rendered Pokédex shell.
- New scenario guarantees get a test in `petal/test`: ambiguity, pending
  discipline, unresolved commands, and definition identity.
- New locators get a test with a real view, not a hand-built `Html` value.

A style refactor is not done until the tests still pass and the diff is smaller
or clearer than before.
