# Idiomatic PureScript in this repository

This is the style standard for `iris-foldkit`. It exists because a PureScript
port of a TypeScript library can compile while still reading like TypeScript
with different syntax. The goal is code that a PureScript or Haskell programmer
would have designed from the start.

## 1. Data first

Model the domain with ADTs and records before writing logic.

- Sum types for alternatives: `data AsyncData value error = Idle | Loading | ...`
- Records for product data: `type Model = { count :: Int }`
- Newtypes for identity and invariants: `newtype CommandName = CommandName String`
- Separate a definition from an instance when they have different lifetimes:
  `Definition args result message` and `Command message`, not one callable
  object that is both.

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
update model = case _ of
  UpdatedZipCode value -> continue model { zipCode = value }
  SubmittedWeatherForm -> ...
  SucceededFetchWeather weather -> ...
  FailedFetchWeather error -> ...
```

If a wildcard is genuinely total, it should be a named function with a clear
reason, not a habit.

## 4. Reader for context, State for the simulation, Except for failure

The test interpreters use the standard transformers directly:

```purescript
ReaderT environment (ExceptT String (State state))
```

- `ReaderT` carries immutable context: `update`, `view`, locator scope.
- `State` carries the model, pending commands, and OutMessage.
- `ExceptT String` carries the first failure.

Use `local` for scope changes instead of storing a scope in the state.

```purescript
within locator action = do
  env <- ask
  let scoped = ...
  local (\current -> current { scope = Just scoped }) action
```

Do not introduce a capability typeclass unless a real helper needs to be
polymorphic over both interpreters and the constraint pays for itself. The two
monads already make "no clicking in a Story" a compile error.

## 5. Standard classes, not bespoke ones

Use `Functor`, `Eq`, `Ord`, `Show`, `MonadState`, `MonadError`, `MonadReader`.
Do not invent a `MonadTest` class to avoid passing `update`; pass `update` or use
`ask`.

`Show` belongs at assertion boundaries where the failure prints a value. If a
model contains a function, `expectModelWith` takes a renderer instead of forcing
a `Show` instance that cannot exist.

## 6. Pure core, effects at the edge

- `Html`, `Locator`, `Command`, `Update`, `Simulation.State` are pure data.
- `Aff` appears only in `Command.run` and the browser runtime.
- The test interpreter resolves commands with values; it never runs them.
- The renderer is a pure fold from `Html` to `String`.
- FFI is a thin edge: DOM operations in `Foldkit.Runtime.js`, exit code in
  `Foldkit.Test.js`.

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

- `iris build` compiles the application graph.
- `iris test` runs `Test.Main` and exits non-zero on failure.
- `iris run` prints the server-rendered counter and weather views.
- New command guarantees get a test: definition vs instance matching, exact
  multiset, ambiguity, `resolveExact`, pending discipline, and `Scene.send`.
- New locators get a test with a real view, not a hand-built `Html` value.

A style refactor is not done until the tests still pass and the diff is smaller
or clearer than before.
