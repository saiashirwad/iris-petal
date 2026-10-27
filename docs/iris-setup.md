# Iris setup and workflow

This project is built with the Iris implementation of PureScript, not `purs`
directly. Iris provides the compiler, build runner, formatter, language server,
and watch query interface.

## Versions

- Iris: 0.1.13 (`iris --version`)
- Spago: 1.0.4
- Node: 22 or newer
- Package set: 81.6.0

The `iris` binary used here is installed at `~/.local/bin/iris`.

## Install Iris

The official installer is the supported path:

```sh
curl -fsSL https://iris-lang.com/install.sh | sh
```

It installs to `~/.local/bin` on macOS and Linux. If `gh` is available, the
installer verifies the release attestation. `iris self update` upgrades in place.

## Create or inspect the workspace

This workspace was created with:

```sh
mkdir iris-foldkit
cd iris-foldkit
iris new --name iris-foldkit
```

`iris new` writes `spago.yaml`, `src/Main.purs`, `test/Test/Main.purs`, and a
`.gitignore`. `spago.yaml` pins the package set and the package's dependencies.

The project adds the runtime and test dependencies through Spago:

```sh
iris add aff arrays console effect either exceptions foldable-traversable \
  maybe prelude refs strings transformers tuples
iris add --test assert
```

`mtl` is not in package set 81.6.0. The project uses the standard transformer
modules directly (`Control.Monad.Reader`, `Control.Monad.State`,
`Control.Monad.Except`, `Control.Monad.Error.Class`), which live in
`transformers`.

`ezfetch` supplies the typed `Aff` HTTP transport used by `Foldkit.Http`.
`argonaut-core`/`argonaut-codecs` decode the PokéAPI payloads.
`ordered-collections` supplies response headers and `url-immutable` parses
request URLs.

## Build, test, run

```sh
iris build              # compile the foldkit and examples packages
iris test               # run both packages' specs with Node
iris run -p examples    # print the server-rendered Pokédex shell
iris format         # format every package's src and test sources in place
iris format --check # verify formatting
```

The workspace has two packages. `foldkit/spago.yaml` is the library and runs
`Test.Foldkit.Main`. `examples/spago.yaml` depends on it and runs
`Test.Examples.Main`. Both use `spec` and `spec-node`; the library ships no test
runner of its own.

`scripts/check.sh` runs the whole gate:

```sh
./scripts/check.sh
```

It runs `iris format --check`, `iris build`, `iris test`, and `iris run`.

`iris run -p examples` runs `Example.Pokedex.Server`, which prints the
server-rendered shell. The browser entry is `Example.Pokedex.Browser`, which
`web/main.js` imports. Neither entry checks which platform it is on.

## Dev server and hot reload

Install the JavaScript tooling once:

```sh
npm install
npx playwright install chromium   # once, for the smoke test
```

Start the live development loop:

```sh
npm run dev
```

`scripts/dev.sh` starts `iris watch` in the background and Vite in the
foreground. Iris rebuilds `output/` on a PureScript edit; Vite watches the
compiled modules and updates the page. The Pokédex is at
<http://localhost:5173/>.

Production build:

```sh
npm run build
```

The browser smoke test needs the dev server to be running:

```sh
npm run smoke
```

It opens Chromium, waits for the real PokéAPI list, types in the search box,
checks that focus survives the re-render, checks the filter, clicks a card, and
waits for the detail panel.

## Build graph

`iris build` compiles every module in both packages' `src`. `iris test` also
compiles each package's `test` directory.

PureScript modules that need JavaScript use an FFI file beside the source:

```text
foldkit/src/Foldkit/Browser.purs
foldkit/src/Foldkit/Browser.js
```

Iris compiles the `.purs` module and links the matching `.js` exports.

## Watch and query

For a long editing session:

```sh
iris watch
```

In another terminal, `iris watch query` asks the running watcher about the
project:

```sh
iris watch query signatures Foldkit.Test
iris watch query definitions Foldkit.Command
iris watch query references Foldkit.Test.resolve
iris watch query diagnostics
```

The watcher is the fastest way to inspect inferred signatures without reading
dependency sources. `iris skills` prints the agent skills bundled with the
compiler version; `iris skills watch` describes the query surface.

## Formatting

The formatter is part of the compiler. Run `iris format` before every commit.
The project does not carry a separate formatter configuration.

## Compiler notes

Iris 0.1.13 once rejected a nested `case` expression in a test runner with
`UnreportedUnknownType`. Rewriting the inner `case` as `maybe` or `either`
compiled correctly. Prefer that form when the error appears.
