# Arena task: candidate design for the redesigned library

Produce ONE candidate design for the whole public API of the library in this repo, as a design package that compiles.

## Read first

1. `/Users/texoport/.claude/plugins/cache/pstack-claude/pstack/0.9.88/skills/architect/references/runner-prompt.md` (your discipline; the rationale template and red flags it links are beside it).
2. `/private/tmp/claude-501/-Users-texoport-code-iris-foldkit/12d4f8cf-ec23-463d-947f-d92b7b16f505/scratchpad/arena/grounding.md` (mandate, toolchain facts, critique).
3. Every file in your worktree's `src/`, `test/`, `web/`, and `README.md`.

## Deliverable (in your git worktree, committed on your worktree branch)

1. `design/DESIGN.md` shaped per the rationale template. Usage section first: show the README quickstart for a counter, then the real Pokédex `Model`/`Message`/`update`/command definitions, a parent/child composition, and two tests (one model-level, one view-level) exactly as an app author would write them under your design.
2. The new library modules under `src/` replacing the current ones (rename/merge/delete modules freely; module namespace may change if you argue for it). Every public type and signature real. Bodies may be `unsafeCrashWith "not implemented"` (from `Partial.Unsafe`, add the `partial` dependency to spago.yaml if needed) only where the logic is non-trivial; trivial bodies should be real.
3. Rewrite `src/Example/Pokedex.purs` and `test/Test/Pokedex.purs` (and Counter/Parent if your composition story changes them) against your API so the call sites are real code, not prose. Delete examples/tests you would delete.
4. `iris build` AND `iris test` must at least COMPILE (runtime crashes from stubs are acceptable; say which tests would pass). Run `iris format` before committing. Paste the tail of both commands' output into DESIGN.md under "Implementation reconciliation".

## You must take a position on each of these (in DESIGN.md)

- The update/transition type and how commands are emitted (record, tuple, Writer-like monad, ...). Where OutMessage lives, if anywhere.
- Command definition vs instance; how a test resolves a library-owned command (Query/Pagination) with a plain result and no generation numbers; whether any `unsafeCoerce` is needed and why it is sound.
- Composition: how a parent embeds a child, a keyed cache, and a paginated list without hand-written `{read, write, toParentMessage}` records (VTA record labels, lenses, or a better idea).
- The test DSL: one monad or two, how a test fails (Either vs Aff throw), locator role typing, removing redundant assertions.
- The Html/Attribute surface and what the runtime diff must handle (Fragment/Empty normalization, keyed children, viewport sentinel vs subscriptions).
- Package layout: library vs examples vs test runner.
- What you delete. Subtraction counts as much as addition.

You are one of several parallel runners on different models. Commit to a strong, coherent direction; do not hedge toward a safe middle. Report back: worktree path, branch name, commit SHA, a 10-line summary of your direction, and the build/test compile result.
