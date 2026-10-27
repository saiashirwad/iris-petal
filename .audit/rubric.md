# Arena rubric (score each criterion 1-5 per candidate, cite file:line evidence)

1. **Call-site ergonomics.** Read the rewritten Pokédex `update`/`view`/command definitions, a parent/child composition, and the tests. Fewest concepts, imports, and ceremony for an app author. No generation numbers or internal messages in tests; no hand-written lens/config records.
2. **Interface depth and one way per task.** Capability hidden behind a small public surface. Count exports. Penalise two ways to do the same thing, pass-through functions, importable internals.
3. **Type safety and soundness.** Illegal states unrepresentable (AsyncData, pagination phases, roles). Any `unsafeCoerce`: is it actually guarded? Can a plausible user mistake reach it?
4. **Test DSL strength.** Pending-command discipline preserved; readable failures; one coherent monad; how much a test must know; does it still catch a regression (look for evidence such as mutation checks).
5. **Runtime correctness and simplicity.** Html representation, diff algorithm (keyed? special cases removed?), onVisible handling, subscriptions; verified in a real browser?
6. **Completeness and honesty.** Builds, tests pass, no stubs, coverage not silently dropped (accessibility, command semantics, rendering), docs/README state, what was deleted vs lost.

Candidates (each is a git worktree; read design/DESIGN.md first, then the code; `git -C <path> diff add912e --stat` shows the change):
- A: /Users/texoport/code/iris-foldkit/.claude/worktrees/agent-ae492fcacd235afa3
- B: /Users/texoport/code/iris-foldkit/.claude/worktrees/agent-a1944dda175ae9676
- C: /Users/texoport/code/iris-foldkit/.claude/worktrees/agent-a8c818d0a279e0d69
Baseline (pre-change) code: commit add912e in /Users/texoport/code/iris-foldkit.
