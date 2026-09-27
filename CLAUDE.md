// SPDX-License-Identifier: CC-BY-SA-4.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
# CLAUDE.md — AI Assistant Instructions for Oblíbený

Oblíbený (Czech for "favourite/beloved") is a **dual-form programming language**
for secure edge computing: a Turing-complete *factory form* that generates a
Turing-**incomplete**, reversible, fully-accountable *constrained form* for
hostile environments (HSMs, smart cards, enclaves, IoT edge). The constrained
form statically guarantees **termination**, **static resource bounds**,
**reversibility**, and an immutable **accountability trace**; the `echo[A,B]`
type carries the proof-relevant residue of irreversible collapse.

Stack: **OCaml** (compiler/runtime), **Idris2** (machine-checked ABI + metatheory
proofs), **Zig** (post-quantum crypto FFI over liboqs/libsodium).

## Orientation order (read before touching anything)

1. `0-AI-MANIFEST.a2ml` — the machine-readable front door for agents.
2. `README.adoc` — project overview and honest status table.
3. `.machine_readable/descriptiles/STATE.a2ml` — **live** state: completion,
   blockers, next actions. Trust this over your assumptions.
4. `AFFIRMATION.adoc` — the dated honesty snapshot: what was verified, by
   running, at a pinned SHA.
5. `EXPLAINME.adoc` — why the language is shaped the way it is.
6. `docs/architecture/REPOSITORY-MAP.adoc` — generated map of the tree
   (regenerate with `just repo-map`; CI fails if stale).

## Machine-readable artefacts

Structured project metadata lives in `.machine_readable/descriptiles/` as
**A2ML** (`.a2ml`) files — this replaced the earlier `.machine_readable/*.scm`
scheme (migrated in PR #56):

- `STATE.a2ml` — current state, blockers, next actions
- `META.a2ml` — architecture decisions and development practices
- `ECOSYSTEM.a2ml` — position in the estate and related repositories
- `AGENTIC.a2ml` — AI agent interaction patterns
- `NEUROSYM.a2ml` — neurosymbolic integration config
- `PLAYBOOK.a2ml` — operational runbook
- `anchor/` — the estate anchor record

The root `SPEC.core.scm`, `ANCHOR.scope-arrest.2026-01-01.Jewell.scm` and
`AUTHORITY_STACK.mustfile-nickel.scm` are **not** legacy metadata: they are the
live formal specification and identity anchor (see `.machine_readable/contractiles/Dustfile.a2ml`).

## Language policy

### This repository

| Language | Use for | Notes |
|----------|---------|-------|
| **OCaml** (≥ 4.14) | Compiler, runtime, LSP, tools (`lib/`, `bin/`, `test/`) | Built with dune + menhir/sedlex/yojson/ppx_deriving; tests via alcotest |
| **Idris2** (0.7.0) | The ABI proof layer and `Lang.*` metatheory (`src/abi/`) | `%default total`; the escape-hatch guard below is CI-enforced |
| **Zig** (0.13.x) | Crypto/package FFI (`ffi/zig/`) | 0.14+ renamed `callconv(.C)` and will not compile these sources |
| **Bash** | Glue, hooks, CI steps | Keep minimal |
| **jq** | JSON processing in workflows | jq, not Python (see ban below) |

Do **not** rewrite components in other languages "to help". The OCaml/Idris2/Zig
split is deliberate: each part lives where its guarantees are strongest.

### Estate-wide bans (enforced by the governance gate)

TypeScript, ReScript, Deno, Node.js, Go, **Python** (fully banned — the
governance gate runs `git ls-files '*.py'` and fails), Java/Kotlin, Swift,
React Native, Flutter/Dart, V-lang, and Makefiles. JS runtime deps (VSCode
extension) use Bun conventions; there is no npm lockfile in this tree.

### Security requirements

- No MD5/SHA1 for security purposes (SHA256+ only)
- HTTPS only; no hardcoded secrets; SHA-pinned CI dependencies
  (`.github/workflows/actions.lock` is authoritative — regenerate in the same
  PR as any `uses:` change)
- `github/codeql-action` is held at **v4.38.0** estate-wide (4.38.1 fails
  workflow startup validation; see `.github/dependabot.yml` before bumping)

## Soundness guardrails (CI-enforced, non-negotiable)

1. **No escape hatches in the proof layer.** Never introduce `believe_me`,
   `postulate`, `assert_total`, `partial`, `idris_crash`, or holes (`?x`) in
   `src/abi/`. CI greps for these and fails. A proof gap is recorded as debt
   (`docs/proof-debt.adoc`), never papered over.
2. **A claim of green must be a run.** Every "it builds / tests pass /
   proofs check" statement must come from actually running the tool in your
   session, at a named SHA. Model ≠ implementation: the `Lang.*` Idris2
   metatheory proves properties *of a faithful model* of `lib/` — say so; do
   not claim the OCaml itself is verified.
3. **The conformance suite (`dune runtest`, 27 tests) must stay green**, and
   behaviour changes need a new conformance test.
4. **Honesty artefacts stay honest.** `AFFIRMATION.adoc` and
   `descriptiles/STATE.a2ml` describe reality; when reality changes, update
   them in the same change — including *downgrading* claims.

## Guardrails — what an agent must NOT do

- **Never edit secrets or request them.** `FARM_DISPATCH_TOKEN` rotation is
  maintainer-only (`gh secret set FARM_DISPATCH_TOKEN --repo hyperpolymath/oblibeny`).
- **Do not delete git branches** (estate rule GS007), rewrite published
  history, or bypass required checks — reach green by satisfying the gate.
- **Do not edit `CLAUDE.md` (this file) unilaterally**; propose changes in a
  PR for maintainer review. Same for `.machine_readable/bot_directives/`.
- **Do not remove or "clean up"** `SPEC.core.scm`, `ANCHOR.*.scm`,
  `AUTHORITY_STACK.*.scm`, or `Mustfile` — they are load-bearing.
- **Do not re-point the standards reusable-workflow pins** to anything but a
  commit reachable from `hyperpolymath/standards` main (the staleness gate
  verifies reachability server-side; unreachable pins kill the workflows at
  startup with no logs).

## Tasks route through the Justfile

```bash
just              # list recipes
just build        # dune build
just test         # dune runtest (conformance suite)
just ci           # lint + test + zig-ffi-check + escape-hatch guard
just proofs       # idris2 --build src/abi/oblibeny-abi.ipkg
just demo         # golden path: examples/hello.obl with --dump-trace
```

`Mustfile` is the deployment contract layer above the Justfile (`just build`,
`just test`, `just lint`, `just golden-path`, and the `release` transition).

## Where things live

The authoritative map is `docs/architecture/REPOSITORY-MAP.adoc` (generated).
Short version: `lib/` + `bin/` (OCaml), `src/abi/` (Idris2 proofs),
`ffi/zig/` (Zig FFI), `test/` (conformance), `examples/` (`.obl` programs +
`alib/` standard library samples), `docs/` (specifications, academic notes,
history), `editors/vscode/` (syntax highlighting), `.machine_readable/`
(manifests, contractiles, policies).
