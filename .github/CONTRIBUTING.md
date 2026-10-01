<!--
SPDX-License-Identifier: CC-BY-SA-4.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
-->

# Contributing Guide

Thank you for considering a contribution to Oblíbený.

## Getting Started

``` bash
git clone https://github.com/hyperpolymath/oblibeny.git
cd oblibeny

just ci      # dune lint + conformance suite + Zig FFI compile-check + escape-hatch guard
just proofs  # type-check (= prove) the Idris2 ABI layer
```

Toolchain installation (OCaml 5.1.1 + dune, Idris2 0.7.0, Zig 0.13) is
documented in [docs/TOOLCHAIN.adoc](../docs/TOOLCHAIN.adoc); the version
pins live in `.tool-versions`.

## Repository Structure

    oblibeny/
    ├── bin/                 # OCaml executable entry point
    ├── lib/                 # OCaml compiler library (lexer, parser, typecheck,
    │                        #   eval, constrained_check, static_analyzer)
    ├── test/                # Conformance + unit suite (dune runtest)
    ├── src/abi/             # Idris2 ABI proof layer (oblibeny-abi.ipkg;
    │                        #   Packages/, Lang/ metatheory)
    ├── ffi/zig/             # Zig crypto FFI (liboqs/libsodium) + obli-pkg
    ├── examples/            # .obl programs and packages/hello.zpkg
    ├── docs/                # Documentation (TOOLCHAIN, DISTRIBUTION-ARCHITECTURE, …)
    ├── deploy/              # Deployment manifests (kubernetes/, svalinn-compose.yaml)
    ├── .machine_readable/   # A2ML project metadata (descriptiles/) + contractiles
    ├── .github/workflows/   # CI gates (see ci.yml root-cause note)
    ├── justfile             # Task runner — all operations go through this
    ├── README.adoc
    ├── docs/GOVERNANCE.adoc # Sole-maintainer governance model (docs/ since 2026-09-27)
    ├── MAINTAINERS.adoc
    ├── ROADMAP.adoc
    └── SECURITY.md

## How to Contribute

### Reporting Bugs

1.  Search existing issues first.

2.  Include environment details (OS, toolchain versions from
    `.tool-versions`), steps to reproduce, and expected vs actual
    behaviour.

### Suggesting Features

Check <a href="../ROADMAP.adoc" class="adoc">ROADMAP</a> and existing
issues first, then open an issue with a problem statement, proposed
solution, and alternatives considered.

## Development Workflow

### Branch Naming

    feat/short-description       # New features
    fix/issue-number-description # Bug fixes
    docs/short-description       # Documentation
    test/what-added              # Test additions
    refactor/what-changed        # Code improvements
    security/what-fixed          # Security fixes

### Commit Messages

We follow [Conventional Commits](https://www.conventionalcommits.org/):
`type(scope):` `description`. Sign off commits (`git` `commit` `-s`,
DCO). Keep commits atomic and focused.

### The Proof Gate

The Idris2 ABI layer is a **proof** layer: CI rejects any soundness
escape hatch (`believe_me`, `postulate`, `assert_total`, `partial`,
`idris_crash`, holes) in `src/abi/Crypto.idr` and `src/abi/Packages`.
Run the same gate locally with `just` `guard-escape-hatches`; `just`
`ci` includes it. A change that only passes by weakening a proof will
not merge.

## License

Contributions are licensed under the project licence (see `LICENSE` and
the SPDX headers each file carries).

## Signed commits

Every commit that reaches the default branch must be signed; a ruleset refuses
unsigned pushes. Estate policy:
[SIGNING-POLICY](https://github.com/hyperpolymath/standards/blob/main/docs/SIGNING-POLICY.adoc).

- **People and interactive agents** sign with an SSH key registered on GitHub
  as a *signing* key (`gpg.format=ssh`, `user.signingkey=<key>.pub`,
  `commit.gpgsign=true`). The committer email must be verified on that account.
- **Apps, bots and workflows** never `git push` local commits. They write
  through the API (`createCommitOnBranch` or the estate `signed-push` action)
  so that GitHub signs each commit.
- Merge PRs with **squash**. The ruleset checks every commit on the PR branch,
  not just the result, so one unsigned commit blocks the merge. Re-create such a
  branch with signed commits (`git cherry-pick -S`) and open a new PR.
  Rebase-merge replays commits unsigned and is disabled.
