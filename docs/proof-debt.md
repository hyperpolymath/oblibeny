<!--
SPDX-License-Identifier: MPL-2.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
-->

# Proof debt

Soundness-relevant escape-hatch ledger for oblibeny, per the
[Trusted-Base Reduction Policy](https://github.com/hyperpolymath/standards/blob/main/docs/TRUSTED-BASE-REDUCTION-POLICY.adoc).

**Status (2026-06-03): cleared.** The automated scan
([`docs/tech-debt-2026-05-26.md`](./tech-debt-2026-05-26.md)) previously counted
**2** soundness-relevant markers (`Idr-blv = 2`) — two `believe_me` coercions in
the hello-package ABI. Both are now **discharged by genuine machine-checked
proofs**; the ABI proof layer contains zero `believe_me`, zero `assert_total`,
no Idris2 `partial`, and no axiom stand-ins.

> Note on history: the two `believe_me` placeholders were masking type
> signatures that did not even type-check (`Not (elem …)` used a `Bool` where a
> `Type` was required), and one of the two laws (`doubleInstallIdempotent`) was
> *false* as originally modelled (naive `::` insertion duplicates the package
> name). Closing the debt therefore required both a corrected, proof-enabling
> model and real proofs — see (a) below.

## Verifying

```
# Idris2 proof layer (toolchain: Idris2 >= 0.7.0)
idris2 --build src/abi/oblibeny-abi.ipkg     # builds = type-checks all proofs

# OCaml language + conformance suite (toolchain: OCaml >= 4.14, dune, menhir)
dune build && dune runtest
```

A green `idris2 --build` *is* the proof check: in Idris2, type-checking a total
definition with no escape hatch is the proof.

## (a) Discharged in this repo

### `src/abi/Packages/Hello/Interface.idr` — `installReversible`

- **Was:** `believe_me ()` standing in for the proof term (formerly at
  `src/abi/packages/hello/Interface.idr:69`).
- **Now:** a genuine, total equational proof that
  `uninstall pkg (install pkg state) = state`, given the package is not already
  installed and its binary path does not already exist. Discharged via the
  set-algebra lemma `removeAddUnique` on each component, reassembled with
  `stateEq`. No functional-extensionality axiom is used (the file-existence
  field is modelled as a path *set*, `List String`, rather than an opaque
  `String -> Bool` predicate — a deliberate, proof-enabling and more faithful
  model; see the module header).

### `src/abi/Packages/Hello/Interface.idr` — `doubleInstallIdempotent`

- **Was:** `believe_me ()` standing in for the proof term (formerly at
  `src/abi/packages/hello/Interface.idr:78`).
- **Now:** a genuine, total proof that
  `install pkg (install pkg state) = install pkg state`, unconditionally.
  `install` uses idempotent set-insert semantics (`addUnique`), so re-inserting
  a present element is a no-op; discharged via `addUniqueIdem` on each
  component. (Under the original naive `::` semantics this law is false, so the
  set-insert model is both correct and necessary.)

## (b) Budgeted — tested with refutation budget

_None._

## (c) Necessary axiom / external boundary

The post-quantum signature properties in `src/abi/Crypto.idr` concern the
external C crypto libraries (liboqs / libsodium) reached over FFI; their
completeness and soundness cannot be proved in Idris and are genuine boundary
assumptions about linked code.

They are **no longer expressed as fake `postulate`s** (Idris2 has no such
keyword — the previous stubs did not compile and were ill-typed). They are now:

- the explicit, well-typed `interface VerifierTrust` (a pure per-scheme
  verifier plus `completeness` and `soundness`), which the trusted C binding
  discharges by providing an implementation; and
- a **proved** intrinsic property, `tripleSignatureConjunction`: a valid triple
  signature forces each of the three independent schemes to verify (defence in
  depth). This is a real, total theorem about the verification *logic*, not an
  assumption.

These carry no `believe_me` and are not counted by the soundness-escape-hatch
scan; they are tracked here as the documented trust boundary.

## (d) DEBT — actively to be closed

_None._

## Related: echo-types proof bridge

`src/abi/Packages/Hello/Echo.idr` formally connects the install/uninstall laws
to the echo-types residue semantics
([`hyperpolymath/echo-types`](https://github.com/hyperpolymath/echo-types)):
`install` is non-injective (irreversible), and its echo/fibre is the residue of
that loss. The module proves that under the freshness preconditions the fibre is
a singleton (`installInjectiveFresh` — reversibility *is* injectivity, no
residue), while without them a concrete two-element fibre exists
(`echoResidueNonTrivial` — a genuine retained echo). This is the proof-layer
counterpart of Oblíbený's in-language `echo[A,B]` type.
