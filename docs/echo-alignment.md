<!--
SPDX-License-Identifier: MPL-2.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
-->

# Echo alignment — Oblíbený ↔ echo-types ↔ EchoTypes.jl

This file records the design decisions that align Oblíbený's local `echo` type
with the wider Echo ecosystem. It is a **decisions ledger**, not an
implementation: the Oblíbený realisation lands via the type-checker work in this
repo; the Agda and Julia work belongs in the other two repos and is tracked here
only so the intent is not lost.

## Global principle

Echo is about **structured, proof-relevant loss**: information may be collapsed,
weakened, sampled, projected, or degraded, but the *residue / provenance /
lineage* of that loss can still be represented.

Echo is **not** a generic wrapper, a generic Σ-type benchmark, or a decorative
effect. The load-bearing semantic object is **retained-loss lineage**.

## Oblíbený realisation (implemented in this repo)

Oblíbený is a *factory language*: it produces deliberately restricted
mini-languages for IoT/IIoT and similar settings. Its constrained form must stay
intentionally small, analysable, and hostile to unwanted expressivity — the
"small entrance" security model (keep the entrance too small for the rhino).
Echo must respect that boundary.

- **Shape:** `echo[A, B]`, indexed by source/domain `A` and visible/codomain `B`.
  Not indexed by a specific map `f`; the dependent Agda definition
  `Echo f y := Σ (x : A), f x ≡ y` is **not** reproduced literally.
- **Runtime value:** a runnable pair `(witness, visible)` (`VEcho`). There is **no**
  statically enforced proof that `f witness = visible`. An optional dynamic
  checked introduction may come later; the core type does not depend on it.
- **Projections are affine-consuming for non-copyable echoes:**
  `echo_visible(e)` and `echo_witness(e)` each consume `e` when `e` is
  non-copyable. Without this, `echo[A, B]` degenerates into an ordinary
  duplicable product `A * B` that can be freely destructured from both sides —
  the wrong semantic smell, and a new unrestricted pairing/decomposition
  mechanism the constrained form must not admit.
- **Copyable echoes may remain unrestricted** (e.g. `echo[i64, i64]`), mirroring
  how copyable values may be duplicated freely.
- **"residue"** names the whole retained-loss object in prose; `echo_witness` /
  `echo_visible` stay as the projection names (the witness projection returns the
  source value, not the residue object).

### Design boundary ("no rhino")

`echo[A, B]` in Oblíbený:

- is allowed as a constrained-form residue value (structured, non-total loss);
- stores a visible observation plus a retained witness;
- is **not** reversible, and does **not** participate in `incr`/`decr`/`swap`
  balancing;
- does **not** add recursion, first-class functions, or tuples;
- does **not** add map-over / degrade / degrade-compose machinery;
- does **not** import the echo-types proof theory.

These boundaries are pinned by conformance tests (echo traversed by
`constrained_check`; recursion hidden in an echo is still rejected; echo memory
bounded as witness + visible; non-copyable echo cannot be projected twice;
copyable echo permitted). Grading is intentionally out of scope for the first
iteration; a graded `echo` may arrive later as a separate change.

## echo-types (Agda) — tracked here, implemented there

Source of truth is the Agda development
([`hyperpolymath/echo-types`](https://github.com/hyperpolymath/echo-types)).

- Agda is authoritative; the Idris2 comparative port is dropped for that repo.
- Tests and benchmarks are mandatory, not optional, and must benchmark the
  **actual Agda artefacts** — not a second-language reimplementation.
- Keep all three benchmark metrics:
  1. type-check / elaboration time;
  2. normalisation / reduction cost;
  3. compiled runtime via Agda's GHC / MAlonzo backend.
- Benchmarks scale along Echo lineage chains: single Echo → map-over chain →
  degrade chain → degrade-compose chain → finite-domain runtime chain.
- The key signal is **retained-loss lineage cost**, not generic Σ-type overhead.
- CI tiers: fast tests on every PR; standard benches manual/scheduled; extended
  benches before releases or major semantic changes.

## EchoTypes.jl (Julia) — tracked here, implemented there

[`hyperpolymath/EchoTypes.jl`](https://github.com/hyperpolymath/EchoTypes.jl) is
the finite-domain executable companion (a runnable model, not a proof). The
lineage-chain benchmarks should be wired into its **existing**
list-of-types / finite-domain harness, after inspecting the repo, rather than a
new bespoke harness.

## Scope note

The Agda benchmark infrastructure and the EchoTypes.jl harness wiring are **not**
Oblíbený work and must not be written from this repo. They require
`hyperpolymath/echo-types` and `hyperpolymath/EchoTypes.jl` to be in session
scope; this ledger exists so the plan is ready to execute there.
