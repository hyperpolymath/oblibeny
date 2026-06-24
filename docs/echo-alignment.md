<!--
SPDX-License-Identifier: CC-BY-SA-4.0
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
- **Affine discipline is content-sensitive:**

  ```
  echo[A, B] is affine  iff  A or B is non-copyable.
  ```

  So `echo[i64, i64]` can be projected freely, while `echo[Trace, i64]`,
  `echo[array[i64], i64]` and `echo[i64, Cargo]` (non-copyable on *either* side)
  are affine. Echo does **not** make copyable data non-copyable by magic; it
  **preserves the copyability discipline of its contents**. Oblíbený is a factory
  for constrained mini-languages, not a proof calculus making every echo linearly
  precious — the boundary should be small, analysable, and resource-aware, not
  gratuitously restrictive for primitive copyable data.
- **Projections are affine-consuming for non-copyable echoes:**
  `echo_visible(e)` and `echo_witness(e)` each consume `e` when `e` is
  non-copyable. Without this, `echo[A, B]` degenerates into an ordinary
  duplicable product `A * B` that can be freely destructured from both sides —
  an unrestricted way to duplicate or inspect non-copyable residue, and a new
  unrestricted pairing/decomposition mechanism the constrained form must not
  admit.
- **Copyable echoes remain unrestricted** (e.g. `echo[i64, i64]`), mirroring how
  copyable values may be duplicated freely. If both sides are copyable,
  duplicating the echo smuggles in no extra authority, heap state, trace state,
  or hidden capability — it is a bounded pair of copyable observations, so the
  "no rhino" boundary stays intact.
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

## Update 2026-06-03 — type-safety hardening + proof-layer bridge

Two changes landed while bringing the ABI proof layer to a true, machine-checked
state. Both reinforce the boundaries above rather than widening them.

### Echo is not structurally comparable (`==`/`!=`)

A type-safety hole was found and closed in the constrained-form type checker:
`Eq`/`Neq` previously accepted *any* two same-typed operands and claimed `bool`,
so `echo(1,2) == echo(3,4)` type-checked yet got stuck at runtime (the evaluator
only decides equality on scalars). Equality is now restricted to integer and
boolean values (`Typecheck.is_comparable`); compound values — echoes, arrays,
structs, refs, traces, unit — are rejected with a clear message.

This restores the progress half of type safety (well-typed ⇒ does not get stuck)
*and* tightens the "no rhino" boundary: a residue may be observed only through
`echo_visible` / `echo_witness`, never structurally compared. Pinned by the
conformance test `echo is not structurally comparable (==)`.

### Proof-layer echo bridge (Idris2)

The install/uninstall ABI is, formally, an echo-types story. `install` is a
non-injective (irreversible) state transformer; its echo/fibre is the residue of
that loss. `src/abi/Packages/Hello/Echo.idr` mirrors the echo-types fibre
`Echo f y := Σ (x : A), f x ≡ y` and proves the duality:

- **No residue under freshness** — `installInjectiveFresh`: reversibility *is*
  injectivity, so the fibre over a fresh install is a singleton.
- **A real residue otherwise** — `echoResidueNonTrivial`: a concrete two-element
  fibre exists when the package/file already exists.

This is the proof-layer (Idris2) counterpart of the in-language `echo[A,B]`
type, and it deliberately stays in the proof layer: per the boundary above, the
constrained form still does **not** import the echo-types proof theory. It
cashes out the comment carried by `TEcho` in `lib/ast.ml` — *"where a
computation cannot be reversed, it retains an echo of what was lost."*

### Observation (not yet a change): reversibility ⟺ trivial echo

The bridge makes precise a latent symmetry in the language: the reversibility
primitives (`incr`/`decr`/`swap`/`^=`) are exactly the *echo-free* operations,
while information-losing steps are exactly those with a non-trivial echo.

## Update 2026-06-04 — overwrite/drop discipline (non-copyable echoes are linear)

That symmetry is now **type-enforced**. A non-copyable `echo` is *linear*
(exactly once): the existing affine rule already forbade using it twice; the
type checker now also requires it be consumed *at least* once — its residue
projected via `echo_visible`/`echo_witness` — before it is either

- **reassigned** (overwrite), or
- allowed to **go out of scope** (drop).

Discarding a non-copyable echo's retained residue without consuming it is a
type error. This is the type-level reading of *"an irreversible step must yield
(and account for) an echo of what it loses"*: loss of a residue is no longer
silent.

Scope and boundary (deliberately narrow, still "no rhino"):

- **Copyable echoes** (e.g. `echo[i64, i64]`) are exempt — duplicating or
  dropping a bounded pair of copyable observations smuggles in nothing.
- **Reversible primitives** (`incr`/`decr`/`swap`/`^=`) are exempt — they lose
  nothing, so they need no echo. Echo still does **not** participate in their
  balancing; the connection is one-directional (loss ⇒ must-capture), not a new
  reversibility mechanism.
- First implementation covers **non-copyable echo bindings** only. Extending
  the same must-consume-before-overwrite/drop rule to other non-copyable
  carriers (structs, arrays) is a possible later generalisation.

Enforced in `lib/typecheck.ml` (the `live_echoes` tracker beside `affine_used`);
pinned by conformance tests *non-copyable echo: drop without consume rejected*,
*overwrite live value rejected*, and *overwrite after consume ok*.
