<!--
SPDX-License-Identifier: MPL-2.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
-->

# Proof debt

Soundness-relevant escape-hatch ledger for oblibeny, per the
[Trusted-Base Reduction Policy](https://github.com/hyperpolymath/standards/blob/main/docs/TRUSTED-BASE-REDUCTION-POLICY.adoc).

Scope: the automated scan
([`docs/tech-debt-2026-05-26.md`](./tech-debt-2026-05-26.md)) counts **2**
soundness-relevant markers in proof-bearing files, both Idris2 `believe_me`
coercions (`Idr-blv= 2`). Each is triaged below into one of: (a) discharged,
(b) budgeted with a refutation budget, (c) necessary axiom, or (d) DEBT.

## (a) Discharged in this repo

_None yet._

## (b) Budgeted — tested with refutation budget

_None yet._

## (c) Necessary axiom

_None._ The post-quantum signature `postulate`s in `src/abi/Crypto.idr`
(`signatureCorrectness`, `signatureSoundness`, `tripleSignatureConjunction`)
state properties of the external C crypto libraries (liboqs / libsodium) reached
over FFI; they are not counted by the soundness-escape-hatch scan, but are
tracked here for completeness as boundary assumptions about linked code rather
than in-language coercions.

## (d) DEBT — actively to be closed

### `src/abi/packages/hello/Interface.idr:69` — `installReversible`

- **Construct:** `believe_me ()` standing in for the proof term.
- **Proof obligation:** `uninstall pkg (install pkg state) = state` — installation
  is reversible (package removed after being added; file deleted after being
  created), given the package is not already installed and its binary path does
  not already exist.
- **Owner:** maintainer (@hyperpolymath, jonathan.jewell@gmail.com).
- **Plan:** replace the coercion with an equational proof: case-split on the
  `SystemState` update and discharge via the list/file-existence lemmas implied
  by the `notInstalled` / `fileNotExists` hypotheses.
- **Deadline:** INDEFINITE — gated on stabilising the FFI ABI surface in
  `src/abi/`; the proof is only meaningful once the install/uninstall model is
  frozen. Tracked here so the obligation is not silently lost.

### `src/abi/packages/hello/Interface.idr:78` — `doubleInstallIdempotent`

- **Construct:** `believe_me ()` standing in for the proof term.
- **Proof obligation:** `install pkg (install pkg state) = install pkg state` —
  installing twice equals installing once (idempotence).
- **Owner:** maintainer (@hyperpolymath, jonathan.jewell@gmail.com).
- **Plan:** prove by reducing the second `install` against the post-state of the
  first; the package-list insertion and file-creation are both idempotent, so
  the obligation follows once those component lemmas are established.
- **Deadline:** INDEFINITE — gated on the same FFI ABI stabilisation as
  `installReversible` above.
