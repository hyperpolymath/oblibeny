# Lago Grey Wiki

**Welcome to the Lago Grey documentation!**

Lago Grey is a 14.6 MB minimal Linux distribution with post-quantum cryptography, formal verification, and community governance.

## Quick Links

### Getting Started
- [[Installation]]
- [[Quick Start Guide]]
- [[Building from Source]]

### Core Concepts
- [[Architecture Overview]]
- [[Ice Formation Metaphor]] (Floes, Icebergs, Glaciers)
- [[Security Model]]
- [[Package Format]] (.zpkg)

### Integration
- **[[Ecosystem Integration]]** ← Start here for integration with Svalinn, Vordr, Selur, Cerro Torre
- [[Svalinn Integration]] - Security layer & crypto
- [[Vordr Integration]] - Monitoring & defense
- [[Selur Integration]] - Orchestration
- [[Cerro Torre Integration]] - Sibling architecture
- [[Stapeln Integration]] - Visual designer

### Development
- [[Contributing]]
- [[Build System]] (Justfile reference)
- [[Testing]]
- [[Release Process]]

### Reference
- [[API Reference]]
- [[obli-pkg CLI]]
- [[Dockerfile Reference]]
- [[Signature Verification]]

---

## What is Lago Grey?

**Lago Grey** is a minimal, formally-verified Linux distribution designed from the ground up for security-critical systems.

### Key Features
- **14.6 MB** - Small Iceberg classification 🏔️
- **Post-quantum ready** - Dilithium5, Kyber-1024, SPHINCS+ by default
- **Formally verified** - Idris2 ABI proofs, machine-checked in CI (PRs #51/#52)
- **Reversible operations** - All package changes are traceable and reversible
- **Community governed** - MPL-2.0 license
- **Zero corporate control** - True community project

### Comparison

| Distribution | Size | PQ Crypto | Formal Verification | Governance |
|--------------|------|-----------|---------------------|------------|
| **Lago Grey** | **14.6 MB** | ✅ Default | ✅ Idris2 | Community |
| Chainguard | 20-40 MB | ❌ | ❌ | Corporate |
| Alpine | 60 MB | ❌ | ❌ | Community |
| Debian | 124 MB | ❌ | ❌ | Community |

---

## Project Status

- **Version:** 0.1.0-alpha
- **Status:** Proof of Concept → MVP
- **Completion:** 50%
- **License:** MPL-2.0
- **Repository:** [github.com/hyperpolymath/oblibeny](https://github.com/hyperpolymath/oblibeny)

### Status update — 2026-06-12 (language toolchain)

The oblibeny language toolchain that underpins Lago Grey moved substantially in June 2026 (PRs #51–#56):

- **Real Idris2 ABI proofs** (#51): the proof layer's placeholder stubs were replaced with genuine, total, machine-checked proofs (`installReversible`, `doubleInstallIdempotent`); one `believe_me`-masked theorem was found to be *false as written* and corrected. A type-safety hole in the constrained-form checker (`==`/`!=` accepting non-scalar operands) was closed.
- **CI gate** (#52, #53): every PR now runs the OCaml build, the 27-test conformance suite, and the Idris2 proof type-check, with an escape-hatch guard (no `believe_me`/`postulate`/`assert_total`). Hypatia and Scorecard wrappers were root-caused and fixed; instant-sync is presence-gated on its token.
- **Echo linearity** (#55): non-copyable `echo[A,B]` residues are now *linear* — consumed exactly once; discarding a residue unconsumed is a type error.
- **Hygiene** (#54, #56): orphaned submodule gitlinks removed; metadata migrated to `.machine_readable/6a2/*.a2ml`; the Zig FFI compiles under Zig 0.13 (the link step needs a system liboqs — exactly the library Lago Grey ships).

The Lago Grey distribution status above (0.1.0-alpha, PoC → MVP) is unchanged by this; the language layer is in active development with the honest blocker list in `.machine_readable/6a2/STATE.a2ml`.

---

## Quick Start

```bash
# Pull the image
podman pull ghcr.io/hyperpolymath/lago-grey:minimal

# Run hello demo
podman run --rm ghcr.io/hyperpolymath/lago-grey:minimal

# Check package manager
podman run --rm --entrypoint=/usr/bin/obli-pkg \
  ghcr.io/hyperpolymath/lago-grey:minimal version
```

---

## Community

- **Author:** Jonathan D.A. Jewell
- **Organization:** hyperpolymath
- **Chat:** TBD
- **Issues:** [GitHub Issues](https://github.com/hyperpolymath/oblibeny/issues)
- **Discussions:** [GitHub Discussions](https://github.com/hyperpolymath/oblibeny/discussions)
