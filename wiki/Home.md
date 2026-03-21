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
- [[Build System]] (justfile reference)
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
- **Formally verified** - Idris2 ABI proofs + Coq crypto proofs
- **Reversible operations** - All package changes are traceable and reversible
- **Community governed** - PMPL-1.0-or-later license
- **Zero corporate control** - True community project

### Comparison

| Distribution | Size | PQ Crypto | Formal Verification | Governance |
|--------------|------|-----------|---------------------|------------|
| **Lago Grey** | **14.6 MB** | ✅ Default | ✅ Idris2 + Coq | Community |
| Chainguard | 20-40 MB | ❌ | ❌ | Corporate |
| Alpine | 60 MB | ❌ | ❌ | Community |
| Debian | 124 MB | ❌ | ❌ | Community |

---

## Project Status

- **Version:** 0.1.0-alpha
- **Status:** Proof of Concept → MVP
- **Completion:** 50%
- **License:** PMPL-1.0-or-later
- **Repository:** [github.com/hyperpolymath/oblibeny](https://github.com/hyperpolymath/oblibeny)

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
