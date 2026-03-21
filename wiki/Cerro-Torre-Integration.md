# Cerro Torre Integration

**Cerro Torre** + **Lago Grey** = Unified architecture with shared patterns

## Overview

Cerro Torre and Lago Grey are **sibling components** in the hyperpolymath ecosystem, sharing:
- **Architectural patterns** (ice formation metaphor)
- **Security model** (post-quantum crypto by default)
- **Verification approach** (Idris2 + Zig + Oblibeny)
- **Package format** (.zpkg interoperability)

---

## Relationship

```
┌─────────────────────────────────────────┐
│     Hyperpolymath Ecosystem             │
├─────────────────────────────────────────┤
│                                          │
│  ┌─────────────┐      ┌──────────────┐ │
│  │ Cerro Torre │◄────►│ Lago Grey    │ │
│  │ (Component) │ Sibs │ (Distro)     │ │
│  └─────────────┘      └──────────────┘ │
│         │                     │         │
│         └─────────┬───────────┘         │
│                   │                     │
│              Shared Patterns:           │
│           • Ice metaphor                │
│           • PQ crypto                   │
│           • Idris2 + Zig                │
│           • .zpkg format                │
│                                          │
└─────────────────────────────────────────┘
```

**Documented in:**
- `oblibeny/ECOSYSTEM.scm`:
  ```scheme
  (related-projects
    (("cerro-torre" sibling-component)))
  ```

---

## Shared Patterns

### 1. Ice Formation Metaphor
Both use the same classification system:

| Formation | Size | Cerro Torre Example | Lago Grey Example |
|-----------|------|---------------------|-------------------|
| 🧊 **Floe** | < 1MB | Module, small lib | hello (29KB), libsodium (506KB) |
| 🏔️ **Iceberg** | 1-75MB | Component, subsystem | liboqs (11MB), full image (14.6MB) |
| 🌊 **Glacier** | 75MB+ | Complete system | N/A (Lago Grey stays minimal) |

**Benefit:** Consistent mental model across projects

### 2. Security Architecture
Both enforce the same crypto requirements:

```scheme
;; Shared security spec (from SECURITY-ARCHITECTURE.adoc)
(crypto-requirements
  (post-quantum (
    ("key-exchange" "Kyber-1024 (ML-KEM-1024)")
    ("signatures" "Dilithium5-AES (ML-DSA-87)")
    ("hash-signatures" "SPHINCS+-SHA2-256f-simple")))

  (classical-hybrid (
    ("signatures" "Ed448 (hybrid with Dilithium5)")))

  (symmetric (
    ("encryption" "XChaCha20-Poly1305 (256-bit)")
    ("hashing" "SHAKE3-512, BLAKE3")))

  (password-hashing (
    ("algorithm" "Argon2id")
    ("memory" "512 MiB")
    ("iterations" 8)
    ("parallelism" 4)))

  (forbidden (
    "IPv4" "HTTP/1.1" "SHA-1" "MD5" "Ed25519")))
```

**Integration:** Cerro Torre components can trust Lago Grey's crypto without re-verification

### 3. Formal Verification Stack
Both use the same tech stack:

| Layer | Technology | Purpose | Shared |
|-------|------------|---------|--------|
| **ABI** | Idris2 | Interface definitions + proofs | ✅ Yes |
| **FFI** | Zig | System implementation | ✅ Yes |
| **Coordination** | Oblibeny | Constrained-form operations | ✅ Yes |
| **Proofs** | Coq | Crypto primitive verification | ✅ Yes |

**Benefit:** Code reuse and unified verification approach

### 4. Package Format (.zpkg)
Both use the same package structure:

```
hello.zpkg/
├── metadata.scm          # Package info (shared format)
├── binaries/
│   ├── x86_64/hello
│   ├── aarch64/hello
│   └── riscv64/hello
├── signatures/
│   ├── package.sig.dilithium5   # Shared sig format
│   ├── package.sig.ed448
│   └── package.sig.sphincs
└── install.obl           # Oblibeny installation script
```

**Interoperability:** Cerro Torre and Lago Grey can share packages

---

## Integration Points

### 1. Shared Crypto Libraries
Cerro Torre components can link against Lago Grey's crypto:

```zig
// cerro-torre-component.zig
const oqs = @cImport(@cInclude("oqs/oqs.h"));  // From Lago Grey
const sodium = @cImport(@cInclude("sodium.h")); // From Lago Grey

pub fn signComponent(data: []const u8) !Signature {
    // Use Lago Grey's liboqs
    const sig_alg = oqs.OQS_SIG_new(oqs.OQS_SIG_alg_dilithium_5);
    defer oqs.OQS_SIG_free(sig_alg);

    // Signature logic...
}
```

**Dockerfile:**
```dockerfile
FROM ghcr.io/hyperpolymath/lago-grey:minimal AS crypto

FROM scratch AS cerro-torre-component
COPY --from=crypto /usr/lib/liboqs.a /lib/
COPY --from=crypto /usr/lib/libsodium.a /lib/
COPY --from=builder /cerro-torre-bin /usr/bin/
```

### 2. Shared ABI Definitions
Cerro Torre and Lago Grey share Idris2 interface definitions:

```idris
-- Shared: src/abi/Crypto.idr
module Crypto

import Data.Vect

-- Triple signature type (used by both projects)
public export
data TripleSignature : Type where
  MkTripleSig : (dilithium : Vect 4595 Bits8) ->
                (ed448 : Vect 114 Bits8) ->
                (sphincs : Vect 49856 Bits8) ->
                TripleSignature

-- Verification proof (all three must succeed)
public export
verifyTriple : TripleSignature -> Message -> PublicKeys -> Bool
verifyTriple sig msg keys =
  verifyDilithium sig.dilithium msg keys.dilithium &&
  verifyEd448 sig.ed448 msg keys.ed448 &&
  verifySphincs sig.sphincs msg keys.sphincs
```

**Repository structure:**
```
hyperpolymath/
├── oblibeny/              # Lago Grey
│   └── src/abi/
│       ├── Crypto.idr     # Shared with Cerro Torre
│       └── Package.idr    # Shared with Cerro Torre
└── cerro-torre/
    └── src/abi/
        ├── @Crypto.idr -> ../../oblibeny/src/abi/Crypto.idr
        └── Component.idr  # Cerro Torre specific
```

### 3. Shared Package Manager
Both can use `obli-pkg` for package operations:

```bash
# Install Cerro Torre component in Lago Grey
obli-pkg install cerro-torre-component-1.0.0.zpkg

# Install Lago Grey package in Cerro Torre system
obli-pkg install hello-1.0.0.zpkg
```

### 4. Compatible Accountability Traces
Same trace format, interoperable monitoring:

```scheme
;; Trace from Cerro Torre component install
(trace
  (id "abc-123")
  (operation "install")
  (package "cerro-torre-module-1.0.0.zpkg")
  (signature-verification (
    (dilithium5 "VALID")
    (ed448 "VALID")
    (sphincs "VALID")))
  ;; ... same format as Lago Grey traces
)
```

**Benefit:** Vordr can monitor both Cerro Torre and Lago Grey with same tools

---

## Deployment Patterns

### Pattern 1: Cerro Torre on Lago Grey
Run Cerro Torre components on Lago Grey base:

```dockerfile
FROM ghcr.io/hyperpolymath/lago-grey:minimal

# Add Cerro Torre component
COPY --from=builder /cerro-torre-component /usr/bin/

# Inherits:
# - PQ crypto libraries
# - obli-pkg for package management
# - Minimal attack surface (14.6 MB)
# - Formal verification guarantees

ENTRYPOINT ["/usr/bin/cerro-torre-component"]
```

### Pattern 2: Lago Grey as Cerro Torre Module
Include Lago Grey as a module in Cerro Torre:

```
cerro-torre/
├── modules/
│   ├── authentication/
│   ├── storage/
│   └── lago-grey/          # Lago Grey as module
│       ├── liboqs.a
│       ├── libsodium.a
│       └── obli-pkg
└── core/
    └── cerro-torre-main
```

### Pattern 3: Parallel Deployment
Deploy both side-by-side in same cluster:

```yaml
# Kubernetes namespace
apiVersion: v1
kind: Namespace
metadata:
  name: hyperpolymath

---
# Lago Grey deployment
apiVersion: apps/v1
kind: Deployment
metadata:
  name: lago-grey-services
  namespace: hyperpolymath
spec:
  template:
    spec:
      containers:
      - name: service
        image: ghcr.io/hyperpolymath/lago-grey:minimal

---
# Cerro Torre deployment
apiVersion: apps/v1
kind: Deployment
metadata:
  name: cerro-torre-components
  namespace: hyperpolymath
spec:
  template:
    spec:
      containers:
      - name: component
        image: ghcr.io/hyperpolymath/cerro-torre:latest
        # Shares crypto with Lago Grey
        volumeMounts:
        - name: shared-crypto
          mountPath: /crypto
```

---

## Code Sharing Example

### Shared Signature Verification

**File:** `shared/sig-verify.zig` (used by both projects)

```zig
// SPDX-License-Identifier: PMPL-1.0-or-later
// Shared by Cerro Torre and Lago Grey

const std = @import("std");
const oqs = @cImport(@cInclude("oqs/oqs.h"));
const sodium = @cImport(@cInclude("sodium.h"));

pub const TripleSignature = struct {
    dilithium5: []const u8,  // 4595 bytes
    ed448: []const u8,       // 114 bytes
    sphincs: []const u8,     // 49856 bytes

    pub fn verify(self: *const TripleSignature, message: []const u8, keys: PublicKeys) !bool {
        // All three must verify
        const d5_ok = try self.verifyDilithium5(message, keys.dilithium5);
        const ed_ok = try self.verifyEd448(message, keys.ed448);
        const sp_ok = try self.verifySphincs(message, keys.sphincs);

        return d5_ok and ed_ok and sp_ok;
    }

    // ... implementation ...
};
```

**Usage in Lago Grey:**
```zig
// oblibeny/ffi/zig/src/obli-pkg.zig
const shared = @import("../../../shared/sig-verify.zig");

fn verifyPackage(pkg_path: []const u8) !void {
    const sig = try loadSignature(pkg_path);
    const keys = try loadPublicKeys();

    if (!try sig.verify(pkg_data, keys)) {
        return error.SignatureVerificationFailed;
    }
}
```

**Usage in Cerro Torre:**
```zig
// cerro-torre/src/verify.zig
const shared = @import("../../../shared/sig-verify.zig");

fn verifyComponent(comp_path: []const u8) !void {
    const sig = try loadSignature(comp_path);
    const keys = try loadPublicKeys();

    if (!try sig.verify(comp_data, keys)) {
        return error.ComponentVerificationFailed;
    }
}
```

---

## Ecosystem Benefits

### For Cerro Torre
- **Reuse Lago Grey's crypto** - No need to maintain separate crypto stack
- **Inherit formal verification** - Idris2 proofs already written
- **Leverage minimal base** - Deploy on 14.6 MB foundation
- **Share package format** - Interoperable .zpkg packages

### For Lago Grey
- **Benefit from Cerro Torre development** - Shared code improvements
- **Expand use cases** - Cerro Torre components run on Lago Grey
- **Community growth** - Unified ecosystem attracts more contributors
- **Testing coverage** - Cerro Torre usage tests Lago Grey in new ways

---

## Migration Path

### From Standalone to Integrated

**Before:** Cerro Torre and Lago Grey as separate projects
```
hyperpolymath/
├── oblibeny/          # Lago Grey (isolated)
└── cerro-torre/       # Cerro Torre (isolated)
```

**After:** Shared components and patterns
```
hyperpolymath/
├── shared/            # NEW: Shared code
│   ├── crypto/
│   ├── abi/
│   └── packages/
├── oblibeny/          # Lago Grey (uses shared/)
└── cerro-torre/       # Cerro Torre (uses shared/)
```

---

## Compatibility Matrix

| Feature | Lago Grey | Cerro Torre | Compatible |
|---------|-----------|-------------|------------|
| **Ice Metaphor** | ✅ | ✅ | ✅ Yes |
| **PQ Crypto** | ✅ Dilithium5, Kyber-1024 | ✅ Same | ✅ Yes |
| **Idris2 ABI** | ✅ | ✅ | ✅ Yes |
| **Zig FFI** | ✅ | ✅ | ✅ Yes |
| **.zpkg Format** | ✅ | ✅ | ✅ Yes |
| **Triple Sigs** | ✅ | ✅ | ✅ Yes |
| **Accountability** | ✅ obli-pkg traces | ✅ Compatible | ✅ Yes |
| **IPv6-only** | ✅ | TBD | ⚠️ If implemented |

---

## Future Collaboration

### Planned Integrations
1. **Shared Component Library** - Common Zig modules
2. **Unified Package Registry** - Single .zpkg repository
3. **Cross-Project CI** - Test changes across both
4. **Joint Documentation** - Shared wiki sections

### Roadmap Alignment
- **Q1 2026:** Finalize shared ABI definitions
- **Q2 2026:** Unified package registry
- **Q3 2026:** Cross-project integration tests
- **Q4 2026:** Joint formal verification proofs

---

## Next Steps

1. ✅ Read this integration guide
2. [ ] Review shared code in `shared/` directory
3. [ ] Align Cerro Torre crypto with Lago Grey's liboqs
4. [ ] Test .zpkg package interoperability
5. [ ] Set up shared CI pipeline

---

**Back to:** [[Ecosystem Integration]]
