# Ecosystem Integration

Lago Grey is designed to integrate seamlessly with the broader hyperpolymath security ecosystem, providing a minimal, formally-verified foundation layer.

## Architecture Overview

```
┌───────────────────────────────────────────────────┐
│         Security Orchestration Layer              │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐           │
│  │ Svalinn │  │  Vordr  │  │  Selur  │           │
│  │ Crypto  │  │ Monitor │  │ Orchestr│           │
│  └────┬────┘  └────┬────┘  └────┬────┘           │
└───────┼────────────┼────────────┼────────────────┘
        │            │            │
        ▼            ▼            ▼
┌───────────────────────────────────────────────────┐
│              Lago Grey (14.6 MB)                   │
│  ┌──────────────┐  ┌──────────────┐              │
│  │ Post-Quantum │  │ Accountability│              │
│  │ Crypto Libs  │  │    Traces     │              │
│  │ (liboqs,     │  │  (obli-pkg)   │              │
│  │  libsodium)  │  └──────────────┘              │
│  └──────────────┘                                 │
│                                                    │
│  ┌──────────────────────────────────────┐        │
│  │    Formal Verification Layer          │        │
│  │    (Idris2 ABI Proofs)               │        │
│  └──────────────────────────────────────┘        │
├───────────────────────────────────────────────────┤
│           Distroless Foundation                    │
│           (~3 MB compressed)                       │
└───────────────────────────────────────────────────┘
```

## Integration Components

### 1. [[Svalinn Integration]] - Security & Cryptography
**What Svalinn Gets:**
- Pre-built post-quantum crypto libraries (liboqs, libsodium)
- Triple signature verification (Dilithium5 + Ed448 + SPHINCS+)
- Argon2id password hashing (512 MiB, 8 iterations)
- Formally verified crypto primitives (Coq proofs)

**Integration Points:**
- `/usr/lib/liboqs.a` - PQ crypto library
- `/usr/lib/libsodium.a` - Classical crypto
- `/usr/lib/libargon2.a` - Password hashing
- `/usr/include/oqs/` - liboqs headers
- `/usr/include/sodium.h` - libsodium header

**Use Case:** Svalinn can link against these libraries for container signing, authentication, and secure communication without maintaining its own crypto stack.

---

### 2. [[Vordr Integration]] - Monitoring & Defense
**What Vordr Gets:**
- Accountability traces from all package operations
- Minimal attack surface (14.6 MB = fewer CVEs to monitor)
- Immutable base (distroless) with traceable changes
- Audit logs in structured format

**Integration Points:**
- `/var/lib/obli-pkg/traces/` - Accountability trace database
- `/var/lib/obli-pkg/installed.db` - Package database
- Reversible operations for forensic analysis
- File-level change tracking

**Use Case:** Vordr monitors accountability traces to detect unauthorized changes, and can trigger automatic rollbacks via obli-pkg's reversibility features.

---

### 3. [[Selur Integration]] - Orchestration
**What Selur Gets:**
- OCI-compliant minimal containers (14.6 MB)
- Fast deployment (small size = quick pulls)
- Predictable behavior (formally verified)
- Secure defaults (PQ crypto, no legacy protocols)

**Integration Points:**
- Standard OCI container format
- Labels for orchestration metadata
- Health check endpoints (via obli-pkg)
- IPv6-only networking (no IPv4 legacy)

**Use Case:** Selur orchestrates Lago Grey containers across infrastructure, leveraging small size for rapid scaling and PQ crypto for secure inter-service communication.

---

### 4. [[Cerro Torre Integration]] - Sibling Architecture
**What Cerro Torre Gets:**
- Shared architectural patterns (ice formation metaphor)
- Common security model (PQ crypto by default)
- Interoperable package format (.zpkg)
- Formal verification alignment (Idris2 + Zig + Oblibeny)

**Integration Points:**
- Shared `ECOSYSTEM.scm` definitions
- Compatible `.zpkg` package format
- Common Idris2 ABI layer
- Unified Zig FFI patterns

**Use Case:** Cerro Torre and Lago Grey share components and patterns, enabling code reuse and consistent security posture across the stack.

---

## Deployment Patterns

### Pattern 1: Lago Grey as Base Image
```dockerfile
FROM ghcr.io/hyperpolymath/lago-grey:minimal

# Svalinn adds security services
COPY --from=svalinn /usr/bin/svalinn-agent /usr/bin/
COPY --from=vordr /usr/bin/vordr-monitor /usr/bin/

# Configure monitoring
ENV VORDR_TRACE_DIR=/var/lib/obli-pkg/traces
ENV SVALINN_CRYPTO_LIB=/usr/lib/liboqs.a

CMD ["/usr/bin/svalinn-agent"]
```

### Pattern 2: Sidecar Architecture
```yaml
# Kubernetes pod with Lago Grey + Svalinn + Vordr
apiVersion: v1
kind: Pod
spec:
  containers:
  - name: app
    image: ghcr.io/hyperpolymath/lago-grey:minimal

  - name: svalinn
    image: ghcr.io/hyperpolymath/svalinn:latest
    volumeMounts:
    - name: crypto-libs
      mountPath: /crypto

  - name: vordr
    image: ghcr.io/hyperpolymath/vordr:latest
    volumeMounts:
    - name: audit-traces
      mountPath: /var/lib/obli-pkg/traces
```

### Pattern 3: Shared Library Linking
```zig
// Svalinn service linking against Lago Grey crypto
const std = @import("std");
const c = @cImport({
    @cInclude("oqs/oqs.h");
    @cInclude("sodium.h");
});

pub fn verifySignature(msg: []const u8, sig: []const u8) !bool {
    // Use liboqs from Lago Grey
    const sig_alg = c.OQS_SIG_new(c.OQS_SIG_alg_dilithium_5);
    defer c.OQS_SIG_free(sig_alg);

    // Verification logic...
}
```

---

## Security Guarantees

When integrating with Lago Grey, your components inherit:

1. **Post-Quantum Security** - All crypto is PQ-ready by default
2. **Formal Verification** - Idris2 proofs guarantee correctness
3. **Audit Trail** - Every change is traceable and reversible
4. **Minimal TCB** - 14.6 MB means fewer components to trust
5. **No Legacy** - IPv6-only, HTTP/3, no SHA-1/MD5

---

## Integration Checklist

- [ ] Read [[Svalinn Integration]] for crypto linking
- [ ] Read [[Vordr Integration]] for monitoring setup
- [ ] Read [[Selur Integration]] for orchestration patterns
- [ ] Review [[Package Format]] for .zpkg structure
- [ ] Test signature verification with [[obli-pkg CLI]]
- [ ] Set up accountability trace monitoring
- [ ] Configure IPv6-only networking
- [ ] Verify Idris2 ABI compatibility

---

## Next Steps

1. **Choose your integration point** - Svalinn, Vordr, Selur, or all three
2. **Review the specific integration guide** - Linked above
3. **Test in development** - Use `lago-grey:minimal` image
4. **Deploy to production** - Leverage formal verification guarantees

---

**Questions?** File an issue: [github.com/hyperpolymath/oblibeny/issues](https://github.com/hyperpolymath/oblibeny/issues)
