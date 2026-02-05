# Svalinn Integration

**Svalinn** + **Lago Grey** = Post-quantum security layer for your infrastructure

## Overview

Svalinn integrates with Lago Grey to leverage:
- Pre-built post-quantum crypto libraries (no need to maintain your own)
- Triple signature verification (Dilithium5 + Ed448 + SPHINCS+)
- Formally verified crypto primitives (Coq proofs)
- Zero-dependency static libraries

---

## What Svalinn Gets from Lago Grey

### 1. Post-Quantum Cryptography
**Library:** `/usr/lib/liboqs.a` (11 MB, full build)

**Algorithms included:**
- **ML-KEM (Kyber-1024)** - Key encapsulation
- **ML-DSA (Dilithium5)** - Digital signatures
- **SPHINCS+** - Stateless hash-based signatures
- Plus: BIKE, Classic McEliece, FrodoKEM, NTRU Prime, HQC, Falcon, MAYO, CROSS

**Headers:** `/usr/include/oqs/*.h`

**Usage example:**
```c
#include <oqs/oqs.h>

// Generate Dilithium5 keypair
OQS_SIG *sig = OQS_SIG_new(OQS_SIG_alg_dilithium_5);
uint8_t public_key[sig->length_public_key];
uint8_t secret_key[sig->length_secret_key];
OQS_SIG_keypair(sig, public_key, secret_key);

// Sign message
uint8_t message[] = "Secure container signature";
uint8_t signature[sig->length_signature];
size_t signature_len;
OQS_SIG_sign(sig, signature, &signature_len, message,
             sizeof(message), secret_key);

// Verify
if (OQS_SIG_verify(sig, message, sizeof(message),
                   signature, signature_len, public_key) == OQS_SUCCESS) {
    // Signature valid!
}

OQS_SIG_free(sig);
```

### 2. Classical Cryptography
**Library:** `/usr/lib/libsodium.a` (506 KB)

**Algorithms included:**
- **Ed448** - EdDSA signatures (hybrid with PQ)
- **XChaCha20-Poly1305** - Authenticated encryption
- **BLAKE2b** - Hashing
- **Argon2id** - Password hashing (see libargon2 below)

**Headers:** `/usr/include/sodium.h`

**Usage example:**
```c
#include <sodium.h>

if (sodium_init() < 0) {
    // Panic! The library couldn't be initialized
}

// Ed448 signing (for hybrid PQ+classical)
unsigned char pk[crypto_sign_ed25519_PUBLICKEYBYTES];
unsigned char sk[crypto_sign_ed25519_SECRETKEYBYTES];
crypto_sign_ed25519_keypair(pk, sk);

unsigned char sig[crypto_sign_ed25519_BYTES];
crypto_sign_ed25519_detached(sig, NULL, message, message_len, sk);

// Verify
if (crypto_sign_ed25519_verify_detached(sig, message, message_len, pk) == 0) {
    // Signature valid!
}
```

### 3. Password Hashing
**Library:** `/usr/lib/libargon2.a` (42 KB)

**Configuration:**
- Memory: 512 MiB
- Iterations: 8
- Parallelism: 4 lanes
- Variant: Argon2id (hybrid mode)

**Headers:** `/usr/include/argon2.h`

**Usage example:**
```c
#include <argon2.h>

#define HASHLEN 32
#define SALTLEN 16

uint8_t hash[HASHLEN];
uint8_t salt[SALTLEN];

// Generate salt (using libsodium)
randombytes_buf(salt, SALTLEN);

// Hash password
argon2id_hash_raw(
    8,              // iterations
    512 * 1024,     // memory (512 MiB in KiB)
    4,              // parallelism
    password, password_len,
    salt, SALTLEN,
    hash, HASHLEN
);
```

---

## Integration Patterns

### Pattern 1: Static Linking (Recommended)
Link Svalinn directly against Lago Grey crypto libraries:

```bash
# Build Svalinn with Lago Grey crypto
zig build-exe src/svalinn.zig \
    -target x86_64-linux-musl \
    -I/usr/include \
    -L/usr/lib \
    -lliboqs \
    -llibsodium \
    -llibargon2 \
    -O ReleaseSafe \
    -static
```

**Advantages:**
- Zero runtime dependencies
- Single binary deployment
- Guaranteed version compatibility

### Pattern 2: Shared Container Volume
Mount Lago Grey libraries into Svalinn container:

```dockerfile
FROM ghcr.io/hyperpolymath/svalinn:latest

# Copy crypto from Lago Grey
COPY --from=ghcr.io/hyperpolymath/lago-grey:minimal /usr/lib/*.a /usr/lib/
COPY --from=ghcr.io/hyperpolymath/lago-grey:minimal /usr/include /usr/include/

# Rebuild Svalinn with Lago Grey crypto
RUN zig build-exe ... -lliboqs -llibsodium -llibargon2
```

### Pattern 3: Library Container
Create a crypto-only container for sharing:

```dockerfile
FROM scratch
COPY --from=ghcr.io/hyperpolymath/lago-grey:minimal /usr/lib/*.a /lib/
COPY --from=ghcr.io/hyperpolymath/lago-grey:minimal /usr/include /include/
```

---

## Triple Signature Verification

Lago Grey's `.zpkg` packages use **triple signatures** for maximum security:

```c
// Verify all three signatures (ALL must pass)
bool verify_zpkg(const char *pkg_path) {
    // 1. Verify Dilithium5 (post-quantum)
    if (!verify_dilithium5(pkg_path)) return false;

    // 2. Verify Ed448 (classical, hybrid protection)
    if (!verify_ed448(pkg_path)) return false;

    // 3. Verify SPHINCS+ (stateless hash-based)
    if (!verify_sphincs(pkg_path)) return false;

    return true;  // All three passed!
}
```

**Why three signatures?**
- **Dilithium5** - Primary PQ signature (lattice-based)
- **Ed448** - Classical fallback if PQ breaks
- **SPHINCS+** - Hash-based (survives quantum + mathematical attacks)

**Signature overhead:** ~54 KB per package
- Dilithium5: ~4,595 bytes
- Ed448: ~114 bytes
- SPHINCS+: ~49,856 bytes

---

## Svalinn-Specific Features

### Container Signing
Use Lago Grey crypto to sign container images:

```zig
const std = @import("std");
const oqs = @cImport(@cInclude("oqs/oqs.h"));

pub fn signContainer(image_digest: []const u8) ![]const u8 {
    const sig_alg = oqs.OQS_SIG_new(oqs.OQS_SIG_alg_dilithium_5);
    defer oqs.OQS_SIG_free(sig_alg);

    var signature = try allocator.alloc(u8, sig_alg.length_signature);
    var sig_len: usize = undefined;

    _ = oqs.OQS_SIG_sign(
        sig_alg,
        signature.ptr,
        &sig_len,
        image_digest.ptr,
        image_digest.len,
        secret_key.ptr
    );

    return signature[0..sig_len];
}
```

### Authentication
Implement PQ-safe authentication:

```zig
pub fn authenticateUser(credentials: Credentials) !AuthToken {
    // Hash password with Argon2id
    const hash = try hashPassword(credentials.password);

    // Verify against stored hash
    if (!verifyPasswordHash(hash, stored_hash)) {
        return error.AuthFailed;
    }

    // Generate PQ-safe session token (Kyber-1024 KEM)
    return try generateSessionToken();
}
```

### Secure Communication
Establish PQ-safe channels:

```zig
pub fn establishSecureChannel(peer: Peer) !Channel {
    // Kyber-1024 key exchange
    const shared_secret = try kyberKeyExchange(peer);

    // XChaCha20-Poly1305 for data encryption
    const cipher = try initCipher(shared_secret);

    return Channel{ .cipher = cipher };
}
```

---

## Performance Considerations

### Library Sizes
| Library | Size | Load Time | Memory |
|---------|------|-----------|--------|
| liboqs.a | 11 MB | ~50ms | ~16 MB RAM |
| libsodium.a | 506 KB | ~5ms | ~1 MB RAM |
| libargon2.a | 42 KB | <1ms | ~512 MB (during hash) |

### Optimization: Minimal liboqs Build
For production Svalinn, use minimal liboqs (coming soon):
- Only Kyber-1024, Dilithium5, SPHINCS+
- Size: ~5 MB (56% reduction)
- Faster load times
- Same security guarantees

---

## Example: Full Svalinn Integration

```zig
// svalinn-with-lago-grey.zig
const std = @import("std");
const oqs = @cImport(@cInclude("oqs/oqs.h"));
const sodium = @cImport(@cInclude("sodium.h"));
const argon2 = @cImport(@cInclude("argon2.h"));

pub const Svalinn = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Svalinn {
        // Initialize libsodium
        if (sodium.sodium_init() < 0) {
            return error.SodiumInitFailed;
        }

        return Svalinn{ .allocator = allocator };
    }

    pub fn signContainer(self: *Svalinn, digest: []const u8) !Signature {
        // Triple signature
        const dilithium_sig = try self.signDilithium5(digest);
        const ed448_sig = try self.signEd448(digest);
        const sphincs_sig = try self.signSPHINCS(digest);

        return Signature{
            .dilithium5 = dilithium_sig,
            .ed448 = ed448_sig,
            .sphincs = sphincs_sig,
        };
    }

    pub fn verifyContainer(self: *Svalinn, digest: []const u8, sig: Signature) !bool {
        // All three must verify
        if (!try self.verifyDilithium5(digest, sig.dilithium5)) return false;
        if (!try self.verifyEd448(digest, sig.ed448)) return false;
        if (!try self.verifySphincs(digest, sig.sphincs)) return false;

        return true;
    }

    // ... implementation details ...
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var svalinn = try Svalinn.init(gpa.allocator());

    // Sign container
    const digest = "sha256:abc123...";
    const signature = try svalinn.signContainer(digest);

    // Verify later
    if (try svalinn.verifyContainer(digest, signature)) {
        std.debug.print("✅ Container signature valid!\\n", .{});
    }
}
```

---

## Testing Integration

### Unit Tests
```bash
# Test Svalinn with Lago Grey crypto
zig test src/svalinn.zig \
    -I/usr/include \
    -L/usr/lib \
    -lliboqs \
    -llibsodium \
    -llibargon2
```

### Integration Tests
```bash
# Build test container
podman build -f Dockerfile.svalinn-test -t svalinn-test .

# Run crypto tests
podman run --rm svalinn-test /usr/bin/run-tests

# Expected output:
# ✅ Dilithium5 signing: PASS
# ✅ Ed448 signing: PASS
# ✅ SPHINCS+ signing: PASS
# ✅ Triple verification: PASS
# ✅ Argon2id hashing: PASS
```

---

## Troubleshooting

### Issue: Undefined symbol errors
**Solution:** Link all three libraries in correct order:
```bash
-lliboqs -llibsodium -llibargon2
```

### Issue: "OQS_ERROR" during runtime
**Solution:** Check algorithm availability:
```c
if (!OQS_SIG_alg_is_enabled(OQS_SIG_alg_dilithium_5)) {
    fprintf(stderr, "Dilithium5 not available!\n");
}
```

### Issue: High memory usage
**Solution:** Argon2 uses 512 MiB by design. For lower memory:
```c
argon2id_hash_raw(
    8,          // iterations
    128 * 1024, // 128 MiB instead of 512 MiB
    4,          // parallelism
    ...
);
```

---

## Next Steps

1. ✅ Read this integration guide
2. [ ] Review [[Package Format]] for .zpkg structure
3. [ ] Test signature verification locally
4. [ ] Integrate crypto into Svalinn codebase
5. [ ] Deploy Svalinn + Lago Grey to staging
6. [ ] Monitor performance and adjust

---

**Back to:** [[Ecosystem Integration]]
