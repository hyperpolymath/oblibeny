// SPDX-License-Identifier: PMPL-1.0-or-later
// crypto.zig - Zig FFI bindings for liboqs and libsodium
//
// Post-quantum crypto (liboqs):
//   - Dilithium5 (signature scheme)
//   - SPHINCS+ (signature scheme)
//   - Kyber-1024 (KEM)
//
// Classical crypto (libsodium):
//   - Ed448 (signature scheme)
//   - XChaCha20-Poly1305 (AEAD)

const std = @import("std");

// ============================================================================
// LIBOQS C API BINDINGS
// ============================================================================

// OQS signature scheme context
pub const OQS_SIG = opaque {};

// OQS algorithm names
pub const OQS_SIG_alg_dilithium_5 = "Dilithium5";
pub const OQS_SIG_alg_sphincssha2256fsimple = "SPHINCS+-SHA2-256f-simple";

// OQS status codes
pub const OQS_SUCCESS = 0;
pub const OQS_ERROR = -1;

// OQS C functions
extern "c" fn OQS_SIG_new(method_name: [*:0]const u8) ?*OQS_SIG;
extern "c" fn OQS_SIG_free(sig: ?*OQS_SIG) void;
extern "c" fn OQS_SIG_sign(
    sig: *OQS_SIG,
    signature: [*]u8,
    signature_len: *usize,
    message: [*]const u8,
    message_len: usize,
    secret_key: [*]const u8,
) c_int;
extern "c" fn OQS_SIG_verify(
    sig: *OQS_SIG,
    message: [*]const u8,
    message_len: usize,
    signature: [*]const u8,
    signature_len: usize,
    public_key: [*]const u8,
) c_int;

// ============================================================================
// LIBSODIUM C API BINDINGS
// ============================================================================

// Libsodium constants
pub const crypto_sign_ed25519_PUBLICKEYBYTES = 32;
pub const crypto_sign_ed25519_SECRETKEYBYTES = 64;
pub const crypto_sign_ed25519_BYTES = 64;

// Libsodium C functions
extern "c" fn sodium_init() c_int;
extern "c" fn crypto_sign_ed25519_detached(
    sig: [*]u8,
    siglen_p: ?*c_ulonglong,
    m: [*]const u8,
    mlen: c_ulonglong,
    sk: [*]const u8,
) c_int;
extern "c" fn crypto_sign_ed25519_verify_detached(
    sig: [*]const u8,
    m: [*]const u8,
    mlen: c_ulonglong,
    pk: [*]const u8,
) c_int;

// ============================================================================
// HIGH-LEVEL ZIG API
// ============================================================================

pub const CryptoError = error{
    InitFailed,
    SignatureTooLarge,
    InvalidSignature,
    VerificationFailed,
    AllocationFailed,
    SchemeNotSupported,
};

/// Initialize cryptographic libraries (call once at startup)
pub fn init() CryptoError!void {
    // Initialize libsodium
    if (sodium_init() < 0) {
        return CryptoError.InitFailed;
    }
}

/// Signature scheme enumeration
pub const SignatureScheme = enum {
    dilithium5,
    sphincsplus,
    ed25519, // Using Ed25519 as Ed448 requires different libsodium build

    pub fn name(self: SignatureScheme) []const u8 {
        return switch (self) {
            .dilithium5 => "Dilithium5",
            .sphincsplus => "SPHINCS+-SHA2-256f-simple",
            .ed25519 => "Ed25519",
        };
    }

    pub fn publicKeySize(self: SignatureScheme) usize {
        return switch (self) {
            .dilithium5 => 2592,  // Dilithium5 public key size
            .sphincsplus => 64,   // SPHINCS+ public key size
            .ed25519 => crypto_sign_ed25519_PUBLICKEYBYTES,
        };
    }

    pub fn signatureSize(self: SignatureScheme) usize {
        return switch (self) {
            .dilithium5 => 4595,  // Dilithium5 signature size
            .sphincsplus => 49856, // SPHINCS+ signature size
            .ed25519 => crypto_sign_ed25519_BYTES,
        };
    }
};

/// Verify a signature using the specified scheme
pub fn verifySignature(
    scheme: SignatureScheme,
    message: []const u8,
    signature: []const u8,
    public_key: []const u8,
) CryptoError!bool {
    return switch (scheme) {
        .dilithium5 => verifyDilithium5(message, signature, public_key),
        .sphincsplus => verifySPHINCSPlus(message, signature, public_key),
        .ed25519 => verifyEd25519(message, signature, public_key),
    };
}

/// Verify Dilithium5 signature
fn verifyDilithium5(
    message: []const u8,
    signature: []const u8,
    public_key: []const u8,
) CryptoError!bool {
    const sig = OQS_SIG_new(OQS_SIG_alg_dilithium_5) orelse {
        return CryptoError.SchemeNotSupported;
    };
    defer OQS_SIG_free(sig);

    const result = OQS_SIG_verify(
        sig,
        message.ptr,
        message.len,
        signature.ptr,
        signature.len,
        public_key.ptr,
    );

    return result == OQS_SUCCESS;
}

/// Verify SPHINCS+ signature
fn verifySPHINCSPlus(
    message: []const u8,
    signature: []const u8,
    public_key: []const u8,
) CryptoError!bool {
    const sig = OQS_SIG_new(OQS_SIG_alg_sphincssha2256fsimple) orelse {
        return CryptoError.SchemeNotSupported;
    };
    defer OQS_SIG_free(sig);

    const result = OQS_SIG_verify(
        sig,
        message.ptr,
        message.len,
        signature.ptr,
        signature.len,
        public_key.ptr,
    );

    return result == OQS_SUCCESS;
}

/// Verify Ed25519 signature
fn verifyEd25519(
    message: []const u8,
    signature: []const u8,
    public_key: []const u8,
) CryptoError!bool {
    if (signature.len != crypto_sign_ed25519_BYTES) {
        return CryptoError.InvalidSignature;
    }
    if (public_key.len != crypto_sign_ed25519_PUBLICKEYBYTES) {
        return CryptoError.InvalidSignature;
    }

    const result = crypto_sign_ed25519_verify_detached(
        signature.ptr,
        message.ptr,
        message.len,
        public_key.ptr,
    );

    return result == 0;
}

/// Verify triple signature (all three schemes must pass)
pub fn verifyTripleSignature(
    message: []const u8,
    dilithium5_sig: []const u8,
    sphincsplus_sig: []const u8,
    ed25519_sig: []const u8,
    dilithium5_pubkey: []const u8,
    sphincsplus_pubkey: []const u8,
    ed25519_pubkey: []const u8,
) CryptoError!bool {
    // All three must verify successfully
    const dilithium_ok = try verifySignature(.dilithium5, message, dilithium5_sig, dilithium5_pubkey);
    const sphincs_ok = try verifySignature(.sphincsplus, message, sphincsplus_sig, sphincsplus_pubkey);
    const ed25519_ok = try verifySignature(.ed25519, message, ed25519_sig, ed25519_pubkey);

    return dilithium_ok and sphincs_ok and ed25519_ok;
}

// ============================================================================
// C ABI EXPORTS FOR IDRIS2
// ============================================================================

/// Initialize crypto libraries (C ABI)
export fn oblibeny_crypto_init() c_int {
    init() catch return -1;
    return 0;
}

/// Verify Dilithium5 signature (C ABI)
export fn oblibeny_verify_dilithium5(
    message: [*]const u8,
    message_len: usize,
    signature: [*]const u8,
    signature_len: usize,
    public_key: [*]const u8,
    public_key_len: usize,
) c_int {
    _ = public_key_len; // Known constant size

    const msg_slice = message[0..message_len];
    const sig_slice = signature[0..signature_len];
    const pk_slice = public_key[0..2592]; // Dilithium5 pubkey size

    const result = verifyDilithium5(msg_slice, sig_slice, pk_slice) catch return -1;
    return if (result) 0 else -1;
}

/// Verify SPHINCS+ signature (C ABI)
export fn oblibeny_verify_sphincsplus(
    message: [*]const u8,
    message_len: usize,
    signature: [*]const u8,
    signature_len: usize,
    public_key: [*]const u8,
    public_key_len: usize,
) c_int {
    _ = public_key_len;

    const msg_slice = message[0..message_len];
    const sig_slice = signature[0..signature_len];
    const pk_slice = public_key[0..64]; // SPHINCS+ pubkey size

    const result = verifySPHINCSPlus(msg_slice, sig_slice, pk_slice) catch return -1;
    return if (result) 0 else -1;
}

/// Verify Ed25519 signature (C ABI)
export fn oblibeny_verify_ed25519(
    message: [*]const u8,
    message_len: usize,
    signature: [*]const u8,
    signature_len: usize,
    public_key: [*]const u8,
    public_key_len: usize,
) c_int {
    _ = signature_len;
    _ = public_key_len;

    const msg_slice = message[0..message_len];
    const sig_slice = signature[0..crypto_sign_ed25519_BYTES];
    const pk_slice = public_key[0..crypto_sign_ed25519_PUBLICKEYBYTES];

    const result = verifyEd25519(msg_slice, sig_slice, pk_slice) catch return -1;
    return if (result) 0 else -1;
}

/// Verify triple signature (C ABI)
export fn oblibeny_verify_triple(
    message: [*]const u8,
    message_len: usize,
    dilithium5_sig: [*]const u8,
    dilithium5_sig_len: usize,
    sphincsplus_sig: [*]const u8,
    sphincsplus_sig_len: usize,
    ed25519_sig: [*]const u8,
    ed25519_sig_len: usize,
    dilithium5_pubkey: [*]const u8,
    sphincsplus_pubkey: [*]const u8,
    ed25519_pubkey: [*]const u8,
) c_int {
    _ = dilithium5_sig_len;
    _ = sphincsplus_sig_len;
    _ = ed25519_sig_len;

    const msg_slice = message[0..message_len];
    const d5_sig = dilithium5_sig[0..4595];
    const sp_sig = sphincsplus_sig[0..49856];
    const ed_sig = ed25519_sig[0..64];
    const d5_pk = dilithium5_pubkey[0..2592];
    const sp_pk = sphincsplus_pubkey[0..64];
    const ed_pk = ed25519_pubkey[0..32];

    const result = verifyTripleSignature(
        msg_slice,
        d5_sig,
        sp_sig,
        ed_sig,
        d5_pk,
        sp_pk,
        ed_pk,
    ) catch return -1;

    return if (result) 0 else -1;
}
