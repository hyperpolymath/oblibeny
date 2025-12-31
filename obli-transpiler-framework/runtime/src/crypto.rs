// SPDX-License-Identifier: MIT OR Palimpsest-0.8
// Copyright (c) 2024 Hyperpolymath

//! Cryptographic utilities for ORAM
//!
//! Provides encryption, hashing, and key derivation used by ORAM implementations.

use aes_gcm::{
    aead::{Aead, KeyInit, OsRng},
    Aes256Gcm, Nonce,
};
use rand::RngCore;
use sha2::{Digest, Sha256};
use std::sync::atomic::{AtomicU64, Ordering};
use zeroize::{Zeroize, ZeroizeOnDrop};

/// Encryption key size (256 bits)
pub const KEY_SIZE: usize = 32;

/// Nonce size for AES-GCM
pub const NONCE_SIZE: usize = 12;

/// Authentication tag size
pub const TAG_SIZE: usize = 16;

/// Maximum nonce counter before key rotation required
/// Using 2^48 to stay well below birthday bound for 96-bit nonces
pub const MAX_NONCE_COUNTER: u64 = 1u64 << 48;

/// A secret key that zeroizes on drop
#[derive(Clone, Zeroize, ZeroizeOnDrop)]
pub struct SecretKey([u8; KEY_SIZE]);

impl SecretKey {
    /// Generate a new random key
    pub fn generate() -> Self {
        let mut key = [0u8; KEY_SIZE];
        OsRng.fill_bytes(&mut key);
        SecretKey(key)
    }

    /// Create from bytes (takes ownership)
    pub fn from_bytes(bytes: [u8; KEY_SIZE]) -> Self {
        SecretKey(bytes)
    }

    /// Get key bytes (be careful with this!)
    pub fn as_bytes(&self) -> &[u8; KEY_SIZE] {
        &self.0
    }
}

/// Stateful encryptor with counter-based nonces
///
/// Uses a monotonic counter for nonces to prevent reuse.
/// Each encryptor instance should be used with a single key.
pub struct Encryptor {
    cipher: Aes256Gcm,
    /// Random prefix for nonce (4 bytes)
    nonce_prefix: [u8; 4],
    /// Monotonic counter (8 bytes) - ensures unique nonces
    counter: AtomicU64,
}

impl Encryptor {
    /// Create a new encryptor with the given key
    pub fn new(key: &SecretKey) -> Self {
        let cipher = Aes256Gcm::new(key.0.as_ref().into());
        let mut nonce_prefix = [0u8; 4];
        OsRng.fill_bytes(&mut nonce_prefix);
        Encryptor {
            cipher,
            nonce_prefix,
            counter: AtomicU64::new(0),
        }
    }

    /// Encrypt data with automatic nonce generation
    ///
    /// # Panics
    /// Panics if nonce counter exceeds MAX_NONCE_COUNTER (requires key rotation)
    pub fn encrypt(&self, plaintext: &[u8]) -> Result<Vec<u8>, CryptoError> {
        let counter = self.counter.fetch_add(1, Ordering::SeqCst);
        if counter >= MAX_NONCE_COUNTER {
            return Err(CryptoError::NonceExhausted);
        }

        let mut nonce_bytes = [0u8; NONCE_SIZE];
        nonce_bytes[..4].copy_from_slice(&self.nonce_prefix);
        nonce_bytes[4..12].copy_from_slice(&counter.to_le_bytes());
        let nonce = Nonce::from_slice(&nonce_bytes);

        let ciphertext = self.cipher
            .encrypt(nonce, plaintext)
            .map_err(|_| CryptoError::EncryptionFailed)?;

        let mut result = Vec::with_capacity(NONCE_SIZE + ciphertext.len());
        result.extend_from_slice(&nonce_bytes);
        result.extend_from_slice(&ciphertext);
        Ok(result)
    }

    /// Decrypt data
    pub fn decrypt(&self, ciphertext: &[u8]) -> Result<Vec<u8>, CryptoError> {
        decrypt_with_cipher(&self.cipher, ciphertext)
    }

    /// Get current nonce counter (for diagnostics)
    pub fn nonce_count(&self) -> u64 {
        self.counter.load(Ordering::SeqCst)
    }
}

/// Encrypt a block of data using AES-256-GCM (stateless, random nonce)
///
/// WARNING: For high-volume encryption, use Encryptor instead to prevent
/// nonce reuse via birthday attack. This function is safe for occasional use.
///
/// Returns ciphertext with nonce prepended.
pub fn encrypt(key: &SecretKey, plaintext: &[u8]) -> Result<Vec<u8>, CryptoError> {
    let cipher = Aes256Gcm::new(key.0.as_ref().into());

    let mut nonce_bytes = [0u8; NONCE_SIZE];
    OsRng.fill_bytes(&mut nonce_bytes);
    let nonce = Nonce::from_slice(&nonce_bytes);

    let ciphertext = cipher
        .encrypt(nonce, plaintext)
        .map_err(|_| CryptoError::EncryptionFailed)?;

    let mut result = Vec::with_capacity(NONCE_SIZE + ciphertext.len());
    result.extend_from_slice(&nonce_bytes);
    result.extend_from_slice(&ciphertext);
    Ok(result)
}

/// Internal decrypt function using pre-created cipher
fn decrypt_with_cipher(cipher: &Aes256Gcm, ciphertext: &[u8]) -> Result<Vec<u8>, CryptoError> {
    if ciphertext.len() < NONCE_SIZE + TAG_SIZE {
        return Err(CryptoError::InvalidCiphertext);
    }

    let nonce = Nonce::from_slice(&ciphertext[..NONCE_SIZE]);
    let ct = &ciphertext[NONCE_SIZE..];

    cipher
        .decrypt(nonce, ct)
        .map_err(|_| CryptoError::DecryptionFailed)
}

/// Decrypt a block of data using AES-256-GCM
///
/// Expects nonce prepended to ciphertext.
pub fn decrypt(key: &SecretKey, ciphertext: &[u8]) -> Result<Vec<u8>, CryptoError> {
    let cipher = Aes256Gcm::new(key.0.as_ref().into());
    decrypt_with_cipher(&cipher, ciphertext)
}

/// Compute SHA-256 hash
pub fn sha256(data: &[u8]) -> [u8; 32] {
    let mut hasher = Sha256::new();
    hasher.update(data);
    hasher.finalize().into()
}

/// Compute BLAKE3 hash
pub fn blake3(data: &[u8]) -> [u8; 32] {
    blake3::hash(data).into()
}

/// Derive a key from a master key and path
pub fn derive_key(master: &SecretKey, path: &[u8]) -> SecretKey {
    let mut hasher = Sha256::new();
    hasher.update(master.as_bytes());
    hasher.update(path);
    SecretKey::from_bytes(hasher.finalize().into())
}

/// Pseudo-random function (PRF) for position map
pub fn prf(key: &SecretKey, input: u64) -> u64 {
    let mut hasher = Sha256::new();
    hasher.update(key.as_bytes());
    hasher.update(input.to_le_bytes());
    let hash: [u8; 32] = hasher.finalize().into();
    u64::from_le_bytes(hash[..8].try_into().unwrap())
}

/// Cryptographic errors
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CryptoError {
    InvalidCiphertext,
    DecryptionFailed,
    EncryptionFailed,
    InvalidKeyLength,
    /// Nonce counter exhausted - key rotation required
    NonceExhausted,
}

impl std::fmt::Display for CryptoError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CryptoError::InvalidCiphertext => write!(f, "invalid ciphertext"),
            CryptoError::DecryptionFailed => write!(f, "decryption failed"),
            CryptoError::EncryptionFailed => write!(f, "encryption failed"),
            CryptoError::InvalidKeyLength => write!(f, "invalid key length"),
            CryptoError::NonceExhausted => write!(f, "nonce counter exhausted - key rotation required"),
        }
    }
}

impl std::error::Error for CryptoError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encrypt_decrypt() {
        let key = SecretKey::generate();
        let plaintext = b"hello, ORAM world!";
        let ciphertext = encrypt(&key, plaintext).unwrap();
        let decrypted = decrypt(&key, &ciphertext).unwrap();
        assert_eq!(decrypted, plaintext);
    }

    #[test]
    fn test_encryptor_stateful() {
        let key = SecretKey::generate();
        let encryptor = Encryptor::new(&key);

        let plaintext = b"test message";
        let ct1 = encryptor.encrypt(plaintext).unwrap();
        let ct2 = encryptor.encrypt(plaintext).unwrap();

        // Same plaintext should produce different ciphertext (different nonces)
        assert_ne!(ct1, ct2);

        // Both should decrypt correctly
        assert_eq!(encryptor.decrypt(&ct1).unwrap(), plaintext);
        assert_eq!(encryptor.decrypt(&ct2).unwrap(), plaintext);

        // Counter should have advanced
        assert_eq!(encryptor.nonce_count(), 2);
    }

    #[test]
    fn test_decrypt_wrong_key() {
        let key1 = SecretKey::generate();
        let key2 = SecretKey::generate();
        let ciphertext = encrypt(&key1, b"secret data");
        assert!(decrypt(&key2, &ciphertext).is_err());
    }

    #[test]
    fn test_prf_deterministic() {
        let key = SecretKey::generate();
        assert_eq!(prf(&key, 42), prf(&key, 42));
        assert_ne!(prf(&key, 42), prf(&key, 43));
    }
}
