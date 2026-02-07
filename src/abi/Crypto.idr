-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

module Crypto

import Data.Buffer

-- ============================================================================
-- POST-QUANTUM CRYPTOGRAPHY ABI
-- ============================================================================

-- Signature scheme enumeration
public export
data SignatureScheme = Dilithium5 | SPHINCSPlus | Ed25519

-- Signature scheme properties
public export
publicKeySize : SignatureScheme -> Nat
publicKeySize Dilithium5 = 2592
publicKeySize SPHINCSPlus = 64
publicKeySize Ed25519 = 32

public export
signatureSize : SignatureScheme -> Nat
signatureSize Dilithium5 = 4595
signatureSize SPHINCSPlus = 49856
signatureSize Ed25519 = 64

-- ============================================================================
-- SIGNATURE VERIFICATION
-- ============================================================================

-- Verification result
public export
data VerificationResult = Valid | Invalid

public export
Eq VerificationResult where
  Valid == Valid = True
  Invalid == Invalid = True
  _ == _ = False

-- FFI declarations for C functions
%foreign "C:oblibeny_crypto_init, liboblibeny_crypto"
prim__crypto_init : PrimIO Int

%foreign "C:oblibeny_verify_dilithium5, liboblibeny_crypto"
prim__verify_dilithium5 : Buffer -> Int -> Buffer -> Int -> Buffer -> Int -> PrimIO Int

%foreign "C:oblibeny_verify_sphincsplus, liboblibeny_crypto"
prim__verify_sphincsplus : Buffer -> Int -> Buffer -> Int -> Buffer -> Int -> PrimIO Int

%foreign "C:oblibeny_verify_ed25519, liboblibeny_crypto"
prim__verify_ed25519 : Buffer -> Int -> Buffer -> Int -> Buffer -> Int -> PrimIO Int

%foreign "C:oblibeny_verify_triple, liboblibeny_crypto"
prim__verify_triple : Buffer -> Int
                   -> Buffer -> Int  -- Dilithium5 sig
                   -> Buffer -> Int  -- SPHINCS+ sig
                   -> Buffer -> Int  -- Ed25519 sig
                   -> Buffer         -- Dilithium5 pubkey
                   -> Buffer         -- SPHINCS+ pubkey
                   -> Buffer         -- Ed25519 pubkey
                   -> PrimIO Int

-- ============================================================================
-- HIGH-LEVEL SAFE API
-- ============================================================================

-- Initialize crypto libraries (call once at startup)
public export
initCrypto : IO (Either String ())
initCrypto = do
  result <- primIO prim__crypto_init
  pure $ if result == 0
         then Right ()
         else Left "Failed to initialize cryptographic libraries"

-- Verify a signature
public export
verifySignature : SignatureScheme
                -> Buffer  -- message
                -> Nat     -- message length
                -> Buffer  -- signature
                -> Nat     -- signature length
                -> Buffer  -- public key
                -> Nat     -- public key length
                -> IO VerificationResult
verifySignature Dilithium5 msg msgLen sig sigLen pk pkLen = do
  result <- primIO $ prim__verify_dilithium5
    msg (cast msgLen)
    sig (cast sigLen)
    pk (cast pkLen)
  pure $ if result == 0 then Valid else Invalid

verifySignature SPHINCSPlus msg msgLen sig sigLen pk pkLen = do
  result <- primIO $ prim__verify_sphincsplus
    msg (cast msgLen)
    sig (cast sigLen)
    pk (cast pkLen)
  pure $ if result == 0 then Valid else Invalid

verifySignature Ed25519 msg msgLen sig sigLen pk pkLen = do
  result <- primIO $ prim__verify_ed25519
    msg (cast msgLen)
    sig (cast sigLen)
    pk (cast pkLen)
  pure $ if result == 0 then Valid else Invalid

-- Verify triple signature (all three must pass)
public export
verifyTripleSignature : Buffer -> Nat  -- message
                     -> Buffer -> Nat  -- Dilithium5 signature
                     -> Buffer -> Nat  -- SPHINCS+ signature
                     -> Buffer -> Nat  -- Ed25519 signature
                     -> Buffer         -- Dilithium5 public key
                     -> Buffer         -- SPHINCS+ public key
                     -> Buffer         -- Ed25519 public key
                     -> IO VerificationResult
verifyTripleSignature msg msgLen d5Sig d5SigLen spSig spSigLen edSig edSigLen d5Pk spPk edPk = do
  result <- primIO $ prim__verify_triple
    msg (cast msgLen)
    d5Sig (cast d5SigLen)
    spSig (cast spSigLen)
    edSig (cast edSigLen)
    d5Pk
    spPk
    edPk
  pure $ if result == 0 then Valid else Invalid

-- ============================================================================
-- FORMAL PROPERTIES (To be proved)
-- ============================================================================

-- PROPERTY: Valid signature always verifies
postulate
signatureCorrectness : (scheme : SignatureScheme)
                    -> (msg : Buffer)
                    -> (msgLen : Nat)
                    -> (sig : Buffer)
                    -> (sigLen : Nat)
                    -> (pk : Buffer)
                    -> (pkLen : Nat)
                    -> verifySignature scheme msg msgLen sig sigLen pk pkLen = pure Valid
                    -> IO VerificationResult

-- PROPERTY: Invalid signature never verifies
postulate
signatureSoundness : (scheme : SignatureScheme)
                  -> (msg : Buffer)
                  -> (msgLen : Nat)
                  -> (sig : Buffer)
                  -> (sigLen : Nat)
                  -> (pk : Buffer)
                  -> (pkLen : Nat)
                  -> Not (verifySignature scheme msg msgLen sig sigLen pk pkLen = pure Valid)
                  -> IO VerificationResult

-- PROPERTY: Triple signature requires all three to verify
postulate
tripleSignatureConjunction : (msg : Buffer) -> (msgLen : Nat)
                          -> (d5Sig : Buffer) -> (d5SigLen : Nat)
                          -> (spSig : Buffer) -> (spSigLen : Nat)
                          -> (edSig : Buffer) -> (edSigLen : Nat)
                          -> (d5Pk : Buffer)
                          -> (spPk : Buffer)
                          -> (edPk : Buffer)
                          -> verifyTripleSignature msg msgLen d5Sig d5SigLen spSig spSigLen edSig edSigLen d5Pk spPk edPk = pure Valid
                          -> (verifySignature Dilithium5 msg msgLen d5Sig d5SigLen d5Pk 2592 = pure Valid)
                          -> (verifySignature SPHINCSPlus msg msgLen spSig spSigLen spPk 64 = pure Valid)
                          -> (verifySignature Ed25519 msg msgLen edSig edSigLen edPk 32 = pure Valid)
                          -> IO ()
