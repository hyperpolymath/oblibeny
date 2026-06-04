-- SPDX-License-Identifier: MPL-2.0
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
public export covering
initCrypto : IO (Either String ())
initCrypto = do
  result <- primIO prim__crypto_init
  pure $ if result == 0
         then Right ()
         else Left "Failed to initialize cryptographic libraries"

-- Verify a signature
public export covering
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
public export covering
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
-- FORMAL PROPERTIES
--
-- Idris2 has no axiom keyword.  Rather than fake the soundness-relevant
-- properties with escape-hatch coercions (which the trusted-base policy
-- counts as debt) or with ill-typed stand-ins, we split the formal content
-- honestly into two parts:
--
--   1. What is INTRINSICALLY TRUE about the verification *logic* is PROVED
--      below as real, total theorems (the triple-signature conjunction law).
--
--   2. What depends on the EXTERNAL cryptographic primitives (liboqs /
--      libsodium, reached over FFI) -- completeness and soundness of the
--      underlying verifier -- is captured as an explicit, well-typed trust
--      interface `VerifierTrust`.  These are the documented boundary
--      assumptions about linked code (see docs/proof-debt.md); a verified C
--      binding discharges them by providing an implementation.
-- ============================================================================

-- The pure decision the triple verifier computes: accept iff all three accept.
public export total
combineTriple : VerificationResult -> VerificationResult -> VerificationResult
             -> VerificationResult
combineTriple Valid Valid Valid = Valid
combineTriple _     _     _     = Invalid

-- THEOREM (defence in depth): a Valid triple forces every component Valid.
-- Forging any single one of the three independent schemes cannot yield a valid
-- triple signature.  Proved by exhausting the 2^3 cases; the seven non-(V,V,V)
-- cases are unreachable because `combineTriple` returns `Invalid` there.
public export total
tripleValidImpliesAll : (a, b, c : VerificationResult)
                     -> combineTriple a b c = Valid
                     -> (a = Valid, b = Valid, c = Valid)
tripleValidImpliesAll Valid   Valid   Valid   Refl = (Refl, Refl, Refl)
tripleValidImpliesAll Valid   Valid   Invalid Refl impossible
tripleValidImpliesAll Valid   Invalid Valid   Refl impossible
tripleValidImpliesAll Valid   Invalid Invalid Refl impossible
tripleValidImpliesAll Invalid Valid   Valid   Refl impossible
tripleValidImpliesAll Invalid Valid   Invalid Refl impossible
tripleValidImpliesAll Invalid Invalid Valid   Refl impossible
tripleValidImpliesAll Invalid Invalid Invalid Refl impossible

-- THEOREM (converse): all three Valid yields a Valid triple.
public export total
allValidImpliesTriple : combineTriple Valid Valid Valid = Valid
allValidImpliesTriple = Refl

-- Abstract, externally-determined fact: `sig` is a genuine signature of `msg`
-- under public key `pk` for `scheme`.  Opaque here -- its truth is decided by
-- the mathematics of the scheme and witnessed by the external library.
public export
data Genuine : SignatureScheme -> (msg, sig, pk : List Bits8) -> Type where

-- The trust boundary.  Completeness and soundness of the EXTERNAL verifier are
-- assumptions about linked C code, not Idris theorems; bundling them as a
-- well-typed interface keeps the boundary explicit and machine-checked at the
-- type level, with no escape-hatch coercion and no fake axiom.  The trusted
-- liboqs/libsodium binding is the implementation that discharges them.
public export
interface VerifierTrust where
  ||| Pure model of the external per-scheme verification decision.
  verifyPure   : SignatureScheme -> (msg, sig, pk : List Bits8) -> VerificationResult
  ||| Completeness: a genuine signature is accepted.
  completeness : (s : SignatureScheme) -> (m, sg, pk : List Bits8)
              -> Genuine s m sg pk -> verifyPure s m sg pk = Valid
  ||| Soundness: a non-genuine signature is rejected.
  soundness    : (s : SignatureScheme) -> (m, sg, pk : List Bits8)
              -> Not (Genuine s m sg pk) -> verifyPure s m sg pk = Invalid

-- The triple decision over the trusted per-scheme verifier.
public export total
verifyTriplePure : VerifierTrust
                => (m, d5, sp, ed, d5pk, sppk, edpk : List Bits8)
                -> VerificationResult
verifyTriplePure m d5 sp ed d5pk sppk edpk =
  combineTriple (verifyPure Dilithium5  m d5 d5pk)
                (verifyPure SPHINCSPlus m sp sppk)
                (verifyPure Ed25519     m ed edpk)

-- THEOREM: a valid triple verification requires each scheme to verify -- so a
-- forged or absent signature in any one scheme is caught.  Combines the
-- conjunction law with whatever trusted per-scheme verifier is in scope.
public export total
tripleSignatureConjunction : VerifierTrust
  => (m, d5, sp, ed, d5pk, sppk, edpk : List Bits8)
  -> verifyTriplePure m d5 sp ed d5pk sppk edpk = Valid
  -> ( verifyPure Dilithium5  m d5 d5pk = Valid
     , verifyPure SPHINCSPlus m sp sppk = Valid
     , verifyPure Ed25519     m ed edpk = Valid )
tripleSignatureConjunction m d5 sp ed d5pk sppk edpk prf =
  tripleValidImpliesAll _ _ _ prf
