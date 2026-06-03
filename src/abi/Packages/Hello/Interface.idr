-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

module Packages.Hello.Interface

import Data.List
import Data.List.Elem
import Decidable.Equality

-- Every definition in this module is total and proof-complete: no escape-hatch
-- coercions, no axioms, no functional-extensionality assumption.  The two
-- install/uninstall algebraic laws (reversibility and idempotence) are
-- discharged by genuine equational proofs, machine-checked by Idris2.
%default total

-- ============================================================================
-- PROOF OF CONCEPT: Hello Package ABI with Idris2 Proofs
-- ============================================================================

-- System state abstraction.
--
-- File existence is modelled as the *set* of existing paths (a `List String`
-- carrying no duplicates), rather than an opaque `String -> Bool` predicate.
-- This is a deliberate, proof-enabling choice: an arbitrary predicate field
-- makes state equality undecidable and would force a functional-extensionality
-- axiom into the reversibility proof.  A path set is decidable, structurally
-- comparable, and a faithful model of what a package manager actually tracks,
-- so the laws below are proved with no axioms at all.
public export
record SystemState where
  constructor MkSystemState
  installedPackages : List String
  existingFiles     : List String

-- Package representation
public export
record HelloPackage where
  constructor MkHello
  name : String
  version : String
  binPath : String  -- e.g., "/usr/bin/hello"

-- ============================================================================
-- OBSERVATIONS (the surviving, queryable view of a state)
-- ============================================================================

-- These recover the `String -> Bool` query surface of the original design as
-- derived functions over the set representation.
public export
isInstalled : String -> SystemState -> Bool
isInstalled pkg st = elem pkg st.installedPackages

public export
fileExists : String -> SystemState -> Bool
fileExists path st = elem path st.existingFiles

-- ============================================================================
-- SET PRIMITIVES (idempotent insert / membership-guarded remove)
-- ============================================================================

-- Insert `x` only if absent: keeps the carrier duplicate-free (set semantics).
public export
addUnique : DecEq a => a -> List a -> List a
addUnique x xs = case isElem x xs of
  Yes _ => xs
  No  _ => x :: xs

-- Remove every occurrence of `x`.
public export
remove : DecEq a => a -> List a -> List a
remove x [] = []
remove x (y :: ys) = case decEq x y of
  Yes _ => remove x ys
  No  _ => y :: remove x ys

-- ============================================================================
-- INSTALLATION OPERATIONS
-- ============================================================================

-- Install: add the package name and create its binary path (both as set
-- insertions, so installing an already-present package is a no-op on the set).
public export
install : HelloPackage -> SystemState -> SystemState
install pkg st = MkSystemState
  (addUnique pkg.name    st.installedPackages)
  (addUnique pkg.binPath st.existingFiles)

-- Uninstall: remove the package name and delete its binary path.
public export
uninstall : HelloPackage -> SystemState -> SystemState
uninstall pkg st = MkSystemState
  (remove pkg.name    st.installedPackages)
  (remove pkg.binPath st.existingFiles)

-- ============================================================================
-- SUPPORTING LEMMAS
-- ============================================================================

-- Removing an absent element is a no-op.
removeNotElem : DecEq a => (x : a) -> (xs : List a)
             -> Not (Elem x xs) -> remove x xs = xs
removeNotElem x [] _ = Refl
removeNotElem x (y :: ys) notElem with (decEq x y)
  _ | Yes prf = void (notElem (replace {p = \z => Elem x (z :: ys)} prf Here))
  _ | No  _   = rewrite removeNotElem x ys (\e => notElem (There e)) in Refl

-- Removing a fresh element immediately after inserting it restores the carrier.
removeAddUnique : DecEq a => (x : a) -> (xs : List a)
               -> Not (Elem x xs) -> remove x (addUnique x xs) = xs
removeAddUnique x xs notElem with (isElem x xs)
  _ | Yes prf = void (notElem prf)
  _ | No  _ with (decEq x x)
    _ | Yes _   = removeNotElem x xs notElem
    _ | No  ctr = void (ctr Refl)

-- After a unique-insert, the element is present.
elemAddUnique : DecEq a => (x : a) -> (xs : List a) -> Elem x (addUnique x xs)
elemAddUnique x xs with (isElem x xs)
  _ | Yes prf = prf
  _ | No  _   = Here

-- Unique-insert is idempotent.
addUniqueIdem : DecEq a => (x : a) -> (xs : List a)
             -> addUnique x (addUnique x xs) = addUnique x xs
addUniqueIdem x xs with (isElem x (addUnique x xs))
  _ | Yes _   = Refl
  _ | No  ctr = void (ctr (elemAddUnique x xs))

-- Congruence for the state constructor (avoids any reliance on record-eta).
stateEq : {a1, a2, f1, f2 : List String}
       -> a1 = a2 -> f1 = f2 -> MkSystemState a1 f1 = MkSystemState a2 f2
stateEq Refl Refl = Refl

-- ============================================================================
-- FORMAL PROOFS (Reversibility)
-- ============================================================================

-- PROOF: Installation is reversible.
--   uninstall (install pkg state) = state
-- given the package is not already installed and its binary path does not yet
-- exist.  Each component reduces by `removeAddUnique`; `stateEq` reassembles the
-- record.  (Formerly an unproved placeholder; now a genuine equational proof.)
public export
installReversible : (pkg : HelloPackage)
                 -> (state : SystemState)
                 -> Not (Elem pkg.name    state.installedPackages)
                 -> Not (Elem pkg.binPath state.existingFiles)
                 -> uninstall pkg (install pkg state) = state
installReversible pkg (MkSystemState pkgs files) notInstalled fileNotExists =
  stateEq (removeAddUnique pkg.name    pkgs  notInstalled)
          (removeAddUnique pkg.binPath files fileNotExists)

-- PROOF: Double installation is idempotent.
--   install pkg (install pkg state) = install pkg state
-- Unconditionally true under set-insert semantics: re-inserting a present
-- element is a no-op.  Discharged by `addUniqueIdem` on each component.
-- (Formerly an unproved placeholder; now a genuine proof.  Note this law is
-- *false* under naive cons (::) semantics, where the name would be duplicated
-- -- making the set-insert model both correct and necessary.)
public export
doubleInstallIdempotent : (pkg : HelloPackage)
                        -> (state : SystemState)
                        -> install pkg (install pkg state) = install pkg state
doubleInstallIdempotent pkg (MkSystemState pkgs files) =
  stateEq (addUniqueIdem pkg.name    pkgs)
          (addUniqueIdem pkg.binPath files)

-- ============================================================================
-- ABI INTERFACE FOR FFI (C-compatible exports)
-- ============================================================================

-- These declarations will generate C headers that Zig can call.

%foreign "C:hello_install, libhello"
prim__install : String -> String -> PrimIO Int

%foreign "C:hello_uninstall, libhello"
prim__uninstall : String -> PrimIO Int

-- Safe wrapper with error handling
public export covering
installPackageFFI : String -> String -> IO (Either String ())
installPackageFFI pkgPath targetRoot = do
  result <- primIO (prim__install pkgPath targetRoot)
  pure $ if result == 0
         then Right ()
         else Left ("Install failed with code: " ++ show result)

public export covering
uninstallPackageFFI : String -> IO (Either String ())
uninstallPackageFFI targetRoot = do
  result <- primIO (prim__uninstall targetRoot)
  pure $ if result == 0
         then Right ()
         else Left ("Uninstall failed with code: " ++ show result)

-- ============================================================================
-- METADATA
-- ============================================================================

public export
helloPackage : HelloPackage
helloPackage = MkHello
  { name = "hello"
  , version = "1.0.0"
  , binPath = "/usr/bin/hello"
  }
