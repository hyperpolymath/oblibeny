-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

module Packages.Hello.Interface

import Data.List
import Data.String

-- ============================================================================
-- PROOF OF CONCEPT: Hello Package ABI with Idris2 Proofs
-- ============================================================================

-- System state abstraction
public export
record SystemState where
  constructor MkSystemState
  installedPackages : List String
  fileExists : String -> Bool

-- Package representation
public export
record HelloPackage where
  constructor MkHello
  name : String
  version : String
  binPath : String  -- e.g., "/usr/bin/hello"

-- ============================================================================
-- INSTALLATION OPERATIONS
-- ============================================================================

-- Install operation (add package, create file)
public export
install : HelloPackage -> SystemState -> SystemState
install pkg state = MkSystemState
  { installedPackages = pkg.name :: state.installedPackages
  , fileExists = \path => if path == pkg.binPath
                          then True
                          else state.fileExists path
  }

-- Uninstall operation (remove package, delete file)
public export
uninstall : HelloPackage -> SystemState -> SystemState
uninstall pkg state = MkSystemState
  { installedPackages = filter (/= pkg.name) state.installedPackages
  , fileExists = \path => if path == pkg.binPath
                          then False
                          else state.fileExists path
  }

-- ============================================================================
-- FORMAL PROOFS (Reversibility)
-- ============================================================================

-- PROOF: Installation is reversible
-- uninstall (install pkg state) ≡ state
public export
installReversible : (pkg : HelloPackage)
                  -> (state : SystemState)
                  -> Not (elem pkg.name state.installedPackages)
                  -> Not (state.fileExists pkg.binPath)
                  -> uninstall pkg (install pkg state) = state
-- These proofs require function extensionality (comparing lambdas in
-- fileExists) which Idris2 does not provide as a built-in. The proof
-- obligations are genuine and the types are correct. We postulate them
-- with explicit documentation rather than using believe_me.
--
-- To complete these formally, SystemState.fileExists would need to be
-- refactored from (String -> Bool) to a decidable set representation
-- (e.g., SortedSet String) where equality is structurally provable.

postulate
installReversibleProof : (pkg : HelloPackage)
                       -> (state : SystemState)
                       -> Not (elem pkg.name state.installedPackages)
                       -> Not (state.fileExists pkg.binPath)
                       -> uninstall pkg (install pkg state) = state

installReversible pkg state notInstalled fileNotExists =
  installReversibleProof pkg state notInstalled fileNotExists

-- NOTE: Double installation is NOT idempotent with the current `install`
-- implementation. install prepends pkg.name unconditionally, so:
--   install pkg (install pkg state).installedPackages
--     = [pkg.name, pkg.name, ...rest]
-- but:
--   (install pkg state).installedPackages
--     = [pkg.name, ...rest]
--
-- To make idempotency hold, install would need deduplication:
--   installedPackages = nub (pkg.name :: state.installedPackages)
--
-- The false postulate that was here has been removed. If idempotency is
-- needed, refactor `install` to deduplicate, then prove it structurally.

-- ============================================================================
-- ABI INTERFACE FOR FFI (C-compatible exports)
-- ============================================================================

-- These declarations will generate C headers that Zig can call

%foreign "C:hello_install, libhello"
prim__install : String -> String -> PrimIO Int

%foreign "C:hello_uninstall, libhello"
prim__uninstall : String -> PrimIO Int

-- Safe wrapper with error handling
public export
installPackageFFI : String -> String -> IO (Either String ())
installPackageFFI pkgPath targetRoot = do
  result <- primIO (prim__install pkgPath targetRoot)
  pure $ if result == 0
         then Right ()
         else Left ("Install failed with code: " ++ show result)

public export
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
