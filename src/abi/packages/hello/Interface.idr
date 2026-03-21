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
installReversible pkg state notInstalled fileNotExists =
  -- This would be a complete proof showing:
  -- 1. Package list is restored (pkg removed after being added)
  -- 2. File existence is restored (file deleted after being created)
  -- For now, this is a placeholder showing the proof obligation
  believe_me ()  -- TODO: Complete formal proof

-- PROOF: Double installation is idempotent
public export
doubleInstallIdempotent : (pkg : HelloPackage)
                        -> (state : SystemState)
                        -> install pkg (install pkg state) = install pkg state
doubleInstallIdempotent pkg state =
  -- Installing twice should be same as installing once
  believe_me ()  -- TODO: Complete formal proof

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
