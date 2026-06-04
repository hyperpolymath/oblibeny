-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

module Packages.Hello.Echo

import Data.List
import Data.List.Elem
import Decidable.Equality
import Packages.Hello.Interface

%default total

-- ============================================================================
-- ECHO-TYPES BRIDGE FOR THE INSTALL/UNINSTALL ABI
--
-- This module formally cashes out the comment carried by Oblíbený's `echo`
-- type (see lib/ast.ml, type `TEcho`):
--
--   "where a computation cannot be reversed, it retains an echo of what was
--    lost."
--
-- `install` is exactly such a computation.  As a state transformer it is, in
-- general, *non-injective*: installing a package that is already present (or
-- whose file already exists) collapses two distinct prior states onto one.
-- Echo types name the proof-relevant residue of that collapse.
--
-- We mirror the echo-types fibre  Echo f y := Σ (x : A), f x ≡ y  (the Agda
-- source of truth, https://github.com/hyperpolymath/echo-types) and prove two
-- complementary facts about `install`:
--
--   * UNDER FRESHNESS the fibre is a singleton: `install` is injective, so the
--     witness is recoverable and there is *no* residue.  This is precisely the
--     reversibility law `installReversible`, read as an injectivity statement.
--
--   * WITHOUT FRESHNESS the fibre is non-trivial: two genuinely distinct source
--     states sit in one fibre, witnessing irreversible loss -- a real echo.
--
-- This is the proof layer (Idris2), where the echo-types *proof theory* is
-- welcome.  It is deliberately separate from Oblíbený's constrained-form
-- `echo[A,B]` (docs/echo-alignment.md), which by design does NOT import the
-- proof theory.
-- ============================================================================

-- Mirror of the echo-types fibre.  `Echo f y` is the proof-relevant set of
-- sources that `f` maps onto the observation `y`, each paired with its proof.
public export
Echo : {a, b : Type} -> (a -> b) -> b -> Type
Echo f y = (x : a ** f x = y)

-- ----------------------------------------------------------------------------
-- NO RESIDUE under freshness: reversibility ⇒ injectivity ⇒ singleton fibre.
-- ----------------------------------------------------------------------------

-- If two fresh source states have the same install image, they are equal.
-- Proof: each is recovered from its image by `uninstall` (installReversible),
-- and the images are assumed equal, so the sources are equal.
public export
installInjectiveFresh :
     (pkg : HelloPackage) -> (s, s' : SystemState)
  -> Not (Elem pkg.name    s.installedPackages)  -> Not (Elem pkg.binPath s.existingFiles)
  -> Not (Elem pkg.name    s'.installedPackages) -> Not (Elem pkg.binPath s'.existingFiles)
  -> install pkg s = install pkg s'
  -> s = s'
installInjectiveFresh pkg s s' n1 n2 n1' n2' eq =
  trans (sym (installReversible pkg s n1 n2))
        (trans (cong (uninstall pkg) eq)
               (installReversible pkg s' n1' n2'))

-- Restated as the fibre fact: under freshness the only witness in the fibre of
-- `install pkg` over `install pkg s` that is *also* fresh is `s` itself.
public export
freshFibreUnique :
     (pkg : HelloPackage) -> (s : SystemState)
  -> Not (Elem pkg.name s.installedPackages) -> Not (Elem pkg.binPath s.existingFiles)
  -> (w : SystemState)
  -> Not (Elem pkg.name w.installedPackages) -> Not (Elem pkg.binPath w.existingFiles)
  -> install pkg w = install pkg s
  -> w = s
freshFibreUnique pkg s ns nf w nw nwf eq =
  installInjectiveFresh pkg w s nw nwf ns nf eq

-- ----------------------------------------------------------------------------
-- A REAL RESIDUE without freshness: a concrete non-injective collapse.
-- ----------------------------------------------------------------------------

-- The empty state and the already-installed state collapse to the same image:
-- installing "hello" onto either yields exactly {pkgs = ["hello"],
-- files = ["/usr/bin/hello"]}.  Decidable string equality makes this hold by
-- computation, so the proof is `Refl`.
public export
installCollapses :
     install (MkHello "hello" "1.0.0" "/usr/bin/hello") (MkSystemState [] [])
   = install (MkHello "hello" "1.0.0" "/usr/bin/hello")
             (MkSystemState ["hello"] ["/usr/bin/hello"])
installCollapses = Refl

-- ...yet the two source states are genuinely distinct (different package sets).
public export
collapseSourcesDistinct :
  Not (the SystemState (MkSystemState [] [])
        = MkSystemState ["hello"] ["/usr/bin/hello"])
collapseSourcesDistinct Refl impossible

-- The two distinct sources both inhabit the SAME fibre of `install hello`,
-- so the fibre (the echo residue) is non-trivial.
public export
residueWitnessFresh :
  Echo (install (MkHello "hello" "1.0.0" "/usr/bin/hello"))
       (install (MkHello "hello" "1.0.0" "/usr/bin/hello") (MkSystemState [] []))
residueWitnessFresh = (MkSystemState [] [] ** Refl)

public export
residueWitnessStale :
  Echo (install (MkHello "hello" "1.0.0" "/usr/bin/hello"))
       (install (MkHello "hello" "1.0.0" "/usr/bin/hello") (MkSystemState [] []))
residueWitnessStale = (MkSystemState ["hello"] ["/usr/bin/hello"] ** sym installCollapses)

-- HEADLINE: the echo residue of `install hello` is non-trivial -- it contains
-- two distinct witnesses.  This is the type-level statement that `install` is
-- irreversible in general, and exactly what an echo retains.
public export
echoResidueNonTrivial :
  Not (fst Packages.Hello.Echo.residueWitnessFresh
        = fst Packages.Hello.Echo.residueWitnessStale)
echoResidueNonTrivial = collapseSourcesDistinct
