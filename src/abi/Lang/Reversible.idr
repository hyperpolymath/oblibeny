-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

||| Reversibility — Slice 4 of programme #1 (soundness of Oblíbený's constrained
||| form).
|||
||| Oblíbený's constrained form has REVERSIBLE primitives — `incr`/`decr` are
||| mutual inverses, `swap` is self-inverse — and an IRREVERSIBLE one, `assign`,
||| which overwrites (and so loses) the prior value.  That loss is exactly what
||| the `echo[A,B]` type names: the proof-layer counterpart is the ABI's
||| `Packages.Hello.Echo` (`install` is non-injective; its fibre is the echo
||| residue).  Here we lift the reversibility story to the language's statement
||| primitives.
|||
||| This module proves the **syntactic inverse is involutive** on the reversible
||| fragment (`inverse (inverse st) = st`) as a general theorem, gives concrete
||| Refl-checked **semantic** reversibility (running a statement then its inverse
||| restores the state), and pins `assign` as the irreversible boundary where an
||| echo is required.
|||
||| Honest scope: fully *general* (universally-quantified-over-variable-names)
||| semantic reversibility needs the store laws `get x (set x v s) = v` etc.,
||| which are not provable in-language for primitive `String` names (`x == x`
||| does not reduce for a variable `x`); they hold for any `DecEq`-reducing name
||| type / lawful store.  That generalization is Slice 4b; the concrete
||| witnesses below are real evidence at the value level.
module Lang.Reversible

import Lang.Syntax
import Lang.Eval

%default total

||| The reversible statement primitives (the ones with a structural inverse).
public export
data Reversible : Stmt -> Type where
  RevIncr : Reversible (SIncr x e)
  RevDecr : Reversible (SDecr x e)
  RevSwap : Reversible (SSwap a b)

||| The syntactic inverse: `incr <-> decr`, `swap` self-inverse.  On a
||| non-reversible statement it is the identity (never consumed: callers guard
||| with `Reversible`).
public export
inverse : Stmt -> Stmt
inverse (SIncr x e) = SDecr x e
inverse (SDecr x e) = SIncr x e
inverse (SSwap a b) = SSwap a b
inverse st          = st

||| GENERAL theorem: on the reversible fragment, `inverse` is an involution —
||| inverting twice returns the original statement.  (Pure syntax; no store or
||| arithmetic reasoning, so it holds for every variable and expression.)
public export
inverseInvolutive : (st : Stmt) -> Reversible st -> inverse (inverse st) = st
inverseInvolutive (SIncr x e) RevIncr = Refl
inverseInvolutive (SDecr x e) RevDecr = Refl
inverseInvolutive (SSwap a b) RevSwap = Refl

-- ===========================================================================
-- Concrete semantic reversibility: run a statement, then its inverse, and the
-- observable state is restored (computes by Refl only because it really does).
-- ===========================================================================

-- (a) the syntactic inverse, and (b) that statement-then-inverse restores the
-- state, proved separately so neither term embeds `inverse` inside the
-- evaluator (which blocks the conversion checker's reduction).

||| The inverse of `incr x 3` is `decr x 3`.
public export
inverse_of_incr : inverse (SIncr "x" (ELitI 3)) = SDecr "x" (ELitI 3)
inverse_of_incr = Refl

||| Running `incr x 3` then `decr x 3` restores x's value.
public export
incr_then_decr_restores :
  get "x" (evalStmtsW Lang.Eval.noCall [("x", VInt 5)]
            [ SIncr "x" (ELitI 3), SDecr "x" (ELitI 3) ]) = VInt 5
incr_then_decr_restores = Refl

||| The inverse of `swap a b` is itself.
public export
inverse_of_swap : inverse (SSwap "a" "b") = SSwap "a" "b"
inverse_of_swap = Refl

||| Running `swap a b` twice restores the whole state (self-inverse).
public export
swap_twice_restores :
  evalStmtsW Lang.Eval.noCall [("a", VInt 1), ("b", VInt 2)]
    [ SSwap "a" "b", SSwap "a" "b" ]
  = [("a", VInt 1), ("b", VInt 2)]
swap_twice_restores = Refl

-- ===========================================================================
-- The irreversible boundary: `assign` overwrites and is NOT reversible — this
-- is where an `echo[A,B]` residue is required (cf. Packages.Hello.Echo, where
-- `install` is non-injective and its fibre is the retained echo).
-- ===========================================================================

||| `assign` is outside the reversible fragment: there is no `Reversible`
||| witness for it.  Overwriting `x := 7` cannot be undone without the prior
||| value — the loss an `echo` would retain.
public export
assignNotReversible : Not (Reversible (SAssign "x" (ELitI 7)))
assignNotReversible RevIncr impossible
