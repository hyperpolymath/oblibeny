-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

||| Big-step evaluator for the call-free constrained core, and the headline
||| termination result.  Slice 1 of programme #1.
|||
||| THE TERMINATION PROOF IS THE TOTALITY OF `evalStmts`.  Under `%default
||| total`, Idris2 accepts `evalStmts` only if every constrained program halts:
||| the only loop is `SForRange`, evaluated by `evalFor` recursing on a `Nat`
||| iteration count derived from the STATIC bounds, and every other clause
||| recurses structurally on the syntax.  So a green `idris2 --build` of this
||| module *is* the proof that the call-free constrained form is terminating
||| (Turing-incomplete-by-construction), with no totality escape hatches, no
||| axiom stand-ins, and no foreign calls.
|||
||| Honest scope: this is the metatheory of a faithful MODEL of `lib/ast.ml` /
||| `lib/eval.ml`, not the OCaml itself (the model-fidelity gap is named in the
||| programme plan).  Calls + the acyclic-call-graph measure are Slice 2;
||| checker-soundness, reversibility, and resource/trace soundness follow.
module Lang.Eval

import Lang.Syntax

%default total

public export
State : Type
State = List (Name, Value)

-- Coercions (the model is untyped at this slice; typing soundness is later).
asInt : Value -> Integer
asInt (VInt n) = n
asInt _        = 0

asBool : Value -> Bool
asBool (VBool b) = b
asBool (VInt n)  = n /= 0
asBool VUnit     = False

||| Variable read; an unset variable reads as 0 (faithful to a fresh cell).
public export
get : Name -> State -> Value
get _ []              = VInt 0
get x ((y, v) :: rest) = if x == y then v else get x rest

||| Variable write (updates in place, or extends the environment).
public export
set : Name -> Value -> State -> State
set x v []               = [(x, v)]
set x v ((y, w) :: rest) = if x == y then (x, v) :: rest else (y, w) :: set x v rest

evalOp : Op -> Value -> Value -> Value
evalOp Add x y = VInt  (asInt x + asInt y)
evalOp Sub x y = VInt  (asInt x - asInt y)
evalOp Mul x y = VInt  (asInt x * asInt y)
evalOp Lt  x y = VBool (asInt x <  asInt y)
evalOp Le  x y = VBool (asInt x <= asInt y)
evalOp Eq  x y = VBool (asInt x == asInt y)

||| Expression evaluation — structural recursion, manifestly total.
public export
evalExpr : State -> Expr -> Value
evalExpr _ (ELitI n)    = VInt n
evalExpr _ (ELitB b)    = VBool b
evalExpr s (EVar x)     = get x s
evalExpr s (EBin o a b) = evalOp o (evalExpr s a) (evalExpr s b)
evalExpr s (EIf c t e)  = if asBool (evalExpr s c) then evalExpr s t else evalExpr s e

||| Iteration count of `for v in lo..hi` — a `Nat` (0 when hi <= lo).  This is
||| the static measure that bounds the loop; `integerToNat` clamps negatives.
public export
rangeCount : Integer -> Integer -> Nat
rangeCount lo hi = integerToNat (hi - lo)

mutual
  ||| Run a statement sequence.  TOTAL ⇒ every constrained program terminates.
  public export
  evalStmts : State -> List Stmt -> State
  evalStmts s []          = s
  evalStmts s (st :: rest) = evalStmts (evalStmt s st) rest

  public export
  evalStmt : State -> Stmt -> State
  evalStmt s (SAssign x e)         = set x (evalExpr s e) s
  evalStmt s (SIncr x e)           = set x (VInt (asInt (get x s) + asInt (evalExpr s e))) s
  evalStmt s (SDecr x e)           = set x (VInt (asInt (get x s) - asInt (evalExpr s e))) s
  evalStmt s (SSwap a b)           = set a (get b s) (set b (get a s) s)
  evalStmt s (SIf c th el)         = if asBool (evalExpr s c) then evalStmts s th else evalStmts s el
  evalStmt s (SForRange v lo hi b) = evalFor (rangeCount lo hi) lo v b s

  ||| The loop driver: recurse on the `Nat` count (decreasing), running the
  ||| body (a strict sub-term of the `SForRange`) each turn.
  public export
  evalFor : Nat -> Integer -> Name -> List Stmt -> State -> State
  evalFor Z     _ _ _    s = s
  evalFor (S k) i v body s = evalFor k (i + 1) v body (evalStmts (set v (VInt i) s) body)

-- ===========================================================================
-- HEADLINE: termination of the call-free constrained form.
-- ===========================================================================

||| Every statement sequence, from any state, reaches a final state.  This type
||| is inhabited only because `evalStmts` is a TOTAL function — i.e. it is a
||| restatement of "the constrained form halts", pinned as a checked theorem.
public export
terminates : (s : State) -> (p : List Stmt) -> (s' : State ** evalStmts s p = s')
terminates s p = (evalStmts s p ** Refl)

-- ===========================================================================
-- Falsifiable evidence: concrete bounded programs that compute by `Refl`
-- (they only reduce because evaluation terminates).
-- ===========================================================================

||| `x := 0; for i in 0..4 { x += i }` computes 0+1+2+3 = 6 — and only reduces
||| to a value because evaluation terminates.
public export
sumLoop_terminates_to_6 :
  get "x" (evalStmts [] [ SAssign "x" (ELitI 0)
                        , SForRange "i" 0 4 [ SIncr "x" (EVar "i") ] ]) = VInt 6
sumLoop_terminates_to_6 = Refl

||| Reversible primitives: `x += 3; x -= 3` is the identity (foreshadows the
||| Slice-4 reversibility theorem).
public export
incrThenDecr_is_id : get "x" (evalStmts [("x", VInt 5)] [SIncr "x" (ELitI 3), SDecr "x" (ELitI 3)]) = VInt 5
incrThenDecr_is_id = Refl

||| `swap` exchanges two cells.
public export
swap_exchanges : ( get "a" (evalStmt [("a", VInt 1), ("b", VInt 2)] (SSwap "a" "b")) = VInt 2
                 , get "b" (evalStmt [("a", VInt 1), ("b", VInt 2)] (SSwap "a" "b")) = VInt 1 )
swap_exchanges = (Refl, Refl)
