-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

||| Resource bounds + accountability trace — Slice 5 (final) of programme #1
||| (soundness of Oblíbený's constrained form).
|||
||| The constrained form guarantees STATIC resource bounds and a complete
||| ACCOUNTABILITY TRACE.  Both follow from the same structural facts as
||| termination: no `while`/`loop`, static `for`-bounds, no recursion.  Here we
||| mechanize the static accountability-trace skeleton `staticTrace` (the
||| sequence of recorded operations, with `for`-loops unrolled by their static
||| iteration count `rangeCount`), and DEFINE the static resource bound as its
||| length — so the resource budget IS the accountability-trace length, by
||| construction.  `cost`/`staticTrace` are total functions, which is exactly
||| the guarantee that every constrained program has a finite, statically-known
||| resource budget and trace.
|||
||| Honest scope: this is the STATIC skeleton.  The runtime trace emitted by
||| `oblibeny --dump-trace` (with actual values; one branch taken per `if`)
||| faithfully realizes this skeleton — that runtime-faithfulness, and the
||| `if`-branch refinement, are the remaining piece (Slice 5b).
module Lang.Resource

import Lang.Syntax
import Lang.Eval
import Data.List

%default total

||| An accountability-trace event: which operation was recorded.
public export
data Event : Type where
  AssignE  : Name -> Event
  IncrE    : Name -> Event
  DecrE    : Name -> Event
  SwapE    : Name -> Name -> Event
  ForIterE : Name -> Event
  IfE      : Event

-- The static accountability trace: the sequence of operations a program
-- records.  `for`-loops are unrolled by their STATIC iteration count; `if`
-- over-approximates with both branches (the skeleton bound).
mutual
  public export
  staticTrace : List Stmt -> List Event
  staticTrace []           = []
  staticTrace (st :: rest) = traceStmt st ++ staticTrace rest

  public export
  traceStmt : Stmt -> List Event
  traceStmt (SAssign x _)            = [AssignE x]
  traceStmt (SIncr x _)              = [IncrE x]
  traceStmt (SDecr x _)              = [DecrE x]
  traceStmt (SSwap a b)              = [SwapE a b]
  traceStmt (SIf _ th el)            = IfE :: (staticTrace th ++ staticTrace el)
  traceStmt (SForRange v lo hi body) = concat (replicate (rangeCount lo hi) (ForIterE v :: staticTrace body))

||| The static resource bound IS the length of the accountability trace.  A
||| total function: every constrained program has a finite, statically-known
||| resource budget (no `while`/`loop`, static `for`-bounds, no recursion).
public export
cost : List Stmt -> Nat
cost = length . staticTrace

-- ===========================================================================
-- HEADLINES
-- ===========================================================================

||| RESOURCE BOUND: every constrained program has a finite static resource
||| budget — the totality of `cost`.
public export
resourceBounded : (stmts : List Stmt) -> (n : Nat ** cost stmts = n)
resourceBounded stmts = (cost stmts ** Refl)

||| The resource budget equals the accountability-trace length, by construction:
||| the bound is exactly what the trace records (resource ↔ accountability).
public export
costIsTraceLength : (stmts : List Stmt) -> cost stmts = length (staticTrace stmts)
costIsTraceLength stmts = Refl

-- Concrete evidence (computes by Refl because the trace/cost are statically
-- determined — the resource analysis terminates).

||| `x := 0; for i in 0..3 { x += i }` has a static budget of 7 operations:
||| 1 assign + 3 iterations of {loop-var, incr}.
public export
sampleCost_is_7 :
  cost [ SAssign "x" (ELitI 0), SForRange "i" 0 3 [ SIncr "x" (EVar "i") ] ] = 7
sampleCost_is_7 = Refl

||| ...and its accountability trace is exactly those 7 recorded operations.
public export
sampleTrace :
  staticTrace [ SAssign "x" (ELitI 0), SForRange "i" 0 3 [ SIncr "x" (EVar "i") ] ]
  = [ AssignE "x"
    , ForIterE "i", IncrE "x"
    , ForIterE "i", IncrE "x"
    , ForIterE "i", IncrE "x" ]
sampleTrace = Refl
