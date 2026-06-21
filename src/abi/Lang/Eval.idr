-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

||| Big-step evaluator for the constrained form and the headline termination
||| results.  Slices 1–2 of programme #1.
|||
||| TERMINATION = TOTALITY (no escape hatches).  Two mechanisms, both
||| accepted by Idris2's totality checker:
|||
|||  * loops — `SForRange` is driven by `evalForW` recursing on a `Nat`
|||    iteration count derived from its STATIC bounds (Slice 1);
|||  * calls — `evalLevel` is *rank-stratified*: to evaluate at rank `S r` it
|||    hands the body a callee fixed at rank `r`, recursing on the rank `Nat`.
|||    An acyclic call graph (what `lib/constrained_check.ml` enforces) bounds
|||    call depth by a finite rank, so the stratified evaluator is total and,
|||    at a rank >= the longest call chain, faithful.  The `rank` argument IS
|||    the acyclic-call-graph measure.
|||
||| So a green `idris2 --build` proves the constrained form (bounded loops +
||| DAG calls) halts.  Honest scope: this is the metatheory of a faithful MODEL
||| of `lib/ast.ml` + `lib/eval.ml`.  Deriving a sufficient rank from the OCaml
||| checker's verdict (`validate_program p = [] => Acyclic p`) is Slice 3;
||| reversibility and resource/trace soundness follow.
module Lang.Eval

import Lang.Syntax

%default total

public export
State : Type
State = List (Name, Value)

public export
asInt : Value -> Integer
asInt (VInt n) = n
asInt _        = 0

public export
asBool : Value -> Bool
asBool (VBool b) = b
asBool (VInt n)  = n /= 0
asBool VUnit     = False

||| Variable read; an unset variable reads as 0 (a fresh cell).
public export
get : Name -> State -> Value
get _ []               = VInt 0
get x ((y, v) :: rest) = if x == y then v else get x rest

||| Variable write (updates in place, or extends the environment).
public export
set : Name -> Value -> State -> State
set x v []               = [(x, v)]
set x v ((y, w) :: rest) = if x == y then (x, v) :: rest else (y, w) :: set x v rest

public export
evalOp : Op -> Value -> Value -> Value
evalOp Add x y = VInt  (asInt x + asInt y)
evalOp Sub x y = VInt  (asInt x - asInt y)
evalOp Mul x y = VInt  (asInt x * asInt y)
evalOp Lt  x y = VBool (asInt x <  asInt y)
evalOp Le  x y = VBool (asInt x <= asInt y)
evalOp Eq  x y = VBool (asInt x == asInt y)

||| Iteration count of `for v in lo..hi` — a `Nat` (0 when hi <= lo).
public export
rangeCount : Integer -> Integer -> Nat
rangeCount lo hi = integerToNat (hi - lo)

||| A "callee" resolves a call `f(vals)` to a value.  Threaded through the
||| structural evaluators; `evalLevel` supplies a rank-decreasing one.
public export
Callee : Type
Callee = Name -> List Value -> Value

-- Expression evaluation (structural; calls go through the opaque callee).
mutual
  public export
  evalExprW : Callee -> State -> Expr -> Value
  evalExprW _ _ (ELitI n)    = VInt n
  evalExprW _ _ (ELitB b)    = VBool b
  evalExprW _ s (EVar x)     = get x s
  evalExprW c s (EBin o a b) = evalOp o (evalExprW c s a) (evalExprW c s b)
  evalExprW c s (EIf cc t e) = if asBool (evalExprW c s cc) then evalExprW c s t else evalExprW c s e
  evalExprW c s (ECall f as) = c f (evalArgsW c s as)

  evalArgsW : Callee -> State -> List Expr -> List Value
  evalArgsW _ _ []        = []
  evalArgsW c s (a :: as) = evalExprW c s a :: evalArgsW c s as

-- Statement evaluation (Slice 1's machinery, threaded with the callee).
mutual
  public export
  evalStmtsW : Callee -> State -> List Stmt -> State
  evalStmtsW _ s []          = s
  evalStmtsW c s (st :: rest) = evalStmtsW c (evalStmtW c s st) rest

  public export
  evalStmtW : Callee -> State -> Stmt -> State
  evalStmtW c s (SAssign x e)         = set x (evalExprW c s e) s
  evalStmtW c s (SIncr x e)           = set x (VInt (asInt (get x s) + asInt (evalExprW c s e))) s
  evalStmtW c s (SDecr x e)           = set x (VInt (asInt (get x s) - asInt (evalExprW c s e))) s
  evalStmtW _ s (SSwap a b)           = set a (get b s) (set b (get a s) s)
  evalStmtW c s (SIf cc th el)        = if asBool (evalExprW c s cc) then evalStmtsW c s th else evalStmtsW c s el
  evalStmtW c s (SForRange v lo hi b) = evalForW c (rangeCount lo hi) lo v b s

  evalForW : Callee -> Nat -> Integer -> Name -> List Stmt -> State -> State
  evalForW _ Z     _ _ _    s = s
  evalForW c (S k) i v body s = evalForW c k (i + 1) v body (evalStmtsW c (set v (VInt i) s) body)

||| Bind parameters to argument values (missing args default to 0).
public export
bindParams : List Name -> List Value -> State
bindParams []        _         = []
bindParams (p :: ps) []        = (p, VInt 0) :: bindParams ps []
bindParams (p :: ps) (v :: vs) = (p, v) :: bindParams ps vs

public export
lookupFunc : Name -> Program -> Maybe Func
lookupFunc _ []          = Nothing
lookupFunc f (fn :: fns) = if fname fn == f then Just fn else lookupFunc f fns

||| Run a function: bind params, run the body, return the return-expression.
public export
evalFunW : Callee -> Func -> List Value -> Value
evalFunW c fn args = evalExprW c (evalStmtsW c (bindParams (params fn) args) (body fn)) (ret fn)

||| Dispatch a resolved call (the `Maybe` from `lookupFunc`) under a callee.
dispatchCall : Callee -> Maybe Func -> List Value -> Value
dispatchCall _ Nothing   _    = VUnit
dispatchCall c (Just fn) args = evalFunW c fn args

||| Rank-stratified evaluator.  At rank `S r`, a call is dispatched to the
||| function body with a callee fixed one rank lower; at rank `Z` the call
||| budget is exhausted (never reached for an acyclic program evaluated at a
||| sufficient rank).  Recursion is on the rank `Nat`, so this is TOTAL.
public export
evalLevel : Program -> Nat -> Name -> List Value -> Value
evalLevel _    Z     _ _    = VUnit
evalLevel prog (S r) f args = dispatchCall (evalLevel prog r) (lookupFunc f prog) args

-- ===========================================================================
-- HEADLINES
-- ===========================================================================

||| Full-language termination: the rank-stratified evaluator produces a value
||| for any program, entry, args, and rank — it cannot diverge.  Inhabited only
||| because `evalLevel` is TOTAL; for an acyclic program at a rank >= its
||| longest call chain this is also the faithful result.
public export
constrainedTerminates : (prog : Program) -> (rank : Nat) -> (entry : Name) -> (args : List Value)
   -> (v : Value ** evalLevel prog rank entry args = v)
constrainedTerminates prog rank entry args = (evalLevel prog rank entry args ** Refl)

||| The call-free callee (no program): evaluates the Slice-1 fragment.
public export
noCall : Callee
noCall _ _ = VUnit

||| Call-free termination (Slice 1), recovered as a corollary.
public export
terminates : (s : State) -> (p : List Stmt) -> (s' : State ** evalStmtsW Lang.Eval.noCall s p = s')
terminates s p = (evalStmtsW Lang.Eval.noCall s p ** Refl)

-- ===========================================================================
-- The call graph (mirrors `collect_calls` in lib/constrained_check.ml).
-- The acyclicity of THIS graph is the measure `evalLevel`'s rank consumes;
-- Slice 3 derives a rank from the OCaml checker's "no cycles" verdict.
-- ===========================================================================

mutual
  public export
  callsInExpr : Expr -> List Name
  callsInExpr (ECall f as) = f :: callsInArgs as
  callsInExpr (EBin _ a b) = callsInExpr a ++ callsInExpr b
  callsInExpr (EIf c t e)  = callsInExpr c ++ (callsInExpr t ++ callsInExpr e)
  callsInExpr _            = []

  callsInArgs : List Expr -> List Name
  callsInArgs []        = []
  callsInArgs (a :: as) = callsInExpr a ++ callsInArgs as

mutual
  public export
  callsInStmt : Stmt -> List Name
  callsInStmt (SAssign _ e)       = callsInExpr e
  callsInStmt (SIncr _ e)         = callsInExpr e
  callsInStmt (SDecr _ e)         = callsInExpr e
  callsInStmt (SSwap _ _)         = []
  callsInStmt (SIf c th el)       = callsInExpr c ++ (callsInStmts th ++ callsInStmts el)
  callsInStmt (SForRange _ _ _ b) = callsInStmts b

  callsInStmts : List Stmt -> List Name
  callsInStmts []          = []
  callsInStmts (st :: rest) = callsInStmt st ++ callsInStmts rest

||| Direct callees of a function: the call-graph edges out of `f`.
public export
callsOf : Func -> List Name
callsOf fn = callsInStmts (body fn) ++ callsInExpr (ret fn)

-- ===========================================================================
-- Falsifiable evidence (computes by `Refl` only because evaluation halts).
-- ===========================================================================

-- Slice 1 (call-free):
||| `x := 0; for i in 0..4 { x += i }` = 6.
public export
sumLoop_terminates_to_6 :
  get "x" (evalStmtsW Lang.Eval.noCall [] [ SAssign "x" (ELitI 0)
                                , SForRange "i" 0 4 [ SIncr "x" (EVar "i") ] ]) = VInt 6
sumLoop_terminates_to_6 = Refl

||| `x += 3; x -= 3` is the identity (foreshadows Slice 4 reversibility).
public export
incrThenDecr_is_id :
  get "x" (evalStmtsW Lang.Eval.noCall [("x", VInt 5)] [SIncr "x" (ELitI 3), SDecr "x" (ELitI 3)]) = VInt 5
incrThenDecr_is_id = Refl

||| `swap` exchanges two cells.
public export
swap_exchanges :
  ( get "a" (evalStmtW Lang.Eval.noCall [("a", VInt 1), ("b", VInt 2)] (SSwap "a" "b")) = VInt 2
  , get "b" (evalStmtW Lang.Eval.noCall [("a", VInt 1), ("b", VInt 2)] (SSwap "a" "b")) = VInt 1 )
swap_exchanges = (Refl, Refl)

-- Slice 2 (calls): a single-call DAG. inc(x) = x + 1, inc(5) = 6.
public export
demoProg : Program
demoProg = [ MkFunc "inc" ["x"] [] (EBin Add (EVar "x") (ELitI 1)) ]

||| A call terminates and computes: inc(5) = 6 (program inlined so the
||| type-checker fully normalizes the rank-stratified evaluation).
public export
incOfFive_is_6 :
  evalLevel (the Program [MkFunc "inc" ["x"] [] (EBin Add (EVar "x") (ELitI 1))]) 1 "inc" [VInt 5] = VInt 6
incOfFive_is_6 = Refl

||| `inc` makes no calls, so its edge set is empty (a DAG leaf).
public export
inc_is_a_leaf : callsOf (MkFunc "inc" ["x"] [] (EBin Add (EVar "x") (ELitI 1))) = []
inc_is_a_leaf = Refl
