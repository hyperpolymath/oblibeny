-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

||| Checker soundness — Slice 3 of programme #1 (soundness of Oblíbený's
||| constrained form).
|||
||| `lib/constrained_check.ml`'s `validate_program` returns no violations
||| exactly when the call graph is a DAG: no direct self-recursion and no
||| cyclic call graph.  Here we formalize that DAG property as a `Ranking` — a
||| rank assignment under which every directly-called function has STRICTLY
||| smaller rank than its caller — and prove its defining consequences: a ranked
||| program has no direct and no mutual recursion.  A ranking is exactly the
||| topological rank of an acyclic call graph, and it is the measure that
||| Slice 2's `evalLevel` consumes (the `rank` argument).
|||
||| Bridge (honest scope): "`validate_program p = []`" (the OCaml verdict) is
||| precisely "`Ranking p` is inhabited".  We pin that correspondence as the
||| formal acceptance condition; mechanizing the OCaml DFS cycle-detector and
||| proving it computes this predicate is a further step (Slice 3b).
module Lang.Checker

import Lang.Syntax
import Lang.Eval
import Data.List.Elem
import Data.Nat

%default total

||| `g` is directly called by the function named `f` in `prog`: `f` resolves to
||| a function whose call-edge set (`callsOf`, mirroring the OCaml
||| `collect_calls`) contains `g`.
public export
DirectCall : Program -> Name -> Name -> Type
DirectCall prog f g = (fn : Func ** (lookupFunc f prog = Just fn, Elem g (callsOf fn)))

||| A DAG-ranking of `prog`: every directly-called function has strictly smaller
||| rank than its caller.  The formal content of the checker's acyclic-call-graph
||| acceptance, and the measure `evalLevel`'s rank argument consumes.
public export
record Ranking (prog : Program) where
  constructor MkRanking
  rank     : Name -> Nat
  descends : (f, g : Name) -> DirectCall prog f g -> LT (rank g) (rank f)

-- `<` is irreflexive: `n < n` is absurd.
ltIrrefl : {n : Nat} -> Not (LT n n)
ltIrrefl {n = S k} (LTESucc p) = ltIrrefl p

-- `<=` and `<` transitivity (self-contained).
lteTrans : LTE a b -> LTE b c -> LTE a c
lteTrans LTEZero     _            = LTEZero
lteTrans (LTESucc p) (LTESucc q)  = LTESucc (lteTrans p q)

lteSuccR : LTE a b -> LTE a (S b)
lteSuccR LTEZero     = LTEZero
lteSuccR (LTESucc p) = LTESucc (lteSuccR p)

ltTrans : LT a b -> LT b c -> LT a c
ltTrans p (LTESucc q) = lteSuccR (lteTrans p q)

||| A ranked program has NO direct self-recursion: no function calls itself.
||| (Exactly the `RecursiveCall` violation the OCaml checker rejects.)
public export
noSelfRecursion : (r : Ranking prog) -> (f : Name) -> Not (DirectCall prog f f)
noSelfRecursion r f dc = ltIrrefl (descends r f f dc)

||| A ranked program has NO mutual recursion either: no `f`, `g` call each other.
||| (Exactly the `CyclicCallGraph` 2-cycle the OCaml checker rejects.)
public export
noMutualRecursion : (r : Ranking prog) -> (f, g : Name)
                 -> DirectCall prog f g -> DirectCall prog g f -> Void
noMutualRecursion r f g fg gf = ltIrrefl (ltTrans (descends r f g fg) (descends r g f gf))
