-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

||| Constrained-form syntax — call-free fragment.  Slice 1 of the language
||| metatheory (programme #1: mechanized soundness of Oblíbený's constrained
||| form).
|||
||| This mirrors the CONSTRAINED subset of Oblíbený's AST (`lib/ast.ml`).  The
||| termination guarantee is already visible in the *types*: there is no
||| `while`/`loop` constructor at all, and `SForRange` carries STATIC bounds —
||| exactly as in `ast.ml`, where `SForRange of string * int64 * int64 * stmt
||| list` makes unbounded iteration *unrepresentable*.  The only thing the
||| checker (`lib/constrained_check.ml`) adds on top is an acyclic call graph;
||| function calls therefore need that measure and are deferred to Slice 2.
|||
||| This slice is the call-free core: assignments, conditionals, the reversible
||| primitives (`incr`/`decr`/`swap`), and bounded for-loops.  The model uses
||| `Integer` for loop bounds rather than `Int64`; termination does not depend
||| on the integer width.
module Lang.Syntax

%default total

public export
Name : Type
Name = String

public export
data Value = VInt Integer | VBool Bool | VUnit

public export
data Op = Add | Sub | Mul | Lt | Le | Eq

public export
data Expr
  = ELitI Integer
  | ELitB Bool
  | EVar  Name
  | EBin  Op Expr Expr
  | EIf   Expr Expr Expr
  | ECall Name (List Expr)                        -- f(e1, ..., en)  (call graph checked: DAG)

public export
data Stmt
  = SAssign   Name Expr                          -- x := e
  | SIncr     Name Expr                           -- x += e   (reversible; inverse is SDecr)
  | SDecr     Name Expr                           -- x -= e   (reversible; inverse is SIncr)
  | SSwap     Name Name                           -- swap(a,b) (self-inverse)
  | SIf       Expr (List Stmt) (List Stmt)
  | SForRange Name Integer Integer (List Stmt)    -- for v in lo..hi   (STATIC bounds, by type)

||| A function: name, parameters, body, and a return expression evaluated over
||| the post-body environment.  (Mirrors `DFunction` in `lib/ast.ml`; the
||| explicit return expr models the `Return` value without modelling the
||| exception.)
public export
record Func where
  constructor MkFunc
  fname  : Name
  params : List Name
  body   : List Stmt
  ret    : Expr

||| A program is a list of function declarations.
public export
Program : Type
Program = List Func
