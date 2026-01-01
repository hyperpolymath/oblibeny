(* SPDX-License-Identifier: MIT OR Palimpsest-0.8 *)
(* Copyright (c) 2026 Hyperpolymath *)

(** Abstract Syntax Tree for Oblíbený

    Oblíbený has a dual-form architecture:
    1. Factory Form - Turing-complete metaprogramming
    2. Constrained Form - Turing-incomplete, reversible, accountable

    This AST covers both forms. The constrained form is a strict subset.
*)

open Location

(** Primitive types available in constrained form *)
type prim_type =
  | TI32           (** 32-bit signed integer *)
  | TI64           (** 64-bit signed integer *)
  | TU32           (** 32-bit unsigned integer *)
  | TU64           (** 64-bit unsigned integer *)
  | TBool          (** Boolean *)
  | TUnit          (** Unit type *)
  [@@deriving show, yojson]

(** Type expressions *)
type typ =
  | TPrim of prim_type
  | TArray of typ * int option     (** Array with optional static size *)
  | TRef of typ                    (** Mutable reference *)
  | TFun of typ list * typ         (** Function type *)
  | TStruct of string              (** Named struct type *)
  | TTrace                         (** Accountability trace type *)
  [@@deriving show, yojson]

(** Literals *)
type literal =
  | LInt of int64
  | LBool of bool
  | LUnit
  [@@deriving show, yojson]

(** Binary operators *)
type binop =
  | Add | Sub | Mul | Div | Mod    (** Arithmetic *)
  | Eq | Neq | Lt | Le | Gt | Ge   (** Comparison *)
  | And | Or                        (** Logical *)
  | BitAnd | BitOr | BitXor         (** Bitwise *)
  [@@deriving show, yojson]

(** Unary operators *)
type unop =
  | Neg | Not | BitNot
  [@@deriving show, yojson]

(** ==========================================================================
    CONSTRAINED FORM EXPRESSIONS
    These are the only expressions allowed in the Turing-incomplete form.
    ========================================================================== *)

type expr = {
  expr_desc: expr_desc;
  expr_loc: Location.t;
  mutable expr_type: typ option;
} [@@deriving show, yojson]

and expr_desc =
  | ELiteral of literal
  | EVar of string
  | EBinop of binop * expr * expr
  | EUnop of unop * expr
  | ECall of string * expr list         (** Function call by name - call graph checked *)
  | EIndex of expr * expr               (** Array indexing *)
  | EField of expr * string             (** Struct field access *)
  | EIf of expr * expr * expr           (** Conditional expression *)
  | EBlock of stmt list * expr option   (** Block with optional final expression *)
  | EStruct of string * (string * expr) list  (** Struct construction *)
  [@@deriving show, yojson]

(** ==========================================================================
    CONSTRAINED FORM STATEMENTS
    ========================================================================== *)

and stmt = {
  stmt_desc: stmt_desc;
  stmt_loc: Location.t;
} [@@deriving show, yojson]

and stmt_desc =
  (* Bindings *)
  | SLet of string * typ option * expr         (** Immutable let binding *)
  | SLetMut of string * typ option * expr      (** Mutable let binding - tracked *)

  (* Assignment - only to mutable bindings *)
  | SAssign of string * expr

  (* Control flow - NO while/loop allowed *)
  | SIf of expr * stmt list * stmt list
  | SForRange of string * int64 * int64 * stmt list  (** for i in start..end - STATIC bounds *)

  (* Expression statement *)
  | SExpr of expr
  | SReturn of expr option

  (* Reversibility primitives *)
  | SSwap of string * string                   (** swap(a, b) - self-inverse *)
  | SIncr of string * expr                     (** incr(x, delta) - inverse is decr *)
  | SDecr of string * expr                     (** decr(x, delta) - inverse is incr *)
  | SXorAssign of string * expr                (** x ^= val - self-inverse *)

  (* Accountability trace *)
  | STrace of string * expr list               (** trace(event, args...) *)
  | SCheckpoint of string                      (** checkpoint(label) *)
  | SAssertInvariant of expr * string          (** assert_invariant(cond, msg) *)
  [@@deriving show, yojson]

(** ==========================================================================
    TOP-LEVEL DECLARATIONS
    ========================================================================== *)

type decl = {
  decl_desc: decl_desc;
  decl_loc: Location.t;
} [@@deriving show, yojson]

and decl_desc =
  | DFunction of {
      name: string;
      params: (string * typ) list;
      return_type: typ;
      body: stmt list;
    }
  | DStruct of {
      name: string;
      fields: (string * typ) list;
    }
  | DConst of {
      name: string;
      typ: typ;
      value: expr;
    }
  [@@deriving show, yojson]

(** A complete program in constrained form *)
type program = {
  module_name: string option;
  declarations: decl list;
} [@@deriving show, yojson]

(** ==========================================================================
    HELPER CONSTRUCTORS
    ========================================================================== *)

let mk_expr loc desc = { expr_desc = desc; expr_loc = loc; expr_type = None }
let mk_stmt loc desc = { stmt_desc = desc; stmt_loc = loc }
let mk_decl loc desc = { decl_desc = desc; decl_loc = loc }

(** ==========================================================================
    CONSTRAINED FORM VALIDATION TYPES
    These are used by the constrained_check module to verify Turing-incompleteness.
    ========================================================================== *)

(** Reasons why a program violates constrained form rules *)
type constraint_violation =
  | WhileLoopFound of Location.t
  | LoopKeywordFound of Location.t
  | RecursiveCall of { func: string; callee: string; loc: Location.t }
  | CyclicCallGraph of string list  (** Functions in cycle *)
  | DynamicForBound of { var: string; loc: Location.t }
  [@@deriving show]
