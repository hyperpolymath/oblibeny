(* SPDX-License-Identifier: MIT OR Palimpsest-0.8 *)
(* Copyright (c) 2024 Hyperpolymath *)

(** Abstract Syntax Tree for Oblibeny language *)

open Location

(** Security labels for information flow *)
type security_label =
  | Low    (** Public data *)
  | High   (** Secret data *)
  [@@deriving show, yojson]

(** Primitive types *)
type prim_type =
  | TInt of int option     (** Integer with optional bit width *)
  | TUint of int option    (** Unsigned integer with optional bit width *)
  | TBool
  | TUnit
  | TByte
  [@@deriving show, yojson]

(** Type expressions *)
type typ =
  | TPrim of prim_type
  | TArray of typ * security_label        (** Regular array *)
  | TOArray of typ                         (** Oblivious array (ORAM-backed) *)
  | TRef of typ * security_label          (** Reference with security label *)
  | TFun of typ list * typ                (** Function type *)
  | TStruct of string                     (** Named struct type *)
  | TGeneric of string * typ list         (** Generic type application *)
  | TVar of string                        (** Type variable *)
  [@@deriving show, yojson]

(** Annotated type with security label *)
type annotated_type = {
  typ: typ;
  security: security_label;
  loc: Location.t;
} [@@deriving show, yojson]

(** Binary operators *)
type binop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Le | Gt | Ge
  | And | Or
  | BitAnd | BitOr | BitXor
  | Shl | Shr
  [@@deriving show, yojson]

(** Unary operators *)
type unop =
  | Neg | Not | BitNot
  [@@deriving show, yojson]

(** Literals *)
type literal =
  | LInt of int64
  | LUint of int64
  | LBool of bool
  | LByte of char
  | LUnit
  [@@deriving show, yojson]

(** Pattern for matching *)
type pattern =
  | PWildcard
  | PVar of string
  | PLiteral of literal
  | PTuple of pattern list
  | PStruct of string * (string * pattern) list
  [@@deriving show, yojson]

(** Expressions *)
type expr = {
  expr_desc: expr_desc;
  expr_loc: Location.t;
  mutable expr_type: annotated_type option;  (** Filled during type checking *)
} [@@deriving show, yojson]

and expr_desc =
  | ELiteral of literal
  | EVar of string
  | EBinop of binop * expr * expr
  | EUnop of unop * expr
  | ECall of expr * expr list
  | EIndex of expr * expr                    (** Array indexing *)
  | EOramRead of expr * expr                 (** Explicit ORAM read: oread(arr, idx) *)
  | EField of expr * string                  (** Struct field access *)
  | EIf of expr * expr * expr                (** Conditional expression *)
  | EBlock of stmt list * expr option        (** Block with optional final expression *)
  | ELambda of (string * annotated_type) list * expr  (** Anonymous function *)
  | ETuple of expr list
  | EStruct of string * (string * expr) list (** Struct construction *)
  | ECmov of expr * expr * expr              (** Constant-time conditional move *)
  [@@deriving show, yojson]

(** Statements *)
and stmt = {
  stmt_desc: stmt_desc;
  stmt_loc: Location.t;
} [@@deriving show, yojson]

and stmt_desc =
  | SLet of pattern * annotated_type option * expr  (** Let binding *)
  | SAssign of expr * expr                          (** Assignment *)
  | SOramWrite of expr * expr * expr                (** ORAM write: owrite(arr, idx, val) *)
  | SExpr of expr                                   (** Expression statement *)
  | SIf of expr * stmt list * stmt list             (** If statement *)
  | SWhile of expr * stmt list                      (** While loop *)
  | SFor of string * expr * expr * stmt list        (** For loop: for i in start..end *)
  | SReturn of expr option                          (** Return statement *)
  | SBreak
  | SContinue
  [@@deriving show, yojson]

(** Top-level declarations *)
type decl = {
  decl_desc: decl_desc;
  decl_loc: Location.t;
} [@@deriving show, yojson]

and decl_desc =
  | DFunction of {
      name: string;
      type_params: string list;             (** Generic type parameters *)
      params: (string * annotated_type) list;
      return_type: annotated_type;
      body: stmt list;
      attributes: attribute list;
    }
  | DStruct of {
      name: string;
      type_params: string list;
      fields: (string * annotated_type) list;
      attributes: attribute list;
    }
  | DConst of {
      name: string;
      typ: annotated_type;
      value: expr;
    }
  | DExtern of {
      name: string;
      typ: annotated_type;
      attributes: attribute list;
    }
  | DImport of string list                  (** Import path *)
  [@@deriving show, yojson]

(** Attributes/annotations *)
and attribute =
  | AOblivious           (** Marks function as requiring oblivious execution *)
  | AInline              (** Hint for inlining *)
  | ANoOptimize          (** Disable optimizations (for crypto code) *)
  | AConstantTime        (** Must be constant-time *)
  | APublic              (** Public interface *)
  | ACustom of string * string option  (** Custom attribute with optional value *)
  [@@deriving show, yojson]

(** A complete compilation unit *)
type program = {
  module_name: string option;
  declarations: decl list;
} [@@deriving show, yojson]

(** Helper constructors *)

let mk_expr loc desc = {
  expr_desc = desc;
  expr_loc = loc;
  expr_type = None;
}

let mk_stmt loc desc = {
  stmt_desc = desc;
  stmt_loc = loc;
}

let mk_decl loc desc = {
  decl_desc = desc;
  decl_loc = loc;
}

let mk_atype loc security typ = {
  typ;
  security;
  loc;
}
