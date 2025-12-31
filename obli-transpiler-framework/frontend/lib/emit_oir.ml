(* SPDX-License-Identifier: MIT OR Palimpsest-0.8 *)
(* Copyright (c) 2024 Hyperpolymath *)

(** OIR (Oblivious Intermediate Representation) emission

    This module transforms the typed AST into OIR, which is then
    serialized to JSON/MessagePack for the Rust backend.
*)

open Ast

(** OIR types - these mirror the Rust OIR definitions *)

module Oir = struct
  type security = Low | High [@@deriving yojson]

  type prim_type =
    | I8 | I16 | I32 | I64
    | U8 | U16 | U32 | U64
    | Bool | Unit
    [@@deriving yojson]

  type typ =
    | Prim of prim_type
    | Array of typ * int option        (* element type, optional size *)
    | OArray of typ * int option       (* oblivious array *)
    | Ref of typ
    | Struct of string
    | Fn of typ list * typ
    [@@deriving yojson]

  type annotated_type = {
    typ: typ;
    security: security;
  } [@@deriving yojson]

  type binop =
    | Add | Sub | Mul | Div | Mod
    | Eq | Ne | Lt | Le | Gt | Ge
    | And | Or
    | BitAnd | BitOr | BitXor | Shl | Shr
    [@@deriving yojson]

  type unop = Neg | Not | BitNot [@@deriving yojson]

  type literal =
    | Int of int64
    | Bool of bool
    | Unit
    [@@deriving yojson]

  type var_id = string [@@deriving yojson]

  type expr =
    | Lit of literal
    | Var of var_id
    | Binop of binop * expr * expr
    | Unop of unop * expr
    | Call of string * expr list
    | Index of expr * expr
    | Field of expr * string
    | Cmov of expr * expr * expr           (* condition, true_val, false_val *)
    | OramRead of expr * expr              (* array, index *)
    | Struct of string * (string * expr) list
    [@@deriving yojson]

  type instr =
    | Let of var_id * annotated_type * expr
    | Assign of expr * expr
    | OramWrite of expr * expr * expr      (* array, index, value *)
    | If of expr * block * block
    | While of expr * block
    | For of var_id * expr * expr * block  (* var, start, end, body *)
    | Return of expr option
    | Expr of expr
    [@@deriving yojson]

  and block = instr list [@@deriving yojson]

  type func = {
    name: string;
    params: (var_id * annotated_type) list;
    return_type: annotated_type;
    body: block;
    is_oblivious: bool;
    is_constant_time: bool;
  } [@@deriving yojson]

  type struct_def = {
    name: string;
    fields: (string * annotated_type) list;
  } [@@deriving yojson]

  type extern_func = {
    name: string;
    params: annotated_type list;
    return_type: annotated_type;
  } [@@deriving yojson]

  type module_def = {
    name: string option;
    structs: struct_def list;
    externs: extern_func list;
    functions: func list;
  } [@@deriving yojson]
end

(** Conversion utilities *)

let convert_security = function
  | Low -> Oir.Low
  | High -> Oir.High

let rec convert_prim_type = function
  | TInt None -> Oir.I64
  | TInt (Some 8) -> Oir.I8
  | TInt (Some 16) -> Oir.I16
  | TInt (Some 32) -> Oir.I32
  | TInt (Some 64) -> Oir.I64
  | TInt (Some _) -> Oir.I64  (* Default to I64 for other widths *)
  | TUint None -> Oir.U64
  | TUint (Some 8) -> Oir.U8
  | TUint (Some 16) -> Oir.U16
  | TUint (Some 32) -> Oir.U32
  | TUint (Some 64) -> Oir.U64
  | TUint (Some _) -> Oir.U64
  | TBool -> Oir.Bool
  | TByte -> Oir.U8
  | TUnit -> Oir.Unit

and convert_type = function
  | TPrim p -> Oir.Prim (convert_prim_type p)
  | TArray (elem, _) -> Oir.Array (convert_type elem, None)
  | TOArray elem -> Oir.OArray (convert_type elem, None)
  | TRef (elem, _) -> Oir.Ref (convert_type elem)
  | TStruct name -> Oir.Struct name
  | TFun (params, ret) -> Oir.Fn (List.map convert_type params, convert_type ret)
  | TGeneric (name, _) -> Oir.Struct name  (* Simplified: treat generics as structs *)
  | TVar _ -> Oir.Prim Oir.Unit  (* Type variables shouldn't reach emission *)

let convert_annotated_type at =
  { Oir.typ = convert_type at.typ; security = convert_security at.security }

let convert_binop = function
  | Add -> Oir.Add | Sub -> Oir.Sub | Mul -> Oir.Mul
  | Div -> Oir.Div | Mod -> Oir.Mod
  | Eq -> Oir.Eq | Neq -> Oir.Ne
  | Lt -> Oir.Lt | Le -> Oir.Le | Gt -> Oir.Gt | Ge -> Oir.Ge
  | And -> Oir.And | Or -> Oir.Or
  | BitAnd -> Oir.BitAnd | BitOr -> Oir.BitOr | BitXor -> Oir.BitXor
  | Shl -> Oir.Shl | Shr -> Oir.Shr

let convert_unop = function
  | Neg -> Oir.Neg
  | Not -> Oir.Not
  | BitNot -> Oir.BitNot

let convert_literal = function
  | LInt n -> Oir.Int n
  | LUint n -> Oir.Int n
  | LBool b -> Oir.Bool b
  | LByte c -> Oir.Int (Int64.of_int (Char.code c))
  | LUnit -> Oir.Unit

(** Name generation for temporaries *)
let temp_counter = ref 0
let fresh_temp () =
  let n = !temp_counter in
  incr temp_counter;
  Printf.sprintf "_t%d" n

(** Expression emission *)
let rec emit_expr expr =
  match expr.expr_desc with
  | ELiteral lit -> Oir.Lit (convert_literal lit)
  | EVar name -> Oir.Var name
  | EBinop (op, lhs, rhs) ->
    Oir.Binop (convert_binop op, emit_expr lhs, emit_expr rhs)
  | EUnop (op, operand) ->
    Oir.Unop (convert_unop op, emit_expr operand)
  | ECall (func, args) ->
    let func_name = match func.expr_desc with
      | EVar name -> name
      | _ -> "_anon_fn"  (* Indirect calls need special handling *)
    in
    Oir.Call (func_name, List.map emit_expr args)
  | EIndex (arr, idx) ->
    Oir.Index (emit_expr arr, emit_expr idx)
  | EOramRead (arr, idx) ->
    Oir.OramRead (emit_expr arr, emit_expr idx)
  | EField (obj, field) ->
    Oir.Field (emit_expr obj, field)
  | EIf (cond, then_expr, else_expr) ->
    (* Convert if-expression to cmov *)
    Oir.Cmov (emit_expr cond, emit_expr then_expr, emit_expr else_expr)
  | EBlock (stmts, final) ->
    (* Blocks in expressions need special handling - simplified here *)
    (match final with
     | Some e -> emit_expr e
     | None -> Oir.Lit Oir.Unit)
  | ELambda _ ->
    (* Lambdas should be lifted to top-level *)
    Oir.Lit Oir.Unit  (* TODO: Lambda lifting *)
  | ETuple exprs ->
    (* Tuples should be converted to structs *)
    Oir.Struct ("_tuple", List.mapi (fun i e -> (Printf.sprintf "_%d" i, emit_expr e)) exprs)
  | EStruct (name, fields) ->
    Oir.Struct (name, List.map (fun (n, e) -> (n, emit_expr e)) fields)
  | ECmov (cond, then_val, else_val) ->
    Oir.Cmov (emit_expr cond, emit_expr then_val, emit_expr else_val)

(** Statement emission *)
let rec emit_stmt stmt : Oir.instr list =
  match stmt.stmt_desc with
  | SLet (pattern, type_annot, init) ->
    let var_name = match pattern with
      | PVar name -> name
      | _ -> fresh_temp ()  (* Pattern matching needs expansion *)
    in
    let at = match type_annot with
      | Some t -> convert_annotated_type t
      | None -> match init.expr_type with
        | Some t -> convert_annotated_type t
        | None -> { Oir.typ = Oir.Prim Oir.Unit; security = Oir.Low }
    in
    [Oir.Let (var_name, at, emit_expr init)]

  | SAssign (lhs, rhs) ->
    [Oir.Assign (emit_expr lhs, emit_expr rhs)]

  | SOramWrite (arr, idx, value) ->
    [Oir.OramWrite (emit_expr arr, emit_expr idx, emit_expr value)]

  | SExpr e ->
    [Oir.Expr (emit_expr e)]

  | SIf (cond, then_stmts, else_stmts) ->
    let then_block = List.concat_map emit_stmt then_stmts in
    let else_block = List.concat_map emit_stmt else_stmts in
    [Oir.If (emit_expr cond, then_block, else_block)]

  | SWhile (cond, body) ->
    let body_block = List.concat_map emit_stmt body in
    [Oir.While (emit_expr cond, body_block)]

  | SFor (var, start_expr, end_expr, body) ->
    let body_block = List.concat_map emit_stmt body in
    [Oir.For (var, emit_expr start_expr, emit_expr end_expr, body_block)]

  | SReturn expr_opt ->
    [Oir.Return (Option.map emit_expr expr_opt)]

  | SBreak ->
    [] (* TODO: Need break instruction in OIR *)

  | SContinue ->
    [] (* TODO: Need continue instruction in OIR *)

(** Declaration emission *)
let emit_function decl =
  match decl.decl_desc with
  | DFunction { name; params; return_type; body; attributes; _ } ->
    let is_oblivious = List.exists ((=) AOblivious) attributes in
    let is_constant_time = List.exists ((=) AConstantTime) attributes in
    Some {
      Oir.name;
      params = List.map (fun (n, t) -> (n, convert_annotated_type t)) params;
      return_type = convert_annotated_type return_type;
      body = List.concat_map emit_stmt body;
      is_oblivious;
      is_constant_time;
    }
  | _ -> None

let emit_struct decl =
  match decl.decl_desc with
  | DStruct { name; fields; _ } ->
    Some {
      Oir.name;
      fields = List.map (fun (n, t) -> (n, convert_annotated_type t)) fields;
    }
  | _ -> None

let emit_extern decl =
  match decl.decl_desc with
  | DExtern { name; typ; _ } ->
    (match typ.typ with
     | TFun (params, ret) ->
       Some {
         Oir.name;
         params = List.map (fun t -> { Oir.typ = convert_type t; security = Oir.Low }) params;
         return_type = { Oir.typ = convert_type ret; security = convert_security typ.security };
       }
     | _ -> None)
  | _ -> None

(** Emit complete module *)
let emit_module program =
  temp_counter := 0;
  {
    Oir.name = program.module_name;
    structs = List.filter_map emit_struct program.declarations;
    externs = List.filter_map emit_extern program.declarations;
    functions = List.filter_map emit_function program.declarations;
  }

(** Serialize to JSON *)
let to_json module_def =
  Yojson.Safe.pretty_to_string (Oir.module_def_to_yojson module_def)

(** Write OIR to file *)
let write_oir filename module_def =
  let json = to_json module_def in
  (* Use open_out_bin for consistent behavior across platforms *)
  let oc = open_out_bin filename in
  output_string oc json;
  close_out oc
