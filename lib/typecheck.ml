(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell *)

(** Type Checker for Oblíbený Constrained Form

    Performs type inference and checking for the constrained form.
    Ensures type correctness of expressions, statements, and function calls.
*)

open Ast
open Errors

module Env = Map.Make(String)

type type_env = {
  vars: typ Env.t;
  functions: (typ list * typ) Env.t;  (* (param types, return type) *)
  structs: (string * typ) list Env.t; (* struct name -> fields *)
}

let empty_env = {
  vars = Env.empty;
  functions = Env.empty;
  structs = Env.empty;
}

(** Type error exception *)
exception TypeError of string * Location.t

(** Check if two types are equal *)
let rec types_equal t1 t2 =
  match t1, t2 with
  | TPrim p1, TPrim p2 -> p1 = p2
  | TArray (t1', n1), TArray (t2', n2) -> types_equal t1' t2' && n1 = n2
  | TRef t1', TRef t2' -> types_equal t1' t2'
  | TFun (args1, ret1), TFun (args2, ret2) ->
    List.length args1 = List.length args2 &&
    List.for_all2 types_equal args1 args2 &&
    types_equal ret1 ret2
  | TStruct s1, TStruct s2 -> s1 = s2
  | TTrace, TTrace -> true
  | TEcho (a1, b1), TEcho (a2, b2) -> types_equal a1 a2 && types_equal b1 b2
  | _, _ -> false

(** Format type for error messages *)
let rec format_type = function
  | TPrim TI32 -> "i32"
  | TPrim TI64 -> "i64"
  | TPrim TU32 -> "u32"
  | TPrim TU64 -> "u64"
  | TPrim TBool -> "bool"
  | TPrim TUnit -> "()"
  | TArray (t, None) -> Printf.sprintf "[%s]" (format_type t)
  | TArray (t, Some n) -> Printf.sprintf "[%s; %d]" (format_type t) n
  | TRef t -> Printf.sprintf "&mut %s" (format_type t)
  | TFun (args, ret) ->
    Printf.sprintf "fn(%s) -> %s"
      (String.concat ", " (List.map format_type args))
      (format_type ret)
  | TStruct name -> name
  | TTrace -> "Trace"
  | TEcho (a, b) -> Printf.sprintf "echo[%s, %s]" (format_type a) (format_type b)

(** ==========================================================================
    AFFINE ECHO DISCIPLINE

    An echo is the residue of a non-injective collapse.  The discipline is
    content-sensitive:

        echo[A, B] is affine  iff  A or B is non-copyable.

    So echo[i64, i64] is unrestricted (project it freely), while echo[Cargo, i64],
    echo[i64, Cargo] and echo[array[i64], i64] are affine.  Echo never makes
    copyable data non-copyable by magic; it preserves the copyability discipline
    of its contents.

    For non-copyable contents, affine consumption is essential: otherwise
    echo[A, B] degenerates into an ordinary product A * B you can freely read from
    both sides, an unrestricted way to duplicate or inspect non-copyable residue.
    A non-copyable echo may be referenced at most once -- each reference, in
    particular each [echo_visible] / [echo_witness] projection, consumes it.

    A *copyable* echo (both components copyable, e.g. echo[i64, i64]) is
    unrestricted, mirroring how copyable values may be duplicated freely: if both
    sides are copyable, duplicating the echo smuggles in no extra authority, heap
    state, trace state, or hidden capability -- it is a bounded pair of copyable
    observations, and the "no rhino" boundary stays intact.

    This discipline is scoped to echo bindings only; all other variables keep
    their existing unrestricted usage.
    ========================================================================== *)

(** A type is copyable when it can be duplicated without affine cost. *)
let rec is_copyable = function
  | TPrim _ -> true
  | TEcho (a, b) -> is_copyable a && is_copyable b
  | TArray _ | TStruct _ | TRef _ | TFun _ | TTrace -> false

let is_affine_echo t = match t with
  | TEcho _ -> not (is_copyable t)
  | _ -> false

(* Names of non-copyable echo bindings that have already been used, mapped to
   the location of that use.  Reset per function body. *)
let affine_used : (string, Location.t) Hashtbl.t ref = ref (Hashtbl.create 16)
let affine_reset () = affine_used := Hashtbl.create 16
let affine_snapshot () = Hashtbl.copy !affine_used
let affine_restore s = affine_used := s
let affine_merge (other : (string, Location.t) Hashtbl.t) =
  Hashtbl.iter (fun k v ->
    if not (Hashtbl.mem !affine_used k) then Hashtbl.add !affine_used k v) other

(** Record a use of [name : t]; raise if a non-copyable echo is used twice. *)
let affine_use name t loc =
  if is_affine_echo t then
    match Hashtbl.find_opt !affine_used name with
    | Some _ ->
      raise (TypeError (
        Printf.sprintf
          "echo value '%s' is affine and was already used; a non-copyable echo \
           (%s) may be projected/used at most once" name (format_type t),
        loc))
    | None -> Hashtbl.add !affine_used name loc

(** Infer the type of an expression *)
let rec infer_expr env expr =
  match expr.expr_desc with
  | ELiteral (LInt _) -> TPrim TI64  (* Default integer type *)
  | ELiteral (LBool _) -> TPrim TBool
  | ELiteral LUnit -> TPrim TUnit

  | EVar name ->
    (match Env.find_opt name env.vars with
     | Some t -> affine_use name t expr.expr_loc; t
     | None ->
       raise (TypeError (Printf.sprintf "undefined variable: %s" name, expr.expr_loc)))

  | EBinop (op, e1, e2) ->
    let t1 = infer_expr env e1 in
    let t2 = infer_expr env e2 in
    (match op with
     | Add | Sub | Mul | Div | Mod ->
       if not (types_equal t1 (TPrim TI64) && types_equal t2 (TPrim TI64)) then
         raise (TypeError (
           Printf.sprintf "arithmetic operation requires i64, got %s and %s"
             (format_type t1) (format_type t2),
           expr.expr_loc));
       TPrim TI64
     | Eq | Neq ->
       if not (types_equal t1 t2) then
         raise (TypeError (
           Printf.sprintf "comparison requires same types, got %s and %s"
             (format_type t1) (format_type t2),
           expr.expr_loc));
       TPrim TBool
     | Lt | Le | Gt | Ge ->
       if not (types_equal t1 (TPrim TI64) && types_equal t2 (TPrim TI64)) then
         raise (TypeError (
           Printf.sprintf "comparison requires i64, got %s and %s"
             (format_type t1) (format_type t2),
           expr.expr_loc));
       TPrim TBool
     | And | Or ->
       if not (types_equal t1 (TPrim TBool) && types_equal t2 (TPrim TBool)) then
         raise (TypeError (
           Printf.sprintf "logical operation requires bool, got %s and %s"
             (format_type t1) (format_type t2),
           expr.expr_loc));
       TPrim TBool
     | BitAnd | BitOr | BitXor ->
       if not (types_equal t1 (TPrim TI64) && types_equal t2 (TPrim TI64)) then
         raise (TypeError (
           Printf.sprintf "bitwise operation requires i64, got %s and %s"
             (format_type t1) (format_type t2),
           expr.expr_loc));
       TPrim TI64)

  | EUnop (op, e) ->
    let t = infer_expr env e in
    (match op with
     | Neg ->
       if not (types_equal t (TPrim TI64)) then
         raise (TypeError (
           Printf.sprintf "negation requires i64, got %s" (format_type t),
           expr.expr_loc));
       TPrim TI64
     | Not ->
       if not (types_equal t (TPrim TBool)) then
         raise (TypeError (
           Printf.sprintf "logical not requires bool, got %s" (format_type t),
           expr.expr_loc));
       TPrim TBool
     | BitNot ->
       if not (types_equal t (TPrim TI64)) then
         raise (TypeError (
           Printf.sprintf "bitwise not requires i64, got %s" (format_type t),
           expr.expr_loc));
       TPrim TI64)

  | ECall (name, args) ->
    (match Env.find_opt name env.functions with
     | Some (param_types, ret_type) ->
       let arg_types = List.map (infer_expr env) args in
       if List.length arg_types <> List.length param_types then
         raise (TypeError (
           Printf.sprintf "function %s expects %d arguments, got %d"
             name (List.length param_types) (List.length arg_types),
           expr.expr_loc));
       List.iter2 (fun expected actual ->
         if not (types_equal expected actual) then
           raise (TypeError (
             Printf.sprintf "type mismatch: expected %s, got %s"
               (format_type expected) (format_type actual),
             expr.expr_loc))
       ) param_types arg_types;
       ret_type
     | None ->
       raise (TypeError (Printf.sprintf "undefined function: %s" name, expr.expr_loc)))

  | EIf (cond, then_e, else_e) ->
    let cond_t = infer_expr env cond in
    if not (types_equal cond_t (TPrim TBool)) then
      raise (TypeError (
        Printf.sprintf "if condition must be bool, got %s" (format_type cond_t),
        expr.expr_loc));
    let then_t = infer_expr env then_e in
    let else_t = infer_expr env else_e in
    if not (types_equal then_t else_t) then
      raise (TypeError (
        Printf.sprintf "if branches must have same type: %s vs %s"
          (format_type then_t) (format_type else_t),
        expr.expr_loc));
    then_t

  | EBlock (stmts, final) ->
    let env' = check_stmts env stmts in
    (match final with
     | Some e -> infer_expr env' e
     | None -> TPrim TUnit)

  | EIndex (arr, idx) ->
    let arr_t = infer_expr env arr in
    let idx_t = infer_expr env idx in
    if not (types_equal idx_t (TPrim TI64)) then
      raise (TypeError (
        Printf.sprintf "array index must be i64, got %s" (format_type idx_t),
        expr.expr_loc));
    (match arr_t with
     | TArray (elem_t, _) -> elem_t
     | _ ->
       raise (TypeError (
         Printf.sprintf "index requires array type, got %s" (format_type arr_t),
         expr.expr_loc)))

  | EField (e, field_name) ->
    let t = infer_expr env e in
    (match t with
     | TStruct name ->
       (match Env.find_opt name env.structs with
        | Some fields ->
          (match List.assoc_opt field_name fields with
           | Some field_t -> field_t
           | None ->
             raise (TypeError (
               Printf.sprintf "struct %s has no field %s" name field_name,
               expr.expr_loc)))
        | None ->
          raise (TypeError (
            Printf.sprintf "undefined struct: %s" name,
            expr.expr_loc)))
     | _ ->
       raise (TypeError (
         Printf.sprintf "field access requires struct type, got %s" (format_type t),
         expr.expr_loc)))

  | EStruct (name, field_inits) ->
    (match Env.find_opt name env.structs with
     | Some expected_fields ->
       (* Check all required fields are provided *)
       let provided_names = List.map fst field_inits in
       List.iter (fun (expected_name, _) ->
         if not (List.mem expected_name provided_names) then
           raise (TypeError (
             Printf.sprintf "missing field %s in struct %s" expected_name name,
             expr.expr_loc))
       ) expected_fields;
       (* Check types of provided fields *)
       List.iter (fun (field_name, field_expr) ->
         let expected_t = List.assoc field_name expected_fields in
         let actual_t = infer_expr env field_expr in
         if not (types_equal expected_t actual_t) then
           raise (TypeError (
             Printf.sprintf "field %s: expected %s, got %s"
               field_name (format_type expected_t) (format_type actual_t),
             expr.expr_loc))
       ) field_inits;
       TStruct name
     | None ->
       raise (TypeError (
         Printf.sprintf "undefined struct: %s" name,
         expr.expr_loc)))

  | EEcho (src, base) ->
    (* echo(source, base) : echo[A, B] where A = type of source, B = type of base.
       The residue retains the source witness alongside the surviving base value. *)
    let a = infer_expr env src in
    let b = infer_expr env base in
    TEcho (a, b)

  | EEchoVisible e ->
    (* The visible projection recovers the surviving base value (lossy direction). *)
    (match infer_expr env e with
     | TEcho (_, b) -> b
     | t ->
       raise (TypeError (
         Printf.sprintf "echo_visible requires an echo type, got %s" (format_type t),
         expr.expr_loc)))

  | EEchoWitness e ->
    (* The witness projection recovers the retained proof-relevant source constraint. *)
    (match infer_expr env e with
     | TEcho (a, _) -> a
     | t ->
       raise (TypeError (
         Printf.sprintf "echo_witness requires an echo type, got %s" (format_type t),
         expr.expr_loc)))

(** Check statements and return updated environment *)
and check_stmts env stmts =
  List.fold_left check_stmt env stmts

and check_stmt env stmt =
  match stmt.stmt_desc with
  | SLet (name, type_ann, init_expr) ->
    let init_t = infer_expr env init_expr in
    let declared_t = match type_ann with
      | Some t -> t
      | None -> init_t
    in
    if not (types_equal declared_t init_t) then
      raise (TypeError (
        Printf.sprintf "let binding: expected %s, got %s"
          (format_type declared_t) (format_type init_t),
        stmt.stmt_loc));
    { env with vars = Env.add name declared_t env.vars }

  | SLetMut (name, type_ann, init_expr) ->
    let init_t = infer_expr env init_expr in
    let declared_t = match type_ann with
      | Some t -> t
      | None -> init_t
    in
    if not (types_equal declared_t init_t) then
      raise (TypeError (
        Printf.sprintf "let mut binding: expected %s, got %s"
          (format_type declared_t) (format_type init_t),
        stmt.stmt_loc));
    { env with vars = Env.add name declared_t env.vars }

  | SAssign (name, expr) ->
    let expr_t = infer_expr env expr in
    (match Env.find_opt name env.vars with
     | Some var_t ->
       if not (types_equal var_t expr_t) then
         raise (TypeError (
           Printf.sprintf "assignment: expected %s, got %s"
             (format_type var_t) (format_type expr_t),
           stmt.stmt_loc));
       env
     | None ->
       raise (TypeError (Printf.sprintf "undefined variable: %s" name, stmt.stmt_loc)))

  | SIf (cond, then_stmts, else_stmts) ->
    let cond_t = infer_expr env cond in
    if not (types_equal cond_t (TPrim TBool)) then
      raise (TypeError (
        Printf.sprintf "if condition must be bool, got %s" (format_type cond_t),
        stmt.stmt_loc));
    (* The branches are alternatives: each may consume the same echo, so check
       them from a common entry state and take the union of what was used. *)
    let entry = affine_snapshot () in
    ignore (check_stmts env then_stmts);
    let after_then = affine_snapshot () in
    affine_restore entry;
    ignore (check_stmts env else_stmts);
    affine_merge after_then;
    env

  | SForRange (_, _, _, body_stmts) ->
    (* A non-copyable echo bound before the loop and used in the body would be
       consumed on every iteration, which is unsound. *)
    let before = affine_snapshot () in
    ignore (check_stmts env body_stmts);
    Hashtbl.iter (fun name loc ->
      if not (Hashtbl.mem before name) then
        match Env.find_opt name env.vars with
        | Some t when is_affine_echo t ->
          raise (TypeError (
            Printf.sprintf
              "non-copyable echo '%s' is used inside a loop body; it would be \
               consumed on every iteration" name,
            loc))
        | _ -> ()
    ) !affine_used;
    env

  | SExpr e ->
    ignore (infer_expr env e);
    env

  | SReturn (Some e) ->
    ignore (infer_expr env e);
    env

  | SReturn None -> env

  | SSwap (a, b) ->
    (match Env.find_opt a env.vars, Env.find_opt b env.vars with
     | Some t1, Some t2 ->
       if not (types_equal t1 t2) then
         raise (TypeError (
           Printf.sprintf "swap requires same types, got %s and %s"
             (format_type t1) (format_type t2),
           stmt.stmt_loc));
       env
     | None, _ ->
       raise (TypeError (Printf.sprintf "undefined variable: %s" a, stmt.stmt_loc))
     | _, None ->
       raise (TypeError (Printf.sprintf "undefined variable: %s" b, stmt.stmt_loc)))

  | SIncr (name, delta) | SDecr (name, delta) ->
    let delta_t = infer_expr env delta in
    (match Env.find_opt name env.vars with
     | Some var_t ->
       if not (types_equal var_t (TPrim TI64)) then
         raise (TypeError (
           Printf.sprintf "incr/decr requires i64 variable, got %s" (format_type var_t),
           stmt.stmt_loc));
       if not (types_equal delta_t (TPrim TI64)) then
         raise (TypeError (
           Printf.sprintf "incr/decr delta must be i64, got %s" (format_type delta_t),
           stmt.stmt_loc));
       env
     | None ->
       raise (TypeError (Printf.sprintf "undefined variable: %s" name, stmt.stmt_loc)))

  | SXorAssign (name, expr) ->
    let expr_t = infer_expr env expr in
    (match Env.find_opt name env.vars with
     | Some var_t ->
       if not (types_equal var_t (TPrim TI64)) then
         raise (TypeError (
           Printf.sprintf "xor_assign requires i64 variable, got %s" (format_type var_t),
           stmt.stmt_loc));
       if not (types_equal expr_t (TPrim TI64)) then
         raise (TypeError (
           Printf.sprintf "xor_assign value must be i64, got %s" (format_type expr_t),
           stmt.stmt_loc));
       env
     | None ->
       raise (TypeError (Printf.sprintf "undefined variable: %s" name, stmt.stmt_loc)))

  | STrace (_, args) ->
    List.iter (fun arg -> ignore (infer_expr env arg)) args;
    env

  | SCheckpoint _ -> env

  | SAssertInvariant (cond, _) ->
    let cond_t = infer_expr env cond in
    if not (types_equal cond_t (TPrim TBool)) then
      raise (TypeError (
        Printf.sprintf "assert_invariant condition must be bool, got %s" (format_type cond_t),
        stmt.stmt_loc));
    env

(** Type check a complete program *)
let typecheck_program program =
  let diags = create_diagnostics () in

  try
    (* First pass: collect struct definitions *)
    let env = List.fold_left (fun env decl ->
      match decl.decl_desc with
      | DStruct { name; fields } ->
        { env with structs = Env.add name fields env.structs }
      | _ -> env
    ) empty_env program.declarations in

    (* Second pass: collect function signatures *)
    let env = List.fold_left (fun env decl ->
      match decl.decl_desc with
      | DFunction { name; params; return_type; _ } ->
        let param_types = List.map snd params in
        { env with functions = Env.add name (param_types, return_type) env.functions }
      | _ -> env
    ) env program.declarations in

    (* Third pass: check function bodies *)
    List.iter (fun decl ->
      match decl.decl_desc with
      | DFunction { name = _; params; body; _ } ->
        affine_reset ();
        let fn_env = List.fold_left (fun env (param_name, param_type) ->
          { env with vars = Env.add param_name param_type env.vars }
        ) env params in
        ignore (check_stmts fn_env body)
      | _ -> ()
    ) program.declarations;

    if has_errors diags then Result.Error diags else Result.Ok ()

  with
  | TypeError (msg, loc) ->
    error ~kind:(Errors.TypeError msg) ~loc ~msg diags;
    Result.Error diags
