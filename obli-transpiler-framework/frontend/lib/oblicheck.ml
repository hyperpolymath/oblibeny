(* SPDX-License-Identifier: MIT OR Palimpsest-0.8 *)
(* Copyright (c) 2024 Hyperpolymath *)

(** Obliviousness checking pass for Oblibeny

    This pass verifies that programs do not leak secret information
    through their access patterns. It enforces:

    1. No branching on secret values (use cmov instead)
    2. No array indexing with secret indices (use oarray with oread/owrite)
    3. No secret-dependent loop bounds (use fixed iteration)
    4. Information flow constraints (high cannot flow to low)
*)

open Ast
open Errors

(** Security context tracking *)
type context = {
  in_secret_branch: bool;         (** Inside a branch dependent on secrets *)
  branch_security: security_label; (** Security of current branch condition *)
  loop_depth: int;                 (** Current loop nesting depth *)
  oblivious_function: bool;        (** Inside @oblivious function *)
}

let initial_context = {
  in_secret_branch = false;
  branch_security = Low;
  loop_depth = 0;
  oblivious_function = false;
}

let enter_secret_branch ctx security = {
  ctx with
  in_secret_branch = true;
  branch_security = security_join ctx.branch_security security;
}

let enter_loop ctx = {
  ctx with loop_depth = ctx.loop_depth + 1;
}

let enter_oblivious_function ctx = {
  ctx with oblivious_function = true;
}

(** State for obliviousness checker *)
type state = {
  diags: diagnostics;
  mutable violations: int;
}

let create_state () = {
  diags = create_diagnostics ();
  violations = 0;
}

(** Get security label of expression (requires prior type checking) *)
let get_security expr =
  match expr.expr_type with
  | Some at -> at.security
  | None -> Low  (* Default if not type-checked yet *)

(** Check if type is oblivious array *)
let is_oarray typ =
  match typ with
  | TOArray _ -> true
  | _ -> false

(** Check expression for obliviousness violations *)
let rec check_expr state ctx expr =
  match expr.expr_desc with
  | ELiteral _ | EVar _ -> ()

  | EBinop (_, lhs, rhs) ->
    check_expr state ctx lhs;
    check_expr state ctx rhs

  | EUnop (_, operand) ->
    check_expr state ctx operand

  | ECall (func, args) ->
    check_expr state ctx func;
    List.iter (check_expr state ctx) args

  | EIndex (arr, idx) ->
    check_expr state ctx arr;
    check_expr state ctx idx;
    (* Check for secret indexing into non-oblivious array *)
    let idx_security = get_security idx in
    let arr_type = match arr.expr_type with
      | Some at -> at.typ
      | None -> TPrim TUnit
    in
    if idx_security = High && not (is_oarray arr_type) then begin
      report state.diags (secret_index "array" expr.expr_loc);
      state.violations <- state.violations + 1
    end

  | EOramRead (arr, idx) ->
    check_expr state ctx arr;
    check_expr state ctx idx
    (* ORAM operations are safe by construction *)

  | EField (obj, _) ->
    check_expr state ctx obj

  | EIf (cond, then_expr, else_expr) ->
    check_expr state ctx cond;
    let cond_security = get_security cond in
    if cond_security = High && ctx.oblivious_function then begin
      (* In oblivious function, secret branches are violations *)
      report state.diags (secret_branch cond.expr_loc);
      state.violations <- state.violations + 1
    end;
    let new_ctx = enter_secret_branch ctx cond_security in
    check_expr state new_ctx then_expr;
    check_expr state new_ctx else_expr

  | EBlock (stmts, expr_opt) ->
    List.iter (check_stmt state ctx) stmts;
    Option.iter (check_expr state ctx) expr_opt

  | ELambda (_, body) ->
    check_expr state ctx body

  | ETuple exprs ->
    List.iter (check_expr state ctx) exprs

  | EStruct (_, fields) ->
    List.iter (fun (_, e) -> check_expr state ctx e) fields

  | ECmov (cond, then_val, else_val) ->
    (* cmov is safe for oblivious selection *)
    check_expr state ctx cond;
    check_expr state ctx then_val;
    check_expr state ctx else_val

(** Check statement for obliviousness violations *)
and check_stmt state ctx stmt =
  match stmt.stmt_desc with
  | SLet (_, _, init) ->
    check_expr state ctx init

  | SAssign (lhs, rhs) ->
    check_expr state ctx lhs;
    check_expr state ctx rhs;
    (* Check information flow: cannot assign high to low *)
    let lhs_security = get_security lhs in
    let rhs_security = get_security rhs in
    if rhs_security = High && lhs_security = Low && ctx.oblivious_function then begin
      report state.diags (information_leak ~from_label:"high" ~to_label:"low" stmt.stmt_loc);
      state.violations <- state.violations + 1
    end

  | SOramWrite (arr, idx, value) ->
    check_expr state ctx arr;
    check_expr state ctx idx;
    check_expr state ctx value
    (* ORAM operations are safe *)

  | SExpr e ->
    check_expr state ctx e

  | SIf (cond, then_stmts, else_stmts) ->
    check_expr state ctx cond;
    let cond_security = get_security cond in
    if cond_security = High && ctx.oblivious_function then begin
      report state.diags (secret_branch cond.expr_loc);
      state.violations <- state.violations + 1
    end;
    let new_ctx = enter_secret_branch ctx cond_security in
    List.iter (check_stmt state new_ctx) then_stmts;
    List.iter (check_stmt state new_ctx) else_stmts

  | SWhile (cond, body) ->
    check_expr state ctx cond;
    let cond_security = get_security cond in
    if cond_security = High && ctx.oblivious_function then begin
      report state.diags (secret_loop_bound cond.expr_loc);
      state.violations <- state.violations + 1
    end;
    let new_ctx = enter_loop (enter_secret_branch ctx cond_security) in
    List.iter (check_stmt state new_ctx) body

  | SFor (_, start_expr, end_expr, body) ->
    check_expr state ctx start_expr;
    check_expr state ctx end_expr;
    let start_security = get_security start_expr in
    let end_security = get_security end_expr in
    let bound_security = security_join start_security end_security in
    if bound_security = High && ctx.oblivious_function then begin
      report state.diags (secret_loop_bound start_expr.expr_loc);
      state.violations <- state.violations + 1
    end;
    let new_ctx = enter_loop ctx in
    List.iter (check_stmt state new_ctx) body

  | SReturn expr_opt ->
    Option.iter (check_expr state ctx) expr_opt

  | SBreak | SContinue -> ()

(** Check declaration *)
let check_decl state decl =
  match decl.decl_desc with
  | DFunction { body; attributes; _ } ->
    let is_oblivious = List.exists (fun a -> a = AOblivious || a = AConstantTime) attributes in
    let ctx = if is_oblivious then enter_oblivious_function initial_context else initial_context in
    List.iter (check_stmt state ctx) body

  | DStruct _ -> ()

  | DConst { value; _ } ->
    check_expr state initial_context value

  | DExtern _ | DImport _ -> ()

(** Check a complete program for obliviousness *)
let check_program program =
  let state = create_state () in
  List.iter (check_decl state) program.declarations;
  (state.diags, state.violations)

(** Summary of obliviousness analysis *)
type analysis_result = {
  total_violations: int;
  secret_branches: int;
  secret_indices: int;
  secret_loops: int;
  info_leaks: int;
}

let analyze_violations diags =
  let errs = get_errors diags in
  let count kind = List.length (List.filter (fun d ->
    match d.kind with k when k = kind -> true | _ -> false
  ) errs) in
  {
    total_violations = List.length errs;
    secret_branches = count Secret_dependent_branch;
    secret_indices = List.length (List.filter (fun d ->
      match d.kind with Secret_array_index _ -> true | _ -> false
    ) errs);
    secret_loops = count Secret_loop_bound;
    info_leaks = List.length (List.filter (fun d ->
      match d.kind with Information_leak _ -> true | _ -> false
    ) errs);
  }
