(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell *)

(** Reference Evaluator for Oblíbený Constrained Form

    This is the authoritative reference implementation for the constrained form
    semantics. It is intentionally simple and not optimized.

    Key properties:
    - All operations produce accountability trace entries
    - Reversible operations record sufficient information for reversal
    - Bounded iteration (for-range with static bounds)
    - No recursion (guaranteed by constrained_check)
*)

open Ast

module Env = Map.Make(String)

type value =
  | VInt of int64
  | VBool of bool
  | VUnit
  | VArray of value array
  | VStruct of (string * value) list
  [@@deriving show]

type env = {
  vars: value Env.t;
  mutable_vars: (string, value ref) Hashtbl.t;
  functions: (string list * stmt list) Env.t;
}

type state = {
  mutable env: env;
  trace: Trace.t;
}

let create_env () = {
  vars = Env.empty;
  mutable_vars = Hashtbl.create 16;
  functions = Env.empty;
}

let create_state () = {
  env = create_env ();
  trace = Trace.create ();
}

exception EvalError of string
exception Return of value

let value_to_trace_value = function
  | VInt i -> Trace.VInt i
  | VBool b -> Trace.VBool b
  | VUnit -> Trace.VUnit
  | _ -> Trace.VRef "<complex>"

(** Evaluate a binary operation *)
let eval_binop op v1 v2 =
  match op, v1, v2 with
  | Add, VInt a, VInt b -> VInt (Int64.add a b)
  | Sub, VInt a, VInt b -> VInt (Int64.sub a b)
  | Mul, VInt a, VInt b -> VInt (Int64.mul a b)
  | Div, VInt a, VInt b -> VInt (Int64.div a b)
  | Mod, VInt a, VInt b -> VInt (Int64.rem a b)
  | Eq, VInt a, VInt b -> VBool (a = b)
  | Eq, VBool a, VBool b -> VBool (a = b)
  | Neq, VInt a, VInt b -> VBool (a <> b)
  | Neq, VBool a, VBool b -> VBool (a <> b)
  | Lt, VInt a, VInt b -> VBool (a < b)
  | Le, VInt a, VInt b -> VBool (a <= b)
  | Gt, VInt a, VInt b -> VBool (a > b)
  | Ge, VInt a, VInt b -> VBool (a >= b)
  | And, VBool a, VBool b -> VBool (a && b)
  | Or, VBool a, VBool b -> VBool (a || b)
  | BitAnd, VInt a, VInt b -> VInt (Int64.logand a b)
  | BitOr, VInt a, VInt b -> VInt (Int64.logor a b)
  | BitXor, VInt a, VInt b -> VInt (Int64.logxor a b)
  | _ -> raise (EvalError "type error in binop")

(** Evaluate a unary operation *)
let eval_unop op v =
  match op, v with
  | Neg, VInt a -> VInt (Int64.neg a)
  | Not, VBool a -> VBool (not a)
  | BitNot, VInt a -> VInt (Int64.lognot a)
  | _ -> raise (EvalError "type error in unop")

(** Evaluate expression *)
let rec eval_expr state env expr =
  match expr.expr_desc with
  | ELiteral (LInt i) -> VInt i
  | ELiteral (LBool b) -> VBool b
  | ELiteral LUnit -> VUnit

  | EVar name ->
    (* Check mutable vars first, then immutable *)
    (match Hashtbl.find_opt env.mutable_vars name with
     | Some r -> !r
     | None ->
       match Env.find_opt name env.vars with
       | Some v -> v
       | None -> raise (EvalError (Printf.sprintf "undefined variable: %s" name)))

  | EBinop (op, e1, e2) ->
    let v1 = eval_expr state env e1 in
    let v2 = eval_expr state env e2 in
    eval_binop op v1 v2

  | EUnop (op, e) ->
    let v = eval_expr state env e in
    eval_unop op v

  | ECall (name, args) ->
    let arg_values = List.map (eval_expr state env) args in
    (match Env.find_opt name env.functions with
     | Some (params, body) ->
       let call_env = { env with vars = Env.empty; mutable_vars = Hashtbl.create 8 } in
       List.iter2 (fun p v ->
         Hashtbl.add call_env.mutable_vars p (ref v)
       ) params arg_values;
       Trace.append state.trace ~op:("call:" ^ name)
         ~inputs:(List.map value_to_trace_value arg_values)
         ~outputs:[];
       (try
         List.iter (eval_stmt state call_env) body;
         VUnit
       with Return v -> v)
     | None -> raise (EvalError (Printf.sprintf "undefined function: %s" name)))

  | EIf (cond, then_e, else_e) ->
    let cond_v = eval_expr state env cond in
    (match cond_v with
     | VBool true -> eval_expr state env then_e
     | VBool false -> eval_expr state env else_e
     | _ -> raise (EvalError "if condition must be bool"))

  | EBlock (stmts, final) ->
    List.iter (eval_stmt state env) stmts;
    (match final with Some e -> eval_expr state env e | None -> VUnit)

  | EIndex (arr, idx) ->
    let arr_v = eval_expr state env arr in
    let idx_v = eval_expr state env idx in
    (match arr_v, idx_v with
     | VArray a, VInt i -> a.(Int64.to_int i)
     | _ -> raise (EvalError "invalid array access"))

  | EField (obj, field) ->
    let obj_v = eval_expr state env obj in
    (match obj_v with
     | VStruct fields ->
       (match List.assoc_opt field fields with
        | Some v -> v
        | None -> raise (EvalError (Printf.sprintf "unknown field: %s" field)))
     | _ -> raise (EvalError "field access on non-struct"))

  | EStruct (_, fields) ->
    let field_values = List.map (fun (n, e) -> (n, eval_expr state env e)) fields in
    VStruct field_values

and eval_stmt state env stmt =
  match stmt.stmt_desc with
  | SLet (name, _, init) ->
    let v = eval_expr state env init in
    Hashtbl.add env.mutable_vars name (ref v)

  | SLetMut (name, _, init) ->
    let v = eval_expr state env init in
    Hashtbl.add env.mutable_vars name (ref v);
    Trace.append state.trace ~op:"let_mut"
      ~inputs:[Trace.VRef name; value_to_trace_value v]
      ~outputs:[]

  | SAssign (name, e) ->
    let v = eval_expr state env e in
    (match Hashtbl.find_opt env.mutable_vars name with
     | Some r ->
       let old_v = !r in
       r := v;
       Trace.append state.trace ~op:"assign"
         ~inputs:[Trace.VRef name; value_to_trace_value old_v]
         ~outputs:[value_to_trace_value v]
     | None -> raise (EvalError (Printf.sprintf "cannot assign to immutable: %s" name)))

  | SIf (cond, then_stmts, else_stmts) ->
    let cond_v = eval_expr state env cond in
    (match cond_v with
     | VBool true -> List.iter (eval_stmt state env) then_stmts
     | VBool false -> List.iter (eval_stmt state env) else_stmts
     | _ -> raise (EvalError "if condition must be bool"))

  | SForRange (var, start_val, end_val, body) ->
    Trace.append state.trace ~op:"for_range_start"
      ~inputs:[Trace.VRef var; Trace.VInt start_val; Trace.VInt end_val]
      ~outputs:[];
    for i = Int64.to_int start_val to Int64.to_int end_val - 1 do
      Hashtbl.replace env.mutable_vars var (ref (VInt (Int64.of_int i)));
      List.iter (eval_stmt state env) body
    done;
    Trace.append state.trace ~op:"for_range_end"
      ~inputs:[Trace.VRef var]
      ~outputs:[]

  | SExpr e ->
    let _ = eval_expr state env e in ()

  | SReturn e ->
    let v = match e with Some e -> eval_expr state env e | None -> VUnit in
    raise (Return v)

  (* Reversibility primitives *)
  | SSwap (a, b) ->
    let va = Hashtbl.find env.mutable_vars a in
    let vb = Hashtbl.find env.mutable_vars b in
    let tmp = !va in
    va := !vb;
    vb := tmp;
    Trace.trace_swap state.trace a b

  | SIncr (var, delta) ->
    let delta_v = eval_expr state env delta in
    (match Hashtbl.find_opt env.mutable_vars var, delta_v with
     | Some r, VInt d ->
       (match !r with
        | VInt v -> r := VInt (Int64.add v d)
        | _ -> raise (EvalError "incr requires int"));
       Trace.trace_incr state.trace var d
     | _ -> raise (EvalError "incr error"))

  | SDecr (var, delta) ->
    let delta_v = eval_expr state env delta in
    (match Hashtbl.find_opt env.mutable_vars var, delta_v with
     | Some r, VInt d ->
       (match !r with
        | VInt v -> r := VInt (Int64.sub v d)
        | _ -> raise (EvalError "decr requires int"));
       Trace.trace_decr state.trace var d
     | _ -> raise (EvalError "decr error"))

  | SXorAssign (var, value) ->
    let value_v = eval_expr state env value in
    (match Hashtbl.find_opt env.mutable_vars var, value_v with
     | Some r, VInt v ->
       (match !r with
        | VInt old -> r := VInt (Int64.logxor old v)
        | _ -> raise (EvalError "xor_assign requires int"));
       Trace.trace_xor_assign state.trace var v
     | _ -> raise (EvalError "xor_assign error"))

  (* Trace operations *)
  | STrace (event, args) ->
    let arg_values = List.map (eval_expr state env) args in
    Trace.trace_custom state.trace event (List.map value_to_trace_value arg_values)

  | SCheckpoint label ->
    Trace.checkpoint state.trace label

  | SAssertInvariant (cond, msg) ->
    let cond_v = eval_expr state env cond in
    (match cond_v with
     | VBool true ->
       Trace.append state.trace ~op:"assert_pass"
         ~inputs:[Trace.VRef msg]
         ~outputs:[]
     | VBool false ->
       Trace.append state.trace ~op:"assert_fail"
         ~inputs:[Trace.VRef msg]
         ~outputs:[];
       raise (EvalError (Printf.sprintf "invariant violated: %s" msg))
     | _ -> raise (EvalError "assert_invariant condition must be bool"))

(** Evaluate a complete program *)
let eval_program program =
  let state = create_state () in

  (* Register all functions *)
  List.iter (fun decl ->
    match decl.decl_desc with
    | DFunction { name; params; body; _ } ->
      let param_names = List.map fst params in
      state.env <- { state.env with
        functions = Env.add name (param_names, body) state.env.functions
      }
    | _ -> ()
  ) program.declarations;

  (* Look for and run main *)
  (match Env.find_opt "main" state.env.functions with
   | Some (_, body) ->
     (try
       List.iter (eval_stmt state state.env) body;
       (VUnit, state.trace)
     with Return v -> (v, state.trace))
   | None -> raise (EvalError "no main function"));
