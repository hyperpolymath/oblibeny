(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell *)

(** Static Analyzer for Oblíbený Constrained Form

    Comprehensive static analysis that enforces Turing-incompleteness
    and provides resource bounds guarantees.

    Analysis passes:
    1. Constrained form validation (no while/loop/recursion)
    2. Static loop bounds checking
    3. Resource usage estimation (max iterations, call depth, memory)
    4. Reversibility pairing analysis (incr/decr, push/pop must balance)
    5. Accountability trace coverage
*)

open Ast

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

(** Analysis result with detailed metrics *)
type analysis_result = {
  is_valid: bool;
  violations: constraint_violation list;
  max_loop_iterations: int64;
  max_call_depth: int;
  estimated_memory: int;  (* In bytes *)
  reversibility_warnings: string list;
  trace_coverage: float;  (* 0.0 to 1.0 *)
}

(** Check if expression is a compile-time constant *)
let rec is_constant_expr = function
  | { expr_desc = ELiteral _; _ } -> true
  | { expr_desc = EVar _name; _ } ->
    (* Variables are constant if they refer to DConst declarations *)
    false  (* TODO: Need symbol table to check this *)
  | { expr_desc = EBinop (_op, e1, e2); _ } ->
    is_constant_expr e1 && is_constant_expr e2
  | { expr_desc = EUnop (_, e); _ } -> is_constant_expr e
  | _ -> false

(** Extract integer value from constant expression *)
let rec eval_constant_expr = function
  | { expr_desc = ELiteral (LInt i); _ } -> Some i
  | { expr_desc = EBinop (Add, e1, e2); _ } ->
    (match (eval_constant_expr e1, eval_constant_expr e2) with
     | (Some v1, Some v2) -> Some (Int64.add v1 v2)
     | _ -> None)
  | { expr_desc = EBinop (Sub, e1, e2); _ } ->
    (match (eval_constant_expr e1, eval_constant_expr e2) with
     | (Some v1, Some v2) -> Some (Int64.sub v1 v2)
     | _ -> None)
  | { expr_desc = EBinop (Mul, e1, e2); _ } ->
    (match (eval_constant_expr e1, eval_constant_expr e2) with
     | (Some v1, Some v2) -> Some (Int64.mul v1 v2)
     | _ -> None)
  | { expr_desc = EUnop (Neg, e); _ } ->
    (match eval_constant_expr e with
     | Some v -> Some (Int64.neg v)
     | None -> None)
  | _ -> None

(** Calculate maximum loop iterations in a program *)
let rec max_iterations_stmt stmt =
  match stmt.stmt_desc with
  | SForRange (_, start, end_, body) ->
    let iterations = Int64.sub end_ start in
    let body_iters = List.fold_left (fun acc s ->
      Int64.add acc (max_iterations_stmt s)
    ) 0L body in
    Int64.mul iterations (Int64.add 1L body_iters)
  | SIf (_, then_stmts, else_stmts) ->
    let then_max = List.fold_left (fun acc s -> Int64.add acc (max_iterations_stmt s)) 0L then_stmts in
    let else_max = List.fold_left (fun acc s -> Int64.add acc (max_iterations_stmt s)) 0L else_stmts in
    max then_max else_max
  | SLet (_, _, _) | SLetMut (_, _, _) | SAssign (_, _) | SExpr _ -> 0L
  | SReturn _ | SSwap _ | SCheckpoint _ -> 0L
  | SIncr _ | SDecr _ | SXorAssign _ | STrace _ | SAssertInvariant _ -> 0L

let max_iterations_program program =
  List.fold_left (fun acc decl ->
    match decl.decl_desc with
    | DFunction { body; _ } ->
      let func_iters = List.fold_left (fun acc s -> Int64.add acc (max_iterations_stmt s)) 0L body in
      max acc func_iters
    | _ -> acc
  ) 0L program.declarations

(** Calculate maximum call depth *)
let rec call_depth_expr expr funcs =
  match expr.expr_desc with
  | ECall (name, args) ->
    (* Depth is 1 + depth of called function + max depth of arguments *)
    let func_depth = try StringMap.find name funcs with Not_found -> 0 in
    let arg_depth = List.fold_left (fun acc e -> max acc (call_depth_expr e funcs)) 0 args in
    1 + func_depth + arg_depth
  | EBinop (_, e1, e2) -> max (call_depth_expr e1 funcs) (call_depth_expr e2 funcs)
  | EUnop (_, e) -> call_depth_expr e funcs
  | EIf (c, t, e) -> max (call_depth_expr c funcs) (max (call_depth_expr t funcs) (call_depth_expr e funcs))
  | EBlock (stmts, final) ->
    let stmt_depth = List.fold_left (fun acc s -> max acc (call_depth_stmt s funcs)) 0 stmts in
    let final_depth = match final with Some e -> call_depth_expr e funcs | None -> 0 in
    max stmt_depth final_depth
  | EIndex (arr, idx) -> max (call_depth_expr arr funcs) (call_depth_expr idx funcs)
  | EField (e, _) -> call_depth_expr e funcs
  | EStruct (_, fields) -> List.fold_left (fun acc (_, e) -> max acc (call_depth_expr e funcs)) 0 fields
  | ELiteral _ | EVar _ -> 0

and call_depth_stmt stmt funcs =
  match stmt.stmt_desc with
  | SLet (_, _, e) | SLetMut (_, _, e) | SAssign (_, e) | SExpr e -> call_depth_expr e funcs
  | SIf (c, t, el) ->
    let cond_depth = call_depth_expr c funcs in
    let then_depth = List.fold_left (fun acc s -> max acc (call_depth_stmt s funcs)) 0 t in
    let else_depth = List.fold_left (fun acc s -> max acc (call_depth_stmt s funcs)) 0 el in
    max cond_depth (max then_depth else_depth)
  | SForRange (_, _, _, body) ->
    List.fold_left (fun acc s -> max acc (call_depth_stmt s funcs)) 0 body
  | SReturn (Some e) -> call_depth_expr e funcs
  | SReturn None | SSwap _ | SCheckpoint _ -> 0
  | SIncr (_, e) | SDecr (_, e) | SXorAssign (_, e) -> call_depth_expr e funcs
  | STrace (_, args) -> List.fold_left (fun acc e -> max acc (call_depth_expr e funcs)) 0 args
  | SAssertInvariant (e, _) -> call_depth_expr e funcs

let calculate_call_depth program =
  (* Build map of function name -> max call depth *)
  let build_depth_map () =
    let depths = ref StringMap.empty in
    let changed = ref true in
    while !changed do
      changed := false;
      List.iter (fun decl ->
        match decl.decl_desc with
        | DFunction { name; body; _ } ->
          let old_depth = try StringMap.find name !depths with Not_found -> 0 in
          let new_depth = List.fold_left (fun acc s -> max acc (call_depth_stmt s !depths)) 0 body in
          if new_depth > old_depth then begin
            depths := StringMap.add name new_depth !depths;
            changed := true
          end
        | _ -> ()
      ) program.declarations
    done;
    !depths
  in
  let depths = build_depth_map () in
  StringMap.fold (fun _ d acc -> max d acc) depths 0

(** Analyze reversibility pairing (incr/decr, push/pop must balance) *)
type balance_state = {
  mutable incr_count: int;
  mutable decr_count: int;
  mutable xor_count: int;  (* Self-inverse operations *)
}

let analyze_reversibility_stmt stmt =
  let state = { incr_count = 0; decr_count = 0; xor_count = 0 } in
  let warnings = ref [] in

  let rec analyze_stmt s =
    match s.stmt_desc with
    | SIncr _ -> state.incr_count <- state.incr_count + 1
    | SDecr _ -> state.decr_count <- state.decr_count + 1
    | SXorAssign _ -> state.xor_count <- state.xor_count + 1
    | SIf (_, then_stmts, else_stmts) ->
      List.iter analyze_stmt then_stmts;
      List.iter analyze_stmt else_stmts
    | SForRange (_, _, _, body) ->
      List.iter analyze_stmt body
    | _ -> ()
  in
  analyze_stmt stmt;

  (* Check if incr/decr are balanced *)
  if state.incr_count <> state.decr_count then
    warnings := Printf.sprintf "Unbalanced reversible operations: %d incr vs %d decr"
      state.incr_count state.decr_count :: !warnings;

  !warnings

(** Estimate memory usage based on variables and arrays *)
let estimate_memory program =
  let rec size_of_type = function
    | TPrim TI32 | TPrim TU32 -> 4
    | TPrim TI64 | TPrim TU64 -> 8
    | TPrim TBool -> 1
    | TPrim TUnit -> 0
    | TRef _ -> 8  (* Pointer size *)
    | TArray (t, Some n) ->
      let elem_size = size_of_type t in
      elem_size * n
    | TArray (_, None) -> 0  (* Dynamic arrays not allowed in constrained form *)
    | TFun _ -> 0  (* Functions are not stored *)
    | TStruct _ -> 0  (* TODO: Need struct definitions *)
    | TTrace -> 1024  (* Estimated trace storage *)
  in

  let rec memory_of_stmt stmt =
    match stmt.stmt_desc with
    | SLet (_, Some t, _) | SLetMut (_, Some t, _) -> size_of_type t
    | SIf (_, then_stmts, else_stmts) ->
      let then_mem = List.fold_left (fun acc s -> acc + memory_of_stmt s) 0 then_stmts in
      let else_mem = List.fold_left (fun acc s -> acc + memory_of_stmt s) 0 else_stmts in
      max then_mem else_mem
    | SForRange (_, _, _, body) ->
      List.fold_left (fun acc s -> acc + memory_of_stmt s) 0 body
    | _ -> 0
  in

  List.fold_left (fun acc decl ->
    match decl.decl_desc with
    | DFunction { body; params; _ } ->
      let param_mem = List.fold_left (fun acc (_, t) -> acc + size_of_type t) 0 params in
      let body_mem = List.fold_left (fun acc s -> acc + memory_of_stmt s) 0 body in
      acc + param_mem + body_mem
    | DConst { typ; _ } -> acc + size_of_type typ
    | DStruct _ -> acc
  ) 0 program.declarations

(** Calculate trace coverage (percentage of code paths with traces) *)
let calculate_trace_coverage program =
  let total_stmts = ref 0 in
  let trace_stmts = ref 0 in

  let rec count_stmt stmt =
    total_stmts := !total_stmts + 1;
    match stmt.stmt_desc with
    | STrace _ | SCheckpoint _ | SAssertInvariant _ ->
      trace_stmts := !trace_stmts + 1
    | SIf (_, then_stmts, else_stmts) ->
      List.iter count_stmt then_stmts;
      List.iter count_stmt else_stmts
    | SForRange (_, _, _, body) ->
      List.iter count_stmt body
    | _ -> ()
  in

  List.iter (fun decl ->
    match decl.decl_desc with
    | DFunction { body; _ } -> List.iter count_stmt body
    | _ -> ()
  ) program.declarations;

  if !total_stmts = 0 then 0.0
  else float_of_int !trace_stmts /. float_of_int !total_stmts

(** Run complete static analysis *)
let analyze (program : program) : analysis_result =
  (* First, validate constrained form constraints *)
  let violations = Constrained_check.validate_program program in

  (* Calculate resource bounds *)
  let max_loop_iters = max_iterations_program program in
  let max_depth = calculate_call_depth program in
  let mem_usage = estimate_memory program in

  (* Analyze reversibility *)
  let rev_warnings = ref [] in
  List.iter (fun decl ->
    match decl.decl_desc with
    | DFunction { body; _ } ->
      List.iter (fun stmt ->
        rev_warnings := !rev_warnings @ analyze_reversibility_stmt stmt
      ) body
    | _ -> ()
  ) program.declarations;

  (* Calculate trace coverage *)
  let coverage = calculate_trace_coverage program in

  {
    is_valid = violations = [];
    violations;
    max_loop_iterations = max_loop_iters;
    max_call_depth = max_depth;
    estimated_memory = mem_usage;
    reversibility_warnings = !rev_warnings;
    trace_coverage = coverage;
  }

(** Format analysis result as human-readable report *)
let format_report result =
  let buf = Buffer.create 1024 in

  Buffer.add_string buf "=== Oblíbený Static Analysis Report ===\n\n";

  (* Validation status *)
  Buffer.add_string buf "## Constrained Form Validation\n";
  if result.is_valid then
    Buffer.add_string buf "✓ VALID - Program conforms to Turing-incomplete constrained form\n"
  else begin
    Buffer.add_string buf "✗ INVALID - Program violates constrained form rules:\n";
    List.iter (fun v ->
      Buffer.add_string buf ("  " ^ Constrained_check.format_violation v ^ "\n")
    ) result.violations
  end;
  Buffer.add_string buf "\n";

  (* Resource bounds *)
  Buffer.add_string buf "## Resource Bounds (Static Guarantees)\n";
  Buffer.add_string buf (Printf.sprintf "Max loop iterations: %Ld\n" result.max_loop_iterations);
  Buffer.add_string buf (Printf.sprintf "Max call depth: %d\n" result.max_call_depth);
  Buffer.add_string buf (Printf.sprintf "Estimated memory: %d bytes\n" result.estimated_memory);
  Buffer.add_string buf "\n";

  (* Reversibility analysis *)
  Buffer.add_string buf "## Reversibility Analysis\n";
  if result.reversibility_warnings = [] then
    Buffer.add_string buf "✓ All reversible operations are properly balanced\n"
  else begin
    Buffer.add_string buf "⚠ Warnings:\n";
    List.iter (fun w -> Buffer.add_string buf ("  " ^ w ^ "\n")) result.reversibility_warnings
  end;
  Buffer.add_string buf "\n";

  (* Accountability *)
  Buffer.add_string buf "## Accountability Trace Coverage\n";
  Buffer.add_string buf (Printf.sprintf "Coverage: %.1f%%\n" (result.trace_coverage *. 100.0));
  if result.trace_coverage < 0.3 then
    Buffer.add_string buf "⚠ Low trace coverage - consider adding more trace/checkpoint statements\n";
  Buffer.add_string buf "\n";

  Buffer.contents buf
