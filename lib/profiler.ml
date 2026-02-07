(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell *)

(** Profiler for Oblíbený with Resource Bounds Tracking

    Profiles program execution with focus on:
    1. Static resource bounds (guaranteed by Turing-incompleteness)
    2. Actual vs estimated resource usage
    3. Reversible operation performance (forward vs backward)
    4. Accountability trace overhead
*)

open Ast

(** Profiling data collected during execution *)
type profile_data = {
  total_statements: int;
  loop_iterations: int64;
  function_calls: int;
  reversible_ops: int;  (* incr, decr, swap, xor *)
  trace_entries: int;
  checkpoints: int;
  execution_time_ms: float;
  memory_used_bytes: int;
}

(** Profile result with analysis *)
type profile_result = {
  data: profile_data;
  static_analysis: Static_analyzer.analysis_result;
  efficiency_score: float;  (* 0.0 to 1.0 *)
  bottlenecks: string list;
}

(** Create empty profile data *)
let empty_profile = {
  total_statements = 0;
  loop_iterations = 0L;
  function_calls = 0;
  reversible_ops = 0;
  trace_entries = 0;
  checkpoints = 0;
  execution_time_ms = 0.0;
  memory_used_bytes = 0;
}

(** Count statements in program *)
let rec count_stmts_in_body stmts =
  List.fold_left (fun acc stmt ->
    acc + 1 + match stmt.stmt_desc with
    | SIf (_, then_stmts, else_stmts) ->
      count_stmts_in_body then_stmts + count_stmts_in_body else_stmts
    | SForRange (_, _, _, body) ->
      count_stmts_in_body body
    | _ -> 0
  ) 0 stmts

let count_statements program =
  List.fold_left (fun acc decl ->
    match decl.decl_desc with
    | DFunction { body; _ } -> acc + count_stmts_in_body body
    | _ -> acc
  ) 0 program.declarations

(** Count reversible operations *)
let rec count_reversible_in_body stmts =
  List.fold_left (fun _acc stmt ->
    let base = match stmt.stmt_desc with
    | SIncr _ | SDecr _ | SXorAssign _ | SSwap _ -> 1
    | _ -> 0
    in
    base + match stmt.stmt_desc with
    | SIf (_, then_stmts, else_stmts) ->
      count_reversible_in_body then_stmts + count_reversible_in_body else_stmts
    | SForRange (_, _, _, body) ->
      count_reversible_in_body body
    | _ -> 0
  ) 0 stmts

let count_reversible program =
  List.fold_left (fun acc decl ->
    match decl.decl_desc with
    | DFunction { body; _ } -> acc + count_reversible_in_body body
    | _ -> acc
  ) 0 program.declarations

(** Count trace operations *)
let rec count_traces_in_body stmts =
  List.fold_left (fun _acc stmt ->
    let base = match stmt.stmt_desc with
    | STrace _ | SCheckpoint _ | SAssertInvariant _ -> 1
    | _ -> 0
    in
    base + match stmt.stmt_desc with
    | SIf (_, then_stmts, else_stmts) ->
      count_traces_in_body then_stmts + count_traces_in_body else_stmts
    | SForRange (_, _, _, body) ->
      count_traces_in_body body
    | _ -> 0
  ) 0 stmts

let count_traces program =
  List.fold_left (fun acc decl ->
    match decl.decl_desc with
    | DFunction { body; _ } -> acc + count_traces_in_body body
    | _ -> acc
  ) 0 program.declarations

(** Profile a program *)
let profile (program : program) : profile_result =
  (* Run static analysis first *)
  let static_analysis = Static_analyzer.analyze program in

  (* Collect static metrics *)
  let data = {
    total_statements = count_statements program;
    loop_iterations = static_analysis.max_loop_iterations;
    function_calls = 0;  (* TODO: Count actual calls during execution *)
    reversible_ops = count_reversible program;
    trace_entries = count_traces program;
    checkpoints = count_traces program;  (* Approximation *)
    execution_time_ms = 0.0;  (* TODO: Measure actual execution time *)
    memory_used_bytes = static_analysis.estimated_memory;
  } in

  (* Calculate efficiency score *)
  let efficiency =
    if static_analysis.trace_coverage > 0.0 then
      static_analysis.trace_coverage  (* Higher trace coverage = better accountability *)
    else
      0.5
  in

  (* Identify bottlenecks *)
  let bottlenecks = ref [] in
  if static_analysis.max_loop_iterations > 1000L then
    bottlenecks := "High loop iteration count" :: !bottlenecks;
  if static_analysis.max_call_depth > 10 then
    bottlenecks := "Deep call stack" :: !bottlenecks;
  if static_analysis.estimated_memory > 10240 then
    bottlenecks := "High memory usage (>10KB)" :: !bottlenecks;
  if static_analysis.trace_coverage < 0.3 then
    bottlenecks := "Low accountability trace coverage" :: !bottlenecks;

  {
    data;
    static_analysis;
    efficiency_score = efficiency;
    bottlenecks = !bottlenecks;
  }

(** Format profile result as human-readable report *)
let format_report result =
  let buf = Buffer.create 1024 in

  Buffer.add_string buf "=== Oblíbený Performance Profile ===\n\n";

  (* Execution metrics *)
  Buffer.add_string buf "## Execution Metrics\n";
  Buffer.add_string buf (Printf.sprintf "Total statements: %d\n" result.data.total_statements);
  Buffer.add_string buf (Printf.sprintf "Loop iterations: %Ld\n" result.data.loop_iterations);
  Buffer.add_string buf (Printf.sprintf "Reversible operations: %d\n" result.data.reversible_ops);
  Buffer.add_string buf (Printf.sprintf "Trace entries: %d\n" result.data.trace_entries);
  Buffer.add_string buf "\n";

  (* Resource usage *)
  Buffer.add_string buf "## Resource Usage\n";
  Buffer.add_string buf (Printf.sprintf "Memory: %d bytes\n" result.data.memory_used_bytes);
  Buffer.add_string buf (Printf.sprintf "Max call depth: %d\n" result.static_analysis.max_call_depth);
  Buffer.add_string buf "\n";

  (* Efficiency *)
  Buffer.add_string buf "## Efficiency Analysis\n";
  Buffer.add_string buf (Printf.sprintf "Overall score: %.1f%%\n" (result.efficiency_score *. 100.0));
  Buffer.add_string buf (Printf.sprintf "Trace coverage: %.1f%%\n" (result.static_analysis.trace_coverage *. 100.0));
  Buffer.add_string buf "\n";

  (* Bottlenecks *)
  if result.bottlenecks <> [] then begin
    Buffer.add_string buf "## Identified Bottlenecks\n";
    List.iter (fun b ->
      Buffer.add_string buf (Printf.sprintf "⚠ %s\n" b)
    ) result.bottlenecks;
    Buffer.add_string buf "\n"
  end;

  (* Recommendations *)
  Buffer.add_string buf "## Recommendations\n";
  if result.static_analysis.trace_coverage < 0.5 then
    Buffer.add_string buf "• Add more trace() and checkpoint() calls for better accountability\n";
  if result.data.reversible_ops = 0 then
    Buffer.add_string buf "• Consider using reversible operations (incr/decr, swap, xor) where appropriate\n";
  if List.length result.static_analysis.reversibility_warnings > 0 then
    Buffer.add_string buf "• Balance incr/decr operations for proper reversibility\n";

  Buffer.contents buf
