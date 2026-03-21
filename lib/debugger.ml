(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell *)

(** Reversible Debugger for Oblíbený

    This debugger understands Oblibeny's reversible operations and can:
    1. Step forward through program execution
    2. Step backward by inverting reversible operations
    3. Show accountability checkpoints and traces
    4. Inspect variable state at any point in execution history

    Reversible operations supported:
    - swap(a, b) - self-inverse
    - incr(x, delta) <-> decr(x, delta)
    - xor(x, val) - self-inverse
    - push(stack, val) <-> pop(stack)
*)

open Ast

(** Debugger state at a single point in execution *)
type debug_state = {
  stmt_index: int;  (* Current statement being executed *)
  variables: (string * Eval.value) list;  (* Variable bindings *)
  trace_entries: Trace.entry list;  (* Accountability trace so far *)
  checkpoint_stack: string list;  (* Stack of checkpoint labels *)
}

(** Debugger command *)
type debug_command =
  | StepForward     (* Execute next statement *)
  | StepBackward    (* Reverse last statement *)
  | Continue        (* Run to next checkpoint or end *)
  | ShowState       (* Display current state *)
  | ShowTrace       (* Display accountability trace *)
  | ShowCheckpoints (* Show checkpoint history *)
  | SetBreakpoint of string  (* Break at checkpoint label *)
  | Quit

(** Execution history for reverse debugging *)
type execution_history = {
  states: debug_state list;  (* Stack of previous states *)
  current: debug_state;      (* Current state *)
}

(** Create initial debugger state *)
let initial_state = {
  stmt_index = 0;
  variables = [];
  trace_entries = [];
  checkpoint_stack = [];
}

(** Reverse a reversible statement *)
let reverse_stmt stmt = match stmt.stmt_desc with
  | SIncr (var, delta) ->
    (* incr(x, delta) reverses to decr(x, delta) *)
    mk_stmt stmt.stmt_loc (SDecr (var, delta))
  | SDecr (var, delta) ->
    (* decr(x, delta) reverses to incr(x, delta) *)
    mk_stmt stmt.stmt_loc (SIncr (var, delta))
  | SXorAssign (_var, _value) ->
    (* xor is self-inverse *)
    stmt
  | SSwap (_a, _b) ->
    (* swap is self-inverse *)
    stmt
  | _ ->
    failwith "Statement is not reversible"

(** Check if statement is reversible *)
let is_reversible stmt = match stmt.stmt_desc with
  | SIncr _ | SDecr _ | SXorAssign _ | SSwap _ -> true
  | _ -> false

(** Step forward: execute one statement *)
let step_forward state program =
  if state.stmt_index >= List.length program then
    None  (* End of program *)
  else
    let _stmt = List.nth program state.stmt_index in
    (* TODO: Actually execute the statement and update variables *)
    (* For now, just advance the index *)
    Some {
      state with
      stmt_index = state.stmt_index + 1;
    }

(** Step backward: reverse last statement *)
let step_backward history =
  match history.states with
  | [] -> None  (* At beginning, can't go back *)
  | prev :: rest ->
    Some {
      states = rest;
      current = prev;
    }

(** Format debugger state for display *)
let format_state state =
  let buf = Buffer.create 256 in

  Buffer.add_string buf "=== Debugger State ===\n";
  Buffer.add_string buf (Printf.sprintf "Statement: %d\n" state.stmt_index);

  Buffer.add_string buf "\nVariables:\n";
  List.iter (fun (name, value) ->
    Buffer.add_string buf (Printf.sprintf "  %s = %s\n" name (Eval.show_value value))
  ) state.variables;

  Buffer.add_string buf "\nCheckpoints:\n";
  List.iter (fun label ->
    Buffer.add_string buf (Printf.sprintf "  - %s\n" label)
  ) (List.rev state.checkpoint_stack);

  Buffer.add_string buf (Printf.sprintf "\nTrace entries: %d\n" (List.length state.trace_entries));

  Buffer.contents buf

(** Format trace for display *)
let format_trace state =
  let buf = Buffer.create 512 in
  Buffer.add_string buf "=== Accountability Trace ===\n";
  List.iteri (fun i entry ->
    Buffer.add_string buf (Printf.sprintf "[%d] %s\n" i (Trace.show_entry entry))
  ) (List.rev state.trace_entries);
  Buffer.contents buf

(** Interactive REPL-style debugger *)
let rec debug_repl history program =
  print_string "\n[oblibeny-debug] ";
  flush stdout;

  match read_line () with
  | exception End_of_file -> ()
  | "s" | "step" ->
    (match step_forward history.current program with
     | Some new_state ->
       let new_history = {
         states = history.current :: history.states;
         current = new_state;
       } in
       Printf.printf "Stepped to statement %d\n" new_state.stmt_index;
       debug_repl new_history program
     | None ->
       print_endline "End of program";
       debug_repl history program)

  | "b" | "back" ->
    (match step_backward history with
     | Some new_history ->
       Printf.printf "Stepped back to statement %d\n" new_history.current.stmt_index;
       debug_repl new_history program
     | None ->
       print_endline "Already at beginning";
       debug_repl history program)

  | "p" | "print" ->
    print_string (format_state history.current);
    debug_repl history program

  | "t" | "trace" ->
    print_string (format_trace history.current);
    debug_repl history program

  | "c" | "continue" ->
    (* TODO: Run to next checkpoint *)
    print_endline "Continue not implemented yet";
    debug_repl history program

  | "h" | "help" ->
    print_endline "Commands:";
    print_endline "  s, step      - Step forward";
    print_endline "  b, back      - Step backward (reversible debugging)";
    print_endline "  p, print     - Print current state";
    print_endline "  t, trace     - Show accountability trace";
    print_endline "  c, continue  - Run to next checkpoint";
    print_endline "  h, help      - Show this help";
    print_endline "  q, quit      - Exit debugger";
    debug_repl history program

  | "q" | "quit" ->
    print_endline "Exiting debugger";
    ()

  | "" ->
    debug_repl history program

  | cmd ->
    Printf.printf "Unknown command: %s (type 'h' for help)\n" cmd;
    debug_repl history program

(** Start interactive debugger on a program *)
let debug_program (program : program) =
  print_endline "Oblíbený Reversible Debugger";
  print_endline "Type 'h' for help";

  (* Extract main function body *)
  let main_body = match List.find_opt (fun decl ->
    match decl.decl_desc with
    | DFunction { name = "main"; _ } -> true
    | _ -> false
  ) program.declarations with
  | Some { decl_desc = DFunction { body; _ }; _ } -> body
  | _ -> failwith "No main function found"
  in

  let initial_history = {
    states = [];
    current = initial_state;
  } in

  debug_repl initial_history main_body
