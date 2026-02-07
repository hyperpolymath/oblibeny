(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell *)

(** Oblíbený CLI

    Secure edge language for reversibility and accountability.
    Dual-form architecture: Turing-complete factory form produces
    Turing-incomplete constrained form.

    Usage: oblibeny [OPTIONS] <input.obl>
*)

let version = "0.1.0"

type options = {
  mutable input_file: string option;
  mutable dump_ast: bool;
  mutable dump_trace: bool;
  mutable check_only: bool;
  mutable analyze: bool;
  mutable verbose: bool;
}

let default_options () = {
  input_file = None;
  dump_ast = false;
  dump_trace = false;
  check_only = false;
  analyze = false;
  verbose = false;
}

let usage_msg = {|oblibeny - Secure edge language for reversibility and accountability

Usage: oblibeny [OPTIONS] <input.obl>|}

let parse_args () =
  let opts = default_options () in
  let specs = [
    ("--dump-ast", Arg.Unit (fun () -> opts.dump_ast <- true),
     " Dump parsed AST");
    ("--dump-trace", Arg.Unit (fun () -> opts.dump_trace <- true),
     " Dump accountability trace after execution");
    ("--check", Arg.Unit (fun () -> opts.check_only <- true),
     " Only check constrained form validity, don't execute");
    ("--analyze", Arg.Unit (fun () -> opts.analyze <- true),
     " Run comprehensive static analysis with resource bounds");
    ("-v", Arg.Unit (fun () -> opts.verbose <- true),
     " Verbose output");
    ("--version", Arg.Unit (fun () ->
      Printf.printf "oblibeny %s\n" version;
      Printf.printf "Secure edge language for reversibility and accountability\n";
      exit 0),
     " Print version and exit");
  ] in
  Arg.parse specs (fun s -> opts.input_file <- Some s) usage_msg;
  opts

(** Parse source file using real parser *)
let parse_file filename =
  Oblibeny.Parse.parse_file filename

let main () =
  let opts = parse_args () in
  let input_file = match opts.input_file with
    | Some f -> f
    | None ->
      prerr_endline "error: no input file specified";
      prerr_endline "usage: oblibeny <input.obl>";
      exit 1
  in

  if opts.verbose then
    Printf.eprintf "[oblibeny] Parsing %s...\n%!" input_file;

  let program = match parse_file input_file with
    | Ok p -> p
    | Error msg ->
      prerr_endline msg;
      exit 1
  in

  if opts.dump_ast then begin
    prerr_endline "=== AST ===";
    prerr_endline (Oblibeny.Ast.show_program program)
  end;

  if opts.verbose then
    Printf.eprintf "[oblibeny] Type checking...\n%!";

  (* Type check *)
  (match Oblibeny.Typecheck.typecheck_program program with
   | Ok () -> ()
   | Error diags ->
     Oblibeny.Errors.print_diagnostics diags;
     exit 1);

  if opts.verbose then
    Printf.eprintf "[oblibeny] Validating constrained form...\n%!";

  (* Validate constrained form *)
  let violations = Oblibeny.Constrained_check.validate_program program in
  if violations <> [] then begin
    prerr_endline "Constrained form violations:";
    List.iter (fun v ->
      prerr_endline (Oblibeny.Constrained_check.format_violation v)
    ) violations;
    exit 1
  end;

  (* Run comprehensive static analysis if requested *)
  if opts.analyze then begin
    if opts.verbose then
      Printf.eprintf "[oblibeny] Running static analysis...\n%!";
    let analysis_result = Oblibeny.Static_analyzer.analyze program in
    print_string (Oblibeny.Static_analyzer.format_report analysis_result);
    exit (if analysis_result.is_valid then 0 else 1)
  end;

  if opts.check_only then begin
    Printf.printf "Constrained form valid.\n";
    exit 0
  end;

  if opts.verbose then
    Printf.eprintf "[oblibeny] Executing...\n%!";

  (* Execute and produce trace *)
  let (result, trace) = Oblibeny.Eval.eval_program program in

  Printf.printf "Result: %s\n" (Oblibeny.Eval.show_value result);

  if opts.dump_trace then begin
    prerr_endline "=== Accountability Trace ===";
    prerr_endline (Oblibeny.Trace.to_json trace)
  end;

  Printf.printf "Trace entries: %d\n" (List.length (Oblibeny.Trace.to_list trace));
  Printf.printf "Trace hash: %d\n" (Oblibeny.Trace.hash trace)

let () = main ()
