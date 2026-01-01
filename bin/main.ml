(* SPDX-License-Identifier: MIT OR Palimpsest-0.8 *)
(* Copyright (c) 2026 Hyperpolymath *)

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
  mutable verbose: bool;
}

let default_options () = {
  input_file = None;
  dump_ast = false;
  dump_trace = false;
  check_only = false;
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

(** Minimal parser for f0 - just creates a test program
    Real parser will be added in f1 *)
let parse_file _filename =
  (* For f0, return a simple test program demonstrating the language *)
  Ok Oblibeny.Ast.{
    module_name = Some "hello";
    declarations = [
      mk_decl Location.dummy (DFunction {
        name = "main";
        params = [];
        return_type = TPrim TUnit;
        body = [
          (* let mut x = 0 *)
          mk_stmt Location.dummy (SLetMut ("x", Some (TPrim TI64), mk_expr Location.dummy (ELiteral (LInt 0L))));
          (* checkpoint("start") *)
          mk_stmt Location.dummy (SCheckpoint "start");
          (* for i in 0..10 { incr(x, 1); trace("increment", x); } *)
          mk_stmt Location.dummy (SForRange ("i", 0L, 10L, [
            mk_stmt Location.dummy (SIncr ("x", mk_expr Location.dummy (ELiteral (LInt 1L))));
            mk_stmt Location.dummy (STrace ("increment", [mk_expr Location.dummy (EVar "x")]));
          ]));
          (* checkpoint("end") *)
          mk_stmt Location.dummy (SCheckpoint "end");
          (* assert_invariant(x == 10, "x should be 10") *)
          mk_stmt Location.dummy (SAssertInvariant (
            mk_expr Location.dummy (EBinop (Eq,
              mk_expr Location.dummy (EVar "x"),
              mk_expr Location.dummy (ELiteral (LInt 10L)))),
            "x should be 10"));
        ];
      })
    ];
  }

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
