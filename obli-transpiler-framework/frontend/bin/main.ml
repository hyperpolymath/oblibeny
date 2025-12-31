(* SPDX-License-Identifier: MIT OR Palimpsest-0.8 *)
(* Copyright (c) 2024 Hyperpolymath *)

(** Oblibeny Frontend CLI

    Parses .obl source files, performs type checking and obliviousness
    analysis, then emits OIR (Oblivious Intermediate Representation)
    for the Rust backend.
*)

open Oblibeny_frontend

let version = "0.1.0"

(** Command line options *)
type options = {
  mutable input_file: string option;
  mutable output_file: string option;
  mutable dump_ast: bool;
  mutable dump_oir: bool;
  mutable check_only: bool;
  mutable verbose: bool;
}

let default_options () = {
  input_file = None;
  output_file = None;
  dump_ast = false;
  dump_oir = false;
  check_only = false;
  verbose = false;
}

let usage_msg = "oblibeny-frontend [OPTIONS] <input.obl>"

let parse_args () =
  let opts = default_options () in
  let specs = [
    ("-o", Arg.String (fun s -> opts.output_file <- Some s),
     "<file>  Output OIR file (default: <input>.oir.json)");
    ("--dump-ast", Arg.Unit (fun () -> opts.dump_ast <- true),
     "  Dump parsed AST to stderr");
    ("--dump-oir", Arg.Unit (fun () -> opts.dump_oir <- true),
     "  Dump OIR to stderr");
    ("--check", Arg.Unit (fun () -> opts.check_only <- true),
     "  Only type-check, don't emit OIR");
    ("-v", Arg.Unit (fun () -> opts.verbose <- true),
     "  Verbose output");
    ("--verbose", Arg.Unit (fun () -> opts.verbose <- true),
     "  Verbose output");
    ("--version", Arg.Unit (fun () ->
      Printf.printf "oblibeny-frontend %s\n" version;
      exit 0),
     "  Print version and exit");
  ] in
  Arg.parse specs (fun s -> opts.input_file <- Some s) usage_msg;
  opts

(** Parse source file *)
let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with
    Lexing.pos_fname = filename;
  };
  try
    let program = Parser.program Lexer.token lexbuf in
    close_in ic;
    Ok program
  with
  | Lexer.Lexer_error (msg, pos) ->
    close_in ic;
    Error (Printf.sprintf "%s:%d:%d: lexer error: %s"
      pos.Lexing.pos_fname
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
      msg)
  | Parsing.Parse_error ->
    let pos = lexbuf.Lexing.lex_curr_p in
    close_in ic;
    Error (Printf.sprintf "%s:%d:%d: syntax error"
      pos.Lexing.pos_fname
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))

(** Main compilation pipeline *)
let compile opts =
  let input_file = match opts.input_file with
    | Some f -> f
    | None ->
      prerr_endline "error: no input file";
      exit 1
  in

  let output_file = match opts.output_file with
    | Some f -> f
    | None ->
      let base = Filename.remove_extension input_file in
      base ^ ".oir.json"
  in

  if opts.verbose then
    Printf.eprintf "Parsing %s...\n%!" input_file;

  (* Parse *)
  let program = match parse_file input_file with
    | Ok p -> p
    | Error msg ->
      prerr_endline msg;
      exit 1
  in

  if opts.dump_ast then begin
    prerr_endline "=== AST ===";
    prerr_endline (Ast.show_program program)
  end;

  if opts.verbose then
    Printf.eprintf "Type checking...\n%!";

  (* Type check *)
  let type_diags = Typecheck.check_program program in
  if Errors.has_errors type_diags then begin
    Errors.print_diagnostics type_diags;
    exit 1
  end;

  if opts.verbose then
    Printf.eprintf "Checking obliviousness...\n%!";

  (* Obliviousness check *)
  let (obli_diags, violations) = Oblicheck.check_program program in
  if violations > 0 then begin
    Errors.print_diagnostics obli_diags;
    let result = Oblicheck.analyze_violations obli_diags in
    Printf.eprintf "\nObliviousness violations: %d\n" result.total_violations;
    Printf.eprintf "  Secret branches: %d\n" result.secret_branches;
    Printf.eprintf "  Secret indices: %d\n" result.secret_indices;
    Printf.eprintf "  Secret loop bounds: %d\n" result.secret_loops;
    Printf.eprintf "  Information leaks: %d\n" result.info_leaks;
    exit 1
  end;

  (* Print warnings *)
  Errors.print_diagnostics type_diags;
  Errors.print_diagnostics obli_diags;

  if opts.check_only then begin
    if opts.verbose then
      Printf.eprintf "Check passed.\n%!";
    exit 0
  end;

  if opts.verbose then
    Printf.eprintf "Emitting OIR to %s...\n%!" output_file;

  (* Emit OIR *)
  let oir_module = Emit_oir.emit_module program in

  if opts.dump_oir then begin
    prerr_endline "=== OIR ===";
    prerr_endline (Emit_oir.to_json oir_module)
  end;

  Emit_oir.write_oir output_file oir_module;

  if opts.verbose then
    Printf.eprintf "Done.\n%!";

  exit 0

let () =
  let opts = parse_args () in
  compile opts
