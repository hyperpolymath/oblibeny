(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell *)

(** Parser integration module

    Connects the lexer and Menhir parser together.
*)

open Errors

(** Parse error exception *)
exception ParseError of string * Location.t

(** Convert lexer tokens to Menhir tokens *)
let rec tokens_of_lexbuf lexbuf =
  match Lexer.next_token lexbuf with
  | Parser.EOF -> []
  | tok -> tok :: tokens_of_lexbuf lexbuf

(** Token supplier for Menhir incremental API *)
let token_supplier tokens =
  let tokens = ref tokens in
  fun () ->
    match !tokens with
    | [] -> (Parser.EOF, Lexing.dummy_pos, Lexing.dummy_pos)
    | tok :: rest ->
      tokens := rest;
      (tok, Lexing.dummy_pos, Lexing.dummy_pos)

(** Parse a string into a program AST *)
let parse_string source =
  try
    (* Lex the source *)
    let lexbuf = Lexer.create_lexbuf source in
    let tokens = ref [] in
    let rec collect () =
      match Lexer.next_token lexbuf with
      | Parser.EOF -> List.rev !tokens
      | tok -> tokens := tok :: !tokens; collect ()
    in
    let token_list = collect () in

    (* Create a stateful token supplier for Menhir *)
    let remaining = ref token_list in
    let lexer _lexbuf =
      match !remaining with
      | [] -> Parser.EOF
      | tok :: rest ->
        remaining := rest;
        tok
    in

    (* Parse using Menhir *)
    let dummy_lexbuf = Lexing.from_string source in
    Ok (Parser.program lexer dummy_lexbuf)

  with
  | Lexer.LexError (msg, loc) ->
    Error (format_error "Lexical error" msg loc)
  | Parser.Error ->
    Error "Parse error: syntax error"
  | e ->
    Error (Printf.sprintf "Unexpected error: %s" (Printexc.to_string e))

(** Parse a file into a program AST *)
let parse_file filename =
  try
    let ic = open_in filename in
    let source = really_input_string ic (in_channel_length ic) in
    close_in ic;
    parse_string source
  with
  | Sys_error msg ->
    Error (Printf.sprintf "File error: %s" msg)
