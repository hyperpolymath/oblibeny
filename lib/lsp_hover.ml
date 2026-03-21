(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell *)

(** Hover information for oblibeny *)

open Lsp_protocol

(** Get hover information at a specific position *)
let get_hover (text : string) (pos : position) : hover_result option =
  (* Parse the file *)
  let tmp_file = Filename.temp_file "oblibeny_hover" ".obl" in
  let oc = open_out tmp_file in
  output_string oc text;
  close_out oc;

  let result = match Parse.parse_file tmp_file with
    | Error _ -> None
    | Ok _program ->
      (* Find symbol at position *)
      (* For MVP: provide basic hover based on keywords and context *)
      let line_num = pos.line + 1 in  (* Convert to 1-indexed *)
      let lines = String.split_on_char '\n' text in
      if line_num > List.length lines then
        None
      else
        let line = List.nth lines pos.line in
        let col = pos.character in
        if col >= String.length line then
          None
        else
          (* Extract word at position *)
          let rec find_word_start i =
            if i < 0 then 0
            else if line.[i] = ' ' || line.[i] = '\t' || line.[i] = '(' || line.[i] = ')' then i + 1
            else find_word_start (i - 1)
          in
          let rec find_word_end i =
            if i >= String.length line then String.length line
            else if line.[i] = ' ' || line.[i] = '\t' || line.[i] = '(' || line.[i] = ')' then i
            else find_word_end (i + 1)
          in
          let word_start = find_word_start col in
          let word_end = find_word_end col in
          let word = String.sub line word_start (word_end - word_start) in

          (* Provide hover info based on word *)
          let hover_text = match word with
            | "let" -> "**let** - Bind a value to a variable\n\nSyntax: `let <name> = <expr>`"
            | "fn" -> "**fn** - Define a function\n\nSyntax: `fn <name>(<params>) = <body>`"
            | "if" -> "**if** - Conditional expression\n\nSyntax: `if <cond> then <expr1> else <expr2>`"
            | "match" -> "**match** - Pattern matching\n\nSyntax: `match <expr> with | <pattern> -> <expr>`"
            | "type" -> "**type** - Define a custom type\n\nSyntax: `type <name> = <definition>`"
            | "module" -> "**module** - Module definition\n\nSyntax: `module <name> = <body>`"
            | "trace" -> "**trace** - Add to accountability trace\n\nOblibeny's accountability feature"
            | "forbid" -> "**forbid** - Constrained form restriction\n\nForbids recursion in constrained form"
            | "bounded" -> "**bounded** - Constrained form loop\n\nBounded iteration (no recursion)"
            | _ ->
              (* Try to get type from typechecker *)
              Printf.sprintf "**%s**\n\nIdentifier in oblibeny program" word
          in

          Some {
            contents = hover_text;
            range = Some {
              start_pos = { line = pos.line; character = word_start };
              end_pos = { line = pos.line; character = word_end };
            };
          }
  in

  (* Clean up temp file *)
  (try Sys.remove tmp_file with _ -> ());

  result
