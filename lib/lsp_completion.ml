(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell *)

(** Code completion for oblibeny *)

open Lsp_protocol

(** Oblibeny keywords *)
let keywords = [
  ("let", "Bind a value to a variable");
  ("fn", "Define a function");
  ("if", "Conditional expression");
  ("then", "If-then-else branch");
  ("else", "Else branch");
  ("match", "Pattern matching");
  ("with", "Match cases");
  ("type", "Type definition");
  ("module", "Module definition");
  ("trace", "Add to accountability trace");
  ("forbid", "Constrained form restriction");
  ("bounded", "Bounded iteration");
  ("for", "For loop");
  ("while", "While loop");
  ("return", "Return from function");
  ("true", "Boolean true");
  ("false", "Boolean false");
]

(** Built-in functions *)
let builtins = [
  ("print", "fn(string) -> unit", "Print to stdout");
  ("println", "fn(string) -> unit", "Print line to stdout");
  ("read_line", "fn() -> string", "Read line from stdin");
  ("trace_add", "fn(string, value) -> unit", "Add entry to trace");
  ("trace_hash", "fn() -> int", "Get current trace hash");
  ("panic", "fn(string) -> never", "Abort with error message");
]

(** Get completion items at a specific position *)
let get_completions (text : string) (pos : position) : completion_item list =
  let line_num = pos.line in
  let lines = String.split_on_char '\n' text in
  if line_num >= List.length lines then
    []
  else
    let line = List.nth lines line_num in
    let col = pos.character in

    (* Extract prefix before cursor *)
    let prefix = if col > String.length line then "" else String.sub line 0 col in
    let words = String.split_on_char ' ' prefix in
    let current_word = if words = [] then "" else List.hd (List.rev words) in

    (* Filter keywords by prefix *)
    let keyword_completions = List.filter_map (fun (kw, doc) ->
      if String.starts_with ~prefix:current_word kw then
        Some {
          label = kw;
          kind = Keyword;
          detail = Some "keyword";
          documentation = Some doc;
          insert_text = Some kw;
        }
      else None
    ) keywords in

    (* Filter builtins by prefix *)
    let builtin_completions = List.filter_map (fun (name, typ, doc) ->
      if String.starts_with ~prefix:current_word name then
        Some {
          label = name;
          kind = Function;
          detail = Some typ;
          documentation = Some doc;
          insert_text = Some (name ^ "()");
        }
      else None
    ) builtins in

    keyword_completions @ builtin_completions

(** Get completion response *)
let completion_response items =
  `List (List.map completion_item_to_json items)
