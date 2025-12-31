(* SPDX-License-Identifier: MIT OR Palimpsest-0.8 *)
(* Copyright (c) 2024 Hyperpolymath *)

(** Error reporting and diagnostics *)

open Location

type severity =
  | Error
  | Warning
  | Note
  [@@deriving show]

type error_kind =
  (* Lexer errors *)
  | Unexpected_character of char
  | Unterminated_comment
  | Unterminated_string
  | Invalid_escape of char

  (* Parser errors *)
  | Syntax_error of string
  | Unexpected_token of string

  (* Type errors *)
  | Type_mismatch of { expected: string; found: string }
  | Unknown_identifier of string
  | Unknown_type of string
  | Duplicate_definition of string
  | Invalid_operation of { op: string; typ: string }
  | Arity_mismatch of { expected: int; found: int }
  | Not_a_function of string
  | Field_not_found of { struct_name: string; field: string }
  | Cannot_infer_type
  | Recursive_type

  (* Obliviousness errors *)
  | Secret_dependent_branch
  | Secret_array_index of string
  | Secret_loop_bound
  | Non_oblivious_operation of string
  | Information_leak of { from_label: string; to_label: string }

  (* Other errors *)
  | Internal_error of string
  [@@deriving show]

type diagnostic = {
  severity: severity;
  kind: error_kind;
  loc: Location.t;
  message: string;
  suggestion: string option;
  related: (Location.t * string) list;
} [@@deriving show]

let make_error kind loc message = {
  severity = Error;
  kind;
  loc;
  message;
  suggestion = None;
  related = [];
}

let make_warning kind loc message = {
  severity = Warning;
  kind;
  loc;
  message;
  suggestion = None;
  related = [];
}

let with_suggestion suggestion diag =
  { diag with suggestion = Some suggestion }

let with_related related diag =
  { diag with related }

(** Diagnostics accumulator *)
type diagnostics = {
  mutable errors: diagnostic list;
  mutable warnings: diagnostic list;
}

let create_diagnostics () = {
  errors = [];
  warnings = [];
}

let report diags diag =
  match diag.severity with
  | Error -> diags.errors <- diag :: diags.errors
  | Warning | Note -> diags.warnings <- diag :: diags.warnings

let has_errors diags = diags.errors <> []

let get_errors diags = List.rev diags.errors
let get_warnings diags = List.rev diags.warnings

(** Pretty printing *)
let severity_to_string = function
  | Error -> "error"
  | Warning -> "warning"
  | Note -> "note"

let format_diagnostic diag =
  let sev = severity_to_string diag.severity in
  let loc = Location.to_string diag.loc in
  let main = Printf.sprintf "%s: %s: %s" loc sev diag.message in
  let suggestion = match diag.suggestion with
    | Some s -> Printf.sprintf "\n  suggestion: %s" s
    | None -> ""
  in
  let related = diag.related
    |> List.map (fun (loc, msg) ->
         Printf.sprintf "\n  %s: note: %s" (Location.to_string loc) msg)
    |> String.concat ""
  in
  main ^ suggestion ^ related

let print_diagnostics diags =
  List.iter (fun d -> prerr_endline (format_diagnostic d)) (get_errors diags);
  List.iter (fun d -> prerr_endline (format_diagnostic d)) (get_warnings diags)

(** Convenience functions for common errors *)
let type_mismatch ~expected ~found loc =
  make_error
    (Type_mismatch { expected; found })
    loc
    (Printf.sprintf "type mismatch: expected `%s`, found `%s`" expected found)

let unknown_identifier name loc =
  make_error
    (Unknown_identifier name)
    loc
    (Printf.sprintf "unknown identifier `%s`" name)

let unknown_type name loc =
  make_error
    (Unknown_type name)
    loc
    (Printf.sprintf "unknown type `%s`" name)

let secret_branch loc =
  make_error
    Secret_dependent_branch
    loc
    "branch condition depends on secret data"
  |> with_suggestion "use cmov() or oblivious selection instead"

let secret_index array_name loc =
  make_error
    (Secret_array_index array_name)
    loc
    (Printf.sprintf "array `%s` indexed with secret value" array_name)
  |> with_suggestion "use oarray<T> with oread()/owrite() for oblivious access"

let secret_loop_bound loc =
  make_error
    Secret_loop_bound
    loc
    "loop bound depends on secret data"
  |> with_suggestion "use fixed iteration count or oblivious loop"

let information_leak ~from_label ~to_label loc =
  make_error
    (Information_leak { from_label; to_label })
    loc
    (Printf.sprintf "information flow from @%s to @%s" from_label to_label)
