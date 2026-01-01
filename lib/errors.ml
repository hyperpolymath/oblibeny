(* SPDX-License-Identifier: MIT OR Palimpsest-0.8 *)
(* Copyright (c) 2026 Hyperpolymath *)

(** Error handling for Oblíbený compiler *)

open Location

type severity =
  | Error
  | Warning
  | Note
  [@@deriving show]

type error_kind =
  | TypeError of string
  | ConstraintViolation of string
  | ParseError of string
  | InternalError of string
  [@@deriving show]

type diagnostic = {
  severity: severity;
  kind: error_kind;
  message: string;
  location: Location.t;
  notes: string list;
} [@@deriving show]

type diagnostics = {
  mutable items: diagnostic list;
}

let create_diagnostics () = { items = [] }

let report diags diag =
  diags.items <- diag :: diags.items

let error ~kind ~loc ~msg ?(notes=[]) diags =
  report diags { severity = Error; kind; message = msg; location = loc; notes }

let warning ~kind ~loc ~msg ?(notes=[]) diags =
  report diags { severity = Warning; kind; message = msg; location = loc; notes }

let has_errors diags =
  List.exists (fun d -> d.severity = Error) diags.items

let get_errors diags =
  List.filter (fun d -> d.severity = Error) diags.items

let format_diagnostic d =
  let sev_str = match d.severity with
    | Error -> "error"
    | Warning -> "warning"
    | Note -> "note"
  in
  let loc_str = Location.to_string d.location in
  let lines = [
    Printf.sprintf "%s: %s: %s" loc_str sev_str d.message
  ] in
  let note_lines = List.map (fun n -> Printf.sprintf "  note: %s" n) d.notes in
  String.concat "\n" (lines @ note_lines)

let print_diagnostics diags =
  let sorted = List.rev diags.items in
  List.iter (fun d -> prerr_endline (format_diagnostic d)) sorted
