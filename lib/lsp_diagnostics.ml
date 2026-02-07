(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell *)

(** Convert oblibeny compiler errors to LSP diagnostics *)

open Lsp_protocol

(** Convert oblibeny location to LSP range *)
let location_to_range (loc : Location.t) : range =
  {
    start_pos = {
      line = loc.loc_start.pos_line - 1;  (* LSP is 0-indexed *)
      character = loc.loc_start.pos_col - 1;
    };
    end_pos = {
      line = loc.loc_end.pos_line - 1;
      character = loc.loc_end.pos_col - 1;
    };
  }

(** Convert oblibeny error to LSP diagnostic *)
let error_to_diagnostic (err : Errors.diagnostic) : lsp_diagnostic =
  {
    range = location_to_range err.location;
    severity = (match err.severity with
      | Errors.Error -> LspError
      | Errors.Warning -> LspWarning
      | Errors.Note -> LspInformation);
    message = err.message;
    source = Some "oblibeny";
  }

(** Get diagnostics for a file by parsing and type-checking *)
let get_diagnostics (_uri : string) (text : string) : lsp_diagnostic list =
  (* Write text to temporary file for parsing *)
  let tmp_file = Filename.temp_file "oblibeny_lsp" ".obl" in
  let oc = open_out tmp_file in
  output_string oc text;
  close_out oc;

  let diagnostics = ref [] in

  (* Try parsing *)
  (match Parse.parse_file tmp_file with
   | Error msg ->
     (* Parse error - create diagnostic at start of file *)
     diagnostics := {
       range = {
         start_pos = { line = 0; character = 0 };
         end_pos = { line = 0; character = 1 };
       };
       severity = LspError;
       message = msg;
       source = Some "oblibeny-parser";
     } :: !diagnostics
   | Ok program ->
     (* Try type checking *)
     (match Typecheck.typecheck_program program with
      | Ok () -> ()
      | Error type_errors ->
        let type_diags = List.map error_to_diagnostic type_errors.items in
        diagnostics := type_diags @ !diagnostics);

     (* Try constrained form validation *)
     let violations = Constrained_check.validate_program program in
     let violation_diags = List.filter_map (fun v ->
       let loc_opt = match v with
         | Ast.WhileLoopFound loc -> Some loc
         | Ast.LoopKeywordFound loc -> Some loc
         | Ast.RecursiveCall { loc; _ } -> Some loc
         | Ast.DynamicForBound { loc; _ } -> Some loc
         | Ast.CyclicCallGraph _ -> None  (* No specific location *)
       in
       match loc_opt with
       | None -> None
       | Some loc ->
         Some {
           range = location_to_range loc;
           severity = LspWarning;
           message = Constrained_check.format_violation v;
           source = Some "oblibeny-constrained";
         }
     ) violations in
     diagnostics := violation_diags @ !diagnostics
  );

  (* Clean up temp file *)
  (try Sys.remove tmp_file with _ -> ());

  List.rev !diagnostics

(** Publish diagnostics notification *)
let publish_diagnostics uri diagnostics =
  let diag_json = `List (List.map lsp_diagnostic_to_json diagnostics) in
  let params = `Assoc [
    ("uri", `String uri);
    ("diagnostics", diag_json);
  ] in
  create_notification "textDocument/publishDiagnostics" params
