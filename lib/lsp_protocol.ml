(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell *)

(** LSP Protocol Types and JSON-RPC handling *)

type position = {
  line: int;
  character: int;
}

type range = {
  start_pos: position;
  end_pos: position;
}

type location = {
  uri: string;
  range: range;
}

type lsp_severity =
  | LspError
  | LspWarning
  | LspInformation
  | LspHint

type lsp_diagnostic = {
  range: range;
  severity: lsp_severity;
  message: string;
  source: string option;
}

type text_document = {
  uri: string;
  language_id: string;
  version: int;
  text: string;
}

type hover_result = {
  contents: string;
  range: range option;
}

type completion_item_kind =
  | Text
  | Function
  | Variable
  | Keyword
  | Module

type completion_item = {
  label: string;
  kind: completion_item_kind;
  detail: string option;
  documentation: string option;
  insert_text: string option;
}

(** Convert LSP severity to LSP number *)
let severity_to_int = function
  | LspError -> 1
  | LspWarning -> 2
  | LspInformation -> 3
  | LspHint -> 4

(** Convert completion item kind to LSP number *)
let completion_kind_to_int = function
  | Text -> 1
  | Function -> 3
  | Variable -> 6
  | Keyword -> 14
  | Module -> 9

(** JSON-RPC message types *)
type message_id =
  | IntId of int
  | StringId of string
  | NullId

type rpc_request = {
  id: message_id;
  method_name: string;
  params: Yojson.Basic.t option;
}

type rpc_response = {
  id: message_id;
  result: Yojson.Basic.t option;
  error: (int * string) option;
}

type rpc_notification = {
  method_name: string;
  params: Yojson.Basic.t option;
}

(** Parse LSP position from JSON *)
let position_of_json json =
  let open Yojson.Basic.Util in
  {
    line = json |> member "line" |> to_int;
    character = json |> member "character" |> to_int;
  }

(** Parse LSP range from JSON *)
let range_of_json json =
  let open Yojson.Basic.Util in
  {
    start_pos = json |> member "start" |> position_of_json;
    end_pos = json |> member "end" |> position_of_json;
  }

(** Convert position to JSON *)
let position_to_json pos =
  `Assoc [
    ("line", `Int pos.line);
    ("character", `Int pos.character);
  ]

(** Convert range to JSON *)
let range_to_json range =
  `Assoc [
    ("start", position_to_json range.start_pos);
    ("end", position_to_json range.end_pos);
  ]

(** Convert LSP diagnostic to JSON *)
let lsp_diagnostic_to_json (diag : lsp_diagnostic) =
  let base = [
    ("range", range_to_json diag.range);
    ("severity", `Int (severity_to_int diag.severity));
    ("message", `String diag.message);
  ] in
  let with_source = match diag.source with
    | Some s -> ("source", `String s) :: base
    | None -> base
  in
  `Assoc with_source

(** Convert hover result to JSON *)
let hover_to_json hover =
  let contents = `Assoc [("language", `String "oblibeny"); ("value", `String hover.contents)] in
  let base = [("contents", contents)] in
  let with_range = match hover.range with
    | Some r -> ("range", range_to_json r) :: base
    | None -> base
  in
  `Assoc with_range

(** Convert completion item to JSON *)
let completion_item_to_json item =
  let base = [
    ("label", `String item.label);
    ("kind", `Int (completion_kind_to_int item.kind));
  ] in
  let with_detail = match item.detail with
    | Some d -> ("detail", `String d) :: base
    | None -> base
  in
  let with_doc = match item.documentation with
    | Some doc -> ("documentation", `String doc) :: with_detail
    | None -> with_detail
  in
  let with_insert = match item.insert_text with
    | Some text -> ("insertText", `String text) :: with_doc
    | None -> with_doc
  in
  `Assoc with_insert

(** Parse JSON-RPC message ID *)
let parse_message_id json =
  match json with
  | `Int i -> IntId i
  | `String s -> StringId s
  | `Null -> NullId
  | _ -> NullId

(** Parse JSON-RPC request *)
let parse_request json =
  let open Yojson.Basic.Util in
  {
    id = json |> member "id" |> parse_message_id;
    method_name = json |> member "method" |> to_string;
    params = json |> member "params" |> to_option (fun x -> x);
  }

(** Parse JSON-RPC notification *)
let parse_notification json =
  let open Yojson.Basic.Util in
  {
    method_name = json |> member "method" |> to_string;
    params = json |> member "params" |> to_option (fun x -> x);
  }

(** Create JSON-RPC response *)
let create_response id result =
  let id_json = match id with
    | IntId i -> `Int i
    | StringId s -> `String s
    | NullId -> `Null
  in
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id_json);
    ("result", result);
  ]

(** Create JSON-RPC error response *)
let create_error_response id code message =
  let id_json = match id with
    | IntId i -> `Int i
    | StringId s -> `String s
    | NullId -> `Null
  in
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id_json);
    ("error", `Assoc [
      ("code", `Int code);
      ("message", `String message);
    ]);
  ]

(** Create JSON-RPC notification *)
let create_notification method_name params =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String method_name);
    ("params", params);
  ]

(** Send JSON message via stdout *)
let send_message json =
  let content = Yojson.Basic.to_string json in
  let length = String.length content in
  Printf.printf "Content-Length: %d\r\n\r\n%s%!" length content

(** Read LSP message from stdin (Content-Length header + JSON body) *)
let read_message () =
  try
    let content_length = ref None in
    let rec read_headers () =
      let line = input_line stdin in
      if line = "\r" || line = "" then
        (* End of headers *)
        (match !content_length with
         | None -> None
         | Some len ->
           let buf = Bytes.create len in
           really_input stdin buf 0 len;
           Some (Bytes.to_string buf))
      else if String.starts_with ~prefix:"Content-Length: " line then
        let len_str = String.sub line 16 (String.length line - 16) in
        let len = int_of_string (String.trim len_str) in
        content_length := Some len;
        read_headers ()
      else
        read_headers ()
    in
    read_headers ()
  with End_of_file -> None
