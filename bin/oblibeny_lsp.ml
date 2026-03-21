(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell *)

(** Oblibeny Language Server Protocol (LSP) implementation *)

open Oblibeny.Lsp_protocol

(** Document store - maps URI to document content *)
let documents = Hashtbl.create 16

(** Server capabilities *)
let server_capabilities = `Assoc [
  ("textDocumentSync", `Assoc [
    ("openClose", `Bool true);
    ("change", `Int 1);  (* Full document sync *)
  ]);
  ("hoverProvider", `Bool true);
  ("completionProvider", `Assoc [
    ("triggerCharacters", `List [`String "."; `String ":"]);
  ]);
  ("definitionProvider", `Bool false);
  ("referencesProvider", `Bool false);
]

(** Handle initialize request *)
let handle_initialize id params =
  let open Yojson.Basic.Util in
  let _client_info = params |> member "clientInfo" in
  let _root_uri = params |> member "rootUri" in

  let result = `Assoc [
    ("capabilities", server_capabilities);
    ("serverInfo", `Assoc [
      ("name", `String "oblibeny-lsp");
      ("version", `String "0.1.0");
    ]);
  ] in

  create_response id result

(** Handle textDocument/didOpen notification *)
let handle_did_open params =
  let open Yojson.Basic.Util in
  let doc = params |> member "textDocument" in
  let uri = doc |> member "uri" |> to_string in
  let text = doc |> member "text" |> to_string in

  (* Store document *)
  Hashtbl.replace documents uri text;

  (* Get diagnostics and publish *)
  let diagnostics = Oblibeny.Lsp_diagnostics.get_diagnostics uri text in
  let notif = Oblibeny.Lsp_diagnostics.publish_diagnostics uri diagnostics in
  send_message notif

(** Handle textDocument/didChange notification *)
let handle_did_change params =
  let open Yojson.Basic.Util in
  let doc = params |> member "textDocument" in
  let uri = doc |> member "uri" |> to_string in
  let changes = params |> member "contentChanges" |> to_list in

  (* Full document sync - just take the new text *)
  if changes <> [] then
    let change = List.hd changes in
    let text = change |> member "text" |> to_string in

    (* Update document store *)
    Hashtbl.replace documents uri text;

    (* Get diagnostics and publish *)
    let diagnostics = Oblibeny.Lsp_diagnostics.get_diagnostics uri text in
    let notif = Oblibeny.Lsp_diagnostics.publish_diagnostics uri diagnostics in
    send_message notif

(** Handle textDocument/didClose notification *)
let handle_did_close params =
  let open Yojson.Basic.Util in
  let doc = params |> member "textDocument" in
  let uri = doc |> member "uri" |> to_string in

  (* Remove document from store *)
  Hashtbl.remove documents uri;

  (* Clear diagnostics *)
  let notif = Oblibeny.Lsp_diagnostics.publish_diagnostics uri [] in
  send_message notif

(** Handle textDocument/hover request *)
let handle_hover id params =
  let open Yojson.Basic.Util in
  let doc = params |> member "textDocument" in
  let uri = doc |> member "uri" |> to_string in
  let position = params |> member "position" |> position_of_json in

  match Hashtbl.find_opt documents uri with
  | None ->
    create_response id `Null
  | Some text ->
    match Oblibeny.Lsp_hover.get_hover text position with
    | None -> create_response id `Null
    | Some hover -> create_response id (hover_to_json hover)

(** Handle textDocument/completion request *)
let handle_completion id params =
  let open Yojson.Basic.Util in
  let doc = params |> member "textDocument" in
  let uri = doc |> member "uri" |> to_string in
  let position = params |> member "position" |> position_of_json in

  match Hashtbl.find_opt documents uri with
  | None ->
    create_response id (`List [])
  | Some text ->
    let items = Oblibeny.Lsp_completion.get_completions text position in
    let result = Oblibeny.Lsp_completion.completion_response items in
    create_response id result

(** Handle shutdown request *)
let handle_shutdown id =
  create_response id `Null

(** Handle LSP request *)
let handle_request (req : rpc_request) =
  match req.method_name with
  | "initialize" ->
    (match req.params with
     | Some p -> send_message (handle_initialize req.id p)
     | None -> send_message (create_error_response req.id (-32602) "Missing params"))

  | "shutdown" ->
    send_message (handle_shutdown req.id)

  | "textDocument/hover" ->
    (match req.params with
     | Some p -> send_message (handle_hover req.id p)
     | None -> send_message (create_error_response req.id (-32602) "Missing params"))

  | "textDocument/completion" ->
    (match req.params with
     | Some p -> send_message (handle_completion req.id p)
     | None -> send_message (create_error_response req.id (-32602) "Missing params"))

  | _ ->
    send_message (create_error_response req.id (-32601) "Method not found")

(** Handle LSP notification *)
let handle_notification notif =
  match notif.method_name with
  | "initialized" ->
    ()  (* Nothing to do *)

  | "textDocument/didOpen" ->
    (match notif.params with
     | Some p -> handle_did_open p
     | None -> ())

  | "textDocument/didChange" ->
    (match notif.params with
     | Some p -> handle_did_change p
     | None -> ())

  | "textDocument/didClose" ->
    (match notif.params with
     | Some p -> handle_did_close p
     | None -> ())

  | "exit" ->
    exit 0

  | _ ->
    ()  (* Ignore unknown notifications *)

(** Main LSP server loop *)
let main () =
  (* Log to stderr for debugging *)
  prerr_endline "[oblibeny-lsp] Starting Language Server Protocol server...";

  (* Main message loop *)
  let rec loop () =
    match read_message () with
    | None ->
      prerr_endline "[oblibeny-lsp] No more input, shutting down";
      ()
    | Some msg ->
      (try
        let json = Yojson.Basic.from_string msg in
        let open Yojson.Basic.Util in

        (* Check if it's a request or notification *)
        let has_id = match json |> member "id" with
          | `Null -> false
          | _ -> true
        in

        if has_id then
          handle_request (parse_request json)
        else
          handle_notification (parse_notification json);

        loop ()
      with
      | Yojson.Json_error err ->
        prerr_endline ("[oblibeny-lsp] JSON parse error: " ^ err);
        loop ()
      | e ->
        prerr_endline ("[oblibeny-lsp] Unexpected error: " ^ Printexc.to_string e);
        loop ())
  in

  loop ()

let () = main ()
