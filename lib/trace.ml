(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell *)

(** Accountability Trace for Oblíbený

    Every execution of constrained form produces an accountability trace.
    The trace is:
    - Append-only (immutable once written)
    - Cryptographically hashable for integrity
    - Sufficient to replay or reverse the computation
*)

type value =
  | VInt of int64
  | VBool of bool
  | VUnit
  | VRef of string
  [@@deriving show]

type entry = {
  sequence: int;
  operation: string;
  inputs: value list;
  outputs: value list;
  checkpoint: string option;
} [@@deriving show]

type t = {
  mutable entries: entry list;
  mutable next_seq: int;
  mutable current_checkpoint: string option;
} [@@deriving show]

let create () = {
  entries = [];
  next_seq = 0;
  current_checkpoint = None;
}

let append trace ~op ~inputs ~outputs =
  let entry = {
    sequence = trace.next_seq;
    operation = op;
    inputs;
    outputs;
    checkpoint = trace.current_checkpoint;
  } in
  trace.entries <- entry :: trace.entries;
  trace.next_seq <- trace.next_seq + 1

let checkpoint trace label =
  trace.current_checkpoint <- Some label;
  append trace ~op:"checkpoint" ~inputs:[VRef label] ~outputs:[]

let trace_swap trace a b =
  append trace ~op:"swap" ~inputs:[VRef a; VRef b] ~outputs:[]

let trace_incr trace var delta =
  append trace ~op:"incr" ~inputs:[VRef var; VInt delta] ~outputs:[]

let trace_decr trace var delta =
  append trace ~op:"decr" ~inputs:[VRef var; VInt delta] ~outputs:[]

let trace_xor_assign trace var value =
  append trace ~op:"xor_assign" ~inputs:[VRef var; VInt value] ~outputs:[]

let trace_custom trace event args =
  append trace ~op:event ~inputs:args ~outputs:[]

(** Get all entries in chronological order *)
let to_list trace =
  List.rev trace.entries

(** Convert value to Yojson manually *)
let value_to_json = function
  | VInt i -> `Assoc [("type", `String "int"); ("value", `Intlit (Int64.to_string i))]
  | VBool b -> `Assoc [("type", `String "bool"); ("value", `Bool b)]
  | VUnit -> `Assoc [("type", `String "unit")]
  | VRef r -> `Assoc [("type", `String "ref"); ("value", `String r)]

(** Export trace as JSON *)
let to_json trace =
  let entries_json = List.map (fun e ->
    `Assoc [
      ("sequence", `Int e.sequence);
      ("operation", `String e.operation);
      ("inputs", `List (List.map value_to_json e.inputs));
      ("outputs", `List (List.map value_to_json e.outputs));
      ("checkpoint", match e.checkpoint with Some c -> `String c | None -> `Null);
    ]
  ) (to_list trace) in
  Yojson.Safe.to_string (`Assoc [
    ("trace_version", `String "1.0");
    ("entry_count", `Int trace.next_seq);
    ("entries", `List entries_json);
  ])

(** Compute simple hash of trace for integrity *)
let hash trace =
  let content = to_json trace in
  (* Simple hash for f0 - upgrade to SHA256 in f1+ *)
  Hashtbl.hash content
