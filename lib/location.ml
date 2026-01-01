(* SPDX-License-Identifier: MIT OR Palimpsest-0.8 *)
(* Copyright (c) 2026 Hyperpolymath *)

(** Source location tracking for Oblíbený *)

type position = {
  pos_line: int;
  pos_col: int;
  pos_offset: int;
} [@@deriving show, yojson]

type t = {
  loc_start: position;
  loc_end: position;
  loc_file: string;
} [@@deriving show, yojson]

let dummy_pos = { pos_line = 0; pos_col = 0; pos_offset = 0 }
let dummy = { loc_start = dummy_pos; loc_end = dummy_pos; loc_file = "<none>" }

let make ~file ~start_line ~start_col ~end_line ~end_col =
  { loc_start = { pos_line = start_line; pos_col = start_col; pos_offset = 0 };
    loc_end = { pos_line = end_line; pos_col = end_col; pos_offset = 0 };
    loc_file = file }

let to_string loc =
  Printf.sprintf "%s:%d:%d" loc.loc_file loc.loc_start.pos_line loc.loc_start.pos_col
