(* SPDX-License-Identifier: MIT OR Palimpsest-0.8 *)
(* Copyright (c) 2024 Hyperpolymath *)

(** Source location tracking *)

type position = {
  line: int;
  column: int;
  offset: int;
} [@@deriving show, yojson]

type t = {
  start_pos: position;
  end_pos: position;
  filename: string;
} [@@deriving show, yojson]

let dummy = {
  start_pos = { line = 0; column = 0; offset = 0 };
  end_pos = { line = 0; column = 0; offset = 0 };
  filename = "<unknown>";
}

let make ~filename ~start_line ~start_col ~end_line ~end_col = {
  start_pos = { line = start_line; column = start_col; offset = 0 };
  end_pos = { line = end_line; column = end_col; offset = 0 };
  filename;
}

let from_lexbuf filename lexbuf =
  let open Lexing in
  let start_p = lexbuf.lex_start_p in
  let end_p = lexbuf.lex_curr_p in
  {
    start_pos = {
      line = start_p.pos_lnum;
      column = start_p.pos_cnum - start_p.pos_bol;
      offset = start_p.pos_cnum;
    };
    end_pos = {
      line = end_p.pos_lnum;
      column = end_p.pos_cnum - end_p.pos_bol;
      offset = end_p.pos_cnum;
    };
    filename;
  }

let merge loc1 loc2 = {
  start_pos = loc1.start_pos;
  end_pos = loc2.end_pos;
  filename = loc1.filename;
}

let to_string loc =
  Printf.sprintf "%s:%d:%d-%d:%d"
    loc.filename
    loc.start_pos.line loc.start_pos.column
    loc.end_pos.line loc.end_pos.column
