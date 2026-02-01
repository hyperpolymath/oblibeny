(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell *)

(** Lexer for Oblíbený Constrained Form

    Tokens are defined to match the Rust-like syntax of constrained form.
    Token types are defined in parser.mly and exposed via Parser module.
*)

(** Lexer state *)
type lexbuf = {
  source: string;
  mutable pos: int;
  mutable line: int;
  mutable col: int;
}

let create_lexbuf source = {
  source;
  pos = 0;
  line = 1;
  col = 0;
}

(** Get current location *)
let current_loc lexbuf =
  let pos = {
    Location.pos_line = lexbuf.line;
    Location.pos_col = lexbuf.col;
    Location.pos_offset = lexbuf.pos;
  } in
  {
    Location.loc_start = pos;
    Location.loc_end = pos;
    Location.loc_file = "<input>";
  }

(** Keyword table *)
let keywords : (string, Parser.token) Hashtbl.t = Hashtbl.create 32
let () = List.iter (fun (kw, tok) -> Hashtbl.add keywords kw tok) [
  (* Control flow *)
  ("let", Parser.KW_LET);
  ("mut", Parser.KW_MUT);
  ("if", Parser.KW_IF);
  ("else", Parser.KW_ELSE);
  ("for", Parser.KW_FOR);
  ("in", Parser.KW_IN);
  ("fn", Parser.KW_FN);
  ("return", Parser.KW_RETURN);
  ("match", Parser.KW_MATCH);
  ("true", Parser.KW_TRUE);
  ("false", Parser.KW_FALSE);
  ("struct", Parser.KW_STRUCT);
  ("const", Parser.KW_CONST);

  (* Reversibility primitives *)
  ("swap", Parser.KW_SWAP);
  ("incr", Parser.KW_INCR);
  ("decr", Parser.KW_DECR);

  (* Trace operations *)
  ("trace", Parser.KW_TRACE);
  ("checkpoint", Parser.KW_CHECKPOINT);
  ("assert_invariant", Parser.KW_ASSERT_INVARIANT);

  (* Types *)
  ("i32", Parser.KW_I32);
  ("i64", Parser.KW_I64);
  ("u32", Parser.KW_U32);
  ("u64", Parser.KW_U64);
  ("bool", Parser.KW_BOOL);
]

(** Lex error exception *)
exception LexError of string * Location.t

(** Helper: peek character at current position *)
let peek lexbuf =
  if lexbuf.pos >= String.length lexbuf.source then
    None
  else
    Some (String.get lexbuf.source lexbuf.pos)

(** Helper: advance position *)
let advance lexbuf =
  match peek lexbuf with
  | Some '\n' ->
    lexbuf.pos <- lexbuf.pos + 1;
    lexbuf.line <- lexbuf.line + 1;
    lexbuf.col <- 0
  | Some _ ->
    lexbuf.pos <- lexbuf.pos + 1;
    lexbuf.col <- lexbuf.col + 1
  | None -> ()

(** Helper: skip whitespace *)
let rec skip_whitespace lexbuf =
  match peek lexbuf with
  | Some (' ' | '\t' | '\n' | '\r') ->
    advance lexbuf;
    skip_whitespace lexbuf
  | _ -> ()

(** Helper: skip single-line comment *)
let rec skip_line_comment lexbuf =
  match peek lexbuf with
  | Some '\n' | None -> ()
  | Some _ ->
    advance lexbuf;
    skip_line_comment lexbuf

(** Helper: skip block comment *)
let rec skip_block_comment lexbuf depth =
  match peek lexbuf with
  | None ->
    raise (LexError ("unclosed block comment", current_loc lexbuf))
  | Some '*' ->
    advance lexbuf;
    (match peek lexbuf with
     | Some '/' ->
       advance lexbuf;
       if depth = 1 then ()
       else skip_block_comment lexbuf (depth - 1)
     | _ -> skip_block_comment lexbuf depth)
  | Some '/' ->
    advance lexbuf;
    (match peek lexbuf with
     | Some '*' ->
       advance lexbuf;
       skip_block_comment lexbuf (depth + 1)
     | _ -> skip_block_comment lexbuf depth)
  | Some _ ->
    advance lexbuf;
    skip_block_comment lexbuf depth

(** Helper: skip whitespace and comments *)
let rec skip_ws_and_comments lexbuf =
  skip_whitespace lexbuf;
  match peek lexbuf with
  | Some '/' ->
    let saved_pos = lexbuf.pos in
    advance lexbuf;
    (match peek lexbuf with
     | Some '/' ->
       advance lexbuf;
       skip_line_comment lexbuf;
       skip_ws_and_comments lexbuf
     | Some '*' ->
       advance lexbuf;
       skip_block_comment lexbuf 1;
       skip_ws_and_comments lexbuf
     | _ ->
       lexbuf.pos <- saved_pos;
       lexbuf.col <- lexbuf.col - 1)
  | _ -> ()

(** Helper: check if character is identifier start *)
let is_ident_start = function
  | 'a'..'z' | 'A'..'Z' | '_' -> true
  | _ -> false

(** Helper: check if character is identifier continue *)
let is_ident_cont = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
  | _ -> false

(** Helper: check if character is digit *)
let is_digit = function
  | '0'..'9' -> true
  | _ -> false

(** Lex identifier or keyword *)
let lex_ident lexbuf =
  let start_pos = lexbuf.pos in
  let rec scan () =
    match peek lexbuf with
    | Some c when is_ident_cont c ->
      advance lexbuf;
      scan ()
    | _ -> ()
  in
  scan ();
  let text = String.sub lexbuf.source start_pos (lexbuf.pos - start_pos) in
  match Hashtbl.find_opt keywords text with
  | Some tok -> tok
  | None -> IDENT text

(** Lex integer literal *)
let lex_int lexbuf =
  let start_pos = lexbuf.pos in
  let rec scan () =
    match peek lexbuf with
    | Some c when is_digit c ->
      advance lexbuf;
      scan ()
    | _ -> ()
  in
  scan ();
  let text = String.sub lexbuf.source start_pos (lexbuf.pos - start_pos) in
  try
    Parser.INT (Int64.of_string text)
  with Failure _ ->
    raise (LexError ("invalid integer literal: " ^ text, current_loc lexbuf))

(** Lex string literal *)
let lex_string lexbuf =
  advance lexbuf; (* skip opening quote *)
  let buf = Buffer.create 16 in
  let rec scan () =
    match peek lexbuf with
    | None ->
      raise (LexError ("unclosed string literal", current_loc lexbuf))
    | Some '"' ->
      advance lexbuf
    | Some '\\' ->
      advance lexbuf;
      (match peek lexbuf with
       | Some 'n' -> Buffer.add_char buf '\n'; advance lexbuf; scan ()
       | Some 't' -> Buffer.add_char buf '\t'; advance lexbuf; scan ()
       | Some '\\' -> Buffer.add_char buf '\\'; advance lexbuf; scan ()
       | Some '"' -> Buffer.add_char buf '"'; advance lexbuf; scan ()
       | Some c ->
         raise (LexError (Printf.sprintf "invalid escape sequence: \\%c" c, current_loc lexbuf))
       | None ->
         raise (LexError ("unclosed string literal", current_loc lexbuf)))
    | Some c ->
      Buffer.add_char buf c;
      advance lexbuf;
      scan ()
  in
  scan ();
  Parser.STRING (Buffer.contents buf)

(** Lex next token *)
let next_token lexbuf =
  skip_ws_and_comments lexbuf;

  let loc = current_loc lexbuf in

  match peek lexbuf with
  | None -> Parser.EOF

  (* Single character tokens *)
  | Some '(' -> advance lexbuf; LPAREN
  | Some ')' -> advance lexbuf; RPAREN
  | Some '{' -> advance lexbuf; LBRACE
  | Some '}' -> advance lexbuf; RBRACE
  | Some '[' -> advance lexbuf; LBRACK
  | Some ']' -> advance lexbuf; RBRACK
  | Some ',' -> advance lexbuf; COMMA
  | Some ':' -> advance lexbuf; COLON
  | Some ';' -> advance lexbuf; SEMICOLON
  | Some '~' -> advance lexbuf; BIT_NOT

  (* Multi-character operators *)
  | Some '+' -> advance lexbuf; PLUS
  | Some '*' -> advance lexbuf; STAR
  | Some '%' -> advance lexbuf; PERCENT

  | Some '-' ->
    advance lexbuf;
    (match peek lexbuf with
     | Some '>' -> advance lexbuf; ARROW
     | _ -> MINUS)

  | Some '/' ->
    advance lexbuf; SLASH

  | Some '=' ->
    advance lexbuf;
    (match peek lexbuf with
     | Some '=' -> advance lexbuf; EQ
     | Some '>' -> advance lexbuf; FAT_ARROW
     | _ -> ASSIGN)

  | Some '!' ->
    advance lexbuf;
    (match peek lexbuf with
     | Some '=' -> advance lexbuf; NEQ
     | _ -> NOT)

  | Some '<' ->
    advance lexbuf;
    (match peek lexbuf with
     | Some '=' -> advance lexbuf; LE
     | _ -> LT)

  | Some '>' ->
    advance lexbuf;
    (match peek lexbuf with
     | Some '=' -> advance lexbuf; GE
     | _ -> GT)

  | Some '&' ->
    advance lexbuf;
    (match peek lexbuf with
     | Some '&' -> advance lexbuf; AND
     | _ -> BIT_AND)

  | Some '|' ->
    advance lexbuf;
    (match peek lexbuf with
     | Some '|' -> advance lexbuf; OR
     | _ -> BIT_OR)

  | Some '^' ->
    advance lexbuf;
    (match peek lexbuf with
     | Some '=' -> advance lexbuf; XOR_ASSIGN
     | _ -> BIT_XOR)

  | Some '.' ->
    advance lexbuf;
    (match peek lexbuf with
     | Some '.' -> advance lexbuf; DOT_DOT
     | _ -> DOT)

  (* String literal *)
  | Some '"' -> lex_string lexbuf

  (* Integer literal *)
  | Some c when is_digit c -> lex_int lexbuf

  (* Identifier or keyword *)
  | Some c when is_ident_start c -> lex_ident lexbuf

  (* Unknown character *)
  | Some c ->
    raise (LexError (Printf.sprintf "unexpected character: '%c'" c, loc))

(** Tokenize entire input *)
let tokenize source =
  let lexbuf = create_lexbuf source in
  let rec loop acc =
    match next_token lexbuf with
    | Parser.EOF -> List.rev (Parser.EOF :: acc)
    | tok -> loop (tok :: acc)
  in
  loop []
