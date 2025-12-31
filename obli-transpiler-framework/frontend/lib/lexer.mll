(* SPDX-License-Identifier: MIT OR Palimpsest-0.8 *)
(* Copyright (c) 2024 Hyperpolymath *)

{
  open Parser

  exception Lexer_error of string * Lexing.position

  let keywords = Hashtbl.create 50
  let () = List.iter (fun (kw, tok) -> Hashtbl.add keywords kw tok) [
    (* Types *)
    ("int", INT_T);
    ("uint", UINT_T);
    ("bool", BOOL_T);
    ("byte", BYTE_T);
    ("unit", UNIT_T);
    ("array", ARRAY_T);
    ("oarray", OARRAY_T);
    ("ref", REF_T);

    (* Security labels *)
    ("low", LOW);
    ("high", HIGH);

    (* Keywords *)
    ("fn", FN);
    ("let", LET);
    ("mut", MUT);
    ("if", IF);
    ("else", ELSE);
    ("while", WHILE);
    ("for", FOR);
    ("in", IN);
    ("return", RETURN);
    ("break", BREAK);
    ("continue", CONTINUE);
    ("struct", STRUCT);
    ("const", CONST);
    ("extern", EXTERN);
    ("import", IMPORT);
    ("true", TRUE);
    ("false", FALSE);
    ("and", AND);
    ("or", OR);
    ("not", NOT);

    (* ORAM operations *)
    ("oread", OREAD);
    ("owrite", OWRITE);
    ("cmov", CMOV);
  ]

  let newline lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

let digit = ['0'-'9']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let alpha = ['a'-'z' 'A'-'Z']
let ident_start = alpha | '_'
let ident_char = alpha | digit | '_'

let integer = digit+
let hex_integer = "0x" hex_digit+
let identifier = ident_start ident_char*

let whitespace = [' ' '\t']+
let newline = '\r'? '\n'

rule token = parse
  | whitespace { token lexbuf }
  | newline    { newline lexbuf; token lexbuf }

  (* Comments *)
  | "//" [^ '\n']* { token lexbuf }
  | "/*"           { block_comment lexbuf; token lexbuf }

  (* Delimiters *)
  | '('  { LPAREN }
  | ')'  { RPAREN }
  | '{'  { LBRACE }
  | '}'  { RBRACE }
  | '['  { LBRACK }
  | ']'  { RBRACK }
  | '<'  { LT }
  | '>'  { GT }
  | ','  { COMMA }
  | ';'  { SEMI }
  | ':'  { COLON }
  | '.'  { DOT }
  | ".." { DOTDOT }
  | "->" { ARROW }
  | "=>" { FAT_ARROW }
  | '@'  { AT }

  (* Operators *)
  | '+'  { PLUS }
  | '-'  { MINUS }
  | '*'  { STAR }
  | '/'  { SLASH }
  | '%'  { PERCENT }
  | '='  { EQ }
  | "==" { EQEQ }
  | "!=" { NEQ }
  | "<=" { LE }
  | ">=" { GE }
  | "<<" { SHL }
  | ">>" { SHR }
  | '&'  { AMP }
  | '|'  { PIPE }
  | '^'  { CARET }
  | '~'  { TILDE }
  | '!'  { BANG }
  | "&&" { AMPAMP }
  | "||" { PIPEPIPE }

  (* Literals *)
  | integer as n     { INT_LIT (Int64.of_string n) }
  | hex_integer as n { INT_LIT (Int64.of_string n) }
  | "0b" (['0' '1']+ as n) { INT_LIT (Int64.of_string ("0b" ^ n)) }

  (* Byte literals *)
  | '\'' ([^ '\\' '\''] as c) '\'' { BYTE_LIT c }
  | "'\\" (['n' 't' 'r' '\\' '\''] as c) '\'' {
      let c' = match c with
        | 'n' -> '\n'
        | 't' -> '\t'
        | 'r' -> '\r'
        | '\\' -> '\\'
        | '\'' -> '\''
        | _ -> assert false
      in BYTE_LIT c'
    }

  (* Identifiers and keywords *)
  | identifier as id {
      try Hashtbl.find keywords id
      with Not_found -> IDENT id
    }

  | eof { EOF }

  | _ as c {
      raise (Lexer_error (Printf.sprintf "Unexpected character: %c" c, lexbuf.Lexing.lex_curr_p))
    }

and block_comment = parse
  | "*/"    { () }
  | newline { newline lexbuf; block_comment lexbuf }
  | _       { block_comment lexbuf }
  | eof     { raise (Lexer_error ("Unterminated block comment", lexbuf.Lexing.lex_curr_p)) }
