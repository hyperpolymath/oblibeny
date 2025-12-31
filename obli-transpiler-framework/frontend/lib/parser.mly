/* SPDX-License-Identifier: MIT OR Palimpsest-0.8 */
/* Copyright (c) 2024 Hyperpolymath */

/* Oblibeny Language Parser */

%{
  open Ast
  open Location

  let loc () =
    let startpos = Parsing.symbol_start_pos () in
    let endpos = Parsing.symbol_end_pos () in
    {
      start_pos = { line = startpos.Lexing.pos_lnum;
                    column = startpos.Lexing.pos_cnum - startpos.Lexing.pos_bol;
                    offset = startpos.Lexing.pos_cnum };
      end_pos = { line = endpos.Lexing.pos_lnum;
                  column = endpos.Lexing.pos_cnum - endpos.Lexing.pos_bol;
                  offset = endpos.Lexing.pos_cnum };
      filename = startpos.Lexing.pos_fname;
    }
%}

/* Tokens */
%token <int64> INT_LIT
%token <char> BYTE_LIT
%token <string> IDENT
%token TRUE FALSE

/* Types */
%token INT_T UINT_T BOOL_T BYTE_T UNIT_T ARRAY_T OARRAY_T REF_T

/* Security labels */
%token LOW HIGH

/* Keywords */
%token FN LET MUT IF ELSE WHILE FOR IN RETURN BREAK CONTINUE
%token STRUCT CONST EXTERN IMPORT
%token AND OR NOT
%token OREAD OWRITE CMOV

/* Delimiters */
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token LT GT COMMA SEMI COLON DOT DOTDOT ARROW FAT_ARROW AT

/* Operators */
%token PLUS MINUS STAR SLASH PERCENT
%token EQ EQEQ NEQ LE GE
%token SHL SHR AMP PIPE CARET TILDE BANG
%token AMPAMP PIPEPIPE

%token EOF

/* Precedence (lowest to highest) */
%left PIPEPIPE OR
%left AMPAMP AND
%left PIPE
%left CARET
%left AMP
%left EQEQ NEQ
%left LT LE GT GE
%left SHL SHR
%left PLUS MINUS
%left STAR SLASH PERCENT
%right BANG NOT TILDE UMINUS
%left DOT LBRACK

%start program
%type <Ast.program> program

%%

program:
  | module_header declarations EOF
    { { module_name = $1; declarations = $2 } }
  | declarations EOF
    { { module_name = None; declarations = $1 } }
;

module_header:
  | IMPORT path SEMI { Some (String.concat "." $2) }
;

path:
  | IDENT { [$1] }
  | path DOT IDENT { $1 @ [$3] }
;

declarations:
  | /* empty */ { [] }
  | declaration declarations { $1 :: $2 }
;

declaration:
  | function_decl { $1 }
  | struct_decl { $1 }
  | const_decl { $1 }
  | extern_decl { $1 }
  | import_decl { $1 }
;

attributes:
  | /* empty */ { [] }
  | attribute attributes { $1 :: $2 }
;

attribute:
  | AT IDENT {
      match $2 with
      | "oblivious" -> AOblivious
      | "inline" -> AInline
      | "no_optimize" -> ANoOptimize
      | "constant_time" -> AConstantTime
      | "public" -> APublic
      | name -> ACustom (name, None)
    }
  | AT IDENT LPAREN IDENT RPAREN { ACustom ($2, Some $4) }
;

function_decl:
  | attributes FN IDENT type_params LPAREN params RPAREN ARROW annotated_type block
    { mk_decl (loc ()) (DFunction {
        name = $3;
        type_params = $4;
        params = $6;
        return_type = $9;
        body = $10;
        attributes = $1;
      })
    }
  | attributes FN IDENT type_params LPAREN params RPAREN block
    { mk_decl (loc ()) (DFunction {
        name = $3;
        type_params = $4;
        params = $6;
        return_type = mk_atype (loc ()) Low (TPrim TUnit);
        body = $8;
        attributes = $1;
      })
    }
;

type_params:
  | /* empty */ { [] }
  | LT type_param_list GT { $2 }
;

type_param_list:
  | IDENT { [$1] }
  | IDENT COMMA type_param_list { $1 :: $3 }
;

params:
  | /* empty */ { [] }
  | param_list { $1 }
;

param_list:
  | param { [$1] }
  | param COMMA param_list { $1 :: $3 }
;

param:
  | IDENT COLON annotated_type { ($1, $3) }
;

struct_decl:
  | attributes STRUCT IDENT type_params LBRACE struct_fields RBRACE
    { mk_decl (loc ()) (DStruct {
        name = $3;
        type_params = $4;
        fields = $6;
        attributes = $1;
      })
    }
;

struct_fields:
  | /* empty */ { [] }
  | struct_field struct_fields { $1 :: $2 }
;

struct_field:
  | IDENT COLON annotated_type COMMA { ($1, $3) }
  | IDENT COLON annotated_type { ($1, $3) }
;

const_decl:
  | CONST IDENT COLON annotated_type EQ expr SEMI
    { mk_decl (loc ()) (DConst {
        name = $2;
        typ = $4;
        value = $6;
      })
    }
;

extern_decl:
  | attributes EXTERN FN IDENT LPAREN params RPAREN ARROW annotated_type SEMI
    { mk_decl (loc ()) (DExtern {
        name = $4;
        typ = mk_atype (loc ()) Low (TFun (List.map snd $6 |> List.map (fun at -> at.typ), $9.typ));
        attributes = $1;
      })
    }
;

import_decl:
  | IMPORT path SEMI
    { mk_decl (loc ()) (DImport $2) }
;

annotated_type:
  | security_label typ { mk_atype (loc ()) $1 $2 }
  | typ { mk_atype (loc ()) Low $1 }
;

security_label:
  | AT LOW { Low }
  | AT HIGH { High }
;

typ:
  | prim_type { TPrim $1 }
  | ARRAY_T LT typ GT { TArray ($3, Low) }
  | ARRAY_T LT typ COMMA security_label GT { TArray ($3, $5) }
  | OARRAY_T LT typ GT { TOArray $3 }
  | REF_T LT typ GT { TRef ($3, Low) }
  | REF_T LT typ COMMA security_label GT { TRef ($3, $5) }
  | LPAREN type_list RPAREN ARROW typ { TFun ($2, $5) }
  | IDENT { TStruct $1 }
  | IDENT LT type_args GT { TGeneric ($1, $4) }
;

prim_type:
  | INT_T { TInt None }
  | INT_T LT INT_LIT GT { TInt (Some (Int64.to_int $3)) }
  | UINT_T { TUint None }
  | UINT_T LT INT_LIT GT { TUint (Some (Int64.to_int $3)) }
  | BOOL_T { TBool }
  | BYTE_T { TByte }
  | UNIT_T { TUnit }
;

type_list:
  | /* empty */ { [] }
  | typ { [$1] }
  | typ COMMA type_list { $1 :: $3 }
;

type_args:
  | typ { [$1] }
  | typ COMMA type_args { $1 :: $3 }
;

block:
  | LBRACE statements RBRACE { $2 }
;

statements:
  | /* empty */ { [] }
  | statement statements { $1 :: $2 }
;

statement:
  | LET pattern type_annotation EQ expr SEMI
    { mk_stmt (loc ()) (SLet ($2, $3, $5)) }
  | lvalue EQ expr SEMI
    { mk_stmt (loc ()) (SAssign ($1, $3)) }
  | OWRITE LPAREN expr COMMA expr COMMA expr RPAREN SEMI
    { mk_stmt (loc ()) (SOramWrite ($3, $5, $7)) }
  | expr SEMI
    { mk_stmt (loc ()) (SExpr $1) }
  | IF expr block
    { mk_stmt (loc ()) (SIf ($2, $3, [])) }
  | IF expr block ELSE block
    { mk_stmt (loc ()) (SIf ($2, $3, $5)) }
  | IF expr block ELSE statement
    { mk_stmt (loc ()) (SIf ($2, $3, [$5])) }
  | WHILE expr block
    { mk_stmt (loc ()) (SWhile ($2, $3)) }
  | FOR IDENT IN expr DOTDOT expr block
    { mk_stmt (loc ()) (SFor ($2, $4, $6, $7)) }
  | RETURN SEMI
    { mk_stmt (loc ()) (SReturn None) }
  | RETURN expr SEMI
    { mk_stmt (loc ()) (SReturn (Some $2)) }
  | BREAK SEMI
    { mk_stmt (loc ()) SBreak }
  | CONTINUE SEMI
    { mk_stmt (loc ()) SContinue }
;

type_annotation:
  | /* empty */ { None }
  | COLON annotated_type { Some $2 }
;

pattern:
  | IDENT { PVar $1 }
  | LPAREN pattern_list RPAREN { PTuple $2 }
;

pattern_list:
  | pattern { [$1] }
  | pattern COMMA pattern_list { $1 :: $3 }
;

lvalue:
  | IDENT { mk_expr (loc ()) (EVar $1) }
  | lvalue DOT IDENT { mk_expr (loc ()) (EField ($1, $3)) }
  | lvalue LBRACK expr RBRACK { mk_expr (loc ()) (EIndex ($1, $3)) }
;

expr:
  | expr_or { $1 }
;

expr_or:
  | expr_and { $1 }
  | expr_or PIPEPIPE expr_and { mk_expr (loc ()) (EBinop (Or, $1, $3)) }
  | expr_or OR expr_and { mk_expr (loc ()) (EBinop (Or, $1, $3)) }
;

expr_and:
  | expr_bitor { $1 }
  | expr_and AMPAMP expr_bitor { mk_expr (loc ()) (EBinop (And, $1, $3)) }
  | expr_and AND expr_bitor { mk_expr (loc ()) (EBinop (And, $1, $3)) }
;

expr_bitor:
  | expr_bitxor { $1 }
  | expr_bitor PIPE expr_bitxor { mk_expr (loc ()) (EBinop (BitOr, $1, $3)) }
;

expr_bitxor:
  | expr_bitand { $1 }
  | expr_bitxor CARET expr_bitand { mk_expr (loc ()) (EBinop (BitXor, $1, $3)) }
;

expr_bitand:
  | expr_eq { $1 }
  | expr_bitand AMP expr_eq { mk_expr (loc ()) (EBinop (BitAnd, $1, $3)) }
;

expr_eq:
  | expr_cmp { $1 }
  | expr_eq EQEQ expr_cmp { mk_expr (loc ()) (EBinop (Eq, $1, $3)) }
  | expr_eq NEQ expr_cmp { mk_expr (loc ()) (EBinop (Neq, $1, $3)) }
;

expr_cmp:
  | expr_shift { $1 }
  | expr_cmp LT expr_shift { mk_expr (loc ()) (EBinop (Lt, $1, $3)) }
  | expr_cmp LE expr_shift { mk_expr (loc ()) (EBinop (Le, $1, $3)) }
  | expr_cmp GT expr_shift { mk_expr (loc ()) (EBinop (Gt, $1, $3)) }
  | expr_cmp GE expr_shift { mk_expr (loc ()) (EBinop (Ge, $1, $3)) }
;

expr_shift:
  | expr_add { $1 }
  | expr_shift SHL expr_add { mk_expr (loc ()) (EBinop (Shl, $1, $3)) }
  | expr_shift SHR expr_add { mk_expr (loc ()) (EBinop (Shr, $1, $3)) }
;

expr_add:
  | expr_mul { $1 }
  | expr_add PLUS expr_mul { mk_expr (loc ()) (EBinop (Add, $1, $3)) }
  | expr_add MINUS expr_mul { mk_expr (loc ()) (EBinop (Sub, $1, $3)) }
;

expr_mul:
  | expr_unary { $1 }
  | expr_mul STAR expr_unary { mk_expr (loc ()) (EBinop (Mul, $1, $3)) }
  | expr_mul SLASH expr_unary { mk_expr (loc ()) (EBinop (Div, $1, $3)) }
  | expr_mul PERCENT expr_unary { mk_expr (loc ()) (EBinop (Mod, $1, $3)) }
;

expr_unary:
  | expr_postfix { $1 }
  | MINUS expr_unary %prec UMINUS { mk_expr (loc ()) (EUnop (Neg, $2)) }
  | BANG expr_unary { mk_expr (loc ()) (EUnop (Not, $2)) }
  | NOT expr_unary { mk_expr (loc ()) (EUnop (Not, $2)) }
  | TILDE expr_unary { mk_expr (loc ()) (EUnop (BitNot, $2)) }
;

expr_postfix:
  | expr_primary { $1 }
  | expr_postfix DOT IDENT { mk_expr (loc ()) (EField ($1, $3)) }
  | expr_postfix LBRACK expr RBRACK { mk_expr (loc ()) (EIndex ($1, $3)) }
  | expr_postfix LPAREN args RPAREN { mk_expr (loc ()) (ECall ($1, $3)) }
;

expr_primary:
  | literal { mk_expr (loc ()) (ELiteral $1) }
  | IDENT { mk_expr (loc ()) (EVar $1) }
  | LPAREN expr RPAREN { $2 }
  | LPAREN expr COMMA expr_list RPAREN { mk_expr (loc ()) (ETuple ($2 :: $4)) }
  | LBRACE statements expr RBRACE { mk_expr (loc ()) (EBlock ($2, Some $3)) }
  | LBRACE statements RBRACE { mk_expr (loc ()) (EBlock ($2, None)) }
  | IF expr LBRACE expr RBRACE ELSE LBRACE expr RBRACE
    { mk_expr (loc ()) (EIf ($2, $4, $8)) }
  | OREAD LPAREN expr COMMA expr RPAREN { mk_expr (loc ()) (EOramRead ($3, $5)) }
  | CMOV LPAREN expr COMMA expr COMMA expr RPAREN { mk_expr (loc ()) (ECmov ($3, $5, $7)) }
  | IDENT LBRACE field_inits RBRACE { mk_expr (loc ()) (EStruct ($1, $3)) }
  | FN LPAREN lambda_params RPAREN FAT_ARROW expr
    { mk_expr (loc ()) (ELambda ($3, $6)) }
;

literal:
  | INT_LIT { LInt $1 }
  | TRUE { LBool true }
  | FALSE { LBool false }
  | BYTE_LIT { LByte $1 }
  | LPAREN RPAREN { LUnit }
;

args:
  | /* empty */ { [] }
  | arg_list { $1 }
;

arg_list:
  | expr { [$1] }
  | expr COMMA arg_list { $1 :: $3 }
;

expr_list:
  | expr { [$1] }
  | expr COMMA expr_list { $1 :: $3 }
;

field_inits:
  | /* empty */ { [] }
  | field_init_list { $1 }
;

field_init_list:
  | field_init { [$1] }
  | field_init COMMA field_init_list { $1 :: $3 }
;

field_init:
  | IDENT COLON expr { ($1, $3) }
;

lambda_params:
  | /* empty */ { [] }
  | lambda_param_list { $1 }
;

lambda_param_list:
  | lambda_param { [$1] }
  | lambda_param COMMA lambda_param_list { $1 :: $3 }
;

lambda_param:
  | IDENT COLON annotated_type { ($1, $3) }
;

%%
