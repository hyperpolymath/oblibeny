/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* Copyright (c) 2026 Jonathan D.A. Jewell */

%{
open Ast

let dummy_loc = Location.dummy
%}

/* Tokens */

/* Keywords */
%token KW_LET KW_MUT KW_IF KW_ELSE KW_FOR KW_IN KW_FN KW_RETURN KW_MATCH
%token KW_TRUE KW_FALSE KW_STRUCT KW_CONST

/* Reversibility primitives */
%token KW_SWAP KW_INCR KW_DECR

/* Trace operations */
%token KW_TRACE KW_CHECKPOINT KW_ASSERT_INVARIANT

/* Type keywords */
%token KW_I32 KW_I64 KW_U32 KW_U64 KW_BOOL

/* Operators */
%token PLUS MINUS STAR SLASH PERCENT
%token EQ NEQ LT LE GT GE
%token AND OR NOT
%token BIT_AND BIT_OR BIT_XOR BIT_NOT
%token ASSIGN XOR_ASSIGN
%token ARROW FAT_ARROW DOT_DOT

/* Delimiters */
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token COMMA COLON SEMICOLON DOT

/* Literals and identifiers */
%token <int64> INT
%token <string> IDENT
%token <string> STRING

%token EOF

/* Operator precedence and associativity */
%left OR
%left AND
%left BIT_OR
%left BIT_XOR
%left BIT_AND
%left EQ NEQ
%left LT LE GT GE
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc NOT BIT_NOT
%nonassoc UMINUS

/* Start symbol */
%start <Ast.program> program

%%

/* ========== PROGRAM ========== */

program:
  | decls = list(decl) EOF { { module_name = None; declarations = decls } }
  ;

decl:
  | func = function_decl { func }
  | st = struct_decl { st }
  | c = const_decl { c }
  ;

/* ========== FUNCTION DECLARATION ========== */

function_decl:
  | KW_FN name = IDENT LPAREN params = separated_list(COMMA, param) RPAREN
    ARROW ret = typ body = block
    {
      mk_decl dummy_loc (DFunction {
        name;
        params;
        return_type = ret;
        body;
      })
    }
  ;

param:
  | name = IDENT COLON t = typ { (name, t) }
  ;

/* ========== STRUCT DECLARATION ========== */

struct_decl:
  | KW_STRUCT name = IDENT LBRACE fields = separated_list(COMMA, field) RBRACE
    {
      mk_decl dummy_loc (DStruct {
        name;
        fields;
      })
    }
  ;

field:
  | name = IDENT COLON t = typ { (name, t) }
  ;

/* ========== CONST DECLARATION ========== */

const_decl:
  | KW_CONST name = IDENT COLON t = typ ASSIGN e = expr SEMICOLON
    {
      mk_decl dummy_loc (DConst {
        name;
        typ = t;
        value = e;
      })
    }
  ;

/* ========== TYPES ========== */

typ:
  | KW_I32 { TPrim TI32 }
  | KW_I64 { TPrim TI64 }
  | KW_U32 { TPrim TU32 }
  | KW_U64 { TPrim TU64 }
  | KW_BOOL { TPrim TBool }
  | LPAREN RPAREN { TPrim TUnit }
  | LBRACK t = typ RBRACK { TArray (t, None) }
  | LBRACK t = typ SEMICOLON n = INT RBRACK { TArray (t, Some (Int64.to_int n)) }
  | name = IDENT { TStruct name }
  ;

/* ========== STATEMENTS ========== */

block:
  | LBRACE stmts = list(stmt) RBRACE { stmts }
  ;

stmt:
  | s = simple_stmt SEMICOLON { s }
  | s = control_stmt { s }
  | s = reversible_stmt SEMICOLON { s }
  | s = trace_stmt SEMICOLON { s }
  ;

simple_stmt:
  | KW_LET name = IDENT t = option(type_annotation) ASSIGN e = expr
    { mk_stmt dummy_loc (SLet (name, t, e)) }
  | KW_LET KW_MUT name = IDENT t = option(type_annotation) ASSIGN e = expr
    { mk_stmt dummy_loc (SLetMut (name, t, e)) }
  | name = IDENT ASSIGN e = expr
    { mk_stmt dummy_loc (SAssign (name, e)) }
  | KW_RETURN e = option(expr)
    { mk_stmt dummy_loc (SReturn e) }
  | e = expr
    { mk_stmt dummy_loc (SExpr e) }
  ;

type_annotation:
  | COLON t = typ { t }
  ;

control_stmt:
  | KW_IF cond = expr then_block = block KW_ELSE else_block = block
    { mk_stmt dummy_loc (SIf (cond, then_block, else_block)) }
  | KW_IF cond = expr then_block = block
    { mk_stmt dummy_loc (SIf (cond, then_block, [])) }
  | KW_FOR var = IDENT KW_IN start = INT DOT_DOT end_ = INT body = block
    { mk_stmt dummy_loc (SForRange (var, start, end_, body)) }
  ;

reversible_stmt:
  | KW_SWAP LPAREN a = IDENT COMMA b = IDENT RPAREN
    { mk_stmt dummy_loc (SSwap (a, b)) }
  | KW_INCR LPAREN x = IDENT COMMA delta = expr RPAREN
    { mk_stmt dummy_loc (SIncr (x, delta)) }
  | KW_DECR LPAREN x = IDENT COMMA delta = expr RPAREN
    { mk_stmt dummy_loc (SDecr (x, delta)) }
  | x = IDENT XOR_ASSIGN e = expr
    { mk_stmt dummy_loc (SXorAssign (x, e)) }
  ;

trace_stmt:
  | KW_TRACE LPAREN event = STRING args = preceded(COMMA, expr)* RPAREN
    { mk_stmt dummy_loc (STrace (event, args)) }
  | KW_CHECKPOINT LPAREN label = STRING RPAREN
    { mk_stmt dummy_loc (SCheckpoint label) }
  | KW_ASSERT_INVARIANT LPAREN cond = expr COMMA msg = STRING RPAREN
    { mk_stmt dummy_loc (SAssertInvariant (cond, msg)) }
  ;

/* ========== EXPRESSIONS ========== */

expr:
  | e = primary_expr { e }
  | e = binary_expr { e }
  | e = unary_expr { e }
  | e = if_expr { e }
  | e = block_expr { e }
  ;

primary_expr:
  | n = INT { mk_expr dummy_loc (ELiteral (LInt n)) }
  | KW_TRUE { mk_expr dummy_loc (ELiteral (LBool true)) }
  | KW_FALSE { mk_expr dummy_loc (ELiteral (LBool false)) }
  | LPAREN RPAREN { mk_expr dummy_loc (ELiteral LUnit) }
  | name = IDENT { mk_expr dummy_loc (EVar name) }
  | name = IDENT LPAREN args = separated_list(COMMA, expr) RPAREN
    { mk_expr dummy_loc (ECall (name, args)) }
  | e = expr LBRACK idx = expr RBRACK
    { mk_expr dummy_loc (EIndex (e, idx)) }
  | e = expr DOT field = IDENT
    { mk_expr dummy_loc (EField (e, field)) }
  | LPAREN e = expr RPAREN { e }
  | name = IDENT LBRACE fields = separated_list(COMMA, struct_field) RBRACE
    { mk_expr dummy_loc (EStruct (name, fields)) }
  ;

struct_field:
  | name = IDENT COLON e = expr { (name, e) }
  ;

binary_expr:
  | e1 = expr PLUS e2 = expr { mk_expr dummy_loc (EBinop (Add, e1, e2)) }
  | e1 = expr MINUS e2 = expr { mk_expr dummy_loc (EBinop (Sub, e1, e2)) }
  | e1 = expr STAR e2 = expr { mk_expr dummy_loc (EBinop (Mul, e1, e2)) }
  | e1 = expr SLASH e2 = expr { mk_expr dummy_loc (EBinop (Div, e1, e2)) }
  | e1 = expr PERCENT e2 = expr { mk_expr dummy_loc (EBinop (Mod, e1, e2)) }
  | e1 = expr EQ e2 = expr { mk_expr dummy_loc (EBinop (Eq, e1, e2)) }
  | e1 = expr NEQ e2 = expr { mk_expr dummy_loc (EBinop (Neq, e1, e2)) }
  | e1 = expr LT e2 = expr { mk_expr dummy_loc (EBinop (Lt, e1, e2)) }
  | e1 = expr LE e2 = expr { mk_expr dummy_loc (EBinop (Le, e1, e2)) }
  | e1 = expr GT e2 = expr { mk_expr dummy_loc (EBinop (Gt, e1, e2)) }
  | e1 = expr GE e2 = expr { mk_expr dummy_loc (EBinop (Ge, e1, e2)) }
  | e1 = expr AND e2 = expr { mk_expr dummy_loc (EBinop (And, e1, e2)) }
  | e1 = expr OR e2 = expr { mk_expr dummy_loc (EBinop (Or, e1, e2)) }
  | e1 = expr BIT_AND e2 = expr { mk_expr dummy_loc (EBinop (BitAnd, e1, e2)) }
  | e1 = expr BIT_OR e2 = expr { mk_expr dummy_loc (EBinop (BitOr, e1, e2)) }
  | e1 = expr BIT_XOR e2 = expr { mk_expr dummy_loc (EBinop (BitXor, e1, e2)) }
  ;

unary_expr:
  | MINUS e = expr %prec UMINUS { mk_expr dummy_loc (EUnop (Neg, e)) }
  | NOT e = expr { mk_expr dummy_loc (EUnop (Not, e)) }
  | BIT_NOT e = expr { mk_expr dummy_loc (EUnop (BitNot, e)) }
  ;

if_expr:
  | KW_IF cond = expr then_e = block_expr KW_ELSE else_e = block_expr
    { mk_expr dummy_loc (EIf (cond, then_e, else_e)) }
  | KW_IF cond = expr then_e = block_expr KW_ELSE else_e = if_expr
    { mk_expr dummy_loc (EIf (cond, then_e, else_e)) }
  ;

block_expr:
  | LBRACE stmts = list(stmt) e = option(expr) RBRACE
    { mk_expr dummy_loc (EBlock (stmts, e)) }
  ;
