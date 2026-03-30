(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> *)

(** S-expression and JSON AST dump for Oblibeny.

    Covers every AST node type in {!Ast}:
    - {!Ast.program}, {!Ast.decl} (DFunction, DStruct, DConst, DError)
    - {!Ast.stmt} (SLet, SLetMut, SAssign, SIf, SForRange, SExpr, SReturn,
      SSwap, SIncr, SDecr, SXorAssign, STrace, SCheckpoint, SAssertInvariant, SError)
    - {!Ast.expr} (ELiteral, EVar, EBinop, EUnop, ECall, EIndex, EField,
      EIf, EBlock, EStruct, EMatch)
    - {!Ast.pattern} (PWild, PVar, PLiteral)
    - {!Ast.typ}, {!Ast.prim_type}, {!Ast.literal}, {!Ast.binop}, {!Ast.unop}
*)

open Ast

(* ======================================================================
   S-EXPRESSION OUTPUT
   ====================================================================== *)

(** Produce a string of [n] spaces for indentation. *)
let indent n = String.make n ' '

(** Convert a primitive type to a short S-expression tag. *)
let prim_type_to_sexpr = function
  | TI32  -> "i32" | TI64  -> "i64"
  | TU32  -> "u32" | TU64  -> "u64"
  | TBool -> "bool" | TUnit -> "unit"

(** Convert a type to S-expression form. *)
let rec typ_to_sexpr = function
  | TPrim p         -> prim_type_to_sexpr p
  | TArray (ty, sz) ->
    (match sz with
     | None   -> Printf.sprintf "(array %s)" (typ_to_sexpr ty)
     | Some n -> Printf.sprintf "(array %s %d)" (typ_to_sexpr ty) n)
  | TRef ty         -> Printf.sprintf "(ref %s)" (typ_to_sexpr ty)
  | TFun (params, ret) ->
    Printf.sprintf "(fn (%s) -> %s)"
      (String.concat " " (List.map typ_to_sexpr params))
      (typ_to_sexpr ret)
  | TStruct name    -> Printf.sprintf "(struct \"%s\")" name
  | TTrace          -> "trace"

(** Convert a literal to S-expression form. *)
let literal_to_sexpr = function
  | LInt n  -> Int64.to_string n
  | LBool b -> if b then "#t" else "#f"
  | LUnit   -> "()"

(** Convert a binary operator to a string. *)
let binop_to_string = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
  | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
  | And -> "and" | Or -> "or"
  | BitAnd -> "bit-and" | BitOr -> "bit-or" | BitXor -> "bit-xor"

(** Convert a unary operator to a string. *)
let unop_to_string = function
  | Neg -> "neg" | Not -> "not" | BitNot -> "bit-not"

(** Convert an expression to S-expression form.
    [d] is the current indentation depth. *)
let rec expr_to_sexpr d e =
  match e.expr_desc with
  | ELiteral lit       -> literal_to_sexpr lit
  | EVar name          -> Printf.sprintf "(id \"%s\")" name
  | EBinop (op, l, r)  ->
    Printf.sprintf "(%s %s %s)" (binop_to_string op)
      (expr_to_sexpr (d+2) l) (expr_to_sexpr (d+2) r)
  | EUnop (op, e)      ->
    Printf.sprintf "(%s %s)" (unop_to_string op) (expr_to_sexpr (d+2) e)
  | ECall (name, args)  ->
    Printf.sprintf "(call \"%s\" %s)" name
      (String.concat " " (List.map (expr_to_sexpr (d+2)) args))
  | EIndex (arr, idx)  ->
    Printf.sprintf "(index %s %s)" (expr_to_sexpr (d+2) arr) (expr_to_sexpr (d+2) idx)
  | EField (e, name)   ->
    Printf.sprintf "(field %s \"%s\")" (expr_to_sexpr (d+2) e) name
  | EIf (cond, then_e, else_e) ->
    Printf.sprintf "(if %s\n%s%s\n%s%s)"
      (expr_to_sexpr (d+2) cond)
      (indent (d+2)) (expr_to_sexpr (d+4) then_e)
      (indent (d+2)) (expr_to_sexpr (d+4) else_e)
  | EBlock (stmts, final_expr) ->
    let stmts_str = String.concat "" (List.map (fun s ->
      Printf.sprintf "\n%s%s" (indent (d+2)) (stmt_to_sexpr (d+2) s)
    ) stmts) in
    let final_str = match final_expr with
      | None -> ""
      | Some e -> Printf.sprintf "\n%s%s" (indent (d+2)) (expr_to_sexpr (d+2) e)
    in
    Printf.sprintf "(block%s%s)" stmts_str final_str
  | EStruct (name, fields) ->
    Printf.sprintf "(struct-lit \"%s\" %s)" name
      (String.concat " " (List.map (fun (f, e) ->
        Printf.sprintf "(%s %s)" f (expr_to_sexpr (d+4) e)
      ) fields))
  | EMatch (scrutinee, arms) ->
    let arms_str = String.concat "" (List.map (fun (pat, arm_e) ->
      let pat_str = match pat with
        | PWild -> "_"
        | PVar name -> Printf.sprintf "(var \"%s\")" name
        | PLiteral lit -> literal_to_sexpr lit
      in
      Printf.sprintf "\n%s(%s => %s)" (indent (d+2)) pat_str (expr_to_sexpr (d+4) arm_e)
    ) arms) in
    Printf.sprintf "(match %s%s)" (expr_to_sexpr (d+2) scrutinee) arms_str

(** Convert a statement to S-expression form.
    [d] is the current indentation depth. *)
and stmt_to_sexpr d s =
  match s.stmt_desc with
  | SLet (name, ty, value) ->
    let ty_str = match ty with
      | None -> ""
      | Some t -> Printf.sprintf " : %s" (typ_to_sexpr t)
    in
    Printf.sprintf "(let \"%s\"%s %s)" name ty_str (expr_to_sexpr (d+2) value)
  | SLetMut (name, ty, value) ->
    let ty_str = match ty with
      | None -> ""
      | Some t -> Printf.sprintf " : %s" (typ_to_sexpr t)
    in
    Printf.sprintf "(let-mut \"%s\"%s %s)" name ty_str (expr_to_sexpr (d+2) value)
  | SAssign (name, value) ->
    Printf.sprintf "(assign \"%s\" %s)" name (expr_to_sexpr (d+2) value)
  | SIf (cond, then_body, else_body) ->
    Printf.sprintf "(if %s\n%s(then%s)\n%s(else%s))"
      (expr_to_sexpr (d+2) cond)
      (indent (d+2))
      (String.concat "" (List.map (fun s ->
        Printf.sprintf " %s" (stmt_to_sexpr (d+4) s)) then_body))
      (indent (d+2))
      (String.concat "" (List.map (fun s ->
        Printf.sprintf " %s" (stmt_to_sexpr (d+4) s)) else_body))
  | SForRange (var, start, end_, body) ->
    Printf.sprintf "(for-range \"%s\" %Ld %Ld%s)" var start end_
      (String.concat "" (List.map (fun s ->
        Printf.sprintf "\n%s%s" (indent (d+2)) (stmt_to_sexpr (d+2) s)) body))
  | SExpr e -> expr_to_sexpr d e
  | SReturn e ->
    (match e with
     | None -> "(return)"
     | Some e -> Printf.sprintf "(return %s)" (expr_to_sexpr (d+2) e))
  | SSwap (a, b) ->
    Printf.sprintf "(swap \"%s\" \"%s\")" a b
  | SIncr (var, delta) ->
    Printf.sprintf "(incr \"%s\" %s)" var (expr_to_sexpr (d+2) delta)
  | SDecr (var, delta) ->
    Printf.sprintf "(decr \"%s\" %s)" var (expr_to_sexpr (d+2) delta)
  | SXorAssign (var, value) ->
    Printf.sprintf "(xor-assign \"%s\" %s)" var (expr_to_sexpr (d+2) value)
  | STrace (event, args) ->
    Printf.sprintf "(trace \"%s\" %s)" event
      (String.concat " " (List.map (expr_to_sexpr (d+2)) args))
  | SCheckpoint label ->
    Printf.sprintf "(checkpoint \"%s\")" label
  | SAssertInvariant (cond, msg) ->
    Printf.sprintf "(assert-invariant %s \"%s\")" (expr_to_sexpr (d+2) cond) msg
  | SError -> "(error)"

(** Convert a declaration to S-expression form.
    [d] is the current indentation depth. *)
let decl_to_sexpr d decl =
  match decl.decl_desc with
  | DFunction { name; params; return_type; body } ->
    Printf.sprintf "(fn \"%s\" (params %s) -> %s%s)" name
      (String.concat " " (List.map (fun (n, t) ->
        Printf.sprintf "(\"%s\" %s)" n (typ_to_sexpr t)
      ) params))
      (typ_to_sexpr return_type)
      (String.concat "" (List.map (fun s ->
        Printf.sprintf "\n%s%s" (indent (d+2)) (stmt_to_sexpr (d+2) s)
      ) body))
  | DStruct { name; fields } ->
    Printf.sprintf "(struct \"%s\" %s)" name
      (String.concat " " (List.map (fun (n, t) ->
        Printf.sprintf "(%s %s)" n (typ_to_sexpr t)
      ) fields))
  | DConst { name; typ; value } ->
    Printf.sprintf "(const \"%s\" %s %s)" name
      (typ_to_sexpr typ) (expr_to_sexpr (d+2) value)
  | DError -> "(error)"

(** Convert a complete program to S-expression form. *)
let program_to_sexpr prog =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "(program";
  (match prog.module_name with
   | None -> ()
   | Some name ->
     Buffer.add_string buf (Printf.sprintf "\n  (module \"%s\")" name));
  List.iter (fun decl ->
    Buffer.add_string buf "\n  ";
    Buffer.add_string buf (decl_to_sexpr 2 decl)
  ) prog.declarations;
  Buffer.add_char buf ')';
  Buffer.contents buf

(* ======================================================================
   JSON OUTPUT (using Yojson)
   ====================================================================== *)

(** Convert a literal to a Yojson value. *)
let literal_to_json = function
  | LInt n  -> `Assoc [("type", `String "int"); ("value", `Int (Int64.to_int n))]
  | LBool b -> `Assoc [("type", `String "bool"); ("value", `Bool b)]
  | LUnit   -> `Assoc [("type", `String "unit")]

(** Convert a type to a Yojson value. *)
let rec typ_to_json = function
  | TPrim p -> `Assoc [("type", `String "prim"); ("name", `String (prim_type_to_sexpr p))]
  | TArray (ty, sz) ->
    let fields = [("type", `String "array"); ("element", typ_to_json ty)] in
    let fields = match sz with None -> fields | Some n -> fields @ [("size", `Int n)] in
    `Assoc fields
  | TRef ty -> `Assoc [("type", `String "ref"); ("inner", typ_to_json ty)]
  | TFun (params, ret) ->
    `Assoc [("type", `String "function");
            ("params", `List (List.map typ_to_json params));
            ("return", typ_to_json ret)]
  | TStruct name -> `Assoc [("type", `String "struct"); ("name", `String name)]
  | TTrace -> `Assoc [("type", `String "trace")]

(** Convert an expression to a Yojson value. *)
let rec expr_to_json e =
  match e.expr_desc with
  | ELiteral lit -> literal_to_json lit
  | EVar name -> `Assoc [("type", `String "identifier"); ("name", `String name)]
  | EBinop (op, l, r) ->
    `Assoc [("type", `String "binary"); ("op", `String (binop_to_string op));
            ("lhs", expr_to_json l); ("rhs", expr_to_json r)]
  | EUnop (op, e) ->
    `Assoc [("type", `String "unary"); ("op", `String (unop_to_string op));
            ("operand", expr_to_json e)]
  | ECall (name, args) ->
    `Assoc [("type", `String "call"); ("name", `String name);
            ("args", `List (List.map expr_to_json args))]
  | EIndex (arr, idx) ->
    `Assoc [("type", `String "index"); ("array", expr_to_json arr);
            ("index", expr_to_json idx)]
  | EField (e, name) ->
    `Assoc [("type", `String "field"); ("object", expr_to_json e);
            ("field", `String name)]
  | EIf (cond, then_e, else_e) ->
    `Assoc [("type", `String "if"); ("condition", expr_to_json cond);
            ("then", expr_to_json then_e); ("else", expr_to_json else_e)]
  | EBlock (stmts, final_e) ->
    `Assoc [("type", `String "block");
            ("statements", `List (List.map stmt_to_json stmts));
            ("final", match final_e with None -> `Null | Some e -> expr_to_json e)]
  | EStruct (name, fields) ->
    `Assoc [("type", `String "struct_literal"); ("name", `String name);
            ("fields", `Assoc (List.map (fun (f, e) -> (f, expr_to_json e)) fields))]
  | EMatch (scrutinee, arms) ->
    let pattern_to_json = function
      | PWild -> `Assoc [("type", `String "wildcard")]
      | PVar name -> `Assoc [("type", `String "variable"); ("name", `String name)]
      | PLiteral lit -> `Assoc [("type", `String "literal"); ("value", literal_to_json lit)]
    in
    `Assoc [("type", `String "match"); ("scrutinee", expr_to_json scrutinee);
            ("arms", `List (List.map (fun (pat, arm_e) ->
              `Assoc [("pattern", pattern_to_json pat); ("expr", expr_to_json arm_e)]
            ) arms))]

(** Convert a statement to a Yojson value. *)
and stmt_to_json s =
  match s.stmt_desc with
  | SLet (name, ty, value) ->
    `Assoc [("type", `String "let"); ("name", `String name);
            ("ty", match ty with None -> `Null | Some t -> typ_to_json t);
            ("value", expr_to_json value)]
  | SLetMut (name, ty, value) ->
    `Assoc [("type", `String "let_mut"); ("name", `String name);
            ("ty", match ty with None -> `Null | Some t -> typ_to_json t);
            ("value", expr_to_json value)]
  | SAssign (name, value) ->
    `Assoc [("type", `String "assign"); ("name", `String name);
            ("value", expr_to_json value)]
  | SIf (cond, then_body, else_body) ->
    `Assoc [("type", `String "if");
            ("condition", expr_to_json cond);
            ("then", `List (List.map stmt_to_json then_body));
            ("else", `List (List.map stmt_to_json else_body))]
  | SForRange (var, start, end_, body) ->
    `Assoc [("type", `String "for_range"); ("var", `String var);
            ("start", `Int (Int64.to_int start));
            ("end", `Int (Int64.to_int end_));
            ("body", `List (List.map stmt_to_json body))]
  | SExpr e -> `Assoc [("type", `String "expr"); ("expr", expr_to_json e)]
  | SReturn e ->
    `Assoc [("type", `String "return");
            ("value", match e with None -> `Null | Some e -> expr_to_json e)]
  | SSwap (a, b) ->
    `Assoc [("type", `String "swap"); ("a", `String a); ("b", `String b)]
  | SIncr (var, delta) ->
    `Assoc [("type", `String "incr"); ("var", `String var); ("delta", expr_to_json delta)]
  | SDecr (var, delta) ->
    `Assoc [("type", `String "decr"); ("var", `String var); ("delta", expr_to_json delta)]
  | SXorAssign (var, value) ->
    `Assoc [("type", `String "xor_assign"); ("var", `String var); ("value", expr_to_json value)]
  | STrace (event, args) ->
    `Assoc [("type", `String "trace"); ("event", `String event);
            ("args", `List (List.map expr_to_json args))]
  | SCheckpoint label ->
    `Assoc [("type", `String "checkpoint"); ("label", `String label)]
  | SAssertInvariant (cond, msg) ->
    `Assoc [("type", `String "assert_invariant");
            ("condition", expr_to_json cond); ("message", `String msg)]
  | SError -> `Assoc [("type", `String "error")]

(** Convert a declaration to a Yojson value. *)
let decl_to_json decl =
  match decl.decl_desc with
  | DFunction { name; params; return_type; body } ->
    `Assoc [("type", `String "function"); ("name", `String name);
            ("params", `List (List.map (fun (n, t) ->
              `Assoc [("name", `String n); ("ty", typ_to_json t)]
            ) params));
            ("return_type", typ_to_json return_type);
            ("body", `List (List.map stmt_to_json body))]
  | DStruct { name; fields } ->
    `Assoc [("type", `String "struct"); ("name", `String name);
            ("fields", `List (List.map (fun (n, t) ->
              `Assoc [("name", `String n); ("ty", typ_to_json t)]
            ) fields))]
  | DConst { name; typ; value } ->
    `Assoc [("type", `String "const"); ("name", `String name);
            ("ty", typ_to_json typ); ("value", expr_to_json value)]
  | DError -> `Assoc [("type", `String "error")]

(** Convert a complete program to a Yojson value. *)
let program_to_yojson prog =
  `Assoc [
    ("format", `String "oblibeny-ast");
    ("version", `String "1.0");
    ("module", match prog.module_name with None -> `Null | Some n -> `String n);
    ("declarations", `List (List.map decl_to_json prog.declarations));
  ]

(** Convert a complete program to a pretty-printed JSON string. *)
let program_to_json prog =
  Yojson.Basic.pretty_to_string (program_to_yojson prog)
