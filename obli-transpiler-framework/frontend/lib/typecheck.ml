(* SPDX-License-Identifier: MIT OR Palimpsest-0.8 *)
(* Copyright (c) 2024 Hyperpolymath *)

(** Type checking pass for Oblibeny *)

open Ast
open Errors

(** Type environment *)
module Env = struct
  type binding =
    | VarBinding of annotated_type
    | FunBinding of { params: annotated_type list; ret: annotated_type }
    | TypeBinding of typ
    | StructBinding of { fields: (string * annotated_type) list }

  type t = {
    bindings: (string, binding) Hashtbl.t;
    parent: t option;
    in_function: annotated_type option;  (* Return type if inside function *)
  }

  let create ?parent () = {
    bindings = Hashtbl.create 16;
    parent;
    in_function = None;
  }

  let enter_function ret_type env = {
    bindings = Hashtbl.create 16;
    parent = Some env;
    in_function = Some ret_type;
  }

  let enter_scope env = {
    bindings = Hashtbl.create 16;
    parent = Some env;
    in_function = env.in_function;
  }

  let rec lookup name env =
    match Hashtbl.find_opt env.bindings name with
    | Some b -> Some b
    | None ->
      match env.parent with
      | Some p -> lookup name p
      | None -> None

  let add name binding env =
    Hashtbl.replace env.bindings name binding

  let add_var name typ env =
    add name (VarBinding typ) env

  let add_fun name params ret env =
    add name (FunBinding { params; ret }) env

  let add_struct name fields env =
    add name (StructBinding { fields }) env

  let return_type env = env.in_function
end

(** Type representation utilities *)
let rec type_to_string = function
  | TPrim (TInt None) -> "int"
  | TPrim (TInt (Some n)) -> Printf.sprintf "int<%d>" n
  | TPrim (TUint None) -> "uint"
  | TPrim (TUint (Some n)) -> Printf.sprintf "uint<%d>" n
  | TPrim TBool -> "bool"
  | TPrim TByte -> "byte"
  | TPrim TUnit -> "unit"
  | TArray (t, _) -> Printf.sprintf "array<%s>" (type_to_string t)
  | TOArray t -> Printf.sprintf "oarray<%s>" (type_to_string t)
  | TRef (t, _) -> Printf.sprintf "ref<%s>" (type_to_string t)
  | TFun (args, ret) ->
    let args_str = String.concat ", " (List.map type_to_string args) in
    Printf.sprintf "(%s) -> %s" args_str (type_to_string ret)
  | TStruct name -> name
  | TGeneric (name, args) ->
    let args_str = String.concat ", " (List.map type_to_string args) in
    Printf.sprintf "%s<%s>" name args_str
  | TVar name -> "'" ^ name

let security_to_string = function
  | Low -> "low"
  | High -> "high"

let annotated_type_to_string at =
  Printf.sprintf "@%s %s" (security_to_string at.security) (type_to_string at.typ)

(** Type equality *)
let rec types_equal t1 t2 =
  match t1, t2 with
  | TPrim p1, TPrim p2 -> p1 = p2
  | TArray (e1, _), TArray (e2, _) -> types_equal e1 e2
  | TOArray e1, TOArray e2 -> types_equal e1 e2
  | TRef (e1, _), TRef (e2, _) -> types_equal e1 e2
  | TFun (a1, r1), TFun (a2, r2) ->
    List.length a1 = List.length a2 &&
    List.for_all2 types_equal a1 a2 &&
    types_equal r1 r2
  | TStruct n1, TStruct n2 -> n1 = n2
  | TGeneric (n1, a1), TGeneric (n2, a2) ->
    n1 = n2 && List.length a1 = List.length a2 &&
    List.for_all2 types_equal a1 a2
  | TVar n1, TVar n2 -> n1 = n2
  | _ -> false

(** Security label lattice *)
let security_join s1 s2 =
  match s1, s2 with
  | High, _ | _, High -> High
  | Low, Low -> Low

let security_leq s1 s2 =
  match s1, s2 with
  | Low, _ -> true
  | High, High -> true
  | High, Low -> false

(** Type checker state *)
type state = {
  diags: diagnostics;
  env: Env.t;
}

let create_state () = {
  diags = create_diagnostics ();
  env = Env.create ();
}

(** Check binary operator types *)
let check_binop state op lhs_type rhs_type loc =
  let numeric_types = [TPrim (TInt None); TPrim (TUint None); TPrim TByte] in
  let is_numeric t = List.exists (types_equal t) numeric_types in
  let is_bool t = types_equal t (TPrim TBool) in
  let is_int t = match t with TPrim (TInt _ | TUint _) -> true | _ -> false in

  match op with
  | Add | Sub | Mul | Div | Mod ->
    if not (is_numeric lhs_type && types_equal lhs_type rhs_type) then
      report state.diags (make_error
        (Invalid_operation { op = show_binop op; typ = type_to_string lhs_type })
        loc "arithmetic operation requires matching numeric types");
    lhs_type

  | Eq | Neq ->
    if not (types_equal lhs_type rhs_type) then
      report state.diags (type_mismatch
        ~expected:(type_to_string lhs_type)
        ~found:(type_to_string rhs_type)
        loc);
    TPrim TBool

  | Lt | Le | Gt | Ge ->
    if not (is_numeric lhs_type && types_equal lhs_type rhs_type) then
      report state.diags (make_error
        (Invalid_operation { op = show_binop op; typ = type_to_string lhs_type })
        loc "comparison requires matching numeric types");
    TPrim TBool

  | And | Or ->
    if not (is_bool lhs_type && is_bool rhs_type) then
      report state.diags (make_error
        (Invalid_operation { op = show_binop op; typ = type_to_string lhs_type })
        loc "logical operation requires boolean operands");
    TPrim TBool

  | BitAnd | BitOr | BitXor ->
    if not (is_int lhs_type && types_equal lhs_type rhs_type) then
      report state.diags (make_error
        (Invalid_operation { op = show_binop op; typ = type_to_string lhs_type })
        loc "bitwise operation requires matching integer types");
    lhs_type

  | Shl | Shr ->
    if not (is_int lhs_type && is_int rhs_type) then
      report state.diags (make_error
        (Invalid_operation { op = show_binop op; typ = type_to_string lhs_type })
        loc "shift operation requires integer operands");
    lhs_type

(** Check unary operator types *)
let check_unop state op operand_type loc =
  match op with
  | Neg ->
    (match operand_type with
     | TPrim (TInt _ | TUint _) -> operand_type
     | _ ->
       report state.diags (make_error
         (Invalid_operation { op = "negation"; typ = type_to_string operand_type })
         loc "negation requires numeric type");
       operand_type)

  | Not ->
    if not (types_equal operand_type (TPrim TBool)) then
      report state.diags (make_error
        (Invalid_operation { op = "not"; typ = type_to_string operand_type })
        loc "logical not requires boolean operand");
    TPrim TBool

  | BitNot ->
    (match operand_type with
     | TPrim (TInt _ | TUint _) -> operand_type
     | _ ->
       report state.diags (make_error
         (Invalid_operation { op = "bitwise not"; typ = type_to_string operand_type })
         loc "bitwise not requires integer type");
       operand_type)

(** Type check expression *)
let rec check_expr state env expr =
  let (typ, security) = check_expr_desc state env expr.expr_desc expr.expr_loc in
  let atype = mk_atype expr.expr_loc security typ in
  expr.expr_type <- Some atype;
  atype

and check_expr_desc state env desc loc =
  match desc with
  | ELiteral lit ->
    let typ = match lit with
      | LInt _ -> TPrim (TInt None)
      | LUint _ -> TPrim (TUint None)
      | LBool _ -> TPrim TBool
      | LByte _ -> TPrim TByte
      | LUnit -> TPrim TUnit
    in
    (typ, Low)

  | EVar name ->
    (match Env.lookup name env with
     | Some (Env.VarBinding at) -> (at.typ, at.security)
     | Some (Env.FunBinding { params; ret }) ->
       (TFun (List.map (fun at -> at.typ) params, ret.typ), Low)
     | _ ->
       report state.diags (unknown_identifier name loc);
       (TPrim TUnit, Low))

  | EBinop (op, lhs, rhs) ->
    let lhs_at = check_expr state env lhs in
    let rhs_at = check_expr state env rhs in
    let result_type = check_binop state op lhs_at.typ rhs_at.typ loc in
    let result_security = security_join lhs_at.security rhs_at.security in
    (result_type, result_security)

  | EUnop (op, operand) ->
    let operand_at = check_expr state env operand in
    let result_type = check_unop state op operand_at.typ loc in
    (result_type, operand_at.security)

  | ECall (func, args) ->
    let func_at = check_expr state env func in
    (match func_at.typ with
     | TFun (param_types, ret_type) ->
       if List.length args <> List.length param_types then
         report state.diags (make_error
           (Arity_mismatch { expected = List.length param_types; found = List.length args })
           loc "wrong number of arguments");
       let arg_security = List.fold_left (fun acc arg ->
         let at = check_expr state env arg in
         security_join acc at.security
       ) Low args in
       (ret_type, arg_security)
     | _ ->
       report state.diags (make_error
         (Not_a_function (type_to_string func_at.typ))
         loc "called expression is not a function");
       (TPrim TUnit, Low))

  | EIndex (arr, idx) ->
    let arr_at = check_expr state env arr in
    let idx_at = check_expr state env idx in
    let elem_type = match arr_at.typ with
      | TArray (elem, _) -> elem
      | TOArray elem -> elem
      | _ ->
        report state.diags (make_error
          (Invalid_operation { op = "index"; typ = type_to_string arr_at.typ })
          loc "indexing requires array type");
        TPrim TUnit
    in
    (elem_type, security_join arr_at.security idx_at.security)

  | EOramRead (arr, idx) ->
    let arr_at = check_expr state env arr in
    let idx_at = check_expr state env idx in
    let elem_type = match arr_at.typ with
      | TOArray elem -> elem
      | _ ->
        report state.diags (make_error
          (Invalid_operation { op = "oread"; typ = type_to_string arr_at.typ })
          loc "oread requires oarray type");
        TPrim TUnit
    in
    (* ORAM read result is high security because it's designed for secret indices *)
    (elem_type, security_join High idx_at.security)

  | EField (obj, field) ->
    let obj_at = check_expr state env obj in
    (match obj_at.typ with
     | TStruct name ->
       (match Env.lookup name env with
        | Some (Env.StructBinding { fields }) ->
          (match List.assoc_opt field fields with
           | Some ft -> (ft.typ, security_join obj_at.security ft.security)
           | None ->
             report state.diags (make_error
               (Field_not_found { struct_name = name; field })
               loc (Printf.sprintf "field `%s` not found in struct `%s`" field name));
             (TPrim TUnit, Low))
        | _ ->
          report state.diags (unknown_type name loc);
          (TPrim TUnit, Low))
     | _ ->
       report state.diags (make_error
         (Invalid_operation { op = "field access"; typ = type_to_string obj_at.typ })
         loc "field access requires struct type");
       (TPrim TUnit, Low))

  | EIf (cond, then_expr, else_expr) ->
    let cond_at = check_expr state env cond in
    if not (types_equal cond_at.typ (TPrim TBool)) then
      report state.diags (type_mismatch ~expected:"bool" ~found:(type_to_string cond_at.typ) loc);
    let then_at = check_expr state env then_expr in
    let else_at = check_expr state env else_expr in
    if not (types_equal then_at.typ else_at.typ) then
      report state.diags (type_mismatch
        ~expected:(type_to_string then_at.typ)
        ~found:(type_to_string else_at.typ)
        else_expr.expr_loc);
    let result_security = security_join cond_at.security (security_join then_at.security else_at.security) in
    (then_at.typ, result_security)

  | EBlock (stmts, final_expr) ->
    let block_env = Env.enter_scope env in
    List.iter (check_stmt state block_env) stmts;
    (match final_expr with
     | Some e -> check_expr state block_env e |> (fun at -> (at.typ, at.security))
     | None -> (TPrim TUnit, Low))

  | ELambda (params, body) ->
    let lambda_env = Env.enter_scope env in
    List.iter (fun (name, at) -> Env.add_var name at lambda_env) params;
    let body_at = check_expr state lambda_env body in
    (TFun (List.map (fun (_, at) -> at.typ) params, body_at.typ), Low)

  | ETuple exprs ->
    (* For simplicity, we don't have tuple types yet - treat as struct *)
    let _ = List.map (check_expr state env) exprs in
    (TPrim TUnit, Low)  (* TODO: Add proper tuple types *)

  | EStruct (name, fields) ->
    (match Env.lookup name env with
     | Some (Env.StructBinding { fields = expected_fields }) ->
       let field_security = List.fold_left (fun acc (fname, fexpr) ->
         let fat = check_expr state env fexpr in
         match List.assoc_opt fname expected_fields with
         | Some expected_at ->
           if not (types_equal fat.typ expected_at.typ) then
             report state.diags (type_mismatch
               ~expected:(type_to_string expected_at.typ)
               ~found:(type_to_string fat.typ)
               fexpr.expr_loc);
           security_join acc fat.security
         | None ->
           report state.diags (make_error
             (Field_not_found { struct_name = name; field = fname })
             fexpr.expr_loc (Printf.sprintf "unknown field `%s`" fname));
           acc
       ) Low fields in
       (TStruct name, field_security)
     | _ ->
       report state.diags (unknown_type name loc);
       (TPrim TUnit, Low))

  | ECmov (cond, then_val, else_val) ->
    let cond_at = check_expr state env cond in
    if not (types_equal cond_at.typ (TPrim TBool)) then
      report state.diags (type_mismatch ~expected:"bool" ~found:(type_to_string cond_at.typ) loc);
    let then_at = check_expr state env then_val in
    let else_at = check_expr state env else_val in
    if not (types_equal then_at.typ else_at.typ) then
      report state.diags (type_mismatch
        ~expected:(type_to_string then_at.typ)
        ~found:(type_to_string else_at.typ)
        else_val.expr_loc);
    let result_security = security_join cond_at.security (security_join then_at.security else_at.security) in
    (then_at.typ, result_security)

(** Type check statement *)
and check_stmt state env stmt =
  match stmt.stmt_desc with
  | SLet (pattern, type_annot, init) ->
    let init_at = check_expr state env init in
    let bound_type = match type_annot with
      | Some annot ->
        if not (types_equal annot.typ init_at.typ) then
          report state.diags (type_mismatch
            ~expected:(type_to_string annot.typ)
            ~found:(type_to_string init_at.typ)
            init.expr_loc);
        annot
      | None -> init_at
    in
    (match pattern with
     | PVar name -> Env.add_var name bound_type env
     | _ -> () (* TODO: Handle other patterns *))

  | SAssign (lhs, rhs) ->
    let lhs_at = check_expr state env lhs in
    let rhs_at = check_expr state env rhs in
    (* Check type compatibility *)
    if not (types_equal lhs_at.typ rhs_at.typ) then
      report state.diags (type_mismatch
        ~expected:(type_to_string lhs_at.typ)
        ~found:(type_to_string rhs_at.typ)
        rhs.expr_loc);
    (* CRITICAL: Check security label - cannot assign high to low (information leak) *)
    if not (security_leq rhs_at.security lhs_at.security) then
      report state.diags (information_leak
        ~from_label:(security_to_string rhs_at.security)
        ~to_label:(security_to_string lhs_at.security)
        stmt.stmt_loc)

  | SOramWrite (arr, idx, value) ->
    let arr_at = check_expr state env arr in
    let _idx_at = check_expr state env idx in
    let value_at = check_expr state env value in
    (match arr_at.typ with
     | TOArray elem_type ->
       if not (types_equal elem_type value_at.typ) then
         report state.diags (type_mismatch
           ~expected:(type_to_string elem_type)
           ~found:(type_to_string value_at.typ)
           value.expr_loc)
     | _ ->
       report state.diags (make_error
         (Invalid_operation { op = "owrite"; typ = type_to_string arr_at.typ })
         stmt.stmt_loc "owrite requires oarray type"))

  | SExpr e ->
    let _ = check_expr state env e in ()

  | SIf (cond, then_stmts, else_stmts) ->
    let cond_at = check_expr state env cond in
    if not (types_equal cond_at.typ (TPrim TBool)) then
      report state.diags (type_mismatch ~expected:"bool" ~found:(type_to_string cond_at.typ) cond.expr_loc);
    let then_env = Env.enter_scope env in
    List.iter (check_stmt state then_env) then_stmts;
    let else_env = Env.enter_scope env in
    List.iter (check_stmt state else_env) else_stmts

  | SWhile (cond, body) ->
    let cond_at = check_expr state env cond in
    if not (types_equal cond_at.typ (TPrim TBool)) then
      report state.diags (type_mismatch ~expected:"bool" ~found:(type_to_string cond_at.typ) cond.expr_loc);
    let body_env = Env.enter_scope env in
    List.iter (check_stmt state body_env) body

  | SFor (var, start_expr, end_expr, body) ->
    let start_at = check_expr state env start_expr in
    let end_at = check_expr state env end_expr in
    let iter_type = match start_at.typ with
      | TPrim (TInt _ | TUint _) -> start_at.typ
      | _ ->
        report state.diags (make_error
          (Invalid_operation { op = "for loop"; typ = type_to_string start_at.typ })
          start_expr.expr_loc "for loop range requires integer type");
        TPrim (TInt None)
    in
    if not (types_equal start_at.typ end_at.typ) then
      report state.diags (type_mismatch
        ~expected:(type_to_string start_at.typ)
        ~found:(type_to_string end_at.typ)
        end_expr.expr_loc);
    let body_env = Env.enter_scope env in
    Env.add_var var (mk_atype stmt.stmt_loc (security_join start_at.security end_at.security) iter_type) body_env;
    List.iter (check_stmt state body_env) body

  | SReturn expr_opt ->
    (match Env.return_type env, expr_opt with
     | Some ret_type, Some expr ->
       let expr_at = check_expr state env expr in
       if not (types_equal ret_type.typ expr_at.typ) then
         report state.diags (type_mismatch
           ~expected:(type_to_string ret_type.typ)
           ~found:(type_to_string expr_at.typ)
           expr.expr_loc)
     | Some ret_type, None ->
       if not (types_equal ret_type.typ (TPrim TUnit)) then
         report state.diags (type_mismatch
           ~expected:(type_to_string ret_type.typ)
           ~found:"unit"
           stmt.stmt_loc)
     | None, _ ->
       report state.diags (make_error
         (Internal_error "return outside function")
         stmt.stmt_loc "return statement outside of function"))

  | SBreak | SContinue -> ()

(** Type check declaration *)
let check_decl state env decl =
  match decl.decl_desc with
  | DFunction { name; params; return_type; body; _ } ->
    let param_types = List.map snd params in
    Env.add_fun name param_types return_type env;
    let fn_env = Env.enter_function return_type env in
    List.iter (fun (pname, ptype) -> Env.add_var pname ptype fn_env) params;
    List.iter (check_stmt state fn_env) body

  | DStruct { name; fields; _ } ->
    Env.add_struct name fields env

  | DConst { name; typ; value } ->
    let value_at = check_expr state env value in
    if not (types_equal typ.typ value_at.typ) then
      report state.diags (type_mismatch
        ~expected:(type_to_string typ.typ)
        ~found:(type_to_string value_at.typ)
        value.expr_loc);
    Env.add_var name typ env

  | DExtern { name; typ; _ } ->
    Env.add_var name typ env

  | DImport _ -> ()

(** Type check a complete program *)
let check_program program =
  let state = create_state () in
  (* Add built-in types and functions *)
  List.iter (check_decl state state.env) program.declarations;
  state.diags
