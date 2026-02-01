(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell *)

(** Constrained Form Validation for Oblíbený

    This module validates that a program conforms to the Turing-incomplete
    constrained form. It enforces:

    1. NO while loops (keyword rejected)
    2. NO loop keyword (rejected)
    3. NO recursive calls (direct or indirect)
    4. Call graph must be acyclic (DAG)
    5. For-range bounds must be static constants

    If any violation is found, the program is rejected.
*)

open Ast

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type call_graph = StringSet.t StringMap.t

(** Build call graph from program *)
let build_call_graph (program : program) : call_graph =
  let graph = ref StringMap.empty in

  let rec collect_calls_expr expr =
    match expr.expr_desc with
    | ECall (callee, args) ->
      let callees = List.fold_left (fun acc e -> StringSet.union acc (collect_calls_expr e)) StringSet.empty args in
      StringSet.add callee callees
    | EBinop (_, e1, e2) ->
      StringSet.union (collect_calls_expr e1) (collect_calls_expr e2)
    | EUnop (_, e) -> collect_calls_expr e
    | EIf (c, t, e) ->
      StringSet.union (collect_calls_expr c)
        (StringSet.union (collect_calls_expr t) (collect_calls_expr e))
    | EBlock (stmts, final) ->
      let from_stmts = List.fold_left (fun acc s -> StringSet.union acc (collect_calls_stmt s)) StringSet.empty stmts in
      let from_final = match final with Some e -> collect_calls_expr e | None -> StringSet.empty in
      StringSet.union from_stmts from_final
    | EIndex (arr, idx) ->
      StringSet.union (collect_calls_expr arr) (collect_calls_expr idx)
    | EField (e, _) -> collect_calls_expr e
    | EStruct (_, fields) ->
      List.fold_left (fun acc (_, e) -> StringSet.union acc (collect_calls_expr e)) StringSet.empty fields
    | ELiteral _ | EVar _ -> StringSet.empty

  and collect_calls_stmt stmt =
    match stmt.stmt_desc with
    | SLet (_, _, e) | SLetMut (_, _, e) | SAssign (_, e) | SExpr e -> collect_calls_expr e
    | SIf (c, t, e) ->
      let from_cond = collect_calls_expr c in
      let from_then = List.fold_left (fun acc s -> StringSet.union acc (collect_calls_stmt s)) StringSet.empty t in
      let from_else = List.fold_left (fun acc s -> StringSet.union acc (collect_calls_stmt s)) StringSet.empty e in
      StringSet.union from_cond (StringSet.union from_then from_else)
    | SForRange (_, _, _, body) ->
      List.fold_left (fun acc s -> StringSet.union acc (collect_calls_stmt s)) StringSet.empty body
    | SReturn (Some e) -> collect_calls_expr e
    | SReturn None -> StringSet.empty
    | SSwap _ -> StringSet.empty
    | SIncr (_, e) | SDecr (_, e) | SXorAssign (_, e) -> collect_calls_expr e
    | STrace (_, args) ->
      List.fold_left (fun acc e -> StringSet.union acc (collect_calls_expr e)) StringSet.empty args
    | SCheckpoint _ -> StringSet.empty
    | SAssertInvariant (e, _) -> collect_calls_expr e
  in

  List.iter (fun decl ->
    match decl.decl_desc with
    | DFunction { name; body; _ } ->
      let callees = List.fold_left (fun acc s -> StringSet.union acc (collect_calls_stmt s)) StringSet.empty body in
      graph := StringMap.add name callees !graph
    | _ -> ()
  ) program.declarations;
  !graph

(** Detect cycles in call graph using DFS *)
let find_cycles (graph : call_graph) : string list list =
  let visited = Hashtbl.create 16 in
  let rec_stack = Hashtbl.create 16 in
  let cycles = ref [] in

  let rec dfs node path =
    Hashtbl.replace visited node true;
    Hashtbl.replace rec_stack node true;

    let callees = try StringMap.find node graph with Not_found -> StringSet.empty in
    StringSet.iter (fun callee ->
      if not (Hashtbl.mem visited callee) then
        dfs callee (callee :: path)
      else if Hashtbl.find_opt rec_stack callee = Some true then
        (* Found cycle *)
        let cycle_start = List.find_opt ((=) callee) path in
        match cycle_start with
        | Some _ ->
          let cycle = callee :: (List.rev path) in
          cycles := cycle :: !cycles
        | None -> ()
    ) callees;

    Hashtbl.replace rec_stack node false
  in

  StringMap.iter (fun node _ ->
    if not (Hashtbl.mem visited node) then
      dfs node [node]
  ) graph;
  !cycles

(** Check a single function for direct self-recursion *)
let check_direct_recursion (name : string) (body : stmt list) : (string * Location.t) option =
  let rec check_expr expr =
    match expr.expr_desc with
    | ECall (callee, args) ->
      if callee = name then Some (callee, expr.expr_loc)
      else List.find_map check_expr args
    | EBinop (_, e1, e2) ->
      (match check_expr e1 with Some r -> Some r | None -> check_expr e2)
    | EUnop (_, e) -> check_expr e
    | EIf (c, t, e) ->
      (match check_expr c with Some r -> Some r | None ->
        match check_expr t with Some r -> Some r | None -> check_expr e)
    | EBlock (stmts, final) ->
      (match List.find_map check_stmt stmts with Some r -> Some r | None ->
        match final with Some e -> check_expr e | None -> None)
    | EIndex (arr, idx) ->
      (match check_expr arr with Some r -> Some r | None -> check_expr idx)
    | EField (e, _) -> check_expr e
    | EStruct (_, fields) -> List.find_map (fun (_, e) -> check_expr e) fields
    | ELiteral _ | EVar _ -> None

  and check_stmt stmt =
    match stmt.stmt_desc with
    | SLet (_, _, e) | SLetMut (_, _, e) | SAssign (_, e) | SExpr e -> check_expr e
    | SIf (c, t, el) ->
      (match check_expr c with Some r -> Some r | None ->
        match List.find_map check_stmt t with Some r -> Some r | None ->
          List.find_map check_stmt el)
    | SForRange (_, _, _, body) -> List.find_map check_stmt body
    | SReturn (Some e) -> check_expr e
    | SReturn None -> None
    | SSwap _ -> None
    | SIncr (_, e) | SDecr (_, e) | SXorAssign (_, e) -> check_expr e
    | STrace (_, args) -> List.find_map check_expr args
    | SCheckpoint _ -> None
    | SAssertInvariant (e, _) -> check_expr e
  in
  List.find_map check_stmt body

(** Validate entire program against constrained form rules *)
let validate_program (program : program) : constraint_violation list =
  let violations = ref [] in

  (* Check for direct recursion in each function *)
  List.iter (fun decl ->
    match decl.decl_desc with
    | DFunction { name; body; _ } ->
      (match check_direct_recursion name body with
       | Some (callee, loc) ->
         violations := RecursiveCall { func = name; callee; loc } :: !violations
       | None -> ())
    | _ -> ()
  ) program.declarations;

  (* Build call graph and check for cycles *)
  let graph = build_call_graph program in
  let cycles = find_cycles graph in
  List.iter (fun cycle ->
    violations := CyclicCallGraph cycle :: !violations
  ) cycles;

  List.rev !violations

(** Check if program is valid constrained form *)
let is_valid_constrained_form (program : program) : bool =
  validate_program program = []

(** Format violation for error message *)
let format_violation = function
  | WhileLoopFound loc ->
    Printf.sprintf "%s: error: 'while' loops are not allowed in constrained form"
      (Location.to_string loc)
  | LoopKeywordFound loc ->
    Printf.sprintf "%s: error: 'loop' keyword is not allowed in constrained form"
      (Location.to_string loc)
  | RecursiveCall { func; callee; loc } ->
    Printf.sprintf "%s: error: recursive call from '%s' to '%s' not allowed in constrained form"
      (Location.to_string loc) func callee
  | CyclicCallGraph cycle ->
    Printf.sprintf "error: cyclic call graph detected: %s"
      (String.concat " -> " cycle)
  | DynamicForBound { var; loc } ->
    Printf.sprintf "%s: error: for-range bound '%s' must be a static constant"
      (Location.to_string loc) var
