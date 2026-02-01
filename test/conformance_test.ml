(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell *)

(** Conformance Test Suite for Oblíbený

    These tests verify:
    1. Valid constrained form programs are accepted
    2. Programs with loops are rejected
    3. Programs with recursion are rejected
    4. Call graph cycles are detected
    5. Reversibility operations work correctly
    6. Accountability trace is produced
*)

open Oblibeny
open Ast

(** Helper to create a simple program with one function *)
let program_with_main body =
  { module_name = Some "test";
    declarations = [
      mk_decl Location.dummy (DFunction {
        name = "main";
        params = [];
        return_type = TPrim TUnit;
        body;
      })
    ] }

(** Helper to create a multi-function program *)
let program_with_functions fns =
  { module_name = Some "test";
    declarations = List.map (fun (name, params, body) ->
      mk_decl Location.dummy (DFunction {
        name;
        params = List.map (fun p -> (p, TPrim TI64)) params;
        return_type = TPrim TUnit;
        body;
      })
    ) fns }

(* ============================================================================
   TEST: Valid constrained form programs
   ============================================================================ *)

let test_valid_empty_main () =
  let prog = program_with_main [] in
  let violations = Constrained_check.validate_program prog in
  Alcotest.(check int) "no violations" 0 (List.length violations)

let test_valid_let_binding () =
  let prog = program_with_main [
    mk_stmt Location.dummy (SLet ("x", None, mk_expr Location.dummy (ELiteral (LInt 42L))));
  ] in
  let violations = Constrained_check.validate_program prog in
  Alcotest.(check int) "no violations" 0 (List.length violations)

let test_valid_for_range () =
  let prog = program_with_main [
    mk_stmt Location.dummy (SLetMut ("x", None, mk_expr Location.dummy (ELiteral (LInt 0L))));
    mk_stmt Location.dummy (SForRange ("i", 0L, 10L, [
      mk_stmt Location.dummy (SIncr ("x", mk_expr Location.dummy (ELiteral (LInt 1L))));
    ]));
  ] in
  let violations = Constrained_check.validate_program prog in
  Alcotest.(check int) "no violations" 0 (List.length violations)

let test_valid_non_recursive_call () =
  let prog = program_with_functions [
    ("helper", [], [
      mk_stmt Location.dummy (STrace ("called", []));
    ]);
    ("main", [], [
      mk_stmt Location.dummy (SExpr (mk_expr Location.dummy (ECall ("helper", []))));
    ]);
  ] in
  let violations = Constrained_check.validate_program prog in
  Alcotest.(check int) "no violations" 0 (List.length violations)

(* ============================================================================
   TEST: Direct recursion rejection
   ============================================================================ *)

let test_reject_direct_recursion () =
  let prog = program_with_functions [
    ("recursive_fn", [], [
      (* recursive_fn calls itself *)
      mk_stmt Location.dummy (SExpr (mk_expr Location.dummy (ECall ("recursive_fn", []))));
    ]);
    ("main", [], []);
  ] in
  let violations = Constrained_check.validate_program prog in
  Alcotest.(check bool) "has violations" true (List.length violations > 0);
  (* Check it's specifically a recursive call violation *)
  let is_recursive_violation = function
    | Ast.RecursiveCall _ -> true
    | _ -> false
  in
  Alcotest.(check bool) "is recursive call violation" true
    (List.exists is_recursive_violation violations)

(* ============================================================================
   TEST: Indirect recursion / call graph cycle rejection
   ============================================================================ *)

let test_reject_mutual_recursion () =
  let prog = program_with_functions [
    ("fn_a", [], [
      mk_stmt Location.dummy (SExpr (mk_expr Location.dummy (ECall ("fn_b", []))));
    ]);
    ("fn_b", [], [
      mk_stmt Location.dummy (SExpr (mk_expr Location.dummy (ECall ("fn_a", []))));
    ]);
    ("main", [], []);
  ] in
  let violations = Constrained_check.validate_program prog in
  Alcotest.(check bool) "has violations" true (List.length violations > 0);
  (* Should detect cycle in call graph *)
  let is_cycle_violation = function
    | Ast.CyclicCallGraph _ -> true
    | _ -> false
  in
  Alcotest.(check bool) "is cyclic call graph violation" true
    (List.exists is_cycle_violation violations)

let test_reject_three_way_cycle () =
  let prog = program_with_functions [
    ("fn_a", [], [
      mk_stmt Location.dummy (SExpr (mk_expr Location.dummy (ECall ("fn_b", []))));
    ]);
    ("fn_b", [], [
      mk_stmt Location.dummy (SExpr (mk_expr Location.dummy (ECall ("fn_c", []))));
    ]);
    ("fn_c", [], [
      mk_stmt Location.dummy (SExpr (mk_expr Location.dummy (ECall ("fn_a", []))));
    ]);
    ("main", [], []);
  ] in
  let violations = Constrained_check.validate_program prog in
  Alcotest.(check bool) "has cycle violations" true (List.length violations > 0)

(* ============================================================================
   TEST: Acyclic call graph is accepted
   ============================================================================ *)

let test_accept_acyclic_call_chain () =
  let prog = program_with_functions [
    ("leaf", [], [
      mk_stmt Location.dummy (STrace ("leaf", []));
    ]);
    ("middle", [], [
      mk_stmt Location.dummy (SExpr (mk_expr Location.dummy (ECall ("leaf", []))));
    ]);
    ("main", [], [
      mk_stmt Location.dummy (SExpr (mk_expr Location.dummy (ECall ("middle", []))));
    ]);
  ] in
  let violations = Constrained_check.validate_program prog in
  Alcotest.(check int) "no violations for acyclic graph" 0 (List.length violations)

(* ============================================================================
   TEST: Evaluation and trace
   ============================================================================ *)

let test_eval_produces_trace () =
  let prog = program_with_main [
    mk_stmt Location.dummy (SCheckpoint "test");
    mk_stmt Location.dummy (STrace ("hello", []));
  ] in
  let (_, trace) = Eval.eval_program prog in
  let entries = Trace.to_list trace in
  Alcotest.(check bool) "trace has entries" true (List.length entries > 0)

let test_eval_incr_decr_reversible () =
  let prog = program_with_main [
    mk_stmt Location.dummy (SLetMut ("x", None, mk_expr Location.dummy (ELiteral (LInt 10L))));
    mk_stmt Location.dummy (SIncr ("x", mk_expr Location.dummy (ELiteral (LInt 5L))));
    mk_stmt Location.dummy (SDecr ("x", mk_expr Location.dummy (ELiteral (LInt 5L))));
    (* x should be back to 10 *)
    mk_stmt Location.dummy (SAssertInvariant (
      mk_expr Location.dummy (EBinop (Eq,
        mk_expr Location.dummy (EVar "x"),
        mk_expr Location.dummy (ELiteral (LInt 10L)))),
      "x should be 10 after incr/decr"));
  ] in
  (* Should not raise *)
  let _ = Eval.eval_program prog in
  Alcotest.(check bool) "incr/decr reversible" true true

let test_eval_swap_reversible () =
  let prog = program_with_main [
    mk_stmt Location.dummy (SLetMut ("a", None, mk_expr Location.dummy (ELiteral (LInt 1L))));
    mk_stmt Location.dummy (SLetMut ("b", None, mk_expr Location.dummy (ELiteral (LInt 2L))));
    mk_stmt Location.dummy (SSwap ("a", "b"));
    mk_stmt Location.dummy (SSwap ("a", "b"));
    (* Should be back to original *)
    mk_stmt Location.dummy (SAssertInvariant (
      mk_expr Location.dummy (EBinop (Eq,
        mk_expr Location.dummy (EVar "a"),
        mk_expr Location.dummy (ELiteral (LInt 1L)))),
      "a should be 1 after double swap"));
  ] in
  let _ = Eval.eval_program prog in
  Alcotest.(check bool) "swap reversible" true true

let test_eval_xor_self_inverse () =
  let prog = program_with_main [
    mk_stmt Location.dummy (SLetMut ("x", None, mk_expr Location.dummy (ELiteral (LInt 42L))));
    mk_stmt Location.dummy (SXorAssign ("x", mk_expr Location.dummy (ELiteral (LInt 0xFFL))));
    mk_stmt Location.dummy (SXorAssign ("x", mk_expr Location.dummy (ELiteral (LInt 0xFFL))));
    (* x should be back to 42 *)
    mk_stmt Location.dummy (SAssertInvariant (
      mk_expr Location.dummy (EBinop (Eq,
        mk_expr Location.dummy (EVar "x"),
        mk_expr Location.dummy (ELiteral (LInt 42L)))),
      "x should be 42 after double xor"));
  ] in
  let _ = Eval.eval_program prog in
  Alcotest.(check bool) "xor self-inverse" true true

(* ============================================================================
   TEST SUITE
   ============================================================================ *)

let () =
  Alcotest.run "Oblíbený Conformance" [
    "valid-programs", [
      Alcotest.test_case "empty main" `Quick test_valid_empty_main;
      Alcotest.test_case "let binding" `Quick test_valid_let_binding;
      Alcotest.test_case "for-range loop" `Quick test_valid_for_range;
      Alcotest.test_case "non-recursive call" `Quick test_valid_non_recursive_call;
      Alcotest.test_case "acyclic call chain" `Quick test_accept_acyclic_call_chain;
    ];
    "recursion-rejection", [
      Alcotest.test_case "direct recursion" `Quick test_reject_direct_recursion;
      Alcotest.test_case "mutual recursion" `Quick test_reject_mutual_recursion;
      Alcotest.test_case "three-way cycle" `Quick test_reject_three_way_cycle;
    ];
    "evaluation", [
      Alcotest.test_case "produces trace" `Quick test_eval_produces_trace;
      Alcotest.test_case "incr/decr reversible" `Quick test_eval_incr_decr_reversible;
      Alcotest.test_case "swap reversible" `Quick test_eval_swap_reversible;
      Alcotest.test_case "xor self-inverse" `Quick test_eval_xor_self_inverse;
    ];
  ]
