;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2026 Hyperpolymath

;; ============================================================================
;; SPEC.core.scm - Oblíbený Language Core Specification (f0)
;; ============================================================================
;;
;; This specification defines the core semantics of Oblíbený, a secure edge
;; language for reversibility and accountability.
;;
;; Key Insight: Oblíbený has a DUAL-FORM architecture:
;;   1. META/FACTORY FORM - Turing-complete, used for metaprogramming
;;   2. CONSTRAINED FORM  - Turing-incomplete, produced by factory, enforces
;;                          reversibility and accountability
;;
;; ============================================================================

(define spec-version "0.1.0")
(define spec-tier "f0")  ;; Foundation pass

;; ============================================================================
;; SECTION 1: META/FACTORY FORM
;; ============================================================================
;;
;; The meta/factory form is Turing-complete. It is used to:
;;   - Define reusable templates and patterns
;;   - Generate constrained form programs
;;   - Perform compile-time computation
;;
;; Programs written in factory form execute at "factory time" (compile time)
;; and produce constrained form programs as output.
;;

(define factory-form
  '((name . "Oblíbený Factory Form")
    (turing-complete . #t)
    (execution-phase . "compile-time / factory-time")

    ;; Syntax elements available in factory form
    (syntax
      . ((literals
           . ((integers . "unbounded precision")
              (booleans . "#t #f")
              (strings  . "UTF-8 string literals")
              (symbols  . "quoted identifiers")))

         (bindings
           . ((let     . "(let ((name value) ...) body)")
              (letrec  . "(letrec ((name value) ...) body) ;; recursive bindings")
              (define  . "(define name value) ;; top-level only")))

         (control-flow
           . ((if      . "(if cond then else)")
              (cond    . "(cond (test expr) ... (else expr))")
              (match   . "(match expr ((pattern) body) ...)")))

         (abstraction
           . ((lambda  . "(lambda (params ...) body)")
              (apply   . "(apply fn args)")))

         ;; Factory-specific constructs for generating constrained form
         (factory-ops
           . ((emit       . "(emit constrained-expr) ;; output to constrained form")
              (quote-cf   . "(quote-cf expr) ;; quote for constrained form")
              (gensym     . "(gensym prefix) ;; generate unique identifier")
              (splice     . "(splice list) ;; splice list into template")
              (when-emit  . "(when-emit cond expr) ;; conditional emit")))))

    ;; Well-formedness rules for factory form
    (well-formedness
      . ((rule-f1 . "All emitted expressions must be valid constrained form")
         (rule-f2 . "Factory form must terminate (enforced via resource bounds)")
         (rule-f3 . "Factory form cannot access runtime values")))))

;; ============================================================================
;; SECTION 2: CONSTRAINED FORM (PRODUCED FORM)
;; ============================================================================
;;
;; The constrained form is Turing-INCOMPLETE by design:
;;   - No unbounded loops or recursion
;;   - All operations are reversible
;;   - Every operation produces an accountability trace
;;
;; Turing-incompleteness is enforced SYNTACTICALLY:
;;   - No `while`, `loop`, or recursive function definitions
;;   - Only bounded iteration via `for-range` with static bounds
;;   - Call graph must be acyclic (no mutual recursion)
;;

(define constrained-form
  '((name . "Oblíbený Constrained Form")
    (turing-complete . #f)  ;; CRITICAL: Must be false
    (execution-phase . "runtime")

    ;; Why Turing-incomplete?
    (rationale
      . "Turing-incompleteness guarantees:
         1. All programs terminate
         2. All operations can be reversed
         3. Resource usage is statically bounded
         4. Accountability traces are finite and complete")

    ;; Syntax elements - RESTRICTED subset
    (syntax
      . ((literals
           . ((integers . "fixed-width: i32, i64, u32, u64")
              (booleans . "true, false")
              (unit     . "()")))

         (bindings
           . ((let     . "let name = expr ;; immutable binding")
              (let-mut . "let mut name = expr ;; mutable, tracked")))

         (control-flow
           . ((if        . "if cond { then } else { else }")
              (match     . "match expr { pattern => body, ... }")
              ;; ONLY bounded iteration allowed
              (for-range . "for i in 0..N { body } ;; N must be static constant")))

         (abstraction
           . ((fn        . "fn name(params) -> ret { body }")
              (call      . "name(args) ;; call graph must be acyclic")))

         ;; Reversibility primitives
         (reversible-ops
           . ((swap      . "swap(a, b) ;; reversible swap")
              (incr      . "incr(x, delta) ;; reversible increment, inverse is decr")
              (decr      . "decr(x, delta) ;; reversible decrement, inverse is incr")
              (push      . "push(stack, val) ;; reversible push, inverse is pop")
              (pop       . "pop(stack) ;; reversible pop, inverse is push")
              (xor-assign . "x ^= val ;; self-inverse")))

         ;; Accountability trace operations
         (trace-ops
           . ((trace     . "trace(event) ;; append to accountability log")
              (checkpoint . "checkpoint(label) ;; create named checkpoint")
              (assert-invariant . "assert_invariant(cond, msg)")))))

    ;; Syntactic restrictions that enforce Turing-incompleteness
    (syntactic-restrictions
      . ((no-while       . "The keyword 'while' is rejected by parser")
         (no-loop        . "The keyword 'loop' is rejected by parser")
         (no-recursion   . "Functions cannot call themselves directly or indirectly")
         (bounded-for    . "for-range upper bound must be a compile-time constant")
         (acyclic-calls  . "Call graph is checked to be a DAG at compile time")))

    ;; Well-formedness rules for constrained form
    (well-formedness
      . ((rule-c1 . "All loops must have static bounds")
         (rule-c2 . "Call graph must be acyclic (topologically sortable)")
         (rule-c3 . "Every mutable variable must have reversibility annotations")
         (rule-c4 . "Every function must produce an accountability trace")))))

;; ============================================================================
;; SECTION 3: REVERSIBILITY INVARIANT
;; ============================================================================
;;
;; Every operation in constrained form must be reversible. This means:
;;   - Given output state, we can reconstruct input state
;;   - The reverse operation is well-defined and computable
;;   - No information is lost (or lost information is traced)
;;

(define reversibility-invariant
  '((name . "Oblíbený Reversibility Invariant")

    (statement
      . "For every operation OP in constrained form, there exists an inverse
         operation OP^(-1) such that:
           OP^(-1)(OP(state, trace)) = (state, trace')
         where trace' extends trace with the reverse operation record.")

    (formal-property
      . ((bidirectional . "OP and OP^(-1) are both total functions on valid states")
         (trace-extension . "Reverse operations append to trace, never discard")
         (identity-law . "OP^(-1)(OP(x)) ≡ x for all x in domain")))

    ;; Classification of operations by reversibility type
    (operation-classes
      . ((self-inverse
           . "Operations where OP = OP^(-1)"
           (examples . ("xor-assign" "swap" "not")))

         (paired-inverse
           . "Operations with distinct inverse"
           (examples . (("incr" . "decr")
                        ("push" . "pop")
                        ("encrypt" . "decrypt"))))

         (traced-destructive
           . "Operations that destroy information but record it in trace"
           (examples . ("overwrite with trace(old-value)"
                        "truncate with trace(removed-elements)")))))

    ;; Verification approach
    (verification
      . ((static-check . "Compiler verifies each operation has defined inverse")
         (runtime-check . "Optional runtime verification that reverse restores state")
         (formal-proof . "f1+ tier: Coq/Lean proofs of reversibility")))))

;; ============================================================================
;; SECTION 4: ACCOUNTABILITY TRACE
;; ============================================================================
;;
;; Every execution of constrained form produces an accountability trace.
;; The trace is:
;;   - Append-only (immutable once written)
;;   - Cryptographically hashable for integrity
;;   - Sufficient to replay or reverse the computation
;;

(define accountability-trace
  '((name . "Oblíbený Accountability Trace")

    (purpose
      . "The accountability trace provides:
         1. Complete audit log of all operations
         2. Sufficient information to reverse any operation
         3. Cryptographic binding to execution history
         4. Integration point for MAA (Mutually-Assured-Accountability)")

    (trace-structure
      . ((entry
           . ((timestamp . "Logical clock / sequence number")
              (operation . "Operation identifier")
              (inputs    . "Input values (hashed or literal)")
              (outputs   . "Output values (hashed or literal)")
              (actor     . "Identity of executing agent")
              (context   . "Execution context / scope")))

         (checkpoints
           . "Named points that can be referenced for rollback")

         (hash-chain
           . "Each entry includes hash of previous entry")))

    ;; Trace operations
    (operations
      . ((append . "trace(entry) -> () ;; append entry, cannot fail")
         (hash   . "trace_hash() -> Hash ;; current trace digest")
         (export . "trace_export(format) -> Bytes ;; serialize trace")))

    ;; Integration with MAA
    (maa-integration
      . ((interface . "Trace can be committed to MAA ledger at checkpoints")
         (verification . "External parties can verify trace integrity")
         (non-repudiation . "Actor cannot deny traced operations")
         (note . "Full MAA integration defined in external contract, not here")))

    ;; Integration with Absolute-Zero
    (absolute-zero-integration
      . ((interface . "Trace entries can be encrypted for privacy")
         (zero-knowledge . "Proofs can attest to trace properties without revealing content")
         (note . "Full Absolute-Zero integration defined in external contract, not here")))))

;; ============================================================================
;; SECTION 5: CONFORMANCE REQUIREMENTS
;; ============================================================================

(define conformance
  '((parser-rejects
      . ((while-loops   . "Parser MUST reject 'while' keyword")
         (loop-keyword  . "Parser MUST reject 'loop' keyword")
         (recursive-def . "Parser MUST reject direct self-reference in function body")))

    (static-checks
      . ((call-graph-acyclic . "Compiler MUST verify call graph is DAG")
         (bounds-static      . "Compiler MUST verify for-range bounds are constants")
         (reversibility      . "Compiler MUST verify each op has defined inverse")))

    (runtime-behavior
      . ((trace-always . "Every operation MUST append to trace")
         (termination  . "Every program MUST terminate in bounded steps")))))

;; ============================================================================
;; SECTION 6: EXTERNAL INTERFACES (CONTRACTS ONLY)
;; ============================================================================
;;
;; Per ANCHOR directive: Define interfaces only, no embedded implementations.
;;

(define external-interfaces
  '((maa-contract
      . ((name . "Mutually-Assured-Accountability Interface")
         (description . "Oblíbený traces can be committed to MAA for external verification")
         (operations
           . ((commit-trace . "Commit current trace hash to MAA ledger")
              (verify-trace . "Request MAA verification of trace")
              (attest       . "Produce attestation of trace validity")))
         (defined-in . "external: maa-protocol/SPEC.scm")))

    (absolute-zero-contract
      . ((name . "Absolute-Zero Privacy Interface")
         (description . "Oblíbený supports zero-knowledge proofs over traces")
         (operations
           . ((encrypt-trace . "Encrypt trace for privacy")
              (prove-property . "Generate ZK proof of trace property")
              (verify-proof   . "Verify ZK proof without seeing trace")))
         (defined-in . "external: absolute-zero/SPEC.scm")))))

;; ============================================================================
;; SECTION 7: REFERENCE EXAMPLES
;; ============================================================================

(define examples
  '((factory-to-constrained
      . ";; Factory form that produces constrained form
         (define (make-bounded-counter max-val)
           (emit
             `(fn counter() -> i32 {
                let mut c = 0;
                for i in 0..,(quote-cf max-val) {
                  incr(c, 1);
                  trace(increment);
                }
                c
              })))

         ;; Invoking (make-bounded-counter 10) produces:
         ;; fn counter() -> i32 {
         ;;   let mut c = 0;
         ;;   for i in 0..10 { incr(c, 1); trace(increment); }
         ;;   c
         ;; }")

    (loop-rejection
      . ";; This constrained form is REJECTED by parser:
         ;; fn bad() {
         ;;   while true { }  // ERROR: 'while' not allowed
         ;; }
         ;;
         ;; fn also_bad() {
         ;;   also_bad()  // ERROR: recursive call not allowed
         ;; }")

    (reversible-operation
      . ";; Reversible swap example
         fn swap_values(x: &mut i32, y: &mut i32) {
           swap(x, y);
           trace(swapped(x, y));
         }
         ;; Inverse is identical: swap(x, y) again
         ")))

;; ============================================================================
;; END OF SPEC.core.scm
;; ============================================================================
