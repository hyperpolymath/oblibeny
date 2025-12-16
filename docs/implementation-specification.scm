;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

;;;============================================================================
;;; Oblíbený Implementation Specification
;;; Theoretical Foundations & Practical Requirements
;;;
;;; Part of PhD Research: "The Dual-Language Paradigm: Achieving Compile-Time
;;; Security Guarantees and Anti-Tamper Resilience for Edge Computing via
;;; Maximal Principle Reduction"
;;;============================================================================

(define-module (oblibeny implementation-spec)
  #:export (research-context
            theoretical-foundations
            practical-requirements
            host-language-analysis
            compilation-architecture))

;;;============================================================================
;;; RESEARCH CONTEXT (PhD Proposal)
;;;============================================================================

(define research-context
  '((title . "The Dual-Language Paradigm: Achieving Compile-Time Security
              Guarantees and Anti-Tamper Resilience for Edge Computing via
              Maximal Principle Reduction")

    (central-problem
     "How can we enable developers to write highly expressive, Turing-Complete
      code while deploying artefacts that are provably secure, resource-bounded,
      and maximally resistant to forensic reverse engineering in hostile,
      resource-constrained environments?")

    (original-contribution
     ((name . "Dual-Language Paradigm (DLP)")
      (description . "Master Language as meta-tool to generate formally
                      constrained Deployment Subset")))

    (dual-language-paradigm
     ((master-language
       ((turing . "complete")
        (role . "High-level logic, metaprogramming, declarative proof generation")
        (value . "Expressiveness & Safety without compromising deployment")))

      (deployment-subset
       ((turing . "incomplete")
        (role . "Deployed artifact with restricted syntax")
        (restrictions . ("No recursion"
                         "No arbitrary loops"
                         "No dynamic allocation"))
        (value . "Compiler becomes Decider for halting problem")))))

    (methodology
     ((formal-proofs
       ((termination-checking . "Call Graph Analysis (Tarjan's SCC)")
        (resource-analysis . "Maximal Principle Reduction (MPR)")
        (capability-model . "Enhanced Capability Model (Hierarchies + Budgets)")))

      (security-obfuscation
       ((approach . "Semantic Obfuscation")
        (techniques . ("Metamorphic Variants"
                       "Control Flow Flattening"
                       "Instruction Substitution"))
        (januskey-utilities . ("rmo (Metamorphic Wipe)"
                               "rmr/undormr (Reversible File Transaction)"))))

      (distributed-verification
       ((platform . "BOINC")
        (distributed-proofs . "Z3 for SMT, Lean 4/Isabelle for rigour")
        (diversity-testing . "Thousands of obfuscated variants tested")
        (seed-security . "HashiCorp Vault")))))

    (technical-stack
     ((host-language . "Rust")
      (verification . ("Lean 4" "Z3" "Isabelle"))
      (persistence . ("Oblidb (Hardened ArangoDB)"
                      "OblimDB (MPR-reduced edge)"))
      (automation . ("Salt" "Podman"))
      (target-os . "Fedora Kinoite")))

    (research-phases
     ((phase-1
       ((years . "1-2")
        (focus . "Theoretical Foundations & Interpreter")
        (deliverables . ("Complete Formal Semantics"
                         "Verification Algorithms"
                         "Working Rust REPL"
                         "ACADEMIC_PORTFOLIO.md publication"))))

      (phase-2
       ((years . "2-3")
        (focus . "Compiler & Verification Platform")
        (deliverables . ("Complete Rust Compiler (WASM/RISC-V target)"
                         "BOINC server and worker images"
                         "Core JanusKey utilities"))))

      (phase-3
       ((years . "3-4")
        (focus . "Formal Proof & Publication")
        (deliverables . ("Soundness Proof of Obfuscation Transformation"
                         "Resource Analysis Verification"
                         "Final dissertation submission"))))))))

;;;============================================================================
;;; PART I: THEORETICAL FOUNDATIONS
;;;============================================================================

(define theoretical-foundations
  '((section . "Theoretical Elements Required")

    ;;=========================================================================
    ;; 1. COMPUTABILITY THEORY
    ;;=========================================================================
    (computability-theory
     ((primitive-recursive-functions
       ((purpose . "Foundation for Turing-incomplete deployment subset")
        (concepts . ("Bounded iteration (bounded-for)"
                     "Primitive recursion schemes"
                     "μ-recursion exclusion in deploy-time"))
        (key-theorem . "PR functions are exactly those computable with
                        bounded loops and no unbounded search")
        (application . "Deploy-time code restricted to PR-equivalent")))

      (total-vs-partial-functions
       ((purpose . "Guarantee termination in deployment code")
        (concepts . ("Total functions always terminate"
                     "Partial functions may diverge"
                     "Deployment code must be provably total"))
        (application . "All deploy-time functions require termination proofs")))

      (decidability
       ((purpose . "Understand limits of static analysis")
        (concepts . ("Halting problem undecidable for general programs"
                     "Termination decidable for primitive recursive subset"
                     "Resource bounds decidable for bounded programs"))
        (key-insight . "Restricting to PR makes termination decidable")))

      (loop-hierarchy
       ((purpose . "Classify computational power of loop constructs")
        (concepts . ("LOOP programs (primitive recursive)"
                     "WHILE programs (Turing complete)"
                     "Bounded FOR vs unbounded WHILE"))
        (application . "compile-time: WHILE allowed, deploy-time: LOOP only")))))

    ;;=========================================================================
    ;; 2. TYPE THEORY
    ;;=========================================================================
    (type-theory
     ((simple-types
       ((purpose . "Basic type checking for primitives and compounds")
        (concepts . ("Base types: u8, i32, bool, etc."
                     "Function types"
                     "Product types (structs)"
                     "Sum types (enums)"))
        (application . "Core type system for Oblíbený")))

      (dependent-types
       ((purpose . "Types that depend on values")
        (concepts . ("Array sizes as type parameters"
                     "Refinement types for bounds"
                     "Compile-time constant propagation"))
        (application . "(array u8 256) - size is part of type")
        (key-insight . "Required for static memory bound verification")))

      (linear-affine-types
       ((purpose . "Resource tracking and ownership")
        (concepts . ("Linear: used exactly once"
                     "Affine: used at most once"
                     "Relevant: used at least once"))
        (application . "Memory allocation tracking, capability consumption")
        (optional . #t)
        (benefit . "Compile-time memory safety without GC")))

      (effect-systems
       ((purpose . "Track side effects in types")
        (concepts . ("Pure functions (no effects)"
                     "IO effects"
                     "State effects"
                     "Exception effects"))
        (application . "(pure) and (idempotent) annotations")
        (key-insight . "Effects as type-level constraints")))

      (substructural-types
       ((purpose . "Control how values can be used")
        (concepts . ("Capability tokens as linear resources"
                     "One-shot capabilities"
                     "Budgeted capabilities"))
        (application . "capability-invoke with budgets")))))

    ;;=========================================================================
    ;; 3. FORMAL VERIFICATION
    ;;=========================================================================
    (formal-verification
     ((termination-analysis
       ((purpose . "Prove functions terminate")
        (techniques . ("Structural recursion on decreasing arguments"
                       "Well-founded orders"
                       "Ranking functions"
                       "Size-change termination"))
        (application . "(prove-terminates (strategy bounded-loop))")
        (decidability . "Decidable for PR subset, undecidable general")))

      (abstract-interpretation
       ((purpose . "Sound approximation of program behavior")
        (concepts . ("Abstract domains"
                     "Galois connections"
                     "Widening/narrowing"
                     "Fixpoint computation"))
        (application . "Resource bound inference, value range analysis")))

      (hoare-logic
       ((purpose . "Reason about program correctness")
        (concepts . ("Preconditions and postconditions"
                     "Loop invariants"
                     "Weakest precondition calculus"))
        (application . "Proving bounded-memory, no-side-effects")
        (optional . #t)))

      (model-checking
       ((purpose . "Exhaustively verify finite-state properties")
        (concepts . ("State space exploration"
                     "CTL/LTL temporal logic"
                     "Bounded model checking"))
        (application . "Call graph acyclicity, capability budgets")))

      (smt-solving
       ((purpose . "Solve logical constraints")
        (concepts . ("Satisfiability Modulo Theories"
                     "Bitvector arithmetic"
                     "Array theory"
                     "Linear integer arithmetic"))
        (tools . ("Z3" "CVC5" "Yices"))
        (application . "Bound verification, constraint solving")))))

    ;;=========================================================================
    ;; 4. COMPILER THEORY
    ;;=========================================================================
    (compiler-theory
     ((lexical-analysis
       ((purpose . "Convert source text to tokens")
        (concepts . ("Regular expressions"
                     "Finite automata"
                     "Maximal munch"))
        (s-expression-note . "Simpler for S-exprs: parens, atoms, strings")))

      (parsing-theory
       ((purpose . "Convert tokens to syntax tree")
        (approaches . (("Recursive descent" . "Simple, good for S-exprs")
                       ("Parser combinators" . "Composable, elegant")
                       ("PEG" . "Packrat parsing, no left recursion")
                       ("LL/LR" . "Overkill for S-expressions")))
        (recommendation . "Recursive descent or combinators for S-exprs")))

      (semantic-analysis
       ((purpose . "Meaning beyond syntax")
        (phases . ("Name resolution"
                   "Scope analysis"
                   "Type checking"
                   "Phase separation"
                   "Well-formedness checks"))))

      (intermediate-representations
       ((purpose . "Bridge source and target")
        (levels . (("HIR" . "High-level, close to source AST")
                   ("MIR" . "Mid-level, control flow explicit")
                   ("LIR" . "Low-level, close to machine")))
        (transforms . ("Lowering" "Optimization passes" "SSA conversion"))))

      (optimization
       ((purpose . "Improve code without changing semantics")
        (phases . ("Constant folding/propagation"
                   "Dead code elimination"
                   "Common subexpression elimination"
                   "Inlining"
                   "Loop optimizations"))
        (constraint . "Must preserve resource bounds")))

      (code-generation
       ((purpose . "Emit target code")
        (targets . ("x86-64 assembly"
                    "ARM assembly"
                    "RISC-V assembly"
                    "LLVM IR"
                    "WebAssembly"))
        (tasks . ("Instruction selection"
                  "Register allocation"
                  "Instruction scheduling"))))))

    ;;=========================================================================
    ;; 5. SECURITY THEORY
    ;;=========================================================================
    (security-theory
     ((capability-based-security
       ((purpose . "Replace ambient authority with explicit capabilities")
        (concepts . ("Principle of least authority"
                     "Capability as unforgeable token"
                     "Attenuation (can only reduce, not amplify)")
                     "Confinement")
        (application . "capability-invoke with CAP_* tokens")))

      (information-flow
       ((purpose . "Track how data propagates")
        (concepts . ("Confidentiality (no leaks up)"
                     "Integrity (no corruption down)"
                     "Noninterference"))
        (application . "Oblivious computing requirements")
        (optional . #t)))

      (side-channel-resistance
       ((purpose . "Prevent timing/power analysis attacks")
        (concepts . ("Constant-time operations"
                     "Memory access pattern hiding"
                     "Branch-free code"))
        (application . "Oblivious computing, edge security")
        (relevance . "Core to oblibeny ecosystem mission")))

      (code-obfuscation
       ((purpose . "Make reverse engineering difficult")
        (techniques . (("Name mangling" . "minimal level")
                       ("Control flow flattening" . "aggressive")
                       ("Opaque predicates" . "aggressive")
                       ("Instruction substitution" . "aggressive")
                       ("Dead code insertion" . "paranoid")
                       ("Code virtualization" . "paranoid")))
        (theoretical-limits . "Perfect obfuscation impossible (Barak et al.)")
        (practical-goal . "Raise reverse engineering cost")))

      (metamorphic-code
       ((purpose . "Code that rewrites itself while preserving semantics")
        (concepts . ("Semantic equivalence"
                     "Instruction substitution rules"
                     "Register reassignment"
                     "Code transposition"))
        (application . "metamorphic-if, rewrite-self")
        (security-model . "Different binary per deployment")))))

    ;;=========================================================================
    ;; 6. CATEGORY THEORY (Optional but Elegant)
    ;;=========================================================================
    (category-theory
     ((optional . #t)
      (benefit . "Unified framework for compiler transformations")

      (functors
       ((purpose . "Structure-preserving maps")
        (application . "AST transformations, IR lowering")))

      (monads
       ((purpose . "Sequence effectful computations")
        (application . "Error handling, state threading in compiler")))

      (natural-transformations
       ((purpose . "Transform between functors")
        (application . "Compiler pass composition")))

      (f-algebras
       ((purpose . "Recursive data types and folds")
        (application . "AST traversal, catamorphisms")))))))

;;;============================================================================
;;; PART II: PRACTICAL IMPLEMENTATION REQUIREMENTS
;;;============================================================================

(define practical-requirements
  '((section . "Practical Implementation Work")

    ;;=========================================================================
    ;; 1. FRONTEND (Source → AST)
    ;;=========================================================================
    (frontend
     ((lexer-tokenizer
       ((deliverable . "S-expression tokenizer")
        (inputs . "Source text (UTF-8)")
        (outputs . "Token stream")
        (tokens . ("LPAREN" "RPAREN"
                   "IDENTIFIER" "KEYWORD"
                   "NUMBER" "STRING" "BOOLEAN"
                   "COMMENT"))
        (features . ("Source location tracking"
                     "Error recovery"
                     "Unicode support"))
        (effort . "low")
        (complexity . "simple")))

      (parser
       ((deliverable . "S-expression parser → AST")
        (approach . "Recursive descent")
        (outputs . "Concrete Syntax Tree")
        (features . ("Syntax error messages"
                     "Source span preservation"
                     "Comment attachment"))
        (effort . "low")
        (complexity . "simple for S-exprs")))

      (ast-construction
       ((deliverable . "Typed AST nodes")
        (node-types . ("TopLevelForm" "DeploymentSpec" "FunctionDef"
                       "Expression" "Statement" "TypeExpr"
                       "Instruction" "etc."))
        (effort . "medium")
        (complexity . "moderate")))

      (macro-expander
       ((deliverable . "defmacro expansion engine")
        (features . ("Hygienic macros (optional)"
                     "Compile-time evaluation"
                     "Expansion tracing"))
        (effort . "medium-high")
        (complexity . "moderate-complex")))))

    ;;=========================================================================
    ;; 2. MIDDLE-END (Analysis & Transformation)
    ;;=========================================================================
    (middle-end
     ((name-resolution
       ((deliverable . "Symbol table, scope analysis")
        (features . ("Module imports/exports"
                     "Shadowing rules"
                     "Forward references"))
        (effort . "medium")
        (complexity . "moderate")))

      (type-checker
       ((deliverable . "Type inference and checking engine")
        (features . ("Bidirectional type checking"
                     "Constraint generation"
                     "Error reporting with suggestions"))
        (effort . "high")
        (complexity . "complex")))

      (phase-separator
       ((deliverable . "Compile-time vs deploy-time classifier")
        (features . ("Identify compile-only constructs"
                     "Verify deploy-time restrictions"
                     "Phase violation errors"))
        (critical . #t)
        (effort . "medium")
        (complexity . "moderate")))

      (call-graph-analyzer
       ((deliverable . "Static call graph construction")
        (features . ("Direct call tracking"
                     "Cycle detection (recursion)"
                     "Call depth calculation"
                     "Reachability analysis"))
        (critical . #t)
        (effort . "medium")
        (complexity . "moderate")))

      (termination-checker
       ((deliverable . "Termination proof verifier")
        (approaches . ("Bounded loop verification"
                       "Decreasing measure tracking"
                       "Size-change graphs"))
        (critical . #t)
        (effort . "high")
        (complexity . "complex")))

      (resource-analyzer
       ((deliverable . "Static resource bound calculator")
        (resources . ("Stack depth"
                      "Memory allocation"
                      "Iteration count"
                      "Call depth"))
        (features . ("Bound propagation"
                     "Overflow detection"
                     "Report generation"))
        (critical . #t)
        (effort . "high")
        (complexity . "complex")))

      (purity-checker
       ((deliverable . "Side-effect analysis")
        (features . ("Pure function verification"
                     "Idempotence checking"
                     "Effect inference"))
        (effort . "medium")
        (complexity . "moderate")))

      (ir-lowering
       ((deliverable . "HIR → MIR transformation")
        (transforms . ("Control flow linearization"
                       "Pattern match compilation"
                       "Closure conversion"))
        (effort . "high")
        (complexity . "complex")))))

    ;;=========================================================================
    ;; 3. BACKEND (IR → Target)
    ;;=========================================================================
    (backend
     ((optimizer
       ((deliverable . "MIR optimization passes")
        (passes . ("Constant folding"
                   "Dead code elimination"
                   "Common subexpression elimination"
                   "Inlining (with bound checking)"))
        (constraint . "Must preserve resource bounds")
        (effort . "high")
        (complexity . "complex")))

      (obfuscator
       ((deliverable . "Code obfuscation engine")
        (levels . (("none" . "No transformation")
                   ("minimal" . "Name mangling, layout randomization")
                   ("aggressive" . "Control flow, opaque predicates")
                   ("paranoid" . "Virtualization, heavy metamorphism")))
        (effort . "very-high")
        (complexity . "very-complex")))

      (code-generator
       ((deliverable . "Target code emission")
        (targets . (("x86-64" . "Primary, per grammar")
                    ("ARM64" . "Mobile/embedded")
                    ("RISC-V" . "Per ecosystem")
                    ("WASM" . "Web deployment")))
        (tasks . ("Instruction selection"
                  "Register allocation"
                  "Stack layout"
                  "ABI compliance"))
        (effort . "very-high")
        (complexity . "very-complex")))

      (assembler-linker
       ((deliverable . "Object file generation")
        (options . ("Emit assembly text"
                    "Direct object emission"
                    "Use system assembler"))
        (effort . "medium-high")
        (complexity . "moderate-complex")))))

    ;;=========================================================================
    ;; 4. RUNTIME SUPPORT
    ;;=========================================================================
    (runtime
     ((capability-runtime
       ((deliverable . "Capability token management")
        (features . ("Token validation"
                     "Budget tracking"
                     "Invocation dispatch"))
        (effort . "medium")
        (complexity . "moderate")))

      (resource-enforcer
       ((deliverable . "Runtime resource limit enforcement")
        (features . ("Stack overflow protection"
                     "Memory limit enforcement"
                     "Iteration counting (paranoid mode)"))
        (note . "Most enforcement is compile-time")
        (effort . "low-medium")
        (complexity . "simple-moderate")))

      (minimal-runtime
       ((deliverable . "Bare-metal runtime for edge deployment")
        (features . ("No heap allocation"
                     "No OS dependencies"
                     "Static initialization"))
        (effort . "medium")
        (complexity . "moderate")))))

    ;;=========================================================================
    ;; 5. TOOLING
    ;;=========================================================================
    (tooling
     ((repl
       ((deliverable . "Interactive development environment")
        (features . ("Compile-time code execution"
                     "AST inspection"
                     "Type queries"))
        (effort . "medium")
        (complexity . "moderate")))

      (formatter
       ((deliverable . "Code formatter (oblíbený-fmt)")
        (effort . "low")
        (complexity . "simple")))

      (lsp-server
       ((deliverable . "Language Server Protocol implementation")
        (features . ("Syntax highlighting"
                     "Go to definition"
                     "Type hover"
                     "Diagnostics"))
        (effort . "high")
        (complexity . "complex")))

      (debugger
       ((deliverable . "Debug information and debugger support")
        (features . ("DWARF emission"
                     "Source mapping"
                     "Breakpoints"))
        (effort . "high")
        (complexity . "complex")))

      (package-manager
       ((deliverable . "Dependency management")
        (features . ("Version resolution"
                     "Capability manifests"
                     "Reproducible builds"))
        (effort . "high")
        (complexity . "complex")))))))

;;;============================================================================
;;; PART III: HOST LANGUAGE ANALYSIS
;;;============================================================================

(define host-language-analysis
  '((section . "Host Language Selection")

    ;;=========================================================================
    ;; DECISION: RUST (Per Research Proposal)
    ;;=========================================================================
    (decision
     ((host-language . "Rust")
      (rationale . "Per PhD research proposal: chosen for memory safety,
                    performance, and production-grade compiler development.")
      (verification-tools . ("Lean 4" "Z3" "Isabelle"))
      (distributed-platform . "BOINC")
      (persistence . ("Oblidb (Hardened ArangoDB)"
                      "OblimDB (MPR-reduced edge)"))
      (automation . ("Salt" "Podman" "Fedora Kinoite"))))

    (evaluation-criteria
     ("S-expression handling (native advantage)"
      "Metaprogramming / macro system"
      "Type system strength"
      "Formal verification integration"
      "Low-level code generation"
      "Ecosystem maturity"
      "Project alignment (existing Scheme usage)"))

    (candidates
     ;;=========================================================================
     ;; OPTION A: Guile Scheme
     ;;=========================================================================
     ((guile-scheme
       ((summary . "GNU's Scheme implementation, already used in project")

        (pros . ("Native S-expression handling"
                 "Powerful macro system (syntax-case, syntax-rules)"
                 "Project already uses Scheme"
                 "Good FFI for native code"
                 "Compiles to native via lightning/libgccjit"
                 "REPL-driven development"
                 "GNU ecosystem integration"))

        (cons . ("Weaker static typing"
                 "Formal verification not built-in"
                 "Smaller community than mainstream"
                 "Performance concerns for heavy compilation"))

        (s-expr-handling . "excellent (native)")
        (metaprogramming . "excellent")
        (type-system . "poor (dynamic)")
        (verification . "poor (external tools needed)")
        (codegen . "good (FFI, inline assembly)")
        (ecosystem . "moderate")
        (alignment . "excellent")

        (overall-score . 7.5)
        (recommendation . "Good choice for rapid prototyping")))

      ;;=========================================================================
      ;; OPTION B: Racket
      ;;=========================================================================
      (racket
       ((summary . "Language-oriented programming platform")

        (pros . ("Designed for building languages"
                 "Native S-expression handling"
                 "Typed Racket for gradual typing"
                 "Excellent macro system"
                 "DrRacket IDE"
                 "Strong documentation"
                 "Contract system for verification"))

        (cons . ("Different Scheme dialect"
                 "Heavier runtime"
                 "Less suitable for embedded targets"
                 "Native codegen requires more work"))

        (s-expr-handling . "excellent (native)")
        (metaprogramming . "excellent")
        (type-system . "good (Typed Racket)")
        (verification . "moderate (contracts)")
        (codegen . "moderate")
        (ecosystem . "good")
        (alignment . "good")

        (overall-score . 8.0)
        (recommendation . "Best for language experimentation")))

      ;;=========================================================================
      ;; OPTION C: Rust
      ;;=========================================================================
      (rust
       ((summary . "Systems language with strong safety guarantees")

        (pros . ("Strong static typing"
                 "Memory safety without GC"
                 "Excellent for low-level codegen"
                 "Great tooling (cargo, clippy)"
                 "Large ecosystem"
                 "Good for production compilers"))

        (cons . ("S-expressions require manual parsing"
                 "More verbose for AST manipulation"
                 "Slower iteration than Lisp"
                 "No native macro for S-exprs"))

        (s-expr-handling . "moderate (libraries exist)")
        (metaprogramming . "moderate (proc macros)")
        (type-system . "excellent")
        (verification . "good (type system + miri)")
        (codegen . "excellent (LLVM, cranelift)")
        (ecosystem . "excellent")
        (alignment . "poor")

        (overall-score . 7.0)
        (recommendation . "Good for production compiler")))

      ;;=========================================================================
      ;; OPTION D: OCaml
      ;;=========================================================================
      (ocaml
       ((summary . "Functional language, traditional compiler choice")

        (pros . ("Algebraic data types (perfect for ASTs)"
                 "Pattern matching"
                 "Strong static typing with inference"
                 "Many compilers written in OCaml"
                 "Good performance"))

        (cons . ("S-expressions not native"
                 "Smaller ecosystem than Rust"
                 "Less metaprogramming"
                 "Learning curve"))

        (s-expr-handling . "moderate")
        (metaprogramming . "moderate (ppx)")
        (type-system . "excellent")
        (verification . "good")
        (codegen . "good")
        (ecosystem . "moderate")
        (alignment . "poor")

        (overall-score . 7.5)
        (recommendation . "Traditional but solid choice")))

      ;;=========================================================================
      ;; OPTION E: Haskell
      ;;=========================================================================
      (haskell
       ((summary . "Pure functional with advanced type system")

        (pros . ("Very strong type system"
                 "Excellent for formal semantics"
                 "Algebraic data types"
                 "Parser combinator libraries (parsec, megaparsec)"
                 "Lazy evaluation for DSLs"))

        (cons . ("S-expressions not native"
                 "Lazy evaluation complicates resource analysis"
                 "Steeper learning curve"
                 "GHC dependency"))

        (s-expr-handling . "moderate")
        (metaprogramming . "good (Template Haskell)")
        (type-system . "excellent")
        (verification . "excellent")
        (codegen . "good")
        (ecosystem . "good")
        (alignment . "poor")

        (overall-score . 7.5)
        (recommendation . "Good for semantics-first approach")))

      ;;=========================================================================
      ;; OPTION F: Lean 4
      ;;=========================================================================
      (lean-4
       ((summary . "Theorem prover with general-purpose programming")

        (pros . ("Built-in formal verification"
                 "Dependent types"
                 "Can prove compiler correctness"
                 "Modern syntax"
                 "Good performance"))

        (cons . ("S-expressions not native"
                 "Smaller ecosystem"
                 "Steeper learning curve"
                 "Newer, less battle-tested"))

        (s-expr-handling . "poor")
        (metaprogramming . "good (macros)")
        (type-system . "excellent (dependent)")
        (verification . "excellent (native)")
        (codegen . "moderate")
        (ecosystem . "small")
        (alignment . "poor")

        (overall-score . 7.0)
        (recommendation . "If formal verification is paramount")))))

    ;;=========================================================================
    ;; RECOMMENDATION
    ;;=========================================================================
    (recommendation
     ((primary . "Racket")
      (rationale . "Best balance of S-expression handling, language tooling,
                    gradual typing (Typed Racket), and ecosystem maturity.
                    Designed specifically for building languages.")

      (secondary . "Guile Scheme")
      (secondary-rationale . "Project alignment, simpler deployment,
                              good enough for MVP, can migrate later.")

      (production-alternative . "Rust with nom/chumsky for parsing")
      (production-rationale . "If targeting production deployment with
                               maximum performance and safety.")

      (bootstrap-strategy
       ((phase-1 . "Prototype in Racket or Guile (rapid iteration)")
        (phase-2 . "Stabilize semantics, formalize in Lean (optional)")
        (phase-3 . "Production compiler in Rust (if needed)")
        (phase-4 . "Self-hosting in Oblíbený (eventual goal)")))))))

;;;============================================================================
;;; PART IV: COMPILATION ARCHITECTURE
;;;============================================================================

(define compilation-architecture
  '((section . "Compilation Stages")

    (answer . "THREE-STAGE compilation required")

    (rationale
     "The dual-phase model (compile-time Turing-complete → deploy-time
      Turing-incomplete) plus macro expansion naturally requires three
      distinct stages. Additionally, the security model demands clear
      separation between trusted (development) and untrusted (deployment)
      code paths.")

    ;;=========================================================================
    ;; STAGE 0: EXPANSION (Turing-Complete, Trusted)
    ;;=========================================================================
    (stage-0
     ((name . "Expansion Stage")
      (phase . "compile-time")
      (turing . "complete")
      (trust . "full")

      (inputs . ("Source files (.obl)"
                 "Macro definitions"
                 "Conditional assembly flags"))

      (operations . ("Lexing and parsing"
                     "Macro expansion (defmacro)"
                     "Conditional assembly (ifdef)"
                     "compile-time block execution"
                     "Constant evaluation"
                     "transaction execution (for testing)"))

      (outputs . ("Expanded AST"
                  "Compile-time computed values"
                  "Expansion trace (for debugging)"))

      (properties . ("May loop forever (Turing-complete)"
                     "May use syscalls"
                     "May use unbounded recursion"
                     "Full metaprogramming power"))

      (diagram . "
        Source.obl ──→ [Lexer] ──→ Tokens
                                    │
                                    ▼
                              [Parser] ──→ CST
                                           │
                                           ▼
                              [Macro Expander] ◄── defmacro definitions
                                    │
                                    ▼ (compile-time blocks executed here)
                              Expanded AST
      ")))

    ;;=========================================================================
    ;; STAGE 1: VERIFICATION (Analysis, Critical)
    ;;=========================================================================
    (stage-1
     ((name . "Verification Stage")
      (phase . "compile-time")
      (turing . "analyzer is complete, analyzed code restricted")
      (trust . "critical")

      (inputs . ("Expanded AST"
                 "Deployment spec"
                 "Resource bounds"
                 "Security constraints"))

      (operations . ("Name resolution"
                     "Type checking / inference"
                     "Phase separation verification"
                     "Call graph construction"
                     "Recursion detection (must fail for deploy)"
                     "Termination proof checking"
                     "Resource bound calculation"
                     "Purity verification"
                     "Capability requirement collection"))

      (outputs . ("Typed IR (HIR)"
                  "Verified proofs"
                  "Resource certificates"
                  "Capability manifest"
                  "Error diagnostics"))

      (properties . ("MUST verify deploy-time code is Turing-incomplete"
                     "MUST verify resource bounds are satisfied"
                     "MUST verify call graph is acyclic"
                     "MUST verify termination proofs"
                     "Compilation fails if verification fails"))

      (critical-checks
       ((recursion-free . "Call graph must be DAG")
        (bounded-loops . "All loops have compile-time constant bounds")
        (bounded-memory . "Total allocation ≤ max-memory")
        (bounded-stack . "Stack depth ≤ max-stack-depth")
        (bounded-calls . "Call depth ≤ max-call-depth")
        (termination . "All functions have valid termination proofs")
        (purity . "Functions marked pure have no side effects")
        (phase-correct . "No compile-only code in deploy-time")))

      (diagram . "
        Expanded AST ──→ [Name Resolution] ──→ Resolved AST
                                                    │
                                                    ▼
                                            [Type Checker]
                                                    │
                                                    ▼
                         ┌──────────────────────────┴──────────────────────────┐
                         │              VERIFICATION BATTERY                    │
                         │  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐   │
                         │  │Phase Check  │ │Call Graph   │ │Termination  │   │
                         │  │             │ │Analysis     │ │Checker      │   │
                         │  └─────────────┘ └─────────────┘ └─────────────┘   │
                         │  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐   │
                         │  │Resource     │ │Purity       │ │Capability   │   │
                         │  │Analyzer     │ │Checker      │ │Collector    │   │
                         │  └─────────────┘ └─────────────┘ └─────────────┘   │
                         └────────────────────────┬────────────────────────────┘
                                                  │
                                    ▼ (all checks pass)
                              Verified HIR + Proofs
      ")))

    ;;=========================================================================
    ;; STAGE 2: GENERATION (Deploy-Time, Restricted)
    ;;=========================================================================
    (stage-2
     ((name . "Generation Stage")
      (phase . "deploy-time")
      (turing . "incomplete (by construction)")
      (trust . "minimal")

      (inputs . ("Verified HIR"
                 "Resource certificates"
                 "Target profile"
                 "Obfuscation level"
                 "Capability grants"))

      (operations . ("HIR → MIR lowering"
                     "Optimization (bound-preserving)"
                     "Obfuscation transformation"
                     "Metamorphic variant generation"
                     "Register allocation"
                     "Instruction selection"
                     "Code emission"))

      (outputs . ("Target binary / object file"
                  "Embedded resource limits"
                  "Capability requirements"))

      (properties . ("Deterministic (same input → same semantics)"
                     "Non-deterministic variants (obfuscation)"
                     "Resource limits embedded in binary"
                     "No new capabilities can be introduced"
                     "Output is Turing-incomplete by construction"))

      (obfuscation-pipeline
       ((none . "Direct codegen")
        (minimal . "Name mangling → Layout randomization → Codegen")
        (aggressive . "Name mangling → CFG flattening → Opaque predicates
                       → Instruction substitution → Codegen")
        (paranoid . "Name mangling → CFG flattening → Opaque predicates
                     → Dead code insertion → Code virtualization
                     → Instruction substitution → Metamorphic variants
                     → Codegen")))

      (diagram . "
        Verified HIR ──→ [Lowering] ──→ MIR
                                         │
                                         ▼
                                  [Optimizer]
                                         │
                                         ▼
                         ┌───────────────┴───────────────┐
                         │      OBFUSCATION PIPELINE     │
                         │   (level-dependent transforms) │
                         └───────────────┬───────────────┘
                                         │
                                         ▼
                                  [Codegen] ◄── Target Profile
                                         │
                                         ▼
                                  Target Binary
      ")))

    ;;=========================================================================
    ;; FULL PIPELINE DIAGRAM
    ;;=========================================================================
    (full-pipeline
     "
     ╔════════════════════════════════════════════════════════════════════════╗
     ║                    OBLÍBENÝ COMPILATION PIPELINE                        ║
     ╠════════════════════════════════════════════════════════════════════════╣
     ║                                                                         ║
     ║  SOURCE.obl                                                             ║
     ║       │                                                                 ║
     ║       ▼                                                                 ║
     ║  ╔═══════════════════════════════════════════════════════════════════╗ ║
     ║  ║ STAGE 0: EXPANSION (Turing-Complete)                              ║ ║
     ║  ║   • Lex/Parse                                                     ║ ║
     ║  ║   • Macro expansion                                               ║ ║
     ║  ║   • compile-time block execution                                  ║ ║
     ║  ║   • Conditional assembly                                          ║ ║
     ║  ╚═══════════════════════════════════════════════════════════════════╝ ║
     ║       │                                                                 ║
     ║       ▼ Expanded AST                                                    ║
     ║  ╔═══════════════════════════════════════════════════════════════════╗ ║
     ║  ║ STAGE 1: VERIFICATION (Critical)                                  ║ ║
     ║  ║   • Type checking                                                 ║ ║
     ║  ║   • Phase separation                                              ║ ║
     ║  ║   • Call graph (no recursion)         ┌─────────────────────────┐ ║ ║
     ║  ║   • Termination proofs                │ DEPLOYMENT SPEC         │ ║ ║
     ║  ║   • Resource bounds ◄─────────────────│ • max-iterations: N     │ ║ ║
     ║  ║   • Purity verification               │ • max-memory: M         │ ║ ║
     ║  ║   • Capability collection             │ • max-call-depth: D     │ ║ ║
     ║  ║                                       └─────────────────────────┘ ║ ║
     ║  ╚═══════════════════════════════════════════════════════════════════╝ ║
     ║       │                                                                 ║
     ║       ▼ Verified HIR + Proofs                                           ║
     ║  ╔═══════════════════════════════════════════════════════════════════╗ ║
     ║  ║ STAGE 2: GENERATION (Turing-Incomplete Output)                    ║ ║
     ║  ║   • Optimization                                                  ║ ║
     ║  ║   • Obfuscation (none/minimal/aggressive/paranoid)                ║ ║
     ║  ║   • Target codegen (x86-64, ARM, RISC-V, WASM)                    ║ ║
     ║  ║   • Resource limit embedding                                      ║ ║
     ║  ╚═══════════════════════════════════════════════════════════════════╝ ║
     ║       │                                                                 ║
     ║       ▼                                                                 ║
     ║  DEPLOY BINARY (Turing-incomplete, resource-bounded, obfuscated)        ║
     ║                                                                         ║
     ╚════════════════════════════════════════════════════════════════════════╝
    ")

    ;;=========================================================================
    ;; WHY THREE STAGES (NOT TWO OR FOUR)
    ;;=========================================================================
    (stage-justification
     ((why-not-two
       "Two stages would conflate macro expansion with verification.
        Macros are Turing-complete and may generate arbitrary code.
        Verification must analyze the *result* of expansion, not
        the macro definitions themselves. Combining them loses the
        clear security boundary.")

      (why-not-four
       "Four stages would unnecessarily separate optimization and
        codegen. Since obfuscation is semantics-preserving, it can
        be integrated with codegen. The three-stage model maps
        cleanly to: (1) metaprogramming, (2) verification,
        (3) emission.")

      (why-three
       "Three stages provide:
        1. Clear Turing-complete / incomplete boundary
        2. Verification as explicit checkpoint
        3. Clean separation of concerns
        4. Natural trust boundaries
        5. Debuggable intermediate artifacts")))))

;;;============================================================================
;;; PART V: ADVANCED DEFENSE THEORY
;;; Homoiconicity, Reflection, Reversibility in Turing-Incomplete Context
;;;============================================================================

(define advanced-defense-theory
  '((section . "Defense Capabilities Across Turing Boundary")

    ;;=========================================================================
    ;; CRITICAL INSIGHT: These Concepts Do NOT Require Turing Completeness
    ;;=========================================================================
    (key-insight
     ((statement . "Homoiconicity, Reflection, and Reversibility do NOT
                    inherently require Turing completeness.")

      (implication . "The Deployment Subset can retain LIMITED but USEFUL
                      forms of these defense mechanisms while remaining
                      provably terminating and resource-bounded.")

      (theoretical-basis
       ((homoiconicity
         ((requires-turing . #f)
          (explanation . "Code-as-data representation (S-expressions) is
                          a syntactic property, not a computational one.
                          JSON/XML are homoiconic but not Turing-complete.")))

        (reflection
         ((requires-turing . "Only for intercession, not introspection")
          (explanation . "Introspection (reading structure) needs no
                          computation. Intercession (modifying structure)
                          can be bounded. Full dynamic code generation
                          requires Turing completeness.")))

        (reversibility
         ((requires-turing . #f)
          (explanation . "Reversible operations (XOR, permutations,
                          bijective functions) are information-preserving
                          but computationally bounded. Reversible Turing
                          machines exist, but reversibility itself is
                          orthogonal to computational power.")))))))

    ;;=========================================================================
    ;; DEFENSE CAPABILITIES BY COMPILATION PHASE
    ;;=========================================================================
    (defense-by-phase
     ((compile-time-defenses
       ((phase . "Stage 0: Expansion")
        (turing . "complete")
        (capabilities
         (("Full Metamorphic Generation"
           ((description . "Arbitrary code rewriting via macros")
            (mechanism . "defmacro, metamorphic-if, rewrite-self")
            (power . "unlimited")))

          ("Dynamic Reflection"
           ((description . "Runtime introspection and modification")
            (mechanism . "Compile-time block execution")
            (power . "full intercession")))

          ("Reversible Transactions"
           ((description . "Undo arbitrary operations")
            (mechanism . "transaction, commit, rollback")
            (power . "unbounded state reversal")))

          ("Unbounded Obfuscation Passes"
           ((description . "Iterate obfuscation until entropy threshold")
            (mechanism . "while loops in compile-time blocks")
            (power . "arbitrary iterations")))

          ("Polymorphic Variant Generation"
           ((description . "Generate thousands of semantically equivalent forms")
            (mechanism . "Macro-driven instruction substitution")
            (power . "combinatorial explosion")))))))

      (deploy-time-defenses
       ((phase . "Stage 2: Generation → Runtime")
        (turing . "incomplete")
        (capabilities
         (("Static Polymorphic Variants"
           ((description . "Pre-computed alternative representations")
            (mechanism . "Compile-time generated lookup tables")
            (limitation . "Fixed set, chosen at compile time")
            (example . "(+ a b) → (- a (- 0 b)) → (xor a (xor a (+ a b)))")))

          ("Metamorphic Seeding"
           ((description . "Deterministic self-modification from seed")
            (mechanism . "Bounded permutation of code blocks at load time")
            (limitation . "Permutation count ≤ max-iterations")
            (example . "Seed selects from N! orderings of M code blocks")))

          ("Hardware-Bound Keys"
           ((description . "Tie decryption to device-specific values")
            (mechanism . "capability-invoke for hardware attestation")
            (limitation . "Key derivation must be bounded")
            (defense . "Binary useless without specific hardware")))

          ("Bounded Reversible Operations"
           ((description . "Information-preserving transforms")
            (mechanism . "XOR, rotate, permute, Feistel networks")
            (limitation . "Fixed round count, no dynamic loops")
            (example . "(xor (xor data key1) key2) ; reversible")))

          ("Control Flow Integrity"
           ((description . "Ensure execution follows intended path")
            (mechanism . "Embedded checksums, canary values")
            (limitation . "Verification count bounded")
            (defense . "Tampering detected, fail triggered")))

          ("Checksum Armoring"
           ((description . "Self-verification of code integrity")
            (mechanism . "CRC/hash embedded in code section")
            (limitation . "Hash computation must be bounded")
            (defense . "Modified code fails checksum")))

          ("Opaque Predicates (Static)"
           ((description . "Conditions that always evaluate same way")
            (mechanism . "Compile-time proven tautologies/contradictions")
            (limitation . "No runtime computation needed")
            (example . "(if (= (* 7 11) 77) real-code fake-code)")))

          ("Dead Code Insertion"
           ((description . "Unreachable code paths for confusion")
            (mechanism . "Compile-time inserted, statically unreachable")
            (limitation . "Does not affect resource bounds")
            (defense . "Analysts waste time on fake paths")))

          ("Data Obfuscation"
           ((description . "Non-obvious data representation")
            (mechanism . "Lookup tables, split values, arithmetic encoding")
            (limitation . "Table size bounded by max-memory")
            (example . "key = table[i] ^ table[j] ; derived at runtime")))))))))

    ;;=========================================================================
    ;; OBFUSCATION STRATEGY BY LEVEL
    ;;=========================================================================
    (obfuscation-strategies
     ((none
       ((compile-time . "No transformations")
        (deploy-time . "Direct codegen")
        (use-case . "Development, debugging")))

      (minimal
       ((compile-time . ("Name mangling"
                         "Layout randomization"))
        (deploy-time . ("Symbol stripping"
                        "Section reordering"))
        (use-case . "Basic IP protection")))

      (aggressive
       ((compile-time . ("Polymorphic variant selection"
                         "Instruction substitution"
                         "Control flow flattening"
                         "Opaque predicate insertion"))
        (deploy-time . ("Metamorphic seeding"
                        "Dead code blocks"
                        "Data encoding"
                        "Control flow integrity"))
        (use-case . "Anti-reverse engineering")))

      (paranoid
       ((compile-time . ("All aggressive transforms"
                         "Virtual machine generation"
                         "Multi-layer encryption"
                         "Trap insertion"
                         "Diversity maximization"))
        (deploy-time . ("Hardware binding"
                        "Environment verification"
                        "Self-destruct triggers"
                        "Checksum armoring"
                        "Maximum metamorphic permutation"))
        (use-case . "Hostile environment deployment")))))

    ;;=========================================================================
    ;; REVERSIBILITY IN TURING-INCOMPLETE CONTEXT
    ;;=========================================================================
    (bounded-reversibility
     ((concept . "Reversible operations without Turing completeness")

      (allowed-operations
       (("XOR" . "a ⊕ b ⊕ b = a")
        ("Rotate" . "ROL(ROR(x, n), n) = x")
        ("Permute" . "P⁻¹(P(x)) = x")
        ("Feistel" . "Symmetric round function")
        ("Affine" . "ax + b mod n (when gcd(a,n)=1)")))

      (forbidden-operations
       (("Unbounded iteration" . "Cannot reverse arbitrary loop count")
        ("Dynamic allocation" . "Cannot track arbitrary heap state")
        ("Recursion" . "Cannot reverse unbounded stack")))

      (use-cases
       (("Selective decryption"
         ((description . "Decrypt only with valid capability")
          (mechanism . "XOR chain with capability-derived key")
          (bounded . "Fixed key schedule rounds")))

        ("Tamper response"
         ((description . "Reverse obfuscation only if unmodified")
          (mechanism . "Checksum gates decryption key")
          (bounded . "Single checksum computation")))

        ("Forensic resistance"
         ((description . "Leave no trace after execution")
          (mechanism . "XOR working memory with final state")
          (bounded . "Linear pass over fixed memory region")))))))

    ;;=========================================================================
    ;; HOMOICONICITY IN DEPLOYMENT
    ;;=========================================================================
    (deployment-homoiconicity
     ((concept . "Code-as-data without Turing completeness")

      (compile-time-use
       ((description . "Full S-expression manipulation")
        (power . "Macros can generate arbitrary code")
        (example . "(defmacro obfuscate (expr) ...arbitrary transform...)")))

      (deploy-time-use
       ((description . "Static AST templates, no runtime generation")
        (limitation . "Cannot create new code at runtime")
        (allowed . ("Pre-computed variant selection"
                    "Table-driven dispatch"
                    "Compile-time specialized functions"))
        (forbidden . ("eval"
                      "Runtime code synthesis"
                      "Dynamic macro expansion"))

        (example . "
          ; Compile-time: generate 16 variants
          (compile-time
            (for variant 0 16
              (emit-function (concat 'process_ variant)
                (obfuscate-with-seed base-function variant))))

          ; Deploy-time: select variant based on capability
          (deploy-time
            (let variant-id: u8 (capability CAP_VARIANT_SELECT))
            (bounded-for i 0 16
              (if (= i variant-id)
                  (call (table-lookup variant-table i) (args)))))
        ")))))

    ;;=========================================================================
    ;; DEFENSE WORTHLESSNESS PRINCIPLE
    ;;=========================================================================
    (worthlessness-principle
     ((goal . "Deployed binary is WORTHLESS if reverse-engineered")

      (strategies
       (("Minimal functionality"
         ((description . "Code does ONE thing, nothing else useful")
          (mechanism . "Extreme DSL restriction")
          (result . "No reusable components")))

        ("Hardware dependency"
         ((description . "Requires specific device to function")
          (mechanism . "Capability-based hardware attestation")
          (result . "Binary useless on other hardware")))

        ("Context dependency"
         ((description . "Requires runtime context to decrypt logic")
          (mechanism . "Sensor readings, timestamps as key material")
          (result . "Static analysis yields encrypted garbage")))

        ("Self-destruction"
         ((description . "Erases itself if tampering detected")
          (mechanism . "Checksum-gated memory wipe")
          (result . "Attacker gets nothing")))

        ("Decoy logic"
         ((description . "Fake code paths outnumber real ones")
          (mechanism . "Static dead code insertion")
          (result . "Which path is real?")))))

      (maximal-obfuscation-principle
       "While deployed code is MINIMAL for function, it should be
        MAXIMAL for obfuscation. Every bit of headroom in resource
        bounds should be used for defense, not functionality.")))))

;;;============================================================================
;;; SUMMARY
;;;============================================================================

(define implementation-summary
  '((theoretical-elements . 6)
    (theoretical-categories . ("Computability Theory"
                               "Type Theory"
                               "Formal Verification"
                               "Compiler Theory"
                               "Security Theory"
                               "Category Theory (optional)"))

    (practical-components . 5)
    (practical-categories . ("Frontend (lexer, parser, AST, macros)"
                             "Middle-end (analysis, verification)"
                             "Backend (optimization, obfuscation, codegen)"
                             "Runtime (capabilities, resource enforcement)"
                             "Tooling (REPL, LSP, debugger, package manager)"))

    (host-language . "Rust (per PhD research proposal)")
    (verification-backend . ("Z3 (SMT)" "Lean 4 / Isabelle (proofs)"))
    (distributed-platform . "BOINC")
    (compilation-stages . 3)
    (stage-names . ("Expansion" "Verification" "Generation"))
    (target-platforms . ("WASM" "RISC-V" "x86-64"))

    (paradigm . "Dual-Language Paradigm (DLP)")
    (key-technique . "Maximal Principle Reduction (MPR)")))

;;; End of implementation-specification.scm
