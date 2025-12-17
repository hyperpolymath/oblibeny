# Oblíbený Language Roadmap

> **The Dual-Language Paradigm: Achieving Compile-Time Security Guarantees and Anti-Tamper Resilience for Edge Computing via Maximal Principle Reduction**

This roadmap outlines the development phases for the Oblíbený programming language, compiler, tooling, and ecosystem.

---

## Table of Contents

1. [Vision and Goals](#vision-and-goals)
2. [Phase 0: Foundation (Current)](#phase-0-foundation-current)
3. [Phase 1: Core Compiler](#phase-1-core-compiler)
4. [Phase 2: Verification Backend](#phase-2-verification-backend)
5. [Phase 3: Runtime and Interpreter](#phase-3-runtime-and-interpreter)
6. [Phase 4: Tooling](#phase-4-tooling)
7. [Phase 5: Standard Library](#phase-5-standard-library)
8. [Phase 6: Frameworks](#phase-6-frameworks)
9. [Phase 7: Ecosystem](#phase-7-ecosystem)
10. [Long-term Vision](#long-term-vision)

---

## Vision and Goals

### Core Principles

1. **Dual-Language Paradigm**: Turing-complete development → Turing-incomplete deployment
2. **Maximal Principle Reduction**: Static resource bounds determined at compile time
3. **Provable Termination**: All deploy-time code must terminate
4. **Capability-Based Security**: No syscalls in deployment; explicit capability tokens
5. **Semantic Obfuscation**: Metamorphic code generation for anti-tamper resilience

### Target Domains

- Edge computing nodes
- IoT devices
- Embedded systems
- Sensor networks
- Secure actuators

---

## Phase 0: Foundation (Current)

**Status**: ✅ Complete

### Grammar Specification (v0.6)
- [x] S-expression syntax definition
- [x] Dual-phase constructs (compile-time / deploy-time)
- [x] Capability model syntax
- [x] Resource bounds annotations
- [x] Obfuscation level specifications
- [x] Target profile definitions

### Compiler Skeleton
- [x] Lexer (`src/lexer.rs`) - Logos-based tokenization
- [x] Parser (`src/parser.rs`) - Recursive descent parser
- [x] AST (`src/ast.rs`) - Full type definitions
- [x] Call Graph (`src/callgraph.rs`) - Tarjan SCC analysis
- [x] Type Checker (`src/typeck.rs`) - Bidirectional typing
- [x] Phase Validator (`src/phase.rs`) - Compile/deploy separation
- [x] Termination Checker (`src/termination.rs`) - Bounded loop analysis
- [x] Resource Analyzer (`src/resources.rs`) - Static resource bounds

### Documentation
- [x] Formal semantics (`docs/oblibeny-semantics.md`)
- [x] Implementation specification (`docs/implementation-specification.scm`)

---

## Phase 1: Core Compiler

**Target**: Q2 2025

### 1.1 Semantic Analysis
- [ ] Name resolution and symbol tables
- [ ] Module system implementation
- [ ] Import/export validation
- [ ] Visibility checking

### 1.2 Type System Completion
- [ ] Full Hindley-Milner inference for compile-time
- [ ] Subtyping for numeric types
- [ ] Struct and enum type checking
- [ ] Generic type support (compile-time only)
- [ ] Type alias resolution

### 1.3 Intermediate Representation
- [ ] High-level IR (HIR) - Desugared AST
- [ ] Mid-level IR (MIR) - Control flow graph
- [ ] Low-level IR (LIR) - Near-assembly

```
Source → AST → HIR → MIR → LIR → Target
          ↓      ↓     ↓
       Types  CFG   Opts
```

### 1.4 Optimization Passes
- [ ] Constant folding and propagation
- [ ] Dead code elimination
- [ ] Inlining (with call depth tracking)
- [ ] Loop unrolling (bounded only)
- [ ] Strength reduction

### 1.5 Code Generation
- [ ] x86-64 backend
- [ ] ARM64 backend
- [ ] RISC-V backend
- [ ] WebAssembly backend (deploy-time subset)

### 1.6 Macro System
- [ ] Hygienic macro expansion
- [ ] Pattern matching in macros
- [ ] Compile-time execution engine
- [ ] Quasiquotation support

---

## Phase 2: Verification Backend

**Target**: Q3 2025

### 2.1 SMT Integration (Z3)
- [ ] Type constraint encoding
- [ ] Resource bound verification
- [ ] Termination proof checking
- [ ] Capability flow analysis

### 2.2 Theorem Prover Integration
- [ ] Lean 4 proof export
- [ ] Isabelle/HOL proof export
- [ ] Proof certificate generation
- [ ] Verified compilation mode

### 2.3 Distributed Verification (BOINC)
- [ ] Proof task distribution
- [ ] Result aggregation
- [ ] Consensus verification
- [ ] Proof caching

### 2.4 Formal Verification Features
- [ ] Pre/post condition checking
- [ ] Loop invariant verification
- [ ] Memory safety proofs
- [ ] Information flow analysis

---

## Phase 3: Runtime and Interpreter

**Target**: Q4 2025

### 3.1 REPL
- [ ] Interactive evaluation
- [ ] Compile-time code execution
- [ ] Pretty printing
- [ ] Tab completion
- [ ] History and persistence
- [ ] Multi-line input
- [ ] REPL commands (`:type`, `:load`, `:quit`, etc.)

```
oblíbený> (defun square (x) (* x x))
=> <function: square>
oblíbený> (square 5)
=> 25
oblíbený> :type square
=> (-> i64 i64)
```

### 3.2 Interpreter
- [ ] AST interpreter for compile-time
- [ ] Bytecode compiler
- [ ] Stack-based VM
- [ ] Garbage collector (compile-time only)
- [ ] Debug stepping

### 3.3 Deploy-Time Runtime
- [ ] Minimal runtime (no GC)
- [ ] Capability token manager
- [ ] Resource budget tracker
- [ ] Watchdog timer
- [ ] Panic handler (fail-safe)

### 3.4 FFI
- [ ] C ABI compatibility
- [ ] Rust interop
- [ ] Callback support (compile-time only)
- [ ] Type marshaling

---

## Phase 4: Tooling

**Target**: Q1 2026

### 4.1 Command-Line Interface (`oblc`)
```
oblc build      # Compile project
oblc check      # Type check without codegen
oblc run        # Build and execute
oblc test       # Run test suite
oblc doc        # Generate documentation
oblc fmt        # Format source code
oblc lint       # Static analysis
oblc verify     # Run formal verification
oblc deploy     # Package for deployment
oblc repl       # Interactive mode
```

### 4.2 Package Manager (`oblpm`)
```
oblpm init              # Create new project
oblpm add <package>     # Add dependency
oblpm remove <package>  # Remove dependency
oblpm update            # Update dependencies
oblpm publish           # Publish to registry
oblpm search            # Search packages
oblpm audit             # Security audit
```

### 4.3 Build System
- [ ] `Oblibeny.toml` project manifest
- [ ] Workspace support
- [ ] Cross-compilation
- [ ] Incremental compilation
- [ ] Build profiles (dev, release, deploy)
- [ ] Custom build scripts

### 4.4 IDE Support
- [ ] Language Server Protocol (LSP)
  - [ ] Go to definition
  - [ ] Find references
  - [ ] Hover information
  - [ ] Diagnostics
  - [ ] Code completion
  - [ ] Signature help
  - [ ] Rename refactoring
- [ ] VS Code extension
- [ ] Neovim/Vim plugin
- [ ] Emacs mode
- [ ] JetBrains plugin

### 4.5 Debugging
- [ ] Source-level debugger
- [ ] DWARF debug info generation
- [ ] GDB/LLDB integration
- [ ] Resource usage profiler
- [ ] Termination proof visualizer

### 4.6 Documentation Generator
- [ ] API documentation
- [ ] Markdown support in doc comments
- [ ] Cross-referencing
- [ ] Search functionality
- [ ] Version comparison

---

## Phase 5: Standard Library

**Target**: Q2 2026

### 5.1 Core (`std.core`)
```lisp
;; Primitives
(std.core/identity x)
(std.core/const x y)
(std.core/compose f g)
(std.core/flip f)

;; Comparison
(std.core/min a b)
(std.core/max a b)
(std.core/clamp x low high)
```

### 5.2 Numeric (`std.numeric`)
```lisp
;; Arithmetic
(std.numeric/abs x)
(std.numeric/signum x)
(std.numeric/gcd a b)
(std.numeric/lcm a b)

;; Fixed-point (deploy-time safe)
(std.numeric.fixed/from-ratio num den)
(std.numeric.fixed/add a b)
(std.numeric.fixed/mul a b)

;; Saturating arithmetic
(std.numeric.sat/add a b)
(std.numeric.sat/sub a b)
```

### 5.3 Collections (`std.collections`)
```lisp
;; Arrays (deploy-time safe)
(std.array/length arr)
(std.array/get arr idx)
(std.array/set arr idx val)
(std.array/slice arr start end)
(std.array/map f arr)        ; bounded iteration
(std.array/fold f init arr)  ; bounded iteration

;; Lists (compile-time only)
(std.list/cons head tail)
(std.list/head lst)
(std.list/tail lst)
(std.list/append a b)
```

### 5.4 Text (`std.text`)
```lisp
;; String operations (bounded)
(std.text/length s)
(std.text/concat a b)
(std.text/slice s start end)
(std.text/char-at s idx)

;; Formatting
(std.text.fmt/format template args)
(std.text.fmt/parse-int s)
```

### 5.5 I/O via Capabilities (`std.io`)
```lisp
;; Capability-based I/O
(std.io/read-sensor cap-token)
(std.io/write-actuator cap-token value)
(std.io/log cap-token message)

;; Compile-time I/O
(std.io.dev/read-file path)
(std.io.dev/write-file path content)
(std.io.dev/print message)
```

### 5.6 Crypto (`std.crypto`)
```lisp
;; Hashing (deploy-time safe)
(std.crypto/sha256 data)
(std.crypto/blake3 data)

;; Signatures
(std.crypto/verify-ed25519 pub-key sig msg)

;; Symmetric encryption
(std.crypto/aes-gcm-encrypt key nonce plaintext)
(std.crypto/aes-gcm-decrypt key nonce ciphertext)
```

### 5.7 Time (`std.time`)
```lisp
;; Timestamps
(std.time/now cap-token)  ; requires capability
(std.time/duration-ms n)

;; Delays (bounded)
(std.time/sleep-ms cap-token ms)
```

### 5.8 Math (`std.math`)
```lisp
;; Transcendental (CORDIC for deploy-time)
(std.math/sin x)
(std.math/cos x)
(std.math/sqrt x)
(std.math/log x)
(std.math/exp x)

;; Lookup tables
(std.math.lut/sin-table precision)
```

---

## Phase 6: Frameworks

**Target**: Q3 2026

### 6.1 Sensor Framework (`obl-sensor`)
```lisp
(require obl-sensor)

(defsensor temperature-reader
  :type :analog
  :range (0.0 100.0)
  :unit :celsius
  :sample-rate 10)

(defhandler on-temperature (reading)
  #:deploy
  (when (> reading 80.0)
    (invoke-capability :alarm :high-temp)))
```

### 6.2 Actuator Framework (`obl-actuator`)
```lisp
(require obl-actuator)

(defactuator motor-controller
  :type :pwm
  :range (0 255)
  :safety-limits (:max-change 10 :rate-limit 100))

(defcommand set-speed (target)
  #:deploy
  #:bounded-time 50ms
  (ramp-to motor-controller target))
```

### 6.3 Protocol Framework (`obl-protocol`)
```lisp
(require obl-protocol)

(defprotocol sensor-mesh
  :transport :lora
  :encoding :cbor
  :max-message-size 256)

(defmessage sensor-report
  (id :type u16)
  (timestamp :type u32)
  (readings :type (array f32 8)))
```

### 6.4 State Machine Framework (`obl-fsm`)
```lisp
(require obl-fsm)

(defmachine device-controller
  #:deploy
  #:max-states 8
  #:max-transitions 16

  (state :idle
    (on :start -> :running))

  (state :running
    (on :pause -> :paused)
    (on :error -> :fault)
    (on :stop -> :idle))

  (state :paused
    (on :resume -> :running)
    (on :stop -> :idle))

  (state :fault
    (on :reset -> :idle)))
```

### 6.5 Testing Framework (`obl-test`)
```lisp
(require obl-test)

(deftest "addition works"
  (assert-eq (+ 2 2) 4))

(deftest "bounded loop terminates"
  #:termination-proof
  (let ((sum 0))
    (bounded-for (i 0 100)
      (set! sum (+ sum i)))
    (assert-eq sum 4950)))

(deftest "resource bounds respected"
  #:max-iterations 1000
  #:max-memory 1024
  (verify-resources (my-function)))
```

---

## Phase 7: Ecosystem

**Target**: Q4 2026

### 7.1 Package Registry
- [ ] Central package repository
- [ ] Verification status badges
- [ ] Security audit reports
- [ ] License compliance checking
- [ ] Version compatibility matrix

### 7.2 Community
- [ ] Official documentation site
- [ ] Tutorial series
- [ ] Example repository
- [ ] Discussion forum
- [ ] Discord/Matrix server

### 7.3 Integration
- [ ] CI/CD templates (GitHub Actions, GitLab CI)
- [ ] Docker images
- [ ] Nix flakes
- [ ] Embedded toolchain bundles

### 7.4 Hardware Support
- [ ] Reference board designs
- [ ] Flashing utilities
- [ ] Hardware-in-loop testing
- [ ] Debug probe support

---

## Long-term Vision

### Research Directions
1. **Verified Compilation**: Machine-checked correctness proofs
2. **Gradual Verification**: Incrementally add proofs to legacy code
3. **Distributed Consensus**: Multi-node verification protocols
4. **Quantum Resistance**: Post-quantum cryptographic primitives
5. **Formal Hardware Models**: Verified hardware abstraction layers

### Potential Extensions
1. **Oblíbený-GPU**: Subset for verified GPU compute
2. **Oblíbený-Net**: Verified network protocol implementations
3. **Oblíbený-ML**: Bounded neural network inference
4. **Oblíbený-Realtime**: Hard real-time guarantees

### Academic Collaboration
- PhD dissertation completion
- Conference publications (PLDI, POPL, OSDI)
- Open research challenges
- Student project opportunities

---

## Milestones Summary

| Phase | Target | Key Deliverable |
|-------|--------|-----------------|
| 0 | ✅ Done | Grammar + Compiler skeleton |
| 1 | Q2 2025 | Full compiler with code generation |
| 2 | Q3 2025 | Formal verification integration |
| 3 | Q4 2025 | REPL + Interpreter + Runtime |
| 4 | Q1 2026 | Complete tooling suite |
| 5 | Q2 2026 | Standard library |
| 6 | Q3 2026 | Domain frameworks |
| 7 | Q4 2026 | Ecosystem and community |

---

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on:
- Code style
- Testing requirements
- Documentation standards
- Verification expectations

---

## License

AGPL-3.0-or-later

Copyright 2025 Jonathan D.A. Jewell
