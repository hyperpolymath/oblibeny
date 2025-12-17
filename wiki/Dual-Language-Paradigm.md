# The Dual-Language Paradigm

The Dual-Language Paradigm (DLP) is the foundational concept of Oblíbený, enabling powerful development-time features while guaranteeing secure, predictable deployment-time execution.

---

## Table of Contents

1. [Concept Overview](#concept-overview)
2. [Master Language (Compile-Time)](#master-language-compile-time)
3. [Deployment Subset (Deploy-Time)](#deployment-subset-deploy-time)
4. [Phase Separation](#phase-separation)
5. [Maximal Principle Reduction](#maximal-principle-reduction)
6. [Theoretical Foundations](#theoretical-foundations)
7. [Practical Examples](#practical-examples)

---

## Concept Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                     DEVELOPMENT PHASE                            │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │              MASTER LANGUAGE (Turing-Complete)             │  │
│  │                                                             │  │
│  │  • Macros and metaprogramming                               │  │
│  │  • Unbounded loops and recursion                            │  │
│  │  • Dynamic memory allocation                                │  │
│  │  • Full I/O access                                          │  │
│  │  • Code generation and transformation                       │  │
│  └───────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ Compilation
                              │ (Macro expansion, verification)
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                     DEPLOYMENT PHASE                             │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │           DEPLOYMENT SUBSET (Turing-Incomplete)            │  │
│  │                                                             │  │
│  │  • Bounded loops only                                       │  │
│  │  • No recursion (acyclic call graph)                        │  │
│  │  • Static memory allocation                                 │  │
│  │  • Capability-based I/O only                                │  │
│  │  • Provably terminating                                     │  │
│  └───────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

### Why Two Languages?

1. **Security**: Deploy-time code cannot hang, crash, or exceed resource limits
2. **Verifiability**: All properties can be statically checked
3. **Expressiveness**: Development remains fully powerful
4. **Predictability**: Runtime behavior is deterministic and bounded

---

## Master Language (Compile-Time)

The Master Language is a full Turing-complete language available during compilation.

### Features Available

| Feature | Purpose |
|---------|---------|
| Macros | Code generation and DSLs |
| Unbounded loops | Arbitrary iteration |
| Recursion | Recursive algorithms |
| Dynamic allocation | Flexible data structures |
| Full I/O | File, network, etc. |
| Exceptions | Error handling |
| Reflection | Code introspection |

### Example: Compile-Time Code Generation

```lisp
(compile-time
  ;; Read configuration at compile time
  (defconst config (read-json "config.json"))

  ;; Generate optimized lookup table
  (defmacro generate-sin-table (precision)
    (let ((table (make-array precision)))
      (for (i (range precision))
        (set! (aref table i)
              (sin (* 2 PI (/ i precision)))))
      `(defconst *sin-table* ,table)))

  ;; Expand to actual table
  (generate-sin-table 1024)

  ;; Generate specialized functions based on config
  (for (sensor (config :sensors))
    (eval
      `(defun ,(symbol (concat "read-" (sensor :name))) ()
         #:deploy
         (invoke-capability :read ,(sensor :cap-token))))))
```

### Macro System

```lisp
;; Hygienic macros
(defmacro unless (condition &rest body)
  `(if (not ,condition)
       (progn ,@body)))

;; Compile-time computation
(defmacro compile-time-factorial (n)
  (letrec ((fact (lambda (x)
                   (if (<= x 1) 1
                       (* x (fact (- x 1)))))))
    (fact n)))

;; Usage
(defconst factorial-10 (compile-time-factorial 10))
;; Expands to: (defconst factorial-10 3628800)
```

### Transaction Blocks

```lisp
(compile-time
  (transaction
    ;; Atomic code generation
    (generate-module-a)
    (generate-module-b)
    ;; If any fails, all changes rolled back
    (if (validation-fails?)
        (rollback)
        (commit))))
```

---

## Deployment Subset (Deploy-Time)

The Deployment Subset is a Turing-incomplete language that guarantees termination and bounded resource usage.

### Restrictions

| Forbidden | Alternative |
|-----------|-------------|
| `while` loops | `bounded-for` with static bound |
| Recursion | Acyclic function calls |
| `alloc`/`free` | Static allocation only |
| `syscall` | Capability tokens |
| Exceptions | Result types |
| Unbounded iteration | `bounded-for` |

### Example: Deploy-Time Safe Code

```lisp
;; This function is deploy-time safe
(defun process-sensor-data (readings cap-token)
  #:deploy
  #:termination bounded-loop
  #:complexity O(n)
  #:max-iterations 256

  (let ((sum 0)
        (count 0))

    ;; Bounded iteration
    (bounded-for (i 0 (min (array-length readings) 256))
      (let ((r (array-get readings i)))
        (when (valid-reading? r)
          (set! sum (+ sum r))
          (set! count (+ count 1)))))

    ;; Capability-based output
    (when (> count 0)
      (invoke-capability :log cap-token
        (/ sum count)))))
```

### Deploy-Time Type Requirements

```lisp
;; All parameters and return types MUST be annotated
(defun calculate-average
    (values :type (array f32 256)
     count  :type u32)
  :returns f32
  #:deploy

  (if (= count 0)
      0.0
      (/ (array-sum values count) (as-f32 count))))
```

---

## Phase Separation

The compiler enforces strict separation between compile-time and deploy-time code.

### Phase Annotations

```lisp
;; Compile-time only
(defun code-generator (spec)
  #:compile-only
  (while (not (spec-complete? spec))
    (generate-next spec)))

;; Deploy-time only
(defun handle-sensor (reading)
  #:deploy
  (bounded-for (i 0 8)
    (process-channel i reading)))

;; Both phases (must satisfy deploy-time restrictions)
(defun pure-calculation (x y)
  #:both
  (+ (* x x) (* y y)))
```

### Phase Checking Rules

1. **Deploy-time code cannot call compile-time-only functions**
2. **Compile-time code can call any function**
3. **#:both functions must satisfy all deploy-time restrictions**
4. **Global state in deploy-time must be statically allocated**

### Phase Errors

```lisp
;; ERROR: Deploy-time function uses compile-time-only feature
(defun bad-deploy-fn (data)
  #:deploy
  (while (not (done? data))  ; ERROR: while is compile-time only
    (process data)))

;; ERROR: Calling compile-time function from deploy-time
(defun another-bad-fn (x)
  #:deploy
  (code-generator x))  ; ERROR: code-generator is #:compile-only
```

---

## Maximal Principle Reduction

MPR determines upper bounds on all resources at compile time.

### Resource Categories

| Resource | Bound Type | Verification |
|----------|------------|--------------|
| Iterations | Static integer | Loop analysis |
| Stack depth | Static integer | Call graph analysis |
| Memory | Static bytes | Type size analysis |
| Call depth | Static integer | Call graph depth |
| Execution time | Estimated cycles | Instruction counting |

### Specifying Bounds

```lisp
(deployment
  :profile edge-minimal

  :bounds
  (:max-iterations 10000      ; Total loop iterations
   :max-stack-depth 256       ; Local variable slots
   :max-memory 4096           ; Bytes of static allocation
   :max-call-depth 16         ; Function call depth
   :max-execution-time 1000)  ; Estimated cycles

  :constraints
  (:forbidden (syscalls recursion dynamic-alloc)
   :required (termination-proof bounded-memory)))
```

### Bound Verification

```lisp
;; Compiler verifies bounds
(defun matrix-multiply (a b result)
  #:deploy
  #:max-iterations 27000  ; 30 * 30 * 30

  (bounded-for (i 0 30)
    (bounded-for (j 0 30)
      (let ((sum 0))
        (bounded-for (k 0 30)
          (set! sum (+ sum (* (aref a i k) (aref b k j)))))
        (set! (aref result i j) sum)))))

;; Compiler calculates:
;; - Iterations: 30 * 30 * 30 = 27,000 ✓
;; - Stack depth: 4 (i, j, k, sum) ✓
;; - Memory: 0 (uses passed arrays) ✓
```

---

## Theoretical Foundations

### Halting Problem Avoidance

By restricting the deployment subset to primitive recursive functions, we avoid the halting problem:

1. **Bounded Loops**: All loops have compile-time-known iteration counts
2. **Acyclic Calls**: Call graph has no cycles (no recursion)
3. **Static Resources**: All memory is statically allocated

### Type Theory

The type system is based on:
- **Bidirectional Type Checking**: Inference in compile-time, checking in deploy-time
- **Substructural Types**: Linear types for capabilities
- **Refinement Types**: Resource bounds as type refinements

### Formal Semantics

```
Compile-Time Semantics: ⟦·⟧_C : Expr → Env → Value
Deploy-Time Semantics:  ⟦·⟧_D : Expr → Env → (Value, Resources)

Phase Separation:
  If e : τ #:deploy and ⟦e⟧_D(ρ) = (v, r)
  Then r ≤ bounds(τ)

Termination Guarantee:
  If e : τ #:deploy
  Then ∃n. ⟦e⟧_D terminates in ≤ n steps
```

---

## Practical Examples

### Sensor Fusion Algorithm

```lisp
;; Compile-time: Generate optimized Kalman filter
(compile-time
  (defmacro generate-kalman (state-dim obs-dim)
    ;; Use unbounded computation to derive optimal matrices
    (let* ((A (optimize-state-transition state-dim))
           (H (optimize-observation obs-dim state-dim))
           (Q (compute-process-noise state-dim))
           (R (compute-measurement-noise obs-dim)))
      `(progn
         (defconst *kalman-A* ,A)
         (defconst *kalman-H* ,H)
         (defconst *kalman-Q* ,Q)
         (defconst *kalman-R* ,R)))))

(generate-kalman 4 2)

;; Deploy-time: Run the filter with bounded resources
(defun kalman-update (state measurement)
  #:deploy
  #:termination bounded-loop
  #:max-iterations 32  ; 4*4 + 4*2 + ...

  ;; Matrix operations are bounded by dimensions
  (let ((predicted (mat-mul *kalman-A* state))
        (innovation (vec-sub measurement
                             (mat-vec-mul *kalman-H* predicted))))
    ;; Kalman gain computation
    (bounded-for (i 0 4)
      (bounded-for (j 0 2)
        ;; Update state estimate
        (update-state-element state i j innovation)))))
```

### Protocol Handler

```lisp
;; Compile-time: Generate parser from protocol spec
(compile-time
  (defconst protocol-spec (parse-protocol-file "protocol.spec"))

  (for (msg-type (protocol-spec :messages))
    (eval
      `(defun ,(symbol (concat "parse-" (msg-type :name)))
           (buffer :type (array u8 256))
         :returns ,(msg-type :result-type)
         #:deploy
         #:max-iterations ,(msg-type :max-fields)

         ;; Generated bounded parsing code
         ,@(generate-parser-body msg-type)))))

;; Deploy-time: Use generated parsers
(defun handle-message (buffer msg-type cap-token)
  #:deploy
  (cond
    ((= msg-type MSG_SENSOR)
     (let ((parsed (parse-sensor buffer)))
       (process-sensor-msg parsed cap-token)))
    ((= msg-type MSG_COMMAND)
     (let ((parsed (parse-command buffer)))
       (execute-command parsed cap-token)))
    (true
     (log-error cap-token "unknown message type"))))
```

---

## Summary

The Dual-Language Paradigm provides:

| Aspect | Master Language | Deployment Subset |
|--------|-----------------|-------------------|
| Turing | Complete | Incomplete |
| Termination | Not guaranteed | Proven |
| Resources | Unbounded | Bounded |
| Verification | Optional | Mandatory |
| Phase | Compile-time | Deploy-time |
| I/O | Unrestricted | Capability-based |

---

## Next Steps

- [Compile-Time Features](Compile-Time-Features.md)
- [Deploy-Time Restrictions](Deploy-Time-Restrictions.md)
- [Termination Proofs](Termination-Proofs.md)
- [Resource Bounds](Resource-Bounds.md)
