# Testing Framework

Oblíbený includes a comprehensive testing framework inspired by property-based testing tools like QuickCheck and Echidna, designed for both theoretical verification and practical testing.

---

## Table of Contents

1. [Overview](#overview)
2. [Unit Tests](#unit-tests)
3. [Property-Based Testing](#property-based-testing)
4. [Invariant Checking](#invariant-checking)
5. [Fuzzing](#fuzzing)
6. [Termination Testing](#termination-testing)
7. [Resource Bound Testing](#resource-bound-testing)
8. [Integration Tests](#integration-tests)
9. [Test Generation](#test-generation)
10. [Coverage Analysis](#coverage-analysis)

---

## Overview

The Oblíbený testing framework operates at multiple levels:

| Level | Purpose | Phase |
|-------|---------|-------|
| Unit | Individual function correctness | Both |
| Property | Universal invariants | Both |
| Invariant | State machine properties | Deploy |
| Fuzz | Edge case discovery | Compile |
| Termination | Proves code terminates | Deploy |
| Resource | Verifies bounds compliance | Deploy |
| Integration | System behavior | Both |

### Running Tests

```bash
# Run all tests
oblc test

# Run specific test file
oblc test tests/unit/math.obl

# Run with coverage
oblc test --coverage

# Run property tests with iterations
oblc test --property-iterations 10000

# Run fuzzer
oblc test --fuzz --timeout 300
```

---

## Unit Tests

### Basic Tests

```lisp
(require obl-test)

(deftest "addition returns correct sum"
  (assert-eq (+ 2 2) 4))

(deftest "subtraction works"
  (assert-eq (- 10 3) 7))

(deftest "string concatenation"
  (assert-eq (concat "hello" " world") "hello world"))
```

### Assertions

```lisp
;; Equality
(assert-eq actual expected)
(assert-neq actual expected)

;; Comparison
(assert-lt a b)           ; a < b
(assert-lte a b)          ; a <= b
(assert-gt a b)           ; a > b
(assert-gte a b)          ; a >= b

;; Boolean
(assert-true expr)
(assert-false expr)

;; Collections
(assert-empty collection)
(assert-contains collection element)
(assert-length collection n)

;; Type
(assert-type value expected-type)

;; Exception (compile-time only)
(assert-throws exception-type
  (code-that-throws))

;; Custom message
(assert-eq actual expected "custom failure message")
```

### Test Organization

```lisp
(defmodule test-math
  (import obl-test)
  (import my-app.math)

  (deftest-group "arithmetic operations"
    (deftest "addition" ...)
    (deftest "subtraction" ...)
    (deftest "multiplication" ...))

  (deftest-group "advanced math"
    (deftest "square root" ...)
    (deftest "power" ...)))
```

### Setup and Teardown

```lisp
(deftest-group "database tests"
  :before-each (fn () (reset-db))
  :after-each (fn () (cleanup-db))
  :before-all (fn () (init-db))
  :after-all (fn () (shutdown-db))

  (deftest "insert works" ...)
  (deftest "query works" ...))
```

---

## Property-Based Testing

Inspired by QuickCheck and Echidna, property-based tests verify that properties hold for randomly generated inputs.

### Defining Properties

```lisp
(require obl-test.property)

;; Basic property
(defproperty "addition is commutative"
  (forall ((a :type i64)
           (b :type i64))
    (= (+ a b) (+ b a))))

;; With constraints
(defproperty "division inverts multiplication"
  (forall ((a :type i64)
           (b :type i64 :where (!= b 0)))
    (= (/ (* a b) b) a)))

;; With custom generators
(defproperty "sorted list remains sorted after insert"
  (forall ((lst :gen sorted-list-gen)
           (x :type i64))
    (is-sorted (sorted-insert lst x))))
```

### Generators

```lisp
;; Built-in generators
(gen/i64)                     ; Random i64
(gen/i64 :min 0 :max 100)     ; Bounded range
(gen/f64)                     ; Random float
(gen/bool)                    ; Random boolean
(gen/string :max-length 100)  ; Random string
(gen/array (gen/i64) :size 10) ; Fixed-size array

;; Custom generators
(defgen positive-i64 ()
  (gen/i64 :min 1 :max i64-max))

(defgen point ()
  (struct-point
    :x (gen/f64 :min -1000.0 :max 1000.0)
    :y (gen/f64 :min -1000.0 :max 1000.0)))

(defgen sorted-list ()
  (sort (gen/list (gen/i64) :max-size 100)))
```

### Shrinking

When a property fails, the framework automatically shrinks the counterexample to find the minimal failing case:

```lisp
;; Property fails for some input
(defproperty "buggy function works"
  (forall ((x :type i64))
    (correct? (buggy-fn x))))

;; Output:
;; FAILED: buggy function works
;; Counterexample (shrunk from 847362 to 42):
;;   x = 42
;; After 1523 shrink attempts
```

### Stateful Properties (Echidna-style)

```lisp
(require obl-test.stateful)

;; Define state machine for testing
(defstateful-test "counter invariants"
  :initial-state {:count 0}

  :operations
  [(op increment ()
       :precondition true
       :action (update :count inc)
       :postcondition (> (:count state') (:count state)))

   (op decrement ()
       :precondition (> (:count state) 0)
       :action (update :count dec)
       :postcondition (< (:count state') (:count state)))

   (op reset ()
       :action (assoc :count 0)
       :postcondition (= (:count state') 0))]

  :invariants
  [(invariant "count never negative"
     (>= (:count state) 0))])
```

---

## Invariant Checking

For deploy-time code, invariants are checked exhaustively or via bounded model checking.

### Global Invariants

```lisp
(require obl-test.invariant)

(definvariant "balance never negative"
  :scope global
  :check (>= *account-balance* 0))

(definvariant "sensor reading in range"
  :scope (module sensors)
  :check (and (>= *last-reading* 0.0)
              (<= *last-reading* 100.0)))
```

### Function Invariants

```lisp
(defun transfer (from to amount)
  #:deploy
  #:pre (and (>= (balance from) amount)
             (> amount 0))
  #:post (= (+ (balance from) (balance to))
            (+ (old (balance from)) (old (balance to))))
  #:invariant (>= (total-supply) 0)
  ...)
```

### Temporal Invariants

```lisp
(definvariant "eventually reaches steady state"
  :temporal always-eventually
  :check (= *state* :steady))

(definvariant "error always leads to recovery"
  :temporal leads-to
  :from (= *state* :error)
  :to (= *state* :idle))
```

---

## Fuzzing

Echidna-inspired fuzzing for discovering edge cases and vulnerabilities.

### Basic Fuzzing

```lisp
(require obl-test.fuzz)

(deffuzz "parse handles all inputs"
  :target parse-message
  :corpus ["valid1" "valid2"]
  :iterations 100000
  :timeout 300)
```

### Guided Fuzzing

```lisp
(deffuzz "crypto function fuzz"
  :target encrypt
  :strategy coverage-guided
  :mutators [bit-flip byte-flip arithmetic dictionary]
  :dictionary ["key" "iv" "plaintext"]
  :max-length 1024
  :iterations 1000000)
```

### Grammar-Based Fuzzing

```lisp
(deffuzz "protocol parser"
  :target parse-protocol
  :grammar
  (grammar protocol-message
    message   := header body checksum
    header    := (magic version length)
    magic     := 0xDEADBEEF
    version   := (one-of 1 2 3)
    length    := (gen/u16)
    body      := (repeat (gen/u8) length)
    checksum  := (crc32 body)))
```

### Differential Fuzzing

```lisp
(deffuzz-differential "compare implementations"
  :implementations [impl-v1 impl-v2]
  :property (fn (input)
              (= (impl-v1 input) (impl-v2 input)))
  :generator (gen/bytes :max-length 256))
```

---

## Termination Testing

Deploy-time code must provably terminate.

### Termination Annotations

```lisp
(defun process-array (arr)
  #:deploy
  #:termination bounded-loop
  #:max-iterations (array-length arr)
  (bounded-for (i 0 (array-length arr))
    (process-element (array-get arr i))))
```

### Termination Tests

```lisp
(deftest-termination "all deploy functions terminate"
  :scope (module deploy-code)
  :strategy bounded-loop)

(deftest-termination "no infinite recursion"
  :target process-tree
  :max-depth 100
  :proof structural-recursion)
```

### Termination Proof Generation

```lisp
;; Generate Lean 4 proof
(generate-termination-proof process-array
  :output "proofs/process_array.lean"
  :prover lean4)

;; Generate Isabelle proof
(generate-termination-proof process-array
  :output "proofs/Process_Array.thy"
  :prover isabelle)
```

---

## Resource Bound Testing

Verify that deploy-time code respects resource limits.

### Resource Assertions

```lisp
(deftest-resources "function respects bounds"
  :target process-data
  :inputs [(gen/array (gen/u8) :size 100)]
  :max-iterations 1000
  :max-stack-depth 16
  :max-memory 4096
  :max-call-depth 8)
```

### Resource Profiling

```lisp
(deftest-profile "measure actual usage"
  :target heavy-computation
  :inputs [test-data]
  :report [:iterations :memory :call-depth :instructions])

;; Output:
;; Profile: heavy-computation
;; Iterations:  847
;; Memory:      2,048 bytes
;; Call depth:  5
;; Instructions: 12,453
```

### Resource Regression

```lisp
(deftest-resource-regression "no performance degradation"
  :target critical-function
  :baseline "baselines/critical.json"
  :tolerance 0.05)  ; 5% tolerance
```

---

## Integration Tests

### System Tests

```lisp
(deftest-integration "sensor to actuator pipeline"
  :setup (init-test-harness)
  :teardown (cleanup-test-harness)

  (step "read sensor"
    (let ((reading (invoke-test-capability :sensor-read)))
      (assert-in-range reading 0.0 100.0)))

  (step "process reading"
    (let ((command (process-reading reading)))
      (assert-valid-command command)))

  (step "actuate"
    (invoke-test-capability :actuator-write command)
    (assert-actuator-state expected)))
```

### Mock Capabilities

```lisp
(deftest-integration "with mocked I/O"
  :mocks
  {:sensor-read (fn () 42.0)
   :actuator-write (fn (v) (record-call v))}

  (run-main-loop)
  (assert-eq (get-recorded-calls) [expected-value]))
```

---

## Test Generation

Automatically generate tests from specifications.

### From Types

```lisp
;; Automatically generate tests for all exported functions
(generate-tests-from-types
  :module my-app.math
  :strategy exhaustive-small  ; or random-sample
  :output "tests/generated/math.obl")
```

### From Invariants

```lisp
;; Generate tests that exercise invariants
(generate-invariant-tests
  :module my-app.state
  :invariants [balance-invariant supply-invariant]
  :sequences 1000
  :max-length 50
  :output "tests/generated/invariants.obl")
```

### From Formal Specs

```lisp
;; Generate tests from formal specification
(generate-tests-from-spec
  :spec "specs/protocol.lean"
  :coverage branch
  :output "tests/generated/protocol.obl")
```

---

## Coverage Analysis

### Code Coverage

```bash
oblc test --coverage --coverage-format html
```

```lisp
;; Programmatic coverage requirements
(deftest-coverage "critical module coverage"
  :module critical-operations
  :min-line-coverage 90
  :min-branch-coverage 85
  :min-function-coverage 100)
```

### Mutation Testing

```bash
oblc test --mutation --mutation-score-threshold 80
```

Mutators applied:
- Arithmetic operator replacement
- Comparison operator replacement
- Constant replacement
- Statement deletion
- Condition negation

---

## Test Configuration

### Project-Level Config

```toml
# Oblibeny.toml
[test]
parallel = true
timeout = 60
fail-fast = false

[test.property]
default-iterations = 1000
shrink-limit = 10000

[test.fuzz]
default-iterations = 100000
corpus-dir = "fuzz-corpus"

[test.coverage]
minimum-line = 80
minimum-branch = 75
exclude = ["tests/*", "examples/*"]
```

### CI Integration

```yaml
# .github/workflows/test.yml
name: Tests
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: oblibeny/setup-oblc@v1
      - run: oblc test --coverage --property-iterations 10000
      - run: oblc test --fuzz --timeout 60
```

---

## Next Steps

- [Property-Based Testing](Property-Based-Testing.md) - Deep dive into properties
- [Fuzzing Guide](Fuzzing-Guide.md) - Advanced fuzzing techniques
- [Formal Verification](Formal-Verification.md) - Theorem prover integration
