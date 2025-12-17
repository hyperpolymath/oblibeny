# Quick Start Tutorial

Get up and running with Oblíbený in 10 minutes.

---

## Prerequisites

- Rust toolchain (for building the compiler)
- Basic familiarity with Lisp-like syntax

---

## Step 1: Install Oblíbený

```bash
# Clone and build
git clone https://github.com/oblibeny/oblibeny.git
cd oblibeny
cargo install --path .

# Verify installation
oblc --version
```

---

## Step 2: Create a Project

```bash
oblc new hello-world
cd hello-world
```

This creates:

```
hello-world/
├── Oblibeny.toml       # Project manifest
├── src/
│   └── main.obl        # Main source file
└── tests/
    └── test_main.obl   # Test file
```

---

## Step 3: Explore the Project

### Oblibeny.toml

```toml
[package]
name = "hello-world"
version = "0.1.0"
edition = "2025"

[dependencies]
std = "0.1"
```

### src/main.obl

```lisp
;; Hello World in Oblíbený

(import std.core)

;; Main entry point
(defun main ()
  #:compile-only
  (print "Hello, World!"))
```

---

## Step 4: Run the Program

```bash
oblc run
```

Output:
```
Hello, World!
```

---

## Step 5: Write Deploy-Time Code

Let's create a function that's safe for edge deployment:

```lisp
;; src/main.obl

(import std.core)
(import std.array)

;; A deploy-time safe function
(defun calculate-average (values :type (array f32 16)
                          count :type u8)
  :returns f32
  #:deploy
  #:termination bounded-loop
  #:max-iterations 16

  (if (= count 0)
      0.0
      (let ((sum 0.0))
        (bounded-for (i 0 (min count 16))
          (set! sum (+ sum (array/get values i))))
        (/ sum (as-f32 count)))))

;; Test it at compile time
(defun main ()
  #:compile-only
  (let ((data (array 1.0 2.0 3.0 4.0 5.0
                     0.0 0.0 0.0 0.0 0.0
                     0.0 0.0 0.0 0.0 0.0 0.0)))
    (print "Average:" (calculate-average data 5))))
```

Run it:
```bash
oblc run
```

Output:
```
Average: 3.0
```

---

## Step 6: Add Tests

Edit `tests/test_main.obl`:

```lisp
(import obl-test)
(import main)

(deftest "average of empty array is zero"
  (let ((data (array/new 16 0.0)))
    (assert-eq (calculate-average data 0) 0.0)))

(deftest "average of single element"
  (let ((data (array/new 16 0.0)))
    (array/set! data 0 42.0)
    (assert-eq (calculate-average data 1) 42.0)))

(deftest "average of multiple elements"
  (let ((data (array 10.0 20.0 30.0 0.0 0.0
                     0.0 0.0 0.0 0.0 0.0
                     0.0 0.0 0.0 0.0 0.0 0.0)))
    (assert-eq (calculate-average data 3) 20.0)))
```

Run tests:
```bash
oblc test
```

Output:
```
   Running tests/test_main.obl
test average of empty array is zero ... ok
test average of single element ... ok
test average of multiple elements ... ok

test result: ok. 3 passed; 0 failed
```

---

## Step 7: Verify Termination

Check that deploy-time functions provably terminate:

```bash
oblc verify
```

Output:
```
   Verifying hello-world v0.1.0

Checking termination...
  ✓ calculate-average: bounded-loop (max 16 iterations)

Checking resource bounds...
  ✓ max_iterations: 16 ≤ 10000
  ✓ max_stack_depth: 3 ≤ 256

Verification: 3 checks passed, 0 failed
```

---

## Step 8: Build for Deployment

```bash
oblc build --deploy
```

Output:
```
   Compiling hello-world v0.1.0
   Analyzing termination...
   Verifying resource bounds...
   Generating code...
    Finished deploy [optimized + verified] in 0.42s
```

---

## Step 9: Try the REPL

```bash
oblc repl
```

```
oblíbený v0.1.0 (grammar 0.6)
Type :help for help, :quit to exit

λ> (+ 1 2 3)
=> 6

λ> (defun square (x) (* x x))
=> <function: square>

λ> (square 7)
=> 49

λ> :type square
=> (-> i64 i64)

λ> :quit
Goodbye!
```

---

## Next Steps

### Learn More

- [Language Overview](Language-Overview.md) - Complete language guide
- [Dual-Language Paradigm](Dual-Language-Paradigm.md) - Compile vs deploy time
- [Testing Framework](Testing-Framework.md) - Property-based testing

### Build Something

- [Sensor Tutorial](Tutorial-Sensor.md) - Read sensor data
- [Protocol Tutorial](Tutorial-Protocol.md) - Implement a protocol
- [State Machine Tutorial](Tutorial-FSM.md) - Build a state machine

### Explore Examples

```bash
# Clone examples repository
git clone https://github.com/oblibeny/examples.git

# Try temperature monitor
cd examples/temperature-monitor
oblc run
```

---

## Common Issues

### "Phase violation" error

You're using a compile-time-only feature in deploy-time code:

```lisp
;; BAD
(defun process ()
  #:deploy
  (while true (step)))  ; while is compile-time only

;; GOOD
(defun process ()
  #:deploy
  (bounded-for (i 0 100) (step)))
```

### "Termination unprovable" error

Your deploy-time function might not terminate:

```lisp
;; BAD
(defun recursive-fn (n)
  #:deploy
  (if (= n 0) 0
      (recursive-fn (- n 1))))  ; Recursion forbidden

;; GOOD
(defun iterative-fn (n)
  #:deploy
  (let ((result 0))
    (bounded-for (i 0 n)
      (set! result (+ result i)))
    result))
```

### "Type annotation required" error

Deploy-time functions need explicit types:

```lisp
;; BAD
(defun add (a b)
  #:deploy
  (+ a b))

;; GOOD
(defun add (a :type i64 b :type i64)
  :returns i64
  #:deploy
  (+ a b))
```

---

## Getting Help

- Documentation: [wiki/Home.md](Home.md)
- Issues: https://github.com/oblibeny/oblibeny/issues
- Discord: https://discord.gg/oblibeny
