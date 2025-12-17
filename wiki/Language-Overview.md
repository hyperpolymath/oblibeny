# Language Overview

Oblíbený is a Lisp-family language with S-expression syntax designed for secure, verifiable edge computing.

---

## Table of Contents

1. [Design Philosophy](#design-philosophy)
2. [Syntax Basics](#syntax-basics)
3. [The Dual-Language Paradigm](#the-dual-language-paradigm)
4. [Program Structure](#program-structure)
5. [Expressions and Statements](#expressions-and-statements)
6. [Functions](#functions)
7. [Control Flow](#control-flow)
8. [Data Types](#data-types)
9. [Modules](#modules)

---

## Design Philosophy

### Core Tenets

1. **Security by Design**: No undefined behavior; all code is verifiable
2. **Explicit over Implicit**: Capabilities must be explicitly granted
3. **Predictable Execution**: Resource usage is statically bounded
4. **Defense in Depth**: Multiple layers of protection (types, phases, capabilities, bounds)

### Why S-Expressions?

- **Homoiconicity**: Code is data, enabling powerful metaprogramming
- **Unambiguous Parsing**: No operator precedence issues
- **Macro-Friendly**: Easy AST manipulation
- **Tool-Friendly**: Simple to analyze and transform

---

## Syntax Basics

### S-Expressions

Everything in Oblíbený is an S-expression:

```lisp
;; Atoms
42                  ; Integer
3.14                ; Float
"hello"             ; String
true                ; Boolean
symbol              ; Symbol/Identifier

;; Lists (function application)
(+ 1 2)             ; => 3
(print "hello")     ; Function call
(if cond then else) ; Special form
```

### Comments

```lisp
; Single-line comment

#|
  Multi-line
  block comment
|#

;;; Documentation comment
;;; Appears in generated docs
(defun documented-function ()
  "Docstring goes here"
  body)
```

### Literals

```lisp
;; Integers
42                  ; Decimal
0x2A                ; Hexadecimal
0b101010            ; Binary
0o52                ; Octal

;; Floats
3.14
1.0e-10
-0.5

;; Strings
"hello world"
"escape: \n \t \\"
"unicode: \u{1F600}"

;; Booleans
true
false

;; Unit (void)
()
```

---

## The Dual-Language Paradigm

### Compile-Time (Master Language)

Full Turing-complete language:

```lisp
(compile-time
  ;; Unbounded loops allowed
  (while (not done)
    (process-next))

  ;; Recursion allowed
  (defun factorial (n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))))

  ;; Macros and metaprogramming
  (defmacro unless (cond &rest body)
    `(if (not ,cond) (progn ,@body)))

  ;; Dynamic allocation
  (let ((data (alloc 1024)))
    (process data)
    (free data)))
```

### Deploy-Time (Deployment Subset)

Turing-incomplete, provably terminating:

```lisp
(deploy-time
  ;; Only bounded loops
  (bounded-for (i 0 100)
    (process i))

  ;; No recursion (call graph must be acyclic)
  (defun safe-process (x)
    #:deploy
    (+ x 1))

  ;; No dynamic allocation
  ;; No syscalls
  ;; Capability-based I/O only
  (invoke-capability :sensor-read cap-token))
```

### Phase Annotations

```lisp
;; Function phase markers
(defun compile-only-fn ()
  #:compile-only
  (while true (do-stuff)))

(defun deploy-only-fn ()
  #:deploy
  #:termination bounded-loop
  (bounded-for (i 0 n) (step i)))

(defun both-phases-fn ()
  #:both
  ;; Must satisfy deploy-time restrictions
  (simple-calculation x))
```

---

## Program Structure

### Top-Level Forms

```lisp
;; Package declaration
(package my-project
  :version "1.0.0"
  :dependencies ((std "0.1")
                 (crypto "0.2")))

;; Module definition
(module sensors
  (import std.io)
  (export read-temperature
          calibrate))

;; Function definitions
(defun read-temperature (sensor-cap)
  #:deploy
  (invoke-capability :read sensor-cap))

;; Global data
(defglobal *calibration-offset* :type i32 :init 0)

;; Deployment specification
(deployment
  :profile edge-minimal
  :bounds (:max-iterations 10000
           :max-memory 4096
           :max-call-depth 16)
  :obfuscation aggressive)
```

### File Organization

```
my-project/
├── Oblibeny.toml          # Project manifest
├── src/
│   ├── main.obl           # Entry point
│   ├── lib.obl            # Library root
│   └── sensors/
│       ├── mod.obl        # Module definition
│       ├── temperature.obl
│       └── humidity.obl
├── tests/
│   ├── unit/
│   └── integration/
└── deploy/
    └── edge-config.obl    # Deployment config
```

---

## Expressions and Statements

### Binding Forms

```lisp
;; Let binding (local scope)
(let ((x 10)
      (y 20))
  (+ x y))

;; Let* (sequential binding)
(let* ((x 10)
       (y (* x 2)))  ; y can use x
  (+ x y))

;; Set! (mutation)
(let ((counter 0))
  (set! counter (+ counter 1)))
```

### Arithmetic

```lisp
(+ 1 2 3)       ; => 6
(- 10 3)        ; => 7
(* 2 3 4)       ; => 24
(/ 10 3)        ; => 3 (integer division)
(% 10 3)        ; => 1 (modulo)

;; Comparison
(< 1 2)         ; => true
(> 3 2)         ; => true
(<= 2 2)        ; => true
(>= 3 3)        ; => true
(= 1 1)         ; => true
(!= 1 2)        ; => true
```

### Logical Operations

```lisp
(and true false)   ; => false
(or true false)    ; => true
(not true)         ; => false

;; Short-circuit evaluation
(and (check-a) (check-b))  ; check-b only if check-a true
(or (check-a) (check-b))   ; check-b only if check-a false
```

---

## Functions

### Definition

```lisp
;; Basic function
(defun add (a b)
  (+ a b))

;; With type annotations (required for deploy-time)
(defun add-typed (a :type i64 b :type i64) :returns i64
  (+ a b))

;; With annotations
(defun process (data)
  #:deploy
  #:pure
  #:termination bounded-loop
  #:complexity O(n)
  (bounded-for (i 0 (length data))
    (transform (get data i))))
```

### Calling

```lisp
;; Standard call
(add 1 2)

;; Named arguments (compile-time)
(configure :host "localhost" :port 8080)

;; Rest arguments (compile-time only)
(defun sum (&rest numbers)
  (fold + 0 numbers))
(sum 1 2 3 4 5)  ; => 15
```

### Lambdas

```lisp
;; Anonymous function
(lambda (x) (* x x))

;; Shorthand
#(* % %)         ; => (lambda (x) (* x x))

;; With closure (compile-time)
(let ((factor 10))
  (lambda (x) (* x factor)))
```

---

## Control Flow

### Conditionals

```lisp
;; If expression
(if (> x 0)
    "positive"
    "non-positive")

;; Cond (multi-way)
(cond
  ((< x 0) "negative")
  ((= x 0) "zero")
  (true    "positive"))

;; When/Unless
(when (ready?)
  (process)
  (complete))

(unless (error?)
  (continue))
```

### Loops

```lisp
;; Bounded for (deploy-time safe)
(bounded-for (i 0 100)
  (process i))

;; While (compile-time only)
(while (not done)
  (step))

;; For-each (compile-time only)
(for (item items)
  (process item))
```

### Pattern Matching

```lisp
(match value
  ((Some x) (use x))
  (None     (default))
  (_        (error "unexpected")))
```

---

## Data Types

### Primitives

| Type | Description | Size |
|------|-------------|------|
| `u8`, `u16`, `u32`, `u64` | Unsigned integers | 1-8 bytes |
| `i8`, `i16`, `i32`, `i64` | Signed integers | 1-8 bytes |
| `f32`, `f64` | Floating point | 4-8 bytes |
| `bool` | Boolean | 1 byte |
| `string` | UTF-8 string | Variable |

### Composite Types

```lisp
;; Arrays (fixed size)
(deftype sensor-readings (array f32 8))

;; Structs
(defstruct point
  (x :type f64)
  (y :type f64))

;; Enums
(defenum status
  :idle
  :running
  :paused
  :error)

;; Pointers (compile-time only)
(deftype buffer-ptr (ptr u8))
```

### Type Aliases

```lisp
(deftype temperature f32)
(deftype sensor-id u16)
```

---

## Modules

### Defining Modules

```lisp
(module math.trig
  (import std.core)
  (export sin cos tan)

  (defconst PI 3.14159265358979)

  (defun sin (x)
    #:deploy
    (cordic-sin x))

  (defun cos (x)
    #:deploy
    (cordic-cos x))

  (defun tan (x)
    #:deploy
    (/ (sin x) (cos x))))
```

### Importing

```lisp
;; Import entire module
(import math.trig)
(sin 1.0)

;; Import specific items
(import (math.trig sin cos))
(sin 1.0)

;; Aliased import
(import (math.trig :as trig))
(trig/sin 1.0)

;; Qualified access
math.trig/PI
```

---

## Next Steps

- [Syntax Reference](Syntax-Reference.md) - Complete syntax details
- [Type System](Type-System.md) - Type system deep dive
- [Dual-Language Paradigm](Dual-Language-Paradigm.md) - Phase separation explained
