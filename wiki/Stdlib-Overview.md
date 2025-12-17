# Standard Library Overview

The Oblíbený standard library provides core functionality for both compile-time and deploy-time code, with clear phase annotations for each function.

---

## Table of Contents

1. [Organization](#organization)
2. [Phase Compatibility](#phase-compatibility)
3. [Module Summary](#module-summary)
4. [Core Modules](#core-modules)
5. [Deploy-Time Safe Modules](#deploy-time-safe-modules)
6. [Compile-Time Only Modules](#compile-time-only-modules)

---

## Organization

```
std/
├── core.obl           # Fundamental operations
├── numeric.obl        # Numeric operations
├── collections/
│   ├── array.obl      # Fixed-size arrays
│   ├── list.obl       # Linked lists (compile-time)
│   └── map.obl        # Hash maps (compile-time)
├── text.obl           # String operations
├── io.obl             # I/O capabilities
├── crypto.obl         # Cryptographic primitives
├── time.obl           # Time operations
├── math.obl           # Mathematical functions
├── result.obl         # Result type
├── option.obl         # Option type
├── iter.obl           # Iterators
├── fmt.obl            # Formatting
└── test.obl           # Testing framework
```

---

## Phase Compatibility

Each function is marked with its phase compatibility:

| Symbol | Meaning |
|--------|---------|
| 🟢 | Deploy-time safe |
| 🟡 | Both phases |
| 🔴 | Compile-time only |

---

## Module Summary

| Module | Description | Phase |
|--------|-------------|-------|
| `std.core` | Identity, composition, basic ops | 🟡 |
| `std.numeric` | Arithmetic, fixed-point | 🟢/🟡 |
| `std.array` | Fixed-size array operations | 🟢 |
| `std.list` | Linked list operations | 🔴 |
| `std.map` | Hash map operations | 🔴 |
| `std.text` | String manipulation | 🟢/🟡 |
| `std.io` | Capability-based I/O | 🟢 |
| `std.crypto` | Hashing, signatures | 🟢 |
| `std.time` | Timestamps, delays | 🟢 |
| `std.math` | Trigonometry, logarithms | 🟢 |
| `std.result` | Error handling | 🟡 |
| `std.option` | Optional values | 🟡 |

---

## Core Modules

### std.core

Fundamental operations available in all phases.

```lisp
(import std.core)

;; Identity and composition 🟡
(identity x)              ; => x
(const x y)               ; => x (ignores y)
(compose f g)             ; => (λ (x) (f (g x)))
(flip f)                  ; => (λ (x y) (f y x))
(pipe x f g h)            ; => (h (g (f x)))

;; Comparison 🟡
(min a b)                 ; => smaller of a, b
(max a b)                 ; => larger of a, b
(clamp x low high)        ; => x clamped to [low, high]

;; Boolean 🟡
(bool/and a b)            ; => a && b
(bool/or a b)             ; => a || b
(bool/not a)              ; => !a
(bool/xor a b)            ; => a ^ b

;; Predicates 🟡
(zero? x)                 ; => x == 0
(positive? x)             ; => x > 0
(negative? x)             ; => x < 0
(even? x)                 ; => x % 2 == 0
(odd? x)                  ; => x % 2 != 0
```

### std.result

Result type for error handling (no exceptions in deploy-time).

```lisp
(import std.result)

;; Type 🟡
(defenum Result
  (Ok value)
  (Err error))

;; Constructors 🟡
(ok value)                ; => (Ok value)
(err error)               ; => (Err error)

;; Predicates 🟡
(ok? result)              ; => true if Ok
(err? result)             ; => true if Err

;; Unwrapping 🟡
(unwrap result)           ; => value or panic
(unwrap-or result default); => value or default
(unwrap-err result)       ; => error or panic

;; Combinators 🟡
(map result f)            ; Apply f to Ok value
(map-err result f)        ; Apply f to Err value
(and-then result f)       ; Flatmap for Ok
(or-else result f)        ; Flatmap for Err

;; Example 🟢
(defun safe-divide (a b)
  #:deploy
  (if (= b 0)
      (err :division-by-zero)
      (ok (/ a b))))

(match (safe-divide 10 2)
  ((Ok v) (process v))
  ((Err e) (handle-error e)))
```

### std.option

Optional values.

```lisp
(import std.option)

;; Type 🟡
(defenum Option
  (Some value)
  None)

;; Constructors 🟡
(some value)              ; => (Some value)
none                      ; => None

;; Predicates 🟡
(some? opt)               ; => true if Some
(none? opt)               ; => true if None

;; Unwrapping 🟡
(unwrap opt)              ; => value or panic
(unwrap-or opt default)   ; => value or default
(unwrap-or-else opt f)    ; => value or (f)

;; Combinators 🟡
(map opt f)               ; Apply f to Some value
(and-then opt f)          ; Flatmap
(or-else opt f)           ; Alternative
(filter opt pred)         ; Keep if pred true

;; Example 🟢
(defun find-sensor (id sensors)
  #:deploy
  (bounded-for (i 0 (array-length sensors))
    (let ((s (array-get sensors i)))
      (when (= (sensor-id s) id)
        (return (some s)))))
  none)
```

---

## Deploy-Time Safe Modules

### std.array

Fixed-size array operations (all deploy-time safe).

```lisp
(import std.array)

;; Creation 🟢
(array/new size init)           ; New array with init value
(array/from-list lst)           ; From compile-time list

;; Access 🟢
(array/get arr idx)             ; Get element
(array/set! arr idx val)        ; Set element
(array/length arr)              ; Get length

;; Slicing 🟢
(array/slice arr start end)     ; Get subarray
(array/copy! src dst)           ; Copy src to dst

;; Searching 🟢
(array/find arr pred)           ; Find first matching
(array/contains? arr val)       ; Check membership
(array/index-of arr val)        ; Find index

;; Bounded iteration 🟢
(array/for-each arr f)          ; Apply f to each (bounded)
(array/map arr f)               ; Map function (bounded)
(array/filter arr pred)         ; Filter elements (bounded)
(array/fold arr init f)         ; Fold/reduce (bounded)
(array/sum arr)                 ; Sum all elements
(array/product arr)             ; Product of all

;; Sorting 🟢 (bounded comparison sort)
(array/sort! arr)               ; In-place sort
(array/sort-by! arr key-fn)     ; Sort by key

;; Example 🟢
(defun average (readings :type (array f32 256) count :type u32)
  :returns f32
  #:deploy
  #:max-iterations 256
  (if (= count 0)
      0.0
      (/ (array/fold (array/slice readings 0 count)
                     0.0
                     +)
         (as-f32 count))))
```

### std.numeric

Numeric operations with deploy-time safe variants.

```lisp
(import std.numeric)

;; Basic arithmetic 🟢
(abs x)                   ; Absolute value
(signum x)                ; Sign (-1, 0, 1)
(gcd a b)                 ; Greatest common divisor
(lcm a b)                 ; Least common multiple

;; Saturating arithmetic 🟢
(sat-add a b)             ; Add with saturation
(sat-sub a b)             ; Subtract with saturation
(sat-mul a b)             ; Multiply with saturation

;; Wrapping arithmetic 🟢
(wrap-add a b)            ; Add with wrapping
(wrap-sub a b)            ; Subtract with wrapping
(wrap-mul a b)            ; Multiply with wrapping

;; Checked arithmetic 🟢
(checked-add a b)         ; => Option, None on overflow
(checked-sub a b)
(checked-mul a b)
(checked-div a b)         ; None on division by zero

;; Fixed-point 🟢
(import std.numeric.fixed)

(deftype fixed32 i32)     ; Q16.16 fixed-point

(fixed/from-int n)        ; Integer to fixed
(fixed/from-ratio n d)    ; Ratio to fixed
(fixed/to-int f)          ; Fixed to integer (truncate)
(fixed/add a b)           ; Fixed addition
(fixed/sub a b)           ; Fixed subtraction
(fixed/mul a b)           ; Fixed multiplication
(fixed/div a b)           ; Fixed division

;; Example 🟢
(defun pid-controller (setpoint measured kp ki kd prev-error integral)
  #:deploy
  (let* ((error (fixed/sub setpoint measured))
         (p-term (fixed/mul kp error))
         (new-integral (fixed/add integral (fixed/mul ki error)))
         (d-term (fixed/mul kd (fixed/sub error prev-error)))
         (output (fixed/add (fixed/add p-term new-integral) d-term)))
    (values output error new-integral)))
```

### std.crypto

Cryptographic primitives (all deploy-time safe).

```lisp
(import std.crypto)

;; Hashing 🟢
(sha256 data)                   ; SHA-256 hash
(sha512 data)                   ; SHA-512 hash
(blake3 data)                   ; BLAKE3 hash
(hmac-sha256 key data)          ; HMAC-SHA256

;; Signatures 🟢
(ed25519-verify pub-key sig msg); Verify Ed25519 signature
(p256-verify pub-key sig msg)   ; Verify P-256 signature

;; Symmetric encryption 🟢
(aes-gcm-encrypt key nonce plaintext)
(aes-gcm-decrypt key nonce ciphertext)
(chacha20-poly1305-encrypt key nonce plaintext)
(chacha20-poly1305-decrypt key nonce ciphertext)

;; Random (requires capability) 🟢
(random-bytes cap n)            ; Generate n random bytes

;; Key derivation 🟢
(hkdf-sha256 salt ikm info len) ; HKDF-SHA256

;; Example 🟢
(defun verify-message (pub-key message signature)
  #:deploy
  (let ((hash (sha256 message)))
    (if (ed25519-verify pub-key signature hash)
        (ok message)
        (err :invalid-signature))))
```

### std.math

Mathematical functions (CORDIC implementations for deploy-time).

```lisp
(import std.math)

;; Trigonometric 🟢 (CORDIC)
(sin x)                   ; Sine
(cos x)                   ; Cosine
(tan x)                   ; Tangent
(asin x)                  ; Arc sine
(acos x)                  ; Arc cosine
(atan x)                  ; Arc tangent
(atan2 y x)               ; Two-argument arc tangent

;; Exponential/Logarithmic 🟢 (table + interpolation)
(exp x)                   ; e^x
(log x)                   ; Natural log
(log2 x)                  ; Base-2 log
(log10 x)                 ; Base-10 log
(pow base exp)            ; Power (integer exp only)

;; Root functions 🟢
(sqrt x)                  ; Square root (Newton's method)
(cbrt x)                  ; Cube root

;; Hyperbolic 🟢
(sinh x)
(cosh x)
(tanh x)

;; Interpolation 🟢
(lerp a b t)              ; Linear interpolation
(smoothstep a b t)        ; Smooth interpolation

;; Lookup tables 🔴 (generate at compile-time)
(compile-time
  (generate-sin-table precision))
```

### std.io

Capability-based I/O operations.

```lisp
(import std.io)

;; Sensor operations 🟢
(sensor-read cap)                     ; Read sensor value
(sensor-read-array cap buffer count)  ; Read multiple values

;; Actuator operations 🟢
(actuator-write cap value)            ; Write value
(actuator-write-array cap buffer count)

;; Logging 🟢
(log-debug cap msg)
(log-info cap msg)
(log-warn cap msg)
(log-error cap msg)

;; Time 🟢
(time-now cap)                        ; Current timestamp
(time-sleep-ms cap ms)                ; Delay (bounded)

;; Compile-time I/O 🔴
(compile-time
  (file-read path)
  (file-write path content)
  (print msg)
  (read-line))
```

---

## Compile-Time Only Modules

### std.list

Linked lists (compile-time only due to dynamic allocation).

```lisp
(import std.list)

;; All operations 🔴
(list/cons head tail)     ; Construct
(list/head lst)           ; First element
(list/tail lst)           ; Rest of list
(list/empty? lst)         ; Is empty?
(list/length lst)         ; Length
(list/append a b)         ; Concatenate
(list/reverse lst)        ; Reverse
(list/map lst f)          ; Map function
(list/filter lst pred)    ; Filter
(list/fold lst init f)    ; Fold/reduce
(list/take lst n)         ; First n elements
(list/drop lst n)         ; Skip n elements
```

### std.map

Hash maps (compile-time only).

```lisp
(import std.map)

;; All operations 🔴
(map/new)                 ; Empty map
(map/insert m k v)        ; Insert key-value
(map/get m k)             ; Get value (Option)
(map/remove m k)          ; Remove key
(map/contains? m k)       ; Check key exists
(map/keys m)              ; List of keys
(map/values m)            ; List of values
(map/entries m)           ; List of pairs
(map/for-each m f)        ; Iterate
```

### std.iter

Iterator combinators (compile-time only).

```lisp
(import std.iter)

;; All operations 🔴
(iter/range start end)
(iter/map iter f)
(iter/filter iter pred)
(iter/take iter n)
(iter/drop iter n)
(iter/zip iter1 iter2)
(iter/chain iter1 iter2)
(iter/cycle iter)         ; Infinite!
(iter/collect iter)       ; To list
```

---

## Using the Standard Library

### Importing

```lisp
;; Import entire module
(import std.array)
(array/length arr)

;; Import specific items
(import (std.array length get set!))
(length arr)

;; Aliased import
(import (std.crypto :as crypto))
(crypto/sha256 data)

;; Multiple imports
(import std.array
        std.result
        (std.crypto sha256 ed25519-verify))
```

### Phase-Aware Usage

```lisp
;; This is fine - array operations are deploy-safe
(defun process (data)
  #:deploy
  (array/map data transform))

;; This fails - list is compile-time only
(defun bad-process (data)
  #:deploy
  (list/map (list/from-array data) transform))  ; ERROR!

;; Use compile-time to prepare, deploy-time to run
(compile-time
  (defconst lookup-table
    (list/fold (list/range 0 256)
               (array/new 256 0)
               (λ (arr i) (array/set! arr i (compute i))))))

(defun fast-lookup (i)
  #:deploy
  (array/get lookup-table i))  ; OK - using pre-computed array
```

---

## Next Steps

- [std.core Documentation](Stdlib-Core.md)
- [std.collections Documentation](Stdlib-Collections.md)
- [std.crypto Documentation](Stdlib-Crypto.md)
