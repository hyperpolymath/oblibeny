# Oblíbený Language Specification

**Version:** 0.1.0
**Status:** Draft
**License:** PMPL-1.0-or-later

## 1. Introduction

Oblíbený (Czech for "favorite" or "beloved") is a secure edge language designed for reversibility and accountability in hostile environments. It features a unique dual-form architecture that ensures Turing-incompleteness at runtime while maintaining Turing-completeness at compile-time.

### 1.1 Design Goals

1. **Provable Termination**: All runtime programs terminate
2. **Complete Reversibility**: Every operation can be undone
3. **Full Accountability**: Every action produces an audit trail
4. **Static Resource Bounds**: All resource usage is known at compile-time

## 2. Dual-Form Architecture

### 2.1 Factory Form (Turing-Complete)

The factory form executes at compile-time and is used for:
- Metaprogramming and code generation
- Template instantiation
- Compile-time computation
- Producing constrained form programs

**Example:**
```scheme
;; Factory form (Turing-complete, compile-time)
(define (generate-loop n)
  (emit `(for i in 0..,(const n) {
    trace("iteration", i);
  })))
```

### 2.2 Constrained Form (Turing-Incomplete)

The constrained form executes at runtime with strict restrictions:
- NO `while` or `loop` keywords
- NO recursive function calls
- ONLY bounded iteration (`for i in 0..N` with static N)
- Acyclic call graph guaranteed

**Example:**
```rust
// Constrained form (Turing-incomplete, runtime)
fn main() -> () {
    let mut counter: i64 = 0;
    checkpoint("start");

    for i in 0..10 {  // Static bound required
        incr(counter, 1);
        trace("count", counter);
    }

    checkpoint("end");
    assert_invariant(counter == 10, "counter must equal 10");
}
```

## 3. Type System

### 3.1 Primitive Types

- `i32`, `i64` - Signed integers (32/64-bit)
- `u32`, `u64` - Unsigned integers (32/64-bit)
- `bool` - Boolean (`true`, `false`)
- `()` - Unit type

### 3.2 Compound Types

- Arrays: `[T; N]` where N is a compile-time constant
- References: `&T`, `&mut T`
- Structs: Named product types
- Functions: `fn(T1, T2) -> R`

### 3.3 Special Types

- `Trace` - Accountability trace type (system-managed)

## 4. Reversible Operations

### 4.1 Operations with Inverses

```rust
incr(x, delta);  // Inverse: decr(x, delta)
decr(x, delta);  // Inverse: incr(x, delta)
push(stack, val); // Inverse: pop(stack)
pop(stack);      // Inverse: push(stack, val)
```

### 4.2 Self-Inverse Operations

```rust
swap(a, b);      // Self-inverse
x ^= val;        // XOR assignment (self-inverse)
```

### 4.3 Reversibility Guarantees

The compiler ensures:
1. Every reversible operation has a well-defined inverse
2. Applying an operation followed by its inverse returns to the original state
3. Reversibility is preserved under composition

## 5. Accountability Trace

### 5.1 Trace Operations

```rust
trace(event_name, args...);  // Log event with arguments
checkpoint(label);           // Mark execution checkpoint
assert_invariant(cond, msg); // Verify invariant with message
```

### 5.2 Trace Properties

- **Complete**: Every significant operation is logged
- **Immutable**: Trace entries cannot be modified after creation
- **Verifiable**: Trace has cryptographic hash for integrity
- **Reversible**: Trace shows forward and reverse operations

## 6. Constrained Form Rules

### 6.1 Syntactic Restrictions

| Forbidden | Reason |
|-----------|--------|
| `while` keyword | Unbounded iteration |
| `loop` keyword | Unbounded iteration |
| Direct recursion | Unbounded computation |
| Indirect recursion | Cyclic call graph |

### 6.2 Static Bounds

```rust
// VALID: Static bound
for i in 0..100 { ... }

// INVALID: Dynamic bound
let n = read_input();
for i in 0..n { ... }  // Compile error
```

### 6.3 Call Graph Requirements

- Must be a Directed Acyclic Graph (DAG)
- Topologically sortable
- No mutual recursion
- Maximum depth statically known

## 7. Security Model

### 7.1 Termination Guarantee

**Theorem**: All valid constrained form programs terminate.

**Proof**:
1. No unbounded loops (only bounded `for`)
2. No recursion (acyclic call graph)
3. Bounded call depth
4. Therefore, finite computation steps ∎

### 7.2 Resource Bounds

All resource usage is statically bounded:
- **Time**: O(max_iterations × max_call_depth)
- **Space**: O(stack_vars + heap_allocations)
- **Trace**: O(trace_operations)

### 7.3 Deployment Safety

Turing-incompleteness provides:
- No infinite loops in production
- Predictable worst-case execution time
- Bounded memory consumption
- Complete audit trail

## 8. Example Programs

### 8.1 Fibonacci (Bounded)

```rust
fn main() -> () {
    let mut a: i64 = 0;
    let mut b: i64 = 1;

    checkpoint("start_fibonacci");

    for i in 0..10 {
        trace("fib", a);
        let tmp: i64 = a + b;
        a = b;
        b = tmp;
    }

    checkpoint("end_fibonacci");
    assert_invariant(a == 55, "10th Fibonacci number should be 55");
}
```

### 8.2 Reversible Swap

```rust
fn main() -> () {
    let mut x: i64 = 42;
    let mut y: i64 = 7;

    checkpoint("before_swap");
    trace("initial", x, y);

    swap(x, y);
    trace("swapped", x, y);

    swap(x, y);
    trace("restored", x, y);

    checkpoint("after_swaps");
    assert_invariant(x == 42 && y == 7, "values restored");
}
```

## 9. Tooling

### 9.1 Compiler

```bash
oblibeny input.obl              # Compile and execute
oblibeny --check input.obl      # Validate only
oblibeny --analyze input.obl    # Static analysis
oblibeny --dump-ast input.obl   # Show AST
```

### 9.2 LSP Server

```bash
oblibeny-lsp  # Start Language Server Protocol server
```

### 9.3 Debugger

Interactive reversible debugger:
```bash
s, step      - Step forward
b, back      - Step backward (reversible!)
p, print     - Show current state
t, trace     - Show accountability trace
```

## 10. Formal Verification

### 10.1 Verified Properties

- ✓ All programs terminate
- ✓ Call graph is acyclic
- ✓ Loop bounds are static
- ✓ Resource usage is bounded

### 10.2 Proof Assistants

Integration with:
- Coq (formal proofs)
- Idris2 (ABI verification)

## 11. References

1. Turchin, V. F. (1986). "The Concept of a Supercompiler"
2. Bennett, C. H. (1973). "Logical Reversibility of Computation"
3. Yokoyama, T. (2010). "Reversible Computation and Reversible Programming Languages"
4. NIST SP 800-160 Vol. 2 (Cyber Resiliency)

## Appendix A: Grammar (BNF)

```bnf
program     ::= decl*
decl        ::= function | struct | const

function    ::= "fn" IDENT "(" params ")" "->" type "{" stmt* "}"
params      ::= (IDENT ":" type ("," IDENT ":" type)*)?

stmt        ::= let_stmt | assign | for_loop | if_stmt | expr_stmt
              | reversible_op | trace_op

let_stmt    ::= "let" "mut"? IDENT ":" type "=" expr ";"
assign      ::= IDENT "=" expr ";"
for_loop    ::= "for" IDENT "in" INT ".." INT "{" stmt* "}"
if_stmt     ::= "if" expr "{" stmt* "}" ("else" "{" stmt* "}")?

reversible_op ::= "incr" "(" IDENT "," expr ")"
                | "decr" "(" IDENT "," expr ")"
                | "swap" "(" IDENT "," IDENT ")"
                | IDENT "^=" expr

trace_op    ::= "trace" "(" STRING ("," expr)* ")"
              | "checkpoint" "(" STRING ")"
              | "assert_invariant" "(" expr "," STRING ")"

expr        ::= INT | BOOL | IDENT | binop | call
type        ::= "i32" | "i64" | "u32" | "u64" | "bool" | "()"
```
