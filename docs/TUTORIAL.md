# Oblíbený Tutorial: Getting Started

Welcome to Oblíbený! This tutorial will teach you the fundamentals of writing reversible, accountable programs.

## Part 1: Hello World

Create a file `hello.obl`:

```rust
fn main() -> () {
    checkpoint("start");
    trace("message", "Hello, Oblíbený!");
    checkpoint("end");
}
```

Run it:
```bash
oblibeny hello.obl
```

**Key concepts:**
- Every program needs a `main()` function
- `checkpoint()` marks important execution points
- `trace()` logs events to the accountability trail

## Part 2: Variables and Types

```rust
fn main() -> () {
    // Immutable binding
    let x: i64 = 42;

    // Mutable binding (required for modifications)
    let mut counter: i64 = 0;

    trace("initial", x, counter);

    // You cannot reassign immutable variables
    // x = 10;  // ERROR!

    // But you can modify mutable ones
    counter = counter + 1;

    trace("updated", counter);
}
```

**Key concepts:**
- `let` creates immutable bindings
- `let mut` creates mutable bindings
- Type annotations are required (for now)

## Part 3: Bounded Loops

Oblíbený only allows loops with **static bounds**:

```rust
fn main() -> () {
    let mut sum: i64 = 0;

    checkpoint("before_loop");

    // VALID: Static bound (0 to 9 inclusive)
    for i in 0..10 {
        sum = sum + i;
        trace("iteration", i, sum);
    }

    checkpoint("after_loop");

    // Sum should be 0+1+2+...+9 = 45
    assert_invariant(sum == 45, "sum should be 45");
}
```

**Key concepts:**
- `for i in 0..N` where N is a compile-time constant
- No `while` or `loop` keywords (they would allow infinite loops!)
- Static bounds guarantee termination

## Part 4: Reversible Operations

Oblíbený provides special operations that can be undone:

```rust
fn main() -> () {
    let mut x: i64 = 10;
    let mut y: i64 = 20;

    checkpoint("initial");
    trace("before", x, y);  // x=10, y=20

    // Increment (can be undone with decrement)
    incr(x, 5);
    trace("after_incr", x);  // x=15

    // Swap (self-inverse)
    swap(x, y);
    trace("after_swap", x, y);  // x=20, y=15

    // XOR (self-inverse)
    x ^= 7;
    trace("after_xor", x);  // x=23

    // Reverse everything!
    x ^= 7;     // Undo XOR
    swap(x, y); // Undo swap
    decr(x, 5); // Undo increment

    trace("restored", x, y);  // x=10, y=20 (back to original!)
    checkpoint("final");
}
```

**Key concepts:**
- `incr(x, delta)` ↔ `decr(x, delta)` are inverses
- `swap(x, y)` is self-inverse
- `x ^= val` (XOR) is self-inverse
- Reversibility enables powerful debugging and auditing

## Part 5: Accountability with Traces

```rust
fn factorial(n: i64) -> i64 {
    let mut result: i64 = 1;
    let mut i: i64 = 1;

    checkpoint("factorial_start");
    trace("input", n);

    for _ in 0..n {
        i = i + 1;
        result = result * i;
        trace("multiply", i, result);
    }

    checkpoint("factorial_end");
    trace("output", result);

    return result;
}

fn main() -> () {
    let five_factorial: i64 = factorial(5);

    assert_invariant(five_factorial == 120, "5! should be 120");
}
```

**Key concepts:**
- Every function should have checkpoints at start/end
- Use `trace()` to log intermediate values
- `assert_invariant()` validates correctness
- Traces create a complete audit trail

## Part 6: Echo Types — Structured, Non-Total Loss

The `echo[A, B]` type is the type-level dual of the reversible core. Where a computation cannot be reversed, it can still retain a structured *echo* of what was lost.

**Concept**: a parity collapse maps any integer to `0` (even) or `1` (odd). The mapping is not injective — both 7 and 9 collapse to 1. An echo retains the original value (the *witness*) alongside the surviving observation (the *visible*):

```rust
fn collapse(n: i64) -> echo[i64, i64] {
    return echo(n, n % 2);
}
```

`echo(n, n % 2)` builds a residue: `n` is the witness (the full source), `n % 2` is the visible (the surviving observation after the parity collapse).

**Reading the echo back**:

```rust
fn main() -> () {
    let a: echo[i64, i64] = collapse(7);
    let b: echo[i64, i64] = collapse(9);

    checkpoint("compare");

    // Both collapsed to 1 (odd parity) — the collapse lost which number it was
    assert_invariant(echo_visible(a) == echo_visible(b), "same surviving observation");

    // But the witnesses retain the original sources — loss was not total
    assert_invariant(echo_witness(a) != echo_witness(b), "distinct retained witnesses");

    checkpoint("done");
}
```

Run this:
```bash
oblibeny examples/echo-types.obl
```

**Affinity**: `echo[A, B]` is usable at most once (affine) if `A` or `B` is a non-copyable type (struct, array, trace). For primitive echoes like `echo[i64, i64]`, both projections can be used freely:

```rust
let e: echo[i64, i64] = collapse(7);
let v: i64 = echo_visible(e);   // fine
let w: i64 = echo_witness(e);   // also fine — echo[i64, i64] is copyable
```

For a non-copyable echo:

```rust
struct Cargo { mass: i64 }
fn ship(m: i64) -> echo[Cargo, i64] {
    return echo(Cargo { mass: m }, m % 2);
}
fn main() -> () {
    let e: echo[Cargo, i64] = ship(7);
    let v: i64 = echo_visible(e);    // OK — e is consumed here
    // let w: Cargo = echo_witness(e); // COMPILE ERROR: e was already consumed
}
```

**Key concepts**:
- `echo(source, base)` forms a residue
- `echo_visible(e)` returns the surviving observation
- `echo_witness(e)` returns the retained source constraint
- Echo is *not* reversible — it exists precisely for computations that cannot be reversed
- See link:LANGUAGE-SPEC.md#_3_4_echo_residue_type[Language Spec §3.4] for the full type rules

## Part 7: What NOT to Do

```rust
// ❌ INVALID: while loop (unbounded)
fn bad_loop() -> () {
    let mut x: i64 = 0;
    while x < 10 {  // COMPILE ERROR: 'while' not allowed
        x = x + 1;
    }
}

// ❌ INVALID: Recursion (unbounded)
fn bad_recursion(n: i64) -> i64 {
    if n == 0 {
        return 1;
    } else {
        return n * bad_recursion(n - 1);  // COMPILE ERROR: recursion detected
    }
}

// ❌ INVALID: Dynamic loop bound
fn bad_dynamic(limit: i64) -> () {
    for i in 0..limit {  // COMPILE ERROR: non-static bound
        trace("i", i);
    }
}
```

**Why these restrictions?**
- Oblíbený guarantees **termination**
- All programs must finish in bounded time
- This makes it safe for deployment in critical systems

## Part 7: Static Analysis

Check your program's resource usage:

```bash
oblibeny --analyze my_program.obl
```

Output:
```
=== Oblíbený Static Analysis Report ===

## Constrained Form Validation
✓ VALID - Program conforms to Turing-incomplete constrained form

## Resource Bounds (Static Guarantees)
Max loop iterations: 100
Max call depth: 3
Estimated memory: 48 bytes

## Reversibility Analysis
✓ All reversible operations are properly balanced

## Accountability Trace Coverage
Coverage: 75.0%
```

## Part 8: Debugging (Reversible!)

```bash
oblibeny --debug my_program.obl
```

Debugger commands:
- `s` or `step` - Execute next statement
- `b` or `back` - **Step backward** through execution!
- `p` or `print` - Show variable values
- `t` or `trace` - Show accountability trace
- `c` or `continue` - Run to next checkpoint

**The debugger can step BACKWARD** because all operations are reversible!

## Next Steps

1. Read the [Language Specification](LANGUAGE-SPEC.md)
2. Study the [Security Model](SECURITY-MODEL.md)
3. Check out more [Examples](../examples/)
4. Try the [VSCode Extension](../editors/vscode/)

## Getting Help

- GitHub Issues: https://github.com/hyperpolymath/oblibeny/issues
- Documentation: https://github.com/hyperpolymath/oblibeny/tree/main/docs

Happy coding with Oblíbený! 🔐✨
