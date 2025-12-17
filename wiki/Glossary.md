# Glossary

Definitions of key terms used in Oblíbený documentation.

---

## A

### Acyclic Call Graph
A call graph with no cycles (no recursion, direct or mutual). Required for deploy-time code to guarantee termination.

### AST (Abstract Syntax Tree)
Tree representation of source code structure. The parser produces an AST from tokens.

### Attenuation
Reducing the permissions or budget of a capability. Capabilities can be attenuated but never elevated.

---

## B

### Bounded-For
A loop construct with compile-time-known iteration bounds. Safe for deploy-time code.
```lisp
(bounded-for (i 0 100) ...)
```

### Budget
A limit on capability usage, such as maximum number of invocations or bytes transferred per time period.

---

## C

### Call Graph
A directed graph where nodes are functions and edges represent function calls. Used for termination and recursion analysis.

### Capability
An unforgeable token granting permission to perform specific operations. No ambient authority exists in deploy-time code.

### Capability Token
A runtime value representing a granted capability, passed explicitly to functions that need it.

### Compile-Time
The phase during compilation when macros are expanded and compile-time blocks execute. Turing-complete.

### Compile-Time Only
Code or features that can only exist during compilation, such as macros and unbounded loops. Marked with `#:compile-only`.

### CORDIC
COordinate Rotation DIgital Computer. An algorithm for computing trigonometric functions using only additions, subtractions, and bit shifts. Used in deploy-time math implementations.

---

## D

### Deploy-Time
The phase when code runs on target devices. Turing-incomplete, with strict resource bounds.

### Deployment Manifest
A configuration file specifying target profile, resource bounds, and capability grants for deployment.

### Deployment Subset
The Turing-incomplete subset of Oblíbený allowed in deploy-time code.

### Dual-Language Paradigm (DLP)
The core concept of Oblíbený: a Turing-complete Master Language for development, and a Turing-incomplete Deployment Subset for production.

---

## E

### Edge Computing
Computing performed at the "edge" of a network, close to data sources like sensors. A primary target domain for Oblíbený.

---

## F

### Fixed-Point Arithmetic
Arithmetic using integers to represent fractional values. Deploy-time safe alternative to floating-point.

### Formal Verification
Mathematically proving properties of code, such as termination or type safety. Oblíbený integrates with Z3 and Lean 4.

---

## G

### Grammar
The formal syntax specification for Oblíbený, defined in EBNF. Current version: 0.6.

---

## H

### HIR (High-Level IR)
Intermediate representation after desugaring. Retains high-level structure but removes syntactic sugar.

### Homoiconicity
The property where code and data share the same representation. In Oblíbený, programs are S-expressions, which are also valid data structures.

---

## I

### Invariant
A property that must always hold. Used in verification and testing.

### IoT (Internet of Things)
Network of physical devices with sensors and actuators. A target domain for Oblíbený.

---

## L

### LIR (Low-Level IR)
Intermediate representation close to machine code. Linear, with explicit registers.

---

## M

### Macro
A compile-time code transformation. Macros can perform arbitrary computation and generate code.

### Master Language
The full Turing-complete language available during compilation.

### Maximal Principle Reduction (MPR)
The methodology of determining upper bounds on all resources at compile time.

### Metamorphic Code
Code that changes its representation while preserving semantics. Used for obfuscation.

### MIR (Mid-Level IR)
Intermediate representation in control-flow-graph form, using SSA (Static Single Assignment).

---

## O

### Obfuscation
Transformation of code to make it difficult to understand while preserving behavior. Levels: none, minimal, aggressive, paranoid.

### Oblíbený
Czech word meaning "beloved" or "favorite". The language name.

### Opaque Predicate
A predicate whose value is known at compile time but difficult to determine through static analysis. Used in obfuscation.

---

## P

### Phase
Either compile-time or deploy-time. The compiler enforces phase separation.

### Phase Annotation
A marker indicating a construct's phase compatibility: `#:compile-only`, `#:deploy`, or `#:both`.

### Phase Violation
An error when compile-time-only features appear in deploy-time code.

### Primitive Recursive
A class of functions that always terminate. Deploy-time code must be primitive recursive.

### Property-Based Testing
Testing approach that verifies properties hold for randomly generated inputs.

---

## R

### Resource Bounds
Static limits on resource usage: iterations, stack depth, memory, call depth, execution time.

### Result Type
A type representing either success (`Ok`) or failure (`Err`). Used instead of exceptions in deploy-time code.

---

## S

### S-Expression
Symbolic expression: the fundamental syntax unit. Either an atom or a list of S-expressions.

### SCC (Strongly Connected Component)
A maximal set of nodes in a graph where every node is reachable from every other. Used to detect recursion.

### Semantic Obfuscation
Obfuscation that changes the structure of code while preserving its meaning.

### SMT (Satisfiability Modulo Theories)
A decision procedure for logical formulas. Z3 is an SMT solver used for verification.

---

## T

### Tarjan's Algorithm
An algorithm for finding strongly connected components in a graph. Used in call graph analysis.

### Termination Proof
A mathematical proof that code will always halt. Required for deploy-time functions.

### Turing-Complete
Capable of computing any computable function, given unlimited resources. The Master Language is Turing-complete.

### Turing-Incomplete
Unable to compute all computable functions. Always terminates. The Deployment Subset is Turing-incomplete.

---

## V

### Verification
Checking that code satisfies specified properties. Includes type checking, phase validation, and formal proofs.

---

## W

### Well-Founded Order
An ordering with no infinite descending chains. Used for termination proofs via structural recursion.

---

## Z

### Z3
An SMT solver from Microsoft Research. Used for constraint-based verification in Oblíbený.
