# Frequently Asked Questions

---

## General

### What is Oblíbený?

Oblíbený is a programming language designed for secure edge computing. It implements a **Dual-Language Paradigm**: a Turing-complete Master Language for development, and a Turing-incomplete Deployment Subset for production. This ensures that deployed code is provably terminating, resource-bounded, and secure.

### Why the name Oblíbený?

Oblíbený (pronounced oh-BLEE-be-nee) is Czech for "beloved" or "favorite". It reflects the care put into designing a language that developers will love to use while maintaining strong security guarantees.

### What makes Oblíbený different from other languages?

1. **Dual-Language Paradigm**: Full power during development, verified safety during deployment
2. **Provable Termination**: All deploy-time code must provably terminate
3. **Capability-Based Security**: No ambient authority; all I/O requires explicit tokens
4. **Static Resource Bounds**: Memory, iterations, and call depth are bounded at compile time
5. **Semantic Obfuscation**: Built-in code protection through metamorphic transformations

### What are the target use cases?

- Edge computing nodes
- IoT devices
- Embedded systems
- Sensor networks
- Secure actuators
- Any domain requiring verified, bounded execution

---

## Language Design

### Why Lisp/S-expression syntax?

1. **Homoiconicity**: Code is data, enabling powerful metaprogramming
2. **Simple Parsing**: No operator precedence ambiguity
3. **Macro-Friendly**: Easy AST manipulation
4. **Tool-Friendly**: Simple to analyze and transform

### Why not use existing languages with verification?

Existing verified languages typically don't provide:
- Seamless separation between development and deployment phases
- Built-in termination proofs for all deployed code
- Capability-based I/O as a first-class feature
- Semantic obfuscation for anti-tamper protection

### Can I use Oblíbený for general-purpose programming?

Yes, but it's optimized for edge computing. General-purpose features are available in compile-time code, while deploy-time code has restrictions that make it ideal for resource-constrained environments.

---

## Compile-Time vs Deploy-Time

### What's the difference between compile-time and deploy-time?

| Aspect | Compile-Time | Deploy-Time |
|--------|--------------|-------------|
| Turing | Complete | Incomplete |
| Loops | Unbounded (`while`, `for`) | Bounded only (`bounded-for`) |
| Recursion | Allowed | Forbidden |
| Memory | Dynamic allocation | Static only |
| I/O | Unrestricted | Capability-based |

### How do I mark code as compile-time only?

```lisp
(defun my-macro-helper ()
  #:compile-only
  (while (not done) (process)))
```

### How do I mark code as deploy-time safe?

```lisp
(defun sensor-handler (reading :type f32)
  :returns bool
  #:deploy
  #:termination bounded-loop
  (bounded-for (i 0 10)
    (process-channel i reading)))
```

### Can deploy-time code call compile-time code?

No. Deploy-time code can only call other deploy-time or `#:both` functions. The compiler enforces this.

---

## Termination and Resources

### How does Oblíbený prove termination?

Through two mechanisms:
1. **Bounded Loops**: All loops must have compile-time-known iteration bounds
2. **Acyclic Call Graph**: No recursion (direct or mutual) allowed

### What happens if termination can't be proven?

The compiler rejects the code with an error like:
```
error: termination unprovable
  --> src/main.obl:10:5
   |
10 |   (while (not done) (step))
   |   ^^^^^^^^^^^^^^^^^^^^^^^^^ unbounded loop in deploy-time function
```

### How are resource bounds specified?

In the deployment specification:
```lisp
(deployment
  :bounds (:max-iterations 10000
           :max-stack-depth 256
           :max-memory 4096
           :max-call-depth 16))
```

### What if my code exceeds resource bounds?

The compiler rejects it:
```
error: resource bound exceeded
  --> src/main.obl:15:1
   |
15 | (defun heavy-computation ...)
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   |
   = note: iterations: 50000 > max 10000
```

---

## Capabilities

### What is a capability?

A capability is an unforgeable token granting permission to perform specific operations. In Oblíbený, all I/O in deploy-time code requires explicit capability tokens.

### How do I get capabilities?

Capabilities are granted in the deployment manifest and passed to functions as parameters:
```lisp
(defun read-sensor (cap :type (cap sensor-read))
  #:deploy
  (invoke-capability :read cap))
```

### Can I create my own capabilities?

No, user code cannot forge capabilities. They must be granted by the deployment environment.

### What happens if I try to do I/O without a capability?

The compiler rejects it:
```
error: capability required
  --> src/main.obl:5:5
   |
 5 |   (read-sensor 0)  ; no capability provided
   |   ^^^^^^^^^^^^^^^ requires sensor-read capability
```

---

## Tooling

### How do I create a new project?

```bash
oblc new my-project
cd my-project
```

### How do I run tests?

```bash
oblc test
```

### How do I deploy?

```bash
oblc deploy --target edge-device --obfuscation aggressive
```

### Is there IDE support?

Yes, through Language Server Protocol (LSP):
- VS Code extension
- Neovim/Vim plugin
- Emacs mode
- JetBrains plugin

---

## Common Errors

### "Phase violation"

You're using compile-time-only features in deploy-time code:
```lisp
;; BAD
(defun process ()
  #:deploy
  (while true (step)))  ; ERROR: while is compile-time only
```

Fix by using bounded alternatives:
```lisp
;; GOOD
(defun process ()
  #:deploy
  (bounded-for (i 0 100) (step)))
```

### "Type annotation required"

Deploy-time functions need explicit types:
```lisp
;; BAD
(defun add (a b) #:deploy (+ a b))

;; GOOD
(defun add (a :type i64 b :type i64) :returns i64 #:deploy (+ a b))
```

### "Recursion detected"

Deploy-time code cannot be recursive:
```lisp
;; BAD
(defun factorial (n) #:deploy
  (if (= n 0) 1 (* n (factorial (- n 1)))))

;; GOOD - use bounded iteration
(defun factorial (n :type u32) :returns u64 #:deploy
  (let ((result 1))
    (bounded-for (i 1 (+ n 1))
      (set! result (* result i)))
    result))
```

---

## Performance

### Is deploy-time code slower due to bounds checking?

No. Bounds are verified at compile time, not runtime. The generated code has no runtime overhead for bound checking.

### How does obfuscation affect performance?

Obfuscation can add overhead:
- Minimal: ~0% overhead
- Aggressive: ~5-10% overhead
- Paranoid: ~20-50% overhead

Choose the level appropriate for your security/performance trade-off.

---

## Comparison with Other Languages

### vs Rust

| Aspect | Oblíbený | Rust |
|--------|----------|------|
| Safety | Termination + resource bounds | Memory safety |
| Syntax | S-expressions | C-like |
| I/O | Capabilities | System calls |
| Target | Edge/embedded | General-purpose |

### vs Ada/SPARK

| Aspect | Oblíbený | Ada/SPARK |
|--------|----------|-----------|
| Verification | Automatic | Manual contracts |
| Syntax | S-expressions | Pascal-like |
| Metaprogramming | Macros | Limited |
| Learning curve | Moderate | Steep |

### vs Idris/Agda

| Aspect | Oblíbený | Idris/Agda |
|--------|----------|------------|
| Focus | Edge computing | Theorem proving |
| Types | Simple + bounds | Dependent types |
| Practicality | Production-ready | Research-oriented |

---

## Future

### What's on the roadmap?

See [ROADMAP.md](../ROADMAP.md) for detailed plans. Key milestones:
- Q2 2025: Full compiler with code generation
- Q3 2025: Formal verification integration
- Q4 2025: REPL + Interpreter
- 2026: Standard library + Frameworks + Ecosystem

### How can I contribute?

See [CONTRIBUTING.md](../CONTRIBUTING.md). We welcome:
- Bug reports
- Documentation improvements
- Feature implementations
- Test contributions

---

## Getting Help

- **Documentation**: [wiki/Home.md](Home.md)
- **Issues**: https://github.com/oblibeny/oblibeny/issues
- **Discord**: https://discord.gg/oblibeny
- **Email**: oblibeny@example.com
