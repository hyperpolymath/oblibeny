# Oblíbený Wiki

> **The Dual-Language Paradigm for Secure Edge Computing**

Welcome to the official Oblíbený documentation wiki.

---

## Quick Links

### Getting Started
- [Installation](Installation.md)
- [Quick Start Tutorial](Tutorial-Quick-Start.md)
- [Hello World](Tutorial-Hello-World.md)
- [IDE Setup](IDE-Setup.md)

### Language
- [Language Overview](Language-Overview.md)
- [Syntax Reference](Syntax-Reference.md)
- [Type System](Type-System.md)
- [Dual-Language Paradigm](Dual-Language-Paradigm.md)
- [Compile-Time Features](Compile-Time-Features.md)
- [Deploy-Time Restrictions](Deploy-Time-Restrictions.md)

### Security
- [Capability Model](Capability-Model.md)
- [Resource Bounds](Resource-Bounds.md)
- [Termination Proofs](Termination-Proofs.md)
- [Semantic Obfuscation](Semantic-Obfuscation.md)

### Standard Library
- [Standard Library Overview](Stdlib-Overview.md)
- [Core Module](Stdlib-Core.md)
- [Collections](Stdlib-Collections.md)
- [Numeric](Stdlib-Numeric.md)
- [Crypto](Stdlib-Crypto.md)
- [I/O Capabilities](Stdlib-IO.md)

### Tooling
- [CLI Reference](CLI-Reference.md)
- [Package Manager](Package-Manager.md)
- [Build System](Build-System.md)
- [REPL Guide](REPL-Guide.md)
- [Debugger](Debugger.md)
- [Profiler](Profiler.md)

### Testing & Verification
- [Testing Framework](Testing-Framework.md)
- [Property-Based Testing](Property-Based-Testing.md)
- [Fuzzing Guide](Fuzzing-Guide.md)
- [Formal Verification](Formal-Verification.md)
- [Invariant Checking](Invariant-Checking.md)

### Frameworks
- [Sensor Framework](Framework-Sensor.md)
- [Actuator Framework](Framework-Actuator.md)
- [Protocol Framework](Framework-Protocol.md)
- [State Machine Framework](Framework-FSM.md)

### Advanced
- [Compiler Internals](Compiler-Internals.md)
- [IR Design](IR-Design.md)
- [Code Generation](Code-Generation.md)
- [Optimization Passes](Optimization-Passes.md)
- [Macro System](Macro-System.md)

### Reference
- [Grammar (EBNF)](Grammar-EBNF.md)
- [Error Messages](Error-Messages.md)
- [Glossary](Glossary.md)
- [FAQ](FAQ.md)
- [Troubleshooting](Troubleshooting.md)

---

## About Oblíbený

Oblíbený (Czech for "beloved" or "favorite") is a programming language designed for secure edge computing. It implements a novel **Dual-Language Paradigm**:

1. **Master Language** (Compile-time): Full Turing-complete language with macros, metaprogramming, and unrestricted computation.

2. **Deployment Subset** (Deploy-time): Turing-incomplete subset that is provably terminating, resource-bounded, and capability-controlled.

### Key Features

| Feature | Description |
|---------|-------------|
| Provable Termination | All deploy-time code must terminate |
| Resource Bounds | Static memory, iteration, and call depth limits |
| Capability Security | Explicit tokens for all I/O operations |
| Semantic Obfuscation | Metamorphic code generation |
| Formal Verification | Z3 + Lean 4 integration |

### Target Platforms

- Edge computing nodes
- IoT devices
- Embedded systems
- Sensor networks
- Secure actuators

---

## Version Information

| Component | Version |
|-----------|---------|
| Grammar | 0.6 |
| Compiler | 0.1.0 |
| Specification | Draft |

---

## License

AGPL-3.0-or-later

Copyright 2025 Jonathan D.A. Jewell
