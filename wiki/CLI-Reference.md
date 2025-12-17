# CLI Reference

The `oblc` command-line interface provides all tools for developing, building, testing, and deploying Oblíbený programs.

---

## Table of Contents

1. [Installation](#installation)
2. [Global Options](#global-options)
3. [Commands](#commands)
4. [Project Management](#project-management)
5. [Building](#building)
6. [Testing](#testing)
7. [Verification](#verification)
8. [Deployment](#deployment)
9. [REPL](#repl)
10. [Configuration](#configuration)

---

## Installation

### From Source

```bash
git clone https://github.com/oblibeny/oblibeny.git
cd oblibeny
cargo install --path .
```

### Package Managers

```bash
# Homebrew (macOS/Linux)
brew install oblibeny

# Cargo
cargo install oblc

# Nix
nix profile install nixpkgs#oblibeny
```

### Verify Installation

```bash
oblc --version
# oblc 0.1.0 (grammar 0.6)
```

---

## Global Options

```
USAGE:
    oblc [OPTIONS] <COMMAND>

OPTIONS:
    -h, --help           Print help information
    -V, --version        Print version information
    -v, --verbose        Increase verbosity (-v, -vv, -vvv)
    -q, --quiet          Suppress output
    --color <WHEN>       Color output [auto|always|never]
    --config <FILE>      Use custom config file
    --offline            Don't access network
    --target <TARGET>    Target architecture
    --profile <PROFILE>  Build profile [dev|release|deploy]
```

---

## Commands

### Quick Reference

| Command | Description |
|---------|-------------|
| `new` | Create new project |
| `init` | Initialize in existing directory |
| `build` | Compile project |
| `check` | Type check without codegen |
| `run` | Build and execute |
| `test` | Run tests |
| `verify` | Formal verification |
| `doc` | Generate documentation |
| `fmt` | Format source code |
| `lint` | Static analysis |
| `deploy` | Package for deployment |
| `repl` | Interactive mode |
| `clean` | Remove build artifacts |

---

## Project Management

### `oblc new`

Create a new Oblíbený project.

```bash
oblc new my-project
oblc new my-library --lib
oblc new sensor-app --template sensor
```

**Options:**

| Option | Description |
|--------|-------------|
| `--lib` | Create library project |
| `--bin` | Create binary project (default) |
| `--template <T>` | Use template (sensor, actuator, protocol) |
| `--name <NAME>` | Override package name |
| `--vcs <VCS>` | Version control [git|none] |

**Generated Structure:**

```
my-project/
├── Oblibeny.toml
├── src/
│   └── main.obl       # or lib.obl for --lib
├── tests/
│   └── test_main.obl
└── .gitignore
```

### `oblc init`

Initialize Oblíbený in existing directory.

```bash
cd existing-project
oblc init
oblc init --lib
```

### `oblc clean`

Remove build artifacts.

```bash
oblc clean              # Remove target directory
oblc clean --doc        # Also remove generated docs
oblc clean --all        # Remove everything including cache
```

---

## Building

### `oblc build`

Compile the project.

```bash
oblc build                    # Debug build
oblc build --release          # Optimized build
oblc build --deploy           # Deploy-ready build
oblc build --target arm64     # Cross-compile
```

**Options:**

| Option | Description |
|--------|-------------|
| `--release` | Enable optimizations |
| `--deploy` | Deploy-time verification + obfuscation |
| `--target <T>` | Target triple (arm64, riscv64, wasm32) |
| `--profile <P>` | Custom profile from Oblibeny.toml |
| `--features <F>` | Enable features |
| `--all-features` | Enable all features |
| `--no-default-features` | Disable default features |
| `--jobs <N>` | Parallel jobs |
| `--timings` | Show compilation timings |

**Output:**

```
   Compiling my-project v1.0.0
   Analyzing termination...
   Verifying resource bounds...
   Generating code...
    Finished deploy [optimized + verified] in 2.34s
     Output target/deploy/my-project
```

### `oblc check`

Type check without generating code.

```bash
oblc check               # Check main project
oblc check --all         # Check all workspace members
oblc check --lib         # Check library only
```

**Options:**

| Option | Description |
|--------|-------------|
| `--all` | Check all targets |
| `--lib` | Check library only |
| `--bin <NAME>` | Check specific binary |
| `--strict` | Treat warnings as errors |

### `oblc run`

Build and execute.

```bash
oblc run                 # Run main binary
oblc run --release       # Run optimized build
oblc run -- arg1 arg2    # Pass arguments
oblc run --bin other     # Run specific binary
```

---

## Testing

### `oblc test`

Run the test suite.

```bash
oblc test                          # All tests
oblc test unit                     # Unit tests only
oblc test integration              # Integration tests
oblc test --filter "sensor*"       # Filter by name
oblc test --coverage               # With coverage
```

**Options:**

| Option | Description |
|--------|-------------|
| `--filter <PAT>` | Filter tests by pattern |
| `--coverage` | Generate coverage report |
| `--coverage-format <F>` | Coverage format [html|lcov|json] |
| `--parallel` | Run tests in parallel (default) |
| `--no-parallel` | Run tests sequentially |
| `--timeout <SEC>` | Test timeout |
| `--fail-fast` | Stop on first failure |
| `--property-iters <N>` | Property test iterations |
| `--fuzz` | Enable fuzzing |
| `--fuzz-timeout <SEC>` | Fuzzing timeout |
| `--mutation` | Run mutation testing |

**Test Output:**

```
   Running tests/unit/math.obl
test addition ... ok
test subtraction ... ok
test multiplication ... ok

   Running tests/property/invariants.obl
property balance_never_negative ... ok (1000 iterations)
property state_transitions_valid ... ok (1000 iterations)

   Running tests/fuzz/parser.obl
fuzz parse_message ... ok (10000 iterations, 0 crashes)

test result: ok. 6 passed; 0 failed; finished in 1.23s
```

### Coverage Report

```bash
oblc test --coverage --coverage-format html
open target/coverage/index.html
```

---

## Verification

### `oblc verify`

Run formal verification.

```bash
oblc verify                    # Verify all
oblc verify --function foo     # Verify specific function
oblc verify --module sensors   # Verify module
oblc verify --property P1      # Verify specific property
```

**Options:**

| Option | Description |
|--------|-------------|
| `--function <F>` | Verify specific function |
| `--module <M>` | Verify specific module |
| `--property <P>` | Verify specific property |
| `--prover <P>` | Prover backend [z3|lean4|isabelle] |
| `--timeout <SEC>` | Verification timeout |
| `--parallel <N>` | Parallel verification jobs |
| `--export-proofs` | Export proof certificates |
| `--verbose` | Show verification details |

**Verification Output:**

```
   Verifying my-project v1.0.0

Checking termination...
  ✓ process_sensor: bounded-loop (max 256 iterations)
  ✓ handle_command: trivial (no loops)
  ✓ main_loop: bounded-loop (max 1000 iterations)

Checking resource bounds...
  ✓ max_iterations: 1256 ≤ 10000
  ✓ max_stack_depth: 12 ≤ 256
  ✓ max_memory: 2048 ≤ 4096
  ✓ max_call_depth: 4 ≤ 16

Checking invariants...
  ✓ balance_invariant: proven via Z3
  ✓ state_invariant: proven via Z3

Verification: 8 checks passed, 0 failed
```

### `oblc prove`

Generate formal proofs.

```bash
oblc prove termination --output proofs/
oblc prove resources --prover lean4
oblc prove invariants --export-to isabelle
```

---

## Deployment

### `oblc deploy`

Package for deployment.

```bash
oblc deploy                        # Default deployment
oblc deploy --target edge-device   # Specific target
oblc deploy --obfuscation paranoid # Max obfuscation
oblc deploy --output dist/         # Custom output
```

**Options:**

| Option | Description |
|--------|-------------|
| `--target <T>` | Deployment target |
| `--profile <P>` | Target profile |
| `--obfuscation <L>` | Level [none|minimal|aggressive|paranoid] |
| `--output <DIR>` | Output directory |
| `--manifest <F>` | Deployment manifest |
| `--sign` | Sign output binary |
| `--verify-deploy` | Verify deployment constraints |

**Deployment Profiles:**

| Profile | Description |
|---------|-------------|
| `edge-minimal` | Minimal edge node |
| `iot-secure` | Secure IoT device |
| `embedded-hardened` | Hardened embedded |
| `sensor-node` | Sensor network node |
| `actuator-minimal` | Minimal actuator |

**Deployment Output:**

```
   Deploying my-project v1.0.0

Target: edge-device
Profile: edge-minimal
Obfuscation: aggressive

Verifying deployment constraints...
  ✓ No syscalls
  ✓ No recursion
  ✓ No dynamic allocation
  ✓ All loops bounded
  ✓ Resource bounds satisfied

Generating obfuscated code...
  Applied 847 semantic transformations
  Code size: 2,048 bytes

Signing binary...
  Algorithm: Ed25519
  Key: deploy-key-001

    Deployed target/deploy/my-project.bin (2,048 bytes)
    Manifest target/deploy/manifest.json
    Signature target/deploy/my-project.sig
```

---

## REPL

### `oblc repl`

Start interactive mode.

```bash
oblc repl                  # Start REPL
oblc repl --load src/lib.obl  # Load file on start
oblc repl --compile-time   # Compile-time mode
```

**REPL Commands:**

| Command | Description |
|---------|-------------|
| `:help` | Show help |
| `:quit` | Exit REPL |
| `:load <file>` | Load file |
| `:reload` | Reload last file |
| `:type <expr>` | Show expression type |
| `:info <name>` | Show binding info |
| `:clear` | Clear screen |
| `:reset` | Reset environment |
| `:history` | Show history |
| `:set <opt>` | Set option |

**REPL Session:**

```
oblíbený v0.1.0 (grammar 0.6)
Type :help for help, :quit to exit

λ> (defun square (x) (* x x))
=> <function: square>

λ> (square 5)
=> 25

λ> :type square
=> (-> i64 i64)

λ> :load src/math.obl
Loaded 12 definitions

λ> (math/factorial 10)
=> 3628800

λ> :quit
Goodbye!
```

---

## Documentation

### `oblc doc`

Generate documentation.

```bash
oblc doc                   # Generate docs
oblc doc --open            # Generate and open in browser
oblc doc --no-deps         # Don't document dependencies
oblc doc --private         # Include private items
```

**Options:**

| Option | Description |
|--------|-------------|
| `--open` | Open in browser |
| `--no-deps` | Exclude dependencies |
| `--private` | Include private items |
| `--output <DIR>` | Output directory |
| `--format <F>` | Format [html|markdown] |

---

## Code Quality

### `oblc fmt`

Format source code.

```bash
oblc fmt                   # Format all files
oblc fmt --check           # Check without modifying
oblc fmt src/main.obl      # Format specific file
```

### `oblc lint`

Static analysis.

```bash
oblc lint                  # Lint all files
oblc lint --fix            # Auto-fix issues
oblc lint --strict         # Stricter checks
oblc lint --clippy         # Enable all lints
```

**Lint Categories:**

| Category | Description |
|----------|-------------|
| `correctness` | Likely bugs |
| `style` | Code style |
| `performance` | Performance issues |
| `security` | Security concerns |
| `deploy` | Deploy-time issues |

---

## Configuration

### Oblibeny.toml

```toml
[package]
name = "my-project"
version = "1.0.0"
edition = "2025"

[dependencies]
std = "0.1"
crypto = { version = "0.2", features = ["sha256"] }

[dev-dependencies]
obl-test = "0.1"

[build]
target = "arm64"
opt-level = 3

[deploy]
profile = "edge-minimal"
obfuscation = "aggressive"
bounds = { max-iterations = 10000, max-memory = 4096 }

[test]
parallel = true
property-iterations = 1000

[lints]
security = "deny"
performance = "warn"
```

### Environment Variables

| Variable | Description |
|----------|-------------|
| `OBLC_HOME` | Oblíbený home directory |
| `OBLC_CACHE` | Cache directory |
| `OBLC_TARGET` | Default target |
| `OBLC_COLOR` | Color output |
| `OBLC_JOBS` | Parallel jobs |

---

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | General error |
| 2 | Parse error |
| 3 | Type error |
| 4 | Phase violation |
| 5 | Termination unprovable |
| 6 | Resource exceeded |
| 7 | Verification failed |
| 101 | Internal error |

---

## Next Steps

- [Package Manager](Package-Manager.md)
- [Build System](Build-System.md)
- [REPL Guide](REPL-Guide.md)
