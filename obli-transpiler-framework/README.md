# Oblibeny Transpiler Framework

The compiler and runtime for the Oblibeny oblivious computing language.

## Architecture

```
┌──────────────────────────────────────────────────────────────────────────┐
│                              Oblibeny Compiler                            │
├──────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌─────────────┐      ┌─────────────┐      ┌─────────────────────────┐  │
│  │   Source    │      │     OIR     │      │    Generated Rust       │  │
│  │   (.obl)    │─────▶│   (JSON)    │─────▶│    + Runtime            │  │
│  └─────────────┘      └─────────────┘      └─────────────────────────┘  │
│         │                    │                        │                  │
│         ▼                    ▼                        ▼                  │
│  ┌─────────────┐      ┌─────────────┐      ┌─────────────────────────┐  │
│  │   OCaml     │      │    Rust     │      │   oblibeny-runtime      │  │
│  │  Frontend   │      │   Backend   │      │   (ORAM + Crypto)       │  │
│  └─────────────┘      └─────────────┘      └─────────────────────────┘  │
│                                                                          │
└──────────────────────────────────────────────────────────────────────────┘
```

## Components

### Frontend (OCaml)

The frontend parses `.obl` source files and performs:
- Lexing and parsing
- Type checking with security labels (@low/@high)
- Obliviousness verification (no secret-dependent branches/indices)
- OIR (Oblivious Intermediate Representation) emission

### Backend (Rust)

The backend consumes OIR and generates:
- Rust code using the oblibeny-runtime
- Calls to constant-time primitives (cmov, cswap)
- ORAM operations (oread, owrite)

### Runtime (Rust)

The runtime library provides:
- **Constant-time primitives**: cmov, cswap, ct_lookup
- **Path ORAM**: O(log N) oblivious memory access
- **Oblivious collections**: OArray, OStack, OQueue, OMap
- **Cryptographic utilities**: AES-GCM, SHA-256, BLAKE3

### Driver

The unified `oblibeny` CLI that orchestrates the pipeline.

## Building

Requires:
- OCaml 4.14+ with opam
- Rust 1.70+
- just (command runner)

```bash
# Install OCaml dependencies
opam install dune menhir sedlex yojson ppx_deriving ppx_deriving_yojson

# Build everything
just build

# Run tests
just test

# Install to ~/.local/bin
just install
```

## Usage

```bash
# Compile to Rust
oblibeny compile program.obl

# Type-check only
oblibeny check program.obl

# Compile and build executable
oblibeny build program.obl
```

## Example

```
// hello.obl - Oblivious array access

@oblivious
fn secret_lookup(arr: oarray<int>, @high idx: int) -> @high int {
    return oread(arr, idx);
}

fn main() {
    let data: oarray<int> = oarray_new(100);

    // Initialize with public indices
    for i in 0..100 {
        owrite(data, i, i * 10);
    }

    // Look up with secret index - access pattern hidden!
    let secret_idx: @high int = get_secret();
    let value: @high int = secret_lookup(data, secret_idx);
}
```

## License

MIT OR Palimpsest-0.8
