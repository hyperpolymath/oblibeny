# Oblíbený VSCode Extension

Language support for Oblíbený - secure edge language for reversibility and accountability.

## Features

- **Syntax Highlighting**: Full syntax highlighting for `.obl` files
- **Reversible Operations**: Special highlighting for reversible operations (swap, incr/decr, xor)
- **Accountability Tracing**: Highlights trace/checkpoint/assert_invariant calls
- **Constrained Form Validation**: Errors on disallowed `while`/`loop` keywords
- **LSP Integration**: Full Language Server Protocol support (coming soon)

## Grammar Awareness

The extension understands Oblíbený's dual-form architecture:
- **Factory Form**: Turing-complete metaprogramming (compile-time)
- **Constrained Form**: Turing-incomplete runtime execution

The syntax highlighter will mark `while` and `loop` keywords as **invalid** since they violate the constrained form's Turing-incompleteness guarantee.

## Reversible Operations

Special highlighting for operations with inverses:
- `swap(a, b)` - self-inverse
- `incr(x, delta)` ↔ `decr(x, delta)`
- `x ^= val` - self-inverse XOR
- `push(stack, val)` ↔ `pop(stack)`

## Accountability Features

Highlighted trace operations:
- `trace(event, args...)` - Append to accountability log
- `checkpoint(label)` - Create named checkpoint
- `assert_invariant(condition, message)` - Verify invariants

## Installation

### From VSIX (Local)
```bash
cd editors/vscode
deno run --allow-all npm:vsce package
code --install-extension oblibeny-0.1.0.vsix
```

### From Source
1. Copy `editors/vscode` to `~/.vscode/extensions/oblibeny-0.1.0/`
2. Reload VSCode

## Requirements

- VSCode 1.75.0 or higher
- Oblíbený compiler installed (`oblibeny` in PATH)
- Oblíbený LSP server (`oblibeny-lsp` in PATH)

## License

PMPL-1.0-or-later (Palimpsest License)
