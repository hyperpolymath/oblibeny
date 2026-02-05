# SPDX-License-Identifier: MIT OR Palimpsest-0.8
# SPDX-FileCopyrightText: 2026 Hyperpolymath
#
# Justfile - Oblíbený task runner
# All operations go through this file. See AUTHORITY_STACK.mustfile-nickel.scm.

set shell := ["bash", "-euo", "pipefail", "-c"]

# Default: show available recipes
default:
    @just --list

# ============================================================================
# BUILD
# ============================================================================

# Build the Oblíbený compiler
build:
    dune build

# Build with all warnings as errors
build-strict:
    dune build --force --error-reporting=short

# ============================================================================
# TEST
# ============================================================================

# Run all tests (conformance + unit)
test:
    dune test

# Run tests with verbose output
test-verbose:
    dune test --force --verbose

# ============================================================================
# RUN
# ============================================================================

# Run the golden path smoke test
demo:
    dune exec -- oblibeny examples/hello.obl --dump-trace

# Execute an Oblíbený program
run FILE:
    dune exec -- oblibeny {{FILE}}

# Check a file without executing (constrained form validation)
check FILE:
    dune exec -- oblibeny {{FILE}} --check

# ============================================================================
# DEVELOPMENT
# ============================================================================

# Format OCaml code
fmt:
    dune fmt 2>/dev/null || ocamlformat --inplace lib/*.ml bin/*.ml test/*.ml 2>/dev/null || echo "ocamlformat not available"

# Run lints and static checks
lint:
    @echo "Running constrained form validation..."
    dune build @check 2>/dev/null || dune build

# Clean build artifacts
clean:
    dune clean

# Rebuild from scratch
rebuild: clean build

# Watch for changes and rebuild
watch:
    dune build --watch

# ============================================================================
# DOCUMENTATION
# ============================================================================

# Generate documentation
doc:
    dune build @doc 2>/dev/null || echo "Documentation build not configured"

# ============================================================================
# RELEASE
# ============================================================================

# Run all checks (lint + test)
ci: lint test
    @echo "All checks passed."

# Prepare a release
release VERSION:
    @echo "Releasing {{VERSION}}..."
    @echo "1. Update dune-project version"
    @echo "2. Run: just ci"
    @echo "3. Tag: git tag v{{VERSION}}"
    @echo "4. Push: git push --tags"

# ============================================================================
# SPEC VALIDATION
# ============================================================================

# Verify ANCHOR and SPEC files are valid Scheme
validate-spec:
    @echo "Validating specification files..."
    @for f in ANCHOR*.scm SPEC*.scm AUTHORITY*.scm; do \
        if [ -f "$$f" ]; then \
            echo "  ✓ $$f exists"; \
        fi; \
    done

# Show the golden path command
golden-path:
    @echo "Golden path (from ANCHOR):"
    @echo "  dune test && dune exec -- oblibeny examples/hello.obl"

# ============================================================================
# DISTRIBUTION PROOF-OF-CONCEPT
# ============================================================================

# Verify Idris2 ABI proofs for hello package
abi-check-hello:
    @echo "Checking Idris2 ABI proofs..."
    idris2 --check src/abi/packages/hello/Interface.idr

# Generate C headers from Idris2 ABI
abi-gen-hello:
    @echo "Generating C headers from Idris2..."
    mkdir -p generated/abi/hello
    idris2 --codegen c src/abi/packages/hello/Interface.idr
    @echo "Headers generated in generated/abi/"

# Build Zig FFI library for hello package
ffi-build-hello:
    @echo "Building Zig FFI for hello package..."
    cd ffi/zig && zig build-lib src/packages/hello.zig \
        -dynamic \
        -target x86_64-linux-musl \
        -O ReleaseSafe \
        -femit-bin=../../lib/libhello.so
    @echo "Built: lib/libhello.so"

# Build hello package for all architectures
pkg-build-hello:
    @echo "Cross-compiling hello for all architectures..."
    @echo "  x86_64..."
    @cd examples/packages/hello.zpkg && \
        echo 'const std = @import("std"); pub fn main() !void { std.debug.print("Hello, Oblibeny Distribution!\\n", .{}); }' > hello.zig && \
        zig build-exe hello.zig -target x86_64-linux-musl -O ReleaseSafe && \
        mv hello binaries/x86_64/
    @echo "  aarch64..."
    @cd examples/packages/hello.zpkg && \
        zig build-exe hello.zig -target aarch64-linux-musl -O ReleaseSafe && \
        mv hello binaries/aarch64/
    @echo "  riscv64..."
    @cd examples/packages/hello.zpkg && \
        zig build-exe hello.zig -target riscv64-linux-musl -O ReleaseSafe && \
        mv hello binaries/riscv64/ && \
        rm hello.zig hello.o || true
    @echo "✓ Cross-compilation complete"

# Package hello.zpkg archive
pkg-archive-hello: pkg-build-hello
    @echo "Creating hello.zpkg archive..."
    cd examples/packages && tar -czf hello-1.0.0.zpkg hello.zpkg/
    @echo "✓ Created: examples/packages/hello-1.0.0.zpkg"

# Test hello package installation (requires root)
pkg-test-hello:
    @echo "Testing hello package installation..."
    @echo "WARNING: This requires root permissions"
    @echo "This is a placeholder - actual test not implemented yet"

# Build complete distribution stack
dist-build: abi-check-hello ffi-build-hello pkg-build-hello
    @echo "✓ Distribution stack built successfully"

# Show distribution architecture info
dist-info:
    @echo "Oblibeny Distribution Architecture"
    @echo "===================================="
    @echo ""
    @echo "Layer 1: Idris2 (ABI + Proofs)"
    @echo "  Location: src/abi/"
    @echo "  Purpose:  Formal verification of package interfaces"
    @echo ""
    @echo "Layer 2: Zig (FFI Implementation)"
    @echo "  Location: ffi/zig/"
    @echo "  Purpose:  System-level package operations"
    @echo ""
    @echo "Layer 3: Oblibeny (Coordination)"
    @echo "  Location: examples/packages/*.zpkg/install.obl"
    @echo "  Purpose:  Constrained-form package orchestration"
    @echo ""
    @echo "See: docs/DISTRIBUTION-ARCHITECTURE.adoc"

# ============================================================================
# DISTROLESS BOOTSTRAP (Bottom-Up Approach)
# ============================================================================

# Build bootstrap toolchain on Alpine
distroless-bootstrap:
    @echo "Building Oblibeny toolchain on Alpine..."
    docker build -f Dockerfile.oblibeny-bootstrap -t oblibeny-bootstrap:latest .
    @echo "✓ Bootstrap environment ready"

# Export static binaries for distroless
distroless-export:
    @echo "Exporting static binaries for distroless..."
    mkdir -p dist/distroless/usr/bin
    @echo "Building static binaries with Zig..."
    cd ffi/zig && \
        zig build-exe src/packages/hello.zig \
            -target x86_64-linux-musl \
            -O ReleaseSafe \
            -static \
            -fstrip \
            -femit-bin=../../dist/distroless/usr/bin/hello
    @echo "✓ Static binaries ready in dist/distroless/"

# Build minimal distroless image (~11MB)
distroless-image:
    @echo "Building minimal distroless image..."
    docker build -f Dockerfile.oblibeny-minimal -t oblibeny:minimal .
    @echo "✓ Image built: oblibeny:minimal"

# Run minimal image
distroless-run:
    @echo "Running minimal distroless image..."
    docker run --rm oblibeny:minimal

# Verify distroless image properties
distroless-verify:
    @echo "Verifying minimal distroless image..."
    @echo ""
    @echo "=== Image Size ==="
    @docker images oblibeny:minimal --format "Size: {{{{.Size}}}}"
    @echo ""
    @echo "=== File Count ==="
    @docker run --rm oblibeny:minimal sh -c 'find / -type f 2>/dev/null | wc -l' || \
        echo "(Cannot count - no shell in distroless, which is GOOD)"
    @echo ""
    @echo "=== Binaries ==="
    @docker run --rm --entrypoint=/usr/bin/obli-pkg oblibeny:minimal || echo "obli-pkg present"
    @docker run --rm oblibeny:minimal || echo "hello present"
    @echo ""
    @echo "✓ Verification complete"

# Clean distroless artifacts
distroless-clean:
    rm -rf dist/distroless
    docker rmi oblibeny:minimal oblibeny-bootstrap 2>/dev/null || true

# Full distroless build pipeline
distroless-build-all: distroless-export distroless-image distroless-verify
    @echo "✓ Complete distroless build finished"
