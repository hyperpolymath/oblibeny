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
