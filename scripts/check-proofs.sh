#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# Reproducible verification of Oblíbený's proof + type-safety claims.
#
# Runs, and fails on the first problem:
#   1. the Idris2 ABI proof layer  (src/abi/oblibeny-abi.ipkg)
#   2. the OCaml language build + conformance suite (dune)
#   3. a guard that no soundness escape hatch has crept into the ABI proofs
#
# Toolchain (install if missing):
#   * Idris2 >= 0.7.0.  Not packaged on most distros; bootstrap from source with
#     Chez Scheme:
#       sudo apt-get install -y chezscheme libgmp-dev
#       git clone --depth 1 -b v0.7.0 https://github.com/idris-lang/Idris2
#       cd Idris2 && make bootstrap SCHEME=chezscheme && sudo make install
#   * OCaml >= 4.14 with dune + menhir + the libs in dune-project.  Via apt:
#       sudo apt-get install -y ocaml ocaml-dune menhir libmenhir-ocaml-dev \
#         libyojson-ocaml-dev libsedlex-ocaml-dev libppx-deriving-ocaml-dev \
#         libppx-deriving-yojson-ocaml-dev libalcotest-ocaml-dev ocaml-findlib
#     ...or via opam: opam install dune menhir sedlex yojson ppx_deriving \
#         ppx_deriving_yojson alcotest

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

fail() { printf '\nFAIL: %s\n' "$1" >&2; exit 1; }

# ---------------------------------------------------------------------------
echo "==> [1/3] Idris2 ABI proof layer"
command -v idris2 >/dev/null 2>&1 || fail "idris2 not found on PATH (see header for install)"
idris2 --build src/abi/oblibeny-abi.ipkg

# ---------------------------------------------------------------------------
echo "==> [2/3] OCaml build + conformance suite"
command -v dune >/dev/null 2>&1 || fail "dune not found on PATH (see header for install)"
dune build
dune runtest

# ---------------------------------------------------------------------------
echo "==> [3/3] escape-hatch guard (no believe_me / postulate / assert_total / partial / holes)"
if grep -rnE 'believe_me|assert_total|\bpostulate\b|\bpartial\b|idris_crash|\?[A-Za-z_][A-Za-z0-9_]*' \
     src/abi/Crypto.idr src/abi/Packages >/dev/null 2>&1; then
  grep -rnE 'believe_me|assert_total|\bpostulate\b|\bpartial\b|idris_crash|\?[A-Za-z_][A-Za-z0-9_]*' \
     src/abi/Crypto.idr src/abi/Packages
  fail "soundness escape hatch found in ABI proofs"
fi

echo
echo "OK: all proofs build, conformance suite passes, no escape hatches."
