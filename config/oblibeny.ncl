# SPDX-License-Identifier: MIT OR Palimpsest-0.8
# SPDX-FileCopyrightText: 2026 Hyperpolymath
#
# Oblíbený Configuration Manifest
# This is the single source of truth for project configuration.
# See AUTHORITY_STACK.mustfile-nickel.scm.

{
  # Project metadata
  project = {
    name = "oblibeny",
    display_name = "Oblíbený",
    version = "0.1.0",
    description = "A secure edge language: a Turing-complete meta/factory form that produces a Turing-incomplete reversible/constraint form, designed for accountability.",

    # Identity from ANCHOR
    identity = {
      kind = "secure edge language for reversibility + accountability",
      domain = "MAA + Absolute-Zero integration",
    },
  },

  # Language specification
  language = {
    # Dual-form architecture
    forms = {
      factory = {
        turing_complete = true,
        execution_phase = "compile-time",
      },
      constrained = {
        turing_complete = false,
        execution_phase = "runtime",
        # Syntactic restrictions for Turing-incompleteness
        restrictions = [
          "no-while-loops",
          "no-loop-keyword",
          "no-recursion",
          "static-loop-bounds",
          "acyclic-call-graph",
        ],
      },
    },

    # Reversibility primitives
    reversible_ops = {
      self_inverse = ["swap", "xor-assign", "not"],
      paired_inverse = {
        incr = "decr",
        decr = "incr",
        push = "pop",
        pop = "push",
      },
    },
  },

  # Build configuration
  build = {
    system = "dune",
    ocaml_version = "4.14+",
    dependencies = [
      "menhir",
      "sedlex",
      "yojson",
      "ppx_deriving",
      "ppx_deriving_yojson",
      "alcotest",
    ],
  },

  # Task routing (from AUTHORITY_STACK)
  tasks = {
    local = "just",
    deployment = "must",
    config = "nickel",
  },

  # Golden path from ANCHOR
  golden_path = {
    smoke_test = "dune test && dune exec -- oblibeny examples/hello.obl",
    success_criteria = [
      "factory form produces constrained form",
      "constrained form is Turing-incomplete by construction",
      "reversibility invariants validated",
    ],
  },

  # RSR tier
  rsr = {
    current_tier = "bronze-now",
    upgrade_path = "silver-after-f1",
  },
}
