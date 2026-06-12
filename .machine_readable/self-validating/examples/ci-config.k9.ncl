K9!
# SPDX-License-Identifier: MPL-2.0
# Example Yard-level K9 component: CI/CD configuration with validation (oblibeny)
# Security Level: Yard (Nickel evaluation, contract validation)
# Signature recommended but not required
#
# Mirrors the real gate in .github/workflows/ci.yml (PR #52): OCaml
# build + conformance suite, Idris2 ABI proof type-check.

{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "ci-configuration",
    security = {
      leash = 'Yard,
      trust_level = "validated-config",
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
    },
    metadata = {
      name = "ci-config",
      version = "1.0.0",
      description = "CI/CD configuration with runtime validation",
      author = "Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>",
    },
  },

  # CI/CD configuration with Nickel contracts
  ci = {
    # Platform must be a known CI provider
    platform
      | [| 'GitHubActions, 'GitLabCI, 'CircleCI, 'TravisCI |]
      = 'GitHubActions,

    # Build matrix with validation
    matrix = {
      # Operating systems to test on (ci.yml runs ubuntu-latest)
      os
        | Array String
        | std.array.NonEmpty
        = ["ubuntu-latest"],

      # Toolchain channels exercised
      versions
        | Array String
        | std.array.NonEmpty
        = ["system-ocaml", "idris2-0.7.0"],
    },

    # Workflow steps with validation (per .github/workflows/ci.yml)
    steps = [
      {
        name = "Checkout",
        action = "actions/checkout@v6",
        # Version must be SHA-pinned for security
        sha | String | std.string.NonEmpty = "df4cb1c069e1874edd31b4311f1884172cec0e10",
      },
      {
        name = "Build",
        run = "dune build",
      },
      {
        name = "Conformance suite",
        run = "dune runtest",
      },
      {
        name = "Idris2 ABI proofs",
        run = "idris2 --build src/abi/oblibeny-abi.ipkg",
      },
    ],

    # Deployment configuration
    deploy = {
      enabled | Bool = false,

      # Only deploy from main branch
      branch
        | String
        | std.contract.from_predicate (fun b => b == "main" || b == "master")
        = "main",

      # Deployment requires manual approval
      requires_approval | Bool = true,
    },

    # Security scanning (the repo's real scanner set)
    security = {
      enabled | Bool = true,

      scanners = [
        {
          name = "CodeQL",
          # This repo scans GitHub Actions workflows, not JS/TS
          # (see 6a2/AGENTIC.a2ml codeql-actions-scan).
          languages = ["actions"],
        },
        {
          name = "OSSF Scorecard",
          enabled = true,
        },
        {
          name = "TruffleHog",
          scan_for = "secrets",
        },
        {
          name = "Hypatia",
          enabled = true,
        },
        {
          name = "Semgrep",
          enabled = true,
        },
      ],
    },

    # Notification settings
    notifications = {
      on_success = "never",
      on_failure = "always",
      channels = ["email"],
    },
  },

  # Validation rules (enforced by Nickel)
  validation = {
    # At least one OS must be specified
    check_os = std.array.length ci.matrix.os > 0,

    # At least one version must be tested
    check_versions = std.array.length ci.matrix.versions > 0,

    # Must have at least build and test steps
    check_steps = std.array.length ci.steps >= 2,

    # Security scanning must be enabled
    check_security = ci.security.enabled == true,
  },
}
