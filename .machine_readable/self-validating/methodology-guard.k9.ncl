# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# K9 Validator: Methodology Guard (oblibeny)
# Checks that agent work respects the constraints declared in
# .machine_readable/bot_directives/ and the CI escape-hatch guard.
#
# Usage: k9 validate methodology-guard

let methodology_guard = {
  name = "methodology-guard",
  version = "1.0.0",
  description = "Validates that agent work respects declared methodology constraints",

  checks = {
    divergent_invariant_language = {
      description = "No files in languages violating the proof-layer language invariant",
      severity = "error",
      # The proof layer is Idris2 (src/abi/). Reject new .lean / .v /
      # .agda files there — the language invariant for proofs is Idris2.
      check_type = "file-extension-guard",
      scope = "src/abi/",
    },

    believe_me_ceiling = {
      description = "believe_me count must not exceed declared ceiling (mirrors the ci.yml escape-hatch guard, PR #52)",
      severity = "error",
      pattern = "believe_me",
      scope = "src/abi/",
      default_ceiling = 0,
    },

    assert_total_ceiling = {
      description = "assert_total count must not exceed declared ceiling (mirrors the ci.yml escape-hatch guard, PR #52)",
      severity = "error",
      pattern = "assert_total",
      scope = "src/abi/",
      default_ceiling = 0,
    },

    state_not_template = {
      description = "STATE.a2ml must not contain template placeholders",
      severity = "warning",
      file = ".machine_readable/6a2/STATE.a2ml",
      reject_patterns = ["{{PLACEHOLDER}}", "{{PROJECT}}", "rsr-template-repo"],
    },

    state_updated = {
      description = "STATE.a2ml should be re-checkpointed within 90 days (oblibeny has no coverage.a2ml; staleness is tracked on STATE itself)",
      severity = "info",
      file = ".machine_readable/6a2/STATE.a2ml",
      staleness_days = 90,
    },
  },
}
in methodology_guard
