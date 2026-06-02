;; SPDX-License-Identifier: MPL-2.0
;; PLAYBOOK.scm - Operational runbook for oblibeny

(define playbook
  `((version . "2.0.0")
    (updated . "2026-06-02")

    (procedures
      ((build
         . (("build" . "dune build")
            ("test" . "dune test")
            ("golden-path" . "dune build && dune test && dune exec oblibeny -- examples/echo-types.obl")
            ("analyze" . "dune exec oblibeny -- --analyze examples/echo-types.obl")))

       (release
         . (("1-check-ci" . "Verify all CI checks green on the branch before merging")
            ("2-update-changelog" . "Add release entry to CHANGELOG.md under [Unreleased] -> [X.Y.Z]")
            ("3-update-state" . "Update .machine_readable/STATE.scm version field and add session-history entry")
            ("4-merge" . "Squash-merge to main via GitHub PR")
            ("5-tag" . "git tag vX.Y.Z main && git push origin vX.Y.Z")
            ("6-cleanup" . "Delete feature branches: git push origin --delete <branch>")))

       (rollback
         . (("identify-bad-commit" . "git log --oneline main | head -20")
            ("revert" . "git revert <sha> -- creates a new revert commit, does not rewrite history")
            ("emergency-revert-pr" . "Open a revert PR via GitHub; do not force-push main")))

       (debug-ci-failure
         . (("trusted-base-fail" . "Check docs/proof-debt.md exists; run governance scan locally if possible; triage new believe_me/escape-hatches")
            ("codeql-fail" . "Check .github/workflows/codeql.yml language field is 'actions' not 'javascript-typescript'")
            ("governance-licence-fail" . "grep -r 'SPDX-License-Identifier' on new files; add missing headers")
            ("governance-language-fail" . "Check CLAUDE.md banned list; search for TypeScript/Go/Python/Makefile additions")
            ("circular-ci-dependency" . "If two governance-fix PRs block each other: merge both into the feature branch locally; push; the combined branch satisfies both checks")))

       (add-new-type
         . (("1-ast" . "Add TEcho-equivalent variant to lib/ast.ml")
            ("2-lexer-parser" . "Add keywords and grammar rules to lib/lexer.ml and lib/parser.mly")
            ("3-typecheck" . "Add structural equality, formatting, type inference to lib/typecheck.ml")
            ("4-eval" . "Add VT-equivalent runtime value and evaluation to lib/eval.ml")
            ("5-constrained-check" . "Add traversal into new expression forms in lib/constrained_check.ml")
            ("6-static-analyzer" . "Add memory sizing and call depth analysis to lib/static_analyzer.ml")
            ("7-tests" . "Add to test/conformance_test.ml: typecheck, constrained-check, eval, type-error rejection, safety/no-rhino")
            ("8-example" . "Add runnable .obl example to examples/")
            ("9-docs" . "Update README.adoc, CHANGELOG.md, SPEC.core.scm, LANGUAGE-SPEC.md, TUTORIAL.md, CONTRACTILES.adoc")
            ("10-machine-readable" . "Update STATE.scm session-history, META.scm arch-decisions, ECOSYSTEM.scm if needed")))

       (echo-specific
         . (("test-affinity" . "dune test -- 2>&1 | grep -E 'echo|affin'")
            ("run-echo-example" . "dune exec oblibeny -- examples/echo-types.obl")
            ("analyze-echo" . "dune exec oblibeny -- --analyze examples/echo-types.obl")
            ("conformance-gate" . "test/conformance_test.ml echo-types suite: 11 tests, all must pass")))))

    (alerts
      ((ci-regression . "If any previously-green CI check regresses, stop and diagnose before pushing more commits")
       (trusted-base-new-escapes . "Any new believe_me in Idris2 files must be triaged in docs/proof-debt.md before merging to main")
       (affinity-regression . "If echo affinity conformance tests fail, check is_copyable in lib/typecheck.ml first")))

    (contacts
      ((maintainer . "Jonathan D.A. Jewell <jonathan.jewell@gmail.com>")
       (repo . "https://github.com/hyperpolymath/oblibeny")
       (issues . "https://github.com/hyperpolymath/oblibeny/issues")))))
