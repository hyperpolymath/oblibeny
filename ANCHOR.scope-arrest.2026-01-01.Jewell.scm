;; SPDX-License-Identifier: MIT OR Palimpsest-0.8
;; SPDX-FileCopyrightText: 2026 Hyperpolymath

;; ANCHOR.scope-arrest.2026-01-01.Jewell.scm  (oblibeny)
(define anchor
  '((schema . "hyperpolymath.anchor/1")
    (repo . "hyperpolymath/oblibeny")
    (date . "2026-01-01")
    (authority . "repo-superintendent")
    (purpose . ("Reclaim identity: Oblíbený is the MAA/Absolute-Zero heart language. Remove unrelated ecosystem content."))
    (identity
      . ((project . "Oblíbený")
         (kind . "secure edge language for reversibility + accountability")
         (domain . "MAA + Absolute-Zero integration")
         (one-sentence . "A secure edge language: a Turing-complete meta/factory form that produces a Turing-incomplete reversible/constraint form, designed for accountability.")))

    (semantic-anchor
      . ((policy . "dual")
         (reference-impl . ("OCaml front-end + reference evaluator are authoritative in f0"))
         (formal-spec . ("SPEC.core.scm defines meta/factory form and the produced constrained form"))
         (integration-contract
           . ("Explicitly document interfaces to Absolute-Zero + Mutually-Assured-Accountability as external contracts, not embedded copies."))))

    ;; f0: strict boundaries. Do NOT run 3 stacks at once unless quarantined.
    (allowed-implementation-languages
      . ("OCaml"))
    (quarantined-optional
      . ("Rust runtime helpers (only if strictly needed, non-authoritative)"
         "LLVM backend (for f2+ only, never required for correctness)"))
    (forbidden
      . ("Oblivious computing ecosystem takeover"
         "Submodules that redefine repo identity"
         "Implementing LLVM backend in f0"
         "Adding extra host languages"))

    (mandatory-restructure
      . ("If repo currently contains unrelated obli-* ecosystems: relocate them to a sibling repo."
         "Root must contain language skeleton: parser/typecheck/eval + conformance corpus."
         "README must state the language identity above; anything else becomes 'related projects'."))

    (golden-path
      . ((smoke-test-command . "dune test && dune exec -- examples/hello.obl")
         (success-criteria . ("factory form produces constrained form"
                              "constrained form is Turing-incomplete by construction (enforced syntactically)"
                              "reversibility invariants validated on at least 1 example"))))

    (first-pass-directives
      . ("Write SPEC.core.scm focusing only on: (1) meta/factory form, (2) produced constrained form, (3) reversibility invariant, (4) accountability trace."
         "Add conformance programs that try to create loops in constrained form; they must be rejected."
         "Do not attempt full integration with other repos in f0; only define interfaces."))

    (rsr
      . ((target-tier . "bronze-now") (upgrade-path . "silver-after-f1")))))
