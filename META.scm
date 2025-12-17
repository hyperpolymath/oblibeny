;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

;;; META.scm — Architecture Decisions and Development Practices
;;; oblibeny
;;; Reference: https://github.com/hyperpolymath/META.scm

(define-module (oblibeny meta)
  #:export (architecture-decisions
            development-practices
            design-rationale))

;;;============================================================================
;;; Architecture Decision Records (ADR)
;;; Following MADR format in structured S-expression form
;;;============================================================================

(define architecture-decisions
  '((adr-001
     (title . "Initial Architecture and RSR Compliance")
     (status . "accepted")
     (date . "2025-12-15")
     (context . "*Oblivious computing ecosystem - privacy-preserving computation tools*")
     (decision . "Establish foundational architecture following Rhodium Standard Repository guidelines with multi-platform CI/CD, SHA-pinned actions, and SPDX headers")
     (consequences . ("RSR Gold compliance target"
                      "SHA-pinned GitHub Actions for security"
                      "SPDX license headers on all source files"
                      "Multi-platform CI/CD (GitHub, GitLab, Bitbucket)"
                      "OpenSSF Scorecard compliance")))))

;;;============================================================================
;;; Development Practices
;;; Codified standards for this repository
;;;============================================================================

(define development-practices
  '((code-style
     (languages . ("Scheme"))
     (formatter . "guile-format")
     (linter . "guile-lint")
     (line-length . 100)
     (indent . "spaces")
     (indent-size . 2))

    (security
     (sast . "CodeQL + Semgrep")
     (dependency-scanning . "Dependabot + OSSF Scorecard")
     (credentials . "Environment variables only, never committed")
     (input-validation . "Whitelist + schema validation at boundaries")
     (license-compliance . "AGPL-3.0-or-later"))

    (testing
     (framework . "language-native")
     (coverage-minimum . 70)
     (unit-tests . "Required for business logic")
     (integration-tests . "Required for API boundaries")
     (property-testing . "Where applicable"))

    (versioning
     (scheme . "Semantic Versioning 2.0.0")
     (changelog . "Keep a Changelog format")
     (release-process . "GitHub releases with auto-generated notes"))

    (documentation
     (format . "AsciiDoc preferred, Markdown accepted")
     (api-docs . "Language-native doc comments")
     (adr-location . "META.scm"))

    (branching
     (strategy . "GitHub Flow")
     (main-branch . "main")
     (pr-required . #t))))

;;;============================================================================
;;; Design Rationale
;;; Explains the "why" behind technical choices
;;;============================================================================

(define design-rationale
  '((why-rsr
     "Following Rhodium Standard Repositories (RSR) ensures consistency,
      security, and maintainability across the hyperpolymath ecosystem.
      RSR provides: SHA-pinned actions, SPDX headers, OpenSSF Scorecard
      compliance, and multi-platform CI/CD. This creates a unified
      approach to quality across all repositories.")

    (why-agpl
     "AGPL-3.0-or-later chosen to ensure derivative works remain open
      source while allowing integration with MIT/BSD libraries. The
      copyleft provision protects community contributions and ensures
      improvements flow back to the ecosystem.")

    (why-polyglot
     "Language selection based on domain fit: Rust for performance-critical
      paths, Elixir for concurrent services, Julia for numerical computing,
      ReScript for type-safe frontends, Ada/SPARK for formally verified code.
      Each language is chosen for its strengths in its domain.")))

;;; End of META.scm
;; ============================================================================
;; CROSS-PLATFORM STATUS (Added 2025-12-17)
;; ============================================================================
;; This repo exists on multiple platforms. GitHub is the primary/source of truth.

(cross-platform-status
  (generated "2025-12-17")
  (primary-platform "github")
  (gitlab-mirror
    (path "hyperpolymath/oblibeny")
    (url "https://gitlab.com/hyperpolymath/oblibeny")
    (last-gitlab-activity "2025-10-31")
    (sync-status "gh-primary")
    (notes "GitHub significantly newer (Dec vs Oct). Safe to sync GH→GL."))
  
  (reconciliation-instructions
    ";; To fetch and compare GitLab content:"
    ";; git remote add gitlab https://gitlab.com/hyperpolymath/oblibeny.git"
    ";; git fetch gitlab"
    ";; git log gitlab/main --oneline"
    ";; git diff main gitlab/main"
    ";;"
    ";; To merge if GitLab has unique content:"
    ";; git merge gitlab/main --allow-unrelated-histories"
    ";;"
    ";; After reconciliation, GitHub mirrors to GitLab automatically.")
  
  (action-required "gh-primary"))

