;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm - Project ecosystem position

(ecosystem
 (version "1.0")
 (name "oblibeny")
 (type "language+distribution")
 (purpose "Secure edge language with dual-form design (Turing-complete factory → Turing-incomplete constrained runtime) + minimal security-first Linux distribution")

 (position-in-ecosystem
  "Oblibeny provides:
   1. A secure language with reversibility and accountability
   2. Foundation for a maintainable, community-driven Linux distribution
   3. Integration point for MAA (Mutually-Assured-Accountability)
   4. Alternative to corporate-controlled distributions (Chainguard/Alpine)")

 (related-projects
  ((rsr-template-repo sibling-standard "Repository structure and workflows")
   (hypatia potential-consumer "Neurosymbolic CI/CD scanning")
   (gitbot-fleet potential-consumer "Automated maintenance and fixes")
   (maa-protocol integration-point "Accountability trace commitment")
   (absolute-zero integration-point "Privacy-preserving proofs over traces")))

 (what-this-is
  "• A dual-form language (factory/constrained) with formal guarantees
   • A minimal Linux distribution using Idris2 (ABI) + Zig (FFI) + Oblibeny (coordination)
   • Reversible package operations with accountability traces
   • Community-maintainable without paid staff (heavy automation)")

 (what-this-is-not
  "• NOT a general-purpose Turing-complete language (runtime is intentionally limited)
   • NOT a comprehensive package ecosystem (curated, not everything)
   • NOT trying to replace Guix/NixOS (different approach: simpler, provably safe)
   • NOT production-ready yet (design phase)"))
