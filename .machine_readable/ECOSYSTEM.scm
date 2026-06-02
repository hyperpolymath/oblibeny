;; SPDX-License-Identifier: MPL-2.0
;; ECOSYSTEM.scm - Ecosystem position for oblibeny
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0")
  (name "oblibeny")
  (type "edge-language / constrained-form compiler")
  (purpose "Turing-incomplete constrained form for reversible, accountable computation on secure edge devices")

  (position-in-ecosystem
    (category "programming-language")
    (subcategory "edge / safety-critical / formally-verified")
    (unique-value
      ("Dual-form: factory (Turing-complete, compile-time) + constrained (Turing-incomplete, runtime)"
       "First-class echo type: structured, proof-relevant residue of irreversible collapse"
       "Full reversibility: every constrained-form operation has a defined inverse"
       "Accountability trace: every operation produces an immutable audit trail"
       "Static resource bounds: termination, memory, call depth all computable at compile time"))
    (target-deployment
      ("Hardware Security Modules (HSMs)"
       "Secure enclaves (SGX, TrustZone)"
       "Smart cards / tamper-resistant hardware"
       "IoT / IIoT edge nodes with accountability requirements"
       "Safety-critical embedded systems")))

  (related-projects
    ((echo-types
       (repo "github.com/hyperpolymath/echo-types")
       (language "Agda")
       (relationship "type-theory-source")
       (description "Constructive Agda formalisation of echo types: loss that is not total erasure (Echo f y := Sigma (x : A), f x == y). Oblibeny's echo[A, B] type is the constrained-form realisation of this fibre/residue.")
       (interface-doc "docs/specs/echo-6a2.adoc")
       (spec-section "SPEC §6.a.2"))
     (EchoTypes.jl
       (repo "github.com/hyperpolymath/EchoTypes.jl")
       (language "Julia")
       (relationship "executable-model")
       (description "Finite-domain executable companion to echo-types (a runnable model, not a proof). Oblibeny's echo_visible / echo_witness projections follow this finite-domain reading.")
       (interface-doc "docs/specs/echo-6a2.adoc")
       (spec-section "SPEC §6.a.2"))
     (svalinn
       (repo "github.com/hyperpolymath/svalinn")
       (language "Rust")
       (relationship "deployment-runtime")
       (description "Edge gateway for verified containers; Oblibeny programs deploy via the Svalinn/Vordr verified container stack."))
     (maa-protocol
       (repo "external: maa-protocol/SPEC.scm")
       (language "Specification")
       (relationship "external-interface")
       (description "Mutually-Assured-Accountability protocol; Oblibeny traces can be committed to the MAA ledger for external verification.")
       (spec-section "SPEC §6.a.1"))
     (absolute-zero
       (repo "external: absolute-zero/SPEC.scm")
       (language "Specification")
       (relationship "external-interface")
       (description "Zero-knowledge proof system for Oblibeny traces; supports proving properties of traces without revealing content.")
       (spec-section "SPEC §6.a.3"))))

  (what-this-is
    ("A programming language where the runtime form (constrained form) is Turing-incomplete by design"
     "A compiler from constrained-form source (.obl files) to an evaluated trace-producing runtime"
     "A factory language: the factory form generates constrained-form programs at compile time"
     "A type system with first-class echo types (structured irreversible collapse residues)"
     "An accountability framework: every runtime operation produces an immutable audit trail"
     "A foundation for formally-verified edge software"))

  (what-this-is-not
    ("A general-purpose programming language"
     "A systems programming language (use Rust for systems)"
     "A proof assistant (use Agda/Coq/Lean for proofs; echo-types is the proof companion)"
     "A scripting language"
     "A language for unbounded computation"
     "A JavaScript/TypeScript/Go/Python alternative (see CLAUDE.md language policy)")))
