;; SPDX-License-Identifier: MPL-2.0
;; ECOSYSTEM.scm - Ecosystem position for oblibeny
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0")
  (name "oblibeny")
  (type "")
  (purpose "")

  (position-in-ecosystem
    (category "")
    (subcategory "")
    (unique-value ()))

  (related-projects
    ((echo-types
       (repo "github.com/hyperpolymath/echo-types")
       (language "Agda")
       (relationship "type-theory-source")
       (description "Constructive Agda formalisation of echo types: loss that is not total erasure (Echo f y := Sigma (x : A), f x == y). Oblibeny's echo[A, B] type is the constrained-form realisation of this fibre/residue."))
     (EchoTypes.jl
       (repo "github.com/hyperpolymath/EchoTypes.jl")
       (language "Julia")
       (relationship "executable-model")
       (description "Finite-domain executable companion to echo-types (a runnable model, not a proof). Oblibeny's echo_visible / echo_witness projections follow this finite-domain reading."))))

  (what-this-is ())

  (what-this-is-not ()))
