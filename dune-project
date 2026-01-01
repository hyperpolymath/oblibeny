; SPDX-License-Identifier: MIT OR Palimpsest-0.8
; Copyright (c) 2026 Hyperpolymath

(lang dune 3.0)
(name oblibeny)
(version 0.1.0)

(generate_opam_files true)

(source (github hyperpolymath/oblibeny))
(license "MIT OR Palimpsest-0.8")
(authors "Hyperpolymath")
(maintainers "Hyperpolymath")

(package
 (name oblibeny)
 (synopsis "Oblíbený - Secure edge language for reversibility and accountability")
 (description
  "Oblíbený is a dual-form language: a Turing-complete meta/factory form
that produces a Turing-incomplete reversible/constraint form.
Designed for accountability and integration with MAA/Absolute-Zero.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  menhir
  sedlex
  yojson
  ppx_deriving
  ppx_deriving_yojson
  (alcotest :with-test)))
