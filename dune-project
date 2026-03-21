(lang dune 3.0)
; SPDX-License-Identifier: PMPL-1.0-or-later
; Copyright (c) 2026 Jonathan D.A. Jewell

(using menhir 2.1)

(name oblibeny)
(version 0.1.0)

(generate_opam_files true)

(source (github hyperpolymath/oblibeny))
(license "PMPL-1.0-or-later")
(authors "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>")
(maintainers "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>")

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
