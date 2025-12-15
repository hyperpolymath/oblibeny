;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; ECOSYSTEM.scm — oblibeny

(ecosystem
  (version "1.0.0")
  (name "oblibeny")
  (type "project")
  (purpose "*Oblivious computing ecosystem - privacy-preserving computation tools*")

  (position-in-ecosystem
    "Part of hyperpolymath ecosystem. Follows RSR guidelines.")

  (related-projects
    (project (name "rhodium-standard-repositories")
             (url "https://github.com/hyperpolymath/rhodium-standard-repositories")
             (relationship "standard")))

  (what-this-is "*Oblivious computing ecosystem - privacy-preserving computation tools*")
  (what-this-is-not "- NOT exempt from RSR compliance"))
