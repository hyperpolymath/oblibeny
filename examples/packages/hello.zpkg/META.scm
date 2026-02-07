;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

;; ============================================================================
;; Hello Package Metadata
;; ============================================================================

(define package
  '((name . "hello")
    (version . "1.0.0")
    (author . "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>")
    (license . "PMPL-1.0-or-later")
    (description . "Proof of concept package for Oblibeny distribution")

    (architecture
      ((supported . ("x86_64" "aarch64" "riscv64"))
       (cross-compiled . #t)))

    (dependencies
      ())  ;; No dependencies for hello

    (files
      ((binary . "/usr/bin/hello")
       (size . 16384)  ;; 16KB
       (permissions . #o755)))

    (verification
      ((abi-proof . "src/abi/packages/hello/Interface.idr")
       (reversibility-proven . #t)
       (termination-proven . #t)))

    (build
      ((language . "zig")
       (source . "examples/hello.zig")
       (build-command . "zig build-exe hello.zig -O ReleaseSafe")
       (reproducible . #t)))

    (installation
      ((script . "install.obl")
       (constrained-form . #t)
       (bounded-loops . 0)  ;; No loops in installation
       (max-operations . 10)))  ;; Maximum 10 operations

    (security
      ((signature . "signatures/package.sig")
       (signature-algorithm . "ed25519")
       (hash-algorithm . "sha256")))))
