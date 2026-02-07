;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm - Project state for oblibeny
;; Media-Type: application/vnd.state+scm

(state
  (metadata
    (version "0.0.1")
    (schema-version "1.0")
    (created "2026-01-03")
    (updated "2026-02-07")
    (project "oblibeny")
    (repo "github.com/hyperpolymath/oblibeny"))

  (project-context
    (name "oblibeny")
    (tagline "Secure edge language for reversibility and accountability")
    (tech-stack (OCaml Zig Idris2))
    (implementation-language "OCaml + Zig")
    (target-domains (security-critical edge-computing cryptography))
    (related-repos
      (obli-transpiler-framework "Transpiler to other languages (11 Rust files)")
      (obli-fs "Filesystem features (WASM)")
      (obli-riscv-dev-kit "RISC-V development tooling")
      (obli-ssg "Static site generator")
      (nextgen-languages/oblibeny "OUTDATED - embedded snapshot, 0 OCaml files")))

  (current-position
    (phase "lsp-complete-build-blocked")
    (overall-completion 60)
    (loc 3376)
    (files 53)
    (ocaml-files 49)
    (zig-files 3)
    (idris-files 1)
    (size "57M")
    (components
      ((lexer (status complete) (file "lib/lexer.ml"))
       (parser (status complete) (file "lib/parse.ml"))
       (typechecker (status complete) (file "lib/typecheck.ml"))
       (evaluator (status complete) (file "lib/eval.ml"))
       (constrained-checker (status complete) (file "lib/constrained_check.ml"))
       (ast (status complete) (file "lib/ast.ml"))
       (crypto-ffi (status complete) (completion 100) (loc 1110)
                   (files ("ffi/zig/src/crypto.zig" "ffi/zig/src/obli-pkg.zig"
                           "src/abi/Crypto.idr" "ffi/zig/build.zig" "Containerfile.crypto"))
                   (description "Post-quantum crypto with liboqs + libsodium"))
       (package-manager (status complete) (completion 100) (implementation "obli-pkg") (language "Zig") (opsm-integrated true) (description "Triple post-quantum signature verification (Dilithium5 + SPHINCS+ + Ed25519)"))
       (lsp-server (status complete-blocked) (completion 100) (loc 789)
                   (files ("lib/lsp_protocol.ml" "lib/lsp_diagnostics.ml" "lib/lsp_hover.ml"
                           "lib/lsp_completion.ml" "bin/oblibeny_lsp.ml"))
                   (features (diagnostics hover completion document-sync initialization shutdown))
                   (blocked "Build requires zstd library (system dependency)"))
       (debugger (status missing))
       (vscode-extension (status missing))
       (documentation (status minimal) (description "BUILD-LOG.md + CRYPTO-FFI-COMPLETE.md + LSP-SERVER-IMPLEMENTATION.md"))
       (deployment (status complete) (description "Containerfile.crypto with multi-stage build"))))
    (working-features
      (lexing "Full lexical analysis with token generation")
      (parsing "Complete parser for Oblibeny syntax")
      (type-checking "Type system with constraint checking")
      (evaluation "Interpreter for core language")
      (constrained-semantics "Constrained computation verification")
      (crypto-integration "liboqs, libsodium, argon2 via Zig FFI")
      (examples "10+ example programs (.obl files)")))

  (route-to-mvp
    (milestones
      ((consolidate-scattered-code
        (priority critical)
        (effort "5-8 hours")
        (description "Bring obli-transpiler-framework and examples from boinc-boinc into main repo"))
       (complete-zig-ffi
        (priority critical)
        (effort "10-15 hours")
        (description "Complete crypto stack integration with liboqs"))
       (complete-package-manager
        (priority high)
        (effort "15-20 hours")
        (description "Finish obli-pkg - dependency resolution and registry client"))
       (add-lsp-server
        (priority high)
        (effort "20-25 hours")
        (description "LSP for editor integration"))
       (write-documentation
        (priority high)
        (effort "12-18 hours")
        (description "Language spec, tutorials, API docs"))
       (consolidate-deployment
        (priority medium)
        (effort "5-8 hours")
        (description "Single Dockerfile instead of 4+ separate ones"))))

  (blockers-and-issues
    (critical
      (code-fragmentation
        (severity critical)
        (impact "Development scattered across 6 repos")
        (description "Examples in boinc-boinc/examples/, transpiler separate, toolchain repos scattered")
        (affected-repos
          "boinc-boinc/examples/crypto-xor.obl"
          "security-audit/repos/oblibeny-playground/"
          "obli-transpiler-framework/"
          "obli-fs/"))
      (crypto-integration-incomplete
        (severity high)
        (impact "Cannot verify security guarantees without complete crypto stack")
        (description "Zig FFI has 3 files but crypto integration incomplete")))
    (high
      (no-package-manager
        (severity medium)
        (impact "Cannot manage dependencies")
        (description "obli-pkg started but incomplete"))
      (no-editor-integration
        (severity medium)
        (impact "Poor developer experience")
        (description "Missing LSP server")))
    (medium
      (documentation-minimal
        (severity low)
        (impact "Difficult for new users to understand")
        (description "Only BUILD-LOG.md, needs comprehensive docs")))
    (low))

  (critical-next-actions
    (immediate
      (consolidate-code
        (description "Move obli-transpiler-framework to transpiler/ subdirectory")
        (description "Move examples from boinc-boinc to examples/")
        (effort "5-8 hours"))
      (complete-crypto-ffi
        (description "Finish Zig FFI bindings for liboqs/libsodium")
        (files ("ffi/zig/"))
        (effort "10-15 hours")))
    (this-week
      (idris2-abi-proofs
        (description "Add Idris2 ABI proofs for FFI safety")
        (effort "8-12 hours"))
      (begin-lsp
        (description "Start LSP server implementation")
        (effort "20-25 hours")))
    (this-month
      (complete-package-manager
        (description "Finish obli-pkg with registry client")
        (effort "15-20 hours"))
      (write-documentation
        (description "Language spec, security model, tutorials")
        (effort "12-18 hours"))))

  (session-history
    ((2026-02-07
      (focus "Verified actual implementation status")
      (discoveries
        "STATE.scm claimed 20% but reality is 45% - 44 OCaml files, 1,677 LOC, full compiler"
        "Code scattered across 6 repos: obli-transpiler-framework, obli-fs, obli-riscv-dev-kit, obli-ssg, boinc-boinc examples")
      (actions
        "Updated STATE.scm to reflect reality"
        "Identified consolidation plan")))))
