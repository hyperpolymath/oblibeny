;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm - Current project state

(define state
  '((metadata
     (version "1.0")
     (schema-version "1.0")
     (created "2026-02-04")
     (updated "2026-02-04")
     (project "oblibeny")
     (repo "hyperpolymath/oblibeny"))

    (project-context
     (name "oblibeny")
     (tagline "Secure edge language + minimal security-first Linux distribution")
     (tech-stack (
       ("idris2" "ABI definitions, formal proofs, interface contracts")
       ("zig" "FFI implementation, system layer, kernel interfaces")
       ("oblibeny" "Package operations in constrained form")
       ("ocaml" "Language implementation (compiler/runtime)")
       ("coq" "Formal verification of crypto primitives")))
     (security-spec (
       ("post-quantum" "Dilithium5-AES, Kyber-1024, SPHINCS+")
       ("classical-compat" "Ed448 (hybrid with PQ)")
       ("symmetric" "XChaCha20-Poly1305 (256-bit)")
       ("hashing" "SHAKE3-512, BLAKE3")
       ("password-hash" "Argon2id (512 MiB, 8 iter, 4 lanes)")
       ("protocols" "IPv6-only, HTTP/3+QUIC, TLS 1.3")
       ("forbidden" "IPv4, HTTP/1.1, SHA-1, MD5, Ed25519"))))

    (current-position
     (phase "minimal-viable-product")
     (overall-completion 50)
     (components (
       ("oblibeny-language" "OCaml implementation" 40)
       ("distribution-design" "Architecture planning & naming" 100)
       ("crypto-stack" "libsodium, liboqs, argon2" 100)
       ("hello-binary" "Zig proof-of-concept (29KB)" 100)
       ("idris2-abi-layer" "Not started" 0)
       ("zig-package-manager" "obli-pkg design" 10)))
     (working-features (
       "Constrained form parser"
       "Factory form evaluator"
       "Basic reversibility primitives"
       "Static crypto libraries (506KB + 11MB + 42KB)"
       "Ice formation metaphor (Floe/Iceberg/Glacier)")))

    (route-to-mvp
     (milestones
      ((milestone-id "m1")
       (name "Distribution Architecture")
       (completion 10)
       (items (
         "Define three-layer stack (Idris2/Zig/Oblibeny)"
         "Design minimal package format"
         "Identify core system components"
         "Document maintenance strategy")))

      ((milestone-id "m2")
       (name "Idris2 ABI Layer")
       (completion 0)
       (items (
         "Package interface definitions"
         "System invariant proofs"
         "Memory layout verification"
         "ABI compatibility proofs"
         "Generate C headers from Idris2")))

      ((milestone-id "m3")
       (name "Zig FFI Implementation")
       (completion 0)
       (items (
         "Package extraction/installation"
         "Filesystem operations"
         "Cryptographic primitives (signing/verification)"
         "Kernel interfaces"
         "Integration with Idris2 headers")))

      ((milestone-id "m4")
       (name "Oblibeny Package Manager")
       (completion 0)
       (items (
         "Package install in constrained form"
         "Dependency resolution (bounded)"
         "Reversible operations"
         "Accountability traces"
         "Automatic rollback on failure")))

      ((milestone-id "m5")
       (name "Distroless Bottom-Up Bootstrap")
       (completion 5)
       (items (
         "Start with Google distroless (~20 files)"
         "Build static binaries on Alpine (scaffolding)"
         "Add only proven-necessary components"
         "Verify < 50 files total"
         "Self-hosting build (optional stretch goal)")))))

    (blockers-and-issues
     (critical (
       "Idris2 code generation for C headers needs validation"))
     (high (
       "Package format specification needed"
       "Distroless base image customization"))
     (medium (
       "Init system needed? (Single process vs init)"
       "musl vs Zig libc decision"
       "Security update automation design"))
     (low (
       "Alpine vs from-scratch (RESOLVED: Use distroless bottom-up)")))

    (critical-next-actions
     (immediate (
       "Create Dockerfile.lago-grey-minimal for first complete image"
       "Assemble: distroless + hello + obli-pkg + crypto libs"
       "Verify total image size < 18 MB"
       "Test running binaries in distroless container"
       "Optimize liboqs: minimal build with only Kyber-1024, Dilithium5, SPHINCS+"))
     (this-week (
       "Create first signed .zpkg package with hello binary"
       "Implement signature verification in obli-pkg (link with liboqs, libsodium)"
       "Create Idris2 ABI proofs for package installation"))
     (this-month (
       "Prove installation reversibility in Idris2"
       "Document: what requires proof to add"
       "Build accountability trace database"
       "Create first reversible package installation")))))
