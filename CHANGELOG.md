<!--
SPDX-License-Identifier: MPL-2.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
-->

# Changelog

All notable changes to `oblibeny` will be documented in this file.

This file is generated from conventional commits by the
[`changelog-reusable.yml`](https://github.com/hyperpolymath/standards/blob/main/.github/workflows/changelog-reusable.yml)
workflow (`hyperpolymath/standards#206`). Adopt the workflow in this repo's CI to keep this file in sync automatically — see
[`templates/cliff.toml`](https://github.com/hyperpolymath/standards/blob/main/templates/cliff.toml)
for the canonical config.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
this project aims to follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- feat: integrate Oblibeny with Svalinn/Vordr verified container stack
- feat: drive Oblibeny to 100% production-ready completion
- feat: add comprehensive static analyzer with resource bounds
- feat: complete oblibeny crypto FFI (90% → 100%)
- feat: implement complete crypto FFI with liboqs and libsodium
- feat: consolidate toolchain and examples into main repo
- feat: complete first Lago Grey minimal image (14.6 MB) + ecosystem integration
- feat(ci): enable Hypatia scanning

### Fixed

- fix(ci): sync hypatia-scan.yml to canonical (kill cd-scanner build drift) (#34)
- fix(ci): build Hypatia escript from repo root (estate dogfood drift)
- fix(ci): bump actions/checkout v4.1.1→v6.0.2 in workflow-linter.yml (Node.js 24 compat)
- fix(ci): bump actions/checkout v4.1.1→v6.0.2 in codeql.yml (Node.js 24 compat)
- fix: remove duplicate SCM files from root
- fix(security): update editorconfig SHA and CodeQL language
- fix: OpenSSF Scorecard compliance (permissions, SHA-pins, SAST)

### Documentation

- docs: update README and STATE.scm to reflect 100% completion
- docs: update SCM files with project information
- docs: add CONTRIBUTING.md
- docs: add SCM checkpoint files
- docs: add checkpoint files for state tracking

### CI

- ci: bump actions/upload-artifact SHA to current v4 (#30)
- ci(secret-scanner): drop duplicate --fail from trufflehog extra_args (#29)
- ci: fix workflow-linter YAML parse error + self-flag bug

## Pre-history

Prior commits to this file's introduction are recorded in git history but not formally classified into Keep-a-Changelog sections. To backfill, run `git cliff -o CHANGELOG.md` locally using the canonical [`cliff.toml`](https://github.com/hyperpolymath/standards/blob/main/templates/cliff.toml) — this is one-shot mechanical work.

---

<!-- This file was seeded by the 2026-05-26 estate tech-debt audit follow-up (Row-2 Phase 3); see [`hyperpolymath/standards/docs/audits/2026-05-26-estate-documentation-debt.md`](https://github.com/hyperpolymath/standards/blob/main/docs/audits/2026-05-26-estate-documentation-debt.md). -->
