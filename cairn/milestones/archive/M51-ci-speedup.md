# M51: Speed up CI — concurrency, paths-ignore, slimmed PR matrix

**Status:** done (2026-07-21, PR #77 https://github.com/jmgirard/circumplex/pull/77)

**Goal:** Cut redundant GitHub Actions runtime on the two check workflows
without reducing what the default branch verifies.

**Outcome:** Config-only edits to `.github/workflows/`. (1) Top-level
`concurrency` block (`group: workflow-ref`, `cancel-in-progress: true`) on
`R-CMD-check.yaml` and `test-coverage.yaml` — superseded runs self-cancel.
(2) `paths-ignore: [cairn/**, man/**, README.md]` on both `pull_request`
triggers (`**/*.Rmd` deliberately excluded so vignette-source changes still
get a full check). (3) `R-CMD-check.yaml` matrix conditional on
`github.event_name`: 1 `ubuntu-latest`/`release` job on PRs, full 5-config
matrix unchanged on push to main/master. No package code, no dependency
change; `.github` is Rbuildignored so the build is untouched.

**Decisions:** none (milestone-local). Follow-up captured as ROADMAP candidate:
per-run heavy-Suggests install cost via `needs: check`.

**Review:** [O] diff-bug lens (Opus, fresh) — no defects; blame-history and
prior-PR-comments lenses inline no-ops (net-new additive config; documented
no-review-thread repo). AC3 confirmed live: PR #77 spawned one R-CMD-check
job. cairn_validate clean.
