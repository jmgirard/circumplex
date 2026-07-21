<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M51: Speed up CI — concurrency cancel, paths-ignore, slimmed PR matrix

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m51-ci-speedup · https://github.com/jmgirard/circumplex/pull/77

## Goal

Cut redundant GitHub Actions runtime on the two check workflows without
reducing what the default branch verifies.

## Scope

**In:** three config-only edits to `.github/workflows/`:
1. A top-level `concurrency` block (`group: ${{ github.workflow }}-${{ github.ref }}`,
   `cancel-in-progress: true`) on `R-CMD-check.yaml` and `test-coverage.yaml`,
   so a superseding push cancels the in-flight run.
2. `paths-ignore: ['cairn/**', 'man/**', 'README.md']` on the `pull_request`
   trigger of both workflows, so tracking-only / generated-doc commits don't
   re-trigger the matrix. (`**/*.Rmd` deliberately excluded — see Decisions.)
3. `R-CMD-check.yaml` matrix conditional on `github.event_name`: a single
   `{os: ubuntu-latest, r: release}` on `pull_request`; the full 5-config
   matrix unchanged on push to `master`.

**Out:** `pkgdown.yaml` and `pr-commands.yaml` (not in scope — user-scoped to
the two check workflows; `pr-commands` triggers on `issue_comment`, not the
push/PR matrix). Trimming the `brms`/`glmmTMB`/`rstan` install cost by moving
Suggests off `needs: check` → candidate (dependency-surface change, deferred).
No package code, no dependency add/remove/re-pin.

## Acceptance criteria

- [x] AC1 `R-CMD-check.yaml` and `test-coverage.yaml` each carry a top-level
      `concurrency` block with `group: ${{ github.workflow }}-${{ github.ref }}`
      and `cancel-in-progress: true`. Evidence: grep both files.
- [x] AC2 Both workflows' `pull_request:` trigger carries
      `paths-ignore: ['cairn/**', 'man/**', 'README.md']` (and `**/*.Rmd` is
      absent). Evidence: grep both files.
- [x] AC3 `R-CMD-check.yaml`'s `matrix.config` resolves to exactly one config
      (`ubuntu-latest`, `release`) when `github.event_name == 'pull_request'`
      and to the original 5-config list otherwise. Evidence: the conditional
      expression in the file, plus this milestone's own PR showing a single
      `R-CMD-check` job while a full run appears on the post-merge master push.
- [x] AC4 All four `.github/workflows/*.yaml` parse as valid YAML and the diff
      changes only the concurrency / trigger / matrix lines (every existing
      step, `permissions`, and env unchanged). Evidence: `yaml::read_yaml()`
      on each file clean + scoped `git diff`.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1
- AC4 → T3

## Tasks

- [x] T1 `R-CMD-check.yaml`: add `paths-ignore` to the `pull_request` trigger,
      add the top-level `concurrency` block, and replace the static
      `matrix.config` with the `github.event_name`-conditional `fromJSON`
      expression (PR → 1 config; else the current 5).
- [x] T2 `test-coverage.yaml`: add the same `paths-ignore` to its
      `pull_request` trigger and the same top-level `concurrency` block.
- [x] T3 Validate: `yaml::read_yaml()` each of the four workflow files clean;
      confirm `git diff` is scoped to the intended lines only; open the PR.

## Work log

- 2026-07-21: created by /milestone-plan.
- 2026-07-21: T1 — R-CMD-check.yaml gets concurrency (cancel-in-progress:true), paths-ignore [cairn/**,man/**,README.md] on pull_request, and github.event_name-conditional matrix (PR=1 ubuntu/release; else full 5). yaml::read_yaml() parses clean; folded matrix scalar verified.
- 2026-07-21: T2 — test-coverage.yaml gets the same concurrency block and paths-ignore; push trigger and job body unchanged. yaml::read_yaml() clean.
- 2026-07-21: T3 — all four workflows parse; diff scoped to concurrency/paths-ignore/matrix only (pkgdown/pr-commands untouched). Branch pushed; PR #77 opened. Status → review.

## Decisions

## Review

_2026-07-21 (/milestone-review). Branch `m51-ci-speedup` cut from master 241eb107; master == origin/master (not stale)._

**Acceptance criteria — fresh evidence:**
- AC1 ✓ `concurrency` block (`group: ${{ github.workflow }}-${{ github.ref }}`, `cancel-in-progress: true`) present in both `R-CMD-check.yaml:17-19` and `test-coverage.yaml:17-19` (grep).
- AC2 ✓ `paths-ignore: [cairn/**, man/**, README.md]` under the `pull_request` trigger of both workflows (`:8-11`); `Rmd` grep count = 0 in both (the deliberately-dropped `**/*.Rmd`).
- AC3 ✓ Conditional `matrix.config` expression present (`R-CMD-check.yaml:32`); parsed folded scalar resolves to 1 config on PR / the original 5 otherwise. **Live confirmation:** PR #77 CI spawned exactly one R-CMD-check job, `ubuntu-latest (release)` — no macOS/Windows/devel/oldrel. Post-merge full matrix is the `|| fromJSON(5-config)` branch (verified by parse + reviewer), manifesting on the merge push.
- AC4 ✓ All four `.github/workflows/*.yaml` parse under `yaml::read_yaml()`; branch diff scoped to only the 2 workflows + ROADMAP + milestone file; every existing step/`permissions`/env unchanged.

**Consistency gate:**
- Universal: `cairn_validate` exit 0 — all checks PASS (incl. `coverage complete`, `mirror agreement`, `at most one in-progress`, `weight caps`). 47 WARNs are M7's pre-existing work-log-format advisories, untouched. No IP/GP change → `cairn_impact` skipped.
- Toolchain (r-package `consistency-gate`): `.github` is `.Rbuildignore`d (`^\.github$`), so the built package tarball is byte-identical to master — `document()` no-diff, generated-files, pkgdown, and README-knit checks are provably unaffected (no R/roxygen/data in the diff). No user-visible change → no NEWS entry owed. Authoritative full `R CMD check` = PR #77's own R-CMD-check CI, required green before merge.

**Fresh-context review (scoped):** 2-file Rbuildignored YAML diff. Ran the substantive **[O] diff-bug lens** (Opus, fresh context) — **no defects**; independently verified the folded conditional-matrix scalar, the concurrency group (distinct refs → no cross-cancellation of push vs PR runs), paths-ignore placement, and that PR-config `http-user-agent` resolves empty exactly as 4/5 original configs already did. **Blame-history lens** handled inline: every changed line is net-new additive config; the full 5-config matrix is preserved on push, so no prior deliberate work is undone. **Prior-PR-comments lens** handled inline: documented permanent no-op in this repo (LESSONS M33; no archived `## Review` touches `.github/`). Zero findings → nothing to score or triage.
