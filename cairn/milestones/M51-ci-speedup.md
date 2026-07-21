<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M51: Speed up CI — concurrency cancel, paths-ignore, slimmed PR matrix

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

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

- [ ] AC1 `R-CMD-check.yaml` and `test-coverage.yaml` each carry a top-level
      `concurrency` block with `group: ${{ github.workflow }}-${{ github.ref }}`
      and `cancel-in-progress: true`. Evidence: grep both files.
- [ ] AC2 Both workflows' `pull_request:` trigger carries
      `paths-ignore: ['cairn/**', 'man/**', 'README.md']` (and `**/*.Rmd` is
      absent). Evidence: grep both files.
- [ ] AC3 `R-CMD-check.yaml`'s `matrix.config` resolves to exactly one config
      (`ubuntu-latest`, `release`) when `github.event_name == 'pull_request'`
      and to the original 5-config list otherwise. Evidence: the conditional
      expression in the file, plus this milestone's own PR showing a single
      `R-CMD-check` job while a full run appears on the post-merge master push.
- [ ] AC4 All four `.github/workflows/*.yaml` parse as valid YAML and the diff
      changes only the concurrency / trigger / matrix lines (every existing
      step, `permissions`, and env unchanged). Evidence: `yaml::read_yaml()`
      on each file clean + scoped `git diff`.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1
- AC4 → T3

## Tasks

- [ ] T1 `R-CMD-check.yaml`: add `paths-ignore` to the `pull_request` trigger,
      add the top-level `concurrency` block, and replace the static
      `matrix.config` with the `github.event_name`-conditional `fromJSON`
      expression (PR → 1 config; else the current 5).
- [ ] T2 `test-coverage.yaml`: add the same `paths-ignore` to its
      `pull_request` trigger and the same top-level `concurrency` block.
- [ ] T3 Validate: `yaml::read_yaml()` each of the four workflow files clean;
      confirm `git diff` is scoped to the intended lines only; open the PR.

## Work log

- 2026-07-21: created by /milestone-plan.

## Decisions

## Review
