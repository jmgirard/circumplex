# Active milestone

## v2.0.0 release preparation

Source: ROADMAP.md CRAN release strategy. M2–M5 are all GitHub-complete and
accumulate on the v2.0.0 train (target ~2026-08-02, code freeze ~2026-07-26).
No new feature milestone is active — M6 (longitudinal) is deliberately deferred
to its own ~v2.1.0 — so the active unit of work is the release gate itself.
M5 (SEM-based SSM) closed 2026-07-08 and is archived in MILESTONES-ARCHIVE.md
with its full log; its milestone-close `/code-review max` is done (statistics
confirmed clean, 9 findings fixed, 5 deferred to ROADMAP's M5 follow-up
bullets).

- [ ] **R1. cpm_pack β-boundary fix (Fable-tier; release blocker).** The sole
  remaining red on the cross-platform CI matrix (the `ci-cross-platform`
  branch / draft PR #29). Linux-only `cpm_pack: all(b_keep > 0)` error when the
  CPM optimizer lands a harmonic weight exactly on the β = 0 boundary; the
  handoff brief is `devel/cpm-pack-boundary-brief.md`. Not reproducible on the
  macOS dev machine, so it needs a Linux reproduction (rocker/r-ver container
  or a CI debug run) before designing the fix. Blocks both PR #29's green merge
  and M5's merge to master (M5 is stacked on ci-cross-platform).
  *Accept:* the four `test-cpm_fit.R` tests pass on the ubuntu R-CMD-check jobs
  and under `covr`; a platform-independent β = 0 (vanishing-harmonic)
  regression test; point estimates on interior fits byte-identical (parity
  test); `/statistical-validation` run.
- [ ] **R2. Run `/release-checklist` for v2.0.0.** After R1 lands and CI is
  green across platforms: bump DESCRIPTION to 2.0.0, rename NEWS.md's
  development heading to 2.0.0, refresh cran-comments.md (test environments,
  0/0/0 check, no revdeps, and the Moss DOI 403 = SAGE bot-block note from the
  urlchecker run), run win-builder / R-devel, then hand `submit_cran()` to
  Jeff. Do not submit autonomously.

## Log

- (fresh log for the release-preparation unit; append one line per task on
  completion, per the workflow loop)

# Completed milestones

Archived with their full logs to **MILESTONES-ARCHIVE.md** (M1 → v1.2.0;
M2+M3 → GitHub-complete, bundled into the held v1.3.0; M4 and M4.5 →
GitHub-complete 2026-07-07, M5 → GitHub-complete 2026-07-08; M4–M5 all fold
into v2.0.0). When the active milestone
completes, the milestone-close archive step (or `/release-checklist` at a CRAN
release) moves it there too. This file stays scoped to the active milestone so
it is cheap to re-read at the start of each task.
