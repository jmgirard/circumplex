# Active milestone

## v2.0.0 release preparation

Source: ROADMAP.md CRAN release strategy. M2–M5 are all GitHub-complete
and accumulate on the v2.0.0 train (target ~2026-08-02, code freeze
~2026-07-26). No new feature milestone is active — M6 (longitudinal) is
deliberately deferred to its own ~v2.1.0 — so the active unit of work is
the release gate itself. M5 (SEM-based SSM) closed 2026-07-08 and is
archived in MILESTONES-ARCHIVE.md with its full log; its milestone-close
`/code-review max` is done (statistics confirmed clean, 9 findings
fixed, 5 deferred to ROADMAP’s M5 follow-up bullets).

**R1. cpm_pack β-boundary fix (Fable-tier; release blocker).** The sole
remaining red on the cross-platform CI matrix (the `ci-cross-platform`
branch / draft PR \#29). **Verified green 2026-07-08: PR \#29 all 7
checks SUCCESS on the real runners (all three ubuntu R-CMD-check jobs +
covr’s test-coverage, the four that carried the cpm_pack red since M4,
plus macOS/Windows/pkgdown).** Linux-only `cpm_pack: all(b_keep > 0)`
error when the CPM optimizer lands a harmonic weight exactly on the β =
0 boundary; the handoff brief is `devel/cpm-pack-boundary-brief.md`. Not
reproducible on the macOS dev machine, so it needs a Linux reproduction
(rocker/r-ver container or a CI debug run) before designing the fix.
Blocks both PR \#29’s green merge and M5’s merge to master (M5 is
stacked on ci-cross-platform). *Accept:* the four `test-cpm_fit.R` tests
pass on the ubuntu R-CMD-check jobs and under `covr`; a
platform-independent β = 0 (vanishing-harmonic) regression test; point
estimates on interior fits byte-identical (parity test);
`/statistical-validation` run.

**R2. Run `/release-checklist` for v2.0.0.** After R1 lands and CI is
green across platforms: bump DESCRIPTION to 2.0.0, rename NEWS.md’s
development heading to 2.0.0, refresh cran-comments.md (test
environments, 0/0/0 check, no revdeps, and the Moss DOI 403 = SAGE
bot-block note from the urlchecker run), run win-builder / R-devel, then
hand `submit_cran()` to Jeff. Do not submit autonomously.

## Log

- 2026-07-08 — R1 VERIFIED + M5 landed on master. PR \#29 (cpm_pack
  fix + CI-portability skips) merged green; PR \#30 (all of M5, rebased
  on \#29) opened, and its first pass through the full CI matrix — which
  the CI-blocked m5-sem-ssm branch had never had — surfaced three real,
  previously-hidden portability defects, each fixed and reproduced under
  reference (netlib) BLAS in a rocker/r-ver container: (a) the three
  `ssm_sem*` exports were missing from `_pkgdown.yml`’s reference index
  (build_reference_index() error); (b) a knife-edge boundary test built
  a population with ρ\*\_1 == 1 exactly, recovered as 1 ± ~1e-7, so
  which sec-4.5 guard fired (point-guard vs draw escalation) flipped by
  platform — rebuilt so ρ\*\_1 ≈ 1.05, robustly over the boundary; (c)
  [`ssm_sem_syntax()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_syntax.md)
  emitted mathematically-zero cos/sin loadings (e.g. cos 90°) as ~1e-16
  libm noise whose low bits differ across platforms’ math libraries,
  breaking the byte-identical emission on Windows — added `snap_trig()`
  to snap exact 0/±1 loadings (cleaner and more correct too; verified
  the snapped syntax still fits). Also hardened the bootstrap-covariance
  advisory test to be deterministic (a se=“standard” fit relabeled to
  “bootstrap”) after lavaan’s small-sample bootstrap vcov recomputation
  threw an internal “model is NULL” on generated-syntax models in the
  full-suite RNG state. PR \#30 merged green across the full matrix
  (macOS/Windows/ubuntu ×3/covr/pkgdown); master is now green on all
  platforms for the first time since M4, clearing the ROADMAP
  CI-portability release blocker. Only R2 (the v2.0.0 release checklist)
  remains before the CRAN submission. (R/cpm_fit.R, R/ssm_sem_syntax.R,
  tests/testthat/test-cpm_fit.R, tests/testthat/test-ssm_sem.R,
  tests/testthat/test-ssm_sem_syntax.R, \_pkgdown.yml, ROADMAP.md,
  MILESTONES.md.)
- 2026-07-08 — R1 (local + container evidence complete; box stays open
  until the ubuntu CI jobs verify it post-push): diagnosed and fixed the
  cpm_pack β-boundary error. Root cause found by container reproduction
  (rocker/r-ver amd64, R 4.6.1): the LS start coefficient for a harmonic
  absent from the population is analytically zero, and the BLAS decides
  its floating-point fate — exact 0.0 under the reference netlib BLAS
  (the ubuntu runners; reproduced: raw LS β₃ = 0.0e0 on the pole
  population), ±1e-16 under OpenBLAS/Accelerate (verified: +1.7e-16).
  The start-value clamp `beta0[beta0 < 0] <- 0.01` missed exact zeros,
  which reached cpm_pack’s softmax inverse
  (`stopifnot(all(b_keep > 0))`) — brief question 2 answered: case (c)
  start values, the ONLY hole (optimizer works in softmax space,
  interior by construction; converged solutions are never re-packed —
  the three production cpm_pack callers are all start sites); question
  3: the stopifnot is the right invariant, fix belongs upstream. Fix:
  extracted `cpm_beta_start_interior()`, flooring surviving exact zeros
  to 0.01 (same treatment as their analytically identical negative
  twins), clamp order chosen so every previously non-crashing input is
  byte-identical (negatives, all-zero fallback, NA fallback preserved
  exactly). Evidence: byte-identical parity on seeded raw-bootstrap +
  cormat + engine fits vs saved pre-fix references; container full suite
  under reference BLAS with CI=true green (0 fail / 101 skip / 1143 pass
  — the runner config); OpenBLAS + macOS suites green (local 1744 pass);
  R CMD check 0/0/0; /statistical-validation 7/7 on both platforms vs a
  hand-written implied-correlation reference (one initial sweep failure
  was the validation script violating cpm_spec’s identification cap —
  invalid reference, corrected); platform-independent regression tests
  pin the exact-zero helper contract and the vanishing-harmonic start
  invariant. Fix committed on ci-cross-platform (924601c, where PR
  \#29’s CI verifies it); m5-sem-ssm rebased onto it, m5-complete
  retagged to the rebased tip. /code-review high: no findings.
  (R/cpm_fit.R, tests/testthat/test-cpm_fit.R, MILESTONES.md.)

# Completed milestones

Archived with their full logs to **MILESTONES-ARCHIVE.md** (M1 → v1.2.0;
M2+M3 → GitHub-complete, bundled into the held v1.3.0; M4 and M4.5 →
GitHub-complete 2026-07-07, M5 → GitHub-complete 2026-07-08; M4–M5 all
fold into v2.0.0). When the active milestone completes, the
milestone-close archive step (or `/release-checklist` at a CRAN release)
moves it there too. This file stays scoped to the active milestone so it
is cheap to re-read at the start of each task.
