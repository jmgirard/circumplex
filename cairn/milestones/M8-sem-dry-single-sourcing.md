<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M8: SEM-layer DRY single-sourcing

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Branch/PR:** m8-sem-dry-single-sourcing / https://github.com/jmgirard/circumplex/pull/32

## Goal

Single-source the SEM layer's duplicated contrast-arity, fit, label, and
strict-tier-syntax logic, changing no exported behavior.

## Scope

**In:** Behaviour-preserving refactors from the M5 close-review (legacy ROADMAP
"Milestone 5" items c, d, e, g):
- (c) one shared internal contrast-arity validator replacing the near-verbatim
  group/measure arity checks and duplicated error strings at
  `R/ssm_sem.R:1099-1103`, `:1134-1150`, `:1366-1388`.
- (d) one internal fit chokepoint owning the `ssm_sem_syntax()` → `lavaan::cfa()`
  translation (estimator / se / `missing` → ml|listwise) and `group.label`
  protection, replacing the two sites at `R/ssm_sem.R:689-704` and `:1170-1178`.
- (e) `summary.circumplex_ssm_sem()` (`R/ssm_sem.R:1582-1600`) delegating its
  replicate/missing/detail lines through one shared label seam.
- (g) strict-tier syntax single-sourced across the single/multi-group branches
  (`R/ssm_sem_syntax.R` + `R/ssm_sem.R` "metric rung is vacuous" duplication),
  plus micro-cleanups: unused `npar` struct field, `sem_details()`'s
  always-overwritten `score_type`, test-fixture consolidation.

**Out:** `sem_estimate()` vectorization + oracle rename → M9. Package-wide
scalar-count validator → M10. No new exports; no user-visible behaviour change.

## Acceptance criteria

- [x] Contrast-arity validation lives in one internal validator called by all
      three former sites; each `cli_abort()` arity branch still fires with an
      unchanged message (a test exercises every arity-failure branch:
      wrong #groups, wrong #measures, single- and user-fit paths).
- [x] The two `lavaan::cfa()` fit sites route through one internal fit helper
      (estimator/se/missing translation + `group.label`); existing SEM fits are
      byte-identical (current `ssm_sem()` tests + snapshots pass unchanged).
- [x] `summary.circumplex_ssm_sem()` output is snapshot-identical, with detail
      lines produced by the shared label seam.
- [x] The strict-tier vacuous-metric rule (`model=="strict" && rung=="metric"`)
      is single-sourced across the three ladder sites via one predicate; the
      pinned strings `exp_strict_nomeas` / `exp_strict_meas`
      (`tests/testthat/test-ssm_sem_syntax.R:242,244`) remain byte-identical.
      (Amended 2026-07-12 via gate: cross-branch *emission* unification
      descoped as not-worth-the-snapshot-risk → candidate row; see work log.)
- [x] Micro-cleanups landed (`npar` field removed, `score_type` overwrite
      removed) with no behaviour change; suite green.
- [x] `devtools::check()` clean (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6

## Tasks

- [x] **T1** — Extract shared contrast-arity validator; route the three sites
      (`R/ssm_sem.R:1099-1103,1134-1150,1366-1388`) through it; keep messages.
- [x] **T2** — Extract internal fit chokepoint for the two `lavaan::cfa()`
      sites (`R/ssm_sem.R:689-704,1170-1178`); assert identical fits.
- [x] **T3** — Route `summary.circumplex_ssm_sem()` detail lines
      (`R/ssm_sem.R:1583-1600`) through a shared label seam; snapshot unchanged.
- [x] **T4** — Single-source the strict-tier vacuous-metric rule via
      `sem_strict_metric_vacuous()` (ladder sites `ssm_sem.R:754,829,882`);
      `exp_strict_*` snapshots byte-identical. (Emission unification descoped.)
- [x] **T5** — Micro-cleanups: removed unused `npar` field (`ssm_sem.R:198`);
      moved `score_type` into `sem_details()` (added `path` param), removing the
      two duplicated overwrites and the dead "Latent" default. Test-fixture
      consolidation deferred to a candidate (not in AC5; ~17-block churn, pure
      test tidiness).
- [x] **T6** — `devtools::document()` (no man/NAMESPACE drift) + full
      `devtools::check()`: 0 errors / 0 warnings / 0 notes (3m33s).

## Work log

_All 2026-07-12; per-task detail preserved in the branch commits._

- Created by /milestone-plan from the legacy M5 close-review follow-ups
  (items c/d/e/g). Planned free-standing (no `Depends on: M7`) per user's
  sequencing choice — the M5 review deferred these "post-v2.0.0, not pre-freeze";
  behaviour-preserving so freeze risk is low, but confirm timing vs the v2.0.0
  freeze (~2026-07-26).
- T1 — `sem_check_contrast_arity()` single-sources the three arity blocks +
  characterization test. AC1 says "cli_abort()" but the conditions are base
  `stop(call.=FALSE)`, preserved verbatim; AC1's intent (unchanged message per
  branch) holds.
- T2 — `sem_fit_cfa()` chokepoint (fiml/listwise + multi-group `group.label`);
  both `cfa()` sites route through it; fits identical.
- T3 — `sem_detail_labels()` seam for `summary()` method/missing labels + direct
  unit test on both branches; output unchanged.
- T4 (narrowed, user-gated) — `sem_strict_metric_vacuous()` single-sources the
  vacuous-metric rule at three ladder sites; strict snapshots unchanged.
  Cross-branch strict *emission* unification descoped (structurally distinct,
  byte-pinned emitters) → ROADMAP candidate.
- T5 — dead `npar` field removed; `score_type` single-sourced into
  `sem_details()` (new `path` param), deleting two identical overwrites + dead
  default. Test-fixture consolidation deferred → ROADMAP candidate (not in AC5).
- T6 — `document()` no drift; `check()` clean; status → review.

## Decisions

## Review

**Reviewed 2026-07-12. PR #32. Consistency gate + full suite + two-lens review.**

Fresh evidence per criterion (full suite: 381 test groups, 0 failed, 2 warnings
[pre-existing CPM-Hessian in `test-ci_accuracy.R`, unrelated], 51 skipped `On CRAN`):

- AC1 — `test-ssm_sem.R` "contrast arity is validated at every branch" +
  "validates its arguments" pass; validator reproduces all three sites' messages
  verbatim (both reviewers confirmed byte-identical). Note: the criterion says
  "cli_abort()" but the existing conditions are base `stop(call.=FALSE)`,
  preserved as-is (documented at T1); substantive requirement (every branch
  fires an unchanged message) is met and tested.
- AC2 — single- and multi-group SEM fit tests (`test-ssm_sem.R`,
  `test-ssm_sem_groups.R`) green; reviewers confirmed both `cfa()` arg sets
  identical (single-group: no group/group.label; multi-group: both, incl.
  `group.label = levels(...)`), `...` and `missing` translation preserved.
- AC3 — summary integration test (`test-ssm_sem.R:719`) + new
  `sem_detail_labels()` unit test pass; label strings byte-identical.
- AC4 — `test-ssm_sem_syntax.R` green incl. pinned `exp_strict_*` snapshots;
  strict-ladder groups tests green. (Emission unification descoped per gate.)
- AC5 — full suite green; reviewers confirmed `npar` genuinely dead (no readers)
  and `score_type` value identical at both call sites (`path` in scope).
- AC6 — `devtools::check()` fresh re-run: 0 errors, 0 warnings, 0 notes (3m40s).

Consistency gate: `cairn_validate.py` PASS; Coverage map complete (AC1–6 →
T1–6, all present); `document()` no diff; README in sync (untouched); pkgdown
`check_pkgdown()` clean; no new exports; no NEWS entry (no user-visible change);
no new top-level files; no DESIGN principle changed (impact report skipped).

Independent fresh-context review (two distinct-evidence lenses):
- [O] diff-bug reviewer (Opus): **0 findings** — every extraction reproduces
  exact branch logic, `cfa()` args, and message strings.
- [S] blame-history reviewer (Sonnet): **0 findings** — `group.label`
  protection (added in `6375452` for the second-minus-first contract,
  CLAUDE.md/DESIGN.md) preserved exactly; no prior intent undone; no D-entry
  contradicted.
- Nothing survived to the scorer (no findings). Both lenses independently noted
  one non-issue — `do.call` vs direct `cfa()` for a pathological user-supplied
  `group.label`/`model`/`data` in `...` — classified as a non-regression (both
  raise the same duplicate-argument error; the blame lens noted it *strengthens*
  the ordering protection). Logged, not actioned.
