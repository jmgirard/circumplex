<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M8: SEM-layer DRY single-sourcing

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Branch/PR:** —

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

- [ ] Contrast-arity validation lives in one internal validator called by all
      three former sites; each `cli_abort()` arity branch still fires with an
      unchanged message (a test exercises every arity-failure branch:
      wrong #groups, wrong #measures, single- and user-fit paths).
- [ ] The two `lavaan::cfa()` fit sites route through one internal fit helper
      (estimator/se/missing translation + `group.label`); existing SEM fits are
      byte-identical (current `ssm_sem()` tests + snapshots pass unchanged).
- [ ] `summary.circumplex_ssm_sem()` output is snapshot-identical, with detail
      lines produced by the shared label seam.
- [ ] Strict-tier syntax is single-sourced; the pinned strings
      `exp_strict_nomeas` / `exp_strict_meas` (`tests/testthat/test-ssm_sem_syntax.R:242,244`)
      remain byte-identical.
- [ ] Micro-cleanups landed (`npar` field removed, `score_type` overwrite
      removed) with no behaviour change; suite green.
- [ ] `devtools::check()` clean (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6

## Tasks

- [ ] **T1** — Extract shared contrast-arity validator; route the three sites
      (`R/ssm_sem.R:1099-1103,1134-1150,1366-1388`) through it; keep messages.
- [ ] **T2** — Extract internal fit chokepoint for the two `lavaan::cfa()`
      sites (`R/ssm_sem.R:689-704,1170-1178`); assert identical fits.
- [ ] **T3** — Route `summary.circumplex_ssm_sem()` detail lines
      (`R/ssm_sem.R:1583-1600`) through a shared label seam; snapshot unchanged.
- [ ] **T4** — Single-source strict-tier syntax across single/multi-group
      branches; keep `exp_strict_*` snapshots byte-identical.
- [ ] **T5** — Micro-cleanups: remove unused `npar` field, drop
      `sem_details()`'s overwritten `score_type`, consolidate test fixtures.
- [ ] **T6** — `devtools::document()` (if roxygen touched) + full
      `devtools::check()`.

## Work log

- 2026-07-12: created by /milestone-plan from the legacy M5 close-review
  follow-ups (items c/d/e/g), grounded on file:line locations verified this
  session. Planned free-standing (no `Depends on: M7`) per user's sequencing
  choice — note the M5 close-review deferred these "post-v2.0.0, not pre-freeze
  because they churn validated code"; behaviour-preserving, so freeze risk is
  low, but implement should still confirm timing against the v2.0.0 freeze
  (~2026-07-26).

## Decisions

## Review
