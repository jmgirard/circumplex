<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M8: SEM-layer DRY single-sourcing

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Branch/PR:** m8-sem-dry-single-sourcing

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
- [ ] The strict-tier vacuous-metric rule (`model=="strict" && rung=="metric"`)
      is single-sourced across the three ladder sites via one predicate; the
      pinned strings `exp_strict_nomeas` / `exp_strict_meas`
      (`tests/testthat/test-ssm_sem_syntax.R:242,244`) remain byte-identical.
      (Amended 2026-07-12 via gate: cross-branch *emission* unification
      descoped as not-worth-the-snapshot-risk → candidate row; see work log.)
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

- 2026-07-12: T1 done — `sem_check_contrast_arity()` single-sources the three
  duplicated arity blocks; added a characterization test locking every branch's
  message. Minor AC1 wording fix: the existing conditions are base
  `stop(call.=FALSE)`, not `cli_abort()` — preserved verbatim to keep messages
  byte-identical (converting would change the output). AC1's intent (each arity
  branch fires an unchanged message) is unchanged.

- 2026-07-12: T2 done — `sem_fit_cfa()` chokepoint owns the fiml/listwise
  `missing` translation and the multi-group `group.label` ordering; both former
  `lavaan::cfa()` sites route through it. Existing single- and multi-group fit
  tests pass unchanged (identical fits).

- 2026-07-12: T3 done — `sem_detail_labels()` seam maps the stored detail codes
  (method, missing) to display labels; `summary()` delegates to it. Added a
  direct unit test locking both branches (the existing integration test only
  hit mvn/fiml). Output unchanged.

- 2026-07-12: T4 done (narrowed) — `sem_strict_metric_vacuous()` single-sources
  the vacuous-metric rule at the three ladder sites; strict snapshots unchanged.
  AC4 amendment (user-gated 2026-07-12): the single-group and multi-group
  strict *emitters* in `ssm_sem_syntax.R` are structurally distinct (plain
  labels vs `c()`-vector cross-group labels) and byte-pinned; unifying them is a
  high-risk rewrite of statistical output for minor gain, so it was descoped to
  a candidate row (`ROADMAP.md`). Roxygen "metric rung is vacuous" prose left as
  is (can't single-source across separate doc blocks without templating).

- 2026-07-12: T5 done — dead `npar` field removed (never read); `score_type`
  single-sourced into `sem_details()` via a new `path` param, deleting the two
  identical overwrites (`ssm_sem.R` former 1297/1518) and the dead default.
  Suite green. Test-fixture consolidation deferred to a ROADMAP candidate
  (AC5 covers only npar + score_type; consolidation is ~17-block test churn
  with no behavioural payoff).

## Decisions

## Review
