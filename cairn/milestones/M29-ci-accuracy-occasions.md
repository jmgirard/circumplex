# M29: `ssm_ci_accuracy()` occasions extension

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M25
- **Principles touched:** —
- **Branch/PR:** m29-ci-accuracy-occasions

## Goal

Replace `ssm_ci_accuracy()`'s occasions error guard with a correct
occasions-aware simulation path that respects within-person cross-occasion
dependence (stacked Monte Carlo population, spec §2.2), validated by a full
§2.3-style oracle battery.

## Scope

**In:**
- Design + implement the occasions plug-in population (§2.2, [[D-017]]): draw
  wide person-rows from the per-group stacked person-level score covariance
  (cross-occasion dependence in the off-diagonal p×p blocks), re-run
  `ssm_analyze(occasions = )` per simulated dataset, and report per-occasion
  CI-accuracy rows plus the paired-contrast row (when `contrast = TRUE`).
- Remove the `details$occasions` error guard
  ([R/ssm_ci_accuracy.R:197](R/ssm_ci_accuracy.R:197)); replace the
  informative-error test with the new run-don't-error contract.
- Full oracle battery (§2.3, amended per [[D-017]]): simulation-coverage + a
  width-based discrimination arm (zeroed-cross-blocks ≡ independent-groups
  diagnostic) + a closed-form Δe width target. Seeded, cell-indexed by level
  (LESSONS 2026-07-13), committed `devel/m29-*-results.rds` + regeneration
  script with pre-registered acceptance in the header.
- NEWS entry (exported behavior change: error → runs).

**Out:**
- occasions × measures ci_accuracy and occasions × contrast × grouping →
  remain ROADMAP candidate rows (their own design).
- Pairwise-deletion occasions (§1.3) untouched — listwise-only stands on the
  RR06 R6 estimand grounds; not reopened here.

## Acceptance criteria

- [ ] **AC1** — `ssm_ci_accuracy()` on a ≥2-occasion object returns a valid
      accuracy object whose simulation population is the stacked cross-occasion
      covariance (off-diagonal within-person blocks non-zero), **not** the
      dependence-ignoring per-group sufficient statistics; asserted
      structurally on a fixture.
- [ ] **AC2** — simulation-coverage oracle: across occasions populations with
      known cross-occasion dependence, the diagnostic's reported coverage
      tracks the true empirical coverage within a pre-registered tolerance, at
      ≥1 interior cell plus a boundary cell (pole-straddling or
      near-zero-amplitude occasion), with ≥1 cell exercising the paired-contrast
      row; seeded `devel/m29-*-results.rds` + regeneration script committed.
      (RB tripwire resolved: RR07 → [[D-017]])
- [ ] **AC3** — a discriminating oracle beyond coverage: coverage alone is
      provably blind to a dependence-dropping population (the adaptive replayed
      procedures cover at nominal even from a wrongly-independent population —
      RR07/[[D-017]]), so the discriminating observable is interval width. Two
      arms: **(invariant)** an occasions run with the cross-occasion blocks
      zeroed reproduces the already-validated two-group independent-groups
      diagnostic (same marginals) on both coverage and `Median_width` within a
      pre-registered SE-based band; **(closed-form)** the dependent-vs-zeroed
      paired-contrast `Median_width` ratio matches the closed-form Δe width
      target √(w′Σw / w′Σ₀w), with a reversal-side Δd cell (|Δd| > 90°)
      expecting the paired-wider reversal. Meets the ≥2-independent-oracle-types
      bar (simulation-coverage + invariant + closed-form).
- [ ] **AC4** — boundary contract per CLAUDE.md and [[D-017]]: a
      flat/zero-variance occasion is refused up front with an error naming the
      occasion (the shipped flat-profile refusal extended row-wise); a
      pole-straddling occasion produces a correctly-wrapped, non-erroring
      diagnostic; a near-zero-amplitude occasion runs, flags its `Structural`
      rows, and reports certification honestly; all tested.
- [ ] **AC5** — the removed error guard's test is updated to the new contract;
      the exported behavior change is documented (NEWS, roxygen);
      `devtools::test()` clean and `devtools::check()` clean (0 errors /
      0 warnings; NOTEs justified).

## Coverage

- AC1 → T2
- AC2 → T3
- AC3 → T4
- AC4 → T2, T3
- AC5 → T2, T5

## Tasks

- [x] **T1** — Design note: how the stacked cross-occasion covariance (§2.2)
      becomes the diagnostic's plug-in simulation population, and how
      per-occasion + contrast accuracy rows are reported. Written before code,
      grounded in [[D-017]] (RR07: construction (a), per-group observed stacked
      covariance, no CPM; `occ_k`-tagged storage; rank-deficiency
      warn-not-refuse; `(r−1) %/% k + 1` row↔group mapping).
- [ ] **T2** — Implement the occasions path in `R/ssm_ci_accuracy.R`: remove
      the guard (:197), wire the stacked-MC population + wide-row simulation +
      per-occasion/contrast accuracy rows. Test-first (regression fixture
      asserting the correct population structure + boundary behavior).
- [ ] **T3** — Simulation-coverage oracle:
      `devel/m29-ci-accuracy-occasions-oracle.R` + committed
      `devel/m29-*-results.rds`, seeded, cell-indexed by level, smoke-first;
      interior + boundary cells; pre-registered acceptance in the script
      header; asserting test with a provenance comment.
- [ ] **T4** — Discrimination oracle (§2.3.2 + RR07): the zeroed-cross-blocks
      occasions run ≡ the two-group independent diagnostic on coverage +
      `Median_width` (SE band); plus the dependent-vs-zeroed contrast
      `Median_width` ratio vs the closed-form Δe width target and a
      reversal-side Δd cell. Committed script + results, asserting test.
- [ ] **T5** — NEWS.md entry (error → runs behavior change); update the
      informative-error test; `devtools::document()`; `devtools::check()`
      clean.

## Work log

- 2026-07-17: created by /milestone-plan (longitudinal deferral §1.4). Sibling
  M28 planned in the same run (§1.1 sugar).
- 2026-07-17: /milestone-implement — status → in-progress; branch
  m29-ci-accuracy-occasions cut from master.
- 2026-07-17: question gate — storage settled (store stacked per-group stats at
  analysis time); population structure (T1 ip-touching tripwire) escalated to a
  fresh Fable RB.
- 2026-07-17: blocked on RB07 (occasions plug-in population design; 6 questions).
- 2026-07-17: ingested RR07 (Fable) → M29-D1 + [[D-017]]; RB07/RR07 archived;
  status → in-progress. No standing D-entry contradicted.
- 2026-07-17: amendment gate (user-approved) — AC2 boundary/contrast, AC3 →
  width + closed-form arms, AC4 → flat-occasion refusal; Scope oracle-battery
  line + T1/T4 synced. Coverage map unchanged.
- 2026-07-17: T1 done — `devel/m29-design.md` implementation blueprint
  (population + loop, storage shape, the `(r−1) %/% k + 1` row↔group refactor,
  structure-refusal, boundary/ladder/contrast unchanged, oracle plan).

## Decisions

### M29-D1 (2026-07-17): RR07 ingested — occasions population design settled

Design promoted to [[D-017]] (apply R1–R11, now encoded in the amended
AC2/AC3/AC4 + T1/T4; the AC amendments landed at the 2026-07-17 amendment gate).
**consider** R12 (`summary()` breadcrumb when the c=1 joint-cert rate ≪ 1−α —
decide T2/T5). Standing **rejects** (do not revisit without a superseding
entry): R13 occasions `data=` fallback (refuse legacy `suff_stats = NULL` with a
re-run message instead); R14 CPM-diagonal + observed-cross (98% non-PSD at
n=25); R15 shrinkage (attenuates the dependence under test); R16 pooling across
groups (replay is per-group).

## Review
