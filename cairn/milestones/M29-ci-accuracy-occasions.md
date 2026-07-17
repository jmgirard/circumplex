<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
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
- Design + implement the occasions plug-in population: build the diagnostic's
  simulation population from the stacked person-level score covariance —
  within-person cross-occasion dependence entering through the off-diagonal
  p×p blocks (§2.2, the same object the occasions Monte Carlo engine already
  uses) — draw wide person-rows, re-run `ssm_analyze(occasions = )` per
  simulated dataset, and report per-occasion CI-accuracy rows (plus the
  paired-contrast row when the object carried `contrast = TRUE`).
- Remove the `details$occasions` error guard
  ([R/ssm_ci_accuracy.R:197](R/ssm_ci_accuracy.R:197)); replace the
  informative-error test with the new run-don't-error contract.
- Full oracle battery (§2.3), the acceptance bar set at plan gate:
  simulation-coverage (occasions population with known cross-occasion
  dependence, interior + boundary cells) **plus** a deterministic invariant
  (independent-re-paired blocks reproduce the independent-groups diagnostic
  within Monte Carlo error). Seeded, cell-indexed by level (LESSONS
  2026-07-13), committed `devel/m29-*-results.rds` + regeneration script with
  pre-registered acceptance in the header.
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
      ≥1 interior cell plus a boundary cell (pole-straddling or flat occasion);
      seeded `devel/m29-*-results.rds` + regeneration script committed.
      (RB tripwire: ip-touching)
- [ ] **AC3** — a second, deterministic oracle type: an invariant
      (independent-re-paired occasion blocks reproduce the independent-groups
      diagnostic within Monte Carlo error) corroborates the occasions path,
      meeting the ≥2-independent-oracle-types bar.
- [ ] **AC4** — boundary regressions per CLAUDE.md: a pole-straddling occasion
      and a flat/zero-variance occasion each produce a sensible (non-erroring,
      correctly-wrapped) diagnostic; tested.
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

- [ ] **T1** — Design note: how the stacked cross-occasion covariance (§2.2)
      becomes the diagnostic's plug-in simulation population, and how
      per-occasion + contrast accuracy rows are reported. Written before code.
      (RB tripwire: ip-touching — the extension is "its own design" per spec
      §1.4; decide at implement whether it needs a fresh Fable RB.)
- [ ] **T2** — Implement the occasions path in `R/ssm_ci_accuracy.R`: remove
      the guard (:197), wire the stacked-MC population + wide-row simulation +
      per-occasion/contrast accuracy rows. Test-first (regression fixture
      asserting the correct population structure + boundary behavior).
- [ ] **T3** — Simulation-coverage oracle:
      `devel/m29-ci-accuracy-occasions-oracle.R` + committed
      `devel/m29-*-results.rds`, seeded, cell-indexed by level, smoke-first;
      interior + boundary cells; pre-registered acceptance in the script
      header; asserting test with a provenance comment.
- [ ] **T4** — Deterministic invariant oracle: independent-re-paired occasion
      blocks reproduce the independent-groups diagnostic within Monte Carlo
      error (the genuinely-discriminating dependence check, §2.3.2).
- [ ] **T5** — NEWS.md entry (error → runs behavior change); update the
      informative-error test; `devtools::document()`; `devtools::check()`
      clean.

## Work log

- 2026-07-17: created by /milestone-plan (longitudinal deferral §1.4, promoted
  from the "Longitudinal deferrals" ROADMAP candidate). Extends the M25
  occasions core's diagnostic; full §2.3 oracle battery set as the acceptance
  bar at the plan gate. Sibling M28 planned in the same run (§1.1 sugar).
- 2026-07-17: /milestone-implement — status → in-progress; branch
  m29-ci-accuracy-occasions cut from master (in sync with origin).
- 2026-07-17: question gate. Storage settled (store stacked per-group stats at
  analysis time). Population structure (T1 ip-touching tripwire) escalated to a
  fresh Fable RB per user decision; implement paused pending the RR.
- 2026-07-17: blocked on RB07 (occasions plug-in population design; 6 questions).
- 2026-07-17: ingested RR07 (Fable) → M29-D1 + [[D-017]]; RB07/RR07 archived;
  status → in-progress. No standing D-entry contradicted.

## Decisions

### M29-D1 (2026-07-17): RR07 ingested — occasions population design settled

The occasions plug-in population design (RB07 → RR07, Fable, 2026-07-17) is
promoted to [[D-017]]. Triage: **apply** R1–R11 (construction (a); refuse
explicit CPM on occasions; `occ_k`-tagged storage; rank-deficiency
warn-not-refuse; ladder unchanged + asymmetric-regime doc; contrast unconditional
`d`, occasion rows D-007 conditional, §2.2 caveat → joint-cert columns; AC3
three-arm battery; AC4 flat refusal; ≥1 AC2 contrast cell; `(r−1) %/% k + 1`
row↔group mapping at `run_one`/`row_n`/`sds`/`build_pop`; legacy
`suff_stats = NULL` refused with a re-run message). **consider** R12 (`summary()`
breadcrumb when the c=1 joint-cert rate ≪ 1−α — decide T2/T5). **reject** R13
(occasions `data=` fallback — no such objects off the dev line), R14 (construction
b, 98% non-PSD), R15 (shrinkage — attenuates the dependence under test), R16
(pooling — replay is per-group).

Two **plan-owned amendments** deferred to the `/milestone-implement` amendment
gate at resume (not made here): **AC3** relabel "deterministic" → "invariant +
closed-form" and add the dependent-vs-zeroed `Median_width` arm + closed-form Δe
width target + a reversal-side Δd cell (coverage is provably blind to
dependence-dropping — the adaptive procedures cover at nominal even from a
wrongly-independent population; width carries the signal); **AC4** → informative
flat-occasion refusal (extend `R/ssm_ci_accuracy.R:314-317` row-wise);
pole-straddling + near-zero occasions run and are tested.

## Review
