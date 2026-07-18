# M29: `ssm_ci_accuracy()` occasions extension

- **Status:** review
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
- [x] **T2** — Implement the occasions path in `R/ssm_ci_accuracy.R`: remove
      the guard (:197), wire the stacked-MC population + wide-row simulation +
      per-occasion/contrast accuracy rows. Test-first (regression fixture
      asserting the correct population structure + boundary behavior).
- [x] **T3** — Simulation-coverage oracle:
      `devel/m29-ci-accuracy-occasions-oracle.R` + committed
      `devel/m29-*-results.rds`, seeded, cell-indexed by level, smoke-first;
      interior + boundary cells; pre-registered acceptance in the script
      header; asserting test with a provenance comment.
- [x] **T4** — Discrimination oracle (§2.3.2 + RR07): the zeroed-cross-blocks
      occasions run ≡ the two-group independent diagnostic on coverage +
      `Median_width` (SE band); plus the dependent-vs-zeroed contrast
      `Median_width` ratio vs the closed-form Δe width target and a
      reversal-side Δd cell. Committed script + results, asserting test.
- [x] **T5** — NEWS.md entry (error → runs behavior change); update the
      informative-error test; `devtools::document()`; `devtools::check()`
      clean.

## Work log

- 2026-07-17: created by /milestone-plan (deferral §1.4; sibling M28 §1.1);
  implement → in-progress, branch cut. Gate: storage settled (stacked per-group
  stats at analysis time); T1 ip-touching → RB07; blocked; ingested RR07 (Fable)
  → M29-D1 + [[D-017]], archived. Amendment gate (approved): AC2/AC3/AC4 +
  T1/T4 synced. T1 done — `devel/m29-design.md` blueprint per [[D-017]].
- 2026-07-17: implement gate → M29-D2 (R12 add; shared-W bootstrap replay).
- 2026-07-17: T2 done — occasions branch in `ssm_ci_accuracy.R` (stored stacked
  `(n,μ,Σ)`; guard removed; `MVN(μ,Σ̂_g)` via `mvn_root`; shared-W bootstrap +
  `ssm_mc_replicates(occ_k=)` MC; flat/rank/structure/legacy refusals;
  occasions-aware `summary()` + R12). AC1/AC4 tests; `test()` clean.
- 2026-07-17: T3 done — AC2 simulation-coverage oracle (`m29-*-oracle.{R,md}` +
  rds): diagnostic-reported vs direct empirical coverage at the plug-in; 27/27
  within the 4-SE band.
- 2026-07-17: T4 done — AC3 discrimination oracle (`m29-*-discrimination.{R,md}`
  + rds): invariant (zeroed-occasions ≡ two-group ref) + closed-form Δe width
  √(w′Σw/w′Σ₀w) <1% + Δd reversal sign; ≥2 oracle types met.
- 2026-07-17: T5 done — NEWS (error→runs); roxygen occasions Limitations + refusal
  note; `document()`; `check()` clean (0/0/0). Status → review.

## Decisions

### M29-D1 (2026-07-17): RR07 ingested — occasions population design settled

Design promoted to [[D-017]] (R1–R11 encoded in the amended AC2/AC3/AC4 + T1/T4
at the 2026-07-17 amendment gate; standing rejects R13 `data=` fallback, R14
CPM-diagonal+observed-cross, R15 shrinkage, R16 cross-group pooling all recorded
there). R12 (`summary()` joint-cert breadcrumb) resolved in M29-D2 below.

### M29-D2 (2026-07-17): R12 breadcrumb added; occasions bootstrap replay reuses the weight machinery

Settled at the implement gate. **R12 (add):** `summary()` notes when a paired
contrast's c=1 joint-certification rate (`cert[T1]&&cert[T2]`) is below `1−α` —
the §2.2 caveat made legible; gates nothing. **Bootstrap replay:** reuse the
weight path with one shared person-resample `W` across all k occasion blocks
(same law as the person-row bootstrap); MC reuses `ssm_mc_replicates(occ_k=)`.

## Review
