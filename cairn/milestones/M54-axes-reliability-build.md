# M54: Axes-reliability (Strack 2013) — build `axes_reliability()`

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** RR09
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Implement and oracle-validate the M53-designed `axes_reliability()` estimator —
an item-level restricted tau-equivalent CFA reading circumplex axes reliability
off the axes variance ξ1 — with its two-layer validation battery, per RR09 GO.

## Scope

**In:** a standalone exported `axes_reliability()` (parallel to
`fit_structure()`) for octant type-a instruments: the flat-form fixed-links
lavaan model (weights via `snap_trig`, `orthogonal = TRUE`, equal-axis and
shared-ζ1 labels, free errors), reusing the `sem_fit_cfa()` chokepoint
(`R/ssm_sem.R:736-757`); Spearman–Brown reliability + SEm (**z-standardized SD
default**, exposed as an argument); the Nunnally–Bernstein comparison; a
list-classed S3 object with print/summary + roxygen; the Layer-A (Table 3) and
Layer-B (population-matrix + finite-sample + cross-engine) oracles; the
refuse/boundary/listwise contract; a **bundled simulated item-level example
dataset** (seed-pinned `data-raw/` generator) for the help page.

**Out:** non-octant types b–f and quasi-circumplex weights → deferred-in-spec
candidate; the secondary correlation-matrix-only input → candidate (N–B is
`NA`-with-reason there); blockwise ζ2 → candidate; FIML on items → candidate.

## Acceptance criteria

- [ ] AC1 (BC1): Spearman–Brown on Table 3's printed col 6 (/100) and col 10
      must reproduce col 11 for the four anchor rows (IAL S1 Self; IPI-A S9
      Self; OCAI S15 Self; COC S16 Self) within ±.005, and for every
      non-blocked type-a row of Table 3 within ±.01.
- [ ] AC2 (BC2): `sqrt(col 12) · sqrt(1 − col 11)` must reproduce col 13 within
      ±.02 for at least the IAL, OCAI, and COC anchor rows.
- [ ] AC3 (BC3): item_n computed from instrument structure as the per-axis Σwᵢ²
      must be **exact** (after snapping): 64-item octant → 32; 32-item → 16;
      16-item → 8; and equal across the two axes for every balanced octant
      instrument.
- [ ] AC4 (BC4): A test must assert, on the fitted lavaan object: all loadings
      fixed (zero free loadings); AX/AY variances equality-
      constrained; all scale-specificity variances share one label; **every**
      latent covariance fixed at 0; item errors free; and `df = p(p+1)/2 − p − 3`
      for the non-blocked MVP model.
- [ ] AC5 (BC5): Fitting the exact population matrix built from known (ξ1, ξ2,
      ζ1, ε) for an octant instrument must recover every component within 1e−4
      and give χ² < 1e−6, with the lavaan (N−1)/N likelihood rescaling
      explicitly handled (wishart likelihood or corrected expectation); all ε̂_i
      must be equal within 1e−6.
- [ ] AC6 (BC6): ≥2 Monte-Carlo cells (distinct ξ1 levels) where the mean ξ̂1
      across replicates is within 2 Monte-Carlo SEs of truth.
- [ ] AC7 (BC7): lavaan and OpenMx fits of the identical model on identical
      input must agree on all free component variances within 1e−3 (expected
      agreement ~1e−5) on ≥2 datasets; the test skips (not passes) when OpenMx
      is unavailable; no new Imports (D-006/D-014).
- [ ] AC8 (BC8): The Nunnally–Bernstein implementation must pass an oracle that
      does not share its code path: a worked example whose Rel_scaleᵢ, Σwᵢ², and
      Var_axis are computed independently (by hand or by an independent route in
      the test), agreeing within 1e−6; Table 3 col 14 must **not** be cited as
      its oracle.
- [ ] AC9 (BC9): A synthetic high-scale-specificity cell (scale-specificity ≥
      .40 of item variance, axes ≤ .15) where the test asserts `NB_reliability −
      CFA_reliability > 0` with a pre-registered margin (≥ .05), reproducing the
      paper's Figure 3 headline.
- [ ] AC10 (BC10): Tests must assert: θ = 360 → weights exactly (+1, 0); θ = 90
      → exactly (0, +1); θ = 0 and θ = 360 yield identical weights; weights pass
      through the snapping helper (no 1e−16 residue in emitted syntax).
- [ ] AC11 (BC11): ξ̂1 ≤ 0, or any negative estimated variance, must yield
      reliability/SEm = NA plus a warning and a boundary flag in the output —
      never a negative, clipped, or silently-zeroed reliability; a small
      positive ξ̂1 (e.g. .03) must flow through to a small reliability (COC-style
      .19), not be treated as degenerate.
- [ ] AC12 (BC12): Each of the following must error informatively: scale count
      ≠ 8; angle multiset ≠ octants() mod 360 (including an unequal-spacing case
      and a duplicate-angle case); NA angle; any scale with < 2 items; item in
      the instrument map absent from the data; non-finite values in the data;
      zero-variance item; complete-case N ≤ p or non-PD correlation matrix;
      lavaan non-convergence.
- [ ] AC13 (BC13): The chosen policy (recommended: listwise with an informative
      message reporting the complete-case N) must be documented and tested,
      including the refusal when complete-case N ≤ p; pairwise correlation input
      must not occur.
- [ ] AC14: A bundled simulated item-level example dataset (seed-pinned
      `data-raw/` generator with provenance; roxygen in `R/example_data.R`;
      `_pkgdown.yml` row) exists and `axes_reliability()` runs on it in a
      non-`\dontrun` help example.
- [ ] AC15: `Rscript -e 'devtools::test()'` clean (verify slot).
- [ ] AC16: `Rscript -e 'devtools::check()'` clean — 0 errors, 0 warnings, NOTEs
      justified (consistency-gate).

## Coverage

- AC1, AC2 → T2
- AC3, AC10 → T1
- AC4 → T3
- AC5 → T4
- AC6 → T5
- AC7 → T6
- AC8, AC9 → T7
- AC11, AC12, AC13 → T8
- AC14 → T10
- AC15 → T1–T11
- AC16 → T9, T10

## Tasks

- [ ] T1. Weights + item_n: route scale weights through `snap_trig`
      (`R/ssm_sem_syntax.R:160-165`) with axes at 0°/90°; per-axis item_n =
      Σwᵢ². Tests first: BC10 pole weights, BC3 exact item_n.
- [ ] T2. Spearman–Brown reliability + SEm (z-standardized SD default arg).
      Layer-A published-value oracle from Table 3 (`references/strack2013.md`).
      Tests first: BC1, BC2.
- [ ] T3. lavaan constraint-set builder — flat fixed-links form,
      `orthogonal = TRUE`, equal-axis + shared-ζ1 labels, free errors; reuse
      `sem_fit_cfa()` (`R/ssm_sem.R:736-757`); flat-vs-hierarchical equivalence
      comment. Tests first: BC4 (constraints + df).
- [ ] T4. Population-matrix recovery oracle with (N−1)/N rescaling handled
      (wishart likelihood). Tests first: BC5.
- [ ] T5. Finite-sample Monte-Carlo recovery oracle (shared generator with the
      T10 dataset). Tests first: BC6.
- [ ] T6. Cross-engine lavaan/OpenMx oracle, `skip` when OpenMx absent, no new
      Imports. Tests first: BC7.
- [ ] T7. Nunnally–Bernstein implementation + code-independent worked-example
      oracle + high-scale-specificity direction cell. Tests first: BC8, BC9.
- [ ] T8. Refuse contract + boundary policy + listwise missing-data: reuse the
      `paf2()` NA precedent (`R/fit_structure.R:16-28`), `!is.finite` guards
      (M32/M35), modular-angle check via `octants()`
      (`R/convenience_functions.R:33-35`). Tests first: BC11, BC12, BC13.
- [ ] T9. OLS-shadow internal estimator (RR09 B-1) — regress off-diagonal r's
      on `(cos Δ, 1, same-scale)`; SEM-independent cross-check + start values;
      assert close agreement with ML on synthetic data.
- [ ] T10. S3 list object + print/summary + roxygen (corr-as-cov SE caveat,
      identical-per-axis rows explained); `_pkgdown.yml` row; NEWS.md entry;
      bundle the simulated dataset (`data-raw/` generator, `data/*.rda`,
      `R/example_data.R` doc) and use it in the help example. (AC14)
- [ ] T11. Supplement retrieval (RR09 B-2) — one attempt at the 2013 SAGE
      LISREL-syntax supplement; bank in `cairn/references/` if found, drop if
      link-rotten.

## Work log

- 2026-07-23: created by /milestone-plan; blocker cleared (cairn `_BC_HEAD` fix landed, parser reads all 13 RR09 BCs); Driving RR09, BC1–BC13 ingested verbatim as AC1–AC13; example = bundled simulated dataset; OLS-shadow (B-1) + supplement retrieval (B-2) folded in.

## Decisions

## Review
