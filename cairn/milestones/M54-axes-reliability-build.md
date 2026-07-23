# M54: Axes-reliability (Strack 2013) — build `axes_reliability()`

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** RR09
- **Principles touched:** —
- **Branch/PR:** `m54-axes-reliability`

## Goal

Implement and oracle-validate the M53-designed `axes_reliability()` estimator —
an item-level restricted tau-equivalent CFA reading circumplex axes reliability
off the axes variance ξ1 — with its two-layer validation battery, per RR09 GO.

## Scope

**In:** a standalone exported `axes_reliability()` (parallel to
`fit_structure()`) for octant type-a instruments: the flat fixed-links lavaan
model (`snap_trig` weights, `orthogonal = TRUE`, equal-axis + shared-ζ1 labels,
free errors) via the `sem_fit_cfa()` chokepoint (`R/ssm_sem.R:736`);
Spearman–Brown reliability + SEm (**z-std SD default**, argument-exposed); the
N–B comparison; a list-classed S3 object (print/summary + roxygen); the Layer-A
(Table 3) and Layer-B (population-matrix + finite-sample + cross-engine)
oracles; the refuse/boundary/listwise contract; and a **bundled simulated
item-level dataset** (seed-pinned `data-raw/` generator) for the help page.

**Out (→ candidate):** non-octant types b–f + quasi-circumplex weights; the
correlation-matrix-only input (N–B `NA`-with-reason there); blockwise ζ2; item
FIML.

## Acceptance criteria

- [ ] AC1 (BC1, revised by RR10; RR09's original in Deviations below): SB(col 6
      /100, col 10) reproduces col 11 within ±.005 for the 4 anchors (IAL S1,
      IPI-A S9, OCAI S15, COC S16 — all Self) and ±.01 for the 11 self-consistent
      non-blocked type-a rows (all but IIP S6 Self, the erratum: assert sum
      101.0±0.1, |SB(.130,32)−.81|>.01, |SB(.120,32)−.81|≤.005); sum guard: the
      12 banked rows each sum to their banked total ±0.1.
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

**Deviations from RR09**

| BC | Disposition |
|---|---|
| BC1 | Superseded in full by RR10 (2026-07-23, Fable) to handle the IIP S6 Self source erratum; RR10's revised BC1 is ingested as AC1 above. BC2–BC13 stand. |

## Coverage

- AC1, AC2 → T2 · AC3, AC10 → T1 · AC4 → T3 · AC5 → T4 · AC6 → T5
- AC7 → T6 · AC8, AC9 → T7 · AC11, AC12, AC13 → T8 · AC14 → T10
- AC15 → T1–T11 · AC16 → T9, T10

## Tasks

- [x] T1. Weights + item_n via `snap_trig` (`R/ssm_sem_syntax.R:160`), axes at
      0°/90°, per-axis Σwᵢ². Tests: BC10, BC3.
- [x] T2a. Extend `strack2013.md` Table 3 banking (12 non-blocked type-a rows,
      cols 5–13) two-channel; done alongside the RR10 erratum ingest.
- [x] T2. Spearman–Brown reliability + SEm (z-std SD default arg); Layer-A
      oracle per revised BC1 (11-row ±.01 sweep, 4 anchors ±.005, IIP S6 Self
      3 assertions, component-sum guard). Tests: BC1, BC2.
- [ ] T3. lavaan constraint set — flat fixed-links, `orthogonal = TRUE`,
      equal-axis + shared-ζ1 labels, free errors; reuse `sem_fit_cfa()`
      (`R/ssm_sem.R:736`); equivalence comment. Tests: BC4.
- [ ] T4. Population-matrix oracle, (N−1)/N handled (wishart). Tests: BC5.
- [ ] T5. Finite-sample MC recovery (generator shared with T10). Tests: BC6.
- [ ] T6. Cross-engine lavaan/OpenMx, `skip` if absent, no new Imports. BC7.
- [ ] T7. N–B implementation + code-independent worked-example oracle +
      high-scale-specificity direction cell. Tests: BC8, BC9.
- [ ] T8. Refuse/boundary/listwise contract: `paf2()` NA precedent
      (`R/fit_structure.R:16`), `!is.finite` guards, modular `octants()` check.
      Tests: BC11, BC12, BC13.
- [ ] T9. OLS-shadow estimator (B-1) — regress off-diag r's on
      `(cos Δ, 1, same-scale)`; SEM-independent cross-check + start values.
- [ ] T10. S3 object + print/summary + roxygen (SE caveat, per-axis rows);
      `_pkgdown.yml` + NEWS; bundle the simulated dataset (`data-raw/` +
      `data/*.rda` + `R/example_data.R`) used in the help example. (AC14)
- [ ] T11. Supplement retrieval (B-2) — one attempt at the SAGE LISREL syntax;
      bank if found, drop if rotten.

## Work log

- 2026-07-23: created by /milestone-plan; blocker cleared (cairn `_BC_HEAD` fix landed, parser reads all 13 RR09 BCs); Driving RR09, BC1–BC13 ingested verbatim as AC1–AC13; example = bundled simulated dataset; OLS-shadow (B-1) + supplement retrieval (B-2) folded in.
- 2026-07-23: T1 done — `axis_weights()` + `axis_item_n()` in `R/axes_reliability.R`; BC3 (item_n 64→32/32→16/16→8, equal axes) + BC10 (pole snap, 0≡360) pass, mutation-proven; full `devtools::test()` clean (0 fail, 3097 pass).
- 2026-07-23: minor amend — T2 gains a source-extraction sub-task: `strack2013.md` banks only the 4 anchors (cols 6/10/11); BC1 needs all non-blocked type-a rows and BC2 needs cols 12/13, so the Layer-A oracle must first extend the banked Table 3 (two-channel protocol).
- 2026-07-23: extended `strack2013.md` Table 3 banking (all 12 non-blocked type-a rows + cols 12/13 for BC2 anchors), two-channel-verified.
- 2026-07-23: BC1 anomaly found — SB reproduces 11/12 non-blocked type-a rows within ±.01; IIP S6 Self fails (.017) and is a provable paper erratum (components sum to 101.0%, unique in the table; corrected %axes 12.0 restores 100.0% and reproduces printed .81). BC1 (Fable-authored) not literally satisfiable for that row — PENDING a handling decision (escalate vs documented erratum).
- 2026-07-23: blocked on RB10 — Fable escalation on BC1's handling of the IIP S6 Self erratum (drafted `cairn/reviews/RB10-axes-reliability-bc1-erratum.md`).
- 2026-07-23: ingested RR10 — erratum confirmed, oracle = option (a) + sum guard; BC1 revised (AC1 + Deviations table); banked Table 3 cols 5–9 for the 12 rows + fixed the source-note overclaim/SEm nuance; RB10/RR10 archived; resumed T2.
- 2026-07-23: T2 checkpoint (NOT yet checked off) — `axis_reliability_sb()` + `axis_sem()` written; Layer-A oracle (BC1 revised: 4 anchors ±.005, 11-row ±.01 sweep, IIP S6 Self erratum assertions, 12-row sum guard; BC2 SEm ±.02) passes targeted + mutation-proven; full `devtools::test()` running — T2 checks off only when it is clean.
- 2026-07-23: T2 verify clean (full `devtools::test()`: 0 fail, 3104 pass) — T2 checked off. Next: T3 lavaan constraint set (BC4).

## Decisions

- 2026-07-23 (RR10, ingested; full reasoning in `reviews/archive/RR10-…`):
  IIP S6 Self of Strack Table 3 is a source erratum — a single-digit %axes typo
  (13.0 for 12.0; ξ1 ≈ .12), over-determined by the 100.0% sum identity and the
  printed reliability; a printed-SEm nuance does not overturn it. Layer-A oracle
  = option (a): sweep the 11 self-consistent rows (±.01; anchors ±.005), pin
  IIP S6 Self with printed-pair-inconsistent + corrected-pair-reproduces
  assertions, plus a component-sum guard over the 12 banked rows (scoped).
- 2026-07-23: BC1 revised by RR10 (verbatim there, faithfully in AC1);
  `Driving RR:` stays RR09 so `cairn_validate` keeps enforcing BC2–BC13, with
  RR09's BC1 in the Deviations-from-RR09 table.

## Review
