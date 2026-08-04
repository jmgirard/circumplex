# M69: Correlation-metric pricing for `axes_reliability()`'s corrected component SEs

- **Status:** blocked
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Price `axes_corrected_se()`'s corrected branch at the implied correlation matrix
`cov2cor(Sigma-hat)` — the matrix `axes_scaling_factor()` already uses — so both
halves of the correlation-metric correction are computed on one metric.

## Scope

**In:** the `corrected` branch of `axes_corrected_se()`
(`R/axes_corrected_se.R:156-158`), repriced at `cov2cor(Sigma-hat)`; the pricing
of the two sides of the FIML `corrected/naive` ratio
(`R/axes_reliability.R:1691`), settled by a Fable review before any code change;
the assertions and committed fixtures whose values move under the repricing;
re-measurement of M66's component-SE calibration; the prose statements of the
correction's size, direction and sign on the four surfaces that carry them; and
the stale cross-reference at `R/axes_scaled_fit.R:135`.

**Out:** the scaled *difference* test for nested comparison, and a
Swain/Bartlett small-sample mean correction to `T` — both stay ROADMAP
candidate rows with their standing promotion conditions. Reporting `p*` and `N`
in `details`, and naming the lavaan variant the scaled `cfi`/`rmsea` match →
M70. The `naive` branch's own pricing stays at the raw `Sigma-hat`: it exists to
reproduce lavaan's number and is fenced against it.

## Acceptance criteria

- [ ] **AC1** — The `naive` branch still reproduces lavaan's own component
      standard errors: `tests/testthat/test-axes-corrected-se.R:67-69` (xi1,
      xi2, zeta1) and `:191-194` (all four, blockwise) pass with their
      assertion lines unedited. Every *other* assertion or committed fixture
      whose value moves under the repricing is enumerated in this file with its
      old value, its new value, and whether it was re-pinned or regenerated —
      at minimum `:203`, `:204`, the live-vs-stored arm over
      `fixtures/m66-corrected-se-cells.rds`, and the `analytic` column of the
      BC6 bootstrap fixture. A regenerated fixture records new provenance; no
      fixture is left carrying pre-M69 values.
- [ ] **AC2** — The `corrected` branch is priced at `cov2cor(Sigma-hat)`,
      verified against an independent recomputation: a vech-space oracle
      forming Delta, V and the standardization Jacobian as literal matrices at
      `cov2cor(Sigma-hat)` and inverting `Delta'V Delta` directly agrees with
      `axes_corrected_se()$corrected` to under 1e-6 relative on the probe maps
      `test-axes-corrected-se.R` builds analytically, evaluated at the same
      fitted `Sigma-hat` with no refit. The bar is set from the discrimination
      required: the superseded raw-`Sigma-hat` pricing differs by 1.05e-3
      relative on its closest component (xi1; xi2 and zeta1 differ by 1.6e-3
      and 1.7e-3) at n = 600, so 1e-6 fences it by 1000x while sitting orders
      above a no-refit oracle's arithmetic jitter.
- [ ] **AC3** — M66's calibration design is re-run under the new pricing at all
      three cells (complete, 15% MCAR, M1 MAR) and the regenerated cells
      replace the committed fixture with updated provenance. Every prose
      passage in `R/axes_reliability.R`'s roxygen, `R/axes_reliability_oop.R`'s
      printed output, `vignettes/axes-reliability.Rmd` and `NEWS.md` that
      characterizes the size, direction or sign of the SE correction is then
      **read for meaning, not grepped for literals** — the shipped statements
      include "about 40%", "about 7% below" and a bare "understates it
      slightly", none of which carry a searchable figure — and each is listed
      with its old wording and its new wording, or "unchanged, because the
      re-run figure rounds the same way".
- [ ] **AC4** — The pricing of the two sides of the FIML `corrected/naive`
      ratio is settled by an ingested Review Report, and a test pins the
      resulting behaviour: the reported FIML SE's response to multiplying the
      fitted `Sigma-hat` by a scalar is exactly what that report directs —
      invariance under same-matrix pricing, or the recorded (N-1)/N factor
      otherwise. The FIML calibration cell in the existing suite passes at its
      stated bar. `(RB tripwire: no-oracle)`
- [ ] **AC5** — `R/axes_scaled_fit.R`'s comment on the Wc construction cites a
      range of `R/axes_corrected_se.R` spanning at most 15 lines that contains
      `diag(wc) <- -rowSums(wc * sigma)`, and states which matrix each side is
      priced at. Evidence: a test parses the cited range out of the comment and
      asserts both properties, so a later edit that moves the construction
      reddens rather than rotting silently.
- [ ] **AC6** — The profile's `verify` slot is clean: `devtools::document()`
      after roxygen changes and `devtools::test()` passing, plus
      `devtools::check()` before review.

## Coverage

- AC1 → T2, T4
- AC2 → T2, T3
- AC3 → T5, T6
- AC4 → T1, T3
- AC5 → T7
- AC6 → T3, T4, T7

## Tasks

- [ ] **T1** — Escalate the FIML ratio-pricing question via `/milestone-brief`
      and ingest the resulting RR before any code change. The brief states the
      measured artifact (a mixed-matrix ratio injects (N-1)/N: 0.17% at
      n = 600, 1% at n = 100, always shrinking the SE) and the candidate fix
      (price both sides at `cov2cor(Sigma-hat)` via a third return value, so
      AC1's lavaan fence survives). `(RB tripwire: no-oracle)`
- [ ] **T2** — Test-first: add the vech-space oracle at `cov2cor(Sigma-hat)`
      and confirm it is RED against the shipped raw-`Sigma-hat` pricing before
      T3 changes anything.
- [ ] **T3** — Reprice the `corrected` branch at `cov2cor(Sigma-hat)`
      (`R/axes_corrected_se.R:156-158`) and wire the FIML ratio
      (`R/axes_reliability.R:1691`) as the ingested RR directs.
- [ ] **T4** — Re-pin the moved assertions (`test-axes-corrected-se.R:203`,
      `:204`) and regenerate both committed fixtures with new provenance,
      recording old and new values per AC1.
- [ ] **T5** — Re-run M66's calibration design at complete, 15% MCAR and M1 MAR
      under the new pricing; regenerate the cells fixture.
- [ ] **T6** — Read the four prose surfaces in full, update every size,
      direction or sign statement the re-run moves, and commit the
      per-passage classification ledger.
- [ ] **T7** — Repair the stale cross-reference at `R/axes_scaled_fit.R:135`
      and add the range guard AC5 requires.

## Work log

- 2026-08-03: created by /milestone-plan.
- 2026-08-03: criteria audit ([O], fresh context, authored none of the drafts) returned findings on all six drafted criteria. Fixed in the wording before this file was written: an unreproducible "about 3e-3" discrimination figure (measured live at 1.05e-3 on the governing component); an AC1 whose "unedited" framing hid four assertions and fixtures that actually break; and a bounded-promise failure whose grep-for-literals procedure provably misses "about 40%", "about 7% below" and a bare sign claim on shipped surfaces. Not fixed here: the FIML ratio's mixed-matrix (N-1)/N artifact, escalated at the gate and now AC4/T1.
- 2026-08-03: plan gate chose escalating the FIML ratio pricing to a Fable review over settling it in session as same-matrix pricing via a third return value, because it changes an exported number by a sample-size-dependent factor and is estimator math where a plausible-but-wrong answer survives ordinary review; falsified by an RR resting only on evidence this session already held.
- 2026-08-03: blocked on RB15 (metric pricing of the corrected component SEs and the FIML ratio), which T1 escalates and AC4 is verified against.
- 2026-08-03: plan gate chose re-running all three calibration cells over re-running the two fast cells and arguing the M1 MAR cell unchanged, because the figures are documented user-facing claims; falsified by a measured demonstration that the repricing is exactly scale-only in that cell.

## Decisions

## Review
