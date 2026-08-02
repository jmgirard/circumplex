# M68: Scaled global test statistic for `axes_reliability()`

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Report a correlation-metric-calibrated global test statistic for
`axes_reliability()` in place of the normal-theory values that flatter fit by
roughly 4%, using a Satorra–Bentler-type scaling factor built from the same Γ
machinery M66 built for the component standard errors.

## Scope

**In:**
- A new `R/axes_scaled_fit.R` computing the scaling factor
  `c = tr(U Γ_R) / df` at the fitted Σ̂, and the independence model's own
  factor `c_b`, where `Γ_R` is the asymptotic covariance of the sample
  correlation matrix and `U = W − W Δ (Δ' W Δ)^-1 Δ' W`.
- `$fit$chisq`, `$pvalue`, `$rmsea` and `$cfi` become the scaled values on all
  three input paths (raw-listwise, `cormat`, `missing = "fiml"`); `$fit$df` and
  `$fit$srmr` are untouched. lavaan's unscaled six land in
  `details$fit_uncorrected`, the factors in `details$scaling_factor`.
- Source notes for the two papers the shipped formula code relies on:
  `satorra1994.md` (the scaling correction) and `cudeck1989.md` (the
  correlation-as-covariance analysis M66 already cites at
  `R/axes_corrected_se.R:22` without a page).
- Rewriting every caveat surface that currently tells the user the fit
  statistics are uncorrected.

**Out:**
- A scaled *difference* test for nested model comparison — no such comparison
  exists in this API; becomes a candidate row.
- SRMR, which is a residual summary rather than a test statistic and carries
  no scaling factor; its own metric question stays where M65 left it.
- Correcting `ssm_sem()`, which lives on the covariance metric and is not
  implicated (D-035).

## Acceptance criteria

- [ ] **AC1** — On raw-listwise, `cormat` and `missing = "fiml"` alike,
      `$fit$chisq`, `$pvalue`, `$rmsea` and `$cfi` are computed from
      `T_s = T / c` (with `$cfi` also using `c_b`); `$fit$df` and `$fit$srmr`
      are bit-identical to the values the current code returns; and
      `details$fit_uncorrected` and `details$scaling_factor` are present on
      every path. No path returns a scaled statistic beside an unscaled one
      *among the four χ²-derived statistics*, verified by a test that reads all
      four from each of the three paths.
- [ ] **AC2** — The factor is backed by two independent oracle types.
      *Closed-form:* an explicit vech-space routine forming `Γ_R`, `W`, `Δ` and
      `U` as literal matrices, agreeing with the shipped trace-identity
      implementation to ≤ 1e-8 relative on the canonical octant probe, a
      6-scale map and a one-item-per-scale map, citing `satorra1994 (p. N)`.
      *Simulation-coverage:* AC3. RR13's `E[T] = 261.1` against `df = 273` at
      its probe population (8 scales × 3 items, ξ1 = .35, ξ2 = .10, ζ1 = .08,
      N = 600; RR13 W-A) is **corroborating** to ≤ 0.5, not a gate — RR13 ships
      no reproduction code for it, so a miss escalates rather than fails.
- [ ] **AC3** — Over 2000 replicates at each of three complete-data
      populations — strong-axes (ξ1 ≈ .35), the Strack Table 3 COC-Other
      weak-axes/strong-general row, and the anti-conservative corner
      (ξ1 ≤ .05, ξ2 ≈ .6, large `p`) — `mean(T_s) / df` ∈ [0.97, 1.03] and the
      empirical rejection rate of `$fit$pvalue` at α = .05 ∈ [.036, .064]
      (±2.8 MC SE, the RR13 Q5 band), with the unscaled rate recorded
      alongside. Harness follows M65's: seed-pinned `devel/` generator,
      committed `.rds` summary, fast live smoke cell in the suite.
- [ ] **AC4** — On the FIML path, regenerating from the stored seeds of the
      M65 fixture's 2 / 5 / 10 % MCAR cells and the M66 fixture's 201-replicate
      M1 MAR cell, `mean(T_s) / df` ∈ [0.95, 1.05] in every cell; and the
      `em_stalled` refusal still fires before any scaled statistic is computed
      (test drives a stalled EM and asserts the refusal message, not a value).
- [ ] **AC5** — No user-facing surface still states the global fit statistics
      are uncorrected. The enumerating procedure is `grep -rn` over `R/`,
      `man/`, `vignettes/`, `NEWS.md` and `tests/testthat/` for `flattered`,
      `not corrected`, `261.1` and `approximate`; each hit is dispositioned in
      the work log as (a) updated to the corrected contract, (b) a historical
      reference inside a NEWS entry for an already-released version, or (c) an
      unrelated use of the term, listed and left untouched.
- [ ] **AC6** — `cairn/references/satorra1994.md` and
      `cairn/references/cudeck1989.md` exist, authored from
      `templates/source-note.md` with provenance blocks and page/equation
      anchors for the scaling formula and the correlation-metric result; both
      carry `INDEX.md` lines; `R/axes_scaled_fit.R` and `R/axes_corrected_se.R`
      cite them as `citekey (p. N)`.
- [ ] **AC7** — `devtools::document()` produces no diff; `devtools::test()` and
      `devtools::check()` clean (0 errors, 0 warnings; NOTEs justified).

## Coverage

- AC1 → T3, T4
- AC2 → T2
- AC3 → T6
- AC4 → T4, T7
- AC5 → T5
- AC6 → T1
- AC7 → T8

## Tasks

- [ ] **T1** — Author `cairn/references/satorra1994.md` and `cudeck1989.md`
      from the source-note template, with provenance blocks and page anchors;
      add both `INDEX.md` lines. **Gated on the maintainer shelving
      `satorra1994.pdf` and `cudeck1989.pdf` in `cairn/references/sources/`**
      (primary-sources hard stop). Do first — T2 cites its page numbers.
- [ ] **T2** — Test-first `R/axes_scaled_fit.R`: `axes_scaling_factor()`
      returning `c` and `c_b` at Σ̂, reusing `axes_se_derivs()`
      (`R/axes_corrected_se.R:50`) for Δ and the same dimnames-realignment
      discipline (`:93-101`), with AC2's explicit vech-space oracle alongside.
      Mirror M66's failure contract: named `reason` + NA, never a fallback to
      the unscaled factor.
- [ ] **T3** — Wire listwise + `cormat` into `R/axes_reliability.R:1654-1681`:
      scale after `fitMeasures()`, move the unscaled six into
      `details$fit_uncorrected`, add `details$scaling_factor`, and extend the
      fit-measure membership guard (`:1670-1677`) to the new fields.
- [ ] **T4** — FIML path. Decide and record which `Γ_R` the factor uses there —
      the complete-data form at Σ̂, or RR13 §4's saturated observed-information
      acov delta-transformed — then wire it, keeping the `em_stalled` refusal
      (`:1387-1393`) strictly ahead of any scaling.
      *(RB tripwire: no-oracle — no complete-data reference value covers this;
      AC4's simulation is the only oracle.)*
- [ ] **T5** — Rewrite the caveat surfaces: `R/axes_reliability_oop.R:52-59`
      and `:216-224`, roxygen `R/axes_reliability.R:663-669` and `:688-691`,
      `vignettes/axes-reliability.Rmd:147-152, 167-169`, `NEWS.md:26-31`, and
      the tests pinning the old strings
      (`tests/testthat/test-axes-corrected-se.R:502, 585, 629, 671`). Run AC5's
      sweep and record every disposition.
- [ ] **T6** — Complete-data simulation: seed-pinned generator under `devel/`,
      three populations × 2000 replicates, committed `.rds` summary with its
      regeneration recipe, plus a fast live smoke cell in the suite.
- [ ] **T7** — FIML simulation cells: regenerate from the M65 fixture's
      2/5/10 % MCAR seeds and the M66 fixture's M1 MAR seeds, store the T_s
      summaries beside AC3's.
- [ ] **T8** — `document()`, `test()`, `check()`; NEWS entry for the changed
      fit statistics.

## Work log

- 2026-08-02: created by /milestone-plan. Promoted from the ROADMAP candidate "Satorra–Bentler-style scaled test statistic for the axes-reliability χ²" (RR13 B-1), which M66 left explicitly uncorrected. Supersedes D-035's holding that the fit indices "keep their caveat" — see D-036.
- 2026-08-02: criteria audit ([O], fresh context) returned 13 findings; 5 clear-fixes applied before the gate — AC1's srmr/df clause contradicted its own no-mixture clause; AC3's [.035,.065] band at 500 reps was ±1.55 MC SE (a calibrated statistic fails ~12% of the time), raised to 2000 reps and the RR13 Q5 ±2.8 MC SE band; AC4 conflated the M65 fixture (no χ² stored, 5-rep M1 cell) with M66's 201-rep M1 cell; AC5's grep over-caught unrelated uses (`evaluating-circumplex-structure.Rmd:93`, `ssm_analysis.R:111`), so a third disposition was added; AC2's 261.1 was demoted from gate to corroboration because RR13 ships no reproduction code for it. Four judgment findings went to the question gate.
- 2026-08-02: plan gate chose scaling all three paths over shipping listwise+`cormat` first with FIML as a dependent milestone, because a path-dependent `$fit$chisq` is the exact trap the M65 SRMR fix cured (`R/axes_reliability.R:1635-1653`); falsified by the FIML calibration of AC4 missing its band under both candidate `Γ_R` constructions.
- 2026-08-02: plan gate chose scaling CFI via the independence model's own `c_b` over leaving CFI uncorrected, because `summary()` prints χ², RMSEA and CFI on one line (`R/axes_reliability_oop.R:257-262`) and RR13 B-2 named mixed-calibration comparison as the harm on the SE side; falsified by `c_b` proving unidentified or unstable on any accepted input.
- 2026-08-02: plan chose replacing `$fit`'s values and retaining the unscaled six in `details$fit_uncorrected` over adding parallel `*_scaled` fields, following M66's `details$se_uncorrected` precedent, so the default-read number is the calibrated one; falsified by a user needing both side by side in printed output.

## Decisions

## Review
