# M171: Flag a lower discrepancy that extra starts find in cpm_fit()

- **Status:** planned
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP3, IP4, GP2, GP4, GP5
- **Resolves:** —
- **Surface tier:** user-facing — a new `cpm_fit()` argument, new `details` fields and a printed note
- **Branch/PR:** —

## Goal

After the engine's fit, `cpm_fit()` runs M170's fixed set of extra starts and reports a discrepancy more than 0.01 chi-square units lower than the fit, without changing the estimate, as D-072 decided.

## Scope

**In:** internal helpers in `R/cpm_fit.R` ported from `devel/m170-engine-estimate-helpers.R` (`m170_w_starts()`, `m170_base_par()`, `m170_theory_par()`, `m170_run_starts()`, `m170_finish()`, the cyclic-order and wall counts). `cpm_fit()` runs them after `cpm_engine()` and before the bootstrap. A new argument `check_lower = TRUE`. The check runs for `model = "quasi-circumplex"` with `scaling = "unit"` only. New `details` fields: whether the check ran, whether it fired, the gap, the lower solution's degrees of freedom, whether the scales' cyclic order changed, and how many more communality indices are at the bound. A note in `cpm_diagnostic_lines()`, so `print()` and `summary()` show it. `ssm_ci_accuracy()`'s internal fit passes `check_lower = FALSE`. The help section "Which solution is reported", the `check_lower` documentation, DESIGN.md's D-071 section, a paragraph in the `cpm-boundary-fits` vignette, and a NEWS.md entry. A devel oracle against M170's stored replicates.

**Out:** the extra starts in bootstrap replicates, by D-072 (top-level fit only). Adopting the lower solution, reporting its estimates, or reporting its chi-square beside the fit's, all rejected by D-072. Re-running the T calibration and changing the CPM oracles, since the estimate does not change. Adding the flag to the boundary markers or to `summary()`'s analytic caution, declined at this plan's question set. Other models and free scaling: the candidate row "Check other CPM models for lower fits".

## Acceptance criteria

- [ ] AC1: The check uses no random numbers and changes no reported value. A test asserts that `.Random.seed` is `identical()` before and after a cormat-path fit with `check_lower = TRUE`. Under the same `set.seed()`, a raw-data bootstrap fit with `check_lower = TRUE` and one with `check_lower = FALSE` return `identical()` `results`, `betas`, `fit`, and pre-existing `details` fields, and `.Random.seed` is `identical()` after the two fits. A test asserts that the start count is p + 1 + 3(p - 1) + 3 choose(p - 1, 2) + p on the verbal (p = 6), vocational (p = 7) and `jz2017` (p = 8) fits, and that every start is finite.
- [ ] AC2: The raw gap is (N - 1) × (F_reported - F_lowest), where F_lowest is the lowest discrepancy that the extra starts reach before the weight polish. If the raw gap exceeds 0.01, the flag fires, and otherwise it does not. When it fires, the lowest point is finished as the engine finishes its own (weight polish, then canonicalization). `details` then holds the gap after the polish in T units, the lower solution's degrees of freedom, whether its cyclic order of scales differs from the reported solution's, and how many more of its communality indices than the reported solution's exceed 0.995. `details` holds none of the lower solution's estimates.
- [ ] AC3: Tests assert four results. The flag fires on the CircE verbal-ability matrix at m = 2 with a changed cyclic order. Its gap there is within 0.01 of 3.158 T units, the lower point that RR27's random restarts and M170's W both found. It does not fire on the `jz2017` octant fit at m = 3 or on the CircE vocational-interest matrix at m = 1. It fires on M169's pinned sample (seed 21261736) with a gap above 1 T unit, the gap that M169's single communality-wall start found. The verbal and pinned tests skip on CRAN, because the basin `nlminb()` reaches can differ by platform. The pinned test replaces the M169 test that asserts that the fit does not signal a lower discrepancy.
- [ ] AC4: The shipped check agrees with M170's W on every accepted replicate of two cells: boundary N = 250 (459 fits) and interior N = 500 (466 fits). A devel script regenerates each replicate from M170's seeds on M170's machine (Apple M5 Pro, R 4.6.1 with R's reference BLAS). It checks that the shipped starts are `all.equal()` to `m170_w_starts()`'s, that the flag fires exactly where the stored `replaced[["W"]]` is `TRUE`, and, where it fires, that the gap is within 1e-6 of the stored E minus W T difference. Its committed record names the machine, the commit and both counts, with 0 mismatches.
- [ ] AC5: When the flag fires, `print()` and `summary()` each print one note. The note states the gap in chi-square units, and it states the lower solution's degrees of freedom when they differ from the fit's. It states whether the lower solution keeps the scales' order around the circle, and how many more communality indices it has at the upper bound. It states that the reported estimates are unchanged, and it names the `cpm_fit()` help section "Which solution is reported". Tests show the note on the verbal fit and no note on the `jz2017` fit or on a fit with `check_lower = FALSE`. On the verbal and `jz2017` fits, `cpm_boundary_markers()` returns the same markers with `check_lower = TRUE` and `FALSE`, and `cpm_marker_labels()` is unchanged.
- [ ] AC6: `check_lower` accepts only a single `TRUE` or `FALSE`, and a test asserts the error for `NA` and for `c(TRUE, TRUE)`. `details` records the check as not run, and no note prints, for `check_lower = FALSE`, for each of `"constrained-angles"`, `"equal-communality"` and `"circulant"`, and for `scaling = "free"`. A test covers each case. A start whose refit throws, or returns a non-finite F, is skipped. Tests make the first start, the last start, and the start that reaches the lowest point fail, and assert the check's result each time. A test makes every start fail and asserts that the check ran and did not fire. A test makes the finish of the lowest point throw and asserts that the flag fires on the raw gap, with the order and wall fields `NA`.
- [ ] AC7: The documentation matches the check. The help section "Which solution is reported" no longer says that no check signals a lower-discrepancy configuration. It describes the extra starts in words, and says that the estimate does not change and that the check runs only for the quasi-circumplex model with unit scaling. It says that the extra starts do not find every lower discrepancy, so a fit with no note can still have one. The `check_lower` documentation says that the check's cost grows with about the square of the number of scales. DESIGN.md's section "CPM engine estimate: what it is (D-071)" no longer says that no such diagnostic ships. The `cpm-boundary-fits` vignette explains the note. NEWS.md has a New features entry. `devtools::check(manual = TRUE)` reports 0 errors, 0 warnings and 0 notes.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T3
- AC3 → T3
- AC4 → T5
- AC5 → T4
- AC6 → T1, T2
- AC7 → T6, T7

## Tasks

- [ ] T1: Write the tests first, then port the helpers into `R/cpm_fit.R` as one internal check function. Build starts from the engine's own parameter vector with communality logits clipped to [-30, 30], and assert that they are finite (M170 lesson). Wrap each refit in `tryCatch()` and treat a non-finite F as a failed start. Use `local_mocked_bindings()` on `cpm_optimize_one()` and on the finish step for the failure probes.
- [ ] T2: Add `check_lower` to `cpm_fit()`, validated with `is_flag()`, and the scope gate and `details` fields. Run the check before `cpm_bootstrap()`. Add the RNG and `identical()` tests. Pass `check_lower = FALSE` in `ssm_ci_accuracy()` (`R/ssm_ci_accuracy.R:435`).
- [ ] T3: Write the flag tests on the verbal, vocational and `jz2017` fits (`tests/testthat/helper-cpm-oracles.R`). Replace the M169 test at `tests/testthat/test-cpm_boundary.R:141` with the pinned flag test.
- [ ] T4: Add the note to `cpm_diagnostic_lines()` (`R/cpm_oop.R:57`), with the M146 wrapping helpers. Add the print, summary and marker tests, and update any snapshot whose fit now fires.
- [ ] T5: Write `devel/m171-lower-check-oracle.R`, reusing `replicate_seed()` and `draw_R()` from `devel/m170-engine-estimate.R`. Run it in the background (about 15 minutes) and commit its record `devel/m171-lower-check-oracle.md`.
- [ ] T6: Write the roxygen section and `@param check_lower`, and run `devtools::document()`. Update DESIGN.md's D-071 section in place, marked `corrected M171`. Write the vignette paragraph in `vignettes/cpm-boundary-fits.Rmd.orig`, and re-render each precomputed vignette whose CPM output changes. Write the NEWS.md entry.
- [ ] T7: Measure `devtools::test()` wall time before T1 and after T6. If the increase is over 60 s, pass `check_lower = FALSE` in tests that are not about the check. Run `devtools::check(manual = TRUE)`.

## Work log

- 2026-10-28: created by /milestone-plan from the candidate row "Flag a lower optimum in CPM fits".
- 2026-10-28: tier check: milestone — a new exported argument, a printed note and design choices.
- 2026-10-28: idea vet: four concerns, each posed at the question set. Cost grows with about the square of p (0.9 s at p = 8, 3.5 s at p = 16): argument, on by default. M170 measured only the quasi-circumplex model with unit scaling: default model only. Notes on small gaps: keep 0.01 and print the gap. Whether the analytic caution fires: no, own note.
- 2026-10-28: lessons used: 2026-10-28 (M170) refit starts from clipped engine logits; 2026-10-26 (M167) theory-centred starts miss lower basins; 2026-10-27 (M169) lower discrepancy means a worse p only at equal df; 2026-08-24 (M108 family) skip preconditions; 2026-08-31 (M116) plant defects in the code under test.
- 2026-10-28: collision check: absorbs the candidate row "Flag a lower optimum in CPM fits"; extends M169 and M170 (done) under D-071 and D-072, superseding neither. Inbox sweep: 0 open issues and 0 open PRs.
- 2026-10-28: plan measured E's Wald coverage on M170's stored replicates, split by whether W found a lower point: 0.82 to 0.94 on those fits and 0.76 to 0.94 on the rest, by cell. Basis for the caution question.
- 2026-10-28: criteria audit (full mode, fresh Opus reader): 16 findings, all fixed. The prototype start check moved to the devel oracle, since `devel/` is outside the build. A `.Random.seed` assertion was added. The flag fires on the raw gap and stores the polished gap. The lower solution's df was added and its estimates were dropped (close to the report-both option that D-072 rejected, and an IP3 burden). The verbal test skips on CRAN. The AC4 gap comparison runs only where the flag fires, on M170's machine. The failure probes vary which start fails. The other models are named. Cost wording uses the square of p.
- 2026-10-28: question set: summary() caution — no, the flag gets its own note; cost control — `check_lower = TRUE`; models — quasi-circumplex with unit scaling only; threshold — 0.01 T units, gap printed.
- 2026-10-28: plan gate chose an argument on by default over always-on with no argument, because 125 test fits and user loops pay about 1 s each; falsified by a check cost under 0.1 s at p = 16.
- 2026-10-28: plan gate chose a separate note over a sixth boundary marker, because flagged fits were not covered worse; falsified by a coverage study where flagged fits mis-cover more than unflagged ones.
- 2026-10-28: plan gate chose the default model only over all models, because M170 measured W there only; falsified by a study of W's cost and misses on the other models.
- 2026-10-28: Fable review of the plan not requested. The question tool holds four questions, so the agent decided this one: the estimate does not change, D-072 already settled the statistics at Fable tier, and the oracles and review cover the wording.
- 2026-10-28: changelog: one NEWS.md New features entry, for the argument and the note.

## Decisions

## Review
