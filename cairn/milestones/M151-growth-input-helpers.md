# M151: Growth input helpers, the long table and the formula builder

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3, GP4, GP5
- **Resolves:** —
- **Surface tier:** user-facing — two new exports that every growth-vignette reader calls
- **Branch/PR:** `m151-growth-input-helpers`

## Goal

Ship `ssm_growth_data()` and `ssm_growth_formula()`. A reader then builds the stacked long table and pastes the validated joint model's fit call for glmmTMB, nlme or brms without composing either.

## Scope

**In:** `ssm_growth_data(data, scales, angles = octants(), id, time)` returns the person-by-time-by-coordinate long table that the growth recipe fits. `ssm_growth_formula(engine, time, id)` returns the fixed joint model as the engine's formula pieces. Its print method shows the complete fit call and the line that extracts fixed effects and their covariance. Engines: `"glmmTMB"`, `"nlme"` and `"brms"`. nlme enters Suggests (D-064). Tests, help pages, NEWS and the pkgdown index.

**Out:** the output side, coefficients or draws to a trajectory object, is M152. The vignette rewrite is M153. No fitter ships, and no engine is called by package code (D-060, annotated by D-064). No covariates, polynomial time or alternative random-effects structures: the builder fixes the model the coverage oracle validated (`devel/m27-coverage-oracle.R`). Extensions are a candidate row.

## Acceptance criteria

- [ ] AC1: `ssm_growth_data(data, scales, angles = octants(), id, time)` returns a data frame with four columns. They are `<id>` (factor), `<time>` (numeric), `dv` (factor with levels `e`, `x`, `y`) and `value`. Rows are ordered by input row, then `dv`. Each `value` equals the `Elev`, `Xval` or `Yval` that `ssm_parameters_id(data, scales, angles)` gives that input row. The test runs it on `simulated_growth`. `id` and `time` are column names only. The test also asserts five refusals, each with a message naming the offending argument. Two are an `id` name and a `time` name absent from `data`. Two are a `time` column of class `Date` and a character `time` column. One is an `NA` in `id` or `time`.
- [ ] AC2: `ssm_growth_formula(engine = c("glmmTMB", "nlme", "brms"), time = "wave", id = "person")` returns a list of class `circumplex_growth_formula` whose elements are formula objects. For glmmTMB they are `formula`, `value ~ 0 + dv + dv:<time> + us(0 + dv | <id>)`, and `dispformula`, `~ 0 + dv`. For nlme they are `fixed`, `value ~ 0 + dv + dv:<time>`, `random`, `~ 0 + dv | <id>`, and `weights`, `~ 1 | dv`. For brms they are `formula`, `value ~ 0 + dv + dv:<time> + (0 + dv | <id>)`, and `sigma`, `sigma ~ 0 + dv`. A test with `time = "t"` and `id = "subject"` compares each deparsed element to these strings. An engine outside the three stops with a message naming the argument. The test needs no engine installed.
- [ ] AC3: `print()` of the object shows the engine's complete fit call on the long table. For glmmTMB and nlme it also shows the line that extracts the fixed effects and their covariance for `ssm_trajectory()`. For brms it shows `draws <- as.matrix(fit)`, whose `b_` columns M152 AC5 accepts. A snapshot test pins the three prints. The test also holds a literal string copied once from the base commit's `vignettes/growth-ssm-analysis.Rmd.orig` fit chunk. After all whitespace is removed, the glmmTMB call in the print equals that string.
- [ ] AC4: Cross-engine parity. A test under `skip_if_not_installed("nlme")` fits the nlme pieces on `ssm_growth_data(simulated_growth, PANO(), id = "person", time = "wave")` by REML. Its six fixed effects match the committed glmmTMB fixture `tests/testthat/fixtures/growth-fixef.rds` within 1e-6. Its fixed-effect covariance entries match within an absolute 1e-6 times the largest absolute entry, because the structural-zero entries are rounding noise in both engines. The fixture holds, for `simulated_growth` and `simulated_growth_origin`, the full-precision `fixef()$cond`, the `vcov()$cond` with dimnames and the glmmTMB version. `data-raw/growth-fixef.R` writes it under a seed. The measured nlme gaps are recorded in the work log.
- [ ] AC5: The verify slot is clean. `devtools::document()` produces no diff. NEWS.md names both exports. `_pkgdown.yml` lists both. DESCRIPTION lists nlme under Suggests. No evaluated example line in either help page contains `glmmTMB::`, `nlme::` or `brms::`, so the examples run with no engine.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4, T5
- AC5 → T6

## Tasks

- [x] T1: Write `tests/testthat/test-ssm_growth_data.R` first, then `R/ssm_growth_data.R`. Reuse `ssm_parameters_id()` with `id = NULL` and build the long table with base `rep()` indexing, not `reshape()`. Validate with `stopifnot()` and the `is_*()` helpers in `R/utils.R`.
- [x] T2: Write `tests/testthat/test-ssm_growth_formula.R` first, then the builder in `R/ssm_growth_formula.R`. Build each formula with `stats::reformulate()` or `as.formula()` on pasted text, with `time` and `id` substituted.
- [x] T3: Add `print.circumplex_growth_formula()`. Each engine's print shows the fit call and the extraction line. Add the snapshot test and the whitespace-collapsed comparison against the vignette's glmmTMB chunk.
- [ ] T4: Write `data-raw/growth-fixef.R`. It fits the vignette's glmmTMB model on both datasets under `set.seed(20260716)` and saves the fixture. Record the glmmTMB version in the fixture and in the work log.
- [ ] T5: Add the nlme parity test. Record the measured fixed-effect and covariance gaps in the work log.
- [ ] T6: Add nlme to Suggests. Roxygen, `devtools::document()`, NEWS entry, `_pkgdown.yml` reference entries under the growth group. Run the verify slot.

## Work log

- 2026-09-24: created by /milestone-plan.
- 2026-09-24: criteria audit ran in full mode on an [O] reader. It returned 18 findings across the first draft of this plan and M152's. Every finding had one repair, applied before the gate. This file's criteria absorbed the fixture-contents finding and the inline-example finding.
- 2026-09-24: plan gate chose input-side helpers plus a formula builder over a one-call fitter. A fitter must return the bare engine object, drags tests onto an optional engine, and overturns the twice-reviewed D-060. Falsified by three independent fitter requests, or one public misuse traceable to the pasted call.
- 2026-09-24: plan gate chose a fixed model with no design options over a flexible builder because each option is a model the coverage oracle never ran. Falsified by a user design the fixed model cannot express that the oracle later validates.
- 2026-09-24: a second audit pass on the changed criteria returned 12 findings. It tightened AC1's refusals, made AC3 compare a pinned literal, and made AC5 grep the examples. It set AC4's tolerances from a measured run: fixed effects agree to 1e-14, the REML log-likelihood is identical, and structural zeros differ only as rounding noise. Its measurement also showed D-016's nlme rationale was wrong, which D-064 records.
- 2026-09-24: the maintainer chose three engines in the builder over glmmTMB and nlme alone. brms's fit is never run by package code or tests (D-015), so its dialect is text, tested as text.
- 2026-09-24: implementation started on branch `m151-growth-input-helpers`. The question gate was skipped because the plan fixes every signature, element name and engine, and the nlme dependency was decided at D-064.
- 2026-09-24: T1 done. `ssm_growth_data()` scores through `ssm_parameters_id(id = NULL)` and stacks with `rep()` indexing. It suppresses the scorer's undefined-displacement warning because the long table carries no displacement. Tests cover the row-then-dv order, the value identity, the angles pass-through, the five refusals of AC1 plus column-number refusals, a flat row, an all-missing row, and zero rows.
- 2026-09-24: T2 done. `ssm_growth_formula()` builds each piece with `as.formula()` on pasted text, environment set to the global one. It carries `engine`, `time` and `id` as attributes for the print. It refuses `time` or `id` equal to `dv`, `value` or each other. With such a name the formula reads the wrong column without error. A test runs `model.matrix()` on the fixed part over `ssm_growth_data()` output. It gives the six coefficient names M152 reads.
- 2026-09-24: T3 done. The print deparses the stored formulas, so text and objects cannot disagree. The fit call binds `data = long`. The three prints are snapshotted, and the glmmTMB call matches the vignette chunk literal after whitespace removal. The two R files in T2 and T3 were appended by shell heredoc, against the tracking rule that repo edits go through Write and Edit. Later edits use those tools.

## Decisions

## Review
