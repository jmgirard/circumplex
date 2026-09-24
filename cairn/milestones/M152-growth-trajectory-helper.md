# M152: Growth output helper, coefficients or draws to a trajectory

- **Status:** review
- **Priority:** high
- **Depends on:** M151
- **Driving RR:** —
- **Principles touched:** IP3, GP2, GP4
- **Resolves:** —
- **Surface tier:** user-facing — a new export with print and plot methods
- **Branch/PR:** m152-growth-trajectory-helper

## Goal

Ship `ssm_trajectory()`. It turns a fitted joint model's fixed effects and covariance, or its posterior draws, into a certified trajectory table that prints and plots.

## Scope

**In:** `ssm_trajectory()` with two input shapes. Shape one is `coef` and `vcov`, from which it draws by `mvn_draws()` in `R/ssm_montecarlo.R`. Shape two is `draws`, a matrix of coefficient draws, the brms path. Both reach `ssm_draws(type = "parameters")` at each time. The object carries a `time` attribute, a print method with the certification marks and the small-N REML caution, and an `ssm_plot_trajectory()` method. The joint-fit refusal on an exactly-zero cross covariance. The default contrast from the M151 coefficient names and a `contrast` override. Two oracle types.

**Out:** the vignette rewrite and the devel recipe switch are M153. No engine is called (D-060, D-064). Degrees-of-freedom or Kenward-Roger corrections stay user-side, as the vignette's Section 7 states.

## Acceptance criteria

- [x] AC1: `ssm_trajectory(coef, vcov, times, draws = NULL, time = "wave", interval = 0.95, n_draws = 4000, contrast = NULL)` returns a data frame of class `circumplex_ssm_trajectory` with attribute `time` naming its time column. It has one row per `times`. Its columns are `<time>`, fifteen estimate and interval columns, and `certified`. The fifteen are `_est`, `_lci` and `_uci` for each of `e`, `x`, `y`, `a` and `d`. It refuses a non-symmetric `vcov` with a message naming the argument. Under the same seed its `a_*` and `d_*` columns match the pre-M153 growth vignette's hand-built per-wave loop with `expect_equal(tolerance = 1e-12)`. The test holds that loop verbatim as the reference. Both run on the M151 fixture `tests/testthat/fixtures/growth-fixef.rds`, so the test needs no glmmTMB.
- [x] AC2: With `draws` supplied and `coef` and `vcov` absent, the function skips the drawing step and summarizes the given matrix. A test draws a matrix with `mvn_draws()` and sets its column names to `names(coef)`. It asserts output identical to the shape-one path under the same seed. A call with `coef` alone, `vcov` alone, all three, or none stops with a message naming the arguments. Each of the four is tested.
- [x] AC3: Numeric oracles, two independent types. Closed form: the test uses a zero `vcov` (exempt from AC4) and coefficients whose trajectory crosses 0 and 360 degrees. At every time, `d_est` equals `atan2(y(t), x(t))` in degrees on [0, 360) within 1e-8. At every time, `a_est` equals `sqrt(x(t)^2 + y(t)^2)` within 1e-8. Linear interval: on the `simulated_growth` fixture with `n_draws = 2e5`, the `x_*` and `y_*` interval bounds match `est ± z * sqrt(L V t(L))` within 0.01 at every time. Boundary cases at `times = 0:4` under a stated seed: on the `simulated_growth` fixture every `d_est` lies in [0, 360). At least one interval straddles 0 and 360 degrees, and every straddling interval is stored with `d_lci > d_uci`. On the `simulated_growth_origin` fixture `certified` is `FALSE` at time 2 and `TRUE` at the other four times. A coefficient vector with zero `x` and `y` at every time and zero `vcov` returns `d_est` `NA` and `certified` `FALSE` without error.
- [x] AC4: The joint-fit refusal reads the derived per-time cross covariance `Cov(x(t), y(t)) = L_x V t(L_y)`. If it is exactly zero at every time in `times`, `ssm_trajectory()` stops with a message naming the joint-fit requirement. A `vcov` that is zero everywhere is exempt. The `draws` shape is exempt. A test asserts that draws with independent `x` and `y` columns pass. The help page says draws are not checked for a joint fit. The test plants exactly one nonzero entry in turn at each of the four cross positions. The positions are intercept by intercept, intercept by slope, slope by intercept and slope by slope. Each plant is mirrored across the diagonal, so `vcov` stays symmetric, and the test asserts no stop. The test also asserts the stop on the all-zero cross block. It asserts the stop on a custom contrast whose `x` and `y` rows share no coefficient with nonzero covariance.
- [x] AC5: The default contrast reads coefficient names of the form `dv<e|x|y>` and `dv<e|x|y>:<time>`. Those are the names the M151 formulas produce on `ssm_growth_data()`'s output. A `b_` prefix on `colnames(draws)` is stripped, and columns outside the six are ignored. So a raw brms draws matrix with `sd_`, `cor_` and `lp__` columns passes. If any of the six names is missing from `names(coef)` or the stripped `colnames(draws)`, the function stops with a message naming the missing coefficient. A test uses a brms-shaped matrix with the extra columns. A `contrast` argument (a function from one time value to a 3 by p matrix) overrides it. A test with a quadratic term and zero `vcov` recovers `x(t)` from the contrast within 1e-8.
- [x] AC6: `print()` of the object shows the table rounded to `digits`, marks each uncertified row, and prints the small-N REML caution line unconditionally. `ssm_plot_trajectory()` gains a method for the class that reads the `time` attribute. `ggplot2::layer_data()` of its plot is identical to the data.frame method's on the same table, and one vdiffr snapshot covers the new method.
- [ ] AC7: The verify slot is clean. `devtools::document()` produces no diff. NEWS.md names the export. `_pkgdown.yml` lists it. The help-page examples use a small inline `coef` and `vcov` and run without glmmTMB installed.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T2, T5
- AC6 → T6
- AC7 → T7

## Tasks

- [x] T1: Copy the vignette's hidden draw function and per-wave loop verbatim into `tests/testthat/test-ssm_trajectory_helper.R` as the reference. Write the AC1 shape, class and refusal tests first.
- [x] T2: Write `R/ssm_trajectory_helper.R`. Draw once from the full `coef` in its given order with `mvn_draws()`. Build the default contrast from the six names. Evaluate each time through `ssm_draws(type = "parameters")`. Attach the `time` attribute and the class.
- [x] T3: Add the `draws` shape and its argument checks.
- [x] T4: Add the closed-form, linear-interval and boundary tests. Pin the seed in the test file.
- [x] T5: Add the per-time cross-covariance refusal, its exemptions, the plant matrix and the custom-contrast tests.
- [x] T6: Add `print.circumplex_ssm_trajectory()` and `ssm_plot_trajectory.circumplex_ssm_trajectory()`. Add the `layer_data()` identity test and one vdiffr snapshot.
- [x] T7: Roxygen with inline examples, `devtools::document()`, NEWS entry, `_pkgdown.yml` entry. Run the verify slot.

## Work log

- 2026-09-24: created by /milestone-plan.
- 2026-09-24: criteria audit ran in full mode on an [O] reader. Findings absorbed here: the zero-covariance oracle collided with the refusal (now exempt). The refusal is defined on the derived per-time cross covariance. A second interval oracle and the `time` attribute were added. The plant matrix varies position and triangle. `layer_data()` replaced a two-snapshot comparison.
- 2026-09-24: a second audit pass on the changed criteria: `mvn_draws()` carries no column names, so AC2 now sets them. Partial argument combinations are each refused. The `draws` exemption gets a passing test and a help-page sentence. brms's `b_` prefix and extra columns are handled in AC5.
- 2026-09-24: plan gate chose an engine-agnostic input (`coef` and `vcov`, or `draws`) over a glmmTMB fit-object method. A method needs tests that skip without glmmTMB, against D-016, and breaks on an accessor rename. Falsified by a user report that the two extraction calls are where readers stop.
- 2026-09-24: /milestone-implement started; branch cut from the synced default branch. Question gate skipped: the plan fixes the signature, class, file names and dependency surface. Two routine calls made here: print `digits` defaults to 2 (the vignette's rounding), and `n_draws` is ignored when `draws` is given (documented on the argument).
- 2026-09-24: T1 to T7 done in one sitting, tests written before each part of the code. The reference in the test file is the vignette's `mvn_draw()` and per-wave loop verbatim, on the fixture in place of a live fit. The plant matrix uses a diagonal base so each of the four cross positions is the only nonzero cross entry. The time attribute survives `[` subsetting in R 4.x, so the plot method's missing-attribute guard is tested by removing the attribute. Verify slot: `devtools::test()` 14283 pass, 0 fail; `document()` no diff and no link warning; `pkgdown::check_pkgdown()` clean; the help-page examples run on base R alone.
- 2026-09-24: claim audit: 52 claims read, 3 corrected — NEWS.md (the all-zero `vcov` exemption was missing), R/ssm_trajectory_helper.R (`n_draws` is now unchecked on the draws path, as its text says), tests/testthat/test-ssm_trajectory_helper.R (the header names the function wrapping as a third difference from the vignette). The reader re-read all three once: each holds. Found in passing and fixed: a column subset that drops `certified` crashed print; it now prints unmarked, with a test. Noted, not changed: the caution names a fitted model's fixed-effect covariance, which does not describe draws-shape input; AC6 binds the line as unconditional.
- 2026-09-24: all tasks checked; `devtools::test()` after the corrections 14286 pass, 0 fail. Status set to review.

## Decisions

## Review

_2026-09-24, /milestone-review, branch at 6ab5862e; default branch unmoved since the cut (no PR yet)._

- AC1: `tests/testthat/test-ssm_trajectory_helper.R` run with NOT_CRAN=true, 24 tests, 149 expectations, 0 failures. The shape test checks class, the `time` attribute, the 17 column names in order, one row per time, numeric (not `circumplex_degree`) columns; the loop test matches the vignette's `mvn_draw()` and per-wave loop (copied verbatim, with the function wrapping, seed and fixture named as the differences) to 1e-12 on both fixture fits; the non-symmetric `vcov` refusal names `vcov`. No glmmTMB is loaded by the file.
- AC2: the draws-shape test asserts `expect_identical()` against shape one under seed 7 with `mvn_draws()` columns named `names(coef)`, and that `.Random.seed` is unchanged. The four partial combinations (`coef` alone, `vcov` alone, all three, none) each stop with a message naming the arguments given.
- AC3: closed form on zero `vcov` with y crossing 0/360 between t = 1 and 2: `d_est` equals `atan2` in degrees on [0, 360) and `a_est` the norm, within 1e-8. Linear interval on `simulated_growth` at `n_draws = 2e5`, seed 20260716: `x_*` and `y_*` bounds match `est ± z·sqrt(L V Lᵀ)` within 0.01 at every time. Boundary at `times = 0:4`, seed 20260716: every `d_est` in [0, 360); wave 2 straddles with `d_lci > d_uci`; `simulated_growth_origin` gives `certified` FALSE at time 2 only; the zero-x-y, zero-`vcov` trajectory returns `d_est` NA and `certified` FALSE with warnings and no error.
- AC4: the zeroed x-y cross block of the fixture stops with the joint-fit message; the plant test puts one nonzero mirrored entry at each of the four cross positions on a diagonal base and asserts no stop, and the diagonal base alone stops; the all-zero `vcov` passes; independent-column draws pass; the block-diagonal custom-contrast case stops. Help page states draws are not checked for a joint fit (`man/ssm_trajectory.Rd`).
- AC5: brms-shaped matrix (`b_` prefix, `lp__`, `sd_`, `cor_`, `sigma_` columns, shuffled order) gives output identical to the six-column matrix; a missing name stops naming it for both shapes and for `time = "month"`; the quadratic custom contrast on zero `vcov` recovers `x(t)` within 1e-8.
- AC6: print test: five table rows with two decimals by default and four under `digits = 4`, the `*` mark on wave 2 only, the caution line in both the certified and uncertified prints. Plot method: `ggplot2::layer_data()` identical to the data.frame method's on `as.data.frame(x)` with `time = "wave"`, also under `drop_xy = TRUE`; one vdiffr snapshot `ssm-trajectory-object.svg` matches.

**Consistency gate.** `cairn_validate.py`: all checks passed, `release window` advisory quiet. No principle changed, so `cairn_impact` was skipped. Toolchain: `document()` no diff and zero `resolve link` lines; no generated file hand-edited (the diff's `NAMESPACE` and `man/` changes regenerate identically); README untouched; `pkgdown::check_pkgdown()` no problems; NEWS names `ssm_trajectory()` under New features; no new top-level file, so no `.Rbuildignore` entry owed. Master watches: the newest push run reaching a verdict on both `R-CMD-check.yaml` and `test-coverage.yaml` is 28bceed5 (2026-09-24), `success` on both. `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R` and `tools/check-branch-protection.R` all exit 0.
