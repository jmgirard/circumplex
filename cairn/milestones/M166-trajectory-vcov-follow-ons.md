# M166: ssm_trajectory() reads vcov by position and scales its balance check

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, IP3
- **Resolves:** —
- **Surface tier:** user-facing — an exported function's input handling, warning and help page
- **Branch/PR:** m166-trajectory-vcov-follow-ons

## Goal

Under the default contrast, `ssm_trajectory()` reads `vcov` through the positions of `names(coef)` and judges balance against a time scale taken from `vcov` itself.

## Scope

**In:** the two candidate rows "Accept a vcov without dimnames" and "Scale the balance check to small times". (1) A `vcov` with no dimnames, which the help page allows ("when present"), stops failing at `vcov[needed, needed, drop = FALSE]` (`R/ssm_trajectory_helper.R:256-257`) with R's "no 'dimnames' attribute for array" error. (2) The unbalanced-fit threshold at `R/ssm_trajectory_helper.R:290`, `sqrt(.Machine$double.eps) * max(1, max(abs(t_k)))`, becomes `sqrt(.Machine$double.eps)` times the largest of `sqrt(v_int / v_slope)` over e, x and y, so the floor of 1 no longer hides an unbalanced fit whose times are all far below 1. (3) The cause lists in the warning, the help page and the NEWS bullet add fixed-effect terms other than the model's. The help page states the new scale and its limit.

**Out:** a balance check for the `draws` shape or a `contrast` function (M165's design, unchanged; no row). A `vcov` with no dimnames whose rows are in another order than `coef` cannot be detected and stays the user's contract (help page: one row and column per element of `coef`). A NEWS entry for the dimnames fix: `ssm_trajectory()` is unreleased, so the fix changes no released behavior. The small-sample draw correction stays its parked candidate row.

## Acceptance criteria

- [ ] AC1: Under the default contrast, `ssm_trajectory()` accepts a `vcov` with no dimnames and reads it through the positions of `names(coef)`. A test in `tests/testthat/test-ssm_trajectory_helper.R` builds a seven-element `coef` whose first entry is an extra coefficient, with a 7 by 7 dimnames-free `vcov` that holds a 6 by 6 model block in the six model positions, and shows three things: the returned table is `expect_identical()` to the one from the same call with `dimnames(vcov) <- list(names(coef), names(coef))` under the same seed; the joint-fit refusal fires, with its message, on a dimnames-free `vcov` with zero x-y covariance; and the unbalanced-fit warning fires on a dimnames-free `vcov` from the closed-form GLS oracle's unequal-counts design.
- [ ] AC2: The unbalanced-fit warning compares `max(t_k) - min(t_k)` with `sqrt(.Machine$double.eps)` times the largest, over e, x and y, of `sqrt(v_int / v_slope)`, the square root of the intercept variance divided by the slope variance in `vcov`. A test on the closed-form GLS covariance, over the grid of intercept covariance `G` multiplied by 0.01, 1 and 100 and design times multiplied by 1e-6, 1 and 1000, shows that the unequal-counts design warns and the complete balanced design does not in each of the nine cells. Every existing test in the file's M165 section keeps its outcome.
- [ ] AC3: The balance check is skipped with no condition when an intercept variance is not positive or the scale is not finite. A test passes a `vcov` with an intercept variance of -1e-12, which the eigenvalue tolerance of `trajectory_check_coef_vcov()` admits, under `expect_no_condition()`.
- [ ] AC4: The `?ssm_trajectory` details paragraph names the new scale in place of "the largest of 1 and their absolute values", says that a change of time units does not change whether the function warns, and says that a random-intercept variance large relative to the residual variance can let an unbalanced fit pass without a warning. A test backs the last clause: the unequal-counts design with `G` multiplied by 1000 does not warn.
- [ ] AC5: The warning message, the `?ssm_trajectory` details paragraph and the NEWS bullet for the warning each list fixed-effect terms other than the model's among the causes. A test shows the warning for a GLS oracle covariance on complete balanced data with three added fixed effects, a 0/1 person-level covariate times time on each of e, x and y, passed with all nine coefficients.
- [ ] AC6: `devtools::test()` passes with no failures, and `devtools::check(args = "--no-manual")` reports 0 errors, 0 warnings and 0 notes.

## Coverage

- AC1 → T1, T2
- AC2 → T3, T4
- AC3 → T3, T4
- AC4 → T3, T5
- AC5 → T3, T4, T5
- AC6 → T6

## Tasks

- [x] T1: Write the AC1 tests in the M165 section of `tests/testthat/test-ssm_trajectory_helper.R`: a helper that pads a 6 by 6 block into a 7 by 7 matrix behind an extra first coefficient, then the three checks. Run them on the unfixed code and record in the work log that each fails with R's "no 'dimnames' attribute for array" error.
- [x] T2: At `R/ssm_trajectory_helper.R:256-257`, read the joint-fit block as `vcov[pos, pos]` with `pos <- match(needed, names(coef))`, the indexing the balance check already uses. Search the function for any other name-indexed read of `vcov` and convert it. Make T1 green.
- [x] T3: Write the AC2 to AC5 tests. Extend `gls_vcov()` with an optional set of per-coordinate covariate-by-time columns and an optional time multiplier, leaving its present callers' output unchanged. The grid test covers nine cells; the AC3 test uses `diag(c(-1e-12, 1, 1, 1, 1, 1))` with x-y covariance 0.1; the AC4 test uses `G` times 1000; the AC5 test uses `z = rep(0:1, 10)` on 20 balanced persons. Record which tests fail on the old rule (the 1e-6 time cells; the plan probe measured spread 6.6e-10 against the old threshold 1.5e-8).
- [x] T4: At `R/ssm_trajectory_helper.R:281-291`, compute the scale from the three intercept variances and `s_k`, skip the check when an intercept variance is not positive or the scale is not finite, and compare the spread with `sqrt(.Machine$double.eps)` times that scale. Update the code comment and add fixed-effect terms other than the model's to the warning's cause list. Make T3 green.
- [x] T5: Rewrite the `?ssm_trajectory` details sentences on the threshold and the causes (AC4, AC5). The help page does not claim the threshold is unchanged by a shift of time. Edit the NEWS bullet at `NEWS.md:91-100`. Run `devtools::document()` and revert `Config/roxygen2/version` if it moves. Grep `R/`, `man/`, `vignettes/` and `NEWS.md` for `residual-variance` and `largest of 1`, and fix each stale hit.
- [x] T6: Run `devtools::test()` and `devtools::check(args = "--no-manual")`; record the counts.

## Work log

- 2026-10-20: created by /milestone-plan.
- 2026-10-20: tier check: not run — work named at the question set.
- 2026-10-20: question set: what to work on — both `ssm_trajectory()` candidate rows in one milestone; whether to run on — plan, then implement.
- 2026-10-20: collision sweep: absorbs the candidate rows "Accept a vcov without dimnames" and "Scale the balance check to small times" (both filed by M165); M165 (done) shipped the check this extends; D-064 read whole, no conflict; inbox 0 open issues, 0 open PRs.
- 2026-10-20: lessons harvested: guard numerics with `is.finite()` (M32 family) and NaN comparisons in `if()` (M148), both behind AC3; `check()` defaults `manual = FALSE` (M7 family), so AC6 does not stand in for the release check.
- 2026-10-20: plan probe (scratch GLS oracle): the old rule misses the unequal-counts design at time x 1e-6; the new rule warns in all nine cells of AC2's grid with balanced silent, and misses at G x 1000; a 0/1 covariate times time on balanced data gives spread 0.086.
- 2026-10-20: criteria audit (full mode, fresh Opus reader): 6 findings, all fixed. (1) NaN from a negative intercept variance → AC3 skip and test. (2) the probes varied only time scale, and the rule loses detection as G/s2 grows → AC2 grid adds the G axis, AC4 states and tests the limit, no shift-equivariance claim. (3) existing tests were under-listed → "every existing test in the M165 section". (4) a centred covariate does not move the times → AC5 fixes a 0/1 covariate per coordinate. (5) AC3's grep was the promise → AC5 names the three texts, the grep moves to T5. (6) AC1's fixture needs a padded 7 by 7 `vcov` → stated in AC1.
- 2026-10-20: plan gate chose the scale `sqrt(v_int / v_slope)` over keeping the floor of 1 (misses unbalanced fits in small time units) and over the smaller of the old and new thresholds (where intercept variance is large, fit error in `t_k` scales with `sqrt(v_int / v_slope)`, so the old threshold risks false warnings there); falsified by a balanced fit from glmmTMB or nlme that warns under the new rule, or an unbalanced fit with a realistic variance ratio that passes silently.
- 2026-10-20: plan gate chose reading `vcov` by position over refusing a dimnames-free `vcov` with a message, because the help page already accepts one; falsified by a user report of a reordered dimnames-free `vcov` giving a wrong table.
- 2026-10-20: Fable escalation considered (the audit reader suggested it for the threshold) and not offered: no RB tripwire, the trade-off is measured on the oracle grid and stated on the help page. Implement tier: Opus. IP6 does not apply (no displacement, contrast or `src/` change).
- 2026-10-20: implement started on branch m166-trajectory-vcov-follow-ons, cut from master at 7ac277fe.
- 2026-10-20: T1: three tests appended (new section "A vcov without dimnames (M166)", `pad_vcov()` helper, `coef7` with `age` first); on the unfixed code all three failed with `Error in vcov[needed, needed, drop = FALSE]: no 'dimnames' attribute for array`.
- 2026-10-20: T2: the joint-fit block reads `vcov[pos, pos]` with `pos <- match(needed, names(coef))`, computed once and shared with the balance check; no other name-indexed `vcov` read in R/. T1 green; `devtools::test()` FAIL 0, PASS 15242.
- 2026-10-20: T3: `gls_vcov()` gains `z` (three per-coordinate covariate-by-time columns named `z:<slope>`); the time multiplier is applied to the designs in the test rather than as a `gls_vcov()` argument (minor deviation, same coverage). Four tests added. On the old rule six expectations failed: the three time x 1e-6 unequal-counts cells (no warning), the subnormal-slope case and the G x 1000 case (old rule warned), and the AC5 message regex; the -1e-12 intercept case passed on the old rule and guards the new one.
- 2026-10-20: T4: the check reads `v_k`, the three intercept variances, skips unless all `s_k` and `v_k` are positive, and compares the spread with `sqrt(.Machine$double.eps) * sqrt(max(v_k / s_k))`, skipping a non-finite scale; the warning's cause list adds "fixed effects beyond that model's six", keeping the "random-effects structure" wording the M165 regex reads. The trajectory helper file passes in full.
- 2026-10-20: T5: `?ssm_trajectory` details now give the scale, the time-units sentence, the large-intercept-variance limit, the added cause and the intercept-variance skip; the NEWS bullet adds the cause. `devtools::document()` rewrote only `man/ssm_trajectory.Rd`; `Config/roxygen2/version` reverted. Sweep of `R/`, `man/`, `vignettes/`, `NEWS.md` for `residual-variance` (5 hits) and `largest of 1` (0 hits): every hit names the added cause; the growth vignette's one sentence on the warning gives a hedged example, not a cause list, and is left. The simple-english hook flagged 435 issues across NEWS.md, almost all older than this branch; out of scope, left.
- 2026-10-20: T6: at 2ef15a36 `devtools::test()` FAIL 0, WARN 11 (lavaan), SKIP 1, PASS 15268; `devtools::check(args = "--no-manual")` 0 errors, 0 warnings, 0 notes (7m 50s).
- 2026-10-20: claim audit: 24 claims read, 2 corrected — R/ssm_trajectory_helper.R (help omitted the overflow skip), tests/testthat/test-ssm_trajectory_helper.R (grid test name said "every variance scale"); also tightened "grows with the random-intercept variance" to "relative to the residual variance". Docs regenerated; the trajectory helper test file passes after the edits.
