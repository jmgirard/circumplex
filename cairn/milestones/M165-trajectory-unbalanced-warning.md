# M165: ssm_trajectory() warns on a vcov from an unbalanced fit

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP4
- **Resolves:** —
- **Surface tier:** user-facing — a new warning from an exported function, with help-page, NEWS and vignette text
- **Branch/PR:** m165-trajectory-unbalanced-warning

## Goal

`ssm_trajectory()` warns when the structure of the `vcov` it is given shows
that the joint growth model was not fit to complete balanced data, the design
that its help page's small-sample coverage figures come from.

## Scope

**In:** For the `ssm_growth_formula()` model fit to complete balanced data,
the fixed-effect covariance has, for each coordinate `k` in `e`, `x`, `y`,
`-vcov["dvk", "dvk:<time>"] / vcov["dvk:<time>", "dvk:<time>"]` equal to the
mean design time, whatever the variance components are (RR25 "Beyond the
brief"; derivation and fixture check in the work log). When persons have
different numbers of observations, or under another random-effects
structure, the three values differ in general; missing waves that leave
every person the same count do not move them. The milestone adds that check to the
`coef` and `vcov` shape with the default contrast, with tests, help-page
text, a NEWS bullet, and one sentence in Section 7 of the growth vignette.

**Out:** A check for the `draws` shape, because the covariance of sampled
draws carries Monte Carlo error and cannot show exact agreement; no row is
owed. A check under a `contrast` function, which has no intercept and slope
pair to read; no row is owed. A small-sample correction for unbalanced fits,
which stays with the parked row "Small-sample correction for trajectory
draws". A print-time caution in place of the warning (rejected at the plan
gate; work log). A `vcov` with no dimnames, which the help page allows but
the default contrast already fails on with R's subscript error, goes to the
candidate row "Accept a vcov without dimnames"; the new check reads `vcov`
by position so it adds no second failing site.

## Acceptance criteria

- [ ] AC1: Called with `coef` and `vcov` and the default contrast,
      `ssm_trajectory()` reads, for `k` in `e`, `x`, `y`, the slope variance
      `s_k = vcov["dvk:<time>", "dvk:<time>"]` and `c_k = vcov["dvk",
      "dvk:<time>"]` (located through `names(coef)`), skips the check when
      any `s_k` is not positive, and otherwise computes `t_k = -c_k / s_k`
      and raises one warning when `max(t_k) - min(t_k) >
      sqrt(.Machine$double.eps) * max(1, max(abs(t_k)))`. The warning text
      names the three `t_k`, says that a fit of the `ssm_growth_formula()`
      model to complete balanced data gives three equal values, names
      persons with different numbers of observations and another
      random-effects structure as causes, and says that the help page's
      small-sample coverage figures come from complete balanced data. Tests
      fire it on a `vcov` built in closed form (the model's GLS covariance,
      summed over persons) for a design whose persons have different numbers
      of observations and unequal variance ratios across the coordinates, and
      on a closed-form balanced `vcov` with random slopes added, and assert
      that the message names the three values.
- [ ] AC2: Tests assert no warning from `ssm_trajectory()` on each of these
      inputs: the closed-form `vcov` of the model on complete balanced data
      at waves 0 to 4 and at the unequally spaced times 2000, 2001, 2003,
      2006; the closed-form `vcov` for the AC1 unequal-count design when the
      intercept covariance is exchangeable, `g * ((1 - r) * diag(3) + r)`
      with `r` not zero, and the residual variances are equal; the closed-form `vcov` for a design in which each person misses
      exactly one wave; the two glmmTMB fits in
      `tests/testthat/fixtures/growth-fixef.rds`; the `draws` shape; a
      `contrast` function; an all-zero `vcov`; and a `vcov` whose row and
      column for one slope are zero while its other entries are not.
- [ ] AC3: Live nlme fits of the `ssm_growth_formula("nlme")` model to
      `simulated_growth` raise no warning by REML, by ML, and by REML with
      `wave + 2000` as the time; the REML fit after a seeded removal of 10%
      of person-waves raises the AC1 warning. The test skips when nlme is
      not installed.
- [ ] AC4: The check consumes no random numbers: `.Random.seed` after a call
      that raises the AC1 warning equals `.Random.seed` after a call from the
      same seed with a balanced `vcov` of the same size and the same
      `n_draws`.
- [ ] AC5: The `ssm_trajectory()` help page states the AC1 condition in
      words, its two causes, that equal values do not show balance (named
      cases: each person missing the same number of waves, and equal
      variance components across the coordinates), and that the `draws`
      shape and a `contrast` function are not checked. NEWS.md's development section has one bullet
      for the warning. Section 7 of the growth vignette, in
      `vignettes/growth-ssm-analysis.Rmd.orig` and the shipped `.Rmd`, says
      in one sentence that the helper warns on such a fit, and
      `tools/prose-sweep.R` reports no finding on the added sentence.
- [ ] AC6: `devtools::test()` and `devtools::check(args = "--no-manual")`
      report 0 errors, 0 warnings, 0 notes.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T2
- AC4 → T1, T2
- AC5 → T4
- AC6 → T3, T5

## Tasks

- [x] T1: Tests first in `tests/testthat/test-ssm_trajectory_helper.R`: a
      test helper that builds the model's GLS `vcov` in closed form from a
      list of per-person wave vectors, a 3 by 3 intercept covariance, an
      optional random-slope covariance, and three residual variances (sum of `X_i' V_i^-1 X_i`, inverted, with the
      arithmetic in comments); the AC1 to AC4 tests. Run them and record
      which fail before the code exists (the warning tests must).
- [x] T2: Add the check to `R/ssm_trajectory_helper.R` after the joint-fit
      refusal (near line 251) and before the per-time draws loop, reading
      `vcov` only and locating entries by position through `names(coef)`.
      Record in the work log the spread `max(t_k) - min(t_k)` measured on
      each AC2 and AC3 input, beside the threshold.
- [ ] T3: Run the whole suite with warnings shown and list every existing
      test, example and vignette call that now warns. Change a hand-built
      test `vcov` only where its test does not depend on its intercept and
      slope covariances; otherwise assert the warning. Re-render nothing
      unless a vignette chunk warns.
- [ ] T4: Docs: the roxygen paragraph and `devtools::document()`; the NEWS
      bullet; the Section 7 sentence in both vignette files; run
      `tools/prose-sweep.R` on the vignette.
- [ ] T5: `devtools::test()` and `devtools::check(args = "--no-manual")`
      clean; record both results.

## Work log

- 2026-10-20: created by /milestone-plan.
- 2026-10-20: tier check: not run — work named at the question set.
- 2026-10-20: question set: which work to plan — the unbalanced-fit warning in `ssm_trajectory()` (the prose-scope question was moot).
- 2026-10-20: collision check: absorbs the candidate row "Unbalanced-fit check in ssm_trajectory()"; the parked row "Small-sample correction for trajectory draws" and D-066 stay as they are (no rule ships, and this check changes no interval); D-060 and D-064 hold, since the check reads `vcov` and fits nothing.
- 2026-10-20: inbox sweep: 0 open issues, 0 open PRs on jmgirard/circumplex.
- 2026-10-20: derivation: under balance, for person design `X_i = I_3 ⊗ [1, t]` and `V_i = G ⊗ 11' + diag(s2) ⊗ I`, `V_i X_i = X_i Q`, so `vcov = (G ⊗ e1 e1' + diag(s2) ⊗ (W'W)^-1) / N` and `-v01 / v11 = mean(t)` per coordinate for any G and s2.
- 2026-10-20: measured: the fixture's two glmmTMB fits give `t_k` within 3e-15 of 2 (waves 0 to 4); glmmTMB fits of `simulated_growth` with 10% and 30% of person-waves removed (seed 1) give spreads of 3.7e-4 and 1.2e-2.
- 2026-10-20: oracle types: closed-form (the GLS `vcov` built by explicit code), frozen (the glmmTMB fixture with its generator), live (nlme REML fits).
- 2026-10-20: plan gate chose a call-time `warning()` over a print-time caution stored as an attribute because the caution would thread a new attribute through `print()`, subsetting and `rbind()`, and the question-set option named a warning; falsified by users reporting the warning as noise on routine fits with missing waves.
- 2026-10-20: plan gate chose the relative threshold `sqrt(.Machine$double.eps) * max(1, max(abs(t_k)))` over an absolute threshold in time units because the time scale is the user's (waves, years, days); falsified by a balanced fit from glmmTMB or nlme whose spread exceeds it.
- 2026-10-20: plan gate scoped the check to the default contrast and the `coef` and `vcov` shape (Out items); falsified by a user model with other terms whose balanced fits have a readable centring time.
- 2026-10-20: lessons harvested: M56 family (sweep every surface; T3, AC5), M90 (name the path a figure measured; the work-log figures name glmmTMB and the seed), M7 family (`knitr` error default if a vignette re-renders).
- 2026-10-20: checkpoint: plan committed before the criteria audit returned; the audit's dispositions follow in a second plan commit.
- 2026-10-20: criteria audit (full mode, fresh Opus reader) returned 6 findings; all fixed in the criteria as below. The premise and the IP/GP reading held.
- 2026-10-20: audit F1 (AC2 equal-components claim false for correlated intercepts, spread about 6e-4): fixed, the case is now `g * diag(3)` with equal residual variances.
- 2026-10-20: audit F2 (the check sees unequal counts per person, not missing waves; one missing wave per person gives a spread of 4.4e-15): fixed in Scope, AC1's cause list, AC5's named cases, and a new AC2 probe.
- 2026-10-20: audit F3 (skip rule sat outside AC1, untested branch): fixed, the rule moved into AC1 and AC2 gained a one-zero-slope `vcov`; AC1 also gained the random-slopes probe the reader measured (t_k 0.67, 0.57, 1.82).
- 2026-10-20: audit F4 (engine probes fixed at waves 0 to 4, REML only): fixed, AC3 adds nlme ML and REML with `wave + 2000`.
- 2026-10-20: audit F5 (AC4's first clause compared two calls that both run the check): fixed, the clause was dropped.
- 2026-10-20: audit F6 (a `vcov` with no dimnames already fails under the default contrast, confirmed): out of scope, recorded as the candidate row "Accept a vcov without dimnames"; T2 reads `vcov` by position.
- 2026-10-20: substantive amendment: AC2's equal-components case changed from `g * diag(3)` to an exchangeable intercept covariance with `r` not zero, because a diagonal G makes the x(t)/y(t) covariance exactly zero and the joint-fit refusal errors first (measured); the deliverable the user sees is unchanged.
- 2026-10-20: re-audit: AC2 (full) — two findings, both answered without new wording: (1) the closed-form inputs reach the check only with a non-zero x-y intercept covariance, which T1's tests use (`G_unequal`, the exchangeable G), so AC2 is satisfiable as written; (2) "`r` not zero" admits non-PD values, where the claim still holds (r = -0.6 spread 1e-14), and the test pins g = 0.8, r = 0.3, s2 = 0.5.
- 2026-10-20: T1 done: the M165 tests are in `tests/testthat/test-ssm_trajectory_helper.R` with the closed-form `gls_vcov()` helper; before the code, the AC1 warning tests (unequal counts, random slopes), the AC4 seed test and the nlme 10%-removal test fail on the missing warning, and every no-warning case passes, nlme REML, ML and `wave + 2000` included.
- 2026-10-20: T2 done: the check sits after the joint-fit refusal in `R/ssm_trajectory_helper.R`, reads `vcov` by position through `names(coef)`, and the helper tests pass. Spreads measured against the threshold (scratch script, nlme 3.1 REML/ML, glmmTMB fixture): balanced closed form 8.9e-16 (waves 0-4) and 6.8e-13 (years, threshold 3.0e-5); exchangeable 1.8e-15; one missing wave per person 1.3e-15; fixture fits 2.7e-15 each; nlme REML 1.2e-14, ML 3.9e-14, `wave + 2000` 1.5e-10 (threshold 3.0e-5); unequal-count closed form 6.6e-4 and nlme with 10% of person-waves removed 1.7e-3 (thresholds near 3e-8).
- 2026-10-20: implement choice: the message gives the three times at 10 significant digits and the spread at 3, so values just over the threshold still print as different (M159 lesson on rounded values in messages).

## Decisions

## Review
