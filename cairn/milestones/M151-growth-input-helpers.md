# M151: Growth input helpers, the long table and the formula builder

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3, GP4, GP5
- **Resolves:** —
- **Surface tier:** user-facing — two new exports that every growth-vignette reader calls
- **Branch/PR:** `m151-growth-input-helpers` — https://github.com/jmgirard/circumplex/pull/186

## Goal

Ship `ssm_growth_data()` and `ssm_growth_formula()`. A reader then builds the stacked long table and pastes the validated joint model's fit call for glmmTMB, nlme or brms without composing either.

## Scope

**In:** `ssm_growth_data(data, scales, angles = octants(), id, time)` returns the person-by-time-by-coordinate long table that the growth recipe fits. `ssm_growth_formula(engine, time, id)` returns the fixed joint model as the engine's formula pieces. Its print method shows the complete fit call and the line that extracts fixed effects and their covariance. Engines: `"glmmTMB"`, `"nlme"` and `"brms"`. nlme enters Suggests (D-064). Tests, help pages, NEWS and the pkgdown index.

**Out:** the output side, coefficients or draws to a trajectory object, is M152. The vignette rewrite is M153. No fitter ships, and no engine is called by package code (D-060, annotated by D-064). No covariates, polynomial time or alternative random-effects structures: the builder fixes the model the coverage oracle validated (`devel/m27-coverage-oracle.R`). Extensions are a candidate row.

## Acceptance criteria

- [x] AC1: `ssm_growth_data(data, scales, angles = octants(), id, time)` returns a data frame with four columns. They are `<id>` (factor), `<time>` (numeric), `dv` (factor with levels `e`, `x`, `y`) and `value`. Rows are ordered by input row, then `dv`. Each `value` equals the `Elev`, `Xval` or `Yval` that `ssm_parameters_id(data, scales, angles)` gives that input row. The test runs it on `simulated_growth`. `id` and `time` are column names only. The test also asserts five refusals, each with a message naming the offending argument. Two are an `id` name and a `time` name absent from `data`. Two are a `time` column of class `Date` and a character `time` column. One is an `NA` in `id` or `time`.
- [x] AC2: `ssm_growth_formula(engine = c("glmmTMB", "nlme", "brms"), time = "wave", id = "person")` returns a list of class `circumplex_growth_formula` whose elements are formula objects. For glmmTMB they are `formula`, `value ~ 0 + dv + dv:<time> + us(0 + dv | <id>)`, and `dispformula`, `~ 0 + dv`. For nlme they are `fixed`, `value ~ 0 + dv + dv:<time>`, `random`, `~ 0 + dv | <id>`, and `weights`, `~ 1 | dv`. For brms they are `formula`, `value ~ 0 + dv + dv:<time> + (0 + dv | <id>)`, and `sigma`, `sigma ~ 0 + dv`. A test with `time = "t"` and `id = "subject"` compares each deparsed element to these strings. An engine outside the three stops with a message naming the argument. The test needs no engine installed.
- [x] AC3: `print()` of the object shows the engine's complete fit call on the long table. For glmmTMB and nlme it also shows the line that extracts the fixed effects and their covariance for `ssm_trajectory()`. For brms it shows `draws <- as.matrix(fit)`, whose `b_` columns M152 AC5 accepts. A snapshot test pins the three prints. The test also holds a literal string copied once from the base commit's `vignettes/growth-ssm-analysis.Rmd.orig` fit chunk. After all whitespace is removed, the glmmTMB call in the print equals that string.
- [x] AC4: Cross-engine parity. A test under `skip_if_not_installed("nlme")` fits the nlme pieces on `ssm_growth_data(simulated_growth, PANO(), id = "person", time = "wave")` by REML. Its six fixed effects match the committed glmmTMB fixture `tests/testthat/fixtures/growth-fixef.rds` within 1e-6. Its fixed-effect covariance entries match within an absolute 1e-4 times the largest absolute entry. The bound sits above two things: the structural-zero entries, which are rounding noise in both engines, and the intercept entries, where the REML criterion is flat and the measured gap is about 7e-6 of the largest entry. The fixture holds, for `simulated_growth` and `simulated_growth_origin`, the full-precision `fixef()$cond`, the `vcov()$cond` with dimnames and the glmmTMB version. `data-raw/growth-fixef.R` writes it under a seed. The measured nlme gaps are recorded in the work log.
- [x] AC5: The verify slot is clean. `devtools::document()` produces no diff. NEWS.md names both exports. `_pkgdown.yml` lists both. DESCRIPTION lists nlme under Suggests. No evaluated example line in either help page contains `glmmTMB::`, `nlme::` or `brms::`, so the examples run with no engine.

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
- [x] T4: Write `data-raw/growth-fixef.R`. It fits the vignette's glmmTMB model on both datasets under `set.seed(20260716)` and saves the fixture. Record the glmmTMB version in the fixture and in the work log.
- [x] T5: Add the nlme parity test. Record the measured fixed-effect and covariance gaps in the work log.
- [x] T6: Add nlme to Suggests. Roxygen, `devtools::document()`, NEWS entry, `_pkgdown.yml` reference entries under the growth group. Run the verify slot.

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
- 2026-09-24: T4 done. `data-raw/growth-fixef.R` fits the joint model through the two new helpers on both datasets under seed 20260716 and writes `tests/testthat/fixtures/growth-fixef.rds`. The fixture holds `coef`, `vcov` with dimnames and the REML log-likelihood per dataset, the glmmTMB version and a provenance string. glmmTMB version at generation: 1.1.15.
- 2026-09-24: T5 measured (nlme 3.1-171 against the glmmTMB 1.1.15 fixture). Fixed effects: largest gap 1.3e-14 on `simulated_growth` and 4.2e-15 on `simulated_growth_origin`. REML log-likelihood: gaps 3.5e-8 and 6.7e-9. Covariance: largest gap 4.7e-9 against a largest entry of 6.9e-4 on `simulated_growth`, at the e-by-x intercept entry, a ratio of 6.9e-6; on the origin dataset 6.4e-6 at the e-by-e entry. AC4's bound of 1e-6 times the largest entry fails by a factor of seven. Refitting nlme at tighter tolerances moves the same entry by 8.6e-9, and a tightened glmmTMB refit reports false convergence, so the gap is optimizer precision where the REML criterion is flat, not a model difference. The person-level standard deviations and correlations from the two engines agree to about 1e-6 relative. The test is written at a proposed bound of 1e-4 times the largest entry and T5 stays open pending the AC4 amendment gate.
- 2026-09-24: T6 done. nlme added to Suggests. NEWS entry, pkgdown group "Growth Model Functions" with both exports, `pkgdown::check_pkgdown()` clean, `devtools::document()` with no unresolved link. Full suite: 0 failures, 14119 passes, the 12 warnings all from pre-existing lavaan tests. Neither help page's examples name an engine.
- 2026-09-24: amendment gate. The user chose to amend AC4's covariance bound from 1e-6 to 1e-4 times the largest absolute entry over dropping the clause or tightening the fits. The amended sentence states the two things the bound sits above.
- 2026-09-24: re-audit: AC4 (full) — the reader passed satisfiability, reachability, bounded promise and proportionality. It returned three findings. "near 1e-5" overstated the measured 6.9e-6 and 6.4e-6, fixed to "about 7e-6". "cross-coordinate intercept entries" misplaced the origin dataset's largest gap, which is the diagonal e-by-e entry, fixed to "intercept entries". Two pre-existing clauses, "writes it under a seed" and "gaps are recorded in the work log", bind an instrument and a process rather than the deliverable. They are outside the gated amendment and stay as written for review to weigh. The reader also noted the test runs on CRAN with no platform measurement behind the bound, with a factor of 14 headroom on this machine.
- 2026-09-24: T5 done at the amended bound.
- 2026-09-24: claim audit: 44 claims read, 10 corrected — R/ssm_growth_data.R, R/ssm_growth_formula.R, data-raw/growth-fixef.R, tests/testthat/test-ssm_growth_parity.R, tests/testthat/test-ssm_growth_formula.R, tests/testthat/test-ssm_growth_data.R, and the two man pages. The two incorrect claims were a help-page reference to `ssm_trajectory()`, which M152 has not shipped, and the fixture header's claim that the seed pins an optimizer start, where glmmTMB draws none and the fixture is identical under any seed. The corrections also added refusals to `ssm_growth_data()` for `id` or `time` equal to `dv`, `value` or each other, a named `scales`/`angles` length check, and a numeric `value` on zero rows. The reader re-read the ten once and found all corrected. It noted one new vague phrase, "coefficient draws" for brms, fixed to "posterior draws" without a further read under the one-pass rule.
- 2026-09-24: all tasks checked. Full suite after the corrections: 0 failures, 14126 passes, 1 skip, 12 warnings from pre-existing lavaan tests. `devtools::document()` produces no diff. Status set to review.
- 2026-09-24: step-7 approval: m151-growth-input-helpers approved for merge.
- 2026-09-24: PR #186 CI red on the allowlist guard: nlme entered Suggests and the four workflow install allowlists did not list it. Added `any::nlme` to each; `tools/check-ci-deps.R` clean; pushed for a rerun.

## Decisions

## Review

Reviewed 2026-09-24 on `m151-growth-input-helpers` at dac43f02, master and origin/master both at the merge base 97ddcaf0, no PR open.

- AC1 evidence: `test-ssm_growth_data.R` ran green on its own, 41 expectations. A direct run on `simulated_growth` gave four columns of classes factor, numeric, factor and numeric. Its `value` column equals the row-wise `(Elev, Xval, Yval)` from `ssm_parameters_id()` with `id = NULL`. The tests assert the row-then-dv order, the angles pass-through by a 90-degree rotation, and the column-number refusals. The five named refusals are asserted with a message naming the argument: absent `id`, absent `time`, `Date` time, character time, and `NA` in `id` or `time`.
- AC2 evidence: `test-ssm_growth_formula.R` ran green on its own, 46 expectations. Its snapshot test skips under `test_file()` without `NOT_CRAN` and runs in the full suite. With `time = "t"` and `id = "subject"`, each deparsed element equals the criterion's formula. The deparser writes a one-sided formula as `~0 + dv`, the same object as the criterion's `~ 0 + dv`. `"lme4"`, a length-two engine and a numeric engine each stop with a message naming `engine`. The file loads no engine.
- AC3 evidence: the three prints are pinned in `_snaps/ssm_growth_formula.md`. Each shows the complete fit call. glmmTMB and nlme add the `coef` and `vcov` extraction lines, and brms adds `draws <- as.matrix(fit)`. The test's literal equals the `fit` chunk at lines 142-147 of `vignettes/growth-ssm-analysis.Rmd.orig` on master, read with `git show`. The whitespace-stripped comparison passes.
- AC4 evidence: `test-ssm_growth_parity.R` ran green with nlme 3.1-171 installed, 14 expectations and no skip. Measured this pass against the glmmTMB 1.1.15 fixture on `simulated_growth`. Fixed-effect gap 1.3e-14 against the 1e-6 bound. Covariance gap 4.7e-9 against a largest entry of 6.9e-4, a ratio of 6.9e-6 against the 1e-4 bound. REML log-likelihood gap 3.5e-8. The fixture holds `coef` at full precision, `vcov` with the six-name dimnames and `glmmTMB_version` for both datasets. `data-raw/growth-fixef.R` calls `set.seed(20260716)`. The gaps are in the work log at T5.
- AC5 evidence: full suite 0 failures, 14126 passes, 1 skip, 12 warnings from pre-existing lavaan and certificate tests. `devtools::document()` at `cli.width = 500` left the tree clean with zero `resolve link` lines. NEWS.md names both exports. `_pkgdown.yml` lists both under "Growth Model Functions". DESCRIPTION has nlme under Suggests. Neither help page's examples section contains `glmmTMB::`, `nlme::` or `brms::`, read from the two Rd files.

Consistency gate: `cairn_validate.py` passed every check. No `DESIGN.md` principle changed, so `cairn_impact.py` was skipped. `devtools::check(args = "--no-manual")` gave 0 errors, 0 warnings, 0 notes in 10m55s. `pkgdown::check_pkgdown()` found no problems. README.md is newer than README.Rmd. Master watches: the newest push runs of `R-CMD-check.yaml` and `test-coverage.yaml` on master both concluded success at c7152b6 (M150). The master-red-alert audits and the branch-protection check all exited clean. No new top-level file needs an `.Rbuildignore` entry.

Independent review, three fresh-context lenses. The blame-history lens found no conflict with D-015, D-016, D-060, D-064 or the growth archives. The prior-review lens found no regression against the archived reviews, and the GitHub inline-comment probe returned empty. The diff-bug lens returned 15 ranked findings, listed here with the disposition proposed at the gate. None makes an acceptance criterion fail as written.

- O1: the printed nlme call fails on a long table with an `NA` value, because `nlme::lme()` defaults to `na.action = na.fail`. Confirmed: `PA[5] <- NA` gives "missing values in object". glmmTMB drops such rows silently. Proposed: fix now, add `na.action = na.omit` to the printed nlme call and say in the help page that every engine drops the `NA` rows.
- O2: `time` and `id` are pasted into formula text unchecked. Confirmed: `time = "wave + age"` adds a covariate, and a legal non-syntactic name such as `"my wave"` fails in `as.formula()` with a message naming no argument. Proposed: fix now, backtick a non-syntactic name in the pasted text, with tests for both cases.
- O3: the glmmTMB fixture is generated through the two helpers, so the parity test is not independent of the long table. Proposed: reject. AC1 pins the long table against `ssm_parameters_id()` on its own, and the parity test checks the model across engines, not the table.
- O4: the brms dialect is presented as the validated model, but its default priors and posterior intervals were never run by the coverage oracle. Proposed: fix now, one help-page sentence.
- O5: `expect_invisible(out <- print(gf))` passes for any return, because the assignment is invisible on its own. Proposed: fix now.
- O6: the three `stopifnot()` branches on `data`, `scales` and `angles` fire in no test, and `inherits(time_col, "Date")` is dead because `is.numeric()` on a `Date` is already `FALSE`. Proposed: fix now, three refusal tests and the dead clause removed.
- O7: `suppressWarnings()` around the scorer hides every warning it raises, not only the undefined-displacement one. Proposed: fix now, muffle that one message with `withCallingHandlers()`.
- O8: duplicate column names in `data` resolve silently to the first match, in the id, time and scale columns alike. Proposed: follow-up candidate row.
- O9: no guard refuses an `id` or `time` that is also a scale, scale numbers pointing at the id or time column, `Inf` in `time`, or duplicate person-by-time rows. Proposed: the same candidate row as O8.
- O10: the fixture's provenance records the glmmTMB version and not the TMB version it was built against. Proposed: fix now, add `TMB_version` to the fixture and regenerate it.
- O11: the parity test runs on CRAN with a bound measured on one platform, with 14-fold headroom on this machine. Proposed: noted. The CI matrix measures it across platforms at merge, and the work log records the headroom.
- O12: two expectations in `test-ssm_growth_data.R` restate the code's own expression while the per-dv checks beside them are independent. Proposed: reject, overlap only.
- O13: the printed lines assign to `coef` and `vcov`, masking the base functions of those names. Proposed: reject. R's call lookup skips non-function bindings, so `vcov(fit2)` still finds the function, and the trajectory helper's argument names are M152's to settle.
- O14: the NEWS entry lists fewer refusals than the code has and describes a test's internals. Proposed: fix now, list the refusals and narrow the test sentence to the behavior it enforces.
- O15: AC5 was unticked at the reviewer's read. Not a finding: its evidence is recorded above.
- S1 (blame-history lens): the NEWS entry describes a workflow the vignette shows only after M153. Proposed: noted, the deferral is the milestone's stated scope.

Gate triage, 2026-09-24: the maintainer accepted every proposed disposition. Fix-now landed on the branch as follows.
- O1: the printed nlme call sets `na.action = na.omit` and the snapshot is updated. The help pages say each engine drops `NA` rows. The call was run on a table with an `NA` row and used 2247 of 2250 rows.
- O2: `backtick_name()` wraps a non-syntactic `time` or `id`. Tests show `"wave + age"` is one column and `"my wave"` parses and reads the long table.
- O4: one help-page sentence on brms priors and intervals.
- O5: `withVisible()` under `capture.output()`.
- O6: three refusal tests, and the dead `Date` clause removed.
- O7: `withCallingHandlers()` muffles only the undefined-displacement message, with a pass-through test.
- O10: `TMB_version` 1.9.25 in the regenerated fixture, whose fits are identical to the prior one.
- O14: NEWS lists the reserved-name and equal-name refusals and states the nlme parity as a behavior.
- Follow-up: O8 and O9 filed as one ROADMAP candidate row. Rejected: O3, O12 and O13 for the reasons above. Noted: O11 and S1.

After the fix-now batch: full suite 0 failures, 14137 passes, 1 skip, 12 pre-existing warnings. `devtools::check(args = "--no-manual")` 0 errors, 0 warnings, 0 notes in 11m11s. `devtools::document()` left the tree clean. `cairn_validate.py` passed after the M147 tombstone row was pruned to keep the ROADMAP under its line cap.
