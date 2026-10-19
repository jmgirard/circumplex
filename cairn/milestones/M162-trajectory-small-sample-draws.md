# M162: Small-sample draws in ssm_trajectory()

- **Status:** planned
- **Priority:** normal
- **Depends on:** M161
- **Driving RR:** —
- **Principles touched:** IP1, IP2, IP3, IP4, IP6, GP2, GP4, GP5
- **Resolves:** —
- **Surface tier:** user-facing — an opt-in argument on an exported function, with its printed caution, help page, vignette section and NEWS entry
- **Branch/PR:** —

## Goal

`ssm_trajectory()` draws by the small-sample rule that M161 selected when the user asks for it, and its help page, the growth vignette and NEWS state the rule's validated range.

## Scope

**In:** one opt-in argument on the `coef` and `vcov` shape of `ssm_trajectory()`, its refusals, a third `input` attribute value with its printed caution, tests, the help page, Section 7 of the growth vignette and one NEWS entry. The argument's name and form follow the rule that the M161 decision entry names (RB tripwire: irreversible-api).

**Out:** a correction for the `draws` shape, which is used as given. Making the correction the default, which needs an input the helper cannot see. Any fitter or engine call (D-060, D-064). The "Peer-review status on SEM and growth help pages" candidate row stays parked unless the roxygen edit here takes it up.

## Acceptance criteria

- [ ] AC1: With the new argument left at its default, `ssm_trajectory()` returns an object `identical()` to the one the pre-milestone function returns at the same seed, with no new attribute. This holds for the help-page example and the two M152 fixture fits. A test compares against fixtures generated from the default branch before the change.
- [ ] AC2: With the argument given, `ssm_trajectory()` draws by the rule that the M161 D-entry names. A test runs at a fixed seed with `n_draws = 1e5` at two argument values, one of them the smallest that the rule allows. It checks that the 2.5% and 97.5% quantiles of e(t), x(t), y(t) and x(t) + y(t) at each of two times match the rule's closed-form quantiles. The test states its tolerance with the derivation. The asserting test cites the M161 record as the second oracle, of the simulation-coverage type.
- [ ] AC3: The argument refuses three inputs, each with a message that a test asserts: an `NA` or non-finite value, a value below the smallest that the M161 rule allows, and the argument given together with `draws`.
- [ ] AC4: A corrected trajectory records the correction as a value of the `input` attribute, beside `"coef_vcov"` and `"draws"`, and the `@return` text lists the third value. Printing it ends with a caution that names the correction and the gated range of persons from the M161 record. An `rbind()` of a corrected and an uncorrected table prints the not-recorded caution. Tests pin each caution's text.
- [ ] AC5: Under the corrected draws, tests assert three facts. `d_est` lies on the correct side of 0/360 at two times either side of a crossing. `d_lci > d_uci` at a time whose interval straddles 0/360. `certified` is FALSE at a flat (zero-amplitude) time.
- [ ] AC6: Three surfaces each state the correction, the M161 coverage result with its gated N range, and that `draws` input is not corrected. They are Section 7 of `vignettes/growth-ssm-analysis.Rmd.orig` with its rendered `.Rmd`, the `ssm_trajectory()` help page, and NEWS.md. A case-insensitive grep for `no shipped correction|ships no small` over `R/`, `man/`, `vignettes/`, NEWS.md and README.md has zero hits. In the same grep for `small.sample`, every hit states the correction, states or describes the caution for uncorrected intervals, or is unrelated.
- [ ] AC7: `devtools::check(args = "--no-manual")` reports 0 errors, 0 warnings and 0 notes, and every vignette renders.

## Coverage

- AC1 → T1, T2
- AC2 → T2, T3
- AC3 → T2, T3
- AC4 → T2, T3
- AC5 → T3
- AC6 → T4
- AC7 → T5

## Tasks

- [ ] T1: Before any code change, generate the default-path fixtures from the default branch (help example and the two M152 fixture fits, fixed seeds) with a committed generator script, and add the identity test.
- [ ] T2: Add the argument and the rule to `ssm_trajectory()` in `R/ssm_trajectory_helper.R` (draw step near line 161), its refusals, the new `input` value and its caution in `print.circumplex_ssm_trajectory()`, and the roxygen. Record in `## Decisions` that the third `input` value extends M155-D1's shape-dependent caution. Keep the default path's RNG use unchanged (IP4).
- [ ] T3: Tests in the trajectory test file: the closed-form quantile oracle at two argument values with the derived tolerance and the M161 citation, each refusal by message, each caution's text, the corrected and uncorrected `rbind()`, and the three boundary cases.
- [ ] T4: Rewrite Section 7 of `vignettes/growth-ssm-analysis.Rmd.orig` and re-render the `.Rmd`, then update the help page and add the NEWS entry. Run the AC6 greps and fix each hit. Grep the Section 5 and Section 10 prose, which cite Section 7, for claims that the change makes false.
- [ ] T5: Run `devtools::document()`, the test suite, the AC6 greps and `devtools::check(args = "--no-manual")`, and render every vignette.

## Work log

- 2026-10-18: created by /milestone-plan. tier check: milestone — a statistical method choice with a design decision and a coverage oracle, over more than one sitting.
- 2026-10-18: feature series: M161 -> M162. M162 carries the series' one NEWS entry. If M161 selects no rule other than the shipped normal draws, M162's goal is wrong and returns to `/milestone-plan`.
- 2026-10-18: criteria audit (full mode, fresh Opus reader): six findings fixed — AC2 now tests x(t) + y(t) at two argument values to separate a shared divisor from separate ones; AC3 lists its refusals; AC4 records the correction in `input` so a drop is all-or-nothing; AC5 names its assertions; AC6 counts three surfaces and catches the stale "no shipped correction" lines at `R/ssm_trajectory_helper.R:45` and in the help page.
- 2026-10-18: criteria re-audit (full mode, same fresh reader) returned three findings, all fixed: AC4 now requires the `@return` text to list the third `input` value, with the M155-D1 extension recorded at T2; AC3 refuses an `NA` value rather than a missing argument, which is the default path; AC6 adds a ledger category for prose that describes the uncorrected caution.
- 2026-10-18: plan gate chose an opt-in argument over a new default because the helper cannot know N from `coef` and `vcov`; falsified by a rule whose degrees of freedom are computable from `coef` and `vcov` alone.
- 2026-10-18: routed from M161's RR25 ingest (`cairn/reviews/archive/RR25-small-n-trajectory-draws.md`): rec 6, an argument giving the df (lower bound 1) rather than N, with the df = N - 1 mapping and its conditions in the help text, and the chi-square divisor drawn after the normals so AC1 holds; rec 7, the help text and Section 7 state the N = 10 boundary-fit rate from the M161 record; rec 9, one test at an `interval` other than .95. The argument name stays this milestone's irreversible-api choice.

## Decisions

## Review
