# M169: State which solution the CPM estimate is on the cpm_fit() help page

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP5
- **Resolves:** —
- **Surface tier:** user-facing — the `cpm_fit()` help page that users read
- **Branch/PR:** m169-cpm-estimate-help-page

## Goal

The `cpm_fit()` help page says which solution the CPM estimate is, and that a configuration with a lower discrepancy can exist, as D-071 decided.

## Scope

**In:** a new roxygen section on the `cpm_fit()` help page (`R/cpm_fit.R`, near the "Confidence intervals" section at about line 1546), with a test that shows the claim holds on one sample. A NEWS.md entry. A correction of DESIGN.md's "at small N" wording in the section "CPM engine estimate: what it is (D-071)", because RR27 Q1 found lower optima up to N = 5000.

**Out:** a printed note in `print()` or `summary()`, and any change to the engine or its estimate: M170 decides the estimator and whether a diagnostic ships, and its ship-step candidate row carries the change. Shares or other figures on the help page: M170 measures them, and they go on the page only with its ship step.

## Acceptance criteria

- [x] AC1: The `cpm_fit()` help page, built by `devtools::document()` from `R/cpm_fit.R` into `man/cpm_fit.Rd`, has a section that states four things. (a) For the free-angle models, the engine starts from the theoretical angles, their mirror image and four fixed angle offsets, with communality and weight starts computed from the data. For the fixed-angle models, it starts from three fixed communality rescalings. It reports the lowest discrepancy those runs reach: the maximum-likelihood solution in the basin they reach. (b) A configuration with a lower discrepancy can exist that these runs do not reach, for example with more scales at a communality of 1 or with scales in other positions. (c) The reproduction part of the convergence check and the "competing near-tied optima" note compare these runs only, so neither signals such a configuration. (d) Where one exists, the fit statistic is larger, so its p-value is smaller and RMSEA and CFI are no better, than at the lower discrepancy. The section states no share or other figure.
- [x] AC2: A test in `tests/testthat/` shows that claims (b) and (c) hold on one sample. The sample is simulated with `cpm_simulate()` at the boundary truth of `devel/m167-interval-oracle.R` (N = 250), with that truth rebuilt in the test. `cpm_fit(..., ci_method = "analytic")` returns its estimate with no convergence warning and without the "competing near-tied optima" marker. From one stated communality-wall start, `nlminb()` reaches a discrepancy lower than the reported one by more than 1 in T units (T = (N - 1) times the discrepancy).
- [x] AC3: NEWS.md's development-version section has an entry. It says that the `cpm_fit()` help page now states which solution the estimate is, and that a configuration with a lower discrepancy can exist.
- [x] AC4: `devtools::document()` leaves no diff. `devtools::test()` and `devtools::check(manual = TRUE)` finish with 0 errors and 0 warnings.

## Coverage

- AC1 → T3
- AC2 → T1, T2
- AC3 → T4
- AC4 → T3, T4

## Tasks

- [x] T1: Pick the pinned sample. Refit RR27's boundary N = 250 cases (M167 raw-data seeds, replicates 11, 21, 26, 28, 30, 31) with `ci_method = "analytic"`. Choose a seed where one communality-wall start (one scale's communality start at logit .99, from the engine optimum) reaches a lower point. Both the gap and the highest F on the segment between the two points must be above 1 T unit. Measure the gap from the committed construction, not from RR27's figures.
- [x] T2: Write the test in `tests/testthat/test-cpm_boundary.R`. Rebuild the boundary truth inline, simulate the sample at the stated seed, and assert the fit's acceptance, the absent marker and the gap. Mark it `skip_on_cran()`, because the basin `nlminb()` lands in can differ by platform. Keep its run time under 2 s.
- [x] T3: Add the roxygen section to `cpm_fit()` with ASCII text only, then run `devtools::document()`. Correct DESIGN.md's "At small N" sentence in place, marked `corrected M169`.
- [x] T4: Add the NEWS.md entry. Run `devtools::test()` and `devtools::check(manual = TRUE)` (TinyTeX on PATH).

## Work log

- 2026-10-26: created by /milestone-plan from the candidate row "State and settle the CPM engine estimate", part (i). Part (ii) is M170.
- 2026-10-26: tier check: milestone — the request includes a design decision (part ii, M170), so the whole request is a milestone.
- 2026-10-26: idea vet: one concern, that the row's "a lower discrepancy can exist at small N" is narrower than RR27 Q1, which found lower optima up to N = 5000. The plan drops the "small N" limit in AC1 and corrects DESIGN.md (decided by the agent as criteria wording). The study-shape concern belongs to M170.
- 2026-10-26: lessons used: 2026-08-16 (M90) for measuring the gap from the committed construction. 2026-10-26 (M167) on theory-centred starts missing Heywood-wall basins.
- 2026-10-26: criteria audit (full mode, fresh Opus reader): 7 findings on M169, all accepted. AC1(d) now says RMSEA and CFI are "no better", because they floor at 0 and cap at 1 (R/cpm_fit.R:1085, 1097). AC1(a) now names the fixed-angle start set and the data-based communality starts. AC1(c) now names only the reproduction part of the check. AC2 now promises that the test shows the claim, uses analytic CIs and a gap above 1 T unit, and is skipped on CRAN for platform basin drift. AC4 now uses the manual check, because roxygen prose can break the PDF manual.
- 2026-10-26: plan gate chose a CRAN skip for the AC2 test over a CRAN-run test with a loose tolerance, because the basin `nlminb()` reaches can change by platform and a tolerance cannot cover a different basin. Falsified by an arm64 or win-builder run in which the pinned sample reaches the same basin.
- 2026-10-26: question set: the user asked to plan without implementing yet. The run ends at the plan commit.
- 2026-10-26: implement started on branch m169-cpm-estimate-help-page, cut from master at f354e17b.
- 2026-10-26: T1 done: refit the six seeds (21261706 + replicate) with analytic CIs and ran one wall start per scale (that u_j at qlogis(.99), from the engine optimum, `cpm_optimize_one()`). Only replicate 30 (seed 21261736), wall start on HI, clears both bars: gap 1.963 T units (T 7.284 to 5.321), segment barrier 10.93 T units, gradient norm 6.7e-7 at the lower point; the fit is accepted with no warning and no multimodal marker. Its other seven wall starts return the engine's F (gap 0.000), so the gap check can fail. Replicate 11 HI/JK reached .75 T, replicate 21 DE/HI .65 T, below the bar.
- 2026-10-26: T2 done: test "a wall start reaches a lower F that the accepted fit does not signal" in test-cpm_boundary.R, skip_on_cran, file runs in 0.45 s. Planted control: the same test with the wall start on PA fails at the gap expectation. devtools::test(): 0 fail, 11 warnings (lavaan, same count on master), 1 skip.
- 2026-10-26: T3 done: section "Which solution is reported" placed before "Confidence intervals", each claim read against cpm_start_set(), cpm_start_values(), cpm_engine()'s free-scaling seed and multimodality loop, and cpm_fit_indices(). Agent choices: the fixed-angle start set is written as "one start and three fixed rescalings" (the base start plus factors .85, 1.1, .7), and the free-scaling unit-solution seed is named, because the code runs both. DESIGN.md's "At small N" sentence corrected from RR27 Q1's survey shares. document() also bumped DESCRIPTION's Config/roxygen2/version 8.1.0 to 8.1.1 (installed roxygen); committed so AC4's no-diff check holds.
- 2026-10-26: T4 done: NEWS.md entry under Documentation. document() leaves no diff. devtools::test(): 0 fail, 11 warnings (lavaan, as on master), 1 skip. devtools::check(manual = TRUE): 0 errors, 0 warnings, 0 notes (7m 31s).
- 2026-10-26: claim audit: 21 claims read, 1 corrected — tests/testthat/test-cpm_boundary.R (section header said "Heywood wall"; the lower point is interior, so it now says "Heywood-wall start"). The reader also noted that after a weight polish the reported F can differ from the lowest run's by up to 1e-8; the help wording is kept, because 1e-8 is the engine's own reproduction tolerance.
- 2026-10-26: status review; handed to /milestone-review.
- 2026-10-26: review pass 1 checkpoint: AC1 to AC3 evidenced and ticked, four lenses settled (16 fix now, 16 reject, 1 follow-up, with duplicates), fixes in 10558935 and 9ad37191; AC4's test and manual check on 9ad37191 still running.
- 2026-10-26: AC4 evidenced on 9ad37191 and ticked. The checkpoint line above miscounts: the dispositions are 17 fix now, 16 reject, 1 follow-up (34 lines, duplicates included).

## Decisions

## Review

Review pass 1, 2026-10-26, evidence on the branch head after the review fixes. Default branch unmoved since the cut (f354e17b).

- AC1 evidence: `man/cpm_fit.Rd` section "Which solution is reported", built by `devtools::document()` from `R/cpm_fit.R`. (a) free-angle starts: theory angles, mirror about the reference, four 15/30-degree offset patterns; fixed-angle: one start and three rescalings of its communality indices; communality-index and weight starts computed from the sample correlation matrix; "the maximum-likelihood solution in the basin they reach". (b) "A configuration with a lower discrepancy can exist that these runs do not reach, for example with more scales at or near a communality index of 1 or with scales in other positions". (c) the reproduction part (glossed) and the "competing near-tied optima" note "compare these runs only, so neither signals such a configuration". (d) chi-square larger; "At the same degrees of freedom" p smaller, RMSEA and CFI no better. The qualifier states (d) for configurations of the reported model, which share its df; it is needed because the harmonic polish can report a reduced model with more df. The section's only numbers are the start specification of (a) (four, three, 15- and 30-degree) and "at least two"; no share or measured figure. Pass.
- AC2 evidence: `tests/testthat/test-cpm_boundary.R` test "a wall start reaches a lower F that the accepted fit does not signal": truth rebuilt inline, `cpm_simulate()` at N = 250, seed 21261736 (replicate 30 of devel/m167-interval-oracle.R), `ci_method = "analytic"`; `expect_no_warning`, accepted TRUE, multimodal marker absent; HI wall start via `cpm_optimize_one()` (nlminb) gives a gap of 1.963 > 1 T unit. `NOT_CRAN=true` run of the file: 18 pass, 0 fail. Planted defects in the code under test fail it: `cpm_reproduced()` returning FALSE, `cpm_boundary_markers()` returning the near-tied label; a PA wall start fails the gap. Pass.
- AC3 evidence: NEWS.md development version, Documentation: "`?cpm_fit` has a new section, "Which solution is reported"" ... estimate is the ML solution in the basin its starts reach; "A configuration with a lower discrepancy can exist." Pass.
- AC4 evidence, on 9ad37191: `document()` (cli.width 500) leaves no diff and prints 0 `resolve link` lines; `devtools::test()` 0 fail, 15292 pass, 11 warnings (lavaan, same count on master), 1 skip; `devtools::check(manual = TRUE)` Status OK, 0 errors, 0 warnings, 0 notes (8m 9s). Pass.

Consistency gate: `cairn_validate.py` all checks passed (exit 0). No principle changed, `cairn_impact` skipped. `document()` with cli.width 500: 0 `resolve link` lines, no diff. README.Rmd not in the diff. `pkgdown::check_pkgdown()`: no problems. NEWS entry present, no milestone numbers. No new top-level files. Master watches: R-CMD-check.yaml and test-coverage.yaml newest push runs success at cacb9a43 (later master commits are cairn-only, path-ignored). `check-master-red-alert.R`, `master-red-alert-dryrun.R`, `check-branch-protection.R`: exit 0.

spawned: diff-bug, blame-history, prior-review, ripple

- diff-bug #1: claim (d) holds only at equal df, since the polish can report a reduced model with more df — fix now, fixed 10558935
- diff-bug #2: the new test has run only on the authoring Mac; platform basin drift could turn master red — reject (false): the PR touches R/ and tests/, so the step-8 CI wait runs it on Windows, macOS and Ubuntu release with NOT_CRAN true before the merge; devel and oldrel run on the post-merge push under the master-red alert
- diff-bug #3: "reproduction part" is defined nowhere a user reads — fix now (glossed), fixed 10558935
- diff-bug #4: "communalities" used for the communality index — fix now, fixed 10558935
- diff-bug #5: the harmonic polish is left out of "lowest discrepancy these runs reach" — fix now (parenthetical added, thresholds left out so the section states no figure), fixed 10558935, 9ad37191
- diff-bug #6: "at a communality of 1" is a limit, not a value — fix now ("at or near a communality index of 1"), fixed 10558935
- diff-bug #7: test comment restates the code and "logit .99" is ambiguous — fix now, fixed 10558935
- diff-bug #8: test comment carries worked seed arithmetic and ties platform drift to the pin — fix now, fixed 10558935
- diff-bug #9: the file header scopes the file to the free-scaling suite — fix now, fixed 10558935
- diff-bug #10: claims (a) and (d) have no test — fix now (start-battery size per model and index monotonicity tests, both unskipped; the free-scaling unit run is pinned by test-cpm_nesting.R), fixed 10558935
- diff-bug #11: `expect_true(accepted)` is implied by `expect_no_warning` — reject (false): the two read different observables, acceptance is the property AC2 names and the warning its signal
- diff-bug #12a: NEWS names the whole criterion, the help its reproduction part — reject (false): both statements hold
- diff-bug #12b: under free scaling the reproduction count excludes the unit seed — reject (false): the gloss now says it counts independent starts, which excludes the seed
- diff-bug #12c: "p-value is smaller" fails strictly under underflow or at df = 0 — reject (edge): df = 0 gives an NA p for a saturated model the engine already warns about
- diff-bug #12d: DESCRIPTION roxygen version bump out of scope — reject (planned change): AC4 requires `document()` to leave no diff
- diff-bug #12e: DESIGN.md:335 overlong line — fix now (reflowed with blame-history #1), fixed 10558935
- blame-history #1: DESIGN's section omits the unit-solution run under free scaling — fix now, fixed 10558935
- blame-history #2: the help does not say summary()'s analytic caution misses these basins — follow-up, added to part (ii) of the candidate row "State and settle the CPM engine estimate" (M170's row)
- blame-history #3: D-071 lacks an annotation for the new N facts; the N = 1000 split is thin — reject (false): a D-entry carries no derived measurements, and 44% and 33% Heywood at N = 1000 leave interior points the majority
- blame-history #4: `skip_on_cran()` keeps the regression off CRAN — reject (planned change): the plan gate chose the skip, with its falsifier logged
- blame-history #5: replicate 30's lower point is interior, so the wall example is untested — reject (planned change): AC2 binds the existence claim on one sample and T1's bars picked replicate 30
- blame-history #6: ROADMAP:19 still says "at small N" — fix now, by step 9 hygiene's narrowing of that row, which removes part (i)
- blame-history #7: DESCRIPTION pin moved — reject (planned change): AC4
- prior-review #1: the test is skipped in CRAN-flavor runs — reject (planned change): plan gate
- prior-review #2: no machine-independent assertion outside the gate — fix now: the start-battery and index tests run on CRAN; the basin check stays gated by the plan's choice, fixed 10558935
- prior-review #3: the planted control perturbed the input, not the code — fix now: defects planted in cpm_reproduced, cpm_boundary_markers, cpm_jitter_offsets_deg and cpm_fit_indices each fail their test (a run, no code change)
- prior-review #4: DESCRIPTION pin change — reject (planned change): AC4
- prior-review #5: LESSONS.md M167 line says "at small N" — fix now (corrected in place), fixed 10558935
- ripple #1: ROADMAP:19 "at small N" — fix now, as blame-history #6
- ripple #2: LESSONS.md:28 — fix now, as prior-review #5, fixed 10558935
- ripple #3: vignette gloss of the near-tied marker — reject (false): it claims near-tied solutions only, nothing about lower ones
- ripple #4: cpm_marker_labels comment — reject (false): still true
- ripple #5: "unambiguous multi-start winner" comment — reject (false): scoped to the multi-start set
- ripple #6: summary note text — reject (false): consistent with the diff
