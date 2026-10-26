# M169: State which solution the CPM estimate is on the cpm_fit() help page

- **Status:** in-progress
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

- [ ] AC1: The `cpm_fit()` help page, built by `devtools::document()` from `R/cpm_fit.R` into `man/cpm_fit.Rd`, has a section that states four things. (a) For the free-angle models, the engine starts from the theoretical angles, their mirror image and four fixed angle offsets, with communality and weight starts computed from the data. For the fixed-angle models, it starts from three fixed communality rescalings. It reports the lowest discrepancy those runs reach: the maximum-likelihood solution in the basin they reach. (b) A configuration with a lower discrepancy can exist that these runs do not reach, for example with more scales at a communality of 1 or with scales in other positions. (c) The reproduction part of the convergence check and the "competing near-tied optima" note compare these runs only, so neither signals such a configuration. (d) Where one exists, the fit statistic is larger, so its p-value is smaller and RMSEA and CFI are no better, than at the lower discrepancy. The section states no share or other figure.
- [ ] AC2: A test in `tests/testthat/` shows that claims (b) and (c) hold on one sample. The sample is simulated with `cpm_simulate()` at the boundary truth of `devel/m167-interval-oracle.R` (N = 250), with that truth rebuilt in the test. `cpm_fit(..., ci_method = "analytic")` returns its estimate with no convergence warning and without the "competing near-tied optima" marker. From one stated communality-wall start, `nlminb()` reaches a discrepancy lower than the reported one by more than 1 in T units (T = (N - 1) times the discrepancy).
- [ ] AC3: NEWS.md's development-version section has an entry. It says that the `cpm_fit()` help page now states which solution the estimate is, and that a configuration with a lower discrepancy can exist.
- [ ] AC4: `devtools::document()` leaves no diff. `devtools::test()` and `devtools::check(manual = TRUE)` finish with 0 errors and 0 warnings.

## Coverage

- AC1 → T3
- AC2 → T1, T2
- AC3 → T4
- AC4 → T3, T4

## Tasks

- [x] T1: Pick the pinned sample. Refit RR27's boundary N = 250 cases (M167 raw-data seeds, replicates 11, 21, 26, 28, 30, 31) with `ci_method = "analytic"`. Choose a seed where one communality-wall start (one scale's communality start at logit .99, from the engine optimum) reaches a lower point. Both the gap and the highest F on the segment between the two points must be above 1 T unit. Measure the gap from the committed construction, not from RR27's figures.
- [x] T2: Write the test in `tests/testthat/test-cpm_boundary.R`. Rebuild the boundary truth inline, simulate the sample at the stated seed, and assert the fit's acceptance, the absent marker and the gap. Mark it `skip_on_cran()`, because the basin `nlminb()` lands in can differ by platform. Keep its run time under 2 s.
- [ ] T3: Add the roxygen section to `cpm_fit()` with ASCII text only, then run `devtools::document()`. Correct DESIGN.md's "At small N" sentence in place, marked `corrected M169`.
- [ ] T4: Add the NEWS.md entry. Run `devtools::test()` and `devtools::check(manual = TRUE)` (TinyTeX on PATH).

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

## Decisions

## Review
