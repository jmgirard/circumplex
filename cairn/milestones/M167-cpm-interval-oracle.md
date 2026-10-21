<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M167: Measure CPM interval coverage per weight and test boundary-safe intervals

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3, GP2, GP5, GP7
- **Resolves:** —
- **Surface tier:** user-facing — it corrects a vignette item and a help page section
- **Branch/PR:** m167-cpm-interval-oracle

## Goal

Measure where the percentile-bootstrap and Wald intervals of `cpm_fit()` leave the [.90, .98] band at the M4 truths, per component and miss direction, and score candidate interval families against a band fixed before the run, so that the docs say only what was measured.

## Scope

**In:**
- A seeded oracle script `devel/m167-interval-oracle.R`. Its header fixes the truths, cells, methods, replicate and resample counts, scoring rule, band, and Monte Carlo and multiplicity rule.
- The two M4 truths: p = 8 octants, ζ = .75, "boundary" β = .45/.35/.15/.05 and "interior" β = .35/.30/.20/.15.
- Raw-data cells at N = 250, 500 and 1000. Correlation-matrix and Wald cells at N = 250, 500, 1000, 2000, 5000 and 20000.
- Baselines: B1, the shipped percentile bootstrap, and B2, the shipped Wald interval.
- F1, a transformed Wald interval by the method of browne1982 (eqs. 1.6.30 to 1.6.32, p. 95). It uses ln v for each communality, with v = (1 − ζ²)/ζ² as CircE uses it, logit for each β weight, and the identity for angles.
- F2, a parametric bootstrap from the fitted Σ̂ at the fit's N, for correlation-matrix input, scored as a percentile interval and as a bias-corrected percentile interval.
- F3, a bias-corrected percentile interval on the B1 replicates, for ζ and β only.
- Added from RR26: a basic bootstrap on the B1 and F2 replicates, and P1, a profile-likelihood interval for ζ and β, both for ζ and β only; a variant-B refit (angles fixed at theory) of each raw-data replicate as a diagnostic arm, not an interval.
- The families are devel prototypes. Nothing is exported.
- A D-entry with the verdict. The DESIGN.md CPM coverage section, the β item of `vignettes/cpm-boundary-fits.Rmd` and the Confidence intervals section of the `cpm_fit()` help page are corrected.

**Out:**
- Shipping a family that passes goes to the "Boundary-regime interval improvement" candidate row, narrowed by this plan.
- Profile-likelihood intervals for angles, studentized-bootstrap intervals, and the non-normal and unequal-communality cells RR26 rated "consider" go to the same row.
- The free-scaling family goes to the same row's ship step. D-010 found its coverage regime is the diag family's.
- SSM amplitude near zero is a different mechanism. `ssm_ci_accuracy()` and the displacement certification rule (D-007) cover it.

## Acceptance criteria

- [x] AC1: `devel/m167-interval-results.rds` reports, for each (cell, method) pair that the header of `devel/m167-interval-oracle.R` lists, baselines included, the coverage of each free angle, each communality and each β weight the fit keeps. Each coverage entry is indexed by a single component. It comes with the share of misses on each side of the truth and the count of replicates it is conditional on. For angles the two sides are counterclockwise and clockwise, as the header defines them. The header's scoring rule says how a removed harmonic, a Heywood case and an NA standard error count.
- [x] AC2: Run under the M4 seeds, the pooled B1 and B2 coverage at the six M4 cells differs from the M4 table in `cairn/DESIGN.md` by at most 0.03 in every entry. Where an entry differs by more, `devel/m167-interval-oracle.md` names the engine change since M4 that explains it.
- [x] AC3: A DECISIONS.md entry states, for each candidate family the header lists, on each path it runs on and for each parameter family, whether every component's coverage lies inside the header's band at every cell the header lists for it, under the header's Monte Carlo and multiplicity rule. It names each (family, path, parameter family) that passed, or says that none passed. It applies the band and scoring rule that the header states at the results commit. It names the commit that fixed them, says that no full-run output existed at that commit, and lists each change made to them after the design review and before the first full run.
- [x] AC4: The DESIGN.md section "CPM confidence intervals: measured coverage (M4/B6 coverage oracle)" states, per truth and per β weight, the B1 coverage and miss split from the AC1 record, and names the weights outside the band or says that none is. It names the miss direction of the B1 communality shortfall at N = 250 and 500. Each sentence of its "Bootstrap default affirmed" bullet and of the "Correlation-function weights" item in `vignettes/cpm-boundary-fits.Rmd` states only what the AC1 or M4 record measured. The claims "at every sample size studied" and "a property of percentile intervals at a boundary" are restated as what the per-weight figures show, or removed.
- [x] AC5: The Confidence intervals section of the `cpm_fit()` help page states that, for `ci_method = "analytic"`, `summary()` prints its caution whenever N < 2000, and for 2000 <= N < 50000 when a boundary or weak-identification marker fires, matching `summary.circumplex_cpm()` and the constants `cpm_analytic_ci_n_caution` and `cpm_analytic_ci_n_boundary_caution` in `R/cpm_fit.R`. `devtools::document()` leaves no diff. `devtools::test()` reports 0 failures. For each (test file, `test_that()` description, message) triple in the `testthat_results` object that `devtools::test()` returns, a branch run holds no more warnings than a run of commit 1a5aa9c4 in the same R library. Two messages are the same when they match after each number is replaced by a placeholder, and a warning outside any `test_that()` has an empty description. `devtools::check(args = "--no-manual")` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T4
- AC2 → T2, T4
- AC3 → T1, T3, T5
- AC4 → T4, T5, T6
- AC5 → T6

## Tasks

- [x] T1: Write the header and skeleton of `devel/m167-interval-oracle.R` from the Scope above. The scoring rule covers a removed harmonic (β̂ = 0), a Heywood communality (ζ̂ = 1, where ln v is undefined), an NA standard error, an excluded fit, and the angle direction. The band is [.90, .98] per component. The Monte Carlo and multiplicity rule is fixed here, before any output. Fresh seeds are used except in the six M4 control cells. Smoke runs are never committed as results.
- [x] T2: Prototype F1 to F3. Make sure that the ζ arm of F1 reproduces the CircE intervals that `tests/testthat/test-cpm_oracles.R:136` reconstructs. Make sure that the F2 standard errors at an interior N = 2000 cell agree with that file's parametric-bootstrap cross-check. Make sure that F3 gives the textbook bias-corrected interval on a toy sample with a known answer. Run the six control cells (AC2). The 0.03 tolerance is about 1.5 Monte Carlo standard errors of a difference between two runs at 500 replicates. Commit the header.
- [x] T3: Write a review brief of the fixed design through `/milestone-brief` and ingest the Fable report. Put each change it asks for into the header before any full run, with a work-log line for each.
- [x] T4: Run the full study in the background (about 15 CPU-hours). Commit the results file and the record `devel/m167-interval-oracle.md`, which states the path each figure measured.
- [x] T5: Write the verdict D-entry and the DESIGN.md section. Set the promotion state of the narrowed candidate row from the verdict.
- [x] T6: Correct the vignette item and the `cpm_fit()` roxygen in ASCII-safe text, then run `document()`, `test()` and `check()`. Apply the rules in `cairn/references/plain-vignettes.md` to the vignette edit.

- [x] T7: Add a NEWS.md Documentation entry for the `cpm_fit()` help-page caution sentence and the `cpm-boundary-fits` vignette corrections (review return 1).

- [x] T8: Fix the harness (review return 2): P1's profile takes the lower of a start from the estimate and a warm start, with a Heywood-regime endpoint check added to the checks script; an error inside one method or one resample no longer drops the replicate; B2 gives [0, 0] for a removed weight; P1 handles a lone kept harmonic; the variant-B arm counts accepted fits only; the commit is read at start; the confirmation takes a non-colliding seed base, recorded in the block as a correction after the first full run.
- [ ] T9: Re-run the full study and the confirmation, then update the record, D-068, DESIGN.md and the candidate row from the new results.
- [ ] T10: Make the documentation and test fixes the Review section settles as fix now (code comments, `ci_method` text, vignette, NEWS, the threshold test, .gitignore), and file the follow-up candidate row.

## Work log

- 2026-10-21: created by /milestone-plan.
- 2026-10-21: tier check: milestone — a new statistical study with a design decision, over more than one sitting.
- 2026-10-21: idea vet: two concerns. (1) The M4 record pools the four β weights, and the interior truth also misses, so the near-zero weight is an unconfirmed cause; the question set chose to diagnose per weight first, then compare. (2) Users mostly read angles and communalities, and correlation-matrix input has only Wald intervals; the question set chose all three gaps.
- 2026-10-21: collision check: the "Boundary-regime interval improvement" row is absorbed in part (this study); the ship step and other families stay on it, narrowed, with the started token. M92 (done) taught boundary fits in the vignette and is distinct; AC4 corrects its β item. D-002 refuses BCa for SSM circular displacement and predates the CPM, so it does not reach the linear ζ and β; any ship step re-reads it before shipping F3. D-010 places the free family out. No planned or active milestone overlaps.
- 2026-10-21: inbox sweep: 0 open issues and 0 open PRs on jmgirard/circumplex.
- 2026-10-21: question set: a Fable review of the fixed design before the full run — granted (yes, before the run).
- 2026-10-21: criteria audit (full mode, fresh Opus reader): 15 findings, all fixed. AC1 takes (cell, method) pairs, names the baselines, scores only free components, defines angle sides and counts conditioning events. The control became AC2. AC3 binds the verdict's band, not the commit order, allows review changes before the run, and names every pass. AC4 states figures per weight and covers ζ. AC5 uses N with `ci_method = "analytic"` and adds check. Scope: F1 uses ln v so the CircE check holds; F3 changed from studentized to bias-corrected; F2 names its interval forms.
- 2026-10-21: plan gate chose a bias-corrected percentile for F3 over a studentized bootstrap, because the studentized form needs a per-resample Wald SE that is NA or very large at the 59-91% Heywood rates of N ≤ 500; falsified by a per-resample SE that stays finite in that regime.
- 2026-10-21: plan gate chose ln v for the ζ arm of F1 over logit ζ, because CircE's published intervals use ln v and a test already reconstructs them; falsified by logit ζ covering where ln v misses on the same replicates.
- 2026-10-21: plan gate chose a study-only milestone over study and ship together, because the export shape depends on which family passes; falsified by a verdict whose ship step changes no exported argument or printed output.
- 2026-10-21: lessons harvested: M161 (a conditioning event scored as a miss can decide a band, so T1 fixes how each counts) and M90 (name the path an oracle figure measured, for T4's record).
- 2026-10-21: implement started on branch m167-cpm-interval-oracle.
- 2026-10-21: checkpoint (T1, T2 in progress): `devel/m167-interval-oracle.R` header and skeleton, `devel/m167-interval-helpers.R` and `devel/m167-interval-checks.R` written; the checks pass 8 of 8 and two planted defects fail them; the control run and the suite are running.
- 2026-10-21: the suite at the branch point (no package code changed) reports 0 failures and 11 warnings from tests this milestone does not touch, so AC5's "0 warnings" cannot be met; AC5 goes to re-audit.
- 2026-10-21: re-audit: AC5 (full) — "0 warnings" replaced by "no more warnings per test than commit 1a5aa9c4"; the reader asked to name the commit, key on test file and description, bound the count, and mask numbers in messages.
- 2026-10-21: re-audit: AC5 (full) — on the wording with those fixes: "up to N = 50000" reads as inclusive while the code tests N < 50000; the thresholds live in `R/cpm_fit.R`, not `R/cpm_oop.R`; the warning comparison must name the `testthat_results` object and the number mask. This is the second re-audit of AC5, so the fixed text goes to the user.
- 2026-10-21: substantive amendment: AC5 takes the fixed text (the 2000 <= N < 50000 rule from `summary.circumplex_cpm()` and its constants, and no new warnings per test against commit 1a5aa9c4), accepted by the user at the stop.
- 2026-10-21: T1 done: the pre-registration block of `devel/m167-interval-oracle.R` runs from `# ==== PRE-REGISTRATION BEGIN ====` to `# ==== PRE-REGISTRATION END ====`; a control mode (B1 and B2 only, six raw-data cells) is a separate mode, not the full run.
- 2026-10-21: T2 done: `devel/m167-interval-checks.R` passes 8 of 8 (F1 zeta arm equals CircE on all seven rows, B1 copy identical to `cpm_fit()` on 9 of 9 interval sets, Wishart and row-draw F2 agree, analytic SE vs F2 SD ratios .95 to .98, bias-corrected closed form); planting logit zeta for ln v and a flipped z0 each fails its check.
- 2026-10-21: T2 control (`devel/m167-interval-control.rds`, 33 min on 17 cores): every pooled B1 and B2 entry of the six M4 cells lies within .020 of the M4 record (bar .03).
- 2026-10-21: T2 baseline finding, B1 per weight at the boundary truth: beta0 (.45) covers .66 to .74 with its misses below the truth, beta2 and beta3 .73 to .79 with misses above, beta1 .93 to .95; at the interior truth beta0 covers .81 to .84; so the shortfall is not carried by the near-zero weight alone. Goes into the review brief.
- 2026-10-21: blocked on RB26 (design review before the full run, granted at the plan question set). The brief is committed on this branch, as RB25 was on M161's branch, because the milestone's tracking lives here until merge.
- 2026-10-21: RR26 ingested (Fable, advisory): Decisions M167-D1 to D4; RB26 and RR26 moved to `cairn/reviews/archive/`; unblocked.
- 2026-10-21: substantive amendment: Scope In adds the basic bootstrap, P1 and the variant-B arm from RR26; Scope Out narrows the deferred profile-likelihood item to angles and adds RR26's two descriptive cells. The deliverable the user sees is unchanged (a study verdict and corrected docs), so no stop.
- 2026-10-21: T3 done: RR26 recs 1 to 4, 6 to 10 and 12 are in the pre-registration block of `devel/m167-interval-oracle.R`, listed under its "CHANGES AFTER THE DESIGN REVIEW" heading, and rec 11 is in the block and in M167-D4. Added `m167_basic()` and `m167_profile_ci()` (P1 follows the profile outward in growing steps, because warm starts carried from a far point gave spurious roots); per-replicate records are packed matrices saved with xz.
- 2026-10-21: T3 checks: `devel/m167-interval-checks.R` passes 11 of 11, adding the basic closed form, P1 within .00057 of Wald at interior N = 20000, and the statistic at 3.842 at P1's endpoints by a gradient-free re-profile; a planted chi-square(2) cutoff moves P1 .0105 from Wald and fails.
- 2026-10-21: T4 started: the full run (`Rscript devel/m167-interval-oracle.R`) runs in the background from commit 908ed678.
- 2026-10-21: T6 checkpoint (in progress): the `cpm_fit()` Confidence intervals section states the analytic caution's two thresholds and the five marker labels read from `cpm_marker_labels()`; a new test in `test-cpm_summary_markers.R` checks each side of N = 2000 and N = 50000 on a clean and a Heywood fit (8 expectations pass), and moving the 50000 constant to 60000 fails it. `document()` rewrote `Config/roxygen2/version`, which was reverted (M21-family lesson). The vignette item waits for the results.
- 2026-10-21: checkpoint (T4, T5, T6 in progress): the full run finished in 97.5 minutes (`devel/m167-interval-results.rds`, 4.0 MB); no family passes the strict verdict, the expected reading; on the ladder B1 angles and P1 communalities pass from N = 1000 on the raw-data path and F2bc weights from N = 5000 on the correlation-matrix path. Record draft `devel/m167-interval-oracle.md`; the DESIGN.md M4 section corrected per AC4 and RR26 rec 15; the vignette β item and the two "bias unmeasured" sentences corrected in both `.Rmd.orig` and `.Rmd` (prose sweep clean). The confirmation run and the AC5 baseline suite are running; the D-entry waits for the confirmation.
- 2026-10-21: T4 done: the confirmation run (44 minutes, `devel/m167-interval-confirm.rds`) confirms the N = 1000 passes (B1 angles, P1 communalities on both paths, F2bc communalities) and does not confirm B1 angles at N = 250; the record `devel/m167-interval-oracle.md` is complete. `git diff 908ed678 eaed2b22` over the oracle script and helpers is empty, so the header at the results commit is the one 908ed678 fixed.
- 2026-10-21: T5 done: D-068 records the verdict (strict: none; ladder: three passes), the fixing commit and the post-review changes, the scope, and the D-002 reading; the candidate row now names the three ladder passes and RR26's deferred items, so its promotion condition holds.
- 2026-10-21: AC5 baseline: `devtools::test()` at commit 1a5aa9c4 (scratch worktree, same R library) gives 0 failures and 11 warnings over 8 (file, test, masked message) triples.
- 2026-10-21: AC5 branch run: `devtools::test()` gives 0 failures and 11 warnings, no triple above its baseline count; `document()` with `cli.width = 500` prints no `resolve link` line and leaves no diff once `Config/roxygen2/version` is reverted. `check()` is running.
- 2026-10-21: claim audit: 84 claims read, 7 corrected — devel/m167-interval-oracle.md, vignettes/cpm-boundary-fits.Rmd, vignettes/cpm-boundary-fits.Rmd.orig (the failing B1 angle is LM, not JK; B2 and F1 pass beta_0 conditionally at boundary N = 250 and 1000; the full run matches the control at five of six cells; N = 1000 is the largest N for the shipped bootstrap only; beta_0 ties beta_3 at boundary N = 250; the marker-study sentence no longer contradicts the measured bias; the control bound reads .021). The same reader is re-reading the corrections.
- 2026-10-21: claim audit re-read: the corrections hold; it found two more, now fixed (the share with no interval at boundary N = 1000 is 16%, not 15%; beta_0's misses are nearly all below the truth, 0.5% above at interior N = 250, in the record, DESIGN.md and the vignette). No second pass, per the stopping rule.
- 2026-10-21: T6 done: `devtools::check(args = "--no-manual")` on 986d7be6 gives Status OK, 0 errors, 0 warnings, 0 notes. All tasks checked; status review, handed to `/milestone-review`.
- 2026-10-21: review return 1: consistency gate — NEWS.md has no Documentation entry for the user-visible changes (the `cpm_fit()` help page's caution sentence and the `cpm-boundary-fits` vignette's weight and bias corrections). The rest of the gate is green: `cairn_validate`, `document()` no diff, README untouched, `pkgdown::check_pkgdown()`, master R-CMD-check and test-coverage push runs success at c215fe31, the two alert audits and the branch-protection check exit 0.
- 2026-10-21: T7 added (minor amendment, review return 1) and done: two Documentation entries at the top of NEWS.md's development section, one for the `?cpm_fit` caution sentence and one for the vignette's weight and bias corrections, written against the help text and the vignette as they stand; the prose sweep reports no new long sentence in them. No test reads NEWS.md, and nothing else changed, so the suite and check runs on 986d7be6 stand. Status review.
- 2026-10-21: review return 2: diff-bug #1 — P1's root search returns false endpoints at Heywood communalities, so P1's figures and its ladder entry in D-068 are not established; with it, diff-bug #3 (a method error drops the replicate from every method) and #6 (B2's removed-harmonic rule) change scored figures, so the full study and the confirmation re-run after the fixes. The other fix-now dispositions in the Review section ride this return.
- 2026-10-21: T8, T9 and T10 added (minor amendment) for review return 2.
- 2026-10-21: T8 done: `m167_profile_setup()` and `m167_profile_stat()` split out of P1, each profile evaluation keeping the lower of the estimate start and the nearest warm start; refit errors count as nonconvergent and per-method errors leave only that method undefined; B2 gives [0, 0] for a removed weight; P1 gives beta_0 [1, 1] when it is the only kept harmonic; VB summaries read accepted fits only; the commit is read at start; the confirmation base is 30261022, recorded under a new "CORRECTIONS AFTER THE FIRST FULL RUN" heading in the block, which also records the correlation-cell base overlap left as it is. Checks pass 12 of 12; the new Heywood-regime check (175 endpoints, 14 on Heywood communalities) finds 0 low endpoints with the fixed P1 and 14 with the P1 of 46243f64.
- 2026-10-21: T9 started: the full study re-runs from b90b6ddd, then the confirmation, in the background.
- 2026-10-21: T10 checkpoint (DESIGN.md waits for the re-run): the threshold comments in `R/cpm_fit.R` and `R/cpm_oop.R` say the coverage was measured on fits with a defined SE; the `ci_method` text names the 50000 bound; the vignette scopes the "coverage, not bias" sentence to the marker study, says beta_2 and beta_3 missed mostly above, and adds that fixing the angles removed the bias; NEWS says "the package's bootstrap validation"; the threshold test runs on CRAN (7 analytic fits, under a second) and checks the clean fit at 50000 (9 expectations pass without NOT_CRAN); .gitignore names the confirmation file; new candidate row "Catch errors in CPM bootstrap refits" (diff-bug #3 and #13); the CPM simulation paper row notes its plan's superseded claims. `document()` regenerated `man/cpm_fit.Rd` and `Config/roxygen2/version` was reverted.

## Decisions

- M167-D1 (2026-10-21, RR26 Q1): the β shortfall is taken to come from an upward finite-sample bias in the per-scale communalities. That bias comes from the weakly identified angle and communality modes of variant A, and a Heywood case is that mode at the wall ζ = 1. The polish and the warm start are not the cause, and the softmax only moves the loss from β0 to β2 and β3. The bootstrap reproduces about 40% of the β0 bias at the boundary truth and 15% at the interior truth. The control's better Wald figure for β0 is conditional on a defined SE, which most Heywood fits lack, so "on the same fits" does not hold. The study tests this in-sample with a variant-B diagnostic arm, whose reading is fixed in the header.
- M167-D2 (2026-10-21, RR26 recommendations): applied 1 to 4, 6 to 12 and 15, which put per-replicate records, the variant-B arm, the basic bootstrap, P1, the "passes from N" ladder, a 5% conditioning limit, an allowance scaled to the realized count, a 5% vacuous-interval limit, the ln v wording fix, the expected outcome and a fresh-seed confirmation of any raw-path pass into the header before the first full run. Rec 5 (P1 for angles), 13 (a non-normal cell) and 14 (an unequal-communality cell) are deferred to the "Boundary-regime interval improvement" row, because the ship step needs them and this study's verdict does not. Rejects 16 to 21 are accepted as reasoned: no fresh seeds at the raw cells, 500 replicates, no studentized bootstrap, no softmax-scale Wald, F3 and the F2 bias-corrected form kept.
- M167-D3 (2026-10-21, RR26 Q2 and rec 21): D-002 does not reach the CPM's ζ and β, because its ground is circularity and these parameters are linear. Where a bias-corrected interval fails here, the record gives RR26's reason: the bootstrap does not see most of the bias.
- M167-D4 (2026-10-21, RR26 rec 11, the expected outcome, written before the run): the β0 shortfall is an estimator bias of about one SD at N <= 1000 that no re-centering of the present estimate removes. So "no family passes β on the raw-data path" is the expected reading, and the ship step reads the "passes from N" ladder.

## Review

Pass 2 (after review return 1), 2026-10-21, on c4be2db6; origin/master is an ancestor of the branch.

- AC1 evidence: a read of `devel/m167-interval-results.rds` finds all 12 header-listed cells; each cell's `by_method` holds exactly the methods the header lists for it (9 at raw-data cells, 6 at correlation-matrix cells); every (method, family) entry has one row per component (7 angles, 8 communalities, 4 weights, distinct component names) with `coverage`, `miss_low`, `miss_high` and `n`; F3, F2bc, Bb, F2b and P1 carry no angle entry, as the header says; 0 bad entries. The header's scoring rule covers the removed harmonic, the Heywood communality and the NA standard error (`devel/m167-interval-oracle.R` lines 86 to 97).
- AC2 evidence: the largest pooled B1 or B2 difference from the M4 record over the six cells is .0200 in both the control run and the full run's control table, under the .03 bar; no entry needs an engine-change explanation.
- AC3 evidence: D-068 states the per-(family, path, parameter family) verdict in both forms, names the three ladder passes and that no family passes in the strict form, applies the header at results commit eaed2b22, names 908ed678 as the fixing commit, says no full-run output existed then, and lists the post-review changes; `git diff 908ed678 eaed2b22 -- devel/m167-interval-oracle.R` is empty.
- AC4 evidence: the DESIGN.md section states the B1 per-weight coverage and miss split per truth (table and bullets), names the weights outside the band, and names the communality miss direction (above) at N = 250 and 500. The two claims AC4 names and "structural rather than small-sample" occur 0 times in DESIGN.md and both vignette files. Sentence ledger, "Bootstrap default affirmed" bullet: angles better pooled (M4 table); ζ not better pooled (M4 table); conditional on a defined SE, 13 to 73% lack it (M167 `se_defined_rate`); band fails (M4 and M167 tables); ζ per-scale ranges and above-side shares (M167 B1 zeta); weights outside the band (M167 B1 beta); β0 largest shortfall, tie at boundary N = 250, 0.5% above at interior N = 250 (M167 B1 beta); bias .65 to 1.05 SD, bootstrap shift 20 to 67%, variant B removes it (M167 `bias`); the scored families (M167 header). "Correlation-function weights" item: its first three sentences are M92's marker-study record, unchanged; β0 missed at least as often as any weight, almost every miss below, β2 and β3 above, no shrinking from 250 to 1000, both patterns, bias as the cause, bootstrap reproduces part (M167 record; the claim audit verified each).
- AC5 evidence: `man/cpm_fit.Rd` carries "for 2000 <= N < 50000" with the `ci_method = "analytic"` condition, matching `summary.circumplex_cpm()` (`R/cpm_oop.R:333-347`) and the two constants; `document()` with `cli.width = 500` printed 0 `resolve link` lines and left no diff; `devtools::test()` 0 failures, and no (file, test, masked message) triple exceeds its count at 1a5aa9c4 (8 triples, 11 warnings both runs); `devtools::check(args = "--no-manual")` on 986d7be6 Status OK, 0/0/0, and `git diff 986d7be6 HEAD` over R, tests, vignettes, man, DESCRIPTION, NAMESPACE and src is empty.
- Consistency gate, pass 2: `cairn_validate` 0 FAIL; `document()` 0 `resolve link` lines, no diff; NEWS.md carries the Documentation entries (T7); README untouched; `pkgdown::check_pkgdown()` no problems; master R-CMD-check and test-coverage push runs success at c215fe31; `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R` and `tools/check-branch-protection.R` exit 0; check clean (AC5). No principle text changed, so `cairn_impact` is skipped.
- spawned: diff-bug, blame-history, prior-review, ripple
- diff-bug #1: P1's root search returns false endpoints for Heywood communalities (statistic far below 3.84 at 54 of 551 endpoints at boundary N = 250), so P1's coverage, vacuous shares and ladder entry are not established — fix now; floor return 2 (the verdict rests on it).
- diff-bug #2: the confirmation seed base is 316 above the M4 base, so up to 184 of 500 confirmation replicates per cell repeat main-run data — fix now (non-colliding base, confirmation re-run, wording corrected).
- diff-bug #3: an error inside one method (F2's refit, a singular `solve()`) drops the whole replicate from every method, against the header's exclusion rule — fix now (per-resample and per-method error handling); the shipped `cpm_bootstrap()` has the same unguarded refit — follow-up (new candidate row).
- diff-bug #4: the ladder wording invites extrapolation past N = 1000 on the raw path and omits the N = 20000 failures — fix now (D-068, ROADMAP row).
- diff-bug #5: F2bc weights pass below .90, inside the band only through the allowance — fix now (D-068 and the row say so).
- diff-bug #6: B2 gives no interval for some removed harmonics, against the header's [0, 0] rule — fix now.
- diff-bug #7: P1 with only harmonic 0 kept leaves beta_0 at [0, 0] — fix now.
- diff-bug #8: the variant-B summaries include fits the engine did not accept — fix now (accepted fits only, count reported).
- diff-bug #9: the correlation-cell and confirmation seed bases differ by 1 — fix now (covered by #2's new base).
- diff-bug #10: DESIGN.md says variant B "removes it" without the reading's limits — fix now.
- diff-bug #11: the vignette says beta_2 and beta_3 missed above at both patterns, but interior beta_2 is in band at N = 250 — fix now.
- diff-bug #12: the results file records HEAD at save time — fix now (captured at start).
- diff-bug #13: the help page lists marker labels in prose with no test tying them to `cpm_marker_labels()` — follow-up (same new candidate row as #3).
- blame-history #1: the threshold comments in `R/cpm_fit.R` and `R/cpm_oop.R` call the analytic coverage measured without saying it was conditional on a defined SE — fix now.
- blame-history #2: the vignette names bias as the cause without the mechanism — fix now (adds that refitting with the angles fixed removed the bias).
- blame-history #3: the vignette's "bias was measured" overstates, since only beta_0's bias was measured — reject (false: the results file holds estimator bias for every angle and communality too).
- blame-history #4: D-002 is not annotated — reject (false: entries are append-only, and D-068's heading names its reading of D-002 as the rules require).
- blame-history #5: the `ci_method` parameter text omits the 50000 bound — fix now; NEWS lacks the analytic qualifier — reject (false: the entry says "analytic (Wald) intervals").
- blame-history #6: the new threshold test skips on CRAN — fix now (it runs analytic fits only, so the skip goes).
- blame-history #7: the .gitignore comment omits the confirmation file — fix now.
- prior-review #1: confirmation seeds not fresh — fix now (same as diff-bug #2).
- prior-review #2: the new test skips under `R CMD check` — fix now (same as blame-history #6).
- prior-review #3: the vignette and NEWS overstate the cause — reject (false: the beta_0 bias is measured in every raw-data cell, and the claim is about that bias, not the deeper mechanism).
- prior-review #4: NEWS calls it "a new validation study" while the vignette says "the package's bootstrap validation" — fix now.
- prior-review #5: the new test has no `skip_on_covr()`, and N >= 50000 is checked only on the Heywood fixture — reject the covr part (false: it asserts no optimizer-sensitive digits); fix now the clean-fixture check at 50000.
- ripple #1: the vignette's "measured interval coverage, not bias" sentence now contradicts the bias text — fix now (scoped to the marker study).
- ripple #2: `R/cpm_fit.R:1515` still says "below n = 2000 and in near-boundary fits" — fix now (same as blame-history #5).
- ripple #3: DESIGN.md's fragility line states the 59 to 91% Heywood rate without the free-angle qualifier — fix now.
- ripple (excluded, dated): the ROADMAP row's "beta ~.77 flat in N (percentile bootstrap)" — fix now (current knowledge); the CPM simulation paper plan's "structural percentile failure" claims — follow-up (noted in that parked row).
