<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M172: Exclude a bootstrap refit that errors in cpm_fit()

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP4, GP2
- **Resolves:** —
- **Surface tier:** user-facing — changes what `cpm_fit()` returns when a bootstrap refit errors, and two help pages
- **Branch/PR:** m172-cpm-bootstrap-refit-errors

## Goal

A bootstrap refit that stops with an error in `cpm_fit()` drops its resample with a counted warning instead of ending the call.

## Scope

**In:** an error handler around each bootstrap refit's estimation and
acceptance steps in `cpm_bootstrap()` (`R/cpm_fit.R`), which adds the resample
to the nonconvergent count (`details$boots_nonconvergent`) that the exclusion
warning and the `print()` note report; the `cpm_fit()` help sentence on
excluded refits; the marker lists on the `cpm_fit()` and
`summary.circumplex_cpm()` help pages written with the labels `summary()`
prints, and a test tying them; a NEWS entry; line-number cites into
`R/cpm_fit.R` and `R/cpm_oop.R` replaced by function names across the tree.
Absorbs the candidate rows "Catch errors in CPM bootstrap refits" (both parts)
and "Repoint stale line cites into cpm_fit.R".

**Out:**
- A separate count or label for refit errors in the warning or `print()`:
  declined at the plan gate; the help page says the count includes them.
- An error raised outside a refit's estimation and acceptance steps (the
  mirror guard, unpacking, the quantiles): a defect in the package, left to
  stop the call.
- A parametric bootstrap, where M167's devel run saw the errors: not shipped,
  and the devel copy already counts them (M167 archive).
- Line cites in `cairn/milestones/`, `cairn/DECISIONS.md`, `cairn/legacy/` and
  `cairn/reviews/`: history and live tracking records, not rewritten.
- Line cites into other files, such as test-file cites in
  `cairn/boundary-coverage.md`: outside both rows; not tracked.

## Acceptance criteria

- [ ] AC1: When a bootstrap refit's optimization or gradient call errors, on
      the first attempt or the restart, or yields a non-finite gradient norm,
      `cpm_fit()` returns a fit: the resample is excluded and added to
      `details$boots_nonconvergent`, which the exclusion warning reports, and
      the intervals come from the remaining replicates. When every refit
      errors, `cpm_fit()` returns with `NA` bootstrap intervals and the
      all-excluded warning.
- [ ] AC2: Where no refit errors, results at a seed are unchanged: the existing
      bootstrap tests and snapshots pass with no edits, and `identical()`
      between master's and the branch's `cpm_fit()` result is `TRUE` at two
      seeds, one of which excludes a nonconvergent refit.
- [ ] AC3: The `cpm_fit()` help page states that the nonconvergent count
      includes refits that stopped with an error. The marker list in its
      "Confidence intervals" text and the marker list on the
      `summary.circumplex_cpm()` page each contain every label
      `cpm_marker_labels()` returns, verbatim.
- [ ] AC4: NEWS.md's development section carries an entry under "Minor
      improvements and fixes" saying that a bootstrap refit error in
      `cpm_fit()` now excludes that resample with a warning instead of stopping
      the call.
- [ ] AC5: `git grep -nE '(^|[^-[:alnum:]_])cpm_(fit|oop)\.R:[0-9]'` returns no
      line outside `cairn/milestones/`, `cairn/DECISIONS.md`, `cairn/legacy/`
      and `cairn/reviews/`.
- [ ] AC6: `devtools::test()` passes, and
      `devtools::check(args = "--no-manual")` reports 0 errors, 0 warnings and
      0 notes.

## Coverage

- AC1 → T1, T2, T6
- AC2 → T1, T2
- AC3 → T2, T3, T6
- AC4 → T2
- AC5 → T4
- AC6 → T5, T6

## Tasks

- [x] T1: Tests first, in `tests/testthat/test-cpm_fit.R` or a new
      `test-cpm_bootstrap.R`. Inside `test_that()`, mock `cpm_optimize_one()`
      and `cpm_gradient()` so they misbehave on bootstrap refits only, leaving
      the engine fit untouched. Probes: an error at each of the four wrapped
      calls (optimize and gradient, first attempt and restart; reach the
      restart by a mocked first gradient above the threshold), in two forms
      (`stop()` and a base-R singular `solve()` error, the form M167 saw); a
      `NaN` gradient; every refit erroring. Assert the warning's counts
      against an unmocked run at the same seed. Run the one-error case on
      master, confirm it fails with the forced error and not another one, and
      log that. Before T2, save master's fits at two seeds (one with a
      nonconvergent exclusion; the seed behind `_snaps/cpm_api.md` has one)
      to the scratchpad for AC2's one-time `identical()` check; nothing is
      committed from master.
- [x] T2: In `cpm_bootstrap()`, wrap each attempt's `cpm_optimize_one()` and
      `cpm_gradient()` calls in `tryCatch(error = )`, and test acceptance with
      `isTRUE(gnorm <= tol)` so a non-finite norm fails it; a caught error or
      a failed test counts as nonconvergent. Nothing else in the loop is
      wrapped. Update the `@noRd` comment and the help sentence "a refit
      failing the convergence acceptance criterion are excluded" so it says
      the count includes refits that errored. NEWS entry.
      `devtools::document()`. Run AC2's `identical()` check and log it.
- [x] T3: Reword both marker lists so each `cpm_marker_labels()` value appears
      verbatim: `cpm_fit()`'s roxygen ("a removed harmonic", "a
      correlation-function weight below 0.10") and `summary.circumplex_cpm()`'s
      in `R/cpm_oop.R` ("removed harmonic", "small correlation-function
      weight", "ill-conditioning"). Add a test that reads each list itself,
      not the whole page, from the rendered Rd text (`man/*.Rd` from source
      when reachable, else the installed Rd database under `R CMD check`;
      `tools::Rd_db()` fails under `load_all`), whitespace collapsed. Make
      sure that it runs, not skips, under both `devtools::test()` and
      `R CMD check`. Plants: a renamed label and a label present only outside
      the list must fail; a label split across a line break must pass; a key
      added to `cpm_marker_labels()` must fail.
- [x] T4: Replace each line-number cite that AC5's grep finds (62 lines in 14
      files at `f237fe11`) with the function or roxygen block it points to.
      Mark the `cairn/references/` pages `corrected M172` where they sit, and
      update `INDEX.md`. Re-run the grep. Cite by function name in this file's
      log.
- [x] T5: `devtools::test()`, then `devtools::check(args = "--no-manual")`.
- [x] T6: Review return 1. Make `cpm_boot_refit()` reject a non-finite gradient
      norm or F, with an `inf` probe (both infinite) that fails first; assert
      the test's replicate-2 precondition; fix the pass-1 fix-now comments and
      the `cpm_fit()` help sentence on excluded resamples; re-run T5's checks.

## Work log

- 2026-10-28: created by /milestone-plan.
- 2026-10-28: tier check: not run — work named at the question set.
- 2026-10-28: question set: which candidate to plan — "Catch errors in CPM bootstrap refits" with the line-cite row; how to count a refit error — as nonconvergent, warning text unchanged.
- 2026-10-28: idea vet: no concern. Excluding a failed resample with a counted warning matches `cpm_bootstrap()`'s estimability convention (GP2); the raw-data path is the only bootstrap path; no name or argument changes. The audit's later label point (finding 3) is settled under the user's choice by the help sentence.
- 2026-10-28: lessons used: 2026-10-28 (M171) mocked bindings last the whole file; 2026-07-19 (M7 family) a skipped step reports success, for T3's help-text test.
- 2026-10-28: collision check: the two absorbed candidate rows only; no D-entry, planned milestone or archive decides refit-error handling. M167's devel copy counted errors as nonconvergent, the precedent taken.
- 2026-10-28: inbox sweep: 0 open issues, 0 open PRs; Resolves stays —, so no assignment option.
- 2026-10-28: scope: absorbed the row's part (ii) (marker labels on the help page) since its trigger is this milestone opening `cpm_bootstrap()`.
- 2026-10-28: plan chose wrapping only the estimation calls and the acceptance test over wrapping the whole loop body, because an error in the mirror guard or unpacking is a package defect that should stop the call; falsified by a user-reported refit error from those steps on valid data.
- 2026-10-28: criteria audit (full, fresh Opus reader) returned 12 findings, all disposed: (1) AC1's master-failure clause moved to T1; (2) probes widened to four call sites, two error forms and a NaN norm in T1; (3) chose the help-sentence fix over reopening the warning label, held by the gate's answer; (4) non-finite gradient norm now fails acceptance (AC1, T2); (5) AC2 rewritten to a one-time `identical()` check plus unedited snapshots, no master fixture committed; (6) AC3 reads source Rd or the installed database, not "installed" alone; (7) the test reads each list, with four plants in T3; (8) `summary.circumplex_cpm()`'s marker list brought in, since it carries the same mismatch; (9) AC3 now promises page content, the test lives in T3; (10) AC4 no finding; (11) AC5 excludes `cairn/milestones/`; (12) AC6 no finding.
- 2026-10-28: changelog: one NEWS entry for the refit fix; the help-page wording changes take none.
- 2026-10-28: implement started on m172-cpm-bootstrap-refit-errors.
- 2026-10-28: T1: probes in test-cpm_api.R through `local_refit_fault()` (four sites x two forms, a NaN norm, every refit); on master the one-error case stopped with "forced refit error" raised from `cpm_optimize_one()` inside `cpm_bootstrap()`, and the all-error case the same. Dropped a wrong precondition (one of master's 20 baseline refits is already nonconvergent); the count checks are relative to the unmocked run.
- 2026-10-28: T2: the attempt moved into a helper, `cpm_boot_refit()`, whose `tryCatch()` covers the optimization, gradient and acceptance test; an attempt that errors is not restarted, since its point is gone. NEWS entry added; `cpm_api` tests pass.
- 2026-10-28: AC2 check: master's and the branch's fits at seeds 1 and 2 (N = 300, boots = 100; 1 and 2 nonconvergent exclusions) are `identical(..., ignore.environment = TRUE)`; plain `identical()` is FALSE even between two reads of the same master file, because `corfun` is a closure.
- 2026-10-28: full suite after T2: 0 failures, 15446 passes.
- 2026-10-28: T3: both lists now carry the five labels verbatim; the `cpm_fit()` page states the 0.10 cut in its own sentence. Test "the help pages list every marker label summary() prints" in test-cpm_summary_markers.R reads the parenthetical after "weak-identification marker" from `man/` or `tools::Rd_db()`. Plants: renamed label fails, label left only outside the list fails (it still occurs once elsewhere on the collapsed page), label split over a line break passes, added key fails; files restored.
- 2026-10-28: T4 delegated to an Opus subagent (map each stale cite to its function by the cite's prose); checkpoint of its partial edits (`R/axes_reliability.R`, `INDEX.md`), T4 not done.
- 2026-10-28: T4 done: 62 cites in 14 files now name a function or roxygen block (stale ones traced through `git show` of the commit that added them); AC5's grep prints nothing; every named function exists in `R/`; three reference pages carry `corrected M172`, hu1999's M164 note extended. Fixed two vague test-comment cites in test-cpm_angle_ci.R by hand. Bare-number cites with no file prefix in `devel/cpm-pack-boundary-brief.md` stay: outside AC5's grep and the row.
- 2026-10-28: claim audit: 49 claims read, 2 corrected — R/cpm_fit.R (`cpm_boot_refit()` comment now names the NaN-F and infinite-F cases), tests/testthat/test-cpm_api.R (`local_refit_fault()` returns an environment).
- 2026-10-28: T5 at 09317c69: `devtools::test()` 0 failures, 15458 passes; `devtools::check(args = "--no-manual")` 0 errors, 0 warnings, 0 notes. 144b17af after it changes comments only. Status → review.
- 2026-10-28: review return 1: AC1 fails for a refit whose gradient norm and F are both infinite: `Inf <= 1e-6 * Inf` accepts it instead of excluding it (diff-bug #2). Fix with the pass-1 fix-now items: diff-bug #3 and #7, blame-history #3 and #4, ripple #2.
- 2026-10-28: T6: added the `inf` probe (infinite F and gradients on both attempts) and a counting-only `none` baseline asserting replicate 2 takes one attempt; on 99456de5 the `inf` probe failed with the refit accepted (nonconvergent 1, expected 2). `cpm_boot_refit()` now requires a finite norm and F; on the finite path the comparison is master's. Comments fixed (helper, restart, roxygen), help sentence names errored refits, narrative comment removed from test-cpm_angle_ci.R. cpm_api, cpm_summary_markers and cpm_angle_ci tests: 0 failures, 1640 passes.
- 2026-10-28: T6 checks at 5a05ef52: `devtools::test()` 0 failures, 15465 passes; `devtools::check(args = "--no-manual")` 0 errors, 0 warnings, 0 notes. claim audit (return 1): not re-run, the added lines are comments, one help sentence and test code that pass-2 review reads. Status → review.
- 2026-10-28: correction to the line above: the claim audit was owed and ran. claim audit: 21 claims read, 4 corrected — R/cpm_fit.R (helper comment names the one restart; roxygen notes an erroring attempt gets none; help sentence says "NA or non-positive-definite"), tests/testthat/test-cpm_api.R (baseline comment). Its coverage gap closed: the `inf` probe now sets only F infinite, keeping the real gradient; on 99456de5's `R/cpm_fit.R` it failed (refit accepted), on the fix it passes; cpm_api and cpm_summary_markers 0 failures, 1629 passes.

## Decisions

## Review

Pass 1 (2026-10-28, head 219bf738).

- AC2 (pre-return evidence, re-run next pass): `identical(..., ignore.environment = TRUE)` TRUE at seeds 1 and 2 against master's saved fits; no `_snaps` change; test-cpm_api.R diff removes no line.
- AC3, AC4, AC5 (pre-return evidence, re-run next pass): both Rd marker lists hold the five labels; the count sentence is in man/cpm_fit.Rd; NEWS entry present; AC5's grep prints nothing.
- gate (pass 1): `cairn_validate` passed; `document()` no diff, 0 `resolve link` lines; `pkgdown::check_pkgdown()` no problems; R-CMD-check.yaml and test-coverage.yaml newest master push runs success (d50ffca9); alert audits and branch-protection check exit 0; README untouched.
- spawned: diff-bug, blame-history, prior-review, ripple
- diff-bug #1: `tryCatch(error=)` cannot tell a package defect from a numerical failure, so a future bug shows as nonconvergence — follow-up, candidate row "Keep the first CPM refit error message"
- diff-bug #2: a refit with infinite gradient norm and infinite F is accepted (`Inf <= Inf`), so AC1's "non-finite gradient norm ... excluded" fails — fix now, review return 1
- diff-bug #3: test comment "Replicate 2 is accepted without a fault" is an unchecked precondition, fragile across BLAS — fix now
- diff-bug #4: nine probes repeat one handler path — reject, planned change (T1 named the battery at the audit's request)
- diff-bug #5: warning and print note call an errored refit nonconvergent — reject, planned change (plan gate kept the warning text)
- diff-bug #6: roxygen lines over 80 columns — reject, style
- diff-bug #7: test-cpm_angle_ci.R:86-87 keeps a narrative "verified out-of-band" comment the Test-comments rule bars — fix now (the diff edits that line)
- blame-history #1: errors from any wrapped call become nonconvergent and the message is lost — follow-up, same row as diff-bug #1
- blame-history #2: warning wording attributes errors to the criterion — reject, planned change
- blame-history #3: comment "Acceptance still keys ONLY on the scaled gradient norm" in `cpm_bootstrap()` is now false — fix now
- blame-history #4: the "its point is gone" rationale is wrong when only the gradient call errors — fix now (comment and wording; behavior kept)
- blame-history #5: the help-list test checks containment only — reject, planned change (AC3 promises containment)
- blame-history #6: line width and a mid-paragraph sentence — reject, style
- prior-review #1: no prior-review point reintroduced; roxygen lines over 80 columns — reject, style
- ripple #1: ROADMAP row "Catch errors in CPM bootstrap refits" now describes fixed code — reject, planned change (step 9 graduates the row)
- ripple #2: the `cpm_fit()` help sentence on excluded resamples names only refits failing the criterion — fix now
- ripple #3: ROADMAP row "Repoint stale line cites into cpm_fit.R" still open — reject, planned change (step 9 graduates the row)
