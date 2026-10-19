# M163: Small-sample coverage in the trajectory docs

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP2, GP5
- **Resolves:** —
- **Surface tier:** user-facing — the growth vignette, the `ssm_trajectory()` help page and NEWS.md
- **Branch/PR:** m163-trajectory-small-sample-docs

## Goal

The growth vignette, the `ssm_trajectory()` help page and NEWS.md state what the M161 oracle measured about the shipped intervals at 10 to 50 persons.

## Scope

**In:** Section 7 of `vignettes/growth-ssm-analysis.Rmd.orig` and its re-rendered `.Rmd`. The small-sample paragraph of the `ssm_trajectory()` roxygen in `R/ssm_trajectory_helper.R`, and the regenerated `man/ssm_trajectory.Rd`. The two measured coverage results outside Section 7 in that vignette. The existing development-version NEWS entry on Section 7.

**Out:** any change to how `ssm_trajectory()` draws or prints. There is no code change, and the printed caution text stays. A small-sample draw rule, which stays the parked candidate row "Small-sample correction for trajectory draws" because D-066 names no rule. Advice on a minimum number of persons, declined at the plan gate. The parked peer-review status row.

## Acceptance criteria

- [x] AC1: Section 7 of `vignettes/growth-ssm-analysis.Rmd.orig` and the re-rendered `vignettes/growth-ssm-analysis.Rmd` state five facts from the M161 record.
  - The simulated design: the `ssm_growth_formula()` model, five balanced waves, the two truths and 1000 replicates per cell.
  - The range of the shipped (R0) mean coverage among converged fits over the six complete-data cells at 20, 30 and 50 persons. Each mean is over the five waves and over e, x, y and d.
  - The share of fits with a non-positive-definite Hessian in the complete-data cells at 10 and at 20 persons, for each truth.
  - That such a fit still yields intervals. With it, the R0 mean coverage at 10 persons for each truth, on the same averaging basis, among converged fits and among non-positive-definite fits.
  - The record path `devel/m161-small-n-oracle.md`.

  Each figure equals, at its printed precision, the value computed from `devel/m161-small-n-results.rds`. No sentence of Section 7 contradicts a stated figure.
- [x] AC2: The small-sample paragraph of the `ssm_trajectory()` help page states two facts. They are the R0 converged-fit coverage range at 20 to 50 persons, and the non-positive-definite share at 10 persons for each truth. This holds in the roxygen of `R/ssm_trajectory_helper.R` and in the regenerated `man/ssm_trajectory.Rd`. Each figure equals the one that Section 7 states. The paragraph keeps its pointer to Section 7, and no sentence of it contradicts a stated figure.
- [x] AC3: Two paragraphs of `vignettes/growth-ssm-analysis.Rmd.orig` report a measured coverage result outside Section 7. They hold the roughly 86% result of Section 4 and the near-origin result of Section 6. Each names the persons per cell and the record that measured its result.
- [x] AC4: Take the development-version NEWS.md entry that opens "The same vignette's Section 7 now states". It also says that Section 7 and `?ssm_trajectory` report the coverage at 10 to 50 persons. NEWS.md gains no other entry from this milestone, and the entry names no milestone.
- [x] AC5: `devtools::check(args = "--no-manual")` reports 0 errors, 0 warnings and 0 notes. `tools/precompute-vignettes.R` renders the growth vignette, and `Rscript tools/check-vignette-staleness.R` passes after the render.

## Coverage

- AC1 → T1, T2, T5
- AC2 → T1, T3
- AC3 → T2
- AC4 → T3
- AC5 → T4, T5

## Tasks

- [x] T1: Compute the Section 7 figures from `devel/m161-small-n-results.rds` with a short script run in the session. Record each figure and its source element in a work-log line.
- [x] T2: Rewrite Section 7 in the `.Rmd.orig`. Name the sample and record in the two AC3 paragraphs, reading the M27 record for each. Grep the vignette and `R/` for text that cites Section 7 (the Overview, Section 5, Section 10 and the printed caution) and fix any claim the rewrite makes false. Re-render with `tools/precompute-vignettes.R` and run the staleness check.
- [x] T3: Update the help-page paragraph, run `devtools::document()`, and edit the NEWS entry.
- [x] T4: Run the test suite and `devtools::check(args = "--no-manual")`.
- [x] T5: Review return 1: name the `ssm_growth_formula()` model in Section 7's design fact, re-render, run the staleness check, and re-run the test suite and the check.

## Work log

- 2026-10-19: created by /milestone-plan. tier check: milestone — the request named M162's replan or drop, a design decision, and the replacement restates oracle figures on three user-facing surfaces.
- 2026-10-19: question set: drop M162 or replan — drop it and fix the docs. Minimum-N advice — figures only. Peer-review status line — keep it parked. No outward action or dependency change was asked or granted.
- 2026-10-19: plan gate chose docs only over a replan with a fresh pre-registered oracle run, because the shipped converged-fit coverage at 20 to 50 persons is .938 to .944, a t rule can only be opt-in, and no draw rule repairs the 10-person boundary fits; falsified by a pre-registered run in which the t rule passes a band fixed before the run (D-066), or by evidence that the documented coverage misleads users at the sample sizes they fit.
- 2026-10-19: plan gate chose figures only over a minimum-N threshold, because the record covers one model and two truths; falsified by an oracle over more designs that places a threshold.
- 2026-10-19: criteria audit (full mode, fresh Opus reader): nine findings, all fixed. AC1 now names the six complete-data cells, the R0 rule, the averaging basis and the simulated design. Its 10-person fact reports coverage among converged and non-PD fits, because a non-PD fit still yields intervals, and the .68 to .78 all-replicate figure is the band's scoring, not what a user sees. AC1 and AC2 forbid contradicting sentences, and AC2 counts per-truth shares. AC3 names its two paragraphs and drops the grep and the ledger (proxy domain, instrument clause). AC5 adds the staleness check. AC4, reachability, probe and proportionality returned nothing.
- 2026-10-19: collision sweep: M162 (planned) dropped at the user's choice and archived. The "Small-sample correction for trajectory draws" row is parked under D-066's reopen condition, and D-066 holds. The peer-review row stays parked at the user's choice, its "next opens those roxygen blocks" condition removed.
- 2026-10-19: inbox sweep: one open issue (#180, a master-red alert) and no open PRs. Neither overlaps.
- 2026-10-19: lessons harvested: M56 family (sweep the prose that cites Section 7, in both directions), M94 (state what the study covered, not a general threshold), M90 (compute each figure from the committed `.rds`).
- 2026-10-19: implement started on branch m163-trajectory-small-sample-docs, cut from master at 5d4cb2e1.
- 2026-10-19: T1: figures from `summary[[cell]]` of the M161 `.rds`, R0, mean over waves 0:4 and e, x, y, d. `cov_conv` at 20/30/50 complete: pole .9380/.9432/.9435, xycor .9414/.9436/.9406, so .938 to .944. Non-PD share `1 - n_ok/n_reps`: n10 pole .274, xycor .177; n20 pole .032, xycor .010. At n10, `cov_conv` pole .9389, xycor .9195; `nonpd_cov$cov` pole .9064 (274 fits), xycor .9031 (177 fits). Design from `meta$waves` (0:4), `meta$reps` (1000) and the script header.
- 2026-10-19: T2: Section 7 rewritten with the T1 figures, the design and both records. The two AC3 paragraphs (Section 4 shortcut result, Section 6 near-origin result) name 200 persons per cell and `devel/m27-coverage-oracle.md`, read from its header and A2/A3 rows. Section 7 citers (Overview `:43`, Section 5 `:220`, Section 10 `:515`, the printed caution) still hold. The helper roxygen's "the coverage oracle" goes plural at T3. The re-render changed prose lines only.
- 2026-10-19: T2 staleness: after the T2 commit, a re-render and `tools/check-vignette-staleness.R` report all 11 pre-computed vignettes up to date.
- 2026-10-19: T3: the `ssm_trajectory()` small-sample roxygen paragraph states the .938 to .944 converged range at 20 to 50 persons and the 27.4% and 17.7% non-PD shares at 10 persons, and keeps the Section 7 pointer; `document()` rewrote `man/ssm_trajectory.Rd` only. The dev NEWS entry on Section 7 now names the 10-to-50-person figures and `?ssm_trajectory`. `devtools::test()`: 14865 passed, 0 failed, 1 skipped.
- 2026-10-19: claim audit: 38 claims read, 3 corrected — vignettes/growth-ssm-analysis.Rmd.orig (and .Rmd), NEWS.md. The five-balanced-waves sentence now covers only the reported figures, since M161 also ran missing-wave cells. The 10-person non-PD coverage now says normal draws on those fits' `vcov`, since the oracle passed its own draws as `draws =` and the helper refuses an asymmetric or invalid `vcov` (`R/ssm_trajectory_helper.R:301-318`); the auditor's spot check found one such fit in 123. NEWS names the non-PD rate at 10 and 20 persons only, as Section 7 states it.
- 2026-10-19: T4: `devtools::check(args = "--no-manual")` on the final tree: Status OK, 0 errors, 0 warnings, 0 notes, vignette re-build OK; a first run started before the claim-audit fixes was stopped unfinished. Test suite as at T3. Status set to review.
- 2026-10-19: review return 1: AC1 — Section 7 does not name the `ssm_growth_formula()` model in its design fact ("this model" only); every figure and the other design facts were found. Status back to in-progress.
- 2026-10-19: T5 added for review return 1 (minor amendment; Coverage AC1 and AC5 now also cite T5). Section 7 now says the oracles fit "the one `ssm_growth_formula()` writes"; checked for M27 too, whose `glmmTMB()` call (`devel/m27-coverage-oracle.R:118-120`) has the same formula and dispformula that `ssm_growth_formula("glmmTMB")` returns. NEWS line re-wrapped. Staleness check: all 11 up to date. `devtools::check(args = "--no-manual")` at 0acf2a3c: Status OK, 0 errors, 0 warnings, 0 notes, tests OK, vignette re-build OK. Status set to review.

## Decisions

## Review

Pass 1 (2026-10-19, branch head 1b95f546, master unmoved at 5d4cb2e1, no PR, Copilot round off).

- AC1: FAIL. A recompute script over `devel/m161-small-n-results.rds` (R0, mean over waves 0:4 and e, x, y, d) found every figure in Section 7 of both files: .938/.944, 27.4%/17.7%, 3.2%/1.0%, .939/.920, .906/.903, 1000 samples, five waves, `devel/m161-small-n-oracle.md`. Section 7 is identical in `.Rmd.orig` and `.Rmd`. But Section 7 does not name the `ssm_growth_formula()` model, which the design fact requires; it says "this model" only. Returned to implement.
- AC2: pass. The roxygen at `R/ssm_trajectory_helper.R:43-51` and `man/ssm_trajectory.Rd:119-126` state .938 to .944 at 20 to 50 persons and 27.4% and 17.7% at 10 persons, equal to Section 7, keep the Section 7 pointer, and contradict no figure (read).
- AC3: pass. `vignettes/growth-ssm-analysis.Rmd.orig:188-191` (Section 4, 86% result) and `:312-315` (Section 6, near-origin result) each name 200 persons per cell and `devel/m27-coverage-oracle.md`.
- AC4: pass. `NEWS.md:335-340`, the dev entry opening "The same vignette's Section 7 now states", says Section 7 reports the coverage at 10 to 50 persons and that `?ssm_trajectory` states the main figures; `git diff master...HEAD -- NEWS.md` adds no other entry and names no milestone.
- AC5: not run this pass; the fresh check was stopped at the AC1 return because the fix changes the vignette.

Pass 2 (2026-10-19, branch head d0453019, master unmoved at 5d4cb2e1, no PR, Copilot round off).

- AC1: pass. The recompute script over the M161 `.rds` found every figure and design phrase in Section 7 of both files (all checks TRUE): .938/.944, 27.4%/17.7%, 3.2%/1.0%, .939/.920 and .906/.903 on the stated basis, 1000 samples, five balanced waves, the two truths, `ssm_growth_formula()` (`.Rmd.orig:361-362`), `devel/m161-small-n-oracle.md`. Section 7 is identical in `.Rmd.orig` and `.Rmd`. A full read of Section 7 found no sentence contradicting a stated figure.
- AC2: pass. Same script: the four help figures appear in the roxygen (`R/ssm_trajectory_helper.R:43-51`) and in `man/ssm_trajectory.Rd`, equal to Section 7, with the Section 7 pointer in both; a read found no contradiction.
- AC3: pass. `vignettes/growth-ssm-analysis.Rmd.orig:188-191` and `:312-315` each name 200 persons per cell and `devel/m27-coverage-oracle.md` (unchanged since pass 1).
- AC4: pass. `NEWS.md:335-340` states the 10-to-50-person coverage and that `?ssm_trajectory` states the main figures; `git diff master...HEAD -- NEWS.md` adds no new entry (0 added `* ` lines) and names no milestone.
- AC5: pass. `devtools::check(args = "--no-manual")` at 0acf2a3c: Status OK, 0 errors, 0 warnings, 0 notes, tests OK, vignette re-build OK; `git diff 0acf2a3c HEAD` touches only `cairn/`, which `.Rbuildignore` excludes. After the T5 commit, a re-render and `tools/check-vignette-staleness.R` reported all 11 vignettes up to date.
- Gate: `cairn_validate` exit 0; `document()` no diff, 0 `resolve link` lines; README untouched; `pkgdown::check_pkgdown()` no problems; NEWS entry present; no new top-level files; master watches: newest push verdicts on `R-CMD-check.yaml` and `test-coverage.yaml` are success (37d75eb1); `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R` and `tools/check-branch-protection.R` exit 0. No principle changed, so no `cairn_impact` run.

spawned: diff-bug, blame-history, prior-review, ripple

- diff-bug #1: the 10-person converged coverage (.939, .920) has no selection caveat, though the record says kept fits run above target — fix now: Section 7 says the two figures run high and gives the record's reason (failed fits come most often from narrow-interval samples).
- diff-bug #2: "Such a fit still returns a finite `coef` and `vcov`" states a general glmmTMB property — fix now: scoped to "In the oracle, each such fit still returned".
- diff-bug #3: the refusal sentence before .906/.903 invites reading them as helper output — fix now: the figures come first and the refusal sentence now says the helper can refuse some of these fits.
- diff-bug #4: the help page does not define "converged" or the averaging basis or engine — fix now: roxygen names glmmTMB, defines converged as a positive-definite Hessian, and states the averaging over waves and e, x, y, d.
- diff-bug #5: ".938 to .944" comes before the averaging and reads as a per-interval range — fix now: the averaging sentence comes first and the figure is called the mean coverage.
- diff-bug #6: ".939 and .920" lacks "on average" — fix now.
- diff-bug #7: "Section 7 names the record" while naming it inline — fix now: one parenthetical record.
- diff-bug #8: the record clause inside the near-origin design description is awkward — fix now: "which used 200 persons per cell (record ...)".
- diff-bug #9: the first oracle's 500 samples per cell is not given — fix now: added (M27 record header).
- diff-bug #10: "did not converge cleanly" is a loose gloss — fix now: Section 7 states the oracle's converged rule (fit without error, positive-definite Hessian).
- diff-bug #11: NEWS "the coverage oracle's result" is singular for two oracles — fix now: "the first coverage oracle's result".
- diff-bug #12: lines past the wrap width in NEWS, roxygen and Section 7 — fix now: re-wrapped.
- blame-history #1: converged figures stated without D-066's bias caveat — fix now, with diff-bug #1. At 20 to 50 persons the recompute shows the bias is small: adding normal draws on non-PD fits' `vcov` moves the range by under .001 (.9375 to .9436), and Section 7 now says so.
- blame-history #2: "At small samples, read the intervals as approximate" drops the old size-relative bound — reject (planned change): the plan gate chose figures only, with no minimum-N advice, and the old bound was relative to a 200-person oracle the page no longer treats as the only one.
- blame-history #3: "essentially every replicate" is not supported by the M27 record — reject (false): `devel/m27-coverage-oracle.md` A2 gives a certification rate of .02 at the degraded wave, and its Reading says the caution fires in 98% of replicates.
- prior-review #1: converged coverage is the headline and the all-replicate basis (.908 at 20 persons) is left out — reject (false) in its premise, fix the caveat with diff-bug #1: the all-replicate basis scores a non-PD fit as a miss, which is the band's rule, not what a user sees; counting those fits' draws gives .9375 to .9436 at 20 to 50.
- prior-review #2: "slightly too narrow" understates a four-point gap — fix now in part: the adjective is replaced by the figure (.006 to .012); the four-point premise is the band's scoring, rejected as in #1.
- prior-review #3: .906 and .903 are not in the record's `.md` — reject (false): they come from `summary[[cell]]$nonpd_cov` in the `.rds`, recomputed by the diff-bug lens and the AC1 script.
- prior-review #4: the help figure has no averaging basis — fix now, with diff-bug #4.
- prior-review #5: "in the same order" leaves the mapping to the reader — fix now: the .906/.903 sentence names each truth.
- prior-review #6: no mapping from "crossing"/"correlated" to the record's `pole`/`xycor` — fix now: each truth names its record label.
- prior-review #7: "At small samples" is unbounded — reject (planned change), as blame-history #2.
- prior-review #8: the NEWS entry is one long sentence — reject (style).
- ripple #1: `R/ssm_growth_formula.R:2-3, 25`, `man/ssm_growth_formula.Rd:62` and `tests/testthat/test-ssm_growth_formula.R:3-4` name only the M27 oracle — reject (false as a staleness claim): each statement stays true, since the M27 oracle did validate the model; nothing the diff changed makes them wrong.
