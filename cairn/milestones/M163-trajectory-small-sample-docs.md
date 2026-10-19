# M163: Small-sample coverage in the trajectory docs

- **Status:** in-progress
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

- [ ] AC1: Section 7 of `vignettes/growth-ssm-analysis.Rmd.orig` and the re-rendered `vignettes/growth-ssm-analysis.Rmd` state five facts from the M161 record.
  - The simulated design: the `ssm_growth_formula()` model, five balanced waves, the two truths and 1000 replicates per cell.
  - The range of the shipped (R0) mean coverage among converged fits over the six complete-data cells at 20, 30 and 50 persons. Each mean is over the five waves and over e, x, y and d.
  - The share of fits with a non-positive-definite Hessian in the complete-data cells at 10 and at 20 persons, for each truth.
  - That such a fit still yields intervals. With it, the R0 mean coverage at 10 persons for each truth, on the same averaging basis, among converged fits and among non-positive-definite fits.
  - The record path `devel/m161-small-n-oracle.md`.

  Each figure equals, at its printed precision, the value computed from `devel/m161-small-n-results.rds`. No sentence of Section 7 contradicts a stated figure.
- [ ] AC2: The small-sample paragraph of the `ssm_trajectory()` help page states two facts. They are the R0 converged-fit coverage range at 20 to 50 persons, and the non-positive-definite share at 10 persons for each truth. This holds in the roxygen of `R/ssm_trajectory_helper.R` and in the regenerated `man/ssm_trajectory.Rd`. Each figure equals the one that Section 7 states. The paragraph keeps its pointer to Section 7, and no sentence of it contradicts a stated figure.
- [ ] AC3: Two paragraphs of `vignettes/growth-ssm-analysis.Rmd.orig` report a measured coverage result outside Section 7. They hold the roughly 86% result of Section 4 and the near-origin result of Section 6. Each names the persons per cell and the record that measured its result.
- [ ] AC4: Take the development-version NEWS.md entry that opens "The same vignette's Section 7 now states". It also says that Section 7 and `?ssm_trajectory` report the coverage at 10 to 50 persons. NEWS.md gains no other entry from this milestone, and the entry names no milestone.
- [ ] AC5: `devtools::check(args = "--no-manual")` reports 0 errors, 0 warnings and 0 notes. `tools/precompute-vignettes.R` renders the growth vignette, and `Rscript tools/check-vignette-staleness.R` passes after the render.

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
- [ ] T5: Review return 1: name the `ssm_growth_formula()` model in Section 7's design fact, re-render, run the staleness check, and re-run the test suite and the check.

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

## Decisions

## Review

Pass 1 (2026-10-19, branch head 1b95f546, master unmoved at 5d4cb2e1, no PR, Copilot round off).

- AC1: FAIL. A recompute script over `devel/m161-small-n-results.rds` (R0, mean over waves 0:4 and e, x, y, d) found every figure in Section 7 of both files: .938/.944, 27.4%/17.7%, 3.2%/1.0%, .939/.920, .906/.903, 1000 samples, five waves, `devel/m161-small-n-oracle.md`. Section 7 is identical in `.Rmd.orig` and `.Rmd`. But Section 7 does not name the `ssm_growth_formula()` model, which the design fact requires; it says "this model" only. Returned to implement.
- AC2: pass. The roxygen at `R/ssm_trajectory_helper.R:43-51` and `man/ssm_trajectory.Rd:119-126` state .938 to .944 at 20 to 50 persons and 27.4% and 17.7% at 10 persons, equal to Section 7, keep the Section 7 pointer, and contradict no figure (read).
- AC3: pass. `vignettes/growth-ssm-analysis.Rmd.orig:188-191` (Section 4, 86% result) and `:312-315` (Section 6, near-origin result) each name 200 persons per cell and `devel/m27-coverage-oracle.md`.
- AC4: pass. `NEWS.md:335-340`, the dev entry opening "The same vignette's Section 7 now states", says Section 7 reports the coverage at 10 to 50 persons and that `?ssm_trajectory` states the main figures; `git diff master...HEAD -- NEWS.md` adds no other entry and names no milestone.
- AC5: not run this pass; the fresh check was stopped at the AC1 return because the fix changes the vignette.
