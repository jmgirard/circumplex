# M155: The trajectory caution by input shape, and the bootstrap path into the helper

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP4, GP5
- **Resolves:** —
- **Surface tier:** user-facing — a print method, a help page and a vignette section that readers use

## Goal

`ssm_trajectory()` prints a caution that describes its input shape, and the growth vignette's Section 7 shows the parametric bootstrap for the shipped engines and its path into the helper as `draws =`.

## Scope

**In:** an `input` attribute on the trajectory object. A print branch on that attribute. It prints a REML caution under `coef` and `vcov`, a draws caution under `draws`, and a shape-neutral caution for an absent attribute. Section 7 rewritten on the glmmTMB `simulate()` and `refit()` bootstrap, with a committed replicate file and its generator. The nlme sentence. The sweep of every sentence that describes the caution. The help page and NEWS.

**Out:** a `draws` joint-fit check stays refused in the help page, because D-064 leaves the joint model to the user under `draws`. An nlme response simulator is not planned, and Section 7 routes the nlme user through the glmmTMB call. The lme4 remedies are dropped from the vignette at this gate and get no home. The `ssm_growth_data()` input guards stay in their own candidate row.

## Acceptance criteria

- [ ] AC1: `ssm_trajectory()` returns an object carrying attribute `input`. The attribute equals `"coef_vcov"` when the call gives `coef` and `vcov`, and `"draws"` when the call gives `draws`. The returned columns are the `<time>` column plus `trajectory_value_cols()`, as before this milestone.
- [ ] AC2: `print()` of a `coef` and `vcov` result ends with the REML caution paragraph of the M154 print. Its text is unchanged and cites Section 7. `print()` of a `draws` result ends instead with a caution that states three things. The intervals summarize the supplied draws as given. Their coverage depends on how the draws were produced. The joint fit was not checked. That caution cites Sections 7 and 10 of the vignette, and the output contains no REML caution. A column subset drops the `input` attribute. An object of the class without it ends with a shape-neutral caution that says the input shape is not recorded and cites Sections 7 and 10. The table lines, the uncertified mark and the uncertified note print as the M154 print emits them under both shapes. The existing print test and a `draws` twin whose table holds an uncertified row show this.
- [ ] AC3: Section 7 of `vignettes/growth-ssm-analysis.Rmd.orig` states the parametric bootstrap of the fixed effects as the small-sample remedy for the shipped engines. An echoed chunk shows the glmmTMB loop of `simulate()` and `refit()` that collects one fixed-effect vector per replicate. An echoed chunk reads the committed replicate file and passes it as `ssm_trajectory(draws = )`. The prose states that bootstrap replicates enter the helper as `draws =`. The rendered section names none of `lme4`, `pbkrtest` and Kenward-Roger. At each wave, the bootstrap table's `x_est` and `y_est` sit within 0.02 of the Section 5 table's values. The prose states the largest gap as an inline knitr expression, never as a typed number.
- [ ] AC4: Section 7 states that the loop needs an engine whose `simulate()` returns responses for the model. It states that glmmTMB does and that nlme's `simulate()` refuses a model with a `varIdent` weight. It states that a user on nlme runs the bootstrap through the glmmTMB call of Section 4.
- [ ] AC5: `vignettes/growth_bootstrap_draws.rds`, written by `data-raw/growth-bootstrap-draws.R`, holds 1000 rows with no missing value. Its six columns are named as the Section 5 fit's `names(glmmTMB::fixef(fit)$cond)`. It carries a provenance attribute that records the seed, the replicate count and the glmmTMB version written at run time. The generator states how it handles a refit that fails or returns a missing value.
- [ ] AC6: Each of these sites is rewritten to the shape-dependent caution: the vignette's line 36 echo of Section 7's title, the Section 7 heading, Section 5's "ends with the caution that Section 7 explains", Section 10's "The note under the table is written for the `coef` and `vcov` shape, and it does not apply here", the roxygen `@return` line "states the small-sample caution" (which also names the `input` attribute), the roxygen details sentence "the ... vignette states the remedies", and the NEWS bullet's "the small-sample caution stated".
- [ ] AC7: `devtools::test()` is clean, and `devtools::check(args = "--no-manual")` reports 0 errors, 0 warnings and 0 notes on the final tree. `Rscript tools/precompute-vignettes.R`, then `Rscript tools/check-vignette-staleness.R` and `Rscript tools/check-vignette-width.R`, exit 0.

## Coverage

- AC1 → T1
- AC2 → T1
- AC3 → T2, T3
- AC4 → T3
- AC5 → T2
- AC6 → T4
- AC7 → T5

## Tasks

- [ ] T1: Tests first in `tests/testthat/test-ssm_trajectory_helper.R` (the AC6 block at line 497): the `input` attribute on both shapes, the three caution branches on captured print output, the `draws` twin with an uncertified row, and column identity. Then set the attribute in `ssm_trajectory()` (`R/ssm_trajectory_helper.R:262`) and branch `print.circumplex_ssm_trajectory()` (line 369) on it. Update `@return` and the details sentence at line 41, then run `devtools::document()`.
- [ ] T2: Write `data-raw/growth-bootstrap-draws.R` on the model of `data-raw/growth-brms-draws.R`. It runs the Section 5 glmmTMB fit, `set.seed`, `simulate(nsim = 1000)` and `refit()` per column. It drops a failed or `NA` refit and continues until 1000 rows, adds the provenance attribute, and calls `saveRDS` on `vignettes/growth_bootstrap_draws.rds`. Run it once (about 190 s) and commit the file.
- [ ] T3: Rewrite Section 7 (lines 341 to 357) and retitle it. The REML conditioning paragraph stays, and the remedy is the bootstrap. Show the loop chunk echoed with `eval = FALSE`, and evaluate the `readRDS` chunk plus `ssm_trajectory(draws = )`. Add the inline largest-gap expression against the Section 5 table and the nlme sentence. Before writing the nlme sentence, run `simulate()` on the Section 10 nlme fit once and make sure that the refusal message appears. Re-render with `tools/precompute-vignettes.R`, look at the rendered section, and record the echo sweep's accepted hits (the two `readRDS` lines) in the work log.
- [ ] T4: Sweep the seven AC6 sites. Then run `grep -n -i "caution\|note under\|Section 7" R/ssm_trajectory_helper.R NEWS.md vignettes/growth-ssm-analysis.Rmd.orig` for any further stranded sentence. Edit the NEWS bullet in the unreleased M152 entry and add a Documentation line for Section 7.
- [ ] T5: Run `devtools::test()`, `devtools::check(args = "--no-manual")` and the three vignette tools. Make sure that `git status` is clean of any stray re-render before the gate.

## Work log

- 2026-09-24: created by /milestone-plan. Absorbs the ROADMAP candidate row from M152 review O8 and M153 review O5. The criteria audit ran in full mode by a fresh [O] reader and returned six findings, all disposed at the gate. The REML fallback became a shape-neutral caution. The gap is an inline expression. The replicate file gained a no-NA clause and a provenance attribute. AC6 enumerates its sites in place of a grep universal. The echo sweep left AC7, because it exits 1 on master at the accepted brms `readRDS` line. Kenward-Roger joined AC3.
- 2026-09-24: plan gate chose a shape-specific printed caution over no caution under `draws` and over keeping the unconditional text, because D-056 keeps a caution printed with a true meaning, and removing one needs a GP4 gate. Falsified by a user report that the draws caution misdescribes a draws source, such as a bootstrap or brms matrix.
- 2026-09-24: plan gate chose a committed 1000-replicate file over `eval = FALSE` code alone and over running the loop at each render, because the reader sees the bootstrap table and the staleness guard stays byte-stable. The M120 lesson applies: optimizer digits move across BLAS builds. Falsified by a staleness failure on the committed table, or by a file whose replicates cannot be regenerated near its values.
- 2026-09-24: plan gate chose dropping the lme4, pbkrtest and Kenward-Roger remedies over keeping an aside, because the builder writes no lme4 model and the aside is untested prose. Falsified by a user request for an lme4 dialect.
- 2026-09-24: plan gate chose stating nlme's `simulate()` refusal and routing through glmmTMB over a hand-written nlme simulator, because the simulator is a new correctness surface in a teaching page. The parity test holds the engines equal. Falsified by a user who cannot install glmmTMB and needs the bootstrap on nlme.

- 2026-09-24: /milestone-implement started; branch m155-trajectory-caution-and-bootstrap cut from master e01c1dc5. Question gate skipped: the plan fixed the attribute name, the caution wording and the demonstration form, and no dependency changes.
- 2026-09-24: checkpoint, half-done and marked so. T1 code, roxygen and tests written; the helper test file is green and the full suite is still running, so T1 stays unticked. The T2 generator is written and running (1000 refits); no replicate file yet. Section 7, the three sibling vignette sentences and the two NEWS edits are drafted, unrendered.

## Decisions

## Review
