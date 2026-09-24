# M153: The growth vignette on the helper workflow

- **Status:** review
- **Priority:** high
- **Depends on:** M152
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — a shipped vignette and the pkgdown site
- **Branch/PR:** `m153-growth-vignette-rewrite`

## Goal

Rewrite the growth vignette so that every code chunk is shown and the workflow is five short calls: `ssm_growth_data()`, `ssm_growth_formula()`, the pasted fit, `ssm_trajectory()` and `ssm_plot_trajectory()`.

## Scope

**In:** `vignettes/growth-ssm-analysis.Rmd.orig` rewritten on the M151 and M152 exports. glmmTMB stays the shown engine. A short section shows the nlme call from the builder and the brms call with a precomputed draws file, following the Bayesian vignette's pattern (D-015). `data-raw/growth-brms-draws.R` writes `vignettes/growth_brms_draws.rds` under a seed. The pre-render, staleness and width checks. `devel/m27-growth-recipe.R` switched to the helpers. A prose pass under `cairn/references/plain-vignettes.md`.

**Out:** the Bayesian vignette itself is untouched. The shared-intercept coverage cell stays its own candidate row. Any new statistic is out, since the page changes form and not claims.

## Acceptance criteria

- [x] AC1: `tools/vignette-echo-sweep.R`'s definition of a hidden chunk finds exactly two hidden chunks in `vignettes/growth-ssm-analysis.Rmd.orig`. They are the unnamed `include = FALSE` options chunk at the top and the chunk labelled `glmmtmb-note`.
- [x] AC2: Each of the vignette's two worked examples reaches `ssm_plot_trajectory()` through `ssm_growth_data()`, `ssm_growth_formula()`, one `glmmTMB::glmmTMB()` call and `ssm_trajectory()`. The fit call reads the builder's elements, so a grep finds `$formula` and `$dispformula` inside each `glmmTMB::glmmTMB(` call. A grep of the `.Rmd.orig` for `reshape(`, `mvn_draw`, `lapply(waves` and `data.frame(wave` returns nothing.
- [x] AC3: The brms section fits `simulated_growth` in a chunk marked `eval = FALSE`. Its draws come from `vignettes/growth_brms_draws.rds`, which `data-raw/growth-brms-draws.R` writes under a seed and which holds only the six `b_` columns. `ssm_trajectory(draws = )` summarizes them. At each wave, `x_est` and `y_est` lie within 0.01 of the glmmTMB table's. The correlation of the `x` and `y` intercept draws lies within 0.1 of the value the glmmTMB covariance implies. The work log records the measured gaps.
- [x] AC4: After the rewrite is committed, `tools/precompute-vignettes.R` regenerates the shipped `.Rmd`, and `tools/check-vignette-staleness.R` and `tools/check-vignette-width.R` pass on it. The baseline is the `.Rmd` at the commit the branch was cut from. The comparison runs on the precompute machine with chunk order and draw count unchanged. There, the rendered glmmTMB trajectory table's `a_*` and `d_*` values equal the baseline's to two decimals.
- [x] AC5: `tools/prose-sweep.R` reports no sentence over 25 words on the whole page.
- [x] AC6: `devel/m27-growth-recipe.R` builds its trajectory through `ssm_trajectory()`. Its guard `stopifnot(any(V_xy != 0))` is removed in favour of the helper's refusal. The script runs to completion under glmmTMB.
- [x] AC7: The verify slot is clean. `devtools::check()` is clean. NEWS.md has an entry for the vignette. `man/simulated_growth.Rd` carries a `\seealso` link to the vignette.

## Coverage

- AC1 → T1
- AC2 → T1
- AC3 → T2
- AC4 → T4
- AC5 → T3
- AC6 → T5
- AC7 → T6

## Tasks

- [x] T1: Rewrite Sections 3 to 6 on the five calls. Delete every hidden chunk except the two exceptions. Keep the joint-fit teaching, the VarCorr print and the cross-block check as prose around the helper's refusal.
- [x] T2: Write `data-raw/growth-brms-draws.R`, run it locally, commit the `.rds`, and add the engines section (nlme call shown, brms chunk `eval = FALSE`, draws summarized).
- [x] T3: Prose pass under the plain-vignettes rules. Run `tools/prose-sweep.R`.
- [x] T4: Commit, run `tools/precompute-vignettes.R`, then the staleness and width checks. Compare the rendered table to the baseline and record the result.
- [x] T5: Switch `devel/m27-growth-recipe.R` to the helpers and run it.
- [x] T6: NEWS entry, the `@seealso` on `simulated_growth`, `devtools::document()`, `devtools::check()`.

## Work log

- 2026-09-24: created by /milestone-plan.
- 2026-09-24: criteria audit ran in full mode on an [O] reader. Findings absorbed here: hidden chunks are defined by the echo sweep with the two exceptions named. The staleness check runs after the commit with the baseline named. The prose check is page-wide. The recipe guard is named by its code. The `\seealso` link is added rather than assumed.
- 2026-09-24: a second audit pass on the changed criteria: AC2 now requires the fit to read the builder's elements. AC3 names the dataset and keeps the draws file to six columns. It compares `x_est`, `y_est` and the intercept-draw correlation instead of amplitude.
- 2026-09-24: plan gate chose a precomputed brms demonstration inside this vignette over a fourth milestone. brms is never run on CI (D-015), so the demonstration is a committed draws file, the same pattern as the Bayesian vignette.
- 2026-09-24: implement gate chose the engines section as Section 10 after Caveats, since the trajectory print cites Section 7 by number, and the nlme demonstration as the printed call only, with no nlme fit on the page.
- 2026-09-24: checkpoint, half done. T1, T2 and T5 work is on disk (vignette rewritten, `data-raw/growth-brms-draws.R` run, `.rds` written, recipe switched and run to completion), and the T6 NEWS and `@seealso` edits are drafted. Nothing is ticked because `devtools::test()` is still running and `document()` has not run. The first generator run failed its own column check, since `^b_` also matched the three `b_sigma_` columns, and the fix selects the six names exactly.
- 2026-09-24: T1 done. The echo sweep's `read_chunks()` finds two hidden chunks (the options chunk at line 10 and `glmmtmb-note` at line 75); both `glmmTMB::glmmTMB(` calls read `$formula` and `$dispformula`; the four forbidden greps return nothing. The wave-2 `ssm_draws()` demonstration was dropped, since the helper runs that call at each time. `devtools::test()`: 0 failures, 12 warnings in unrelated files, 14298 passes.
- 2026-09-24: T2 done. `vignettes/growth_brms_draws.rds` holds 4000 draws by 6 `b_` columns (brms 2.23.0, seed 20260716, 168888 bytes). AC3 gaps against the glmmTMB table: `x_est` at most 3.3e-4 and `y_est` at most 5.0e-4 across the five waves; the `x`-`y` intercept-draw correlation is 0.0147 against the implied -0.0172, a gap of 0.032. `d_est` differs by at most 0.037 degrees. The nlme figures in Section 10 were measured in session: fixed effects within 1.3e-14 and covariance entries within 4.7e-9 of glmmTMB's.
- 2026-09-24: T4 done. The render committed at dab22714 differed from the source only by the two T1 prose splits, re-rendered here; the width check reports 112 output lines, all fit. Against the master baseline the Section 5 table's `a_*` and `d_*` values are equal at two decimals at every wave, and the origin case's wave-2 amplitude 0.02 [0.00, 0.05] and displacement 80.71 [322.34, 214.10] equal the old `mid` print. The old page never printed the origin table.
- 2026-09-24: T5 done. The recipe builds its fit call from `ssm_growth_formula()` and its table from `ssm_trajectory()`, the `stopifnot(any(V_xy != 0))` guard is gone, and the script runs to completion with every wave certified and every true `d(t)` inside its interval.
- 2026-09-24: T3 done. The new prose was written to the rules (25 words, no dash, no semicolon, terms glossed at first use), and `tools/prose-sweep.R` on the whole `.Rmd.orig` exits 0 after two long sentences found on the first run were split. Sections 7 to 9 and the References are the pre-rewrite text, unchanged.
- 2026-09-24: claim audit: 52 claims read, 5 corrected — NEWS.md (hidden-chunk count, "five calls" for the second example, "links" for a plain `\code{}` See also), vignettes/growth-ssm-analysis.Rmd.orig (the Overview's "names the next page" against the Wrap-up's "no page follows", and the refusal's place in the call order, which runs after the draws, not before). No numeric claim was wrong. The reader's one re-read of the corrected lines is recorded below.
- 2026-09-24: the reader's re-read found four of five corrections right and the NEWS hidden-chunk sentence still wrong, since the hidden options chunk does compute (the seed, the width, the glmmTMB flag). Reworded to name the two hidden chunks without that claim. No further pass, per the stopping rule.
- 2026-09-24: T6 done. NEWS has a Documentation bullet for the rewrite and the earlier dev-version bullet on the cross-block check now describes the helper's refusal. `document()` regenerated `man/simulated_growth.Rd` with the `\seealso` and no unresolved link. `devtools::check(args = "--no-manual")`: 0 errors, 0 warnings, 0 notes, run on the tree at d26a62d5, before the two prose-only correction commits. Status set to review.
- 2026-09-24: review: seven criteria passed on fresh evidence, gate green, three-lens review logged (0 prior-review, 3 history notes, 22 diff-bug findings, 11 fixed at the gate, 1 follow-up, 9 rejected, 1 resolved).
- 2026-09-24: step-7 approval: m153-growth-vignette-rewrite approved for merge

## Decisions

## Review

Evidence gathered 2026-09-24 at branch head f4a91f8d, master at 6e78a9fc (not moved since the cut).

- AC1: `read_chunks()` from `tools/vignette-echo-sweep.R` on the `.Rmd.orig` reports 2 hidden chunks, at line 10 (`{r, include = FALSE}`) and line 75 (`{r glmmtmb-note, echo = FALSE, results = "asis"}`). Pass.
- AC2: the two `glmmTMB::glmmTMB(` calls (lines 142 and 286) each read `f_glmmTMB$formula` and `f_glmmTMB$dispformula`; both examples run `ssm_growth_data()`, the fit, `ssm_trajectory()` and `ssm_plot_trajectory()`, the second reusing the first's `ssm_growth_formula()` object; the grep for `reshape(`, `mvn_draw`, `lapply(waves` and `data.frame(wave` returns 0 lines. Pass.
- AC3: the `brms-fit` chunk is `eval = FALSE`; `vignettes/growth_brms_draws.rds` is 4000 by 6 with exactly the six `b_` columns and a provenance attribute naming the generator, seed 20260716 and brms 2.23.0; the generator's `brm()` call is the vignette's plus `refresh = 0`. Measured fresh: `x_est` gaps at most 3.3e-4, `y_est` at most 5.0e-4 across the five waves (bound 0.01); intercept-draw correlation 0.0147 against the implied -0.0172, gap 0.032 (bound 0.1). Pass.
- AC4: re-rendered at head; `git status` shows no change to the `.Rmd`; the staleness check reports all 11 vignettes up to date and the width check 112 output lines, all fit. Against master's `.Rmd` the Section 5 `a_*` and `d_*` values are equal at two decimals at every wave (350.30/346.93/353.69 through 14.48/11.14/17.80). Chunk order and 4000 draws unchanged. Pass.
- AC5: `tools/prose-sweep.R` on the whole `.Rmd.orig` exits 0. Pass.
- AC6: `grep -c 'stopifnot(any(V_xy'` returns 0; the trajectory comes from `ssm_trajectory(coef, vcov, times = waves, n_draws = 4000)` at line 108; the script's run to completion is recorded under AC7's background run. Pass pending that run.

Consistency gate, 2026-09-24: `cairn_validate.py` all checks passed (exit 0), no principle changed so `cairn_impact` was skipped. `devtools::document()` at `cli.width = 500` produced no diff and 0 `resolve link` lines. `pkgdown::check_pkgdown()` no problems; the articles index, level map and `vignettes/` agree on 14 pages. CI dependency allowlists in sync with the 15 Suggests. `check-master-red-alert.R` and the dry run both clean; branch protection matches the committed rulesets. Master watches: the newest push runs of `R-CMD-check.yaml` and `test-coverage.yaml` on master (d1823eac, 2026-09-24) both concluded success. README.md is knitted from a README.Rmd it postdates and neither is in the diff. NEWS has the entry (AC7). New files: `data-raw/` and `tools/` are build-ignored, and the `.rds` ships beside `bayesian_ssm_draws.rds` in `vignettes/`.

- AC6 (completed): `Rscript devel/m27-growth-recipe.R` on the head tree ran to completion under glmmTMB, every wave certified and every true `d(t)` inside its interval. Pass.
- AC7: on the head tree (08e56c7c, identical to f4a91f8d outside `cairn/`): `devtools::test()` 0 failures, 12 warnings in unrelated files (`test-pole-values.R`, `test-print-width.R`, `test-ssm_sem.R`), 1 skip, 14298 passes; `devtools::check(args = "--no-manual")` Status: OK; `document()` no diff; NEWS has the Documentation bullet at line 250; `man/simulated_growth.Rd` carries the `\seealso` naming `vignette("growth-ssm-analysis")`. Pass.

Independent review, 2026-09-24, three lenses. Prior-review lens: no prior-review evidence contradicted (archives M123 to M152 and the plain-vignettes ledger read; the PR-comments probe returned an empty list). History lens: no undone intent and no decision contradicted; H1 the printed `any(V != 0)` chunk and its 1e-20 residue figure are gone with the check moved into the helper (intentional, D-064); H2 the "carry this check to your own fits" guidance is gone for the same reason (intentional); H3 the brms intercept-draw correlation 0.0147 has the opposite sign to the implied -0.0172, both within 0.02 of zero and the gap 0.032 inside AC3's 0.1 bound (noted, no text change). Diff-bug lens, 22 findings, none a criterion failure, dispositions proposed at the gate:
- O1 "the caution fires at the degraded wave" now collides with "the caution" naming the REML note. Fix now: "the uncertified mark appears".
- O2 the brms table prints the REML note, which does not describe posterior draws, and the prose only says the note is written for the other shape. Fix now in prose: say the note does not apply to posterior draws and why; the print-side branch is the existing candidate row (M152 O8), extended at hygiene.
- O3 the implement-phase check ran before the last two commits. Resolved: the check above ran on the head tree.
- O4 "a joint fit estimates that covariance, so it is not zero" overstates. Fix now: "and it is almost never exactly zero".
- O5 Section 7's remedies name lme4 and nlme and do not say how a remedy feeds `ssm_trajectory()`. Pre-existing text; follow-up candidate row (a parametric-bootstrap path through `draws =`).
- O6 "Its fixed effects" reads as glmmTMB's after the sentence about glmmTMB, and the line is over 80 columns. Fix now.
- O7 the nlme agreement figures are free-standing on the page (the derived-figures rule). Fix now: a procedural claim pointing at the package's parity test.
- O8 "the same residual variance per coordinate" can read as one shared variance. Fix now.
- O9 the generator adds `refresh = 0` to the shown call, and the provenance names no Stan backend version. Fix now: the generator comment says so; the backend version is not added, since the attribute is written by the fit and a refit is not owed.
- O10 the 168,888-byte `.rds` ships in the tarball although the precomputed `.Rmd` never reads it. Reject: deliberate parity with `bayesian_ssm_draws.rds`, so a reader who runs the page's chunk has the file.
- O11 the echo sweep's search mode flags the echoed `readRDS()`. Reject: not in the verify slot, and the same precedent stands in `advanced-visualization`.
- O12 "No page follows this one" then a pointer to the visualization vignette. Reject: pre-existing Wrap-up text, and the Overview's new wording is accurate.
- O13 "fits a second recipe" for a section that fits nothing. Reject: pre-existing text, unmodified line.
- O14 "exact posterior inference". Reject: pre-existing text, and a claim change is out of scope.
- O15 "will happily summarize" and which helper takes which draws. Reject: pre-existing text.
- O16 "their defaults" is ambiguous since `ssm_growth_data()` has none. Fix now.
- O17 the page says "three calls" where NEWS says four. Fix now: the page drops the count.
- O18 the recipe's containment check ignores a wrapped interval and labels it "CrI". Fix now: circular containment and "interval".
- O19 two NEWS bullets describe the vignette and one cites `data-raw/`. Reject: the earlier bullet records a different, earlier change, and NEWS elsewhere cites `data-raw/` scripts.
- O20 prose source lines over 80 columns. Reject as cosmetic; line 426 is rewrapped under O6.
- O21 `library(ggplot2)` in the hidden chunk is unused. Reject: unmodified line.
- O22 "each row of the wide table". Fix now: "each row of `simulated_growth`".

Gate, 2026-09-24: the user chose the recommended option, the eleven fixes then the merge. Fixed on the branch: O1, O2, O4, O6, O7, O8, O16, O17 and O22 in the vignette source (O22 as "each input row", since the dataset is introduced in the next paragraph; O7 as a procedural claim naming `tests/testthat/test-ssm_growth_parity.R`), O9 in the generator comment, O18 in the recipe (circular containment, "interval"). After the fixes: the page re-rendered, the prose sweep exits 0 after one 26-word sentence was shortened, the width check 112 lines all fit, the recipe to completion with the circular containment TRUE. Follow-up: O5 becomes a candidate row at hygiene. Rejected with reasons as listed: O10, O11, O12, O13, O14, O15, O19, O20, O21. Resolved: O3. Noted: H1, H2, H3.
