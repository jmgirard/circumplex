# M153: The growth vignette on the helper workflow

- **Status:** planned
- **Priority:** high
- **Depends on:** M152
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — a shipped vignette and the pkgdown site
- **Branch/PR:** —

## Goal

Rewrite the growth vignette so that every code chunk is shown and the workflow is five short calls: `ssm_growth_data()`, `ssm_growth_formula()`, the pasted fit, `ssm_trajectory()` and `ssm_plot_trajectory()`.

## Scope

**In:** `vignettes/growth-ssm-analysis.Rmd.orig` rewritten on the M151 and M152 exports. glmmTMB stays the shown engine. A short section shows the nlme call from the builder and the brms call with a precomputed draws file, following the Bayesian vignette's pattern (D-015). `data-raw/growth-brms-draws.R` writes `vignettes/growth_brms_draws.rds` under a seed. The pre-render, staleness and width checks. `devel/m27-growth-recipe.R` switched to the helpers. A prose pass under `cairn/references/plain-vignettes.md`.

**Out:** the Bayesian vignette itself is untouched. The shared-intercept coverage cell stays its own candidate row. Any new statistic is out, since the page changes form and not claims.

## Acceptance criteria

- [ ] AC1: `tools/vignette-echo-sweep.R`'s definition of a hidden chunk finds exactly two hidden chunks in `vignettes/growth-ssm-analysis.Rmd.orig`. They are the unnamed `include = FALSE` options chunk at the top and the chunk labelled `glmmtmb-note`.
- [ ] AC2: Each of the vignette's two worked examples reaches `ssm_plot_trajectory()` through `ssm_growth_data()`, `ssm_growth_formula()`, one `glmmTMB::glmmTMB()` call and `ssm_trajectory()`. The fit call reads the builder's elements, so a grep finds `$formula` and `$dispformula` inside each `glmmTMB::glmmTMB(` call. A grep of the `.Rmd.orig` for `reshape(`, `mvn_draw`, `lapply(waves` and `data.frame(wave` returns nothing.
- [ ] AC3: The brms section fits `simulated_growth` in a chunk marked `eval = FALSE`. Its draws come from `vignettes/growth_brms_draws.rds`, which `data-raw/growth-brms-draws.R` writes under a seed and which holds only the six `b_` columns. `ssm_trajectory(draws = )` summarizes them. At each wave, `x_est` and `y_est` lie within 0.01 of the glmmTMB table's. The correlation of the `x` and `y` intercept draws lies within 0.1 of the value the glmmTMB covariance implies. The work log records the measured gaps.
- [ ] AC4: After the rewrite is committed, `tools/precompute-vignettes.R` regenerates the shipped `.Rmd`, and `tools/check-vignette-staleness.R` and `tools/check-vignette-width.R` pass on it. The baseline is the `.Rmd` at the commit the branch was cut from. The comparison runs on the precompute machine with chunk order and draw count unchanged. There, the rendered glmmTMB trajectory table's `a_*` and `d_*` values equal the baseline's to two decimals.
- [ ] AC5: `tools/prose-sweep.R` reports no sentence over 25 words on the whole page.
- [ ] AC6: `devel/m27-growth-recipe.R` builds its trajectory through `ssm_trajectory()`. Its guard `stopifnot(any(V_xy != 0))` is removed in favour of the helper's refusal. The script runs to completion under glmmTMB.
- [ ] AC7: The verify slot is clean. `devtools::check()` is clean. NEWS.md has an entry for the vignette. `man/simulated_growth.Rd` carries a `\seealso` link to the vignette.

## Coverage

- AC1 → T1
- AC2 → T1
- AC3 → T2
- AC4 → T4
- AC5 → T3
- AC6 → T5
- AC7 → T6

## Tasks

- [ ] T1: Rewrite Sections 3 to 6 on the five calls. Delete every hidden chunk except the two exceptions. Keep the joint-fit teaching, the VarCorr print and the cross-block check as prose around the helper's refusal.
- [ ] T2: Write `data-raw/growth-brms-draws.R`, run it locally, commit the `.rds`, and add the engines section (nlme call shown, brms chunk `eval = FALSE`, draws summarized).
- [ ] T3: Prose pass under the plain-vignettes rules. Run `tools/prose-sweep.R`.
- [ ] T4: Commit, run `tools/precompute-vignettes.R`, then the staleness and width checks. Compare the rendered table to the baseline and record the result.
- [ ] T5: Switch `devel/m27-growth-recipe.R` to the helpers and run it.
- [ ] T6: NEWS entry, the `@seealso` on `simulated_growth`, `devtools::document()`, `devtools::check()`.

## Work log

- 2026-09-24: created by /milestone-plan.
- 2026-09-24: criteria audit ran in full mode on an [O] reader. Findings absorbed here: hidden chunks are defined by the echo sweep with the two exceptions named. The staleness check runs after the commit with the baseline named. The prose check is page-wide. The recipe guard is named by its code. The `\seealso` link is added rather than assumed.
- 2026-09-24: a second audit pass on the changed criteria: AC2 now requires the fit to read the builder's elements. AC3 names the dataset and keeps the draws file to six columns. It compares `x_est`, `y_est` and the intercept-draw correlation instead of amplitude.
- 2026-09-24: plan gate chose a precomputed brms demonstration inside this vignette over a fourth milestone. brms is never run on CI (D-015), so the demonstration is a committed draws file, the same pattern as the Bayesian vignette.

## Decisions

## Review
