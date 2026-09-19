# M140: The visualization vignette draws a fitted quasi-circumplex, measure vectors and the correlation function from existing layers

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — a shipped vignette section on the pkgdown site
- **Branch/PR:** —

## Goal

Add a section to `advanced-visualization` that builds three figures from a `cpm_fit()` and an `ssm_analyze()` result with the layers the package already exports: the estimated scale angles as rim ticks, measure locations as vectors from the origin, and the fitted correlation function against angular separation.

## Scope

**In:** one new section in `vignettes/advanced-visualization.Rmd.orig` (after "6. Composing custom layers", before "7. Trajectories"), re-rendered `.Rmd` and figures via `tools/precompute-vignettes.R`; prose that names what each figure's angles come from; a NEWS line.

**Out:** any new exported function (a `cpm_plot_curve()` helper for `corfun` → a candidate row, promoted if readers ask); the confidence-ellipse layer → M141; covariate locations on a *free-angle* circumplex (SPMC-E γ/δ) → the "Covariate extension of the CPM" candidate row; edits to the introductory or intermediate vignettes.

## Acceptance criteria

- [ ] AC1: `vignettes/advanced-visualization.Rmd.orig` has a new `##` section whose echoed chunks, run after the vignette's setup chunk in a fresh `Rscript` session (the AC4 procedure), produce a `ggcircumplex()` canvas with one short radial `geom_ssm_path()` segment per scale at that scale's `Angle` from a `cpm_fit()` result on `jz2017`, with the `Angle_theory` values as the labelled breaks.
- [ ] AC2: The same section produces a second canvas with one `geom_ssm_path()` segment per measure from the origin to the measure's `a_est`/`d_est` from `ssm_analyze()` on `jz2017`, on the same rim ticks as AC1.
- [ ] AC3: The same section produces a linear `ggplot()` line of the `cpm_fit()` object's `corfun` over separations 0° to 180°, with the observed inter-scale correlations from `matrices$R` plotted as points at the absolute angular separation of each pair's estimated `Angle`, computed inline as `abs(((a - b + 180) %% 360) - 180)`.
- [ ] AC4: The new section contains no chunk with `echo = FALSE` or `include = FALSE`, and its echoed chunks, extracted with `knitr::purl()` and run after the vignette's setup chunk in a fresh `Rscript` session with `--vanilla`, complete without error; that run is the procedure AC1–AC3 cite.
- [ ] AC5: The section's prose contains these four sentences' key phrases, each present verbatim in the rendered `.Rmd`: (i) "the ticks are the angles `cpm_fit()` estimated, and the labelled spokes are the theoretical angles"; (ii) "the spoke and the tick for the same scale come from different models, and the vectors come from a third: the SSM computed on the theoretical angles"; (iii) "the points are observed correlations and the line is the fitted correlation function"; (iv) "a vector's length is the measure's amplitude, not its correlation with the latent circumplex".
- [ ] AC6: `Rscript tools/check-vignette-width.R`, `Rscript tools/check-vignette-staleness.R` and `Rscript tools/check-pkgdown-vignettes.R` each exit 0 on the branch; `devtools::check()` reports 0 errors, 0 warnings, 0 notes; `git diff master -- vignettes/advanced-visualization.Rmd.orig` touches only lines inside the new section and the setup chunk.
- [ ] AC7: `NEWS.md` has one entry under the development heading naming the new section without a milestone number.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T2
- AC4 → T5
- AC5 → T2, T4
- AC6 → T3, T6
- AC7 → T6

## Tasks

- [ ] T1: Write the section's echoed chunks in `advanced-visualization.Rmd.orig`: fit `cpm_fit(jz2017, scales, boots = ...)` once (seeded), build the tick data frame (two rows per scale, amplitude `c(0.94, 1)` at `Angle`, `group = Scale`), the vector data frame from `ssm_analyze(jz2017, scales, measures = c("NARPD", "AVPD", ...))$results` (two rows per measure: origin and `a_est`/`d_est`), and the correlation-function frame from `corfun` and `matrices$R` with pairwise separations from `results$Angle` via the inline modular expression (`angle_dist()` is not exported and is signed).
- [ ] T2: Write the prose carrying the four AC5 phrases; re-render with `Rscript tools/precompute-vignettes.R advanced-visualization`; open each PNG and look at it (M33 lesson); check the file tail bytes (M34 lesson).
- [ ] T3: Run the three named `tools/check-vignette-*.R` guards; fix width overruns by narrowing prints, never by exemption.
- [ ] T4: Add `tests/testthat/test-vignette-latent-figures.R`: reads the installed vignette via `system.file("doc", ...)` with the covr/CRAN skip pattern from `test-plot-cran-guards.R`, asserts the four AC5 phrases.
- [ ] T5: Run the AC4 procedure (`knitr::purl()` on the section's chunks, `Rscript --vanilla` after the setup chunk); then plant two defects — a name from the vignette's mid-page hidden `people` chunk and a name shadowing an export — in an echoed chunk, see the run fail on each, revert, and record all three runs in the work log.
- [ ] T6: NEWS entry; `devtools::document()` no diff; `devtools::check()`; `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-19: created by /milestone-plan.
- 2026-09-19: criteria audit ran in full mode (fresh-context reader) and returned 13 findings across M140/M141; all fixed at the gate (unsatisfiable self-containment script replaced by a no-hidden-chunks structural claim plus a purl run; guard misapplied to sections replaced by a git-diff bound; non-exported `angle_dist()` replaced by an inline expression; AC1 split into three; phrase test given its phrases; instrument clauses moved to tasks) except the ellipse centre, posed as a gate question.
- 2026-09-19: plan gate chose two milestones (this docs-only one, M141 for the geom) over one because the vignette figures ship independently and one milestone crossed both sizing tripwires; falsified by M141 needing to reopen this section's prose beyond adding a subsection.
- 2026-09-19: plan gate chose vignette-only ggplot code on `corfun` over exporting `cpm_plot_curve()` because an export is a GP4 commitment for eight lines of ggplot; falsified by a reader request or a second vignette needing the same figure (candidate row holds it).
- 2026-09-19: plan chose one canvas combining `cpm_fit()` ticks with SSM vectors plus caveat prose over separate canvases because the papers' figure and the model distinction are the teaching point (GP5 carried by AC5); falsified by a review finding that the prose still lets a reader read a vector as a latent location.

## Decisions

## Review
