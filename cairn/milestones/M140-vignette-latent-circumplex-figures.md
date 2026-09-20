# M140: The visualization vignette draws a fitted quasi-circumplex, measure vectors and the correlation function from existing layers

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — a shipped vignette section on the pkgdown site
- **Branch/PR:** `m140-vignette-latent-circumplex-figures` · https://github.com/jmgirard/circumplex/pull/173

## Goal

Add a section to `advanced-visualization` that builds three figures from a `cpm_fit()` and an `ssm_analyze()` result with the layers the package already exports: the estimated scale angles as rim ticks, measure locations as vectors from the origin, and the fitted correlation function against angular separation.

## Scope

**In:** one new section in `vignettes/advanced-visualization.Rmd.orig` (after "6. Composing custom layers", before "7. Trajectories"), re-rendered `.Rmd` and figures via `tools/precompute-vignettes.R`; prose that names what each figure's angles come from; a NEWS line.

**Out:** any new exported function (a `cpm_plot_curve()` helper for `corfun` → a candidate row, promoted if readers ask); the confidence-ellipse layer → M141; covariate locations on a *free-angle* circumplex (SPMC-E γ/δ) → the "Covariate extension of the CPM" candidate row; edits to the introductory or intermediate vignettes.

## Acceptance criteria

- [x] AC1: `vignettes/advanced-visualization.Rmd.orig` has a new `##` section whose echoed chunks, run after the vignette's setup chunk in a fresh `Rscript` session (the AC4 procedure), produce a `ggcircumplex()` canvas with one short radial `geom_ssm_path()` segment per scale at that scale's `Angle` from a `cpm_fit()` result on `jz2017`, with the `Angle_theory` values as the labelled breaks.
- [x] AC2: The same section produces a second canvas with one `geom_ssm_path()` segment per measure from the origin to the measure's `a_est`/`d_est` from `ssm_analyze()` on `jz2017`, on the same rim ticks as AC1.
- [x] AC3: The same section produces a linear `ggplot()` line of the `cpm_fit()` object's `corfun` over separations 0° to 180°, with the observed inter-scale correlations from `matrices$R` plotted as points at the absolute angular separation of each pair's estimated `Angle`, computed inline as `abs(((a - b + 180) %% 360) - 180)`.
- [x] AC4: The new section contains no chunk with `echo = FALSE` or `include = FALSE`, and its echoed chunks, extracted with `knitr::purl()` and run after the vignette's setup chunk in a fresh `Rscript` session with `--vanilla`, complete without error; that run is the procedure AC1–AC3 cite.
- [x] AC5: The section's prose contains these four sentences' key phrases, each present verbatim in the rendered `.Rmd`: (i) "the ticks are the angles `cpm_fit()` estimated, and the labelled spokes are the theoretical angles"; (ii) "the spoke and the tick for the same scale come from different models, and the vectors come from a third: the SSM computed on the theoretical angles"; (iii) "the points are observed correlations and the line is the fitted correlation function"; (iv) "a vector's length is the measure's amplitude, not its correlation with the latent circumplex".
- [x] AC6: `Rscript tools/check-vignette-width.R`, `Rscript tools/check-vignette-staleness.R` and `Rscript tools/check-pkgdown-vignettes.R` each exit 0 on the branch; `devtools::check()` reports 0 errors, 0 warnings, 0 notes; `git diff master -- vignettes/advanced-visualization.Rmd.orig` touches only lines inside the new section, the setup chunk, the number prefix of each `##` heading after the new section, the Overview paragraph that maps the sections (the one beginning "This vignette works through each of these"), including its re-wrapping and the sentence added for the new section, with its existing sentences unchanged except for section numbers and line breaks, and one `## References` entry for each source the new section cites that the list did not already carry.
- [x] AC7: `NEWS.md` has one entry under the development heading naming the new section without a milestone number.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T2
- AC4 → T5
- AC5 → T2, T4
- AC6 → T3, T6
- AC7 → T6

## Tasks

- [x] T1: Write the section's echoed chunks in `advanced-visualization.Rmd.orig`: fit `cpm_fit(jz2017, scales, ci_method = "analytic")` once (gate choice; only point estimates are drawn), build the tick data frame (two rows per scale, amplitude `c(0.94, 1)` at `Angle`, `group = Scale`), the vector data frame from `ssm_analyze(jz2017, scales, measures = c("NARPD", "AVPD", ...))$results` (two rows per measure: origin and `a_est`/`d_est`), and the correlation-function frame from `corfun` and `matrices$R` with pairwise separations from `results$Angle` via the inline modular expression (`angle_dist()` is not exported and is signed).
- [x] T2: Write the prose carrying the four AC5 phrases; re-render with `Rscript tools/precompute-vignettes.R advanced-visualization`; open each PNG and look at it (M33 lesson); check the file tail bytes (M34 lesson).
- [x] T3: Run the three named `tools/check-vignette-*.R` guards; fix width overruns by narrowing prints, never by exemption.
- [x] T4: Add `tests/testthat/test-vignette-latent-figures.R`: reads the installed vignette via `system.file("doc", ...)` with the covr/CRAN skip pattern from `test-plot-cran-guards.R`, asserts the four AC5 phrases.
- [x] T5: Run the AC4 procedure (`knitr::purl()` on the section's chunks, `Rscript --vanilla` after the setup chunk); then plant two defects — a name from the vignette's mid-page hidden `people` chunk and a name shadowing an export — in an echoed chunk, see the run fail on each, revert, and record all three runs in the work log.
- [x] T6: NEWS entry; `devtools::document()` no diff; `devtools::check()`; `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-19: created by /milestone-plan.
- 2026-09-19: criteria audit ran in full mode (fresh-context reader) and returned 13 findings across M140/M141; all fixed at the gate (unsatisfiable self-containment script replaced by a no-hidden-chunks structural claim plus a purl run; guard misapplied to sections replaced by a git-diff bound; non-exported `angle_dist()` replaced by an inline expression; AC1 split into three; phrase test given its phrases; instrument clauses moved to tasks) except the ellipse centre, posed as a gate question.
- 2026-09-19: plan gate chose two milestones (this docs-only one, M141 for the geom) over one because the vignette figures ship independently and one milestone crossed both sizing tripwires; falsified by M141 needing to reopen this section's prose beyond adding a subsection.
- 2026-09-19: plan gate chose vignette-only ggplot code on `corfun` over exporting `cpm_plot_curve()` because an export is a GP4 commitment for eight lines of ggplot; falsified by a reader request or a second vignette needing the same figure (candidate row holds it).
- 2026-09-19: plan chose one canvas combining `cpm_fit()` ticks with SSM vectors plus caveat prose over separate canvases because the papers' figure and the model distinction are the teaching point (GP5 carried by AC5); falsified by a review finding that the prose still lets a reader read a vector as a latent location.
- 2026-09-19: implement started; branch `m140-vignette-latent-circumplex-figures` cut from the synced default branch.
- 2026-09-19: question gate chose five spread measures (NARPD, ASPD, HISPD, AVPD, SCZPD) for the vector figure, analytic CPM intervals (only point estimates are drawn; a minor edit to T1's `boots = ...`), and showing the ill-conditioned-Hessian warning inside a precompute volatile-numbers region rather than hiding it.
- 2026-09-19: substantive amendment (AC6): `tests/testthat/test-vignette-frame.R` requires consecutive `##` numbering and an Overview naming every heading, so inserting section 7 renumbers sections 7–9 and edits the Overview's section map; the mini gate chose widening AC6's diff bound to those edits over appending the section last or returning to plan. No defect return is on record, so the return-adjacent rule did not apply.
- 2026-09-19: re-audit: AC6 (full) — returned one finding: the "section map" gloss excluded the Overview paragraph's re-wrapping and its added sentence; wording fixed to name the paragraph by its opening phrase.
- 2026-09-19: re-audit: AC6 (full) — second reader cleared the fixed wording on all six questions and returned two refinements (bound the paragraph's existing sentences to number and line-break changes; permit a `## References` entry for a newly cited source, since the draft cites Browne 1992); the stop reached, both went to the user, who adopted both. Amended AC6 text landed as written above.
- 2026-09-20: T1–T3 done. Section 7 "The latent circumplex from a CPM fit" written with four echoed chunks (`latent-fit` in a volatile-numbers region, `latent-ticks`, `latent-vectors`, `latent-corfun`); the corfun figure also draws `matrices$Phat` as hollow points because under unit scaling `Phat_ij = Zeta_i * Zeta_j * corfun(sep)` (checked to 6e-16), so observed points sit below the line by the communality product and the prose says so. Sections 7–9 renumbered to 8–10, Overview map gained one sentence, Browne (1992) added to References. Re-rendered; all three PNGs opened and read correctly; `.Rmd` tail bytes clean. `ssm_analyze()` in the new section consumes RNG, so the later occasions bootstrap CIs and two occasions PNGs re-rendered with different draws (point estimates unchanged). Width and pkgdown guards exit 0; the staleness guard reads stale until this render is committed. `devtools::test()`: 0 failures, 11327 passes, 12 pre-existing lavaan warnings.
- 2026-09-20: T4 done. `tests/testthat/test-vignette-latent-figures.R` reads the rendered `.Rmd` via `vignette_source()` (skips where absent), collapses whitespace, asserts the four AC5 phrases; a second block reads the `.Rmd.orig` (source tree only; chunk options do not survive rendering) and asserts no `echo = FALSE`/`include = FALSE` fence among at least four fences in the section. Discrimination: one phrase mutated on its own line in the rendered page failed the test (1 failure at line 27), restored copy passed (7 passes); a first mutation attempt matched nothing because the phrase wraps across lines, which is why the test collapses whitespace.
- 2026-09-20: T5 done. AC4 procedure (`knitr::purl()` of the section's four chunks after the `setup` chunk, `Rscript --vanilla`): clean source exits 0. Plant 1 (`nrow(people)` in `latent-ticks`, a name from the hidden section-6 chunk): exit 1, `Error: object 'people' not found`. Plant 2 first tried `PANO <- NULL` before `PANO()` and did NOT fail, because R skips non-function bindings on a call lookup; replanted as `jz2017 <- NULL` before `cpm_fit(jz2017, ...)` (the dataset export shadowed): exit 1, `Error: Supply exactly one of \`data\` or \`cormat\`.`. Plants ran on scratch copies; the working copy was never modified.
- 2026-09-20: claim audit: 29 claims read, 1 corrected — vignettes/advanced-visualization.Rmd.orig, vignettes/advanced-visualization.Rmd, tests/testthat/test-vignette-latent-figures.R, NEWS.md. The correction: the cross-referenced page is titled "CPM Fits at a Boundary", not "CPM Boundary Fits"; fixed in source and rendered copy, re-read once by the same reader as correct.
- 2026-09-20: T6 in progress. NEWS entry written; `devtools::document()` produced no diff; `pkgdown::check_pkgdown()` found no problems; `devtools::check(args = "--no-manual")` still running at this checkpoint (it built from the tree before the one-title fix, which changes prose only).
- 2026-09-20: T6 done. `devtools::check(args = "--no-manual")`: Status: OK (0 errors, 0 warnings, 0 notes). All tasks checked; status set to review.
- 2026-09-20: review checkpoint (partial). AC1 to AC5 and AC7 verified with fresh evidence and ticked. Consistency gate green apart from `devtools::check()`, still running. Three fresh-context reviewers running. AC6 and the gate presentation follow.
- 2026-09-20: review fix-now work from the three-lens review landed (Heywood and NA-interval prose, PA fixed-angle consequence, eight sentence splits, signed residual, `Zeta` column, shape legend on the corfun figure, `pair_idx` rename, `ggcircumplex()` canvases, test guard). Vignette re-rendered. AC1 to AC5 and AC7 re-verified on the fixed tree. Findings and dispositions in the Review section. Full check re-running.
- 2026-09-20: gate triage: O5 kept the added hollow-points sentence (no amendment); O8/S1 rejected, no follow-up. step-7 approval: m140-vignette-latent-circumplex-figures approved for merge.
- 2026-09-20: PR #173 opened after approval. CI wait hit the harness ceiling: macOS job failed in dependency install (pak could not extract the CRAN RcppArmadillo binary, a corrupt download unrelated to the branch); ubuntu and windows still pending, matrix, pkgdown and vignette-precompute green. Watcher stopped. Next: re-run the failed macOS job once the run completes, then resume `/milestone-review M140` for the merge.
- 2026-09-20: resume: PR #173 OPEN; conversation read empty (no reviews, comments or threads); macOS job re-run after its corrupt-download failure. step-7 approval: m140-vignette-latent-circumplex-figures approved for merge (re-posed).
- 2026-09-20: merge held at the user's choice: the macOS CI job fails on every re-run because CRAN serves the RcppArmadillo 15.6.0-1 macOS binary as a zstd archive that the runner's pak cannot extract (external, not the branch). Required checks are green. A /hotfix to the workflow goes first; then resume `/milestone-review M140` on green. Approval stands.

## Decisions

## Review

Fresh evidence on 2026-09-20, branch `m140-vignette-latent-circumplex-figures` at `20396822`. The default branch did not move since the cut. Procedure for AC1 to AC4: the section's four chunks were purled with `knitr::purl()` after the `setup` chunk and run with `Rscript --vanilla` from the repo root. A second run evaluated the same purled code and inspected the three `ggplot` objects it produced.

- AC1: verified. The first plot uses `CoordCircumplex` with one `GeomSsmPath` layer over 16 rows (8 scales, 2 rows each, amplitudes 0.94 and 1). Each scale's displacement equals its `Angle` in the `cpm_fit()` result. The x scale's breaks equal `Angle_theory` and its labels equal `PANO()`.
- AC2: verified. The second plot has two `GeomSsmPath` layers. The vector layer holds 10 rows for 5 measures, each measure two rows from amplitude 0 to its `a_est` at `d_est` from `ssm_analyze()`. The tick layer's data is identical to AC1's and the breaks equal `Angle_theory`.
- AC3: verified. The third plot is `CoordCartesian` with `GeomLine` and two `GeomPoint` layers. The line data spans separations 0 to 180 and equals `cpm$corfun()` at each. The 28 observed points equal the upper triangle of `matrices$R`, placed at the inline modular separation of each pair's estimated `Angle` (range 18.8 to 179.5 degrees). The hollow points equal `sqrt(comm_i * comm_j) * corfun(sep)`, as the prose claims.
- AC4: verified. The section's four fences carry no `echo = FALSE` or `include = FALSE`. The purled run exited 0 with one warning (the ill-conditioned Hessian the prose names) and no error.
- AC5: verified. All four phrases match verbatim in the rendered `.Rmd` after whitespace collapse. `test-vignette-latent-figures.R` passes 7 expectations with 0 failures.
- AC6: verified on the fixed tree at `e67657a0`. The width, staleness and pkgdown guards each exit 0. `devtools::check(args = "--no-manual")` reports Status: OK (0 errors, 0 warnings, 0 notes). The source diff against master has five hunks. They are the Overview paragraph, the new section with the renumbered heading that follows it, two heading renumbers, and one Browne (1992) References entry. The setup chunk is untouched. A sentence comparison of the Overview paragraph shows one added sentence and ten existing sentences that differ only in section numbers and line breaks.
- AC7: verified. One entry under `# circumplex (development version)` names "The latent circumplex from a CPM fit". No `M1nn` token appears in NEWS.

Independent review, 2026-09-20, three fresh-context lenses. Prior-review lens [S]: no prior-review evidence on the touched files (18 archives read, the PR-comment probe returned nothing), zero findings. Findings below are ranked as the reviewers ranked them, with the disposition taken at the gate.

- O1 (diff-bug): the fit is a Heywood-type solution (`details$heywood` is TRUE, NO's `Zeta` is 1) and the four printed columns hide the note. Fix now: the prose names the boundary and the note, and points to the boundary page for both.
- O2: PA's tick sits on its spoke because its angle is fixed, and the other ticks are read relative to it. Fix now: two sentences added.
- O3: nine sentences over the 25-word prose rule on a page clean at the cut (`tools/prose-sweep.R` 0 to 9). Fix now for eight. The ninth is AC5 phrase (ii), 27 words by the criterion's own text, so it stays. Rejected for that one sentence.
- O4: "that gap is part of the model, not a misfit" attributed the whole gap to attenuation. Fix now: "part of that gap is attenuation, not misfit".
- O5: AC5 phrase (iii) "the points are observed correlations" reads as mislabelling the hollow points. Put to the user at the gate. Applied disposition: a sentence directly after the phrase says the hollow points are reproduced, not observed. The alternative is an amendment return rewording AC5(iii).
- O6: the prose said the fit "uses analytic confidence intervals" while every interval is `NA`. Fix now: the prose says the intervals are not drawn and all come back `NA`.
- O7: `tools/vignette-echo-sweep.R` flags five frame-building lines in the echoed chunks. Rejected with reason: AC4 forbids hidden chunks by the plan gate's choice, and the frames are the teaching content here. The sweep is not wired into CI or the test suite as a vignette gate.
- O8 and S1 (history lens): the new `ssm_analyze()` call consumes RNG, so section 8's bootstrap intervals and two of its PNGs re-rendered with different draws. The proposed fix, a `set.seed()` at the occasions chunk, edits lines outside AC6's diff bound. Put to the user at the gate. Applied disposition: rejected. The introduction vignette teaches the seed-once semantics, and point estimates are unchanged.
- O9: the prose sent readers to square roots of `Communality` when `Zeta` is that column. Fix now: the chunk prints `Zeta` and the prose names it.
- O10: the two point sets in the corfun figure were indistinguishable. Fix now: a shape legend (Observed filled, Reproduced hollow). Figure re-rendered and read.
- O11: the sentence said the warning caused the BLAS sensitivity. Fix now: reworded.
- O12: "vertical distance" dropped the residual's sign. Fix now: "the filled point minus the hollow point".
- O13 and S2: the no-hidden-chunk test reads the `.Rmd.orig` and skips under R CMD check, and a renamed heading errors in `min(integer(0))`. Fix now for the guard. Chunk options do not survive rendering, so the source-tree-only scope is the accepted category that `test-vignette-echo-sweep.R` established. Logged here as that classification.
- O14: `pairs` shadowed `base::pairs`. Fix now: renamed `pair_idx`.
- O15: AC1 says a `ggcircumplex()` canvas but the chunks composed the canvas from parts. Fix now: both canvases are `ggcircumplex(angles = cpm$results$Angle_theory, labels = PANO(), amax = )`, which the function accepts. The code now matches the criterion as written.

Consistency gate, 2026-09-20. `cairn_validate.py` exit 0, all checks pass, no advisory fired. No DESIGN.md principle changed, so `cairn_impact.py` was skipped. `devtools::document()` at `cli.width = 500` printed no `resolve link` line and left no diff in `NAMESPACE`, `man/` or `R/RcppExports.R`. README is untouched by the branch. `pkgdown::check_pkgdown()` found no problems. NEWS carries the entry (AC7). Master watches: the newest push run of `R-CMD-check.yaml` and of `test-coverage.yaml` on master (`0c37cddb`, 2026-09-18) both concluded `success`. `check-master-red-alert.R`, `master-red-alert-dryrun.R` and `check-branch-protection.R` each exit 0.
