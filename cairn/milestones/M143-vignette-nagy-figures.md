# M143: The visualization vignette's latent-circumplex figures follow Nagy, Etzel and Lüdtke (2019)

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M142
- **Driving RR:** —
- **Principles touched:** IP2, IP3, GP5
- **Resolves:** —
- **Surface tier:** user-facing — a vignette section and its figures
- **Branch/PR:** m143-vignette-nagy-figures

## Goal

Redraw the circle figures of the visualization vignette's section 7 on the M142 cartesian canvas in the form of Nagy, Etzel and Lüdtke (2019, Figures 3 and 6): estimated angles only on the rim, measures as labelled points joined to the origin, one annotated measure, and the confidence ellipse with its tangent and distance lines.

## Scope

**In:** `vignettes/advanced-visualization.Rmd.orig` section 7: chunks `latent-ticks`, `latent-vectors`, a new `latent-measure`, a new `ellipse-lines` computation and `ellipse-figure`, their prose, the References entry, the phrase test, the source note `cairn/references/nagy2019.md`, NEWS. The `latent-corfun` figure is unchanged.

**Out:** a tangent-line aesthetic on `geom_ssm_ellipse()` → stays parked on the "Covariate extension" ROADMAP row. An exported helper for the tangent angles and distances → candidate row. Nagy's left-hand panels (a covariate's correlation against scale angle) → not planned.

## Acceptance criteria

- [ ] AC1: In the rendered `vignettes/advanced-visualization.Rmd`, the section "The latent circumplex from a CPM fit" builds the plots of chunks `latent-ticks`, `latent-vectors` and `latent-measure` on `ggcircumplex(angles = cpm$results$Angle, labels = <scale names>, grid = "cartesian", angle_labels = TRUE)`. For each of those three plot objects (the section's chunks purled and run under `Rscript --vanilla`), the theta breaks equal `cpm$results$Angle`, each break label contains that scale's name and its estimated angle rounded to the nearest degree, and the `Angle_theory` column enters no layer's data (every layer's `data` inspected). The `latent-ticks` plot holds only the canvas's `geom_blank` layer.
- [ ] AC2: The `latent-vectors` plot draws each of the five measures from `ssm_analyze(jz2017, scales = PANO(), measures = c("NARPD", "ASPD", "HISPD", "AVPD", "SCZPD"))` as one point at `(a_est, d_est)`, one path from amplitude 0 to that point with no arrow, and one text label equal to the measure's `Label`. The `latent-measure` plot draws one of those measures as a point, a path from the origin, an arc path sweeping increasing displacement from 0 to `d_est` at one fixed amplitude, and two text labels giving `d_est` rounded to the nearest degree with a degree sign and `a_est` rounded to two decimals. In both plots the plotted amplitudes and displacements equal the `ssm$results` columns.
- [ ] AC3: An echoed chunk `ellipse-lines` computes, from the one-row `ssm_ellipse_data()` result and the ellipse's `level`, the two tangent angles from the origin to the ellipse (degrees in [0, 360)) and the nearest and farthest distances from the origin to the ellipse boundary (both strictly positive). When the origin lies inside the ellipse the tangent angles are `NA` and the nearest distance is the minimum boundary radius. A testthat test purls that chunk from the `.Rmd.orig`, evaluates it in a fresh environment, and verifies it against two oracles. Oracle (i): on the vignette's ellipse and on three further ellipses whose centres sit at displacements 45, 200 and 358 degrees (the last with a tangent pair straddling 0/360), each tangent angle is within 0.1° of the corresponding extreme polar angle over the vertices of the `GeomSsmEllipse` outline built with `n = 3600`, and each distance within 1e-3 of the extreme vertex radius. The test records that oracle (i) is a discretization of the shipped outline. Oracle (ii): on circular ellipses (`var_x = var_y`, `cov_xy = 0`) at radius-to-centre ratios 0.2, 0.5 and 0.9, centre displacements 30 and 350, and levels 0.5 and 0.95, the tangent angles equal the centre's displacement ± asin(r / c) and the distances equal c − r and c + r, each within 1e-6. On one such ellipse containing the origin the tangent angles are `NA` and the nearest distance equals r − c. Oracle (ii) is independent of `GeomSsmEllipse` and carries the IP3 burden.
- [ ] AC4: The `ellipse-figure` plot is built on `ggcircumplex(octants(), labels = PANO(), grid = "cartesian", angle_labels = TRUE)` and draws the ellipse from `ssm_ellipse_data(post)`, the point at `(a_est, d_est)`, one text label, two dashed paths from the origin along the two tangent angles reaching the rim, two dotted paths from the origin to the nearest and farthest boundary points, and the wedge from `post$results` at `alpha` no greater than 0.15. The dashed paths' displacements equal the chunk's tangent angles and the dotted paths' end amplitudes equal its two distances.
- [ ] AC5: The rendered page states, in sentences pinned verbatim by `tests/testthat/test-vignette-latent-figures.R`, that (a) the ticks and their labels sit at the angles `cpm_fit()` estimated and the theoretical angles appear only in the table above the figure, (b) a measure's distance from the origin is its SSM amplitude computed on the theoretical angles and not a loading on the latent circle, (c) the tangent lines bound the directions the ellipse spans under the normal approximation and are not the displacement interval the wedge draws, and (d) the figures follow Nagy, Etzel and Lüdtke (2019), whose entry is in the page's References. The phrases "the ticks are the angles `cpm_fit()` estimated, and the labelled spokes are the theoretical angles" and "the spoke and the tick for the same scale come from different models, and the vectors come from a third: the SSM computed on the theoretical angles" leave the test with the figures that made them true.
- [ ] AC6: `Rscript tools/precompute-vignettes.R advanced-visualization` re-renders the page on the branch head. Then `Rscript tools/check-vignette-staleness.R`, `Rscript tools/check-vignette-width.R`, `Rscript tools/check-vignette-split.R` and `Rscript tools/prose-sweep.R` exit 0, the section hides no chunk (the existing test), `Rscript -e 'devtools::test()'` is clean, `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors, 0 warnings and no note attributable to this milestone, and `NEWS.md` carries one entry under the development version stating that the section's figures follow Nagy et al. (2019).

## Coverage

- AC1 → T2
- AC2 → T2, T3
- AC3 → T4
- AC4 → T5
- AC5 → T2, T3, T5, T6
- AC6 → T6

## Tasks

- [x] T1: Author `cairn/references/nagy2019.md` from the source-note template with its `INDEX.md` line: page anchors for Figures 1 (p. 406), 3 (p. 410), 4 (p. 416) and 6 (p. 421) and Appendix B and C (pp. 426–427), each anchor naming its channel (`pdftotext` text or a visual read of the rendered page). A figure convention is a rasterized read (M42-D1).
- [x] T2: `latent-ticks` and `latent-vectors` on the cartesian canvas, prose rewritten with estimated angles only. The comparison with `Angle_theory` moves to the table paragraph. No `echo = FALSE` chunk.
- [x] T3: `latent-measure`: one measure with the increasing-displacement arc and the two value labels, prose naming Nagy Figure 3 as its model.
- [x] T4: Test first: `tests/testthat/test-vignette-ellipse-lines.R` purling `ellipse-lines` with both oracles and the origin-inside case. Then the chunk: tangent directions from the quadratic in the direction vector, distances from the boundary extremes, `NA` tangents when the origin's Mahalanobis distance is within the level's radius. The no-oracle RB tripwire does not fire because oracle (ii) is closed-form.
- [ ] T5: `ellipse-figure` with ellipse, point, label, dashed tangents to the rim, dotted distance paths and the light wedge. Prose for phrases (c) and (d). References entry for Nagy et al. (2019).
- [ ] T6: Phrase test update, `tools/precompute-vignettes.R advanced-visualization`, the four guards, NEWS, render-and-inspect pass on the four circle figures against Nagy pp. 410 and 421 (LESSONS M33), `devtools::test()` and `devtools::check(args = "--no-manual")`.

## Work log

- 2026-09-20: created by /milestone-plan. Lineage: extends M140 (its ticks and vectors figures are replaced) and M141 (its ellipse figure is redrawn). Depends on M142.
- 2026-09-20: plan gate chose estimated angles only on the rim over faint theoretical marks because it is what Nagy's figures show; falsified by readers missing the estimated-versus-theoretical comparison the table now carries.
- 2026-09-20: plan gate chose both measure figures (five labelled points and one annotated measure) over either alone. Jeff's choice, against the five-point recommendation.
- 2026-09-20: plan gate chose the tangent and distance lines as echoed vignette code with the wedge kept light, over no lines or an unchanged figure; the geom aesthetic stays parked. Falsified by a reader asking for the lines outside the vignette, which promotes the helper candidate row.
- 2026-09-20: plan chose the increasing-displacement arc for `latent-measure` (Nagy Figure 3 sweeps from 0° counterclockwise) over the short way; falsified by a measure past 180° whose arc reads wrong on inspection.
- 2026-09-20: criteria audit in full mode ([O] reader), two rounds. Round two on this file: FIX applied (one distance definition across both oracles, `geom_blank` wording), DECIDE settled (arc direction, oracle (i) kept as a discretization with oracle (ii) independent).
- 2026-09-20: /milestone-implement started; branch `m143-vignette-nagy-figures` cut from pushed master. Implement gate: `latent-measure` annotates SCZPD (187°, past 180 so the sweeping arc is exercised; nearest Nagy's 205° example) and both measure figures are black with text labels and no legend, as Nagy draws them. Both the recommended options.
- 2026-09-20: T1 done. `cairn/references/nagy2019.md` and its INDEX line: captions and p. 415 body text by `pdftotext`, figure geometry by a visual read of rendered pp. 406, 410, 416, 421; Appendices B and C anchored by heading only. Figure 4's scenario covariances are not printed, so its values are no oracle. The simple-english lint hook flags cairn record prose (em dashes, the template's one-line Extraction status); left as the template shapes it.
- 2026-09-20: T2 and T3 done in one edit of section 7. `latent-ticks` is the canvas alone (`canvas` object reused by the next two chunks, amax 0.3); `latent-vectors` draws five black points, spokes with `group = Label`, and `geom_text()` labels placed by hand-chosen `hjust`/`vjust` columns (NARPD and ASPD at 109° and 116° collided under a radial rule); `latent-measure` draws SCZPD with the arc at amplitude 0.08 and the two value labels (the displacement label moved off the vertical axis after a render-and-inspect pass). New `tests/testthat/test-vignette-latent-plots.R` purls the four chunks through a new `vignette_chunk()` helper and checks AC1 and AC2 on the plot objects; the theoretical-angle comparison now lives in the table paragraph. The rendered page is not yet re-rendered, so the old phrase test still passes against the stale render until T6.
- 2026-09-20: T4 done, test first. `tests/testthat/test-vignette-ellipse-lines.R` was red (chunk absent), then green at 65 expectations once the echoed `ellipse-lines` chunk landed: `ellipse_lines(ellipse, level)` returns rows tangent_1, tangent_2, nearest, farthest with `displacement` and `amplitude` columns; tangents from the double-root quadratic in the slope (or its reciprocal, whichever leading coefficient is larger), ordered so the counterclockwise arc from the first to the second holds the centre; distances from a 3600-point grid refined by `optimize()`. Oracle (i) on the vignette ellipse and centres at 45, 200 and 358 degrees (the last straddling the seam) plus an origin-inside case; oracle (ii) on 12 circular ellipses plus an origin-inside circle. Check discrimination: a planted wrong-matrix defect (`outside * S` for `outside * A`) turned 10 expectations red. Absolute tolerances throughout. One test edit went through a python script rather than the Edit tool, against the tracking rules; noted, not repeated.

## Decisions

## Review
