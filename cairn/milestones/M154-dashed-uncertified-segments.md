# M154: Dashed displacement segments at uncertified time points

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2
- **Resolves:** —
- **Surface tier:** user-facing — an exported plotting function's rendering and two vignettes
- **Branch/PR:** m154-dashed-uncertified-segments

## Goal

On the displacement panel of `ssm_plot_trajectory()`, draw each line segment that touches an uninterpretable time point as dashed. The path through such a point then reads as an unsupported interpolation and the unwrapped branch stays visible.

## Scope

**In:** The displacement-panel line in `ssm_trajectory_ggplot()` (`R/ssm_trajectory.R`) becomes one segment per pair of consecutive time points. A segment is dashed when either endpoint's verdict is not `TRUE` (`FALSE` or `NA`) and solid when both are `TRUE`. The line type joins the existing `Displacement interpretable` legend as one key per verdict. An `NA` verdict on a defined displacement is drawn as a hollow point. Today its shape is `NA` and the point is silently not drawn. Tests on both entry paths. The help page, the growth vignette's Section 6 and the advanced-visualization vignette's trajectory paragraph. Both pre-rendered vignettes re-rendered. A NEWS entry.

**Out:** Any change to the certification rule (D-007), the unwrap, or the interval placement. A lighter or faded encoding (rejected at the plan gate, see the work log). The elevation, x, y and amplitude panels, which carry no verdict. The circular `geom_ssm_path()` canvas, which draws no per-point verdict today (a candidate row if a reader asks).

## Acceptance criteria

- [ ] AC1: On the displacement panel, a segment joins each pair of consecutive time points. If either endpoint's verdict is not `TRUE` (`FALSE` or `NA`), the segment is dashed. If both verdicts are `TRUE`, the segment is solid. Each segment's y endpoints equal the two points' unwrapped `est`. Evidence: `ggplot2::layer_data()` on the displacement segment layer. On the table path with `traj_table()`, the verdicts `T,T,F,T,T` give solid, dashed, dashed, solid. The dashed seam segment from 355 to 362 carries no jump. The verdicts `F,T,T,T,T` give dashed, solid, solid, solid. The verdicts `T,F,F,T,T` give dashed, dashed, dashed, solid. The verdicts `T,NA,T,T,T` give dashed, dashed, solid, solid. On the occasions path, `traj_fit()` forced uncertified at occasion 2 and separately at the last occasion gives the predicted pattern. On a grouped fit where one group's occasion is uncertified, the other group's segments all stay solid.
- [ ] AC2: With `drop_xy = FALSE`, every other panel's line is one solid line. A table with no verdict (no `certified` column, or one entirely `NA`) draws every displacement segment solid with no line-type legend. A time point with undefined displacement is spanned by no segment. Evidence: `layer_data()` shows no displacement segment with `x < t_gap < xend` on the table path (`d_est` `NA` at wave 2) and on the occasions path (flat occasion 2). The four other panels' line layer has one solid group each.
- [ ] AC3: A time point with a defined displacement and an `NA` verdict is drawn as a hollow point. Evidence: `layer_data()` on the displacement point layer under `T,NA,T,T,T` shows five points, the second with shape 1.
- [ ] AC4: The `Displacement interpretable` legend shows exactly two keys. `TRUE` is a filled point on a solid line and `FALSE` is a hollow point on a dashed line. Each key holds one point grob and one line grob, drawn in black. Both keys are drawn even when no time point is uncertified. Evidence: `legend_key_glyphs()` extended to read each key's line type and colour. It is asserted on a fully certified fit, on the uncertified table fixture, and on a grouped fit.
- [ ] AC5: Three text sites describe the dashed segments: `?ssm_plot_trajectory`, Section 6 of `vignette("growth-ssm-analysis")` and the trajectory paragraph of `vignette("advanced-visualization")`. Each says that a dashed segment touches a time point whose displacement is not interpretable. Each says that the direction of change along a dashed segment is not to be read. Both vignettes' rendered `.Rmd` files and figures are regenerated from `.orig`. The growth vignette's Section 6 figure shows dashed segments on either side of wave 2. NEWS.md carries the entry.
- [ ] AC6: `devtools::document()` produces no diff. `devtools::test()` passes. The vdiffr baselines live under `_snaps/ssm_trajectory/`, `_snaps/ssm_trajectory_table/` and `_snaps/ssm_trajectory_helper/`. Each changed baseline differs from its predecessor only in the segments AC1 predicts dashed and in the legend keys of AC4. `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T2
- AC4 → T1, T2
- AC5 → T3, T4
- AC6 → T5

## Tasks

- [x] T1: Tests first, red before T2. In `tests/testthat/test-ssm_trajectory_table.R`, add the four verdict patterns of AC1. Read them through `layer_data()` on the segment layer: line type per segment, and y endpoints equal to unwrapped `est`. Add the no-verdict cases of AC2 (no column, and an all-`NA` column). Add the gap case reading segments rather than `p$data`. Add the `NA`-verdict hollow point of AC3 and the other-panel solid-line check with `drop_xy = FALSE`. In `tests/testthat/test-ssm_trajectory.R`, add occasion 2 and last-occasion uncertified, the grouped case (`traj_fit(grouping = )`), and the flat-occasion gap read through segments. Extend `legend_key_glyphs()` (`tests/testthat/helper-ssm-legend.R:32`) to return each key's line grobs with their `lty` and colour, and assert AC4 on the three fits.
- [x] T2: In `ssm_trajectory_ggplot()` (`R/ssm_trajectory.R`, from the `geom_line` call), build a segment frame for the displacement rows per group in time order. Pair each row with its successor in the ordered frame. An `NA` displacement row then pairs with nothing and the gap stays. Set `Interpretable = isTRUE(cert[i]) & isTRUE(cert[i+1])`. Draw the frame with `geom_segment(aes(linetype = Interpretable))` and `scale_linetype_manual(name = "Displacement interpretable", values = c("TRUE" = "solid", "FALSE" = "dashed"), limits = c("TRUE", "FALSE"), drop = FALSE)`, with `show.legend = TRUE`. Keep `geom_line` for the other panels' rows only. Map the point shape from `Certified %in% TRUE` so that an `NA` verdict is hollow. Remove `linetype = 0` from the shape guide's `override.aes` and give both guides the same title so that they merge into one legend with black keys. Under no verdict (all `NA`), draw the displacement rows with the plain `geom_line` and no line-type scale, as today.
- [x] T3: Roxygen on `ssm_plot_trajectory()` (the hollow-point paragraph, `R/ssm_trajectory.R:428`), then `devtools::document()`. NEWS.md bullet under the development heading, with no milestone number.
- [x] T4: Prose in `vignettes/growth-ssm-analysis.Rmd.orig` Section 6 (the "Uncertified waves are drawn as hollow points" paragraph) and `vignettes/advanced-visualization.Rmd.orig:842`. Run `Rscript tools/precompute-vignettes.R growth-ssm-analysis` and the same for `advanced-visualization` with the package installed. Inspect the rendered figures by eye. LESSONS M33 records that data fences pass a figure that reads wrong.
- [ ] T5: Regenerate the changed vdiffr baselines under `NOT_CRAN=true` by deleting them first (LESSONS M31). Read each SVG diff against AC6. Then run `devtools::test()` and `devtools::check()`.

## Work log

- 2026-09-24: created by /milestone-plan.
- 2026-09-24: criteria audit ran in full mode ([O], user-facing tier). It returned six findings. They were an uncovered `NA`-verdict case, gap evidence read from the input frame, missing first-point and adjacent-pair probes, the legend override that blanks key lines, a stale advanced-visualization legend, and circular vdiffr wording. All were applied, and the `NA` case was posed at the gate.
- 2026-09-24: plan gate chose dashed segments over a lighter (alpha) line. Dashed joins the point-shape legend as one key per verdict and opacity cannot. Falsified by a reader report that the dashed key reads as a second series.
- 2026-09-24: plan gate chose fail-closed on an `NA` verdict (hollow point, dashed segments) over an undrawn point. A silently missing point hides a row (GP2). Falsified by a caller who relies on `NA` meaning "omit this point".
- 2026-09-24: plan gate chose re-rendering both trajectory vignettes over the growth vignette alone. Every trajectory legend changes. Falsified by nothing. A stale figure is a defect.
- 2026-09-24: implement started on `m154-dashed-uncertified-segments`. No question gate: the plan left nothing open.
- 2026-09-24: T1 done. New helper `tests/testthat/helper-ssm-trajectory.R` reads the built segment layer, and `legend_key_lines()` reads each key's line grobs. Eleven new tests across the two files, all red against the old line layer.
- 2026-09-24: T2 done. `ssm_trajectory_segments()` builds the per-pair frame. The displacement panel is always a segment layer, with a linetype scale only under a verdict, so the no-verdict baseline changed in form (four `<line>` elements for one `<polyline>`) with no visible change. A second `override.aes` on the merged guide warned and was dropped. Six vdiffr baselines regenerated under `NOT_CRAN=true`; the uncertified table baseline reads dashed, dashed, dashed, solid as AC1 predicts. Render inspected by eye.
- 2026-09-24: T3 done. Help-page paragraph extended, `document()` clean with no link warning, NEWS bullet under Minor improvements and fixes.
- 2026-09-24: T4 done. Both vignettes re-rendered against the installed tree. Rendered text differs only in the new paragraphs. Three trajectory figures changed and were inspected: the growth Section 6 figure shows dashed segments either side of wave 2, and both legends show the line types. Four advanced-visualization figures that draw no trajectory changed bytes in the re-render (LESSONS M112) and were restored from HEAD.
- 2026-09-24: claim audit: 22 claims read, 2 corrected — R/ssm_trajectory.R, tests/testthat/test-ssm_trajectory_table.R. The dashed legend key read as solid at the default key width because the dash gap fell under the point, so the theme now widens `legend.key.width` to 2.4 lines. A test comment named the wrong reason for assigning the verdict by name. Baselines regenerated and both vignettes re-rendered again after the width change.

## Decisions

## Review
