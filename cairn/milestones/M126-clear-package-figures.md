# M126: Package figures show every point, interval and axis label clearly

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP5
- **Resolves:** —
- **Surface tier:** user-facing — changes the figures that three exported plot functions draw
- **Branch/PR:** —

## Goal

`plot()` for CPM fits, `ssm_plot_circle()` and `ssm_plot_trajectory()` draw figures in which each interval, point estimate and axis label can be read at vignette size.

## Scope

**In:** `plot.circumplex_cpm()` (`R/cpm_oop.R:355`) draws intervals of zero width as visible marks. `ssm_plot_circle()` (`R/ssm_plot.R:71`) and `plot.circumplex_cpm()` pass an `r_axis_angle` to `coord_circumplex()` that keeps the amplitude axis away from plotted points. The default rule `ssm_r_axis_angle()` (`R/coord_circumplex.R:165`) stays. `ssm_plot_trajectory()` (`R/ssm_trajectory.R:516`, drawn by `ssm_trajectory_ggplot()` at `:561`) and its vignette figure sizes change so that the panels are not squeezed. The help-page doc bug (i) from the former doc-bug candidate row: the roxygen at `R/ssm_trajectory.R:412` says each bound sits at its signed distance from its own estimate, but `ssm_interval_on_branch()` does not place bounds that way. Every precomputed vignette that calls one of the three functions is re-knitted so that its figures show the change. `grep -ln "plot(cpm\|ssm_plot_circle\|ssm_plot_trajectory" vignettes/*.Rmd.orig` lists them, because the staleness guard does not compare figures. The advanced-visualization chunk that builds its canvas by hand sets `r_axis_angle`. NEWS.md.

**Out:** chunk rewrites and prose in the structure, SEM, Bayesian and growth vignettes → M128. Printed reports → M127. A new `r_axis_angle` argument on `ggcircumplex()` → not planned. Hand-built canvases already accept `coord_circumplex(r_axis_angle =)`.

## Acceptance criteria

- [ ] AC1: `plot()` on a `cpm_fit()` result draws an interval mark for every scale whose communality and angle intervals pass `ssm_has_region()` and whose angle span from `ssm_arc_span()` lies in [0, 360). This includes a scale whose angle interval has zero width. It also includes a scale whose communality interval has zero width at 0 or at 1, a scale with both widths zero and communality above 0, and a zero-width angle interval at 0/360. Tests build each of these cases and assert that the built plot holds an interval mark with nonzero drawn length for that scale. A scale with both widths zero and communality 0 draws its point only. Scales that fail the region or span condition keep today's warning and draw a point only.
- [ ] AC2: `ssm_plot_circle()` and `plot()` for CPM fits place the amplitude axis at the midpoint of the widest spoke gap that holds no plotted point estimate. Ties break to the smallest midpoint. A point on a spoke counts as in both adjacent gaps. When every gap holds a point, the current widest-gap rule applies. Tests assert both branches. One test uses displacements 332.4, 355.9 and 17.8 degrees, the three occasions of the advanced-visualization vignette. One test puts points at 0 and at 360 degrees with `octants()` spokes.
- [ ] AC3: In the re-knitted figures `occasions-plot`, `occasions-path` and `occasions-path-wrapper` (advanced-visualization) and `cpm_plot` (evaluating-circumplex-structure), no axis label is drawn over a point estimate or over another label. In `occasions-plot` each data panel is at least as wide as it is tall. An interval mark that overlaps another interval mark because of the data is allowed.
- [ ] AC4: The `ssm_plot_trajectory()` help page describes the placement of each confidence bound as `ssm_interval_on_branch()` in `R/ssm_trajectory.R` computes it.
- [ ] AC5: NEWS.md has an entry for each visible change to `plot.circumplex_cpm()`, `ssm_plot_circle()` and `ssm_plot_trajectory()`. `devtools::test()` and `devtools::check(args = "--no-manual")` report no failure, warning or note that is new relative to master.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3, T5
- AC4 → T4
- AC5 → T6

## Tasks

- [ ] T1: Write failing tests in `tests/testthat/test-cpm_plot.R` for the five zero-width cases in AC1. Then change `plot.circumplex_cpm()` to draw a zero-width angle interval as a radial segment and a zero-width communality interval as an arc with a visible line weight, or to use one shared mark with the same effect. Render each case and look at it (lesson M33).
- [ ] T2: Write a data-aware axis-angle helper beside `ssm_r_axis_angle()`, with tests for AC2's rule, ties, a point on a spoke and the all-gaps-full fallback. Call it from `ssm_plot_circle()` and `plot.circumplex_cpm()`. Update both help pages to state the rule.
- [ ] T3: Make the `occasions-plot` panels at least as wide as tall. First try the chunk's figure size in `advanced-visualization.Rmd.orig:331` and in the growth vignette's `plot` chunks. If that is not enough, change the legend, caption or facet layout in `ssm_trajectory_ggplot()`. Keep `panel.spacing.x` (lesson M50).
- [ ] T4: Fix the roxygen at `R/ssm_trajectory.R:412` against `ssm_interval_on_branch()` (`R/ssm_trajectory.R:58`), then run `devtools::document()`. Revert any `Config/roxygen2/version` change (lesson M85).
- [ ] T5: In `advanced-visualization.Rmd.orig`, set `r_axis_angle` in the `occasions-path` chunk's canvas. After `devtools::install()`, re-knit with `tools/precompute-vignettes.R` every vignette the Scope's grep lists. View each AC3 figure's PNG, and record one verdict per figure as review evidence.
- [ ] T6: Regenerate the changed vdiffr snapshots under `NOT_CRAN=true` (lesson M31), and add the NEWS.md entries. Run `devtools::test()` and `devtools::check(args = "--no-manual")`.

## Work log

- 2026-09-14: created by /milestone-plan (split from one feedback list into M126 to M128). The plan gate chose package changes plus vignette edits over vignette-only fixes, because `ssm_plot_circle()` offers no way to move the amplitude axis. Falsified by a figure that stays unreadable after the package change, or by a user who needs the old axis placement.
- 2026-09-14: plan gate absorbed doc bug (i) of the doc-bug candidate row instead of routing it to /hotfix, because this milestone opens `R/ssm_trajectory.R`.
- 2026-09-14: criteria audit (full mode, fresh [O] reader) found 8 items on this file's draft. All were fixed before writing: AC1's region definition and extra zero-width cases, AC2's tie rule and exact displacements, AC3's recording act moved to T5, and AC5's snapshot promise moved to T6.
- 2026-09-14: second fresh [O] audit of the written criteria found 4 items, all fixed: the re-knit list became a grep over callers, AC1 excludes the zero-size mark at communality 0, AC2 adds points at 0 and 360, and the Scope's line citation was corrected.

## Decisions
