# M126: Package figures show every point, interval and axis label clearly

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP5
- **Resolves:** —
- **Surface tier:** user-facing — changes the figures that three exported plot functions draw
- **Branch/PR:** m126-clear-package-figures

## Goal

`plot()` for CPM fits, `ssm_plot_circle()` and `ssm_plot_trajectory()` draw figures in which each interval, point estimate and axis label can be read at vignette size.

## Scope

**In:** `plot.circumplex_cpm()` (`R/cpm_oop.R:355`) draws intervals of zero width as visible marks. `ssm_plot_circle()` (`R/ssm_plot.R:71`) and `plot.circumplex_cpm()` pass an `r_axis_angle` to `coord_circumplex()` that keeps the amplitude axis away from plotted points. The default rule `ssm_r_axis_angle()` (`R/coord_circumplex.R:165`) stays. `ssm_plot_trajectory()` (`R/ssm_trajectory.R:516`, drawn by `ssm_trajectory_ggplot()` at `:561`) and its vignette figure sizes change so that the panels are not squeezed. The help-page doc bug (i) from the former doc-bug candidate row: the roxygen at `R/ssm_trajectory.R:412` says each bound sits at its signed distance from its own estimate, but `ssm_interval_on_branch()` does not place bounds that way. Every precomputed vignette that calls one of the three functions is re-knitted so that its figures show the change. `grep -ln "plot(cpm\|ssm_plot_circle\|ssm_plot_trajectory" vignettes/*.Rmd.orig` lists them, because the staleness guard does not compare figures. The advanced-visualization chunk that builds its canvas by hand sets `r_axis_angle`. NEWS.md.

**Out:** chunk rewrites and prose in the structure, SEM, Bayesian and growth vignettes → M128. Printed reports → M127. A new `r_axis_angle` argument on `ggcircumplex()` → not planned. Hand-built canvases already accept `coord_circumplex(r_axis_angle =)`.

## Acceptance criteria

- [x] AC1: At the default `amax`, `plot()` on a `cpm_fit()` result draws an interval mark for every scale whose communality and angle intervals pass `ssm_has_region()`, whose angle span from `ssm_arc_span()` lies in [0, 360), and whose communality upper bound is above 0. This includes a scale whose angle interval has zero width, including one stored at the seam as 0/0, 360/360 or 360/0. It includes a scale whose communality interval has zero width above 0 (tested at 0.5 and at 1), with an angle interval that crosses the seam and with one whose lower bound is negative. It includes a scale with both widths zero and `comm_lci` = `comm_uci` > 0, at a seam angle and with its communality estimate away from that bound; its mark is centered on the interval, not on the point. It includes a zero-width angle interval with a communality interval from 0 to a value above 0. For each of these cases a test asserts that the built plot holds, for that scale, an interval mark with a line width above 0 and a length above 0 in canvas coordinates after the coordinate transform. Scales that fail the region, span or upper-bound condition get a warning that names each scale and its reason, and draw a point only when they have a location.
- [x] AC2: `ssm_plot_circle()` and `plot()` for CPM fits place the amplitude axis at the midpoint of the widest spoke gap that holds no plotted point estimate. Ties break to the smallest midpoint. A point on a spoke counts as in both adjacent gaps. When every gap holds a point, the current widest-gap rule applies. Tests assert both branches. One test uses displacements 332.4, 355.9 and 17.8 degrees, the three occasions of the advanced-visualization vignette. One test puts points at 0 and at 360 degrees with `octants()` spokes.
- [x] AC3: In the re-knitted figures `occasions-plot`, `occasions-path` and `occasions-path-wrapper` (advanced-visualization) and `cpm_plot` (evaluating-circumplex-structure), no axis label is drawn over a point estimate or over another label. In `occasions-plot` each data panel is at least as wide as it is tall. An interval mark that overlaps another interval mark because of the data is allowed.
- [x] AC4: The `ssm_plot_trajectory()` help page describes the placement of each confidence bound as `ssm_interval_on_branch()` in `R/ssm_trajectory.R` computes it.
- [x] AC5: NEWS.md has an entry for each visible change to `plot.circumplex_cpm()`, `ssm_plot_circle()` and `ssm_plot_trajectory()`. `devtools::test()` and `devtools::check(args = "--no-manual")` report no failure, warning or note that is new relative to master.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3, T5
- AC4 → T4
- AC5 → T6

## Tasks

- [x] T1: Write failing tests in `tests/testthat/test-cpm_plot.R` for the zero-width cases and probes in AC1. Then change `plot.circumplex_cpm()` to draw a zero-width angle interval as a radial segment, a zero-width communality interval as an arc with a visible line weight, and a both-zero interval as a short cap of fixed drawn length. Render each case and look at it (lesson M33).
- [x] T2: Write a data-aware axis-angle helper beside `ssm_r_axis_angle()`, with tests for AC2's rule, ties, a point on a spoke and the all-gaps-full fallback. Call it from `ssm_plot_circle()` and `plot.circumplex_cpm()`. Update both help pages to state the rule.
- [x] T3: Make the `occasions-plot` panels at least as wide as tall. First try the chunk's figure size in `advanced-visualization.Rmd.orig:331` and in the growth vignette's `plot` chunks. If that is not enough, change the legend, caption or facet layout in `ssm_trajectory_ggplot()`. Keep `panel.spacing.x` (lesson M50).
- [x] T4: Fix the roxygen at `R/ssm_trajectory.R:412` against `ssm_interval_on_branch()` (`R/ssm_trajectory.R:58`), then run `devtools::document()`. Revert any `Config/roxygen2/version` change (lesson M85).
- [x] T5: In `advanced-visualization.Rmd.orig`, set `r_axis_angle` in the `occasions-path` chunk's canvas. After `devtools::install()`, re-knit with `tools/precompute-vignettes.R` every vignette the Scope's grep lists. View each AC3 figure's PNG, and record one verdict per figure as review evidence.
- [x] T6: Regenerate the changed vdiffr snapshots under `NOT_CRAN=true` (lesson M31), and add the NEWS.md entries. Run `devtools::test()` and `devtools::check(args = "--no-manual")`.

## Work log

- 2026-09-14: created by /milestone-plan (split from one feedback list into M126 to M128). The plan gate chose package changes plus vignette edits over vignette-only fixes, because `ssm_plot_circle()` offers no way to move the amplitude axis. Falsified by a figure that stays unreadable after the package change, or by a user who needs the old axis placement.
- 2026-09-14: plan gate absorbed doc bug (i) of the doc-bug candidate row instead of routing it to /hotfix, because this milestone opens `R/ssm_trajectory.R`.
- 2026-09-14: criteria audit (full mode, fresh [O] reader) found 8 items on this file's draft. All were fixed before writing: AC1's region definition and extra zero-width cases, AC2's tie rule and exact displacements, AC3's recording act moved to T5, and AC5's snapshot promise moved to T6.
- 2026-09-14: second fresh [O] audit of the written criteria found 4 items, all fixed: the re-knit list became a grep over callers, AC1 excludes the zero-size mark at communality 0, AC2 adds points at 0 and 360, and the Scope's line citation was corrected.
- 2026-09-14: implement started on branch m126-clear-package-figures. Question gate chose lines plus a fixed-length cap for zero-width CPM intervals, drawn inside `plot.circumplex_cpm()` only.
- 2026-09-14: amendment (substantive, narrowing): AC1 excludes a communality interval whose upper bound is 0, because every angle at communality 0 maps to the one canvas center point (checked by transforming x = 10 and 80 at y = 0). T1 wording follows.
- re-audit: AC1 (full) — 6 items: data-space vs canvas length, redundant reason clause, silent drop at upper bound 0 (GP2), no interior zero-width case, missing seam and negative-bound probes. All fixed in the rewrite.
- re-audit: AC1 (full) — 9 items: seam storage forms, cap placement, point needs a location, warning names its reason, default `amax`, line width floor, assertions on every probe, "for that scale". All fixed; the user accepted the final wording at a gate (second-line stop).
- 2026-09-14: T1 done. `plot.circumplex_cpm()` draws zero-width intervals with a `geom_segment()` layer and names each point-only scale with its reason. Six new tests failed before the change and pass after it. A render of radial, arc and cap cases looked right. CPM snapshots regenerated; `devtools::test()` 0 failures.
- 2026-09-14: T2 done. `ssm_r_axis_angle_clear()` in `R/coord_circumplex.R` feeds both plot functions, and both help pages state the rule. The clean CPM fit's estimates sit about 1e-7 degrees off their spokes, so its axis moves to 157.5 and its snapshots changed, as did `cross-zero circle`. `devtools::test()` 0 failures.
- 2026-09-14: T3 done by figure size alone. `occasions-plot` goes from `fig.height = 6` to 3.2, which measured 1.77 in wide by 1.70 in tall per panel at 7 in wide (gtable arithmetic, local render). The growth vignette's two-panel `plot` chunks are already wider than tall and stay unchanged. No change to `ssm_trajectory_ggplot()`.
- 2026-09-14: T4 done. The help text now states the lower bound's counterclockwise offset below the estimate and the upper bound's arc-span offset, as read from `ssm_interval_on_branch()`. `document()` left `Config/roxygen2/version` unchanged. Trajectory tests pass, including the existing wider-than-half-turn tests that back the new sentence.
- 2026-09-14: T5 done. The `occasions-path` chunk now builds its canvas from parts with `coord_circumplex(amax = 0.8, r_axis_angle = 67.5)`, matching the vignette's no-second-coord note. After `devtools::install()`, all six grep-listed vignettes were re-knitted. Only four figure PNGs changed, and the other `.Rmd` diff is one elapsed-time line.
- 2026-09-14: T5 figure verdicts (PNG viewed). `occasions-plot` PASS: 3 panels in one row, each wider than tall, no label over a point. `occasions-path` PASS: axis labels in the 45-90 gap, points near LM clear. `occasions-path-wrapper` PASS: axis labels in the 45-90 gap, clear of the three points and wedges. `cpm_plot` PASS: axis in the JK-LM gap with no label on a point, PA radial line and NO rim arc visible.
- 2026-09-14: T6 done. The changed snapshots were regenerated under `NOT_CRAN=true` in T1 and T2 (`cpm_plot` x2, `cross-zero circle`). NEWS.md has three improvement entries and two documentation entries. `devtools::test()` FAIL 0, WARN 9 (the same Hessian, zero-SD and lavaan warnings seen before this branch's changes). `devtools::check(args = "--no-manual")` 0 errors, 0 warnings, 0 notes.
- claim audit: 22 claims read, 1 corrected — tests/testthat/test-cpm_plot.R
- 2026-09-14: the claim auditor noted that the `occasions-path` figure's spoke labels changed from degrees to PA to NO, which NEWS did not mention. The NEWS vignette entry now says so (figure viewed in T5).
- 2026-09-14: all tasks checked, status set to review.

## Decisions

- 2026-09-14 (implement gate): a zero-width CPM interval is drawn as a line along its nonzero side, and a both-zero interval with communality above 0 as a short cap of fixed drawn length, centered on the interval. The marks live in a line layer inside `plot.circumplex_cpm()`. `geom_ssm_arc()` keeps dropping zero-width angle spans, so SSM circle plots and user plots do not change.

## Review

Evidence gathered 2026-09-14 on branch head 0b7a36ec; master had not moved since the branch was cut.

- AC1: PASS. Each case has a test in `tests/testthat/test-cpm_plot.R`. Each test asserts a line width above 0 and a chord length above 1e-3 in canvas coordinates after `coord$transform()`. The cases are: a radial line for PA (interior) and for LM stored as 0/0, 360/360 and 360/0. An arc at communality 0.5 and 1, with angles 350/10 and with a negative lower bound. A both-zero cap at 0/0 and 360/360, at communality 0.64 against estimate 0.8464, with its midpoint at the interval angle. A radial line from communality 0 to 0.81. The full suite passed (see AC5). A probe set BC inestimable, DE at 0/360 and FG to zeta bounds -0.3/0. It gave one warning that named "BC (inestimable interval), DE (full-circle angle interval), FG (communality interval at 0)". With BC's angle also NA, the plot drew 4 wedges, 1 line mark and 7 of 8 points, and the warning did not name BC, as on master. `cpm_fit()` fills `Angle` from the optimizer's `theta` (`R/cpm_fit.R:1712`), so a scale with no angle is outside the criterion's `cpm_fit()` domain.
- AC2: PASS. `ssm_r_axis_angle_clear()` (`R/coord_circumplex.R:181`) sets the axis in both `ssm_plot_circle()` and `plot.circumplex_cpm()`, read in the diff. The test in `test-coord_circumplex.R` asserts 67.5 for points 332.4, 355.9 and 17.8 with `octants()`. It asserts 67.5 for a point at 0, at 360 and at both. It asserts 112.5 for a point on the 45 spoke and a tie break to the lower midpoint. With a point in every gap, it asserts the widest-gap fallback. `test-cpm_plot.R` and `test-ssm_plot.R` assert both branches through each plot function. All passed in the full suite.
- AC3: PASS. PNGs viewed this session. `occasions-plot` (1008 x 460 px): three panels in one row, the Elevation data panel about 256 px wide by 245 px tall without its strip, no label over a point or another label. `occasions-path`: amplitude labels 0.2 to 0.8 run along the 45-90 gap, clear of the three points near LM and of the spoke labels. `occasions-path-wrapper`: labels 0.25 to 1.00 in the same gap, clear of the three points and their marks. `cpm_plot`: labels run along the LM-JK gap, and no label touches a point, wedge or other label.
- AC4: PASS. The help text at `R/ssm_trajectory.R:412` places the lower bound below the estimate by the counterclockwise angle from lower bound to estimate. It places the upper bound above the lower bound by the counterclockwise angle from lower to upper bound. `ssm_interval_on_branch()` (`R/ssm_trajectory.R:58`) computes `lo <- branch - ((est - lci) %% 360)` and `hi <- lo + ssm_arc_span(lci, uci)`, which matches. `document()` gave no diff.
- AC5: PASS. NEWS.md has three entries under Minor improvements (CPM zero-width marks, the CPM warning reasons, the axis placement for both functions) and two Documentation entries (the trajectory help page, the vignette figures). `devtools::test()` on the branch: FAIL 0, WARN 9, SKIP 1, PASS 9424. The same run on a `git archive master` copy: FAIL 0, WARN 9, SKIP 1, PASS 9319, with the same warning and skip sites. `devtools::check(args = "--no-manual")`: 0 errors, 0 warnings, 0 notes.

Consistency gate: all PASS. `cairn_validate.py` exit 0. No principle text changed, so `cairn_impact` was skipped. `document()` gave no diff and 0 `resolve link` lines. `pkgdown::check_pkgdown()` found no problems. README.Rmd and top-level files are untouched. NEWS has no milestone numbers. Master push runs of `R-CMD-check.yaml` and `test-coverage.yaml` are `success` at c2606e41, the newest code-bearing master commit. Later master commits touch only `cairn/`. `check-master-red-alert.R`, `master-red-alert-dryrun.R` and `check-branch-protection.R` exited 0.

Independent review (three fresh reviewers). Blame-history: no findings. Prior-review: GitHub probe found no inline review comments. Findings, ranked, with the disposition proposed at the gate:

- F1 (diff-bug): the axis rule's tolerance is 1e-9 degrees, but fitted CPM estimates sit about 1e-7 degrees off their spokes. So numerical noise decides which gap is empty. The clean fit's axis goes to 157.5, and a sign flip on another platform moves it and breaks the local `cpm circle plot` snapshots. The test pins `Angle <- Angle_theory` and does not test fitted angles. Proposed: fix now.
- F2 (diff-bug): the "fixed length" cap is limited to 45 degrees of half-width, so near the centre it shrinks (0.048 at communality 0.25, 0.0057 at 0.01 on a rim radius of 0.4). The help page and NEWS call it fixed-length. Proposed: fix now by correcting the wording.
- F3 (diff-bug): zero width is found by exact equality, so a width of 1e-12 draws an invisible wedge with no mark. Proposed: reject. Exact zero comes from the fixed reference angle and from the 0 and 1 clamps, and a near-zero width drew the same on master.
- F4 (diff-bug): `advanced-visualization.Rmd.orig` still says the axis goes in the widest gap (line 151) and that `ssm_plot_circle()` is `ggcircumplex()` plus two geoms so rebuilt figures line up (line 458). The wrapper now sets the axis from the data. Proposed: fix now.
- F5 (diff-bug): NEWS says the axis labels "no longer sit on top of a point", which is false when every gap holds a point. Proposed: fix now.
- F6 (diff-bug): a CPM fit with more than 8 scales now gets the Set2 palette warning twice, and marks for scales 9 and later have no colour. Proposed: reject. The fill scale already had this limit on master.
- F7 (diff-bug): no test asserts the "inestimable interval" or "full-circle angle interval" reason text. Proposed: fix now (a probe in the AC1 line shows the text is correct).
- F8 (diff-bug): the both-zero cap test puts the interval at LM's own estimate angle, so it cannot tell a cap centred on the interval from one centred on the point. Proposed: fix now by moving the estimate's angle away.
- F9 (diff-bug): a scale with no angle is dropped with no warning. Proposed: reject. The behavior is unchanged from master and `cpm_fit()` cannot produce it (see AC1).
- F10 (diff-bug): with `amax` below a communality, marks draw outside the rim. Proposed: reject. Wedges already do this on master.
- F11 (diff-bug): a zero-width interval stored as 0/360 is reported as a full-circle interval. Proposed: reject. By the package convention `ssm_arc_span(0, 360)` is 360, a full circle.
- F12 (diff-bug): the vignette hard-codes `r_axis_angle = 67.5`. Proposed: reject. The chunk teaches the argument, and 67.5 is what the rule gives for this data.
- F13 (prior-review): the hand-built `occasions-path` canvas drops `ggcircumplex()`'s blank extent layer and y-scale name, the kind of chunk rewrite that broke in an earlier vignette review. Proposed: reject. The figure was viewed and renders with the correct range and no axis title.
