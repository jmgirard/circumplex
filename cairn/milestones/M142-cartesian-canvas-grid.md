# M142: A Cartesian-grid style for the circumplex canvas

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, GP5
- **Resolves:** —
- **Surface tier:** user-facing — exported arguments on `coord_circumplex()` and `ggcircumplex()`
- **Branch/PR:** —

## Goal

Give `coord_circumplex()` a `grid = "cartesian"` mode that draws a plain rim, a labelled crosshair and no rings or spokes, and give `ggcircumplex()` that mode plus `angle_labels` (rim tick marks and labels carrying each angle in degrees), the canvas style of Nagy, Etzel and Lüdtke (2019, Figures 3 and 6).

## Scope

**In:** `coord_circumplex(grid = c("polar", "cartesian"))`. In cartesian mode `CoordCircumplex$render_bg` draws a rim ring at `amax`, two crosshair lines through the origin along displacements 0/180 and 90/270, tick marks and signed numeric labels at every radial break strictly between `center` and `amax` (positive on the 0 and 90 halves, negative on the 180 and 270 halves, the signs being Cartesian coordinates, not amplitudes), no other ring, no spoke and no radial-axis guide. The polar transform is untouched. `ggcircumplex(grid = c("polar", "cartesian"), angle_labels = FALSE)`: `grid` passes to the coord; cartesian mode turns theta tick marks on across the rim (`axis.ticks.theta`, `axis.ticks.length.theta`); `angle_labels = TRUE` formats each theta label as `<label> (<angle>°)`, the angle rounded to the nearest whole degree, and rotates the labels along the radius. Tests, roxygen, DESIGN.md, NEWS.

**Out:** the vignette figures on this canvas → M143. A helper computing the ellipse's tangent angles and distances → M143 vignette code, its export a candidate row. `theme_circumplex()` unchanged. A `style = "nagy"` name (rejected in the work log).

## Acceptance criteria

- [ ] AC1: `grid = "polar"` is the default and leaves every existing canvas unchanged: every vdiffr snapshot in the suite (all nine `tests/testthat/_snaps/` directories holding `.svg` files, `ci_accuracy`, `fit_structure_api`, `ssm_trajectory` and `ssm_trajectory_table` included) passes without regeneration. A `grid` value outside the two aborts through an explicit `stop()` whose message names `grid` (`match.arg()`'s message does not).
- [ ] AC2: For a canvas built with `grid = "cartesian"`, the rendered panel (`ggplotGrob()` of the plot, its panel background and foreground grobs walked by name and class) holds one rim ring at `amax`, two crosshair lines through the origin along displacements 0/180 and 90/270, a tick mark and a signed numeric label at every radial break strictly between `center` and `amax` on each of the four half-axes, and holds no ring below the rim, no spoke and no radial-axis guide. A vdiffr snapshot of `ggcircumplex(octants(), labels = PANO(), grid = "cartesian")` is committed.
- [ ] AC3: `ggcircumplex(angle_labels = TRUE)` labels each theta break as `<label> (<angle>°)` with the angle rounded to the nearest whole degree and rotates the labels along the radius. Any supplied angle equal to 0 or 360 is labelled `(360°)`, never `(0°)` (the LM = 360 invariant). Labels resolved from `instrument =` get the same format. `ggcircumplex(grid = "cartesian")` sets `axis.ticks.theta` and `axis.ticks.length.theta` to visible values and `grid = "polar"` leaves them as today. An `angle_labels` value other than a single `TRUE` or `FALSE` aborts naming it. A vdiffr snapshot of `ggcircumplex(octants(), labels = PANO(), grid = "cartesian", angle_labels = TRUE)` is committed.
- [ ] AC4: The polar transform is untouched by the grid mode: for the five seam cases in `tests/testthat/test-geom_ssm_path.R` and `tests/testthat/test-geom_ssm.R` (a path across the seam, a straddling arc, a seam-adjacent arc, a point at 0 and at 360, a seam-adjacent ellipse), the data-layer grob vertex coordinates in the panel from `ggplotGrob()` on the `grid = "cartesian"` canvas equal those on the default canvas, and the `ggplot_build()` layer data are equal as a side assertion.
- [ ] AC5: Roxygen documents `grid` on `coord_circumplex()` and `grid` and `angle_labels` on `ggcircumplex()` with one example each and states that the negative half-axis labels are Cartesian coordinates, not negative amplitudes. `devtools::document()` leaves no diff. The Coordinate-system and Canvas bullets of `cairn/DESIGN.md` name the cartesian grid mode. `NEWS.md` carries one entry under the development version naming both arguments.
- [ ] AC6: `Rscript -e 'devtools::test()'` is clean and `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors, 0 warnings and no note attributable to this milestone.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2, T5
- AC3 → T1, T3, T5
- AC4 → T1, T2
- AC5 → T4
- AC6 → T5

## Tasks

- [ ] T1: Tests first in `tests/testthat/test-coord_circumplex.R` and `test-ssm_plot.R`: `grid` and `angle_labels` validation with the failure message asserted, the background-grob walk of AC2, the label format of AC3 with angles 0, 360, 11.4 and an `instrument =` case, the five-case seam identity of AC4, and the two vdiffr cases of AC2 and AC3 (snapshots generated at T5 under `NOT_CRAN=true`).
- [ ] T2: `grid` on `coord_circumplex()` (R/coord_circumplex.R:54) stored on the ggproto object, and a `render_bg` override on `CoordCircumplex` (beside `render_fg`, R/coord_circumplex.R:387) that in cartesian mode draws the rim ring, crosshair, tick marks and signed labels from the `panel.grid.major`, `axis.ticks.r` and `axis.text.r` theme elements and suppresses the radial-axis guide. (RB tripwire: irreversible-api)
- [ ] T3: `ggcircumplex(grid, angle_labels)` (R/ssm_plot.R:657): label formatting after `resolve_circumplex_labels()` with the 0-or-360 rule, theta tick theme lines in cartesian mode, radial rotation through `guide_axis_theta()`.
- [ ] T4: Roxygen for both functions, `Rscript -e 'devtools::document()'`, the two DESIGN.md bullets, the NEWS entry.
- [ ] T5: Render-and-inspect pass (LESSONS M33): a PNG of the cartesian canvas at `cpm_fit(jz2017, scales = PANO(), angles = octants())`'s angles with `angle_labels = TRUE`, compared by eye against Nagy Figure 3's right panels (`cairn/references/sources/nagy2019.pdf` p. 410). Regenerate the two new snapshots. Run `devtools::test()` and `devtools::check(args = "--no-manual")`.

## Work log

- 2026-09-20: created by /milestone-plan. Lineage: the M140 figures Jeff judged unlike Nagy's. Planned with M143.
- 2026-09-20: plan gate chose an exported canvas style over vignette-only code (Jeff's choice, against a vignette-only recommendation) because readers should get the look in one call; falsified by no reader building the figure outside the vignette.
- 2026-09-20: plan chose a `grid` argument on `coord_circumplex()`/`ggcircumplex()` over a separate `ggcircumplex_cartesian()` constructor or a `style = "nagy"` value, because the coord already owns the panel furniture and one canvas keeps `amax` single-owned; falsified by a user needing rim ticks in polar mode or the crosshair without ticks, which would split the switch into two arguments.
- 2026-09-20: criteria audit ran twice in full mode ([O] reader, fresh context). Round one: 5 FIX applied, 5 DECIDE settled by the planner. Round two on the split plan: 6 FIX applied (snapshot domain widened to all nine directories, AC4 bound to grob vertices, five seam cases closed, `stop()` naming `grid`), DECIDE settled: 0 or 360 labels as 360°, signed labels documented as Cartesian coordinates.

## Decisions

## Review
