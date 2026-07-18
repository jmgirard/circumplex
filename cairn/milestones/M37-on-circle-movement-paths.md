# M37: On-circle movement paths across occasions

- **Status:** review
- **Priority:** normal
- **Depends on:** M31, M32, M33
- **Principles touched:** —
- **Branch/PR:** `m37-on-circle-movement-paths`

## Goal

Draw a profile's movement across occasions as a curved, arrowed path on the
circumplex canvas, so change in (amplitude, displacement) reads as motion in
circumplex space rather than only as parallel Cartesian panels.

## Scope

**In:**
- A new exported layer (working name `geom_ssm_path()`) taking `amplitude` /
  `displacement` aesthetics plus an ordering, emitting them as the coord's
  `y`/`x`. `coord_circumplex()` reports `is_linear() == FALSE`, so ggplot2's
  munching already curves each segment along the polar geodesic — the layer
  supplies ordering, seam handling, and arrows, not drawing code.
- **Seam correctness:** consecutive occasions travel the *short* way across the
  0/360 boundary. Angles are unwrapped before the coord sees them (reusing
  `ssm_unwrap_gapped()`, `R/ssm_trajectory.R`); unwrapped values outside
  `[0, 360]` reach layer data uncensored (probed 2026-07-18: `390` and `-10`
  both survive), consistent with M31's coord-side-`thetalim` invariant.
- Optional arrowheads marking the direction of time.
- A plot-level convenience surface over the layer, taking an
  `ssm_analyze(occasions = )` object (D-018b: composable layers for power users,
  thin wrappers for everyone else). Its exact shape — a new `ssm_plot_*()`
  function versus an argument on `ssm_plot_circle()`, which already tolerates the
  conditional `Occasion` column (`R/ssm_plot.R:308-312`) — is settled at the
  implement gate. **(RB tripwire: irreversible-api)**
- A figure in the plotting vignette and pkgdown reference placement, matching
  the M34 grouping.

**Out:**
- True animation (gganimate or otherwise) and any new dependency — plan-gate
  decision 2026-07-18; recorded as a `candidate` ROADMAP row, not a rejection.
- Confidence regions along the path (wedge-per-occasion already ships via
  `geom_ssm_arc()`; a swept uncertainty band is not planned).
- Cartesian trajectory panels — `ssm_plot_trajectory()` (M33/M35) owns those.
- The certification legend fix and the `amax`/`center` guards → M36.

## Acceptance criteria

- [ ] `geom_ssm_path()` is exported, documented with a runnable example, and
      renders a connected path for a multi-occasion profile on a
      `ggcircumplex()` canvas; a plot-structure test locates the layer by geom
      class (never a positional `data[[i]]` index, per the M31 lesson).
- [ ] A seam test asserts that a path between occasions straddling 0/360 (e.g.
      `350 -> 10`) traverses the **short** arc: the built layer's x values span
      `20` degrees, not `340`. The test fails against a naive implementation
      that feeds raw `[0, 360)` angles to the coord.
- [ ] Degenerate input is handled at both ends: an occasion whose displacement
      is undefined (flat / zero-amplitude profile, `ssm_has_location()` FALSE)
      breaks the path rather than interpolating through it, with the post-gap
      tail still drawn (`ssm_unwrap_gapped()`'s bridging contract, M33); and
      non-finite angles are rejected by a `!is.finite()` guard before reaching
      the unwrap (the M32/M35 recurring trap: `is.na(Inf)` is FALSE, and
      `ssm_has_location()` reads `Inf` as located, NaN-ing a `cumsum()` unwrap).
- [ ] Arrowheads render when requested and are absent by default; verified at
      grob level, not by baseline alone.
- [ ] The convenience surface produces a correct figure from an
      `ssm_analyze(occasions = )` object with occasions in `details$occasions`
      order — a `T10`/`T2` pair must not flip (M33 lesson).
- [ ] One render-and-inspect pass is recorded per new figure (M33 lesson:
      data-level fences and a vdiffr baseline both pass a figure that reads
      wrong), plus a vdiffr baseline.
- [ ] `devtools::test()` clean; `devtools::check()` at 0 errors / 0 warnings /
      0 notes; `check_pkgdown()` clean with the new exports placed.

## Coverage

- AC1 → T2, T6
- AC2 → T1, T3
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6, T7
- AC7 → T7

## Tasks

- [x] T1: Write the failing seam test first (path `350 -> 10` spans 20 degrees,
      not 340), against the intended layer API.
- [x] T2: Implement `GeomSsmPath` / `geom_ssm_path()` in `R/geom_ssm.R`,
      emitting `amplitude`/`displacement` as `y`/`x` and letting the coord munch;
      follow the `GeomSsmPoint` `setup_data()` pattern (`R/geom_ssm.R:146-166`).
- [x] T3: Wire ordering + seam unwrapping through `ssm_unwrap_gapped()`, the
      gap-breaking behavior, and the `!is.finite()` guard; tests for each.
- [x] T4: Add the arrow parameter and its grob-level test.
- [x] T5: Settle the convenience-surface shape at the implement gate
      (new `ssm_plot_*()` vs an `ssm_plot_circle()` argument), then build it with
      occasion ordering taken from `details$occasions`.
      **(RB tripwire: irreversible-api)**
- [x] T6: Vignette figure + pkgdown reference placement + `@family` cross-links,
      matching M34's grouping; render-and-inspect pass recorded.
- [x] T7: `devtools::document()`, full `devtools::test()`, `devtools::check()`,
      `check_pkgdown()`; NEWS.md entry.

## Work log

- 2026-07-18: created by /milestone-plan. Promotes the M33 deferral from the
  ROADMAP visualization candidate row; both of its stated revisit conditions
  (trajectory viz shipped, M31 coord API settled) now hold. Animation excluded
  at the plan gate.
- 2026-07-18: T1-T5 done. `geom_ssm_path()`/`GeomSsmPath` added to `R/geom_ssm.R`
  (seam unwrap per group via `ssm_unwrap_gapped()`, `!is.finite()` guard before
  the unwrap, optional `order` aes, `arrow` param); `ssm_plot_circle(path = )`
  added. 31 tests in `tests/testthat/test-geom_ssm_path.R`; the AC5 ordering
  guard mutation-checked (alphabetical ordering makes it fail). Question gate
  settled the API shapes — see M37-D1. No new dependency: `ggplot2` re-exports
  `arrow()`/`unit()`.
- 2026-07-18: T6-T7 done. Vignette section added to `advanced-visualization.Rmd`
  (layer form + wrapper form, on the existing seam-crossing three-wave fixture);
  pkgdown Building Blocks row; `ssm_plot_trajectory()` `@seealso` cross-link;
  NEWS entries; 2 vdiffr baselines. The render-and-inspect pass caught a defect
  no data-level fence or baseline can — the terminal arrowhead drawn underneath
  the final occasion's point marker, hiding the direction of time. Fixed by
  drawing the wrapper's path last and sizing the arrow to clear a size-3 marker.
  Status -> review. `devtools::test()` 2936 pass / 0 fail;
  `devtools::check()` 0 errors / 0 warnings / 0 notes; `check_pkgdown()` clean.

## Decisions

### M37-D1 (2026-07-18): the movement-path convenience surface is `ssm_plot_circle(path = )`

Settled at the implement question gate — the plan's `irreversible-api` tripwire
on T5. `ssm_plot_circle()` is the door M34's vignette teaches for circle figures
and already tolerates the conditional `Occasion` column; a separate
`ssm_plot_path()` would have duplicated ~150 lines of canvas/palette/arc/point
assembly and added a second permanent exported name for the same figure. Cost
accepted: `path = TRUE` is meaningful only for an `ssm_analyze(occasions = )`
object, and errors for anything else, naming the two ways to produce one.

Two consequences inside the wrapper, both deliberate:

- The path is built from the pre-filter results frame, not the `df_plot` the
  points and arcs use, so an occasion with an undefined displacement stays in
  the frame as `NA` and **breaks** the path. Reusing `df_plot` would drop the
  row and silently connect the occasions on either side of the gap, drawing a
  movement that never happened.
- With a contrast, the historical `df[1:2, ]` slice truncates an occasions
  object to its first two occasions; the path branch drops only the contrast
  row (`-nrow(df)`, the positional detector `ssm_trajectory_frame()` uses).

Layer-level shapes settled at the same gate: connection follows `geom_path()`
(data row order, `group` separates series) with an optional `order` aesthetic
sorting within group; `arrow` takes a `ggplot2::arrow()` object and is `NULL` by
default. `ggplot2` re-exports both `arrow()` and `unit()`, so neither the layer
nor its examples add a dependency on `grid`.

## Review
