<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M34: Plotting vignette + pkgdown reference

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M31, M32, M33
- **Principles touched:** —
- **Branch/PR:** `m34-plotting-vignette-pkgdown`

## Goal

Document the improved visualization surface: refresh/extend the plotting-focused
vignette to teach the coordinate system, the composable layers, and trajectory
plots, and organize the pkgdown reference so the plotting API is discoverable.

## Scope

**In:**
- Extend `vignettes/advanced-visualization.Rmd` (and/or add a focused vignette)
  to cover: the coordinate system and configurable center (M31), subclassing the
  exported geoms and the new styling options (M32), and occasions/growth
  trajectory plots (M33) — each with a runnable example.
- Reorganize the pkgdown reference (`_pkgdown.yml`) so the visualization
  functions group coherently (canvas/coord, composable layers, high-level
  `ssm_plot_*` wrappers, trajectory).
- Keep vignette prose statistically precise (CLAUDE.md: never describe an
  angular CI excluding 0° as a significance test).

**Out:**
- Any new plotting capability (all shipped by M31/M32/M33); this is docs-only
  over the merged API.

## Acceptance criteria

- [ ] The vignette teaches the coordinate system + configurable center, geom
      subclassing + new styling options, and trajectory plots, each with a
      runnable chunk; it builds under `devtools::check()` (the authoritative
      vignette build — [LESSONS.md M21](../LESSONS.md)).
- [ ] `_pkgdown.yml` groups the visualization functions coherently; `pkgdown::
      check_pkgdown()` (or build) reports every exported plotting function
      referenced (no orphaned topics).
- [ ] Prose reviewed for statistical precision (no CI-as-significance-test
      phrasing); rendered figures reflect the actual current output (re-run the
      chunks, don't edit narrative by guess — [LESSONS.md M16](../LESSONS.md)).
- [ ] `devtools::check()` clean (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T1, T3
- AC4 → T3

## Tasks

- [x] **T1** — Extend/refresh the plotting vignette with coord + center, geom
      subclassing + styling, and trajectory sections, each with a runnable
      example rendered from actual output.
- [x] **T2** — Reorganize `_pkgdown.yml` visualization reference groups; run
      `pkgdown::check_pkgdown()` for orphaned/missing topics.
- [x] **T3** — Statistical-precision prose pass; full `check()`.

## Work log

- 2026-07-17: created by /milestone-plan (viz expansion, area D).
- 2026-07-18: gate — single rewritten vignette; occasions trajectory path only (growth vignette keeps the table path); two pkgdown viz groups; M35 legend-glyph defect stays a candidate (no scope amendment).
- 2026-07-18: T1 done — vignette rewritten (stale per-layer `amax` teaching removed; coord/center/r_axis_angle, theming, geom subclassing, occasions trajectory added). Render-and-inspect fixed an illegible center demo and two "Coordinate system already present" messages. Suite 2886 pass.
- 2026-07-18: T2 done — `_pkgdown.yml` split into "Visualization - Complete Plots" (the four `ssm_plot_*`, moved out of Primary SSM Functions) and "Visualization - Building Blocks"; added `@family visualization functions` to the plot trio and a new `@family circumplex layers` across the six building blocks, replacing their incomplete hand-kept `@seealso` lists. `check_pkgdown()`: no problems found.
- 2026-07-18: T3 done — precision pass added the unwrap's unverifiable half-turn assumption, the bands-are-pointwise-not-simultaneous caveat, and the vector-averaging reading of a short group amplitude (printed live, not asserted in prose); rescaled the individuals figure (amax 3 -> 1.75) so the shrinkage it describes is actually visible. NEWS entry added. `devtools::check()`: 0 errors / 0 warnings / 0 notes.

## Decisions

## Review
