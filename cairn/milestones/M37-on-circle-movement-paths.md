# M37: On-circle movement paths across occasions

- **Status:** planned
- **Priority:** normal
- **Depends on:** M31, M32, M33
- **Principles touched:** —
- **Branch/PR:** —

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

- [ ] T1: Write the failing seam test first (path `350 -> 10` spans 20 degrees,
      not 340), against the intended layer API.
- [ ] T2: Implement `GeomSsmPath` / `geom_ssm_path()` in `R/geom_ssm.R`,
      emitting `amplitude`/`displacement` as `y`/`x` and letting the coord munch;
      follow the `GeomSsmPoint` `setup_data()` pattern (`R/geom_ssm.R:146-166`).
- [ ] T3: Wire ordering + seam unwrapping through `ssm_unwrap_gapped()`, the
      gap-breaking behavior, and the `!is.finite()` guard; tests for each.
- [ ] T4: Add the arrow parameter and its grob-level test.
- [ ] T5: Settle the convenience-surface shape at the implement gate
      (new `ssm_plot_*()` vs an `ssm_plot_circle()` argument), then build it with
      occasion ordering taken from `details$occasions`.
      **(RB tripwire: irreversible-api)**
- [ ] T6: Vignette figure + pkgdown reference placement + `@family` cross-links,
      matching M34's grouping; render-and-inspect pass recorded.
- [ ] T7: `devtools::document()`, full `devtools::test()`, `devtools::check()`,
      `check_pkgdown()`; NEWS.md entry.

## Work log

- 2026-07-18: created by /milestone-plan. Promotes the M33 deferral from the
  ROADMAP visualization candidate row; both of its stated revisit conditions
  (trajectory viz shipped, M31 coord API settled) now hold. Animation excluded
  at the plan gate.

## Decisions

## Review
