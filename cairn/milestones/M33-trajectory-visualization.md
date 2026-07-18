<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M33: Longitudinal trajectory visualization

- **Status:** planned
- **Priority:** high
- **Depends on:** —
- **Principles touched:** — (works under the CLAUDE.md angle invariants: displacement seam unwrap, LM=360)
- **Branch/PR:** —

## Goal

Package the occasions/growth trajectory figure users currently hand-roll: a new
exported plot of SSM parameter trajectories (including displacement) across
occasions, with confidence bands and correct 0/360 seam unwrapping.

## Scope

**In:**
- A new exported plotting function (name settled at implement, e.g.
  `ssm_plot_trajectory()`) for occasions objects — the output of `ssm_analyze()`
  with occasions and of `ssm_analyze_long()` — that plots each SSM parameter
  (e/x/y/a/d) over occasions with its confidence interval, replacing the raw
  `ggplot()` users hand-roll ([growth-ssm-analysis.Rmd:208](../../vignettes/growth-ssm-analysis.Rmd)).
- **Displacement trajectory seam handling**: unwrap each occasion's displacement
  endpoint by its *signed circular distance* from that occasion's estimate
  (`((bound - d_est + 180) %% 360) - 180`), so a trajectory crossing the 0/360
  seam renders as one continuous path, not a full-turn jump ([LESSONS.md M27
  2026-07-17](../LESSONS.md)).
- Faceting by SSM parameter and/or grouping; degenerate/flat occasions
  classified through the shared `ssm_has_location()` predicate so their handling
  matches the other geoms.
- Update the growth vignette's hand-rolled figure to call the new function.

**Out:**
- On-circle animated/arrow movement paths across occasions → candidate row
  (registered at plan time).
- The coordinate-system rewrite → M31; it does not gate this milestone (built on
  the existing layers, kept working by M31's back-compat contract).
- Geom ergonomics → M32; plotting vignette + pkgdown reorg → M34.

## Acceptance criteria

- [ ] The new function returns a ggplot of SSM parameter trajectories across
      occasions for both an `ssm_analyze()` occasions object and an
      `ssm_analyze_long()` object; happy-path test for each.
- [ ] A displacement trajectory whose occasions cross the 0/360 seam renders as
      a **continuous** path — asserted at the data level (the plotted/unwrapped
      coordinates, via `ggplot_build()` or the plot data), never by eye, because
      `devtools::check()` cannot catch a wrong figure ([LESSONS.md M27](../LESSONS.md));
      a linearly-wrapped implementation fails the test. *(source: CLAUDE.md
      Statistical invariants; LESSONS.md M27.)*
- [ ] Confidence intervals are drawn per occasion; a flat/degenerate occasion
      (undefined displacement) is named/dropped via `ssm_has_location()`, not
      mis-drawn.
- [ ] Error branches fire: a non-occasions object errors informatively; the
      growth vignette builds using the new function. A vdiffr baseline is
      recorded (`skip_on_ci()` if BLAS-sensitive).
- [ ] `devtools::test()` and `devtools::check()` clean (0 errors / 0 warnings /
      0 notes).

## Coverage

- AC1 → T1, T2
- AC2 → T2, T3
- AC3 → T2, T3
- AC4 → T3, T4
- AC5 → T4

## Tasks

- [ ] **T1** — Inspect the occasions object shape (`ssm_analyze()` occasions /
      `ssm_analyze_long()` results: Occasion column, per-occasion e/x/y/a/d +
      CIs); design the trajectory data reshape.
- [ ] **T2** — Implement the plotting function with per-parameter trajectories +
      CIs; displacement path unwrapped by signed circular distance (M27 pattern,
      reuse the `d_covered()`-style branch, not a fresh `%%`).
- [ ] **T3** — Tests: seam-straddling continuity (data-level), CI presence,
      flat-occasion handling, non-occasions error; force a real seam straddle
      through a fixture (M13 teeth lesson), don't re-type the unwrap.
- [ ] **T4** — Swap the growth vignette figure to the new function; vdiffr
      baseline; full `test()` + `check()`.

## Work log

- 2026-07-17: created by /milestone-plan (viz expansion, area C).

## Decisions

## Review
