<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M32: Circumplex geom & layer ergonomics

- **Status:** planned
- **Priority:** normal
- **Depends on:** M31
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Make the circumplex ggplot2 layers extensible and ergonomic: export the
`GeomSsmPoint`/`StatSsmArc` generators for subclassing, bring `na.rm` to the
ggplot2 warn-parity convention, and add the richer styling/aesthetic options
users need to compose custom figures.

## Scope

**In:**
- Export the `GeomSsmPoint` and `StatSsmArc` ggproto generators (`@format NULL`)
  so downstream packages can subclass them ([DESIGN.md:355-357](../DESIGN.md):
  "a cheap future addition").
- `na.rm` warn-parity: when `na.rm = FALSE` the geoms **warn** by count before
  dropping degenerate rows (currently always-silent, a deviation from the
  ggplot2 convention, [DESIGN.md:348-354](../DESIGN.md)); `na.rm = TRUE` stays
  silent. `ssm_plot_circle()`'s own by-name warning is unchanged.
- Additional styling/aesthetic options on the existing layers as needed for
  custom composition (e.g. finer control over point/arc styling), each with a
  documented default matching current output.

**Out:**
- The coordinate-system rewrite → M31 (this builds on M31's final layer contract).
- Longitudinal trajectory viz → M33; plotting vignette + pkgdown → M34.

## Acceptance criteria

- [ ] `GeomSsmPoint` and `StatSsmArc` are exported (NAMESPACE + `@format NULL`
      docs); a test defines a trivial subclass and renders it, proving the
      generators are usable downstream.
- [ ] With `na.rm = FALSE`, a geom given a degenerate (missing amplitude/
      displacement) row **warns** with the dropped-row count before dropping;
      with `na.rm = TRUE` it stays silent — both asserted by a test.
- [ ] Any new styling aesthetic has a default that reproduces current rendering
      (existing vdiffr/snapshot baselines unchanged where no option is set) and
      a test exercising the non-default path.
- [ ] `devtools::test()` and `devtools::check()` clean (0 errors / 0 warnings /
      0 notes).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4

## Tasks

- [ ] **T1** — Export the two ggproto generators with `@format NULL` roxygen;
      add a downstream-subclass test.
- [ ] **T2** — Implement `na.rm = FALSE` warn-parity in `GeomSsmPoint$setup_data`
      / `StatSsmArc$compute_panel`; test both flag values.
- [ ] **T3** — Add the styling/aesthetic options with output-preserving defaults;
      test the non-default path.
- [ ] **T4** — `document()`; full `test()` + `check()`.

## Work log

- 2026-07-17: created by /milestone-plan (viz expansion, area B).

## Decisions

## Review
