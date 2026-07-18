<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M32: Circumplex geom & layer ergonomics

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M31
- **Principles touched:** —
- **Branch/PR:** m32-geom-ergonomics

## Goal

Make the M31 circumplex ggplot2 layers extensible and ergonomic: export the
`GeomSsmPoint` / `GeomSsmArc` / `CoordCircumplex` ggproto generators for
subclassing, give `na.rm` opt-in ggplot2 warn-parity, fix the due-East
`0.5`/`LM` label overlap on the canvas, and firm up the repel/label ergonomics
users need to compose custom figures.

## Scope

**In:**
- Export the `GeomSsmPoint`, `GeomSsmArc`, and `CoordCircumplex` ggproto
  generators (`@format NULL` docs) so downstream packages can subclass them
  ([DESIGN.md:330-334](../DESIGN.md) defers "the `GeomSsmPoint`/`GeomSsmArc`/coord
  ggproto generators" to M32; the arc is `GeomSsmArc ⊂ GeomRect`, **not** a Stat —
  the DESIGN.md:319 `StatSsmArc ⊂ ggforce::StatArcBar` phrase is stale post-M31/
  D-020 and is swept here).
- `na.rm` **opt-in warn-parity**: default stays `na.rm = TRUE` (silent — no new
  warnings in existing plots/vignettes); when a geom is given `na.rm = FALSE` it
  **warns with the dropped-row count** before dropping degenerate rows
  ([R/geom_ssm.R:113-120](../../R/geom_ssm.R), :187-193 currently always-silent).
  `ssm_plot_circle()`'s own by-name warning is unchanged (its internal geoms keep
  the silent default).
- Fix the `0.5`/`LM` (amplitude-ring vs. due-East angle) label overlap on the
  canvas furniture (M31 handoff, [archive:24-25](archive/M31-coord-system-build.md)):
  relocate/nudge so the radial-axis labels and the due-East spoke label no longer
  collide. This intentionally changes default rendering — its vdiffr baseline is
  regenerated.
- Firm up repel/label ergonomics (the `ssm_plot_circle(repel=)` path,
  [R/ssm_plot.R:204-221](../../R/ssm_plot.R), currently flagged experimental) plus
  any additional styling/aesthetic options for custom composition, **each new
  option with a default that reproduces current rendering**.

**Out:**
- The coordinate-system rewrite → M31 (done; this builds on its shipped layer
  contract).
- Longitudinal trajectory viz → M33; plotting vignette + pkgdown → M34.

## Acceptance criteria

- [ ] `GeomSsmPoint`, `GeomSsmArc`, and `CoordCircumplex` are exported (NAMESPACE +
      `@format NULL` docs); a test defines a trivial subclass of each and renders
      it, proving the generators are usable downstream. The stale
      `StatSsmArc ⊂ ggforce::StatArcBar` phrase is gone from DESIGN.md.
- [ ] With `na.rm = FALSE`, each geom (`GeomSsmPoint`, `GeomSsmArc`) given a
      degenerate (missing amplitude/displacement or incomplete-CI) row **warns**
      with the dropped-row count before dropping; with `na.rm = TRUE` (the default)
      it stays silent — all four cases asserted by a test.
- [ ] The default canvas no longer overlaps the `0.5` amplitude-ring label with
      the due-East angle label — asserted at grob level (positions/extents do not
      intersect) and by a regenerated vdiffr baseline; unrelated plot baselines
      (curve/diagnostic) regenerate byte-identically.
- [ ] Each new styling/repel aesthetic has a default that reproduces current
      rendering (existing vdiffr/snapshot baselines unchanged where no new option
      is set) and a test exercising the non-default path; `repel = TRUE` yields
      non-overlapping labels.
- [ ] `devtools::test()` and `devtools::check()` clean (0 errors / 0 warnings /
      0 notes).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4, T5
- AC5 → T6

## Tasks

- [ ] **T1** — Export `GeomSsmPoint`, `GeomSsmArc`, `CoordCircumplex` with
      `@format NULL` roxygen; add a downstream-subclass test (trivial subclass of
      each, rendered). Sweep the stale `StatSsmArc` phrase from DESIGN.md:319.
- [ ] **T2** — Implement `na.rm = FALSE` warn-by-count in `GeomSsmPoint$setup_data`
      and `GeomSsmArc$setup_data` (default TRUE stays silent); test all four
      geom×flag cases.
- [ ] **T3** — Fix the due-East `0.5`/`LM` label overlap in `coord_circumplex()` /
      `ggcircumplex()` furniture; regenerate the affected vdiffr baseline; assert
      label separation at grob level.
- [ ] **T4** — Firm up the `ssm_plot_circle(repel=)` path (non-overlapping
      labels); test.
- [ ] **T5** — Add the styling/aesthetic options with output-preserving defaults;
      test the non-default path.
- [ ] **T6** — `document()`; full `test()` + `check()`.

## Work log

- 2026-07-17: created by /milestone-plan (viz expansion, area B).
- 2026-07-18: re-planned by /milestone-plan (amend-via-gate). Corrected stale
  `StatSsmArc` → `GeomSsmArc` refs and DESIGN.md line citations (M31 shipped no
  Stat); added `CoordCircumplex` to the export set (DESIGN.md:330-334 deferral;
  user gate); folded in the M31 handoff (`0.5`/`LM` label overlap + repel/label
  ergonomics, archive:24-25); pinned `na.rm` to opt-in parity (default stays TRUE,
  user gate). Tasks 4→6.

## Decisions

## Review
