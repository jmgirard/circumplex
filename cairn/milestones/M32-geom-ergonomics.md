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

- [x] **T1** — Export `GeomSsmPoint`, `GeomSsmArc`, `CoordCircumplex` with
      `@format NULL` roxygen; add a downstream-subclass test (trivial subclass of
      each, rendered). Sweep the stale `StatSsmArc` phrase from DESIGN.md:319.
- [x] **T2** — Implement `na.rm = FALSE` warn-by-count in `GeomSsmPoint$setup_data`
      and `GeomSsmArc$setup_data` (default TRUE stays silent); test all four
      geom×flag cases.
- [x] **T3** — Fix the due-East `0.5`/`LM` label overlap in `coord_circumplex()` /
      `ggcircumplex()` furniture; regenerate the affected vdiffr baseline; assert
      label separation at grob level.
- [x] **T4** — Firm up the `ssm_plot_circle(repel=)` path (non-overlapping
      labels); test.
- [x] **T5** — Add the styling/aesthetic options with output-preserving defaults;
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
- 2026-07-18: T1 — exported `GeomSsmPoint`/`GeomSsmArc`/`CoordCircumplex` under
  shared `circumplex-ggproto` Rd page (`@keywords internal`, `@format/@usage
  NULL`); swept stale `StatSsmArc ⊂ ggforce::StatArcBar` from DESIGN.md; updated
  DESIGN.md export note. Added test-ggproto-classes.R (export + subclass-render
  for all three). Viz test files green.
- 2026-07-18: T2 — `na.rm` opt-in warn-parity via shared `ssm_warn_dropped()`
  helper wired into both geoms' `setup_data` (reads `params$na.rm`; default TRUE
  silent, FALSE warns by count on NA-drops; zero-width arc stays a silent
  geometry rule). Updated both `@param na.rm` docs + DESIGN.md. 4 geom×flag test
  cases; no double-warn in `ssm_plot_circle`/`plot.circumplex_cpm`.
- 2026-07-18: T3 — fixed the due-East `0.5`/`LM` overlap: coord auto-places the
  amplitude (radial) axis in the widest spoke gap (new `ssm_r_axis_angle()`
  helper: octants→22.5°, poles→45°, 12-pt→15°; off every spoke) via
  `setup_panel_params`, with a new `r_axis_angle=` override on
  `coord_circumplex()`. Fenced at helper + built-coord level (`r_axis_inside`
  moved off theta 0, not on any spoke). Regenerated 14 canvas vdiffr baselines;
  all cartesian curve/contrast/ladder baselines byte-identical (env-fidelity
  signal, M31 lesson).
- 2026-07-18: T4 — firmed up `repel`: gated on new `has_ggrepel()` with a clear
  install-hint error (Suggests idiom), rewrote the stale "experimental" doc.
  Tests: coord-aware repel layer present + maps to amplitude/displacement;
  mocked-absent ggrepel errors by name.
- 2026-07-18: T5 — exported the canvas theme as `theme_circumplex(base_size)`
  (was internal `circumplex_theme`; DESIGN.md deferral discharged), added
  `_pkgdown.yml` row. Default path output-preserving (`ggcircumplex()` uses it;
  baselines unchanged); non-default `base_size` path tested.

## Decisions

## Review
