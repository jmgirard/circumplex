# M50: Advanced Visualization vignette — rework for a less-advanced audience

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m50-viz-vignette-approachable

## Goal

Make `advanced-visualization.Rmd` approachable for readers who are not
ggplot2 experts, and fix the one shipped-function defect that rework surfaces.

## Scope

**In:**

- Replace the instrument-labels example (`canvas-instrument` chunk): every
  bundled octant instrument has `Scales$Abbrev == PANO()` and
  `ggcircumplex(instrument=)` renders that field (`R/scale_circumplex.R:12`),
  so `instrument = csip` draws a plot identical to the `labels = PANO()` figure
  above it. Show full descriptive scale names instead (`csip$Scales$Label` —
  Domineering, Self-Centered, …) and keep one sentence that an instrument
  object can be passed rather than typing labels.
- Cut the deliberately-broken coordinate-system figure (the `coord-bare` chunk
  drawing spokes on ggplot2 default breaks 0/100/200/300 against octant data)
  and its "visibly unfinished" prose; lead with the correctly-scaled
  construction and explain the scale line in prose.
- Remove the "Extending the layers" section (the `GeomSsmStar` ggproto
  subclass) — out of scope for this audience.
- Minimize advanced base-R subsetting/construction in the vignette's *visible*
  (echoed) chunks: `results$results[, c(...)]` column selection, the
  `do.call(rbind, lapply(...))` + `matrix(...)` occasions-data build,
  `people[!is.na(people$Disp), ]` row filtering.
- Fix trajectory panel-label crowding as a **package default** in
  `ssm_trajectory_ggplot()` (`R/ssm_trajectory.R`): with `scales = "free_y"`
  the Amplitude/Displacement panels' interior y-axis labels press against the
  panel to their left; add horizontal panel spacing so they clear it.

**Out:**

- Retitling the vignette — it stays "Advanced Circumplex Visualization".
- True animation / gganimate — stays the existing ROADMAP candidate.
- Any change to the exported geoms/coord themselves (`GeomSsmPoint`,
  `CoordCircumplex` stay exported; only the vignette's subclassing section goes).

## Acceptance criteria

- [ ] The instrument/label example renders spokes with full descriptive scale
      names (not `PA`–`NO`), and no vignette prose claims a bundled instrument
      relabels spokes. Evidence: the reworked chunk knits; grep the Rmd.
- [ ] No figure in the coordinate-system section shows spokes on ggplot2
      default breaks (0/100/200/300); every circumplex figure carries octant
      spokes. Evidence: the `coord-bare` broken-figure chunk is gone; render.
- [ ] The "Extending the layers" section is removed. Evidence: the Rmd contains
      no `ggproto`, `GeomSsmStar`, or "Extending the layers".
- [ ] The vignette's echoed R chunks contain none of `do.call(`, `lapply(`,
      `matrix(`, `[, c(`, or logical/negative row-filtering `[!is.na(`; any
      unavoidable construction is confined to a non-echoed setup chunk.
      Evidence: grep over echoed chunks + render shows equivalent figures.
- [ ] `ssm_trajectory_ggplot()` sets a horizontal panel spacing, the 5
      trajectory vdiffr baselines are regenerated (only the intended ones
      move), and a render-and-inspect confirms no interior-label collision.
      Evidence: `R/ssm_trajectory.R` diff; `_snaps/ssm_trajectory*`; the figure.
- [ ] `devtools::test()` (NOT_CRAN=true) green and
      `devtools::check(args = "--no-manual")` clean with every vignette figure
      knitting. Evidence: check/test logs (the `verify` slot).

## Coverage

- AC1 → T2
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T1
- AC6 → T6

## Tasks

- [x] T1 — Add horizontal panel spacing to `ssm_trajectory_ggplot()`'s theme
      (`R/ssm_trajectory.R:607-611`); render-and-inspect the grouped,
      ungrouped, and table figures (M33/M38: data-level tests and a stale
      baseline both pass a figure that reads wrong); regenerate the 5
      trajectory vdiffr baselines under `NOT_CRAN=true` (M31: bare `Rscript`
      auto-skips vdiffr) and confirm no unrelated baseline moved.
- [x] T2 — Rework the instrument/label example (`advanced-visualization.Rmd`
      ~L66–82): descriptive full-scale-name spokes; one sentence that an
      instrument object can be passed; correct the surrounding prose.
- [x] T3 — Cut the broken `coord-bare` figure and its "unfinished" prose
      (~L84–122); lead with the correctly-scaled construction, explaining the
      scale line in prose. Verify no remaining figure shows default breaks.
- [x] T4 — Remove the "Extending the layers" section (~L236–270) and any
      cross-reference to it.
- [x] T5 — Simplify advanced base-R in the visible chunks (the `[, c(...)]`
      selections, the `do.call(rbind, lapply())` + `matrix()` occasions build,
      `people[!is.na(...), ]`) into simpler idioms, or move unavoidable
      construction into a non-echoed setup chunk; keep every figure equivalent.
- [x] T6 — `devtools::test()` (NOT_CRAN=true) and
      `devtools::check(args = "--no-manual")`; confirm the vignette builds and
      every figure knits; final render-and-inspect sweep of the reworked figures.

## Work log

- 2026-07-21: created by /milestone-plan.
- 2026-07-21: T1 — added `panel.spacing.x = grid::unit(1.2, "lines")` to `ssm_trajectory_ggplot()`; render-inspected 3- and 5-panel layouts (labels clear); regenerated 5 vdiffr baselines (env parity confirmed via coord_circumplex byte-identical); trajectory tests 145 pass / 0 fail.
- 2026-07-21: T2 — replaced the `instrument = csip` figure (identical PA–NO abbrevs) with descriptive full scale-name spokes (`labels = csip$Scales$Label`); kept a one-sentence note that an instrument can be passed. Render-inspected: all 8 names fit at fig.width=7.
- 2026-07-21: T3 — cut the deliberately-broken `coord-bare` figure (spokes on default 0/100/200/300) and its "unfinished" prose; lead with the correctly-scaled `coord-built` figure and explain the scale line in prose.
- 2026-07-21: T4 — removed the "Extending the layers" section (`GeomSsmStar` ggproto subclass); no dangling cross-references remain.
- 2026-07-21: T5 — `[, c()]` displays → `subset(select=)`; `people[!is.na(),]` → `subset()`; moved the `do.call(rbind, lapply())`+`matrix()` occasions simulation into a non-echoed `echo = FALSE` chunk. Echoed chunks now carry no flagged base-R; full vignette knits under load_all (KNIT OK, 13 figures).
- 2026-07-21: T6 — `devtools::check(args = "--no-manual")` clean: 0 errors / 0 warnings / 0 notes (4m52s), authoritative vignette build included. Status → review.

## Decisions

## Review
