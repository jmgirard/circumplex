# M31: Circumplex coordinate-system build — DONE

- **Outcome:** shipped `coord_circumplex()` (⊂ ggplot2 `CoordRadial`), a real
  coordinate system that owns `amax` and the displacement→angle polar transform.
  Resolves the three DESIGN.md viz defects: per-layer `amax` shared state (now
  structurally impossible), no configurable center (now `rlim = c(center, amax)`),
  theme-frozen canvas (now themed panel furniture). PR #55 (squash 2433baa,
  2026-07-18).
- **Delivered:** exported `coord_circumplex(amax, center)`; geoms refactored
  (point → `x/y`; arc → coord-bent `GeomRect` unwrapping a seam-straddling
  interval by extension in `setup_data`); `ggcircumplex()` rebuilt on the coord
  (`circle_base()` removed); `ssm_plot_circle()` incl. coord-aware `repel`; both
  hidden consumers (`plot.circumplex_cpm`, `plot.circumplex_fit_structure`)
  ported. Per-layer `amax`/`n` soft-deprecated (one-time note, never error). AC4
  boundary battery fenced at data + grob level.
- **Key decisions:** D-019 (design GO Option A; ggplot2 `>= 4.0.0`) built here;
  **D-020** — `ggforce` removed from Imports (arc → `GeomRect`, rings → coord
  gridlines), superseding the DESIGN.md V6 KEEP holding; dead `ggrad`/
  `ssm_to_cartesian`/`ssm_radius` swept.
- **Verified:** `check()` 0 errors / 0 warnings / 0 notes; suite 2713 pass;
  three-lens review (no regressions; no prior-PR evidence) fixed F2 (coord
  validation order). Invariants (LM=360, pole, seam short-way, zero-width/flat
  drop) fenced data + grob.
- **Deps:** M30. **Enables:** M32, M34. Remainder → M32: the `0.5`/`LM` label
  overlap at due-East; enhanced repel/label ergonomics.
