# M50: Advanced Visualization vignette — rework for a less-advanced audience

**Status:** done (2026-07-21, PR #76 https://github.com/jmgirard/circumplex/pull/76)

**Goal:** Make `advanced-visualization.Rmd` approachable for non-ggplot2-experts, and fix the one shipped-function defect that rework surfaced.

**Outcome:** Vignette — descriptive full scale-name spokes (`csip$Scales$Label`) replace the redundant `instrument = csip` figure (every bundled octant instrument shares the PA–NO abbrevs, so that figure taught nothing); the deliberately-broken default-breaks coordinate figure is cut; the "Extending the layers" ggproto-subclass section is removed; visible base-R simplified to `subset()` and the occasions simulation moved into a non-echoed chunk. Code — `ssm_trajectory_ggplot()` gains `panel.spacing.x = grid::unit(1.2, "lines")` so free-`y` panel axis labels clear their neighbours; 5 trajectory vdiffr baselines regenerated (geometry-only). Exported API unchanged.

**Decisions:** none (milestone-local).

**Review:** 3-lens fan-out, no regressions (statistical core in `GeomSsmArc$setup_data()` untouched). 2 findings fixed on-branch: F1 (98) garbled descriptive-labels sentence, flagged by all three lenses; F2 (92) echoed `occasions-path` chunk referenced `angles` after it moved into the hidden chunk. None <80; nothing graduated or retired.
