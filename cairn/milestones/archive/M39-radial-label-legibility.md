# M39: Legible radial axis labels over data layers (done)

**Goal:** amplitude axis labels stay readable where they fall over dark or dense
geom layers.

**Outcome:** shipped a translucent white plate behind each amplitude tick label
in `coord_circumplex()`, via a `render_fg` override on `CoordCircumplex` plus
`label_backdrop()`/`add_label_backdrop()` helpers. Applies to every circumplex
canvas, users' plots included; no API added. The originating candidate's
diagnosis ("labels drawn beneath the layers") was wrong and was corrected at the
plan gate — the axis is a foreground guide, so the defect was contrast, not draw
order. PR #65 (squash `f10b483a`), 9/9 CI green, `check(manual = TRUE)` 0/0/0.

**Key decisions:**
- **M39-D1:** built by wrapping the located label grobs, not computing positions
  (`CoordRadial` places the axis via the unexported `rotate_r_axis()`).
- **D-022:** `grid` added to Imports (base R, already loaded via ggplot2).

**Review:** four findings, all >= 80, all fixed — grid D-entry (F1); spoke labels
plated when they read like amplitudes (F2); plotmath sized to deparsed source
(F3); fence missed plate extent/offset, caught only by skip_on_ci vdiffr (F4).

**Merge order:** NEWS entry is in the v2.0.0 section, so M39 merged before the
release; M7 stays blocked on its `submit_cran()` handoff.
