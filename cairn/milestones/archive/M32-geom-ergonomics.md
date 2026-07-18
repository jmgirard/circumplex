# M32: Circumplex geom & layer ergonomics — DONE

- **Outcome:** made the M31 circumplex ggplot2 layers extensible + ergonomic.
  Exported `GeomSsmPoint`/`GeomSsmArc`/`CoordCircumplex` for subclassing (shared
  `circumplex-ggproto` Rd page); the amplitude (radial) axis now auto-places in
  the widest spoke gap so its labels clear the due-East `0.5`/`LM` spoke label;
  `na.rm` follows the ggplot2 opt-in convention; exported `theme_circumplex()`.
  PR #56 (squash a34794a, 2026-07-18).
- **Delivered:** `coord_circumplex(r_axis_angle=)` override + `ssm_r_axis_angle()`
  widest-gap helper (octants→22.5°); `ssm_warn_dropped()` na.rm warn-by-count on
  both geoms (default TRUE silent); `theme_circumplex(base_size)` (was internal);
  `has_ggrepel()` install-hint gate for `ssm_plot_circle(repel=)`. Swept the stale
  `StatSsmArc` phrase from DESIGN.md. 14 canvas vdiffr baselines regenerated.
- **Key decisions:** AC3 amended at review (fence the off-spoke property, not a
  fragile grob-extent test). No new D-entry; no DESIGN principle touched.
- **Verified:** `check()` 0/0/0; suite 2751 pass; three-lens review — 1 finding
  (score 78) actioned (`!is.finite()` guard on `r_axis_angle` Inf); blame/prior-PR
  clean. CI green ×7 platforms.
- **Deps:** M31. **Enables:** M34 (plotting vignette + pkgdown). Follow-up:
  pre-existing `amax`/`center` Inf-guard gap → background task task_010f992f.
