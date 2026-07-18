# M34: Plotting vignette + pkgdown reference — DONE

- **Outcome:** brought the plotting docs in line with the M31–M33 API.
  `vignettes/advanced-visualization.Rmd` rewritten: it had taught the obsolete
  "keep `amax` in sync across canvas and every layer" rule (that argument is now
  deprecated/ignored), and now teaches `coord_circumplex()` as sole owner of the
  amplitude→radius mapping, plus the configurable center, `r_axis_angle`, canvas
  theming, geom subclassing, and the occasions trajectory path — the last of
  which had no vignette coverage at all. PR #59 (squash e26d1ee, 2026-07-18).
- **Delivered:** 14 runnable chunks across 13 sections; `_pkgdown.yml` split into
  "Visualization - Complete Plots" (the four `ssm_plot_*`, moved out of Primary
  SSM Functions) and "Visualization - Building Blocks"; `@family visualization
  functions` on the plot trio + new `@family circumplex layers` across the six
  building blocks, replacing partial hand-kept `@seealso` lists (resolves the
  M33-review `@family` candidate). NEWS entry.
- **Key decisions:** no scope amendment — the M35 legend-glyph defect stayed a
  ROADMAP candidate rather than being folded into a docs-only milestone.
- **Statistical precision:** added the trajectory unwrap's unverifiable
  half-turn assumption, the bands-are-pointwise-not-simultaneous caveat, and the
  vector-averaging reading of a short group amplitude (printed live, not asserted).
- **Verified:** `check()` 0 errors / 0 warnings / 0 notes; `check_pkgdown()` clean;
  9/9 CI green. Three-lens review: 6 findings, F1 (98) + F2 (90) + F3 (80)
  actioned, F5 (78) + F4 (72) fixed anyway, F6 (55) rejected as pre-existing.
- **Deps:** M31, M32, M33. **Enables:** M7 (v2.0.0 release prep).
