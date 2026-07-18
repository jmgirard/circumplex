# M33: Longitudinal trajectory visualization (occasions objects) — done

**Goal:** export a Cartesian trajectory plot of SSM parameters across occasions,
with confidence bands, 0/360 seam unwrapping, and D-007 certification marking.
PR #57 (squash 079cfef), merged 2026-07-18.

**Outcome:** `ssm_plot_trajectory()` — one facet per parameter (e/x/y/a/d,
`drop_xy`), ribbon CIs, one series per `Group`; takes `ssm_analyze(occasions =
)` and `ssm_analyze_long()` objects. Internals `ssm_trajectory_frame()`,
`ssm_unwrap_gapped()`, `ssm_interval_on_branch()` in `R/ssm_trajectory.R`.
25 tests; suite 2812 passing; check clean; CI green all platforms.

**Key decisions:**
- Re-planned against the shipped M31/M32 contract: no vignette builds an
  occasions object (the growth figure is glmmTMB + `ssm_draws()`), so the
  vignette-swap scope moved to M35 (`Depends on: M33`); M7 gained it.
- Displacement intervals anchored at the lower bound and widened by their stored
  `ssm_arc_span()`. The M27 per-bound form clamps bounds into (-180, 180] and
  **inverts the ribbon** for any arc > 180° — review finding (95); LESSONS.
- `ssm_unwrap_gapped()` bridges a gap rather than inheriting `angle_unwrap()`'s
  NA-onward policy, which would blank the post-gap tail.
- Contrast row dropped; uncertified occasions (D-007) draw hollow.

**Deferred:** `@family` cross-links only to `plot.circumplex_ci_accuracy()` —
finding scored 45 → ROADMAP candidate; M34 owns the reference index.
