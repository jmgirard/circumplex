<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M31: Circumplex coordinate-system build

- **Status:** planned
- **Priority:** high
- **Depends on:** M30
- **Principles touched:** — (implements under the CLAUDE.md angle invariants: [0,360), LM=360; IP/GP formalization deferred to /design-interview)
- **Branch/PR:** —

## Goal

Implement the M30-ratified coordinate system that owns `amax` and the polar
transform, adding a configurable amplitude center and a theme-responsive canvas,
refactoring the existing geoms and `ssm_plot_*`/`ggcircumplex()` wrappers onto
it without changing their correct output.

## Scope

**In:**
- Build the coord/scale per the M30 spec so `amax` (and the polar transform)
  lives in one place; the canvas and data layers can no longer disagree.
- Configurable amplitude center, implemented so the axis labels and the
  amplitude→radius mapping always agree (never the R3 mislabel).
- Theme-responsive canvas furniture per the spec.
- Refactor `geom_ssm_point/arc()`, `ggcircumplex()`, `scale_x_circumplex()`, and
  `ssm_plot_circle/curve/contrast()` onto the coord; the latter stay as thin
  convenience wrappers (D-018b) with unchanged public signatures.
- Boundary + regression tests at the angle invariants; refresh vdiffr baselines.

**Out:**
- Exported ggproto generators, `na.rm` warn-parity, new styling aesthetics → M32.
- Longitudinal trajectory viz → M33; plotting vignette + pkgdown → M34.
- Any API surface M30 marked NO-GO or deferred.

## Acceptance criteria

- [ ] `amax` is a single source of truth: a plot whose canvas and layers are
      built from one coord/scale aligns points to rings with no per-layer `amax`
      restatement; a regression test asserts alignment (the old dual-supply
      mismatch is unrepresentable or errors).
- [ ] Configurable center: setting a non-zero center relabels the rings **and**
      remaps amplitudes consistently — a test asserts a point at a given
      amplitude lands on its labelled ring for a non-zero center (guards the R3
      mislabel, [DESIGN.md:338-343](../DESIGN.md)).
- [ ] Angle invariants hold through the new transform, each with a test:
      a profile peaking at 0°/360°, a CI straddling the 0/360 seam (arc drawn
      the short way), LM=360 labelling, and a flat zero-variance profile
      (dropped/handled, not mis-drawn). *(source: CLAUDE.md Statistical
      invariants; boundary list fixed by M30 AC3.)*
- [ ] Back-compat: `ssm_plot_circle/curve/contrast()` and `ggcircumplex()` keep
      their signatures and produce correct output — existing vdiffr/snapshot
      tests pass (baselines re-recorded only where the new transform
      deliberately changes rendering, with the change reviewed).
- [ ] `devtools::test()` and `devtools::check()` clean (0 errors / 0 warnings /
      0 notes); BLAS-sensitive vdiffr tests use `skip_on_ci()` (legacy lesson).

## Coverage

- AC1 → T1, T4
- AC2 → T2, T4
- AC3 → T4
- AC4 → T3, T4
- AC5 → T5

## Tasks

- [ ] **T1** — Implement the coord/scale owning `amax` + the polar transform
      (`R/`), per the M30 spec; internal `circle_base()`/`ggcircumplex()` route
      through it.
- [ ] **T2** — Add the configurable amplitude center with agreeing label/mapping.
- [ ] **T3** — Refactor `geom_ssm_point/arc()` and the `ssm_plot_*` wrappers onto
      the coord, preserving public signatures; wire theme-responsiveness.
- [ ] **T4** — Boundary + alignment regression tests (0/360 peak, seam-straddle,
      LM=360, flat profile, non-zero center, amax single-source); drive real
      values through the transform, not re-typed expressions (M13 teeth lesson).
- [ ] **T5** — Refresh vdiffr baselines for intended rendering changes;
      `skip_on_ci()` the BLAS-sensitive ones; full `test()` + `check()`.

## Work log

- 2026-07-17: created by /milestone-plan (viz expansion, area A build half).

## Decisions

## Review
