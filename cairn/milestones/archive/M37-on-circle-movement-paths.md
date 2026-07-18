# M37: On-circle movement paths across occasions (done)

**done** · PR [#61](https://github.com/jmgirard/circumplex/pull/61) · squash `15ed598c` · depends on M31, M32, M33

## Goal & outcome

Draw a profile's movement across occasions as a curved, arrowed path on the
circumplex canvas, so change reads as motion rather than only as Cartesian panels.
Shipped `geom_ssm_path()`/`GeomSsmPath` + an `ssm_plot_circle(path = )` wrapper:
`coord_circumplex()` munches each segment along the polar geodesic; the layer owns
ordering, seam, gaps, arrows. Occasions cross 0/360 the short way (350→10 is a 20°
arc), an undefined occasion breaks the path with the tail still on the correct
branch, and a `!is.finite()` guard precedes the unwrap so an infinite angle cannot
NaN the series. Vignette, pkgdown, NEWS, 41 tests, 2 vdiffr baselines. No new dep.

## Decisions & review
- **M37-D1** — surface is `ssm_plot_circle(path = )`, not a new `ssm_plot_path()`
  (the gate's `irreversible-api` tripwire); path built pre-filter so gaps survive
  as `NA` and break it.
- **M37-D2** — `order` aesthetic removed at review (supersedes D1's ordering
  clause): `add_group()` fragmented it into a `zeroGrob`. Strict `geom_path()`
  parity; ordering lives in the wrapper.
Findings 96/90/82, all fixed: `order` zeroGrob; `path=TRUE` bypassing
`drop_lowfit`; an example drawing a zero-length path. AC2/AC3/AC5 mutation-checked.
2944 tests, `check()` 0/0/0, CI green on 9.
