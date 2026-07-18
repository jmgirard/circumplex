# RR08: Circumplex coordinate-system design (M30) — independent review

- **Date:** 2026-07-17
- **Reviewer:** Fable (claude-fable-5), independent review per RB08
- **Materials read:** `devel/m30-coord-spec.md` (full), `R/geom_ssm.R` (full),
  `R/ssm_plot.R` (`ssm_plot_circle()`, repel branch, `ggcircumplex()`,
  `circle_base()`), `R/scale_circumplex.R` (full), `R/utils.R` (`ggrad()`),
  `cairn/DESIGN.md` §Visualization extension, `R/cpm_oop.R`
  (`plot.circumplex_cpm`), `R/fit_structure_oop.R` (call sites)
- **Environment probed:** ggplot2 4.0.3, ggforce 0.5.0 (installed); ggplot2
  v3.5.2 source (fetched from the tidyverse GitHub tag) for the floor check.
  All empirical claims below were run, not assumed; probe scripts lived in the
  session scratchpad and their key numbers are quoted inline.

---

## 1. Option A vs Option B — the load-bearing call

**Recommendation: Option A, definite.** `CoordRadial`'s guide/label machinery
expresses every circumplex label convention without a fight, verified
empirically:

- **LM = 360 at the pole.** With `scale_x_continuous(breaks = c(45, ..., 315,
  360))`, `thetalim = c(0, 360)`, `expand = FALSE`, `start = pi/2`,
  `reverse = "theta"`, the theta guide's computed key retains the 360 break and
  labels it `360` at panel position `(0.9, 0.5)` — i.e. exactly on the +x
  spoke. Probe output (guide key): `.value 360, .label "360", x 0.9, y 0.5`.
  Because `octants()` ends at 360 and never emits 0 (I1), there is no
  overlapping 0/360 break pair to fight. LM = 360 is *native* here: the label
  text is whatever the x-scale's `labels` say, so the existing
  `resolve_circumplex_labels()` (`R/scale_circumplex.R:8`) plugs in unchanged —
  scale abbreviations, instrument abbreviations, or
  `circumplex_degree_labels()` all just become `breaks`/`labels` on the theta
  scale.
- **CCW-from-right orientation (I1).** `start = pi/2, reverse = "theta"`
  yields displacement 0/360 at +x and 90 at the top (probe: x=90 transformed to
  `(0.5, 0.74)`, the top of the panel; x=0 and x=360 to `(0.74, 0.5)`, the
  right). The convention is two constructor pins, not a transform override.
- **Spokes and rings as themed furniture (R3).** The panel background under
  `coord_radial` + `theme_bw()` contains a `grill` gTree (probe: `ggplotGrob`
  panel children), i.e. gridlines driven by `panel.grid.*`; theta-specific
  theme elements exist (`axis.text.theta`, `axis.ticks.theta`,
  `axis.line.theta`, `axis.text.r` — probed via `get_element_tree()`). Spokes =
  theta major gridlines at the scale breaks; rings = r major gridlines.
- **Amplitude ring labels.** The r-axis can be placed inside along a chosen
  radius (`r.axis.inside` accepts a numeric angle since 4.0.0 — ggplot2 NEWS,
  4.0.0 block), themed via `axis.text.r`. That reproduces the *placement* of
  the current interior amplitude labels (`circle_base()`,
  `R/ssm_plot.R:599-612`). The current white boxed-label *style* is not a stock
  axis style; either accept the restyle (it is the point of R3) or draw label
  boxes in the `render_bg()` override. Neither is "fighting" the coord — see
  recommendation R-9.

**Why not B.** Option B's only advantage is no floor bump. Its cost is owning
the entire polar pipeline — transform, munching distances, theta/r axis
rendering, and interop with a guide system that was itself rewritten twice
(ggproto guides in 3.5.0, S7 in 4.0.0). That is *more* exposure to unstable
internal API than subclassing, not less: a subclass overrides two or three
methods; a bespoke coord implements all of them and must still hand its guides
to the same churned guide system. B also re-derives the annular-wedge
tessellation that DESIGN.md:322-325 explicitly called "exactly the fiddly
geometry worth *not* owning." B is higher total risk. The one real cost of A is
that the floor is higher than the spec claims — see §Beyond-the-brief B1 and
the verdict: the tabled `rlim`/`thetalim`/`reverse` parameters **do not exist
in ggplot2 3.5.x** (verified against the v3.5.2 source: signature is
`theta, start, end, expand, direction, clip, r.axis.inside, rotate.angle,
inner.radius, r_axis_inside, rotate_angle` — no `thetalim`, no `rlim`, no
`reverse`). Option A as designed requires **ggplot2 ≥ 4.0.0** (released
2025-09-11, per the CRAN archive). I recommend A *with that corrected floor*,
not A-on-3.5.0.

## 2. Seam-straddling arc (invariant I2)

**Recommendation: neither of the two offered mechanisms as stated. Unwrap by
*extension* in the geom's `setup_data()` — `xmax <- xmin + ssm_arc_span(xmin,
xmax)`, allowing `xmax > 360` — and let the coord's periodic transform carry it
across the seam. Do not pre-split at the seam; do not put the unwrap in the
coord's `transform()`.**

Empirical basis (all under `coord_radial(start = pi/2, reverse = "theta",
thetalim = c(0, 360), rlim = c(0, 0.5), expand = FALSE)`, ggplot2 4.0.3):

- **Raw stored representation is wrong under the stock coord, confirming the
  brief's premise.** `geom_rect(xmin = 350, xmax = 10)` renders one polygon
  whose vertices cover *every* 30° bin from 10° to 350° — the 340° wedge the
  long way. The unwrap is mandatory (this is the discriminating T-i2 case).
- **Extension works perfectly.** `geom_rect(xmin = 350, xmax = 370)` renders
  **one** polygon (33 vertices) whose angular coverage is exactly
  `(330, 360] ∪ (0, 30]` — a smooth 20° wedge across the pole, no gap, no
  double-draw, no seam edge. This works because `CoordRadial`'s transform is
  periodic and does not clamp: probing `transform()` directly, `x = 370` maps
  to theta `1.3962634` — the *bit-identical* position of `x = 10` — and
  `x = -10` to the position of `x = 350`.
- **Pre-splitting is a visual regression.** Two rects `[350, 360]` + `[0, 10]`
  render as **two** polygons meeting at the seam. The pieces abut exactly (the
  0/360 edge transforms bit-identically both sides, see Q3), so there is no gap
  and no alpha double-draw — but each polygon carries its own stroke, so the
  wedge grows two visible radial border lines at the seam
  (`ssm_plot_circle()` draws arcs with `colour` + `linewidth = 1`,
  `R/ssm_plot.R:156-161`; `plot.circumplex_cpm` with `linewidth = 0.5`,
  `R/cpm_oop.R:374-375`). Today's ggforce path draws one polygon
  (`arcPaths` sweeps angles past 2π). Reject pre-split.
- **Unwrap-inside-`transform()` is not viable at all.** By the time
  `Coord$transform()` runs, `coord_munch()` has already interpolated the rect's
  edges *in data space* — a rect stored `350 → 10` has been munched into ~250
  vertices sweeping the long way (probe: 256 vertices covering all bins) before
  the coord ever sees a coordinate. The coord receives anonymous vertices, not
  the rect's CI semantics; it cannot recover the short-way intent, and it also
  cannot distinguish an arc's x from a point's x. The unwrap *must* run before
  munching, i.e. in the geom.

This split keeps the division of labor clean and testable: **the geom's
`setup_data()` owns the CI convention** (min > max ⇒ +360 extension via the
existing `ssm_arc_span()`, `R/geom_ssm.R:47`; span validation and the
full-circle rejection currently at `R/geom_ssm.R:193-202` move here);
**the coord owns periodicity**. The invariant is assertable at two levels:
after `ggplot_build()`, the arc layer's data has `xmax = 370` for input
`(350, 10)` (data-level, per the M13/M27 lesson the spec already cites); and a
grob-level assertion via `layer_grob()` + `atan2()` angular binning (the exact
probe recipe above — hand it to M31) proves the drawn coverage is
`(330, 360] ∪ (0, 30]` and not the complement. No adverse interaction with
`CoordRadial`'s own theta wrapping was found: the wrap is exact, and
`GeomRect$setup_data()` does not reorder `xmin`/`xmax` (probed:
`xmin = 350, xmax = 10` passes through untouched).

**One guard this depends on:** out-of-`[0,360]` theta must reach the coord.
That holds when the panel range is pinned by the **coord** (`thetalim` zooms
without censoring). It would *break* if the range were pinned by **scale
limits** instead (`scale_x_continuous(limits=)` censors out-of-limits position
values to NA and drops the row). `coord_circumplex()` must therefore pin the
range coord-side, never scale-side, and a regression test should lock that in
(see Q6, gap 2).

## 3. Pole exposure (invariant I3)

**Confirmed — `d = 0.0` and `d = 360.0` draw bit-identically — but only under
a guard the spec must make explicit: `expand = FALSE` and a coord-pinned
`thetalim = c(0, 360)`.**

- With `thetalim = c(0, 360), expand = FALSE`: `transform()` maps `x = 0` and
  `x = 360` to positions with difference **exactly 0** in both x and y (probe:
  both at `(0.74, 0.5)`, diffs `0` and `0`; the internal theta values are both
  `1.5707963267948966`, i.e. ggplot2 itself reduces mod 2π). No seam gap of any
  size, no duplicated draw (a point row is drawn once), no float divergence.
- **With the default `expand = TRUE` the invariant fails badly**: the theta
  range expands to `[-18, 378]`, and `x = 0` vs `x = 360` land **33.4° apart**
  (probe: npc y differs by 0.133 — a visible gap, not one pixel). This is the
  concrete hazard the brief asked to name. The guard: `coord_circumplex()`
  hard-pins `expand = FALSE` (at minimum in theta) and `thetalim = c(0, 360)`
  internally and does **not** expose either as a user argument. A second,
  quieter dependence: if the theta range were left to train from data (no
  `thetalim`), a dataset whose displacements don't span [0, 360] would change
  the angle mapping entirely — same guard covers it.
- T-i3 should assert both float labels **at grob level** (positions extracted
  from the built plot), using a tolerance (`≤ 1e-12` npc) rather than
  `identical()` — the observed difference is exactly 0 on this platform, but
  the mod-2π reduction is one `%%` away from a 1-ulp platform difference, and
  a sub-pixel tolerance keeps the test meaningful without being brittle.
- Bonus robustness worth a test line: even out-of-branch inputs (`-10`, `370`)
  wrap to the correct drawn angle under this coord, so a float artifact that
  escapes `modu` degrades gracefully instead of clamping to an edge.

## 4. `amax` back-compat on the geoms

**Accept-and-note is the right stance; erroring is wrong. Two amendments.**

- **Why not error:** R4 requires existing scripts to keep working, and the
  package's *own documented examples* pass `amax` to the geoms
  (`R/geom_ssm.R:77-83`, `:149-158`), as do all three internal plot families
  (`R/ssm_plot.R:159,:176,:195`; `R/cpm_oop.R:372,:387`;
  `R/fit_structure_oop.R:297`). An error would break every script derived from
  the docs — the opposite of a self-healing migration.
- **Why the silent-ignore hazard is acceptable here:** normally, ignoring an
  argument the user believes is load-bearing is dangerous because it can yield
  a silently wrong result. Here the coord *guarantees* ring/data alignment, so
  a stray geom `amax` cannot produce an internally inconsistent figure — the
  failure mode collapses from "silent misalignment" (the current defect 1) to
  "notified no-op". The only real behavior change is the old *mismatched*
  case, which stops rendering the historically misaligned figure and starts
  rendering the correct one — the note is the user's only explanation, so it
  must fire.
- **Amendment 1 — make it unconditional, not "when a `coord_circumplex()` is
  present".** After M31, `ggcircumplex()` always returns the coord-based
  canvas, so there is *no* code path in which a geom `amax` can ever be honored
  again. A conditional note implies a live alternative semantics that won't
  exist; it also forces the geom to introspect the plot's coord, which a layer
  cannot cleanly do at layer-construction time. Simpler and more honest: the
  argument is inert, note once (`rlang::inform(..., .frequency = "once")`),
  text naming the new owner, e.g. "amplitude scaling is owned by
  `coord_circumplex()`; `amax` supplied to `geom_ssm_*()` is ignored."
- **Amendment 2 — change the default to a sentinel** (`amax = NULL` or a
  deprecated() marker) so the note fires only on *explicit* use. `amax` sits
  after `...` (named-only, `R/geom_ssm.R:84-87`), so changing its default is
  signature-compatible. Without this, the note can never distinguish "user
  passed 0.5" from "default 0.5".
- Escalation: note → warning in a later release per the r-package profile's
  deprecation slot; permanent acceptance is also defensible under R4. The same
  treatment applies to `geom_ssm_arc(n=)`, which the rewrite also makes
  vestigial (see Q6, gap 9).

## 5. `ggforce` retention (V6 holding)

**Under Option A nothing load-bearing remains in principle — but do not touch
the pin in the M30/M31 design; make removal its own gated decision.**

Complete inventory of ggforce use (grep over `R/`, `src/`, `NAMESPACE`,
`DESCRIPTION`): `ggforce::GeomArcBar` (`R/geom_ssm.R:164`),
`ggforce::StatArcBar` (`:171`, `:205`), `ggforce::geom_circle` ×2
(`R/ssm_plot.R:575`, `:593`), plus the Imports pin (`DESCRIPTION:35`). That is
all. The downstream plot families (`plot.circumplex_cpm`,
`plot.circumplex_fit_structure`) reach ggforce only *through* the shared geoms
and `ggcircumplex()`, never directly. Option A eliminates both uses: arc →
coord-bent `GeomRect` (verified renderable, Q2), rings → r gridlines in the
coord's background.

What M31 must verify before touching the pin (the brief's actual question):

1. All three plot families are actually off `StatArcBar`/`geom_circle` — i.e.
   the rewrite is *complete*, including `plot.circumplex_cpm` and
   `plot.circumplex_fit_structure` (the spec omits both; see Q6, gap 7).
2. The zero-width-wedge behavior is re-owned: `plot.circumplex_cpm` *depends*
   on a zero-angular-width wedge dropping out of the arc layer's computed data
   (the legend-ordering workaround documented at `R/cpm_oop.R:350-354`). That
   dropping is a `StatArcBar` tessellation side effect; a zero-width `GeomRect`
   instead draws a degenerate radial line. The new `setup_data()` must drop
   `xmin == xmax` rows explicitly, with a test (Q6, gap 5).
3. `grep -r "ggforce"` over `R/`, `tests/`, `vignettes/`, `NAMESPACE` returns
   nothing, and `R CMD check` + the full suite pass with ggforce removed from
   Imports.
4. The removal is recorded as a D-entry superseding the V6 KEEP holding —
   dropping a dependency is the doctrine-favored direction (D-006/D-014), but
   it is still a dependency decision and V6 is an explicit written holding; it
   should be overturned on evidence, not as a silent side effect of M31.

## 6. Overall soundness & gaps

The requirement set R1–R5 and invariants I1–I4 are the right frame, Option C is
correctly rejected (a carrier scale cannot deliver R3, and no simpler
alternative delivers all of R1–R3 — concur with the spec's analysis). The
recommendation logic of §4 is sound. Gaps, in rough severity order:

1. **The §7 floor claim is factually wrong** (blocking for the T3 gate as
   written). The Option-A parameter table is drawn from the ggplot2 4.0.x
   signature: `thetalim`, `rlim`, and `reverse` do not exist in any 3.5.x
   release (verified against v3.5.2 source; 3.5.x has `direction`, which 4.0.0
   deprecates, and no limits arguments — limits could only come from scales,
   which *censor* rather than zoom, breaking the Q2 mechanism). The real floor
   for the design as specced is **ggplot2 ≥ 4.0.0** (2025-09-11). Supporting
   3.5.x as well would mean writing the subclass against two incompatible
   generations of coord/guide internals (pre/post S7, `direction`/`reverse`,
   `r_axis_inside` rename) — reject that; see verdict.
2. **Missing test: panel-range pinning.** Nothing in T-i1…T-r2 asserts that
   the built panel's theta range is exactly `[0, 360]` with expansion off
   *regardless of the data's displacement range*. This is the single guard
   both Q2 (periodic extension) and Q3 (pole closure) hang on, and it is one
   `ggplot_build()` assertion (`panel_params$theta.range`). Add it (T-i1b).
3. **Missing test: seam-adjacent non-straddling arcs.** `[350, 360]` and
   `[0, 10]` — bounds *touching* the pole without straddling it. These
   exercise `ssm_arc_span()`'s edge (`max == 360`, `min == 0`, no +360
   extension) and the drawn adjacency at the seam. The current suite's
   convention (test at 0°/360°) demands them; the T list has only the
   straddling case.
4. **Missing test: full-circle rejection survives the move.** The `span >= 360`
   error (`R/geom_ssm.R:194-202`) currently lives in `StatSsmArc`, which
   Option A deletes. The T list must carry a test that the rejection (same
   message contract) survives in the new `setup_data()` home — this is also
   what `plot.circumplex_cpm`'s `drawable` pre-filter assumes exists
   (`R/cpm_oop.R:335`).
5. **Missing test/decision: zero-width wedge.** See Q5 item 2 —
   `xmin == xmax` must drop, not draw a radial line, and the cpm legend-order
   coupling must keep holding.
6. **Hidden coupling the spec misses: the repel branch.**
   `ssm_plot_circle(repel = TRUE)` computes canvas cartesian coordinates
   directly (`ssm_to_cartesian()`, `R/ssm_plot.R:211-213`) and nudges labels in
   *canvas units* (`nudge_x = -8 - .canvas_x`, `:222`). Under the coord,
   layer positions are data-space `(displacement, amplitude)`; hand-computed
   cartesian positions would be re-transformed and land nonsensically, and the
   "-8 canvas units left" nudge has no data-space equivalent (x-nudge becomes
   *angular*). ggrepel under a non-linear coord also needs its own behavioral
   check. This branch needs an explicit M31 task (redesign, likely via
   `coord`-aware annotation or ggrepel's npc-space hooks), not a mechanical
   port.
7. **Hidden consumers the spec's R4 list misses:** `plot.circumplex_cpm`
   (`R/cpm_oop.R:355-392`) and `plot.circumplex_fit_structure`
   (`R/fit_structure_oop.R:290-297`) both compose `ggcircumplex()` + the geoms
   with per-layer `amax`. They ride the same rewrite and must be in R4's
   keep-working set and in the snapshot-regeneration sweep. (cpm maps
   *communality* to the radial axis with `amax = 1` semantics — `rlim = c(0,
   1)` expresses it directly; no special handling needed, but it must be
   tested.)
8. **Scale-collision semantics change.** Under `theta = "x"`, a user's
   `+ scale_x_continuous()` on a circle plot now replaces the canvas's spoke
   breaks (today it only touches the hidden position scales). This is mostly
   *good* — `scale_x_circumplex()` becomes genuinely useful on the circle, and
   its docs (`R/scale_circumplex.R:29-36`, "linear circumplex plot") should be
   updated — but the "replacing scale" message and the break-override behavior
   are user-visible and belong in the M31/M34 doc plan.
9. **`geom_ssm_arc(n=)` becomes vestigial** — curve smoothness is now owned by
   the coord's munching (which 4.0.0 explicitly improved: "Munching in
   `coord_polar()` and `coord_radial()` now adds more detail", NEWS 4.0.0; the
   probe's 20° wedge munched to 33 vertices, ~1.2°/vertex — smooth). Give `n`
   the same accept-and-note treatment as `amax`; the spec only mentions `amax`.
10. **`inner.radius` conflation in the §3 table.** R2 does not need
    `inner.radius`: `rlim = c(center, amax)` alone puts amplitude = center at
    the exact panel center (probed: `rlim = c(0.2, 0.5)`, point at `y = 0.2` →
    `(0.5, 0.5)`). `inner.radius` is an independent donut-hole *visual* and
    should default to 0 to preserve the current look (points reach the
    center). The spec should decouple the two so M31 doesn't wire `center`
    through the wrong parameter.
11. **Snapshot strategy note.** Every visual snapshot regenerates wholesale
    under the new canvas; the spec's data-level assertion stance (M13/M27
    lesson) is right and should be paired with the grob-level angular-coverage
    recipe from Q2/Q3 for the boundary tests, plus one human-eyeball pass at
    review. No byte-level continuity with V4 snapshots is achievable or worth
    chasing.

With gaps 2–5 added, the test list is sufficient to catch angular/boundary
regressions; gaps 6–7 are scope corrections for M31's task list rather than
design flaws; gap 1 must be fixed in §7 before the T3 dependency gate records
its D-entry.

---

## Beyond the brief

- **B1 (subsumes gap 1, stated as a finding):** the spec's central table (§3)
  was evidently written against the *installed* ggplot2 (4.0.3) while §7
  reasoned about the 3.5.0 floor — a version mismatch inside the spec itself.
  On 3.5.x, besides the missing limits/`reverse` arguments, the r-axis-inside
  numeric placement used for ring labels is also absent (`r.axis.inside`
  numeric is 4.0.0, NEWS). Everything this review verified was verified on
  4.0.3 and holds there.
- **B2:** `CoordRadial` stores its limits as a plain field
  (`cr$limits ≡ list(theta = c(0, 360), r = c(0, 0.5))`, probed). If the T3
  gate ever *did* insist on a 3.5.0 floor, a subclass could in principle pin
  panel ranges in `setup_panel_params()` without the constructor arguments —
  but that means re-owning limits logic against pre-S7 internals and testing
  two generations; rejected above (R-12), recorded here so the option is known
  to have been considered rather than missed.
- **B3:** `ggrad()` (`R/utils.R:72`) and `ssm_to_cartesian()`
  (`R/geom_ssm.R:16`) become dead code after M31 if the repel branch is
  redesigned off hand-computed cartesian coordinates; `ssm_arc_span()`,
  `ssm_has_location()`, `ssm_has_region()` remain load-bearing (geom
  `setup_data()`, cpm pre-filters). Worth an explicit dead-code sweep in M31's
  review so the polar transform genuinely lives in exactly one place — the
  coord — which is the whole point of the milestone.
- **B4:** The theta guide places labels *outside* the circle by default and
  `clip = "off"` is `coord_radial`'s default; the current canvas's manual
  25%/10% axis expansions for label room (`R/ssm_plot.R:572-573`) are replaced
  by ordinary margin/axis-text spacing, which is another quiet win for R3 —
  no action needed, noted so M31 doesn't re-add the expansions reflexively.

## Recommendations

- **R-1 (apply):** GO on Option A — `coord_circumplex()` subclassing
  `CoordRadial`. Evidence in §1; B is higher total risk.
- **R-2 (apply):** Correct §7 and record the dependency D-entry as **ggplot2
  ≥ 4.0.0**, not 3.5.0 (§6 gap 1 / B1). The spec's own effective-floor logic
  still holds (CRAN users on R ≥ 4.1 get 4.0.3); name the excluded cohort
  honestly: environments pinned to pre-S7 ggplot2 (e.g. renv locks avoiding
  the 4.0.0 transition).
- **R-3 (apply):** Seam unwrap by *extension* in the arc geom's
  `setup_data()` (`xmax <- xmin + ssm_arc_span(...)`, may exceed 360), one
  rect, coord wraps periodically. Full-circle rejection and span validation
  move there too. (§2)
- **R-4 (apply):** `coord_circumplex()` hard-pins `thetalim = c(0, 360)`,
  `expand = FALSE`, `start = pi/2`, `reverse = "theta"` internally; none are
  user arguments; range pinning is coord-side (zoom), never scale-limits
  (censor). (§2 guard, §3 guard)
- **R-5 (apply):** `amax` soft-deprecation is *unconditional* with a sentinel
  default and a one-time note naming the coord as owner; same treatment for
  `geom_ssm_arc(n=)`. (§4, §6 gap 9)
- **R-6 (apply):** Extend the T list: panel-range pin (T-i1b), seam-adjacent
  non-straddling arcs `[350,360]`/`[0,10]`, full-circle rejection relocation,
  zero-width-wedge drop, and grob-level pole assertions with tolerance.
  (§6 gaps 2–5, §3)
- **R-7 (apply):** Add to M31's scope: the `repel = TRUE` branch redesign and
  the `plot.circumplex_cpm` / `plot.circumplex_fit_structure` ports; add both
  families to R4's keep-working set. (§6 gaps 6–7)
- **R-8 (consider):** ggforce removal — likely fully removable, but only after
  M31 passes the §5 verification checklist; record as a D-entry superseding
  V6, never as a silent side effect.
- **R-9 (consider):** Ring-label styling — stock `axis.text.r` (fully themed,
  plain text) vs reproducing the boxed-label look in `render_bg()`. An
  aesthetic call for M31/the user; neither endangers an invariant.
- **R-10 (reject — breaks R4):** Erroring on a stray geom `amax`. It would
  break the package's own documented examples and every script derived from
  them; the coord makes the argument harmless, so an error punishes users for
  a bug that no longer exists. (§4)
- **R-11 (reject — visual regression):** Pre-splitting the seam arc into two
  rects. Two polygons ⇒ stroked radial borders at the seam inside the wedge
  (probed); the extension mechanism achieves a single clean polygon. (§2)
- **R-12 (reject — compat-matrix explosion):** Supporting both ggplot2 3.5.x
  and 4.0.x internals in the subclass to keep the lower floor. Two
  incompatible coord/guide generations for a transient cohort, in the package
  layer that carries the angle invariants — exactly where cross-version
  rendering drift is least acceptable. (B2)

## Verdict

**GO (Option A)** — subclass `CoordRadial` as `coord_circumplex()`, with the
ggplot2 floor re-pinned to **≥ 4.0.0** (not 3.5.0; the spec's tabled mechanism
does not exist in 3.5.x), the R-3/R-4 seam-and-pole guards made part of the
design, and the R-6/R-7 test and scope additions folded into M31.
