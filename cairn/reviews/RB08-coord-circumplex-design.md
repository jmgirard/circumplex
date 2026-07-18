# RB08: Circumplex coordinate-system design (M30)

- **Date:** 2026-07-17
- **Output required:** write findings to `cairn/reviews/RR08-coord-circumplex-design.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

**The package.** `circumplex` is an R package (on CRAN) for circumplex data
analysis via the Structural Summary Method (SSM). An SSM analysis summarizes a
profile of correlations/means across circumplex scales by fitting a cosine
curve, yielding parameters **amplitude** `a` (how peaked the profile is; the
signal magnitude, ≥ 0) and **displacement** `d` (the angular location of the
peak, in **degrees, counterclockwise from the right/+x axis**), plus elevation,
x-value, y-value, and model fit. Results carry bootstrap confidence intervals
for each parameter.

**Angle conventions (hard invariants).** Angles are degrees in `[0, 360)` in the
user API, but the 0/360 pole is labelled **LM = 360** (it prints as `360`, not
`0`; `octants()` returns `..., 315, 360`). Displacement runs CCW from +x. A
displacement CI that straddles the 0/360 seam is stored with its lower bound
greater than its upper bound (`d_lci > d_uci`, e.g. `350 → 10` is a 20° interval
across the pole). A profile with zero/near-zero amplitude has *undefined*
displacement (`d_est = NA`) and no angular location.

**The visualization extension (what this review is about).** The package draws
circumplex figures on a circular canvas. The current implementation
(`R/ssm_plot.R`, `R/geom_ssm.R`, `R/scale_circumplex.R`) is a ggplot2 extension:

- `ggcircumplex()` → internal `circle_base()` builds the empty canvas — amplitude
  rings (`ggforce::geom_circle`), displacement spokes (`geom_segment`), and scale
  labels (`geom_label`) — on a `theme_void()` base with hidden continuous x/y
  scales in a radius space of ~`[-5, 5]`, held square by `coord_fixed()`.
- `geom_ssm_point()` (`GeomSsmPoint ⊂ GeomPoint`) and `geom_ssm_arc()`
  (`StatSsmArc ⊂ ggforce::StatArcBar`) are the data layers. They compute
  cartesian `x`/`y` **themselves** — the amplitude→radius rescale is
  `a * 5 / amax` (`R/geom_ssm.R:11`, `ssm_radius()`), displacement→angle via
  `ggrad()`.
- `ssm_plot_circle()` composes the canvas + arcs + points; `ssm_plot_curve()` and
  `ssm_plot_contrast()` are separate (a Cartesian angle-axis curve and a
  Cartesian difference plot).

**The three defects M30 addresses** (recorded in the design doc; see the spec
§1): (1) `amax` — the amplitude→radius scale factor — is a per-layer parameter
supplied *independently* to `ggcircumplex()` and to each geom, and the caller
must keep them equal by hand; a mismatch **silently misaligns** points/arcs from
the rings. (2) There is **no configurable amplitude center** (hard-wired to 0); a
past attempt at an `amin` argument relabelled the rings while the geoms still
mapped `a*5/amax`, silently mislabelling the axis, and was removed. (3) The
canvas **does not respond to themes** because its furniture is drawn geoms under
`theme_void()`. All three share one root cause: **no object owns the polar
transform**.

**Milestone M30** is the *design* milestone (docs-only) that decides the
coordinate-system API; **M31** builds it. M30 is tagged `irreversible-api` (a new
exported coordinate system) + `ip-touching` (it re-owns the 0/360 polar
transform that carries the angle invariants), which is why it is being escalated
to independent review before any code is written.

**The proposed design** is in `devel/m30-coord-spec.md` (read it in full). In
brief, it recommends **Option A**: a new exported `coord_circumplex()` that
subclasses ggplot2's `CoordRadial` (ggplot2 ≥ 3.5.0), so that `amax` and the
center become the r-scale limits (`rlim` / `inner.radius`) — making defects 1
and 2 structurally impossible — the canvas grid themes natively (defect 3), and
the arc geom simplifies to a `GeomRect` that the polar coord bends into an
annular wedge. **Option B** (a bespoke coord on the base `Coord`/`CoordPolar`
API, no ggplot2 floor bump, more owned code) is the fallback. **Option C**
(carrier scale, keep the drawn canvas) is rejected as a half-measure.

## Materials

Read, in this order:

1. **`devel/m30-coord-spec.md`** — the full design spec under review (problem,
   requirements R1–R5 + invariants I1–I4, the three mechanism options, the
   Option-A API design, the invariant-preservation test list, the dependency
   implications, and §8 "Open questions for Fable"). Your review answers §8 and
   assesses the whole.
2. **`R/geom_ssm.R`** (all 210 lines) — the current geoms/stat: `ssm_radius()`
   / `ssm_to_cartesian()` (`:11`, `:16`), the `ssm_has_location()` /
   `ssm_has_region()` plottability predicates (`:31`, `:34`), `ssm_arc_span()`
   and its 0/360 unwrap (`:47`), `GeomSsmPoint` (`:95`), `StatSsmArc` (`:170`,
   note its `compute_panel()` seam-unwrap and the full-circle rejection at
   `:194`).
3. **`R/ssm_plot.R`** — `ssm_plot_circle()` (`:46`) shows how canvas + arc +
   point compose today, including the `amax` dual-supply (`:159`, `:96`) and the
   undefined-displacement warning (`:82`); `circle_base()` (`:562`) is the drawn
   canvas.
4. **`R/scale_circumplex.R`** — `scale_x_circumplex()` and
   `resolve_circumplex_labels()` (the shared label resolver, used by both the
   canvas and the linear axis).
5. **`R/utils.R`** around `ggrad()` (`:72`) — the degrees→ggforce-arc-radian
   convention.
6. **`cairn/DESIGN.md`** "Visualization extension" section (≈ lines 289–357) —
   the architecture-as-built and the recorded known limitations (including the
   V6 "KEEP ggforce" holding and the R3 `amin`-removal history).
7. `octants()` in the package (returns the 8 default scale angles ending in
   360); `R/ssm_plot.R:565` shows `circumplex_degree_labels()`.

**To run code:** `Rscript -e 'devtools::load_all(); <expr>'` from the repo root.
`coord_radial()`'s parameters: `theta, start, end, thetalim, rlim, expand,
direction, clip, r.axis.inside, rotate.angle, inner.radius, reverse`. Installed
here: ggplot2 4.0.3, ggforce 0.5.0. A minimal end-to-end figure to study:
`data("jz2017"); res <- ssm_analyze(jz2017, scales = 2:9, measures = c("NARPD",
"ASPD")); ssm_plot_circle(res)`.

## Questions

1. **Option A vs Option B — the load-bearing call.** Is subclassing
   `CoordRadial` (Option A) the right mechanism, given the goal is a *fixed
   circumplex canvas* with specific label conventions? Concretely: can
   `CoordRadial`'s guide/label machinery (`guide_axis_theta`, the r-axis guide)
   express the circumplex conventions — **scale-abbreviation or degree labels at
   the spokes, amplitude-value labels on the rings, and LM = 360 at the pole** —
   cleanly, without fighting the stock coord? Or does the label/guide surface
   fight `coord_radial` badly enough that the bespoke Option B (base `Coord`,
   full `render_bg`/guide control, no floor bump) is actually *lower total
   risk*? Give a definite recommendation.

2. **Seam-straddling arc under a polar coord (invariant I2).** Option A turns the
   confidence-region arc into a `GeomRect` in `(displacement, amplitude)` space
   that the coord bends into an annular wedge. A rectangle with `xmin > xmax`
   (the stored representation of a seam-straddling CI, e.g. `d_lci = 350,
   d_uci = 10`) does **not** wrap across the theta seam under a stock polar
   coord. Where should the unwrap live — pre-split the rectangle at the 0/360
   seam in the geom's `setup_data()` (as the current `StatSsmArc` effectively
   does via `ssm_arc_span()`, `R/geom_ssm.R:193`), or inside the coord's
   `transform()`? Which keeps the invariant testable and avoids a double-draw or
   a gap at the seam? Does either approach interact badly with `CoordRadial`'s
   own theta wrapping?

3. **Pole exposure (invariant I3 / the float-label issue).** A displacement of
   exactly the pole can arrive as `360.0` or as `0.0` (a documented float
   `modu`-at-the-edge artifact; the package treats both as the same pole). Under
   Option A the theta position is computed from the x-scale range `[0, 360]`.
   Confirm (or refute) that `d = 360.0` and `d = 0.0` map to the **identical**
   drawn angle with no one-pixel seam gap, no duplicated draw, and no dependence
   on `expand`/`thetalim`. If there is a risk, name the guard.

4. **`amax` back-compat on the geoms.** The spec proposes that when a
   `coord_circumplex()` is present, a geom's now-redundant `amax` argument is
   **accepted but ignored with a one-time note** (soft-deprecation), so the old
   mismatch bug self-heals rather than breaking existing scripts. Is
   accept-and-note the right stance, or should a stray geom `amax` error? Is
   there a hazard in a geom silently ignoring a positional/aesthetic-adjacent
   argument the user may believe is doing something?

5. **`ggforce` retention (V6 holding).** DESIGN.md's V6 review decided to KEEP
   `ggforce` because `StatSsmArc` reuses its annular-sector tessellation
   (`StatArcBar`) and `circle_base()` uses `geom_circle` for the rings. Under
   Option A the arc becomes a coord-bent `GeomRect` (no `StatArcBar` needed) and
   the rings move into the coord's `render_bg`. Assess: does Option A make
   `ggforce` fully removable from Imports, or does something load-bearing remain?
   (Do **not** recommend dropping it speculatively — advise what M31 must verify
   before touching the pin.)

6. **Overall soundness & anything missed.** Is the spec's requirement set
   (R1–R5) and invariant set (I1–I4) complete for this rewrite? Is the
   invariant-preservation test list (§6, tests T-i1…T-i4, T-r1/T-r2) sufficient
   to catch a regression in the angular/boundary behavior, or is a boundary case
   missing? Flag any correctness risk, hidden coupling, or simpler alternative
   the spec did not consider.

## Constraints

Fixed; flag disagreement explicitly rather than silently working around these.

- **Minimal-dependency doctrine (D-006, D-014).** Base R + a minimal Imports set
  (boot, ggplot2, ggforce, htmlTable, Rcpp/RcppArmadillo, rlang); no tidyverse in
  package code. **Adding a new package to Imports is out of scope** for this
  design. Re-pinning the *existing* ggplot2 floor (3.3.0 → 3.5.0 for Option A) is
  permitted **but is a dependency decision** gated separately and recorded as a
  D-entry (M30 T3) — so if you recommend Option A, treat the floor bump as a real
  cost to weigh, not free.
- **Back-compat / wrappers retained (D-018).** `ssm_plot_circle/curve/contrast()`
  and `ggcircumplex()` must keep their public signatures and remain as
  convenience wrappers over the new system — **no deprecation of these**, no
  breaking change to their call sites. Folding them into `plot()` S3 methods was
  considered and rejected; do not reopen it.
- **The angle invariants are inviolable** (LM = 360; `[0,360)` API; CCW-from-right;
  seam-straddle `d_lci > d_uci`; the pole float-label equivalence; undefined
  displacement dropped). The new transform must preserve every one; that is the
  point of the `ip-touching` tag. Do not propose a design that relaxes any of
  them.
- This is a **design review of a written spec**, not an implementation review —
  no code exists yet. Your job is to de-risk the M31 build: confirm the
  mechanism, settle the open questions, and hand M31 a concrete, testable
  direction (or a clear NO-GO with rationale).

## Output format

In `RR08-coord-circumplex-design.md`: answer each question 1–6 by number with
your reasoning and evidence (cite file:line and, where you ran something, the
result). List any additional findings separately under "Beyond the brief". End
with concrete recommendations, each marked **apply / consider / reject-with-
reason**, and a one-line **overall verdict**: GO (Option A), GO (Option B), or
NO-GO (keep the current drawn-canvas known-limitations), with the ggplot2-floor
recommendation if applicable.
