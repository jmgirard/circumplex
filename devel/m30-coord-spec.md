# M30 — Circumplex coordinate-system design spec

_Status: REVISED per RR08 (Fable, 2026-07-17); **read §11 first** — it is the
authoritative outcome and M31 punch-list and supersedes any conflicting detail
in §3–§7 below. Milestone: M30 (design) → M31 (build)._
_Author: /milestone-implement M30, 2026-07-17. Binding once M30 records its
GO/NO-GO D-entry (M30 T3)._

Scope tags: **irreversible-api** (introduces a new exported coordinate system /
API surface) + **ip-touching** (re-owns the 0/360 polar transform that
implements the package's angle invariants). This spec exists so that decision is
made once, reviewed, and testable.

---

## 1. Problem statement

The M3 visualization extension draws circumplex figures on a *drawn-geometry*
canvas: `ggcircumplex()` → internal `circle_base()` emits rings
(`ggforce::geom_circle`), spokes (`geom_segment`), and labels (`geom_label`) onto
a `theme_void()` base with hidden continuous x/y scales in a radius space of
roughly `[-5, 5]`, held square by `coord_fixed()`. The data layers
(`geom_ssm_point`, `geom_ssm_arc`) compute cartesian `x`/`y` themselves — in
`GeomSsmPoint$setup_data()` and `StatSsmArc$compute_panel()` — via
`ssm_to_cartesian()` / `ssm_radius()`, rescaling amplitude as `a * 5 / amax`.

DESIGN.md ("Visualization extension" → Known limitations) records three coupled
defects, all rooted in the same cause — **there is no object that owns the
polar transform**:

1. **`amax` is a per-layer parameter, not shared state.** The amplitude→radius
   scale factor lives independently on `ggcircumplex()` *and* on each geom, and
   the caller must keep them equal by hand; a mismatch silently misaligns points
   and arcs from the rings (`DESIGN.md:332-337`, `R/geom_ssm.R:11`,
   `R/ssm_plot.R:159`).
2. **No configurable amplitude center.** The center is hard-wired to amplitude 0.
   An early exported `ggcircumplex(amin=)` relabelled the rings on an
   `amin..amax` scale while the geoms still mapped `a*5/amax` (center 0),
   silently mislabelling the axis; `amin` was removed from the public
   constructor (R3, 2026-07-03) and survives only as `circle_base()`'s internal
   default (`DESIGN.md:338-343`).
3. **The canvas does not respond to themes.** Because rings/spokes/labels are
   drawn geoms under `theme_void()`, `+ theme_bw()` etc. cannot restyle them;
   only the data layers theme normally (`DESIGN.md:344-347`).

All three are "fix via a coord/scale that owns `amax` and the polar transform —
a substantial rewrite, deferred" in DESIGN.md. M30 decides that coord/scale;
M31 builds it.

## 2. Requirements

**Functional.**

- **R1 (single source of truth).** `amax` (and the polar transform) is owned in
  exactly one place; a canvas and its data layers cannot disagree. Ideally the
  per-layer `amax` argument disappears from the public call.
- **R2 (configurable center).** The amplitude at the circle's center is
  configurable, with the ring labels and the amplitude→radius mapping guaranteed
  to agree (defect 2 is structurally impossible, not merely fixed).
- **R3 (theme-responsive furniture).** Rings/spokes/labels restyle through
  ggplot2 theme elements (`panel.grid.major`, `axis.text`, …) rather than being
  frozen drawn geoms.
- **R4 (back-compat).** `geom_ssm_point()`, `geom_ssm_arc()`, `ggcircumplex()`,
  `scale_x_circumplex()`, and `ssm_plot_circle/curve/contrast()` keep their
  public signatures and keep producing correct figures. `ssm_plot_*` and
  `ggcircumplex()` are retained as convenience wrappers (D-018b), not deprecated.
- **R5 (composability).** The new system is a normal ggplot2 object added with
  `+`, usable with arbitrary other layers/scales/themes (the M3 promise).

**Invariants that MUST survive the transform (CLAUDE.md; ip-touching).**

- **I1** Angles are degrees in `[0, 360)` in the user API, **LM = 360** (the
  0/360 pole prints as `360`, not `0`); displacement runs counterclockwise from
  the right (+x axis). `octants()` and instrument angles are the inputs.
- **I2** A displacement CI stored with `d_lci > d_uci` is a seam-straddling
  interval and its arc is drawn **the short way across the 0/360 seam** (current
  `ssm_arc_span()` convention, `R/geom_ssm.R:47`).
- **I3** A profile peaking at 0°/360° renders at the pole with no artifact
  (D-003: the pole may arrive as `360.0` or `0.0` from float `modu`; the drawn
  position must be identical either way).
- **I4** A flat / zero-variance profile (undefined displacement, `d_est = NA`)
  has no location and is dropped, not mis-drawn (`ssm_has_location()`,
  `ssm_has_region()`).

## 3. Mechanism options

Three candidate mechanisms. The pivotal realization: **ggplot2 already has a
polar coordinate family** (`coord_polar()`, and since 3.5.0 the richer
`coord_radial()`), and a coord is precisely "an object that owns the transform
from data space to the drawn panel." A polar coord makes R1/R2/R3 fall out of
existing machinery instead of being hand-built.

### Option A — subclass `CoordRadial` as `CoordCircumplex` (recommended)

`coord_radial()` (ggplot2 ≥ 3.5.0) maps a continuous `theta` variable to angle
and the other position to radius, with parameters that line up one-to-one with
our requirements:

| coord_radial param | circumplex use |
|---|---|
| `theta = "x"`, `start`, `direction` | displacement axis: x-scale over `[0,360]`, `start`/`direction` set to LM=360, CCW-from-right (I1) |
| `rlim = c(center, amax)` | **R1 + R2**: `amax` and the center are the r-scale limits — one owner, trained once |
| `inner.radius` | **R2**: the hole at the center (and the center-value semantics) |
| panel grid (`panel.grid.*`, `guide_axis_theta`) | **R3**: spokes = theta gridlines, rings = r gridlines, themed |

Under this option the data geoms become *simple*:

- `geom_ssm_point` → emits `aes(x = displacement, y = amplitude)`; the coord
  bends `(theta, r)` → canvas. **The per-layer `amax` argument is deleted** (R1):
  amplitude is a normal `y` trained by the shared r-scale.
- `geom_ssm_arc` → emits a **rectangle** in `(displacement, amplitude)` space
  (`xmin=d_lci, xmax=d_uci, ymin=a_lci, ymax=a_uci`); a polar coord bends a
  data-space rectangle into an annular wedge automatically. This is the elegant
  payoff — the annular-sector geometry we currently borrow from
  `ggforce::StatArcBar` becomes a plain `GeomRect` the coord curves.

`CoordCircumplex` subclasses `CoordRadial` and overrides:

- **`render_bg()`** — draw the circumplex-specific furniture (numbered amplitude
  rings, scale-labelled spokes, degree/abbrev labels) as themed panel elements,
  reproducing the current look but responsive to theme (R3). This is where most
  of the code lives.
- **`setup_panel_params()` / `transform()`** — pin the LM=360 / CCW-from-right
  convention (I1) and the seam-straddle handling for arcs (I2), independent of
  `coord_radial`'s default guide labelling.

**Cost:** re-pins the ggplot2 floor `>= 3.3.0` → `>= 3.5.0` (dependency change →
gate + D-entry at M30 T3; see §7). `coord_radial` is newer and its internal
guide/label API is less settled than base `Coord`.

### Option B — bespoke `CoordCircumplex` on the base `Coord` API

Subclass ggplot2's base `Coord` (or `CoordPolar`, available since 3.0) and
implement `transform()`, `setup_panel_params()`, `render_bg()`,
`render_axis_*()` from scratch. Full control of every convention; **no ggplot2
floor bump** (base `Coord` and `CoordPolar` predate 3.5). Cost: we own the
entire polar-render machinery `coord_radial` would give for free (more code, the
larger part of the "substantial rewrite"), and must re-derive the annular-wedge
tessellation the geoms need (either keep `ggforce::StatArcBar`, or hand-roll).

### Option C — carrier scale, keep the drawn-geometry canvas

Introduce a scale/annotation object that carries `amax` and injects it into the
geoms (e.g. a stat that reads a plot-level constant), leaving `circle_base()`'s
drawn rings/spokes as they are. Smallest diff; partially fixes R1 (one place to
set `amax`). **Does not deliver R2 or R3** — the center stays 0 and the canvas
stays theme-frozen, because the furniture is still drawn geoms. Rejected as a
half-measure that leaves two of the three defects standing.

## 4. Recommendation

**Option A (subclass `CoordRadial`), conditional on Fable concurrence and the
§7 ggplot2-floor gate.** Rationale:

- It is the only option under which R1 and R2 become *structural* — `amax` and
  center are r-scale limits owned once, so the dual-supply mismatch (defect 1)
  and the label/mapping disagreement (defect 2) cannot be expressed, rather than
  being fixed and re-breakable.
- R3 comes from the coord's themed panel grid.
- It *simplifies* the data geoms (point → `GeomPoint`; arc → `GeomRect` under
  polar bending), shrinking the surface that carries the invariants.

**Fallback: Option B** if the Fable review finds `coord_radial`'s guide/label
API cannot express the circumplex label conventions (LM=360 spoke labels,
amplitude-ring labels) cleanly, or if the ggplot2-floor bump is rejected at the
§7 gate. B reaches the same requirements with more owned code and no floor bump.

The choice between A and B is the **central question for Fable** (§8).

## 5. API design (Option A)

Exported surface (final names subject to Fable / M31):

- **`coord_circumplex(amax = NULL, center = 0, angles = octants(), start/direction = <LM=360, CCW>, ...)`** —
  the coordinate system. `amax = NULL` trains from the data's amplitude range
  (as `ssm_plot_circle()` already does via `pretty_max()`); `center` is R2.
- **`ggcircumplex(...)`** — **retained** (R4/D-018b) as a convenience constructor
  that returns `ggplot() + coord_circumplex(...) + <themed furniture>`, i.e. the
  empty canvas, exactly as today from the caller's view.
- **`geom_ssm_point()` / `geom_ssm_arc()`** — **retained** signatures. Internally
  they stop computing cartesian `x/y`; they map amplitude/displacement to the
  coord's `x/y` aesthetics. The `amax` argument is **deprecated-but-accepted**
  (ignored with a one-time note when a `coord_circumplex()` is present, since the
  coord now owns it) — a soft-deprecation, not a removal (R4). *(Exact
  deprecation mechanics: M31; the R-package profile's deprecation slot.)*
- **`scale_x_circumplex()`** — unchanged (it labels the linear angle axis of
  `ssm_plot_curve()`, which is Cartesian and not on this coord).
- **`ssm_plot_circle/curve/contrast()`** — **retained** as wrappers (R4/D-018b);
  `_circle()` is rebuilt on `coord_circumplex()`, `_curve()`/`_contrast()` are
  Cartesian and unchanged.

**Back-compat contract (R4).** A pre-M31 script that calls
`ggcircumplex(amax = k) + geom_ssm_point(..., amax = k)` still renders correctly:
the coord reads `amax = k`, and the geom's now-redundant `amax = k` is ignored
with a note. A script that passes *mismatched* `amax` values (the old silent
bug) now renders correctly to the coord's `amax` and notes the ignored geom
value — a strict improvement. Every existing vignette/example keeps working.

## 6. Invariant preservation & the M31 test list

How each invariant maps onto Option A, and the test M31 must carry (these
become M31's AC3 boundary tests):

- **I1 / LM=360 (T-i1).** `coord_circumplex` fixes `start`/`direction` so
  displacement 0 is at +x and increases CCW; a scale/label at the pole reads
  `360`. Test: an octant canvas labels the pole `360°`, and a point at
  displacement 0 and one at 360 land at the identical canvas position.
- **I2 / seam-straddle arc (T-i2).** A `GeomRect` with `xmin > xmax` does not
  cross the theta seam under a stock polar coord; `CoordCircumplex` must unwrap
  it (reuse the `ssm_arc_span()` short-way convention) — either by pre-splitting
  the rect at the seam in the geom's `setup_data`, or in the coord's transform.
  Test (the discriminating one): an arc with `d_lci = 350, d_uci = 10` renders a
  20° wedge across the pole, **not** a 340° wedge the long way; asserted at the
  data level (`ggplot_build()`), because `check()` cannot see a wrong figure
  (M13/M27 lesson).
- **I3 / pole (T-i3).** A profile with `d_est ∈ {0, 360}` (both float labels of
  the pole, D-003/M20) draws at the identical position; test both labels.
- **I4 / flat profile (T-i4).** A row with `d_est = NA` is dropped by
  `ssm_has_location()` before the coord sees it (unchanged predicate); test it
  is absent from the built data, and `ssm_plot_circle()` still warns by name.
- **R1 alignment (T-r1).** A point at amplitude `= amax` lands exactly on the
  outer ring; at `= center` lands at the center — with **no** per-layer `amax`.
- **R2 center (T-r2).** With `center = c0 ≠ 0`, a point at amplitude `c0` is at
  the middle, and the innermost ring label reads `c0` (guards the R3-era
  mislabel).

## 7. Dependency implications

**[CORRECTED per RR08 §6 gap 1 / B1 — the original 3.5.0 claim was wrong.]**
Option A re-pins **`ggplot2 (>= 3.3.0)` → `(>= 4.0.0)`**. The tabled parameters
`thetalim`, `rlim`, and `reverse` **do not exist in ggplot2 3.5.x** (verified by
Fable against the v3.5.2 source); the `r.axis.inside` numeric placement used for
ring labels is also 4.0.0-only. The design as specced therefore requires
**ggplot2 ≥ 4.0.0** (released 2025-09-11). This is a dependency change →
**question gate + D-entry, at M30 T3** (tracking rules; D-006/D-014 minimal-deps
doctrine). Assessment for that gate:

- 4.0.0 shipped 2025-09-11; installed here is 4.0.3. A user on a current ggplot2
  already satisfies it.
- Per the D-014 lesson, the *effective* floor must be measured, not the declared
  one: circumplex's install floor is already R ≥ 4.1 via ggplot2/htmlTable, and
  a user on R ≥ 4.1 installing from CRAN gets current ggplot2 (≥ 4.0). The
  honestly-named excluded cohort is environments *pinned* to pre-S7 ggplot2
  (e.g. an renv lock avoiding the 4.0.0 transition).
- Supporting 3.5.x *as well* is rejected (RR08 R-12): it would mean writing the
  subclass against two incompatible coord/guide generations (pre/post S7) in the
  exact layer that carries the angle invariants — where cross-version render
  drift is least acceptable.
- No **new** package enters Imports (ggplot2 and ggforce are already Imports);
  Option A may *reduce* `ggforce` reliance on the arc path (the coord curves a
  `GeomRect`), though `geom_circle` and other uses may remain — M31 measures
  whether ggforce is still load-bearing before touching its pin (V6 said KEEP;
  do not drop it speculatively).

Option B needs **no** floor bump. The floor decision is therefore *contingent on
the A-vs-B choice* and is recorded together with the GO/NO-GO in M30 T3.

## 8. Open questions for Fable (RB)

1. **A vs B — the load-bearing call.** Can `CoordRadial`'s guide/label API
   (`guide_axis_theta`, r-axis guides) express the circumplex label conventions
   — scale abbreviations at the spokes, amplitude-ring labels, LM=360 — cleanly
   enough to justify the floor bump, or does the label surface fight the stock
   coord badly enough that bespoke Option B is actually less total risk?
2. **Seam-straddle mechanism (I2).** Pre-split the arc rectangle at the 0/360
   seam in `geom_ssm_arc`'s `setup_data`, or unwrap inside the coord's
   `transform`? Which keeps the invariant testable and avoids double-drawing at
   the seam? (The current inline `ssm_arc_span()` is the reference behavior.)
3. **Does the polar coord change the pole float-label exposure (I3/D-003)?**
   Under `coord_radial` the theta position is computed from the x-scale; confirm
   a `d_est` arriving as `360.0` vs `0.0` maps to the identical angle and cannot
   produce a one-pixel seam gap or a duplicated draw.
4. **`amax` soft-deprecation on the geoms.** Is "accept-and-ignore-with-note when
   a `coord_circumplex` is present" the right back-compat stance (R4), or should
   a stray geom `amax` error? (Leaning: note, not error — the old mismatch case
   should self-heal, not break.)
5. **ggforce retention.** If the arc becomes a coord-bent `GeomRect`, is any
   ggforce use still load-bearing (rings via `geom_circle`?) or does the canvas
   move fully into the coord's `render_bg`? (Affects the V6 KEEP holding.)

## 9. Non-goals (M30)

- Implementation — M31.
- Geom ergonomics (exported ggproto generators, `na.rm` warn-parity, styling) —
  M32.
- Trajectory viz — M33; plotting vignette + pkgdown — M34.
- Any animation / on-circle movement paths — deferred candidate (ROADMAP).

## 10. Decision record (filled at M30 T3)

- A-vs-B outcome: **Option A** (subclass `CoordRadial`), RR08 verdict GO —
  empirically verified on ggplot2 4.0.3 that `CoordRadial` expresses every
  circumplex convention natively; Option B is higher total risk (owns more
  unstable internal API + re-derives the annular tessellation).
- ggplot2 floor bump: **≥ 4.0.0** (corrected from 3.5.0), gated + recorded as
  DECISIONS.md **D-019**.
- GO/NO-GO for M31: **GO** — see D-019.

## 11. Authoritative outcome & M31 punch-list (RR08-applied, 2026-07-17)

This section is binding for M31 and supersedes any conflicting detail above.

**Mechanism (RR08 R-1, R-3, R-4).**

- `coord_circumplex()` subclasses `CoordRadial`; ggplot2 floor **≥ 4.0.0**.
- The coord **hard-pins internally** (none exposed as user args):
  `thetalim = c(0, 360)`, `expand = FALSE`, `start = pi/2`, `reverse = "theta"`.
  Range pinning is **coord-side** (`thetalim` zooms), never scale-limits (which
  censor out-of-range x to NA and would break the seam mechanism).
- **Configurable center = `rlim = c(center, amax)` ALONE** (RR08 gap 10):
  amplitude = center lands at the exact panel center. `inner.radius` is an
  independent donut-hole visual and **defaults to 0** (points reach the center,
  preserving the current look). Do not wire `center` through `inner.radius`.
- **Seam-straddle (I2)** unwrap by **extension** in the arc geom's
  `setup_data()`: `xmax <- xmin + ssm_arc_span(xmin, xmax)` (may exceed 360);
  emit **one** `GeomRect`; the coord's periodic transform carries it across the
  pole (verified: 350→370 renders one clean 20° wedge). The span validation +
  full-circle (`span >= 360`) rejection move from `StatSsmArc` into this
  `setup_data()` with the same message contract.
- **`amax` and `geom_ssm_arc(n=)`** become inert: **unconditional**
  soft-deprecation with a **sentinel default** (`= NULL`/`deprecated()`) and a
  one-time `rlang::inform(.frequency = "once")` naming the coord as owner. Do
  not error (breaks the package's own documented examples — RR08 R-10).

**Back-compat / R4 keep-working set (RR08 R-7, gap 7)** — all must render
correctly and be in the snapshot/boundary sweep:
`ggcircumplex()`, `geom_ssm_point()`, `geom_ssm_arc()`, `scale_x_circumplex()`,
`ssm_plot_circle/curve/contrast()`, **`plot.circumplex_cpm`**
(`R/cpm_oop.R:355-392`; maps communality with `amax = 1` → `rlim = c(0, 1)`),
and **`plot.circumplex_fit_structure`** (`R/fit_structure_oop.R:290-297`).

**Two M31 tasks the spec originally missed (RR08 gap 6, R-7).**

- **`ssm_plot_circle(repel = TRUE)`** hand-computes canvas cartesian coords
  (`ssm_to_cartesian()`, `R/ssm_plot.R:211-213`) and nudges in canvas units —
  meaningless under the coord (x-nudge becomes angular). Needs a redesign
  (coord-aware annotation / ggrepel npc-space hooks), not a mechanical port.
- **Dead-code sweep** (RR08 B3): `ggrad()` (`R/utils.R:72`) and
  `ssm_to_cartesian()` become dead once the repel branch is off cartesian
  coords — confirm the polar transform lives in exactly one place (the coord).
  `ssm_arc_span`/`ssm_has_location`/`ssm_has_region` stay load-bearing.

**M31 boundary/regression test list (RR08 R-6; extends §6 T-i1…T-r2).**

- **T-i1b** panel-range pin: built `panel_params$theta.range == c(0, 360)` with
  expansion off, **regardless of the data's displacement range** (the guard both
  the seam and pole mechanisms hang on).
- **T-i2** seam-straddle `(350, 10)`: arc-layer data has `xmax == 370` after
  build; grob-level angular coverage is `(330,360] ∪ (0,30]` (via `layer_grob()`
  + `atan2()` binning), **not** the 340° complement.
- **T-i2b** seam-adjacent non-straddling: `[350, 360]` and `[0, 10]` (touch the
  pole without straddling; exercise `ssm_arc_span()`'s `max==360`/`min==0` edge).
- **T-i2c** full-circle rejection relocated: `span >= 360` still errors with the
  same message from the new `setup_data()` home (`plot.circumplex_cpm`'s
  `drawable` pre-filter, `R/cpm_oop.R:335`, assumes it exists).
- **T-i3** pole: `d ∈ {0, 360}` (both float labels) draw at the identical angle,
  asserted at **grob level with tolerance** (`≤ 1e-12` npc, not `identical()`);
  bonus: out-of-branch `-10`/`370` wrap correctly.
- **T-arc0** zero-width wedge: `xmin == xmax` **drops**, does not draw a
  degenerate radial line, and `plot.circumplex_cpm`'s legend-order coupling
  (`R/cpm_oop.R:350-354`) still holds.
- **T-r1** amplitude `= amax` on the outer ring / `= center` at center, no
  per-layer `amax`. **T-r2** non-zero `center` relabels rings + remaps
  consistently (guards the R3 mislabel).
- Snapshot strategy: data-level + grob-level assertions (M13/M27 lesson) for the
  boundary cases; wholesale snapshot regeneration + one human-eyeball pass at
  review (no V4 byte continuity — RR08 gap 11). Don't re-add `circle_base()`'s
  manual 25%/10% axis expansions (`clip="off"` + margins replace them — B4).

**ggforce (RR08 R-8, Q5).** Likely **fully removable** (4 call sites, all
eliminated by A) but only after M31 verifies: all three plot families off
`StatArcBar`/`geom_circle`; the zero-width-wedge behavior re-owned (T-arc0);
`grep -r ggforce` clean over `R/`,`tests/`,`vignettes/`,`NAMESPACE`; `check()` +
suite pass with it dropped. Removal is its **own D-entry superseding the V6 KEEP
holding** (DESIGN.md) — never a silent M31 side effect. Until then, keep the pin.

**Docs (M31/M34).** Under `theta = "x"`, a user's `+ scale_x_continuous()` now
replaces the spoke breaks (was only the hidden scales) — mostly good
(`scale_x_circumplex()` becomes useful on the circle); update its docs
(`R/scale_circumplex.R:29-36`) and note the behavior at M34.
