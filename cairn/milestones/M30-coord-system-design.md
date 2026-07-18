<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M30: Circumplex coordinate-system design

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Principles touched:** — (works under the CLAUDE.md angle invariants: [0,360), LM=360; IP/GP formalization deferred to /design-interview)
- **Branch/PR:** m30-coord-system-design

## Goal

Decide, via a Fable-reviewed spec, the coordinate-system API (a `CoordCircumplex`
or a carrier scale) that owns `amax` and the polar transform for circumplex
plots — resolving shared-state alignment, a configurable amplitude center,
theme-responsiveness, and back-compat with the existing geoms and `ssm_plot_*`
wrappers.

## Scope

**In:**
- Draft `devel/m30-coord-spec.md`: choose the extension mechanism (ggplot2
  `Coord` subclass vs a carrier position scale vs a hybrid), and specify how it
  owns `amax` so the canvas and the data layers can never disagree (the current
  silent-misalignment trap, [DESIGN.md:332-337](../DESIGN.md)).
- Specify the **configurable amplitude center** design and why it avoids the R3
  mislabel bug — the removed `ggcircumplex(amin=)` relabelled rings on an
  `amin..amax` scale while geoms mapped `a*5/amax` (center 0), silently
  mislabelling the axis ([DESIGN.md:338-343](../DESIGN.md)).
- Specify the **theme-responsiveness** approach (rings/spokes/labels currently
  drawn geoms under `theme_void()`, unresponsive to `+ theme_bw()`,
  [DESIGN.md:344-347](../DESIGN.md)).
- Specify the **back-compat contract**: `geom_ssm_point/arc()`,
  `ggcircumplex()`, `scale_x_circumplex()`, and `ssm_plot_circle/curve/contrast()`
  keep producing correct output — the latter three become thin convenience
  wrappers over the coord/layers (D-018b) — with a stated migration path.
- Argue preservation of the CLAUDE.md **angle invariants** through the new
  transform (profiles peaking at 0°/360°, CIs straddling the seam, LM=360, flat
  zero-variance profiles) and name the boundary tests M31 must carry.
- Escalate the spec to Fable (RB→RR) — `irreversible-api` + `ip-touching`.

**Out:**
- Implementation of the coord/scale → M31.
- Geom/layer ergonomics (exported generators, `na.rm` parity, styling) → M32.
- Longitudinal trajectory viz → M33; plotting vignette + pkgdown → M34.

## Acceptance criteria

- [ ] `devel/m30-coord-spec.md` exists and specifies the chosen mechanism, how
      it owns `amax` (single source of truth), the configurable-center design
      (with the R3 mislabel bug it avoids), and the theme-responsiveness path.
- [ ] The spec states the back-compat contract: the existing geoms,
      `ggcircumplex()`, and `ssm_plot_*` keep correct output; `ssm_plot_*` and
      `ggcircumplex()` are retained as convenience wrappers (D-018b); migration
      path named.
- [ ] The spec argues preservation of the angle invariants through the new
      polar transform (0°/360° peak, seam-straddling CI, LM=360, flat profile)
      and names the specific boundary tests M31 must carry.
- [ ] Fable review completed (RB→RR under `cairn/reviews/`), verdict recorded;
      spec revised per RR or each rejection accepted with rationale; a GO/NO-GO
      decision + the ratified design recorded as a D-entry (M31 inherits it, or
      M31 is retired on NO-GO — the D-009 pattern).

## Coverage

- AC1 → T1
- AC2 → T1
- AC3 → T1, T2
- AC4 → T2, T3

## Tasks

- [x] **T1** — Draft `devel/m30-coord-spec.md`: survey ggplot2 `Coord`/scale
      extension mechanics for a fixed-aspect polar canvas; specify `amax`
      ownership, configurable center, theme path, back-compat contract, and the
      invariant-preservation argument with the M31 boundary-test list.
      *(RB tripwire: irreversible-api, ip-touching)*
- [ ] **T2** — Escalate the spec to Fable via `/milestone-brief` (RB); ingest
      the RR; revise the spec (or accept rejections with rationale).
- [ ] **T3** — Record the GO/NO-GO + ratified design as a D-entry; on GO, M31
      builds it; on NO-GO, M31 is retired and the trade-offs stay DESIGN.md
      known-limitations.

## Work log

- 2026-07-17: created by /milestone-plan (viz expansion, area A design half).
- 2026-07-17 (T1): drafted `devel/m30-coord-spec.md`. Recommends Option A —
  subclass `CoordRadial` as `coord_circumplex()` so `amax`/center become r-scale
  limits (R1/R2 structural) and the arc becomes a coord-bent `GeomRect`; Option B
  (bespoke base-`Coord`, no floor bump) as fallback. Central Fable question:
  A-vs-B / whether coord_radial's guide API expresses the circumplex labels.
  Option A re-pins ggplot2 >= 3.5.0 (contingent floor gate + D-entry at T3).

## Decisions

## Review
