<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M30: Circumplex coordinate-system design

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Principles touched:** — (works under the CLAUDE.md angle invariants: [0,360), LM=360; IP/GP formalization deferred to /design-interview)
- **Branch/PR:** m30-coord-system-design · [PR #54](https://github.com/jmgirard/circumplex/pull/54)

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

- [x] `devel/m30-coord-spec.md` exists and specifies the chosen mechanism, how
      it owns `amax` (single source of truth), the configurable-center design
      (with the R3 mislabel bug it avoids), and the theme-responsiveness path.
- [x] The spec states the back-compat contract: the existing geoms,
      `ggcircumplex()`, and `ssm_plot_*` keep correct output; `ssm_plot_*` and
      `ggcircumplex()` are retained as convenience wrappers (D-018b); migration
      path named.
- [x] The spec argues preservation of the angle invariants through the new
      polar transform (0°/360° peak, seam-straddling CI, LM=360, flat profile)
      and names the specific boundary tests M31 must carry.
- [x] Fable review completed (RB→RR under `cairn/reviews/`), verdict recorded;
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
- [x] **T2** — Escalate the spec to Fable via `/milestone-brief` (RB); ingest
      the RR; revise the spec (or accept rejections with rationale).
      *(RB08 → RR08 GO Option A; spec revised, §11 punch-list added; pair
      archived.)*
- [x] **T3** — Record the GO/NO-GO + ratified design as a D-entry; on GO, M31
      builds it; on NO-GO, M31 is retired and the trade-offs stay DESIGN.md
      known-limitations. *(GO — D-019; ggplot2 floor >= 4.0.0, user-approved.)*

## Work log

- 2026-07-17: created by /milestone-plan (viz expansion, area A design half).
- 2026-07-17 (T1): drafted `devel/m30-coord-spec.md`. Recommends Option A —
  subclass `CoordRadial` as `coord_circumplex()` so `amax`/center become r-scale
  limits (R1/R2 structural) and the arc becomes a coord-bent `GeomRect`; Option B
  (bespoke base-`Coord`, no floor bump) as fallback. Central Fable question:
  A-vs-B / whether coord_radial's guide API expresses the circumplex labels.
  Option A re-pins ggplot2 >= 3.5.0 (contingent floor gate + D-entry at T3).
- 2026-07-17 (T2): blocked on RB08 — coord-system design escalated to Fable.
- 2026-07-17 (T2): ingested RR08 (GO Option A). Spec revised: floor corrected to
  ggplot2 >= 4.0.0, §11 authoritative punch-list added, RB08/RR08 archived.
  Unblocked → in-progress. T3 (GO + floor-bump D-entry) is the remaining step.
- 2026-07-17 (T3): GO ratified (Jeff, T3 gate) — D-019 records GO Option A +
  ggplot2 floor >= 4.0.0. All tasks done, all ACs met → review.

## Decisions

### RR08 (Fable, 2026-07-17) — coord-system design review, ingested
Verdict **GO (Option A)**: `coord_circumplex()` subclasses `CoordRadial`.
Load-bearing holdings applied to the spec (§11) and inherited by M31:
- **Floor is ggplot2 >= 4.0.0**, not 3.5.0 — `thetalim`/`rlim`/`reverse` /
  numeric `r.axis.inside` are 4.0.0-only (verified vs v3.5.2 source). Dependency
  gate + D-entry = **D-019** (cross-cutting; below).
- **Seam (I2)** unwrap by *extension* (`xmax = xmin + span`, may exceed 360) in
  the arc geom's `setup_data()`; one `GeomRect`, coord wraps periodically. Not
  pre-split (stroked seam borders), not in `transform()` (munching precedes it).
- **Pole (I3)** identical only if the coord hard-pins `expand = FALSE` +
  `thetalim = c(0,360)` (default `expand = TRUE` = 33° gap); range coord-side,
  never scale-limits (censor).
- **Center** = `rlim = c(center, amax)` alone; `inner.radius` defaults 0
  (decoupled — RR08 gap 10).
- **`amax`/`n`** unconditional soft-deprecation, sentinel default, note-once;
  never error (breaks documented examples).
- **Scope M31 missed:** the `repel` branch redesign; two hidden consumers
  `plot.circumplex_cpm` + `plot.circumplex_fit_structure` join R4's keep-working
  set; +5 boundary tests; ggforce likely fully removable after a checklist, its
  own D-entry superseding V6 (not a silent side effect).
Triage: R-1…R-7 **apply** (in §11); R-8/R-9 **consider** (M31-gated: ggforce
removal, ring-label styling); R-10…R-12 **reject** (per Fable's rationale:
erroring on `amax`, pre-splitting the seam, dual 3.5/4.0 support).

## Review

_Reviewed 2026-07-17, PR #54. Docs-only design milestone (no R build surface —
`git diff --name-only master..HEAD` touches only `cairn/`, `devel/`,
`.gitignore`; `devel/` is Rbuildignored)._

**Acceptance-criteria evidence (fresh, by command):**

- **AC1** — `devel/m30-coord-spec.md` present (380 lines); §3–§5/§11 specify the
  mechanism (`coord_circumplex()` subclassing `CoordRadial`), single-owner
  `amax` (`rlim`), configurable center (`rlim = c(center, amax)`, `inner.radius`
  decoupled/0), and the theme-responsive panel-grid path. ✓
- **AC2** — §5/§11 state the R4 keep-working set (existing geoms,
  `ggcircumplex()`, `scale_x_circumplex()`, `ssm_plot_*` as convenience
  wrappers per D-018b, **plus** the two consumers RR08 caught —
  `plot.circumplex_cpm`, `plot.circumplex_fit_structure`) with the
  soft-deprecation migration path. ✓
- **AC3** — §6/§11 trace I1–I4 through the new transform and enumerate the M31
  boundary tests: T-i1b, T-i2, T-i2b, T-i2c, T-i3, T-arc0, T-r1, T-r2. ✓
- **AC4** — RB08 → RR08 archived under `cairn/reviews/archive/`; verdict **GO
  (Option A)** recorded; spec revised per RR08 (§11 punch-list; floor corrected
  to 4.0.0); GO ratified as **D-019**. ✓

**Consistency gate:**
- Universal cairn-file checks: `cairn_validate` exit 0 (mirror agreement,
  coverage complete, weight caps, all checks pass). `cairn_impact` skipped — no
  IP/GP principle changed.
- Toolchain (`r-package` consistency-gate slot): **N/A** — the diff touches no R
  build surface (no `R/`, `src/`, `man/`, `DESCRIPTION`, `NAMESPACE`, `NEWS`,
  `vignettes/`, `data/`), so `document()`/`check()`/pkgdown/NEWS checks have no
  M30 change to test. The coord + ggplot2-floor bump (D-019) ship in M31 and
  carry their own NEWS entry + full check there.

**Independent review (proportionate to a docs-only design milestone).** The
design *substance* already received the strongest independent lens — the Fable
review (RR08) — and the diff contains no code to regress. One fresh-context
**[O]** reviewer therefore covered both the diff-bug and history-contradiction
concerns (D-entry supersession correctness, spec↔RR08 fidelity, mirror/
cross-reference consistency, angle-invariant contradictions); the prior-PR lens
no-ops (docs-only tracking diff, no regressed code). This folds the three-lens
fan-out for this one docs-only case — logged deviation, rationale here.

- **Result: no substantive defects.** The reviewer verified D-018/D-019 cite and
  supersede correctly (no minimal-deps violation — re-pin, not new Import; V6
  ggforce deferred to M31's own D-entry, not over-claimed), all seven RR08
  holdings match the spec/D-019 exactly, ROADMAP/M7 mirrors + the M30–M34
  dependency chain are coherent, and every `file:line` cross-reference in the new
  records is accurate against the code. No angle-invariant contradiction.
- No findings to score/triage. No follow-ups spawned.
