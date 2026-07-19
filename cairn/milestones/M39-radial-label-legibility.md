# M39: Legible radial axis labels over data layers

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** `m39-radial-label-legibility` / —

## Goal

Give the amplitude (radial) axis labels a backdrop in `coord_circumplex()` so
they stay readable where they fall over dark or dense geom layers.

## Scope

**In:**
- A backdrop behind the radial axis tick labels, applied package-side in
  `CoordCircumplex` so every canvas gets it — users' plots included, not only
  the vignette figures.
- Whatever escape hatch that needs (an argument or theme element) if the
  implementation exposes one.
- A vdiffr baseline that actually exercises a label-over-dark-mark collision,
  plus a structural fence on the backdrop.
- Re-render and inspect the three figures the defect was reported in:
  `advanced-visualization.Rmd`'s `occasions-path` (`:408`),
  `occasions-path-wrapper` (`:441`), and `individuals` (`:280`).

**Out:**
- The theta/spoke labels and the M38 rim label. If they need the same
  treatment, that becomes a ROADMAP candidate row — not this milestone.
- Moving the axis angularly. `r_axis_angle` (M32) already does that and it is
  the wrong tool here: it solves label-vs-*spoke-label*, and the data it would
  need to dodge moves with the dataset.
- Any change to break generation, the rim ring, or `ssm_r_axis_angle()`'s
  placement rule — M38 and M32 own those and their fences must stay green.

**Merge gate:** M7 is at T4 with a v2.0.0 CRAN submission pending. This
milestone may be *built* on its branch at any time, but must not merge to
master until that submission is handed off, so the submitted tarball and
master do not diverge mid-review. Review confirms before merging.

## Acceptance criteria

- [ ] The radial axis tick labels render with a backdrop grob behind them,
      asserted structurally (the property — that a backdrop is emitted, one per
      labelled break, positioned with the labels), not by eyeballing a render.
      M32 established that label overlap itself cannot be fenced at grob level
      because the label grobs are nested unnamed gTrees; fence what is
      constructible instead.
- [ ] A new vdiffr baseline covers a radial label falling over a dark mark —
      a case the existing 14 canvas baselines do not contain. Verified to have
      teeth: it must differ from what the pre-change code renders.
- [ ] Existing radial-axis fences still pass unchanged:
      `test-coord_circumplex.R`'s `ssm_r_axis_angle()` unit tests and the two
      `r_axis_inside` placement tests (`:218-253`), and the
      `test-ggproto-classes.R:53` field-copy fence.
- [ ] The three reported figures re-rendered and read legibly; any baseline
      that moved is accounted for as intended (M38: a clean vdiffr run is
      evidence only after checking the baselines exercise the changed path).
- [ ] `devtools::document()` no diff; `devtools::test()` clean;
      `devtools::check(manual = TRUE)` 0 errors / 0 warnings / 0 notes, with
      `checking PDF version of manual ... OK` and
      `checking re-building of vignette outputs ... OK` confirmed present in
      the log by name (M7: never read `Status: OK` as coverage).
- [ ] `NEWS.md` carries a user-visible entry; if anything new is exported, a
      `_pkgdown.yml` row in the same commit.

## Coverage

- AC1 → T2, T3
- AC2 → T4
- AC3 → T3, T4
- AC4 → T5
- AC5 → T6
- AC6 → T6

## Tasks

- [x] **T1** — Choose the backdrop mechanism and record why in the milestone's
      Decisions section. Two candidates are known: (a) post-process the grob
      tree returned by `CoordRadial$render_fg` to wrap the label grobs — direct,
      but walks the nested unnamed gTrees M32 found fragile; (b) compute the
      label positions ourselves from `params$r.major` and
      `params$axis_rotation`, both already available in `setup_panel_params`
      ([coord_circumplex.R:202-215](R/coord_circumplex.R:202)), and draw the
      backdrops in a `render_fg` override before delegating — avoids grob
      spelunking entirely. Prefer (b) unless it proves unworkable.
- [x] **T2** — Write the structural fence first (test-first), asserting the
      backdrop property against the chosen mechanism. It must fail against
      current `main`.
- [x] **T3** — Implement the backdrop in `CoordCircumplex`
      ([R/coord_circumplex.R:178-217](R/coord_circumplex.R:178)). Decide whether
      to expose an escape hatch; exposing one is a new CRAN API commitment, so
      it takes the question gate and a D-entry rather than a unilateral call.
      *(RB tripwire: irreversible-api — only if an argument is exported.)*
- [x] **T4** — Add the collision vdiffr baseline; prove it has teeth by running
      it against the pre-change renderer (M38's lesson: baselines that never
      exercise the changed path report green regardless).
- [x] **T5** — Re-render `occasions-path`, `occasions-path-wrapper`, and
      `individuals`; inspect each and record the result. Regenerate any canvas
      baselines that legitimately moved, and state which and why.
- [ ] **T6** — `document()`, `test()`, `check(manual = TRUE)`; NEWS entry;
      `_pkgdown.yml` row if anything was exported.

## Work log

- 2026-07-19: created by /milestone-plan. Promoted from the ROADMAP candidate added 2026-07-18 out of M38's T6 render-and-inspect sweep. **The candidate row's diagnosis was wrong and was corrected at this gate:** it recorded the amplitude labels as "panel furniture drawn beneath the geom layers" and offered "drawing the radial guide above the layers" as a remedy. They are already drawn above. Three confirmations, on all three reported figures: the panel's children print in draw order with the axis gTree **last** (`grill` → geom layers → axis gTree); `CoordCircumplex$setup_panel_params` always assigns a *numeric* `r_axis_inside` (`R/coord_circumplex.R:202-207`), which defeats the `isFALSE(self$r_axis_inside)` early return in ggplot2 4.0.3's `CoordRadial$render_fg` and routes the r-axis guides into the foreground, while `render_bg` draws only `guide_grid()`; and recolouring `axis.text.y` red renders every reported label fully legible **on top of** the mark that had hidden it. The defect is contrast — grey30 text over dark arrowheads/markers (`occasions-path`, `occasions-path-wrapper`) and over a pale busy scatter (`individuals`) — so a backdrop is the only live remedy of the two the row proposed. Jeff accepted the correction and chose the package-level backdrop over a vignette-only `r_axis_angle` retune, radial labels only, and the property-fence-plus-new-baseline acceptance bar.
- 2026-07-19: sequencing recorded rather than expressed as a dependency. M39 is technically independent of M7, so `Depends on:` stays `—` and the milestone is buildable now; the constraint is on *merge* only, while M7's v2.0.0 submission is in flight (see Scope → Merge gate).

- 2026-07-19: started (/milestone-implement). Branch `m39-radial-label-legibility` cut from master at `894207a5`. Status planned→in-progress; the slot was freed by parking M7 as `blocked` on its external `submit_cran()` handoff (Jeff's gate choice) rather than by overriding the `at most one in-progress` check.

- 2026-07-19: T1 done. Mechanism chosen and gated (see M39-D1): wrap the located label text grobs rather than compute label positions. Investigation that produced the reversal — the parent's foreground tree is `theta guide / zeroGrob / zeroGrob / r-axis gTree / border`, with the r-axis text nested `absoluteGrob → gtable "axis" → titleGrob → text`; `CoordRadial` positions it through the unexported `rotate_r_axis()`, so matching it needs `ggplot2:::` (a CRAN problem and a private-API dependency) or re-derived arithmetic that drifts silently. Also confirmed there is no theme route: `element_text()` in ggplot2 4.0.3 has no `fill`, and ggplot2 ships no text-with-background element. Jeff's gate answers: mechanism (a′), **no exported argument** (so the plan's `(RB tripwire: irreversible-api)` does not fire and no escalation was needed), semi-transparent white.
- 2026-07-19: T2 done. Fence added to `test-coord_circumplex.R`, written before the implementation and confirmed to fail against it (10 failures + 1 error with no backdrop present).
- 2026-07-19: T3 done, but **the first implementation was wrong in a way the T2 fence passed**, and that is the entry's point. The plates were given the labels' x/y and drawn as one vectorized `rectGrob`, on the assumption that a sibling of the text grob inherits its placement. It does not: the labels carry `rot = 67.5`, because the radial axis sits at an angle and each label is turned about its own anchor to stay readable, and `rectGrob()` has no rotation. The plates rendered axis-aligned and slid off their labels — **every structural assertion still passed**, and only rendering the canvas with the plates coloured bright red exposed it (the M33/M38 render-and-inspect lesson, and M36's "it works is not it is the mechanism"). Fixed by giving each label its own plate in a `viewport(x, y, angle = rot)`: a viewport rotates about its own centre, so centring it on the label's anchor reproduces exactly what `textGrob()` does. The fence was then **strengthened to catch the bug it had missed** — it now asserts the per-plate rotation equals the text's `rot`, that each plate is anchored at its own label rather than all at one point, and that the label font is inherited so `stringWidth()` measures the text as drawn. Mutation-verified across five mutations: rotation dropped **10 failures** (the bug that previously passed silently), shared anchor 1, font not inherited 5, opaque plate 5, backdrop suppressed 6.
- 2026-07-19: T3 — two portability defects caught before commit, neither visible in a passing `load_all()` test run. `%||%` only reached base R in 4.4 while this package declares R (>= 4.1) (D-021) and rlang's is not imported, so it was replaced with a local helper; and `grDevices::adjustcolor()` would have added an undeclared second dependency, so the fill is written as the literal `#FFFFFFBF` rather than gaining one for a constant (D-006/D-014 minimal deps). `grid` **is** now in Imports — base R, ships with every install, already a ggplot2 dependency — user-approved at the T3 gate. `document()` produces no `man/`/`NAMESPACE` diff (both new functions are internal); the `cpm_gradient` link warning it emits was verified pre-existing on clean master.

- 2026-07-19: T4 done. New baseline `amplitude-labels-over-dark-marks` puts heavy dark markers and a large arrowhead exactly where the amplitude labels fall, which no existing canvas baseline did — every one of them draws its labels over empty panel, so none could have seen a contrast defect (the M38 lesson, applied rather than rediscovered). **Teeth proven by mutation, not assumed:** with `label_backdrop()` stubbed to `NULL` the baseline fails; the artifact that mutated run wrote was deleted rather than accepted.
- 2026-07-19: T5 done. **17 canvas baselines legitimately moved and were accepted; 12 did not, and the split is exactly the canvas/non-canvas boundary** — everything unmoved is a curve plot, a contrast panel plot, a trajectory plot, or the ladder plot, none of which draw a radial axis (verified by reading each test's plotting call, e.g. `single group mean ssm with labels` is `ssm_plot_curve()` and `group-constrast correlation ssm` is `ssm_plot_contrast()`, not circle plots as their names suggest). All three reported vignette figures re-rendered and inspected: `0.6` now reads over the arrowhead in `occasions-path`, `0.50` over the marker in `occasions-path-wrapper` (sampled `srgb(210,210,210)` behind the final glyph — exactly 75% white over the dark arrow, so the plate is compositing as designed), and `0.0`/`0.5` over the scatter in `individuals`. Full `devtools::test()` clean afterwards: 0 failures, the only warnings the 4 pre-existing `test-ci_accuracy.R` Hessian diagnostics. Stray PNGs the render scripts had dropped in the repo root were removed and the scripts repointed at the scratchpad (the M31 `Rplots.pdf` failure class).

## Decisions

### M39-D1 (2026-07-19): the label backdrop wraps the label grobs; it does not compute label positions

**Context:** the amplitude labels are illegible where they fall over dark or
dense layers. They are already drawn above the data (the radial axis is a
foreground guide), so this is a contrast problem and the remedy is a plate
behind each label. Two ways to place that plate: compute the positions from
`params$r.major` + `params$axis_rotation`, or derive them from the label grobs.
**Decision:** derive them from the grobs. `CoordRadial` places the radial axis
through an unexported `rotate_r_axis()`; matching it means either `ggplot2:::`
— a CRAN check problem and a dependency on private API that can break on any
ggplot2 release — or re-deriving arithmetic that drifts silently and leaves the
plate beside the label rather than behind it. Sizing each plate with
`stringWidth()`/`stringHeight()` and anchoring it at the label's own
x/y/just/rot hands the geometry to grid at draw time, so it is exact by
construction.
**Consequences:** the residual risk moves from silent misplacement to failure
to *find* the label subtree, which draws no plate at all — visible, and fenced.
The labels are located by matching the radial view scale's label set rather than
by index, since M32 established these grobs are nested in unnamed gTrees and the
theta labels sit in a sibling subtree that M39 leaves alone. **No argument is
exported**, so nothing enters the CRAN API surface and adding a control later
stays purely additive; the plan's `(RB tripwire: irreversible-api)` therefore
does not fire. Appearance is white at 75% alpha with 1pt padding — legible
without erasing the data beneath. Jeff approved mechanism, API shape, and
appearance at the T3 gate.

## Review
