# M39: Legible radial axis labels over data layers

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** —

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

- [ ] **T1** — Choose the backdrop mechanism and record why in the milestone's
      Decisions section. Two candidates are known: (a) post-process the grob
      tree returned by `CoordRadial$render_fg` to wrap the label grobs — direct,
      but walks the nested unnamed gTrees M32 found fragile; (b) compute the
      label positions ourselves from `params$r.major` and
      `params$axis_rotation`, both already available in `setup_panel_params`
      ([coord_circumplex.R:202-215](R/coord_circumplex.R:202)), and draw the
      backdrops in a `render_fg` override before delegating — avoids grob
      spelunking entirely. Prefer (b) unless it proves unworkable.
- [ ] **T2** — Write the structural fence first (test-first), asserting the
      backdrop property against the chosen mechanism. It must fail against
      current `main`.
- [ ] **T3** — Implement the backdrop in `CoordCircumplex`
      ([R/coord_circumplex.R:178-217](R/coord_circumplex.R:178)). Decide whether
      to expose an escape hatch; exposing one is a new CRAN API commitment, so
      it takes the question gate and a D-entry rather than a unilateral call.
      *(RB tripwire: irreversible-api — only if an argument is exported.)*
- [ ] **T4** — Add the collision vdiffr baseline; prove it has teeth by running
      it against the pre-change renderer (M38's lesson: baselines that never
      exercise the changed path report green regardless).
- [ ] **T5** — Re-render `occasions-path`, `occasions-path-wrapper`, and
      `individuals`; inspect each and record the result. Regenerate any canvas
      baselines that legitimately moved, and state which and why.
- [ ] **T6** — `document()`, `test()`, `check(manual = TRUE)`; NEWS entry;
      `_pkgdown.yml` row if anything was exported.

## Work log

- 2026-07-19: created by /milestone-plan. Promoted from the ROADMAP candidate added 2026-07-18 out of M38's T6 render-and-inspect sweep. **The candidate row's diagnosis was wrong and was corrected at this gate:** it recorded the amplitude labels as "panel furniture drawn beneath the geom layers" and offered "drawing the radial guide above the layers" as a remedy. They are already drawn above. Three confirmations, on all three reported figures: the panel's children print in draw order with the axis gTree **last** (`grill` → geom layers → axis gTree); `CoordCircumplex$setup_panel_params` always assigns a *numeric* `r_axis_inside` (`R/coord_circumplex.R:202-207`), which defeats the `isFALSE(self$r_axis_inside)` early return in ggplot2 4.0.3's `CoordRadial$render_fg` and routes the r-axis guides into the foreground, while `render_bg` draws only `guide_grid()`; and recolouring `axis.text.y` red renders every reported label fully legible **on top of** the mark that had hidden it. The defect is contrast — grey30 text over dark arrowheads/markers (`occasions-path`, `occasions-path-wrapper`) and over a pale busy scatter (`individuals`) — so a backdrop is the only live remedy of the two the row proposed. Jeff accepted the correction and chose the package-level backdrop over a vignette-only `r_axis_angle` retune, radial labels only, and the property-fence-plus-new-baseline acceptance bar.
- 2026-07-19: sequencing recorded rather than expressed as a dependency. M39 is technically independent of M7, so `Depends on:` stays `—` and the milestone is buildable now; the constraint is on *merge* only, while M7's v2.0.0 submission is in flight (see Scope → Merge gate).

## Decisions

## Review
