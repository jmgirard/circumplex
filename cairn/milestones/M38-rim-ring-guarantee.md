<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M38: Guaranteed rim ring for the circumplex canvas

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Guarantee that `coord_circumplex()` draws a labeled amplitude ring at `amax`, so
every circumplex canvas closes at its rim instead of trailing off into an open
panel.

## Scope

**In:** force `amax` into the radial breaks so the rim always carries a ring and
its amplitude label; suppress the last generated break when it would crowd that
label; regenerate and inspect the affected vdiffr baselines; rework the
`coord-bare` chunk of `vignettes/advanced-visualization.Rmd` to pair the bare
figure with its corrected version; render and visually inspect every figure in
that vignette.

**Out:**
- An unlabeled rim ring, and a custom rim grob in an overridden `render_fg()` —
  both rejected at the 2026-07-18 plan gate (an unlabeled rim leaves the reader
  no amplitude at the boundary; a custom grob is materially more code for the
  same visual, since `panel.border` renders as a rect in polar coordinates and
  cannot be reused).
- The floating-point censor defect where a break landing *on* the rim was
  dropped — already fixed by the 2026-07-18 hotfix (PR #62); this milestone
  builds on that headroom rather than replacing it.
- Rendering and inspecting the figures in the other six vignettes — offered and
  declined at the plan gate; no row.

## Acceptance criteria

- [ ] For every finite `amax > center`, the built panel's radial breaks include
      `amax` and it carries an amplitude label — verified across a case table
      covering `amax` already a break (0.3, 0.5, 0.8, 1.2), `amax` short of the
      top generated break (0.7, 1.75, 2.4), a nonzero `center`, and a trained
      (`amax = NULL`) canvas.
- [ ] Where the last generated break crowds the rim (gap below the calibrated
      fraction of the break spacing), it is suppressed and the rim label stands
      alone; where it does not crowd, both rings are drawn. Tests pin behavior
      on both sides of the calibrated threshold, including the
      `center = 0.15, amax = 0.28` case that motivated the rule.
- [ ] Canvases whose rim was already a break render byte-identically (existing
      vdiffr baselines unchanged); every baseline that does move is inspected
      and shown in the milestone to differ only by the intended rim ring and
      label.
- [ ] Every figure in `vignettes/advanced-visualization.Rmd` is rendered and
      visually inspected, with the inspection recorded; the `coord-bare` chunk
      shows the bare figure alongside the same plot with the scale breaks
      supplied, and the surrounding prose describes that contrast.
- [ ] `devtools::test()` clean, `devtools::document()` no diff, and
      `devtools::check()` 0 errors / 0 warnings (NOTEs justified).

## Coverage

- AC1 → T2, T3
- AC2 → T1, T2, T3
- AC3 → T4
- AC4 → T5, T6
- AC5 → T3, T4, T7

## Tasks

- [ ] T1. Calibrate the crowding threshold: extend the plan-gate probe into a
      case table of `(center, amax)` pairs, choose the suppression fraction of
      the break spacing, and record the table plus the chosen value in this
      file. A threshold at 0.5 sits exactly on the common `amax = 0.7` and
      `1.75` cases and splits them on floating-point luck; ~0.3 separates them
      cleanly. Calibrate the rule to the cases, not the other way round.
- [ ] T2. Test-first in `tests/testthat/test-coord_circumplex.R`: the case table
      from T1 as contract tests, asserting both the break set and the rendered
      labels (`panel_params$r$get_labels()`), plus the trained-`amax` and
      nonzero-`center` cases. Confirm they fail before T3.
- [ ] T3. Implement in `R/coord_circumplex.R` — a `rim_breaks()` helper beside
      `rim_limit()` (added by PR #62), applied in `setup_panel_params()` via the
      established mutate-`self`-before-delegating pattern (M32 lesson). Keep the
      ULP headroom: it still carries the already-a-break cases.
- [ ] T4. Regenerate the vdiffr baselines per the M31 procedure — delete the
      stale `_snaps/<file>/*.svg`, re-run under `NOT_CRAN=true` (a bare
      `Rscript` run silently skips the comparison), and diff each moved SVG to
      confirm only the rim ring and its label were added.
- [ ] T5. Rework the `coord-bare` chunk at `vignettes/advanced-visualization.Rmd`
      :90-108 into the bare figure plus its corrected counterpart, and rewrite
      the prose at :104-108 around that contrast.
- [ ] T6. Render every figure in `advanced-visualization.Rmd` and look at each
      one (M33/M36/M37 lesson: data-level fences and vdiffr baselines both pass
      a figure that reads wrong). Record what was inspected; route anything
      found beyond this scope to a candidate row rather than absorbing it.
- [ ] T7. Update the DESIGN.md visualization section (the coord owns the rim
      ring, not only the radial limits), add the NEWS entry, and run the full
      profile check surface.

## Work log

- 2026-07-18: created by /milestone-plan; absorbs two candidate rows added the same day (guaranteed rim ring; bare-coord vignette figure), both spun out of the PR #62 hotfix. Plan gate: labeled break + collision rule over an unlabeled rim or a custom grob; vignette figure paired rather than silently fixed; inspection sweep scoped to this one vignette; added to M7's v2.0.0 bundle.

## Decisions

## Review
