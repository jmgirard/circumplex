<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M38: Guaranteed rim ring for the circumplex canvas

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** `m38-rim-ring`

## Goal

Guarantee that `coord_circumplex()` draws a labeled amplitude ring at `amax`, so
every circumplex canvas closes at its rim instead of trailing off into an open
panel.

## Scope

**In:** draw an amplitude ring at `amax` whenever the break algorithm does not
already place one there, leaving the generated breaks and their labels
untouched; regenerate and inspect the affected vdiffr baselines; rework the
`coord-bare` chunk of `vignettes/advanced-visualization.Rmd` to pair the bare
figure with its corrected version; render and visually inspect every figure in
that vignette.

**Out:**
- A labeled rim with neighbour suppression — the plan-gate mechanism, tried at
  T1 and abandoned on render evidence: label collisions persist to a
  gap/spacing ratio of ~0.5 (`1.00`/`1.10` printed as `1.0010`), and a
  threshold high enough to clear them fires on the most common ratio, deleting
  a chosen ring and leaving an uneven ladder. Superseded by M38-D1.
- A custom rim grob in an overridden `render_fg()` — rejected at the plan gate:
  materially more code for the same visual, since `panel.border` renders as a
  rect in polar coordinates and cannot be reused.
- The floating-point censor defect where a break landing *on* the rim was
  dropped — already fixed by the 2026-07-18 hotfix (PR #62); this milestone
  builds on that headroom rather than replacing it.
- Rendering and inspecting the figures in the other six vignettes — offered and
  declined at the plan gate; no row.

## Acceptance criteria

- [ ] For every finite `amax > center`, the built panel draws an amplitude ring
      at the rim — verified across a case table covering `amax` already a break
      (0.3, 0.5, 0.8, 1.2), `amax` short of the top generated break (0.7, 1.1,
      1.75, 2.4), a nonzero `center`, and a trained (`amax = NULL`) canvas.
- [ ] The rim ring carries the break algorithm's label when `amax` is itself a
      generated break, and is unlabeled otherwise. No break the algorithm
      generated is ever removed, so the labeled ladder is unchanged from
      current behavior. Pinned by tests on both sides, including the
      `center = 0.15, amax = 0.28` case that motivated the abandoned
      suppression rule.
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
- AC2 → T1, T2, T3   <!-- T1 fixed the labelling rule AC2 pins -->
- AC3 → T4
- AC4 → T5, T6
- AC5 → T3, T4, T7

## Tasks

- [x] T1. Choose the rim mechanism on render evidence rather than arithmetic:
      render candidate outcomes across gap/spacing ratios and compare a labeled
      rim with neighbour suppression against an unlabeled rim. Record the
      outcome as M38-D1.
- [ ] T2. Test-first in `tests/testthat/test-coord_circumplex.R`: the case table
      as contract tests, asserting the ring is present at the rim
      (`panel_params$r$get_breaks()`) and that its label is blank exactly when
      `amax` is not itself a generated break
      (`panel_params$r$get_labels()`), plus the trained-`amax` and
      nonzero-`center` cases. Confirm they fail before T3.
- [ ] T3. Implement in `R/coord_circumplex.R` — a helper beside `rim_limit()`
      (added by PR #62) that wraps the radial `ViewScale` returned by the
      parent's `setup_panel_params()`, appending the rim to its breaks and
      blanking that entry's label. The grid reads `r$mapped_breaks()`, so the
      ring and the guide both follow from the one patched break set. Keep the
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
- 2026-07-18: T1 done; substantive amendment at the implement gate (Jeff approved) — Scope In, AC1 and AC2 replaced, the labeled-rim-with-suppression mechanism moved to Out, T1/T2/T3 rewritten. Rationale in M38-D1: render evidence, not arithmetic, settled it.
- 2026-07-18: FLAG for the user — the Goal still says the rim ring is "labeled", which M38-D1 contradicts. The Goal is create-only (never edited in place), and the milestone's substance (the canvas closes at its rim) is unchanged, so it is left as written and surfaced here rather than quietly corrected. Decide at review whether to strike the word or re-cut.

## Decisions

### M38-D1 (2026-07-18): the rim ring is unlabeled unless `amax` is already a generated break

The plan gate chose a labeled rim with a rule suppressing the last generated
break when it crowded. T1's render evidence refuted the premise: crowding is
governed by rendered label *width*, not break spacing, so collisions persist to
a gap/spacing ratio of ~0.5 (`amax = 1.1` printed `1.00`/`1.10` as `1.0010`;
`center = 0.15, amax = 0.28` printed `0.2780`). A threshold high enough to clear
them fires at ratio 0.5 — the most common case (`amax` = 0.35, 0.45, 0.55, 0.7,
1.75, 7 …) — deleting a ring the break algorithm chose and leaving a visibly
uneven last step.

**Decision:** append `amax` to the radial breaks with a blank label, and never
remove a generated break. The labeled ladder is exactly what it is today; the
rim adds a ring and nothing else. Where `amax` is itself a generated break (0.3,
0.5, 0.8, 1.2, 2, 3, 5, and every trained-`amax` canvas) it keeps its own label,
so the unlabeled rim appears only where the user chose an unround `amax`
deliberately. Not promoted to `DECISIONS.md`: this is local to how the coord
draws its own furniture.

## Review
