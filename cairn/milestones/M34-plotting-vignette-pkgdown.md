<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M34: Plotting vignette + pkgdown reference

- **Status:** review
- **Priority:** normal
- **Depends on:** M31, M32, M33
- **Principles touched:** —
- **Branch/PR:** `m34-plotting-vignette-pkgdown`

## Goal

Document the improved visualization surface: refresh/extend the plotting-focused
vignette to teach the coordinate system, the composable layers, and trajectory
plots, and organize the pkgdown reference so the plotting API is discoverable.

## Scope

**In:**
- Extend `vignettes/advanced-visualization.Rmd` (and/or add a focused vignette)
  to cover: the coordinate system and configurable center (M31), subclassing the
  exported geoms and the new styling options (M32), and occasions/growth
  trajectory plots (M33) — each with a runnable example.
- Reorganize the pkgdown reference (`_pkgdown.yml`) so the visualization
  functions group coherently (canvas/coord, composable layers, high-level
  `ssm_plot_*` wrappers, trajectory).
- Keep vignette prose statistically precise (CLAUDE.md: never describe an
  angular CI excluding 0° as a significance test).

**Out:**
- Any new plotting capability (all shipped by M31/M32/M33); this is docs-only
  over the merged API.

## Acceptance criteria

- [x] The vignette teaches the coordinate system + configurable center, geom
      subclassing + new styling options, and trajectory plots, each with a
      runnable chunk; it builds under `devtools::check()` (the authoritative
      vignette build — [LESSONS.md M21](../LESSONS.md)).
- [x] `_pkgdown.yml` groups the visualization functions coherently; `pkgdown::
      check_pkgdown()` (or build) reports every exported plotting function
      referenced (no orphaned topics).
- [x] Prose reviewed for statistical precision (no CI-as-significance-test
      phrasing); rendered figures reflect the actual current output (re-run the
      chunks, don't edit narrative by guess — [LESSONS.md M16](../LESSONS.md)).
- [x] `devtools::check()` clean (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T1, T3
- AC4 → T3

## Tasks

- [x] **T1** — Extend/refresh the plotting vignette with coord + center, geom
      subclassing + styling, and trajectory sections, each with a runnable
      example rendered from actual output.
- [x] **T2** — Reorganize `_pkgdown.yml` visualization reference groups; run
      `pkgdown::check_pkgdown()` for orphaned/missing topics.
- [x] **T3** — Statistical-precision prose pass; full `check()`.

## Work log

- 2026-07-17: created by /milestone-plan (viz expansion, area D).
- 2026-07-18: gate — single rewritten vignette; occasions trajectory path only (growth vignette keeps the table path); two pkgdown viz groups; M35 legend-glyph defect stays a candidate (no scope amendment).
- 2026-07-18: T1 done — vignette rewritten (stale per-layer `amax` teaching removed; coord/center/r_axis_angle, theming, geom subclassing, occasions trajectory added). Render-and-inspect fixed an illegible center demo and two "Coordinate system already present" messages. Suite 2886 pass.
- 2026-07-18: T2 done — `_pkgdown.yml` split into "Visualization - Complete Plots" (the four `ssm_plot_*`, moved out of Primary SSM Functions) and "Visualization - Building Blocks"; added `@family visualization functions` to the plot trio and a new `@family circumplex layers` across the six building blocks, replacing their incomplete hand-kept `@seealso` lists. `check_pkgdown()`: no problems found.
- 2026-07-18: T3 done — precision pass added the unwrap's unverifiable half-turn assumption, the bands-are-pointwise-not-simultaneous caveat, and the vector-averaging reading of a short group amplitude (printed live, not asserted in prose); rescaled the individuals figure (amax 3 -> 1.75) so the shrinkage it describes is actually visible. NEWS entry added. `devtools::check()`: 0 errors / 0 warnings / 0 notes.

- 2026-07-18: review — three-lens fan-out returned 6 findings; F1/F2/F3 (>=80) actioned, F5/F4/F6 (<80) logged below. F1 and F2 were real content defects that `check()` could not see.
- 2026-07-18: all tasks done; `devtools::check()` re-run on the final tree (incl. NEWS): 0 errors / 0 warnings / 0 notes. Status -> review.

## Decisions

## Review

**PR:** https://github.com/jmgirard/circumplex/pull/59 (2026-07-18)

### Acceptance criteria — fresh evidence

- **AC1 (vignette teaches the three topics, runnable chunks, builds under `check()`).**
  Coordinate system + center: sections "The coordinate system" (chunk `coord-bare`),
  "Moving the center" (`coord-center`), "Moving the amplitude axis" (`coord-r-axis`).
  Geom subclassing + styling: "Extending the layers" (`subclass`, a working
  `GeomSsmStar` subclass of `GeomSsmPoint`), "Restyling the canvas" (`theming`).
  Trajectory: "Trajectories across occasions" (`occasions-data`, `occasions-plot`).
  14 code chunks total, all executing. `devtools::check()` reports
  "re-building of vignette outputs ... OK".
- **AC2 (`_pkgdown.yml` grouping; no orphaned topics).** `pkgdown::check_pkgdown()`:
  "No problems found". Audited against the dev tree (`load_all()`, not the installed
  package — LESSONS M21): all 10 user-facing plotting exports are listed, none
  duplicated across groups. The 3 unlisted exports (`CoordCircumplex`, `GeomSsmArc`,
  `GeomSsmPoint`) are documented on the `@keywords internal` `circumplex-ggproto`
  page and are index-exempt by design (LESSONS M32).
- **AC3 (statistical precision; figures from actual output).** Grep for
  significance-test phrasing finds only the two intentional disclaimers (the arc
  paragraph and the hollow-point rule). Every figure was rendered and visually
  inspected; the render pass caught three defects `check()` passed silently (an
  illegible `center` demo, two leaked "Coordinate system already present" messages,
  and an `amax` that hid the shrinkage the prose described). Numeric claims are
  printed live from the chunk, not asserted in prose. The occasion-ordering claim
  was corrected after review verified it empirically (see F2).
- **AC4 (`check()` clean).** 0 errors / 0 warnings / 0 notes, re-run after the
  review fixes on the final tree.

### Consistency gate

`cairn_validate.py`: exit 0, all 15 checks pass. No DESIGN principle changed, so
`cairn_impact` was skipped. Toolchain slot (`r-package`): `devtools::document()`
produces no diff; README.md in sync; `pkgdown::check_pkgdown()` clean; NEWS.md
entry present; no new top-level files; `check()` clean.

CI on PR #59: all 9 checks green (R CMD check on macOS, Windows, Ubuntu devel /
release / oldrel-1; pkgdown; test-coverage; both codecov gates). An earlier run
failed `ubuntu-latest (devel)` in `setup-r-dependencies` — a transient upstream
outage ("cannot open URL ... pak_0.11.0_R-4-7"), which passed on re-run,
confirming it was never related to this diff.

### Independent review — three lenses + scorer

Diff-bug **[O]**, blame-history **[S]**, prior-PR-comments **[S]**, then a
confidence scorer **[S]**. The prior-PR lens was a clean no-op (this repo has zero
inline PR comments; LESSONS M33 records that its silence carries no evidential
weight here).

**Actioned (score >= 80):**

- **F1 (98) — fixed.** Leaked tool-call scaffolding (`</content>`, `</invoke>`)
  committed at the end of the vignette, introduced by this branch. Invisible to
  `check()` because pandoc passes unmatched closing tags through as raw HTML and
  browsers drop them; would have shipped to CRAN in the vignette source. Found
  independently by two lenses.
- **F2 (90) — fixed.** The vignette claimed occasions "appear in the order they
  were supplied, never in alphabetical order, so a `T2`/`T10` pair cannot silently
  swap." False for a factor occasion column: `ssm_analyze_long()` uses
  `levels(droplevels())` for factors, and `factor()` sorts levels alphabetically.
  Verified empirically — a default factor yields `T1, T10, T2`, exactly the swap
  the prose called impossible. Rewritten to state first-appearance order for
  character, level order for factor, with the alphabetical-default warning.
- **F3 (80) — fixed.** The NEWS entry claimed trajectory and the composable layers
  are "reachable from each other's help pages"; the two families are disjoint and
  no such direct link exists. Reworded to claim only the within-family links that
  are real.

**Below threshold (logged, not silently dropped):**

- **F5 (78) — fixed anyway.** "Everything the parent geom does --- the polar
  placement ..." misattributed the polar transform to the geom, contradicting the
  vignette's own opening bullet list; `GeomSsmPoint$setup_data` only assigns
  `x`/`y` and the coord owns the transform. Verified correct and one line to fix
  in a file already being edited.
- **F4 (72) — fixed anyway.** `angles <- octants()` had been hoisted out of the
  `curve-axis` chunk into the trajectory simulation two sections earlier, so
  copy-pasting the angle-axis example alone failed. Restored to the chunk.
- **F6 (55) — rejected.** `plot.circumplex_ci_accuracy` carries
  `@family visualization functions` but sits in the pkgdown "Structure Evaluation
  Functions" group. Both the tag and the grouping predate this diff, which only
  made the mismatch more visible; a simulation diagnostic also belongs beside its
  analysis function. Left as-is.
