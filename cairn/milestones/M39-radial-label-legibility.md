# M39: Legible radial axis labels over data layers

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** `m39-radial-label-legibility` / [PR #65](https://github.com/jmgirard/circumplex/pull/65)

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

**Merge gate (amended 2026-07-19 — the original clause is inverted):** M39's
change is documented in **v2.0.0's** NEWS at Jeff's gate choice, which claims it
as part of that release. So M39 must merge **before** `submit_cran()` runs, not
after, or the submitted tarball would omit the code its own NEWS describes. The
tarball is not yet submitted and M7 stays `blocked` in the meantime, so there is
no conflict to resolve — the ordering is simply M39 merges, then M7 unblocks and
ships. Review confirms M39 is merged ahead of the release handoff.

## Acceptance criteria

- [x] The radial axis tick labels render with a backdrop grob behind them,
      asserted structurally (the property — that a backdrop is emitted, one per
      labelled break, positioned with the labels), not by eyeballing a render.
      M32 established that label overlap itself cannot be fenced at grob level
      because the label grobs are nested unnamed gTrees; fence what is
      constructible instead.
- [x] A new vdiffr baseline covers a radial label falling over a dark mark —
      a case the existing 14 canvas baselines do not contain. Verified to have
      teeth: it must differ from what the pre-change code renders.
- [x] Existing radial-axis fences still pass unchanged:
      `test-coord_circumplex.R`'s `ssm_r_axis_angle()` unit tests and the two
      `r_axis_inside` placement tests (`:218-253`), and the
      `test-ggproto-classes.R:53` field-copy fence.
- [x] The three reported figures re-rendered and read legibly; any baseline
      that moved is accounted for as intended (M38: a clean vdiffr run is
      evidence only after checking the baselines exercise the changed path).
- [x] `devtools::document()` no diff; `devtools::test()` clean;
      `devtools::check(manual = TRUE)` 0 errors / 0 warnings / 0 notes, with
      `checking PDF version of manual ... OK` and
      `checking re-building of vignette outputs ... OK` confirmed present in
      the log by name (M7: never read `Status: OK` as coverage).
- [x] `NEWS.md` carries a user-visible entry; if anything new is exported, a
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
- [x] **T6** — `document()`, `test()`, `check(manual = TRUE)`; NEWS entry;
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

- 2026-07-19: T6 done. `devtools::check(manual = TRUE)`: **Status OK, 0 errors / 0 warnings / 0 notes** (5m13s), with both steps this repo has learned to verify by name rather than infer from the summary line confirmed present in the log — `checking PDF version of manual ... OK` (the class win-builder caught in M7, invisible under `--no-manual`) and `checking re-building of vignette outputs ... OK` (38s, exercising the knit that `devtools::test()` never touches). `document()` no diff; `pkgdown::check_pkgdown()` no problems; no `_pkgdown.yml` row needed since nothing new is exported (both new functions are internal). NEWS entry added to the **v2.0.0 Visualization section** at Jeff's gate choice, which carries a consequence recorded rather than absorbed silently: documenting the change in that release means the submitted tarball must contain it, so M39's merge gate is **inverted** — M39 merges before `submit_cran()`, not after. Jeff confirmed the tarball is unsubmitted and M7 stays `blocked` meanwhile, so the sequence is simply M39 merges, then M7 unblocks and ships; the ordering is cross-referenced in M7's work log so a session reading it alone still sees the predecessor.

- 2026-07-19: status in-progress→review (/milestone-implement). All six tasks done; branch is 2 commits over 22 files. `check(manual = TRUE)` 0/0/0 on the tip. Acceptance-criterion boxes deliberately left unticked for `/milestone-review` to tick against fresh evidence (AC fencing). **One thing review should weigh rather than take on trust:** the T2 fence as first written passed an implementation that rendered visibly wrong (plates unrotated, sliding off their labels), so the structural assertions alone were not sufficient evidence — the rotation, per-label anchor, and font-inheritance assertions were added afterwards specifically to close that gap, and the render-and-inspect evidence in T5 is doing real work here, not decorating it.

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

- 2026-07-19: review — four findings from three lenses, all scored >= 80, all fixed on the branch and re-verified fresh (not by trusting the reviewers' own reproductions). **F1** (grid without a D-entry): D-022 written. **F2** (spoke labels plated when they read like amplitudes): plating now requires the grob be inside the guide's own `axis` gtable AND match the labels — reproduction went 2 backdrop groups -> 1. **F3** (plotmath sized to deparsed source): measured with `grobWidth`/`grobHeight` on the label grob — plotmath widths now glyph-scale (7.57/18.53/9.07/6.36/5.88pt vs the scorer's 54.5pt-for-7.1pt), byte-identical for plain labels so all 17 baselines still match. **F4** (fence missed plate extent/offset, caught only by skip_on_ci vdiffr): three structural regression tests added — fixed-size plate now fails 15, dropped re-centring fails 10, previously zero each. Post-fix: full `devtools::test()` clean (0 failures, 4 pre-existing Hessian warnings), `document()` no diff, `check(manual = TRUE)` 0/0/0 with both named steps present, `cairn_validate` 15/15, PR #65 CI 9/9 green. AC1-AC6 boxes ticked against this evidence.

## Review

Reviewed 2026-07-19. PR [#65](https://github.com/jmgirard/circumplex/pull/65).

### Acceptance-criterion evidence

- **AC1 — backdrop rendered, asserted structurally.** `test-coord_circumplex.R`'s four M39 T2 tests pass with 51 assertions: a backdrop gTree exists carrying one `rect` per non-blank label, each plate rotated to the label's own `rot`, anchored at its own label rather than all at one point, inheriting the label font, and filled `#FFFFFF` at less than full opacity with `col = NA`. Fenced by mutation rather than inspection — suppressing the backdrop fails 6, dropping the rotation fails 10.
- **AC2 — collision baseline with teeth.** New vdiffr baseline `amplitude-labels-over-dark-marks` puts heavy dark markers and an arrowhead where the labels fall; no pre-existing canvas baseline drew labels over anything. Teeth re-proven **fresh at review**, not inherited: stubbing `label_backdrop()` to `NULL` fails it, and dropping the plate rotation fails it.
- **AC3 — pre-existing fences unchanged.** All fences the criterion names by path pass untouched: `ssm_r_axis_angle()` unit tests (9 assertions), both `r_axis_inside` placement tests (2 and 4), and `test-ggproto-classes.R`'s field-copy fence (7). Whole file: 0 failed, 0 error.
- **AC4 — figures legible, baselines accounted for.** All three reported figures re-rendered and read: `0.6` over the arrowhead, `0.50` over the marker (pixel behind the final glyph sampled `srgb(210,210,210)` — exactly 75% white over the dark arrow, so the plate composites as designed), `0.0`/`0.5` over the scatter. 17 canvas baselines moved, 12 did not, and the split is exactly the canvas/non-canvas boundary — every unmoved one is a curve, contrast-panel, trajectory, or ladder plot with no radial axis, verified by reading each test's plotting call rather than inferring from its name. The history lens independently diffed all 17 line-by-line: **pure additions of `<rect>` elements, zero removed lines**, nothing else moved.
- **AC5 — checks clean.** `devtools::check(manual = TRUE)`: **Status OK, 0/0/0** (5m30s), with `checking PDF version of manual ... OK` and `checking re-building of vignette outputs ... OK` confirmed present by name, never inferred from the summary line (M7's lesson). `document()` no diff. Full suite clean.
- **AC6 — NEWS + pkgdown.** NEWS entry present in the v2.0.0 Visualization section; `check_pkgdown()` no problems; no `_pkgdown.yml` row needed since nothing is exported. No milestone numbers leak into user-facing text (grep over NEWS, README, cran-comments).

### Consistency gate

`cairn_validate` exit 0, all 15 checks PASS. 47 advisory `work-log format` warnings, every one on a hard-wrapped pre-M39 entry in M7's log — history, advisory by design, untouched. No principle changed (`Principles touched: —`), so the impact report is a clean skip. Toolchain slot: `document()` no diff; `NAMESPACE`/`man/` clean; `check_pkgdown()` passes; one added file, a snapshot under `tests/`, needing no `.Rbuildignore` entry; full check clean. PR #65 CI: **9/9 green** across macOS/Windows/Ubuntu (release, devel, oldrel-1), pkgdown, and both codecov gates.

### Independent review — three lenses

- **[O] diff-bug (Opus):** 3 findings plus one unnumbered note. Verified the shift formula, traversal safety, and matching robustness empirically (32 `center`x`amax` combinations; zero label ink off-plate across nine `r_axis_angle` values) before reporting.
- **[S] blame-history (Sonnet):** 1 finding. No silent undoing: `render_fg` is purely additive (zero `-` lines in `R/coord_circumplex.R`), M38-D1's blank rim label is respected and fenced, and M32's "match by content, not index" lesson is honoured.
- **[S] prior-PR-comments (Sonnet):** no prior-PR evidence — all merged PRs touching these files carry zero inline review comments. Clean no-op, zero findings, as LESSONS records is permanent for this repo.

### Findings actioned (all four scored >= 80)

**F1 (90) — `grid` added to Imports with no D-entry.** `DESCRIPTION` gained `grid`, but `cairn/DECISIONS.md` was untouched by the branch and its only `grid` string is inside the word "r-gridlines". The tracking rules require dependency changes to take a question gate **and** a D-entry; the gate was held at T3 but the entry was never written, and a work-log narration is not the required artifact. Every prior dependency change has one, including D-021, which was argued into an entry despite the constraint already being transitively true.
**Fixed:** D-022 written, recording `grid` as base R, already transitively loaded via ggplot2, adding no install burden and unable to raise the R floor — and noting that `grDevices` was deliberately NOT taken for a colour constant.

**F2 (85) — spoke labels could be plated, which Scope puts explicitly Out.** `add_label_backdrop()` matched on label-vector equality alone and did not stop at the first hit, so any foreground text grob whose labels equalled the radial labels got plates. Reproduced by both the reviewer and the scorer: `scale_x_continuous(labels = c("0.0", ..., "0.8"))` produced **two** backdrop groups, one behind the spoke labels. The code comment claimed the design fails visibly "rather than silently styling the wrong text" — here it silently styled the wrong text.
**Fixed:** plating now requires **both** that the grob sits inside the guide's own `axis` gtable **and** that its labels match. Short-circuiting on first match would have been wrong — the theta guide is traversed first, so it would have selected the wrong grob. **Verified against the reproduction: 2 backdrop groups -> 1.**

**F3 (88) — plotmath labels got plates sized to deparsed source text.** `as.character(txt$label)` deparses an expression, so `stringWidth()` measured the string `"gamma^2"` rather than the single rendered glyph. Reviewer measured ~4x oversize; the scorer independently measured **54.5pt of plate for a 7.1pt glyph**, and the base glyph of `gamma^2` fell outside its own plate vertically. Introduced by this diff — before it there was no plate.
**Fixed:** the label is kept in its original form and measured with `grobWidth()`/`grobHeight()` on a text grob built from the label itself, which measures what the device draws for plain strings and expressions alike. **Verified: plate widths for the plotmath reproduction are now 7.57 / 18.53 / 9.07 / 6.36 / 5.88 pt** — glyph-scale, with the subscript and superscript cases correctly wider. Byte-identical for plain labels (all 17 baselines still match).

**F4 (88) — the fence constrained rotation and anchor but not plate extent or padding offset.** Review mutation-verified that a plate hardcoded to 30x30pt, and one with the `pad * (2 * just - 1)` re-centring dropped, both **passed every structural test** and failed only the two vdiffr baselines — which both carry `skip_on_ci()`, so on CI those regressions went green. The same gap as the milestone's own headline lesson, one level down.
**Fixed:** three regression tests added asserting the size units derive from a grob measurement (a constant-size plate cannot satisfy them) and that the anchors carry the padding term. **Mutation-verified: fixed-size plate now fails 15 structural assertions, dropped re-centring fails 10** — previously zero each.

### Findings logged, not actioned

- **Unnumbered minor (diff lens, below the bar and not scored) — helper inconsistency.** `label_backdrop()` recycles `x`/`y` with a hand-rolled `recycle()` handling only length-1 and length-n, while `hjust`/`vjust`/`rot` use `rep_len()`. A text grob with, say, 3 x-values for 6 labels is legal under grid recycling and would be subset by a length-6 logical. The reviewer could construct no path reaching it and flagged it only because two helpers in one function disagree. Left as-is: no reachable defect, and the ggplot2 axis guide always emits either a scalar or a full-length vector.
