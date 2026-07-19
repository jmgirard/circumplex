<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M38: Guaranteed rim ring for the circumplex canvas

- **Status:** review
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

- [x] For every finite `amax > center`, the built panel draws an amplitude ring
      at the rim — verified across a case table covering `amax` already a break
      (0.3, 0.5, 0.8, 1.2), `amax` short of the top generated break (0.7, 1.1,
      1.75, 2.4), a nonzero `center`, and a trained (`amax = NULL`) canvas.
- [x] The rim ring carries the break algorithm's label when `amax` is itself a
      generated break, and is unlabeled otherwise. No break the algorithm
      generated is ever removed, so the labeled ladder is unchanged from
      current behavior. Pinned by tests on both sides, including the
      `center = 0.15, amax = 0.28` case that motivated the abandoned
      suppression rule.
- [x] Canvases whose rim was already a break render byte-identically (existing
      vdiffr baselines unchanged); every baseline that does move is inspected
      and shown in the milestone to differ only by the intended rim ring and
      label.
- [x] Every figure in `vignettes/advanced-visualization.Rmd` is rendered and
      visually inspected, with the inspection recorded; the `coord-bare` chunk
      shows the bare figure alongside the same plot with the scale breaks
      supplied, and the surrounding prose describes that contrast.
- [x] `devtools::test()` clean, `devtools::document()` no diff, and
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
- [x] T2. Test-first in `tests/testthat/test-coord_circumplex.R`: the case table
      as contract tests, asserting the ring is present at the rim
      (`panel_params$r$get_breaks()`) and that its label is blank exactly when
      `amax` is not itself a generated break
      (`panel_params$r$get_labels()`), plus the trained-`amax` and
      nonzero-`center` cases. Confirm they fail before T3.
- [x] T3. Implement in `R/coord_circumplex.R` — a helper beside `rim_limit()`
      (added by PR #62) that wraps the radial `ViewScale` returned by the
      parent's `setup_panel_params()`, appending the rim to its breaks and
      blanking that entry's label. The grid reads `r$mapped_breaks()`, so the
      ring and the guide both follow from the one patched break set. Keep the
      ULP headroom: it still carries the already-a-break cases.
- [x] T4. Regenerate the vdiffr baselines per the M31 procedure — delete the
      stale `_snaps/<file>/*.svg`, re-run under `NOT_CRAN=true` (a bare
      `Rscript` run silently skips the comparison), and diff each moved SVG to
      confirm only the rim ring and its label were added.
- [x] T5. Rework the `coord-bare` chunk at `vignettes/advanced-visualization.Rmd`
      :90-108 into the bare figure plus its corrected counterpart, and rewrite
      the prose at :104-108 around that contrast.
- [x] T6. Render every figure in `advanced-visualization.Rmd` and look at each
      one (M33/M36/M37 lesson: data-level fences and vdiffr baselines both pass
      a figure that reads wrong). Record what was inspected; route anything
      found beyond this scope to a candidate row rather than absorbing it.
- [x] T7. Update the DESIGN.md visualization section (the coord owns the rim
      ring, not only the radial limits), add the NEWS entry, and run the full
      profile check surface.

## Work log

- 2026-07-18: created by /milestone-plan; absorbs two candidate rows added the same day (guaranteed rim ring; bare-coord vignette figure), both spun out of the PR #62 hotfix. Plan gate: labeled break + collision rule over an unlabeled rim or a custom grob; vignette figure paired rather than silently fixed; inspection sweep scoped to this one vignette; added to M7's v2.0.0 bundle.
- 2026-07-18: T1 done; substantive amendment at the implement gate (Jeff approved) — Scope In, AC1 and AC2 replaced, the labeled-rim-with-suppression mechanism moved to Out, T1/T2/T3 rewritten. Rationale in M38-D1: render evidence, not arithmetic, settled it.
- 2026-07-18: FLAG for the user — the Goal still says the rim ring is "labeled", which M38-D1 contradicts. The Goal is create-only (never edited in place), and the milestone's substance (the canvas closes at its rim) is unchanged, so it is left as written and surfaced here rather than quietly corrected. Decide at review whether to strike the word or re-cut.
- 2026-07-18: T2+T3 done in one commit (a test-first commit would land a red suite). `rim_view_scale()` wraps the parent's radial ViewScale, appending the rim with a blank label; `r.major` recomputed to stay consistent. Rendered and inspected amax=1.75 and center=0.15/amax=0.28: circle closed, ladder unchanged, no label collision. Suite 2980 passing under NOT_CRAN=true.
- 2026-07-18: T4 done — NO existing vdiffr baseline moved, and the reason is not a silent skip: every vdiffr canvas in the suite uses amax 0.5, 0.6 or 1.0, all already-a-break cases the change does not touch (the same run under NOT_CRAN=true did flag the two geom_ssm_path baselines for PR #62, so comparison is live). That left the new behavior with no visual guard, so one baseline was ADDED at amax = 1.75: 14 polylines, labels 0.00/0.50/1.00/1.50 and no 1.75 — five rings, four labels, exactly M38-D1.
- 2026-07-18: T5+T6 done. `coord-bare` now pairs the bare figure with a `coord-bare-scaled` counterpart carrying the octant breaks; the prose makes the difference between them the lesson. All 15 figures in the vignette knitted (under `load_all()`, dev version 1.3.0.9002 printed — the M21/M34 installed-vs-dev trap) and inspected one by one: every circumplex canvas closes at its rim, the Cartesian figures (occasions-plot, curve-axis) are unaffected. One out-of-scope defect found and routed to a candidate row rather than absorbed: radial axis labels are drawn beneath the geom layers and get obscured by markers/arrowheads.
- 2026-07-18: T7 done — DESIGN.md's coord bullet now records that the coord owns the rim ring and why the appended ring is unlabeled; NEWS entry added above the PR #62 hotfix line. `devtools::document()` no diff; `devtools::check(args = "--no-manual")` 0 errors / 0 warnings / 0 notes; suite 2981 passing. Status → review.
- 2026-07-18: review — 5 findings from the diff-bug lens, all scored >=80, all actioned; two were user-facing regressions this milestone introduced (a hard build error on scales carrying explicit `labels`, and literal NA labels under `labels = NULL`). Fixed on the branch with regression tests; M38-D2 appended to correct M38-D1's overstated rationale. All 5 AC verified with fresh evidence; consistency gate clean.

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

### M38-D2 (2026-07-18, review): correcting M38-D1's claim about trained canvases

M38-D1's parenthetical listed "every trained-`amax` canvas" among the cases
where the rim keeps its own label, and concluded that "the unlabeled rim appears
only where the user chose an unround `amax` deliberately". Both are wrong, and
the review's diff-bug lens caught it: a trained rim is the data maximum, which
is essentially never a generated break -- data maxima of 0.42, 0.37, 0.61,
0.835, 1.23 and 0.17 all produce a blank-labeled rim. Since `amax = NULL` is
`coord_circumplex()`'s documented default, **an unlabeled rim is the common
case, not the rare one.**

The decision itself is unchanged and stands: the rim is unlabeled unless `amax`
is itself a generated break. Only its rationale's reach was overstated.
`NEWS.md` and `DESIGN.md` state the rule correctly and needed no change.
Recorded here rather than by editing M38-D1, which is append-only history (IP4).

## Review

**PR:** https://github.com/jmgirard/circumplex/pull/63

**AC1 — ring at the rim.** Fresh probe over the case table: `amax` 0.7, 1.1,
1.75, 2.4 and the `center = 0.15, amax = 0.28` case each end their break set
exactly at the rim; 0.3, 0.5, 0.8, 1.2 (already-a-break) are unchanged and carry
one ring at the rim, not two. Trained (`amax = NULL`) canvas at data max 0.73
appends the rim. `min(breaks) == center` in every case.

**AC2 — labelling rule.** Same probe: labels blank at the rim exactly for
0.7/1.1/1.75/2.4/0.28, and the generated ladders are byte-for-byte what the
scale produces alone. Pinned by two tests, including the crowded 0.275/0.28
case. The rim is not handed to the labeller at all, so it cannot drag extra
decimals onto the visible labels (`0.0 0.5 1.0 1.5`, not `0.00 …`).

**AC3 — baselines.** `git diff --name-status master..HEAD -- tests/testthat/_snaps/`
reports exactly one file, an addition: `coord_circumplex/rim-ring-at-an-unround-amax.svg`.
No existing baseline moved, and not because comparison skipped — the same
harness flagged two `geom_ssm_path` baselines for PR #62. Every vdiffr canvas in
the suite uses `amax` in {0.5, 0.6, 1.0}, all already-a-break, which is why the
new baseline was added rather than reporting the clean run as evidence. The new
SVG holds 14 polylines and labels 0.0/0.5/1.0/1.5 with no 1.75 — five rings,
four labels.

**AC4 — vignette.** All 15 figures knitted under `load_all()` (dev version
1.3.0.9002 printed, guarding the M21/M34 installed-vs-dev trap) and inspected
individually. Every circumplex canvas closes at its rim; the two Cartesian
figures are unaffected. `coord-bare` is now paired with `coord-bare-scaled`.
One out-of-scope defect found (radial labels obscured by data layers) and routed
to a ROADMAP candidate row.

**AC5 — checks.** `devtools::test()` 2986 passing / 0 failures under
`NOT_CRAN=true`; `devtools::document()` no diff; `devtools::check(args =
"--no-manual")` 0 errors / 0 warnings / 0 notes.

**Consistency gate.** `cairn_validate` all checks passed (49 advisories, all
pre-existing). `pkgdown::check_pkgdown()` no problems. README.Rmd/README.md
present and untouched. No DESIGN.md principle changed, so `cairn_impact` was
skipped.

**Independent review — three lenses, then a scorer.**

- [S] prior-PR-comments: no prior-PR evidence (PRs 55/56/59/60/61/62 carry zero
  inline comments or review bodies — this repo reviews locally). Zero findings.
- [S] blame-history: no conflict with prior intent. D-019's holdings untouched;
  M32's mutate-self-before-delegating pattern and M38's post-delegation patch
  are orthogonal; PR #62's `rim_limit()` preserved and generalized, not defeated.
- [O] diff-bug: five findings, all scored at or above 80, all actioned.

**Findings and disposition** (scores from the independent [S] scorer):

- F1 (97) — appending the rim broke any plot whose radial scale supplies
  explicit `labels`: the scale pairs labels positionally with its own breaks and
  aborts on a length mismatch, so the build errored out entirely. **Fixed:**
  `get_labels()` now asks the scale to label only its own breaks and blanks the
  rim separately. Regression test added.
- F2 (90) — `scale_y_continuous(labels = NULL)` rendered four literal `NA`
  labels, because assigning into `NULL` by index fabricates a vector. **Fixed:**
  NULL labels return NULL. Regression test added in the same block.
- F3 (82) — the trained-`amax` assertion had no teeth: the helper placed the
  datum at exactly 1, which the break generator already emits, so it passed with
  the feature stubbed out. **Fixed:** the trained case now uses data max 0.73 and
  asserts the blank rim label.
- F4 (80) — M38-D1 claimed trained canvases keep their own rim label and
  concluded the unlabeled rim is rare; both are wrong, and inverted. **Fixed** by
  appending M38-D2 rather than editing the append-only decision. Shipped docs
  (NEWS, DESIGN) already stated the rule correctly.
- F5 (80) — the new vignette prose named "the theme" as a difference between the
  paired figures, but both chunks call `theme_circumplex()`. **Fixed:** the prose
  now names the one line that actually differs.

None scored below 80, so nothing was excluded.

**Open item for the user.** The Goal still reads "draws a labeled amplitude ring
at `amax`", which M38-D1 contradicts. The Goal is create-only and the
milestone's substance is unchanged, so it was flagged rather than edited.
