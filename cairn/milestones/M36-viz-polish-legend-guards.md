# M36: Visualization polish — certification legend key + non-finite guards

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** `m36-viz-polish`

## Goal

Close the two shipped-code remainders of the M31–M33 visualization track: the
certification legend's missing `FALSE` key glyph, and `coord_circumplex()`'s
`amax`/`center` acceptance of non-finite values.

## Scope

**In:**
- `ssm_plot_trajectory()`'s "Displacement interpretable" legend draws **both**
  keys (`TRUE` solid, `FALSE` hollow) whenever certification is shown, including
  when the data contain no uncertified point. Affects the occasions path and the
  `ssm_draws()` table path alike (one shared site, `R/ssm_trajectory.R:610-635`).
- `!is.finite()` guards on `coord_circumplex()`'s `amax` and `center`, matching
  the treatment M32 gave `r_axis_angle` (LESSONS 2026-07-18).

**Out:**
- Any change to *which* points are certified (D-007 rule) or to the shape
  encoding itself — the encoding stands, only its legend rendering is fixed.
- On-circle movement paths → M37.
- A sweep of every other numeric API argument for `is.na()`-vs-`is.finite()`
  gaps → stays the standing "continuous / infrastructure refactors" candidate
  row, folded into whichever milestone next touches those functions.

## Acceptance criteria

- [x] A grob-level test asserts the shape legend draws one key glyph per scale
      break (2) for an **all-certified** trajectory, and fails against the
      pre-fix code. Verified by extracting `pch` from the `guide-box-bottom`
      grob tree: pre-fix yields `16` only; post-fix `16, 1`.
- [x] The same assertion holds on the `ssm_draws()` table path (a table whose
      `certified` column is all `TRUE`) and on a mixed table (unchanged: `16, 1`).
- [x] `coord_circumplex(amax = Inf)` and `coord_circumplex(center = -Inf)` each
      error at call time naming the offending argument, matching the message
      style of the existing `r_axis_angle` guard; `NA` and `NaN` keep erroring.
- [x] A vdiffr baseline for the all-certified trajectory is regenerated and shows
      both legend keys (regenerated per the M31 lesson: delete stale `_snaps`
      SVGs, re-run under `NOT_CRAN=true`).
- [x] `devtools::test()` clean and `devtools::check()` at 0 errors / 0 warnings /
      0 notes.

## Coverage

- AC1 → T1, T2
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5

## Tasks

- [x] T1: Write the failing grob-level legend test (extract `pch` from the
      `guide-box-bottom` grob tree for an all-certified fixture); confirm red
      against current `R/ssm_trajectory.R`. Note for implement: neither
      `override.aes$shape` nor a 2-level `factor(Certified)` with `drop = FALSE`
      restores the key — both probed 2026-07-18 and rejected. ggplot2 draws key
      glyphs only for values present in layer data, so the fix must make the
      absent value present (e.g. a zero-size / `alpha = 0` presence layer keyed
      to the missing break) or build the key manually.
- [x] T2: Implement the fix at `R/ssm_trajectory.R:610-635`; extend the test to
      the table path and re-assert the mixed-data case is unchanged.
- [x] T3: Add `!is.finite()` guards for `amax` and `center` in
      `R/coord_circumplex.R`, with error-branch tests for `Inf`/`-Inf`/`NA`/`NaN`.
- [x] T4: Regenerate the affected vdiffr baseline(s) and confirm unaffected
      plots regenerate byte-identically.
- [x] T5: `devtools::document()`, full `devtools::test()`, `devtools::check()`;
      NEWS.md entries for both fixes.

## Work log

- 2026-07-18: created by /milestone-plan. Absorbs two items from the ROADMAP
  "continuous / infrastructure refactors" candidate row (legend glyph, M35-found,
  M33-inherited; `amax`/`center` guard, M32 review). Legend behavior chosen at
  the plan gate: both keys always drawn.
- 2026-07-18: T1–T4 done on `m36-viz-polish`. The legend defect reads at grob
  level as a key gTree holding a `zeroGrob` where its glyph belongs; test red
  pre-fix (one glyph, `16`), green post-fix (`16`, `1`), mixed-data case
  unchanged throughout. Four fix techniques probed: `override.aes` shape vector
  (fails, confirming the M35 finding), `alpha = 0` presence layer (works, but
  adds invisible geometry), zero-row presence layer (works, draws nothing —
  adopted), `geom_blank()` (wrong glyph). Tightened mid-task after the baseline
  diff showed the fix made BOTH layers claim the legend, overdrawing the TRUE
  key: the real layer now takes `show.legend = FALSE` and the helper counts
  every glyph per key so overdraw cannot pass. Render-and-inspect done (legend
  reads "● TRUE ○ FALSE"); baseline diff is confined to two legend circles, no
  panel movement.
- 2026-07-18: T3 changed two pre-existing coord assertions that pinned incidental
  message text (`amax = NA` → "greater than"; `center = NA` → "is.na"). Both now
  assert the argument name plus "finite", which is the contract; the amax-below-
  center comparison keeps its own distinct message and its own assertion.
- 2026-07-18: T5 done; status → review. `document()` no diff; full suite 2903
  passing / 0 failed / 0 errors / 0 skipped (the 4 CPM Hessian warnings are
  pre-existing); `check()` 0 errors / 0 warnings / 0 notes. NEWS entries added
  for both fixes.

## Decisions

## Review

Reviewed 2026-07-18. PR #60. Branch `m36-viz-polish`.

### Acceptance-criteria evidence

- **AC1 (legend draws both keys; test fails pre-fix).** VERIFIED. Fresh
  mutation proof: the pre-fix construction (real layer at `show.legend = NA`,
  no other change) run through the same `legend_key_glyphs()` helper the test
  uses reports keys `16 | NA` — a glyph-less key; post-fix `16 | 1`. The guard
  bites. `test-ssm_trajectory.R` 66 passing.
- **AC2 (table path + mixed unchanged).** VERIFIED. `test-ssm_trajectory_table.R`
  79 passing, covering an all-`TRUE` table and the mixed table. Post-fix sweep of
  every reachable certification state: all-certified, mixed, all-uncertified and
  partly-NA all give `16 | 1`; all-NA correctly yields no legend at all; a table
  whose displacements are all NA still gives `16 | 1`.
- **AC3 (non-finite amax/center rejected, naming the argument).** VERIFIED. All
  eight combinations (`amax`/`center` × `NA`/`NaN`/`Inf`/`-Inf`) error naming
  the offending argument and the word "finite"; the `amax <= center` comparison
  keeps its own distinct message; a valid `amax` still builds a
  `CoordCircumplex`. `test-coord_circumplex.R` 47 passing.
- **AC4 (baseline regenerated, both keys).** VERIFIED. Each of the two vdiffr
  baselines differs from master by exactly one added `<circle>` — stroke, no
  fill (the hollow FALSE key) — at the legend row `cy=543`; no panel geometry
  moved. Render-and-inspect done: the legend reads "● TRUE ○ FALSE".
- **AC5 (test + check clean).** VERIFIED. Suite 2903 passing / 0 failed /
  0 errors / 0 skipped. `check()` 0 errors / 0 warnings / 0 notes.

### Consistency gate

`cairn_validate.py`: all checks passed. No principle change → `cairn_impact`
skipped. Toolchain slot: `document()` no diff; `check_pkgdown()` no problems;
README.md in sync (untouched); NEWS.md carries both user-visible entries with no
milestone numbers; no new top-level files; full `check()` clean.

### Independent review (three lenses + scorer)

- **[O] diff-bug (Opus):** two findings, below.
- **[S] blame-history (Sonnet):** no findings — M33's `override.aes` hollow-key
  fix is carried forward intact, the M33 `d_rows`/`other_rows` panel split is
  untouched, and M31/M32's deliberate `center`-before-comparison validation
  order is preserved.
- **[S] prior-PR-comments (Sonnet):** no prior-PR evidence (PRs #54–#59 carry
  zero review comments). A permanent clean no-op in this repo per the M33
  lesson; carries no evidential weight.

**F1 (score 92) — ACTIONED, fixed.** *"the comment states a mechanism that is
false, and the machinery it justifies is inert … What actually fixes the legend
is `show.legend = TRUE` — the zero-row `key_rows` frame contributes nothing …
deleting the presence layer and setting `show.legend = TRUE` on the existing
`d_rows` layer produces identical legends."* Independently reproduced: variant A
(pre-fix) `16 | NA`, variant B (`show.legend = TRUE`, no presence layer)
`16 | 1`, variant C (as shipped) `16 | 1`. The presence layer and its
compensating `show.legend = FALSE` were removed; the comment now states the
mechanism that actually operates. Confirmation the layer was inert: after the
simplification both vdiffr baselines are **byte-identical** to the ones
committed with it, and the full suite is unchanged at 2903 passing. This also
retires the false premise the plan's T1 note inherited (recorded here rather
than by rewriting a plan-owned task).

**F2 (score 85) — ACTIONED, fixed.** *"the diff appended a new comment block
without retiring the old one it supersedes … The retained sentence now describes
an `is.na()` guard that no longer exists in the function."* The superseded
sentence was removed; the surviving comment carries the ordering rationale once.

No findings scored below 80.
