# M37: On-circle movement paths across occasions

- **Status:** review
- **Priority:** normal
- **Depends on:** M31, M32, M33
- **Principles touched:** —
- **Branch/PR:** `m37-on-circle-movement-paths`

## Goal

Draw a profile's movement across occasions as a curved, arrowed path on the
circumplex canvas, so change in (amplitude, displacement) reads as motion in
circumplex space rather than only as parallel Cartesian panels.

## Scope

**In:**
- A new exported layer (working name `geom_ssm_path()`) taking `amplitude` /
  `displacement` aesthetics plus an ordering, emitting them as the coord's
  `y`/`x`. `coord_circumplex()` reports `is_linear() == FALSE`, so ggplot2's
  munching already curves each segment along the polar geodesic — the layer
  supplies ordering, seam handling, and arrows, not drawing code.
- **Seam correctness:** consecutive occasions travel the *short* way across the
  0/360 boundary. Angles are unwrapped before the coord sees them (reusing
  `ssm_unwrap_gapped()`, `R/ssm_trajectory.R`); unwrapped values outside
  `[0, 360]` reach layer data uncensored (probed 2026-07-18: `390` and `-10`
  both survive), consistent with M31's coord-side-`thetalim` invariant.
- Optional arrowheads marking the direction of time.
- A plot-level convenience surface over the layer, taking an
  `ssm_analyze(occasions = )` object (D-018b: composable layers for power users,
  thin wrappers for everyone else). Its exact shape — a new `ssm_plot_*()`
  function versus an argument on `ssm_plot_circle()`, which already tolerates the
  conditional `Occasion` column (`R/ssm_plot.R:308-312`) — is settled at the
  implement gate. **(RB tripwire: irreversible-api)**
- A figure in the plotting vignette and pkgdown reference placement, matching
  the M34 grouping.

**Out:**
- True animation (gganimate or otherwise) and any new dependency — plan-gate
  decision 2026-07-18; recorded as a `candidate` ROADMAP row, not a rejection.
- Confidence regions along the path (wedge-per-occasion already ships via
  `geom_ssm_arc()`; a swept uncertainty band is not planned).
- Cartesian trajectory panels — `ssm_plot_trajectory()` (M33/M35) owns those.
- The certification legend fix and the `amax`/`center` guards → M36.

## Acceptance criteria

- [x] `geom_ssm_path()` is exported, documented with a runnable example, and
      renders a connected path for a multi-occasion profile on a
      `ggcircumplex()` canvas; a plot-structure test locates the layer by geom
      class (never a positional `data[[i]]` index, per the M31 lesson).
- [x] A seam test asserts that a path between occasions straddling 0/360 (e.g.
      `350 -> 10`) traverses the **short** arc: the built layer's x values span
      `20` degrees, not `340`. The test fails against a naive implementation
      that feeds raw `[0, 360)` angles to the coord.
- [x] Degenerate input is handled at both ends: an occasion whose displacement
      is undefined (flat / zero-amplitude profile, `ssm_has_location()` FALSE)
      breaks the path rather than interpolating through it, with the post-gap
      tail still drawn (`ssm_unwrap_gapped()`'s bridging contract, M33); and
      non-finite angles are rejected by a `!is.finite()` guard before reaching
      the unwrap (the M32/M35 recurring trap: `is.na(Inf)` is FALSE, and
      `ssm_has_location()` reads `Inf` as located, NaN-ing a `cumsum()` unwrap).
- [x] Arrowheads render when requested and are absent by default; verified at
      grob level, not by baseline alone.
- [x] The convenience surface produces a correct figure from an
      `ssm_analyze(occasions = )` object with occasions in `details$occasions`
      order — a `T10`/`T2` pair must not flip (M33 lesson).
- [x] One render-and-inspect pass is recorded per new figure (M33 lesson:
      data-level fences and a vdiffr baseline both pass a figure that reads
      wrong), plus a vdiffr baseline.
- [x] `devtools::test()` clean; `devtools::check()` at 0 errors / 0 warnings /
      0 notes; `check_pkgdown()` clean with the new exports placed.

## Coverage

- AC1 → T2, T6
- AC2 → T1, T3
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6, T7
- AC7 → T7

## Tasks

- [x] T1: Write the failing seam test first (path `350 -> 10` spans 20 degrees,
      not 340), against the intended layer API.
- [x] T2: Implement `GeomSsmPath` / `geom_ssm_path()` in `R/geom_ssm.R`,
      emitting `amplitude`/`displacement` as `y`/`x` and letting the coord munch;
      follow the `GeomSsmPoint` `setup_data()` pattern (`R/geom_ssm.R:146-166`).
- [x] T3: Wire ordering + seam unwrapping through `ssm_unwrap_gapped()`, the
      gap-breaking behavior, and the `!is.finite()` guard; tests for each.
- [x] T4: Add the arrow parameter and its grob-level test.
- [x] T5: Settle the convenience-surface shape at the implement gate
      (new `ssm_plot_*()` vs an `ssm_plot_circle()` argument), then build it with
      occasion ordering taken from `details$occasions`.
      **(RB tripwire: irreversible-api)**
- [x] T6: Vignette figure + pkgdown reference placement + `@family` cross-links,
      matching M34's grouping; render-and-inspect pass recorded.
- [x] T7: `devtools::document()`, full `devtools::test()`, `devtools::check()`,
      `check_pkgdown()`; NEWS.md entry.

## Work log

- 2026-07-18: created by /milestone-plan. Promotes the M33 deferral from the
  ROADMAP visualization candidate row; both of its stated revisit conditions
  (trajectory viz shipped, M31 coord API settled) now hold. Animation excluded
  at the plan gate.
- 2026-07-18: T1-T5 done. `geom_ssm_path()`/`GeomSsmPath` added to `R/geom_ssm.R`
  (seam unwrap per group via `ssm_unwrap_gapped()`, `!is.finite()` guard before
  the unwrap, optional `order` aes, `arrow` param); `ssm_plot_circle(path = )`
  added. 31 tests in `tests/testthat/test-geom_ssm_path.R`; the AC5 ordering
  guard mutation-checked (alphabetical ordering makes it fail). Question gate
  settled the API shapes — see M37-D1. No new dependency: `ggplot2` re-exports
  `arrow()`/`unit()`.
- 2026-07-18: T6-T7 done. Vignette section added to `advanced-visualization.Rmd`
  (layer form + wrapper form, on the existing seam-crossing three-wave fixture);
  pkgdown Building Blocks row; `ssm_plot_trajectory()` `@seealso` cross-link;
  NEWS entries; 2 vdiffr baselines. The render-and-inspect pass caught a defect
  no data-level fence or baseline can — the terminal arrowhead drawn underneath
  the final occasion's point marker, hiding the direction of time. Fixed by
  drawing the wrapper's path last and sizing the arrow to clear a size-3 marker.
  Status -> review. `devtools::test()` 2936 pass / 0 fail;
  `devtools::check()` 0 errors / 0 warnings / 0 notes; `check_pkgdown()` clean.
- 2026-07-18: review sent back to in-progress on 3 findings (scored 96/90/82).
  F1 `order` aesthetic rendered a zeroGrob and misordered — removed, see M37-D2.
  F2 `path=TRUE` bypassed `drop_lowfit`, routing the path through a dropped
  occasion — the occasion is now blanked so the path breaks there; regression
  test added. F3 the roxygen example drew a zero-length path (identical
  occasions) — rebuilt on a rotating fixture, re-rendered and inspected.
  Re-verified: 2944 pass / 0 fail; check 0/0/0; `check_pkgdown()` clean.
  Blame-history and prior-PR lenses returned no findings; none scored below 80.

## Decisions

### M37-D1 (2026-07-18): the convenience surface is `ssm_plot_circle(path = )`

Settled at the implement gate — the plan's `irreversible-api` tripwire on T5.
`ssm_plot_circle()` is the door M34's vignette teaches for circle figures and
already tolerates the conditional `Occasion` column; a separate
`ssm_plot_path()` would have duplicated ~150 lines of canvas/palette/arc/point
assembly for the same figure. Cost accepted: `path = TRUE` is meaningful only
for an occasions object and errors otherwise, naming how to produce one.

Two deliberate consequences inside the wrapper: the path is built from the
pre-filter results frame, not `df_plot`, so an undefined-displacement occasion
survives as `NA` and **breaks** the path instead of being silently bridged
(review later extended this to `drop_lowfit`, which was leaking dropped
occasions into the path); and with a contrast the path branch drops only the
contrast row (`-nrow(df)`, the detector `ssm_trajectory_frame()` uses) rather
than the historical `df[1:2, ]` slice, which truncates an occasions object.

Also settled here: `arrow` takes a `ggplot2::arrow()` object, `NULL` by default.
`ggplot2` re-exports `arrow()`/`unit()`, so nothing depends on `grid`.

### M37-D2 (2026-07-18): the `order` aesthetic is removed — supersedes M37-D1's ordering clause

Found at review (diff-bug lens, scored 96) and reproduced. ggplot2's
`add_group()` builds `group` from *every* discrete column when the caller maps
none, and offers no extension point to exempt an aesthetic. So a character
`order` — the occasion labels the docs advertised it for — put each row in its
own group and rendered a `zeroGrob` (nothing drawn), while sorting rows
alphabetically into the exact `T1, T10, T2` misordering the aesthetic existed to
prevent. With an explicit `group` it still misordered; only a numeric or
correctly-levelled factor worked, and a caller holding either can just sort the
data frame. No acceptance criterion required it.

`geom_ssm_path()` now has strict `geom_path()` parity: data row order, `group`
separating series. The T10/T2 protection stays where it is real and tested —
`ssm_plot_circle(path = TRUE)` ordering from `details$occasions` (AC5,
mutation-checked). A test locks the aesthetic out so it cannot return by accident.

## Review

**Reviewed 2026-07-18. PR #61. Verdict: pass after one send-back.**

### Acceptance-criteria evidence (fresh, by command)

- **AC1** — `NAMESPACE` exports `geom_ssm_path` + `GeomSsmPath`; `man/geom_ssm_path.Rd`
  present. The Rd example was extracted and executed: it builds and yields a path
  layer at 3 distinct locations (324.3, 279.3, 234.3). Layer located by geom class
  in all tests via `inherits(l$geom, "GeomSsmPath")`, never a positional index.
  Grob is a `polyline`, not a `zeroGrob`.
- **AC2** — `diff(range(x)) == 20` for `350 -> 10`; multi-crossing extends
  (350/370/390/410); clockwise crossing goes negative. **Mutation-checked:** deleting
  the unwrap loop fails 8 assertions, so the "fails against a naive implementation"
  clause is verified, not assumed.
- **AC3** — gap test asserts `x == c(350, NA, 390)` (breaks *and* bridges the branch).
  **Mutation-checked:** substituting the `is.na`-based `ssm_has_location()` for the
  `!is.finite()` guard fails 3 assertions, confirming `Inf` would otherwise NaN the tail.
- **AC4** — grob-level: `arrow` is `NULL` by default, `inherits("arrow")` when requested.
- **AC5** — `path = TRUE` orders by `details$occasions` on a T1/T2/T10 fixture.
  **Mutation-checked:** alphabetical ordering yields 324.3/234.3/279.3 and fails.
  Non-occasions object errors; `path = FALSE` adds no layer; `drop_lowfit` regression test.
- **AC6** — render-and-inspect performed on 4 figures (layer, wrapper, gap, corrected
  Rd example); 2 vdiffr baselines committed. The pass caught the hidden-arrowhead
  defect that every data-level fence and the baseline itself passed.
- **AC7** — `devtools::test()` 2944 pass / 0 fail; `devtools::check()` 0 errors /
  0 warnings / 0 notes; `pkgdown::check_pkgdown()` clean; `document()` no diff.
  CI green on 7 platforms + codecov (re-run after the fixes).

### Consistency gate

`cairn_validate` exit 0, all 15 checks PASS (64 advisory work-log-wrap warnings,
D-046: warns, never fails). No principle changed, so `cairn_impact` skipped.
Profile `r-package` toolchain slot: `document()` no-diff, generated files clean,
README in sync, `check_pkgdown()` clean, NEWS entries present, no new top-level files.

### Independent review — 3 lenses + scorer

- **[O] diff-bug (Opus):** 3 findings, all reproduced and actioned (below).
- **[S] blame-history (Sonnet):** no findings. Independently cleared the
  `!is.finite()` bypass of `ssm_has_location()` as a strict superset that cannot
  desynchronize the geoms, and confirmed the `df[-nrow(df), ]` contrast branch
  against M37-D1.
- **[S] prior-PR-comments (Sonnet):** no prior-PR evidence — this repo reviews
  locally, not through GitHub's review API. Clean no-op.
- **[S] scorer (Sonnet):** 96 / 90 / 82. None below 80, so nothing excluded.

**Findings actioned — all fixed on the branch:**

1. (96) `order` aesthetic rendered a `zeroGrob` and misordered under
   `add_group()` fragmentation → aesthetic removed (M37-D2), locked out by test.
2. (90) `path = TRUE` bypassed `drop_lowfit`, routing the path through a dropped
   occasion → occasion blanked so the path breaks there; regression test added.
3. (82) the Rd example drew a zero-length path → rebuilt on a rotating fixture,
   re-rendered and inspected.
