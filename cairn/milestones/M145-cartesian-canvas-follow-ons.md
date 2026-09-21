# M145: Per-side label margins and plotmath crosshair labels on the circumplex canvas

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Surface tier:** user-facing — changes the drawn output of the exported `ggcircumplex()` and `coord_circumplex()`
- **Branch/PR:** m145-cartesian-canvas-follow-ons

## Goal

Give `ggcircumplex(angle_labels = TRUE)` a plot margin sized per side from the labels that point toward that side, and draw plotmath amplitude labels on the Cartesian crosshair as plotmath.

## Scope

**In:** the `angle_labels = TRUE` margin rule in `ggcircumplex()` (`R/ssm_plot.R`, the `label_guide` block) for both grids. Expression amplitude labels in `cartesian_grid_grob()` (`R/coord_circumplex.R`), with their negated 180° and 270° halves. Tests, roxygen and NEWS. The milestone absorbs the ROADMAP candidate row "Cartesian canvas follow-ons" (M142 review O4, O7, O12).

**Out:** measuring rendered label widths at draw time (rejected at the plan gate, see Work log). Keeping the circle centered on the page (rejected at the plan gate). The 0°/360° theta-label merge (M142 review O8, rejected there as pre-existing). No new dependency (GP3).

## Acceptance criteria

- [ ] AC1: With `angle_labels = TRUE` and text labels, `ggcircumplex()` sets each side of `plot.margin` to the larger of two values: `theme_circumplex()`'s margin on that side, and the longest outward reach of any label toward that side. A label drawn at angle θ with n characters reaches `max(0, u) * 0.5 * font_size * n` pt. Here n is `nchar()` of the drawn label, the ` (θ°)` suffix included. The value u is cos θ for the right side, sin θ for the top, −cos θ for the left and −sin θ for the bottom. The rule is the same for `grid = "polar"` and `grid = "cartesian"`. Tests compare all four sides with values that the test computes on its own. The test reads the theme margin through `calc_element("plot.margin", complete_theme(...))`. The tests use four canvases. The first is `octants()` with `PANO()` labels (LM at 360°). The second has angles `c(0, 180)` with labels of different lengths, so left and right differ. The third has angles `c(90, 270)` with two labels, and its left and right sides equal the theme's. The fourth is one of these at `font_size = 20`.
- [ ] AC2: With `angle_labels = FALSE`, or with the default degree labels, `plot.margin` equals `theme_circumplex()`'s on all four sides. A test asserts this for all four combinations of those two cases and the two grids.
- [ ] AC3: With `grid = "cartesian"`, the amplitude scale's labels can be a list or expression of language objects. Then the crosshair label grobs carry those objects, never their deparsed text. The 0° and 90° half-axes carry each label unchanged. The 180° and 270° half-axes carry it with a leading minus. A symbol, a number, or a call to `[`, `^` or `(` gets a bare minus. Any other call is wrapped in parentheses. An `NA` or blank element draws nothing and no minus sign. A test builds labels with `labels = function(b) ...` that returns language objects. It checks each grob label with `identical()` against the expected calls for `alpha`, `alpha[2]`, `a + b`, `a - b`, `a / b` and `a == b`, and for a blank element. Character labels draw as before, as the existing tests show unedited.
- [ ] AC4: NEWS.md's development section states the per-side margin rule and the expression-label fix. The `angle_labels` roxygen states the margin rule. `devtools::document()` regenerates `man/ggcircumplex.Rd`.
- [ ] AC5: `devtools::test()` reports no failures, and `devtools::check(args = "--no-manual")` reports 0 errors, 0 warnings and 0 notes.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T3, T4
- AC4 → T2, T5
- AC5 → T5

## Tasks

- [x] T1: Write the tests first, in `tests/testthat/test-ssm_plot.R` beside the M142 block (~line 306). Add the AC1 margin cases on both grids, with an oracle written in the test, and the AC2 four-way case. Before T2, see the AC1 cases fail on the current symmetric rule.
- [x] T2: Replace the symmetric `plot.margin` in `ggcircumplex()` (`R/ssm_plot.R`, `label_guide` block, ~lines 710-723) with the per-side rule, in a small internal helper. Update the code comment and the `angle_labels` roxygen. Render the PANO canvas and a long-label canvas on both grids and look at them (M33 lesson). Then regenerate the vdiffr snapshot that the margin change moves (`test-ssm_plot.R` ~line 421) under `NOT_CRAN=true` (M31 lesson).
- [x] T3: Write the tests first, in `tests/testthat/test-coord_circumplex.R`. Add the AC3 expression-label cases, and walk the cartesian grid grob by name (`circumplex-cartesian-labels-x`/`-y`). Before T4, see them fail on the current `as.character()`.
- [x] T4: In `cartesian_grid_grob()` (`R/coord_circumplex.R`, ~lines 170-250), keep language labels as they are and build the negated halves by the AC3 rule. Replace the `is.na()`/`!= ""` blank test with one that is safe for language objects. Render one canvas with plotmath labels and look at it.
- [x] T5: Add NEWS.md entries for both changes. Run `devtools::document()`, then `devtools::test()` and `devtools::check(args = "--no-manual")`.

## Work log

- 2026-09-20: created by /milestone-plan. The criteria audit (full mode, [O] fresh reader) returned 12 findings on the draft. 11 were fixed before the gate: discriminating margin canvases, a varied `font_size`, n defined on the drawn label, the four-way AC2 case, `get_labels()` returning a list, a parenthesis rule that covers every call, blank language labels, a wider probe set, an instrument-bound clause restated as behavior, and NEWS for the fix. AC5 had no finding.
- 2026-09-20: plan gate chose a per-side margin estimated from label length over measuring rendered widths at draw time, because the estimate needs no graphics device at build time and stays in `ggcircumplex()`. Falsified by a rendered canvas in a common font where a label leaves the page or the margin exceeds the label by more than half its length.
- 2026-09-20: plan gate chose off-center margins over a centered circle (left/right and top/bottom paired at their maximum), because Nagy's figures do the same and it saves space. Falsified by a user report that an off-center circle misreads or misaligns in a multi-panel figure.
- 2026-09-20: T1 done. The AC1 margin tests failed on the old symmetric rule (every side 54 pt, or 190 pt at font size 20), and the AC2 cases passed. T2 code, roxygen and the regenerated snapshot are in this checkpoint. Renders of the PANO, long-label and uneven canvases kept every label on the page. T2 stays open until the full suite result is in.
- 2026-09-20: T2 done. The full `devtools::test()` run on 1cddc494 had no failures.
- 2026-09-20: T3 done. The AC3 tests fail on the current code because the grob labels are character, for example `'-a - b'` where the call `-(a - b)` is expected.
- 2026-09-20: T4 done. `cartesian_grid_grob()` keeps language labels as a list, and two helpers apply the AC3 minus rule and the blank test. The coord and plot tests pass, and a rendered canvas shows α, α₂, −(a + b) and −(a/b) drawn as plotmath.
- 2026-09-20: T5 in progress (checkpoint). NEWS entries are committed. `document()`, the full tests and `check()` are running in the background, and a claim audit ([O] fresh reader) is running on the branch diff.
- 2026-09-20: claim audit: 17 claims read, 4 corrected — NEWS.md, R/ssm_plot.R, R/coord_circumplex.R, tests/testthat/test-ssm_plot.R. The corrections were the parenthesis rule in NEWS, "off-centre" spelled "off-center", the blank-test comment, and the oracle comment. The reader's one re-read confirmed all four after the Rd regeneration and one comment re-wrap.
- 2026-09-20: T5 done. `devtools::test()` had no failures. `devtools::check(args = "--no-manual")` gave 0 errors, 0 warnings and 0 notes, and it ran after the audit corrections were on disk. Status set to review.

## Decisions

## Review
