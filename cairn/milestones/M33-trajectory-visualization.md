<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M33: Longitudinal trajectory visualization (occasions objects)

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Principles touched:** — (works under the CLAUDE.md angle invariants: LM=360,
  displacement seam unwrap, occasion order = list order, never alphabetical)
- **Branch/PR:** `m33-trajectory-viz` · https://github.com/jmgirard/circumplex/pull/57

## Goal

Export a Cartesian trajectory plot of SSM parameters across occasions for
occasions objects, with per-occasion confidence bands, correct 0/360 seam
unwrapping, and D-007 certification marking on the displacement panel.

## Scope

**In:**
- Exported `ssm_plot_trajectory()`: a Cartesian ggplot faceting e/x/y/a/d over
  occasions with ribbon CIs and `drop_xy`, for `ssm_analyze(occasions = )` and
  `ssm_analyze_long()` objects, in the M31/M32 house conventions.
- **Temporal occasion ordering** — factor `Occasion` against
  `details$occasions`; unfactored, a discrete scale flips a `T10`/`T2` pair.
- **Displacement seam handling** — per group series, `angle_unwrap(d_est)` for
  the branch; the interval anchored at its lower bound and widened by its stored
  arc span (`ssm_arc_span()`), the only form that survives an arc > 180°.
- **Certification marking** — uncertified occasions (`ssm_certified()`) draw
  hollow on the displacement panel (D-013/RR06 per-t caution).
- **Contrast row dropped**, documented, users pointed at `ssm_plot_contrast()`;
  grouping supported, one series per `Group` level.

**Out:**
- Model-based trajectories from an `ssm_draws()` table + growth vignette swap → **M35**.
- On-circle animated/arrow movement paths across occasions → ROADMAP candidate.
- `coord_circumplex()` `amax`/`center` non-finite guard → ROADMAP candidate.
- Plotting vignette + pkgdown reorg → M34.

## Acceptance criteria

- [x] `ssm_plot_trajectory()` returns a ggplot of faceted SSM parameter
      trajectories for both an `ssm_analyze(occasions = )` object and an
      `ssm_analyze_long()` object; happy-path test for each, plus a grouped
      object yielding one series per `Group` level.
- [x] Occasions plot in **temporal (list) order, never alphabetical**: a fixture
      with occasions named `T2`, `T10` (in that order) is asserted at the data
      level to keep that order on the x scale. An implementation omitting the
      `details$occasions` factoring fails. *(source: CLAUDE.md Statistical
      invariants, "Occasion contrasts: second listed minus first listed".)*
- [x] A displacement trajectory whose occasions cross the 0/360 seam renders as a
      **continuous** path with its ribbon on the estimate's branch — asserted at
      the data level (`ggplot_build()` layer data), never by eye, because
      `devtools::check()` runs clean on a visually wrong figure. Teeth proven by
      mutation: replacing the signed-distance placement with a naive branch-offset
      or a linear wrap turns the test red (M13 teeth rule). *(source: LESSONS M27;
      [ssm_bootstrap.R:190](../../R/ssm_bootstrap.R).)*
- [x] An occasion failing `ssm_certified()` renders hollow on the displacement
      panel while a certified one renders filled — asserted at the data level on
      the layer's shape column. *(source: D-007; D-013/RR06 holding 3.)*
- [x] Degenerate handling: a flat/zero-amplitude occasion (`d_est = NA`) is
      classified through `ssm_has_location()`/`ssm_has_region()`, leaves a gap in
      the series rather than a spurious segment or a broken unwrap chain, and
      obeys `na.rm` parity (default silent; `na.rm = FALSE` warns once naming the
      dropped count). A `contrast = TRUE` object plots k occasion rows, not k+1.
- [x] Error branches fire with informative messages: a non-`circumplex_ssm`
      object; a `circumplex_ssm` without occasions; a non-finite/non-scalar
      numeric argument (`!is.finite()`, LESSONS M32); an unrecognized `...`
      (`chkDots()` "disregarded" warning).
- [x] `devtools::test()` and `devtools::check()` clean (0 errors / 0 warnings /
      0 notes); a vdiffr baseline recorded under `skip_on_ci()`.

## Coverage

- AC1 → T1, T3
- AC2 → T1, T4
- AC3 → T2, T4
- AC4 → T3, T4
- AC5 → T1, T3, T4
- AC6 → T3, T4
- AC7 → T5

## Tasks

- [x] **T1** — Internal reshape helper: occasions object → long frame keyed on
      (Group × Occasion × Parameter); `Occasion` factored to
      `details$occasions`, contrast row dropped positionally. Unit-tested.
- [x] **T2** — Displacement branch: per-group `angle_unwrap()` + span-anchored
      interval placement, after T1's ordering, NA-tolerant across a gap.
- [x] **T3** — `ssm_plot_trajectory()`: roxygen, argument validation,
      `chkDots()`, `drop_xy`, grouping series, ribbon/line/point layers,
      certification shape mapping, `facet_wrap(~Panel, drop = FALSE)`.
- [x] **T4** — Tests: seam continuity (mutation-proven), T2/T10 ordering,
      certification shapes, flat-occasion gap, `na.rm` parity, error branches.
- [x] **T5** — `document()`, NEWS entry, vdiffr baselines, full `test()` +
      `check()`.

## Work log

- 2026-07-17: created by /milestone-plan (viz expansion, area C).
- 2026-07-18: re-planned by /milestone-plan against the shipped M31/M32 contract.
  Investigation found no vignette constructs an occasions object — the growth
  vignette figure is built from glmmTMB + `ssm_draws()` — so the vignette-swap
  scope and its criterion moved to M35 (plan gate: split). Gate also settled
  certification marking (in), contrast row (dropped), and left the parked
  `coord_circumplex()` non-finite guard candidate untouched.
- 2026-07-18: in-progress on `m33-trajectory-viz`. No implementation question
  gate — the plan was written this session against a fresh investigation and
  left nothing open; conventions settled by precedent (`drop_xy = FALSE` per
  `ssm_plot_contrast()`, `scales = "free_y"`, legend hidden when ungrouped).
- 2026-07-18: T1+T2 done together (the unwrap runs inside the reshape, before
  the melt, so it sees the ordered series — splitting the commit would have
  split one unit). `ssm_trajectory_frame()` + `ssm_unwrap_gapped()` +
  `ssm_bound_on_branch()` in `R/ssm_trajectory.R`; 31 direct tests. Both guards
  proven to bite by mutation (naive branch-offset placement → ribbon assertions
  red; unfactored Occasion → T2/T10 order red). Fixture retuned to
  `d = c(350, 359, 8, 16)`, noise 1.0 after the first choice produced no
  stored-reversed interval — the seam assertions would have been vacuous.
  Deviation from plan: `ssm_unwrap_gapped()` bridges a gap rather than
  inheriting `angle_unwrap()`'s NA-onward policy, which would blank the whole
  post-gap tail (AC5 requires a gap, not a broken chain); the widened
  assumption is documented at the helper. Full suite 0 failures / 2782 pass.
- 2026-07-18: T3+T4 done. `ssm_plot_trajectory()` exported (roxygen, NAMESPACE,
  `_pkgdown.yml` row); 55 tests in `test-ssm_trajectory.R`, 0 failures.
  Rendered and inspected the real figures rather than trusting the suite: the
  displacement panel runs 353->380 continuously across the seam, the forced
  uncertified occasion draws hollow. Two cosmetic defects found only by looking
  and fixed -- an ungrouped series drew in a hue encoding nothing (now black,
  matching the other Cartesian plots), and the grouped shape legend's hollow key
  was invisible (now pinned black via override.aes). `na.rm = FALSE` count
  corrected to one per profile; it had reported a flat occasion once per
  affected panel.
- 2026-07-18: T5 done; status -> review. NEWS entry added; two vdiffr baselines
  recorded under `skip_on_ci()` (secondary to the data-level fences — bootstrap
  CI positions are BLAS-sensitive). `devtools::document()` produces no diff.
  Full `devtools::test()` and `devtools::check()` clean: 0 errors / 0 warnings /
  0 notes.
- 2026-07-18: Scope's seam bullet corrected to describe the shipped
  span-anchored placement (the pre-fix wording described the superseded
  per-bound form); no In/Out item added or removed.
- 2026-07-18: review — three lenses + scorer; F1 (95) and F2 (85) fixed on the
  branch, F3 (45) logged and absorbed into a ROADMAP candidate. All seven
  criteria ticked against fresh evidence.
- 2026-07-18: minor amendment — Scope compressed (no In/Out item changed, only
  rationale now carried by code comments) to bring the plan-owned body under
  the 150-line cap.

## Decisions

## Review

Reviewed 2026-07-18. PR #57. Evidence gathered fresh by command, not recall.

### Acceptance-criteria evidence

- **AC1** — `test-ssm_trajectory.R`: "both occasions constructors yield a
  ggplot" (an `ssm_analyze(occasions = )` fit and an `ssm_analyze_long()` fit
  each return a ggplot) and "a grouped object draws one series per group"
  (2 line-layer groups). Pass.
- **AC2** — "occasions keep their list order when labels sort the other way"
  and "the long-format path preserves its occasion ordering too": a `T2`/`T10`
  fixture keeps `levels(Occasion) == c("T2","T10")` on both construction paths.
  Mutation-verified: replacing the `details$occasions` factoring with a bare
  `factor()` turns it red (`"T10" "T2"` returned). Pass.
- **AC3** — "the displacement branch is continuous across the seam", "each CI
  bound lands on its own estimate's branch", and the built-layer assertion "the
  plotted displacement path is continuous across the seam" (all steps < 90°).
  Fixture non-vacuity is itself fenced by "the fixture really does straddle the
  0/360 seam" (asserts a stored-reversed `d_lci > d_uci` interval exists).
  Mutation-verified: naive branch-offset placement turns the ribbon assertions
  red. Pass.
- **AC4** — "uncertified occasions render hollow and certified ones filled":
  built layer data for the displacement point layer carries shapes {16, 1} with
  exactly one hollow occasion. Pass.
- **AC5** — "an occasion with no location leaves a gap, not a broken tail"
  (gap at the flat occasion, post-gap occasions defined and continuous), "the
  contrast row is dropped, not plotted as a time point" (k rows, not k+1), and
  "na.rm = FALSE names the dropped occasion count" (silent by default; warns
  "Removed 1 row" when opted out). Pass.
- **AC6** — "a non-SSM object is refused", "an SSM object without occasions is
  refused informatively" (names `ssm_analyze_long`), "non-finite and non-scalar
  arguments are refused by name" (`Inf`, `NA`, `0`, length-2 `base_size`; `NA`
  flags), "an unrecognized argument warns rather than passing silently"
  (`chkDots` "disregarded"). Pass.
- **AC7** — `devtools::test()` full suite and `devtools::check(args =
  "--no-manual")` re-run fresh at review: **0 errors / 0 warnings / 0 notes**
  (4m 45s). Two vdiffr baselines recorded under `skip_on_ci()`. Pass.

New-file totals: 25 tests, 57 assertions, 0 failures, 0 skips locally.

### Consistency gate

- `cairn_validate.py`: all checks passed (exit 0).
- `devtools::document()`: no diff.
- `pkgdown::check_pkgdown()`: no problems found (`ssm_plot_trajectory` row
  added to `_pkgdown.yml`).
- NEWS.md: entry added under the development heading, no milestone number.
- No `DESIGN.md` principle changed, so `cairn_impact` was not run.

### Independent review (three lenses + scorer)

- **[O] diff-bug (Opus)** — 3 findings (below).
- **[S] blame-history (Sonnet)** — no findings; verified the M27 unwrap
  expression is reused faithfully, `ssm_certified()` is applied only to profile
  rows (never the contrast row, M15-D1), and `angle_unwrap()`'s NA policy is
  diverged from deliberately and disclosed, not silently.
- **[S] prior-PR-comments (Sonnet)** — clean no-op: every merged PR touching the
  sibling plotting/SSM files reports 0 review comments. This repo reviews
  through cairn, not GitHub PR comments, so this lens has no evidence base here.

Scored by a fresh [S] agent. Two findings scored >= 80 and were actioned; one
scored below threshold and is logged, not actioned.

**F1 (score 95) — FIXED.** `ssm_bound_on_branch()` inverted the ribbon for any
displacement CI whose stored arc exceeds a half-turn. Placing each bound by its
own signed distance from the estimate clamps both into (-180, 180], which cannot
represent such an interval. Reproduced end-to-end with no object mutation: a
zero-amplitude occasion stored `d_est = 253.9, d_lci = 42.6, d_uci = 19.7`
(arc span 337.1 degrees -- displacement essentially unknown) plotted as a
22.9-degree INVERTED band with the estimate outside its own bounds, reading as
the most precise occasion in the series. Silent: no warning, `check()` green.
Replaced by `ssm_interval_on_branch()`, which anchors the lower bound and
derives the upper from `ssm_arc_span()` -- the package's standing convention for
reading a stored pair. A strict generalization: agrees to the digit on narrow,
seam-straddling, and wide-but-centred intervals, so every prior assertion held.
- **F2 (score 85) — FIXED.** The test `expect_true(all(d$uci - d$lci < 180))`
  was not an independent property; F1's clamping guaranteed it arithmetically
  even when the ribbon was inverted, so it baked the defect into the suite. No
  fixture reached the wide-CI regime. Replaced with the real invariant (plotted
  width equals the stored `ssm_arc_span()`) and added a zero-amplitude fixture
  reaching arc span > 180. Teeth proven: reverting to the old formula turns all
  three of the new test's assertions red.
- **F3 (score 45) — LOGGED, not actioned.** `@family visualization functions`
  cross-links the new export only to `plot.circumplex_ci_accuracy()`, since the
  three `ssm_plot_*` siblings carry no `@family` tag -- so the See Also points
  away from the functions it belongs beside. Real but a pre-existing docs gap in
  files outside this milestone's scope; absorbed into the ROADMAP
  infrastructure-refactor candidate for whichever milestone next touches the
  reference index (M34 owns pkgdown/reference organization).

Post-fix verification: full suite **0 failures / 2812 passing**;
`devtools::check(args = "--no-manual")` **0 errors / 0 warnings / 0 notes**;
`cairn_validate` all checks passed. vdiffr baselines unchanged by the fix
(narrow-CI fixtures agree to the digit), confirming only the intended regime
moved.

