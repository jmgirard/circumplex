<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M33: Longitudinal trajectory visualization (occasions objects)

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Principles touched:** — (works under the CLAUDE.md angle invariants: LM=360,
  displacement seam unwrap, occasion order = list order, never alphabetical)

- **Branch/PR:** `m33-trajectory-viz`

## Goal

Export a Cartesian trajectory plot of SSM parameters across occasions for
occasions objects, with per-occasion confidence bands, correct 0/360 seam
unwrapping, and D-007 certification marking on the displacement panel.

## Scope

**In:**
- A new exported `ssm_plot_trajectory()` for occasions objects — the output of
  `ssm_analyze(occasions = )` and of `ssm_analyze_long()` — faceting the SSM
  parameters (e/x/y/a/d) over occasions with ribbon CIs, `drop_xy` mirroring
  `ssm_plot_contrast()` ([ssm_plot.R:462](../../R/ssm_plot.R)). A plain Cartesian
  ggplot, not a circumplex canvas; house conventions per M31/M32
  (`chkDots()`, `is_*()` + `!is.finite()` guards, `na.rm` opt-in parity, shared
  `ssm_has_location()`/`ssm_has_region()` predicates, `theme_bw()`).
- **Temporal occasion ordering.** `results$Occasion` is character, so a naive
  discrete scale re-sorts alphabetically and flips a `T10`/`T2` pair; the plot
  factors it against `details$occasions` (the canonical order —
  [ssm_analysis.R:319](../../R/ssm_analysis.R),
  [ssm_analyze_long.R:114](../../R/ssm_analyze_long.R)).
- **Displacement seam handling.** Per group series: `angle_unwrap(d_est)` for the
  branch, then place each bound by its *signed* circular distance from its own
  estimate — `((bound - d_est + 180) %% 360) - 180` — because non-contrast bounds
  are each independently wrapped into [0, 360] and a straddling interval has
  `d_lci > d_uci` ([ssm_bootstrap.R:190](../../R/ssm_bootstrap.R); LESSONS M27).
- **Certification marking.** Uncertified occasions (`ssm_certified(a_lci, a_uci)`,
  [ssm_oop.R:132](../../R/ssm_oop.R)) render hollow on the displacement panel —
  the `shape = Structural` idiom of `plot.circumplex_ci_accuracy()`
  ([ssm_ci_oop.R:610](../../R/ssm_ci_oop.R)) — per D-013/RR06's per-t caution.
- **Contrast row dropped**, documented, with users pointed at
  `ssm_plot_contrast()`: it is not a time point and rides the opposite branch
  convention (`circumplex_contrast_radian`, contiguous, may be negative or > 360).
  Detected positionally (`details$contrast` && last row) as the print method does
  ([ssm_oop.R:164](../../R/ssm_oop.R)) — never `ssm_plot_circle()`'s `df[1:2, ]`
  slice, which truncates k>2 and grouped objects.
- Grouping supported: one series per `Group` level.

**Out:**
- Model-based trajectories from an `ssm_draws()` per-time-point table, and the
  `vignettes/growth-ssm-analysis.Rmd` figure swap → **M35** (planned this run;
  that figure comes from glmmTMB + `ssm_draws()`, not an occasions object, so
  it is not this function's input).
- On-circle animated/arrow movement paths across occasions → ROADMAP candidate.
- `coord_circumplex()` `amax`/`center` non-finite guard → stays a ROADMAP
  candidate (M33 is Cartesian and does not touch that file).
- Plotting vignette + pkgdown reorg → M34.

## Acceptance criteria

- [ ] `ssm_plot_trajectory()` returns a ggplot of faceted SSM parameter
      trajectories for both an `ssm_analyze(occasions = )` object and an
      `ssm_analyze_long()` object; happy-path test for each, plus a grouped
      object yielding one series per `Group` level.
- [ ] Occasions plot in **temporal (list) order, never alphabetical**: a fixture
      with occasions named `T2`, `T10` (in that order) is asserted at the data
      level to keep that order on the x scale. An implementation omitting the
      `details$occasions` factoring fails. *(source: CLAUDE.md Statistical
      invariants, "Occasion contrasts: second listed minus first listed".)*
- [ ] A displacement trajectory whose occasions cross the 0/360 seam renders as a
      **continuous** path with its ribbon on the estimate's branch — asserted at
      the data level (`ggplot_build()` layer data), never by eye, because
      `devtools::check()` runs clean on a visually wrong figure. Teeth proven by
      mutation: replacing the signed-distance placement with a naive branch-offset
      or a linear wrap turns the test red (M13 teeth rule). *(source: LESSONS M27;
      [ssm_bootstrap.R:190](../../R/ssm_bootstrap.R).)*
- [ ] An occasion failing `ssm_certified()` renders hollow on the displacement
      panel while a certified one renders filled — asserted at the data level on
      the layer's shape column. *(source: D-007; D-013/RR06 holding 3.)*
- [ ] Degenerate handling: a flat/zero-amplitude occasion (`d_est = NA`) is
      classified through `ssm_has_location()`/`ssm_has_region()`, leaves a gap in
      the series rather than a spurious segment or a broken unwrap chain, and
      obeys `na.rm` parity (default silent; `na.rm = FALSE` warns once naming the
      dropped count). A `contrast = TRUE` object plots k occasion rows, not k+1.
- [ ] Error branches fire with informative messages: a non-`circumplex_ssm`
      object; a `circumplex_ssm` without occasions; a non-finite/non-scalar
      numeric argument (`!is.finite()`, LESSONS M32); an unrecognized `...`
      (`chkDots()` "disregarded" warning).
- [ ] `devtools::test()` and `devtools::check()` clean (0 errors / 0 warnings /
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
      (Group × Occasion × Parameter). Strip info columns **by name**
      (`setdiff(names(x), c("Label","Group","Measure","Occasion"))`, the
      [ssm_plot.R:309](../../R/ssm_plot.R) idiom — never positional); factor
      `Occasion` to `details$occasions`; drop the contrast row positionally; build
      the `Panel` factor from a named label vector as
      `plot.circumplex_ci_accuracy()` does ([ssm_ci_oop.R:582](../../R/ssm_ci_oop.R)).
      Unit-test the helper directly (independent logic).
- [x] **T2** — Displacement branch: per-group `angle_unwrap(d_est)` + per-bound
      signed-distance placement, applied *after* T1's temporal ordering (the
      unwrap chain is order-dependent) and NA-tolerant across a degenerate
      occasion. Reuse the existing idiom, do not retype a fresh `%%`.
- [x] **T3** — `ssm_plot_trajectory()` itself: signature + roxygen, argument
      validation, `chkDots()`, `drop_xy`, grouping series, ribbon/line/point
      layers, certification shape mapping, `facet_wrap(~Panel, drop = FALSE)`,
      `theme_bw()` + bottom legend, `@family visualization functions`.
- [x] **T4** — Tests: seam continuity with a fixture forced to straddle (prove
      teeth by breaking the guarded line); T2/T10 ordering; certification shapes;
      flat-occasion gap + `na.rm` parity; contrast-row drop; every error branch;
      both object constructors and a grouped object.
- [ ] **T5** — `devtools::document()`, NEWS entry, vdiffr baseline (delete stale
      snaps, run under `NOT_CRAN=true` or the comparison auto-skips — LESSONS
      M31), full `test()` + `check()`.

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

## Decisions

## Review
