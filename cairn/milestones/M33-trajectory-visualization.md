<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M33: Longitudinal trajectory visualization (occasions objects)

- **Status:** review
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
- Exported `ssm_plot_trajectory()` for occasions objects (`ssm_analyze(occasions
  = )`, `ssm_analyze_long()`): a Cartesian ggplot faceting e/x/y/a/d over
  occasions with ribbon CIs and `drop_xy`, in the M31/M32 house conventions
  (`chkDots()`, `is_*()`/`!is.finite()`, `na.rm` parity, `theme_bw()`).
- **Temporal occasion ordering** — factor `Occasion` against
  `details$occasions`; the column is character, so an unfactored discrete scale
  re-sorts alphabetically and flips a `T10`/`T2` pair.
- **Displacement seam handling** — per group series, `angle_unwrap(d_est)` for
  the branch, then each bound placed by its *signed* circular distance from its
  own estimate: bounds are stored wrapped, so a straddler has `d_lci > d_uci`
  (LESSONS M27).
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
      (Group × Occasion × Parameter); info columns stripped by name, `Occasion`
      factored to `details$occasions`, contrast row dropped positionally,
      `Panel` factor from a named label vector. Unit-tested directly.
- [x] **T2** — Displacement branch: per-group `angle_unwrap()` + per-bound
      signed-distance placement, after T1's ordering (the unwrap chain is
      order-dependent) and NA-tolerant across a degenerate occasion.
- [x] **T3** — `ssm_plot_trajectory()`: roxygen, argument validation,
      `chkDots()`, `drop_xy`, grouping series, ribbon/line/point layers,
      certification shape mapping, `facet_wrap(~Panel, drop = FALSE)`.
- [x] **T4** — Tests: seam continuity on a forced-straddle fixture (teeth proven
      by breaking the guarded line); T2/T10 ordering; certification shapes;
      flat-occasion gap + `na.rm` parity; contrast drop; every error branch.
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
- 2026-07-18: minor amendment — Scope compressed (no In/Out item changed, only
  rationale now carried by code comments) to bring the plan-owned body under
  the 150-line cap.

## Decisions

## Review
