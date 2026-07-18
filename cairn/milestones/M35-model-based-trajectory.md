<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M35: Model-based trajectory plotting (`ssm_draws()` tables)

- **Status:** review
- **Priority:** normal
- **Depends on:** M33
- **Principles touched:** — (works under the CLAUDE.md angle invariants: LM=360,
  displacement seam unwrap)
- **Branch/PR:** `m35-model-based-trajectory` / [#58](https://github.com/jmgirard/circumplex/pull/58)

## Goal

Extend M33's trajectory plot to model-based trajectories assembled from
`ssm_draws()` over a continuous time variable, and retire the hand-rolled figure
in the growth vignette.

## Scope

**In:**
- A second, documented entry point for a **per-time-point trajectory table** —
  a data.frame carrying a time column plus `a_est/a_lci/a_uci` and
  `d_est/d_lci/d_uci` (optionally `e_*`, `x_*`, `y_*`), the shape the growth
  vignette assembles at
  [growth-ssm-analysis.Rmd:171](../../vignettes/growth-ssm-analysis.Rmd). Two
  candidate shapes, settled at implement: an `ssm_plot_trajectory()` S3 generic
  with `circumplex_ssm` + `data.frame` methods, or an `ssm_trajectory()`
  constructor validating the table into a class the existing function dispatches
  on. *(RB tripwire: irreversible-api)*
- **Continuous time axis** (numeric `wave`), as against M33's discrete ordered
  occasions — the axis is the substantive difference between the two paths;
  everything downstream (unwrap, ribbons, certification marking, faceting) is
  M33 machinery reused, not reimplemented.
- Swap the growth vignette figure to the new function, deleting its inline
  `rel()` unwrap and `plot_dat` assembly
  ([growth-ssm-analysis.Rmd:194-214](../../vignettes/growth-ssm-analysis.Rmd)),
  and surface the `certified` column the vignette already computes but cannot
  currently show (Section 5's own caveat).
- Validation of the supplied table: required columns present, time column
  numeric and non-degenerate, bound columns finite where estimates are.

**Out:**
- Occasions objects → M33 (this milestone reuses that code, never forks it).
- On-circle animated/arrow movement paths across occasions → ROADMAP candidate.
- Plotting vignette + pkgdown reorg → M34; any pkgdown row for the new export is
  M34's to place if M34 ships after this.
- Fitting the growth model itself (glmmTMB recipe, D-016) — unchanged.

## Acceptance criteria

- [x] The new entry point returns a ggplot from a per-time-point trajectory
      table with a continuous time axis; happy-path test, plus a table carrying
      only `a_*`/`d_*` (no `e_*`/`x_*`/`y_*`) yielding only the panels it can fill.
- [x] The seam and certification behavior are **shared with M33, not duplicated**:
      a seam-straddling model trajectory renders continuous and uncertified time
      points render hollow, asserted at the data level, and the displacement
      unwrap has exactly one definition in `R/` (asserted by inspection at review,
      recorded as evidence). *(source: LESSONS M27; D-007.)*
- [x] Malformed input errors informatively and specifically: missing required
      columns names which; a non-numeric or single-valued time column; a
      non-`data.frame` input. Each branch fired by a test.
- [x] `vignettes/growth-ssm-analysis.Rmd` builds under `devtools::check()` using
      the new function, with no inline `rel()`/`%% 360` expression left in the
      vignette (grep-asserted), and its displacement figure marks uncertified
      waves.
- [x] `devtools::test()` and `devtools::check()` clean (0 errors / 0 warnings /
      0 notes).

## Coverage

- AC1 → T1, T2
- AC2 → T1, T3
- AC3 → T2, T3
- AC4 → T4
- AC5 → T4

## Tasks

- [x] **T1** — Factor M33's reshape/unwrap/certification internals so both paths
      call one implementation; confirm no behavior change to `ssm_plot_trajectory()`
      on occasions objects (M33's suite stays green as the fence).
- [x] **T2** — Decide and build the entry point (generic + `data.frame` method, or
      `ssm_trajectory()` constructor); validation, roxygen, NAMESPACE.
      *(RB tripwire: irreversible-api — settle at the pre-implementation gate.)*
- [x] **T3** — Tests: continuous-axis happy path, partial-column table,
      seam continuity, certification shapes, every error branch.
- [x] **T4** — Rewrite the growth vignette figure chunk; NEWS entry;
      `devtools::document()`; vdiffr baseline if the figure warrants one
      (delete stale snaps, run under `NOT_CRAN=true` — LESSONS M31); full
      `test()` + `check()`.

## Work log

- 2026-07-18: created by /milestone-plan, split out of M33 at the plan gate —
  the growth vignette figure is built from glmmTMB + `ssm_draws()`, not an
  occasions object, so it needs its own input contract. Extends the D-018
  visualization expansion (M30–M34) with a fifth milestone; M7 gains the
  dependency per the same gate.
- 2026-07-18: T1 — extracted `ssm_trajectory_long()` as the single unwrap /
  certification / melt implementation, parameterized by the time column's
  *name* so the occasions path keeps its `Occasion` column and M33's suite
  stayed green with zero test edits (fence held literally). 0 failures / 2812.
- 2026-07-18: T2 — gate settled all three open choices (S3 generic; explicit
  `time =`; absent `certified` shows no claim). Built the generic + three
  methods and the table validator; see M35-D1/M35-D2. Code landed before its
  tests (T3), branches exercised by hand before commit, then pinned in T3.
- 2026-07-18: T3 — 63 tests in `test-ssm_trajectory_table.R`; suite 0 failures
  / 2875. Teeth proven by mutation: swapping `ssm_interval_on_branch()` for the
  naive M27 per-bound expression turns the new suite red (4) and M33's red (3).
  The first wide-arc fixture had NO teeth (estimate at the arc's centre, where
  clamping agrees by symmetry); moved it off-centre, the diffuse-draws case.
- 2026-07-18: T4 — vignette Section 4 figure is now one `ssm_plot_trajectory()`
  call (inline `rel()`/`%% 360` deleted, grep-clean); new Section 5 figure
  surfaces the `certified` column the vignette computed but could not show.
  NEWS; 3 vdiffr baselines; `document()` no diff; `check()` Status OK (no
  errors, warnings, or notes); `check_pkgdown()` clean.
- 2026-07-18: render-and-inspect pass (LESSONS M33) over all four new figures.
  Real vignette data validates the wide-arc path end to end: Section 5's wave 2
  is uncertified at a_est = 0.028 with a 182.6-degree arc — wider than a
  half-turn, the regime the naive recipe cannot represent — drawn hollow at full
  width. One cosmetic M33-inherited defect out of scope → ROADMAP candidate.
- 2026-07-18: review — 2 findings actioned and fixed on the branch (F1 non-finite
  estimate blanking the series, 93; F3 time column clobbered by a parameter
  column, 94), each with a regression test confirmed red before the fix; 1
  logged below threshold (F2 mixed-NA `certified`, 76). Suite 0 failures / 2886.

## Decisions

### M35-D1 (2026-07-18): the table entry point is an S3 generic, not a constructor

`ssm_plot_trajectory()` becomes an S3 generic with `circumplex_ssm`,
`data.frame`, and `default` methods, rather than an `ssm_trajectory()`
constructor validating a table into a class the function dispatches on. Settled
at the pre-implementation gate (T2 was tagged `RB tripwire: irreversible-api`);
Jeff chose the generic, no Fable escalation. One entry point users already know
and no new user-facing concept, against a class with no second use today
(no print/summary planned). Reversible one way only: a constructor could still
be added later, but the `data.frame` method could not be withdrawn without a
deprecation cycle. First formal renamed `ssm_object` -> `x`; safe because M33
is unreleased and no caller passes it by name.

### M35-D2 (2026-07-18): new error conditions use base `stop()`, matching the package

The `r-package` profile's `test-doctrine` slot asks for `cli::cli_abort()`; this
milestone uses base `stop(..., call. = FALSE)`. `cli` is not in DESCRIPTION
Imports, so adopting it is a dependency change needing its own gate + D-entry,
and the package has zero `cli_abort()` call sites. CLAUDE.md's "Base R + minimal
deps" and "match existing code style" bind; a cli migration is its own milestone.

## Review

Reviewed 2026-07-18. PR [#58](https://github.com/jmgirard/circumplex/pull/58).

### Acceptance-criteria evidence (fresh, by command)

- **AC1** — `ssm_plot_trajectory(traj_table(), time = "wave")` returns a ggplot;
  x scale `inherits(..., "ScaleContinuous")` TRUE and the plot data carries a
  numeric column under the caller's own name. Partial table (`a_*`/`d_*` only)
  yields `levels(Panel) == c("Amplitude", "Displacement")`; adding an `e_*`
  triple adds exactly that panel in canonical order; a full table yields all
  five and `drop_xy = TRUE` drops the coordinate pair.
- **AC2** — `ssm_unwrap_gapped()` (L32) and `ssm_interval_on_branch()` (L58)
  each have exactly one definition in `R/`, and `ssm_trajectory_long()` (L78) is
  their only caller, invoked from exactly two sites (L163 occasions, L351+
  table); grep for the M27 clamping expression `+ 180) %% 360) - 180` finds no
  hit in `R/` outside `angle_unwrap()`'s own successive-difference step
  (`convenience_functions.R:102`, a different computation). Seam continuity and
  span equality asserted at the data level. **Teeth proven by mutation**:
  substituting the naive per-bound expression turns the new suite red (4
  failures) and M33's red (3). `git diff master..HEAD --
  tests/testthat/test-ssm_trajectory.R` is **empty** — M33's suite is an
  untouched fence — and its two vdiffr baselines compare byte-identical.
- **AC3** — all **17** `stop()` branches in `R/ssm_trajectory.R` fired in a
  live battery, 17/17 erroring with distinct, specific messages naming the
  offending column or argument; 32 `expect_error`/`expect_warning` assertions
  in the test file.
- **AC4** — `check()` re-builds the vignette OK (~38s). `grep -rnE "rel\(|%% 360"
  vignettes/` returns nothing. Section 5's new figure marks uncertified waves
  hollow, validated against **real** glmmTMB output: wave 2 is uncertified at
  `a_est = 0.028` with a 182.6-degree displacement arc — wider than a half-turn,
  the regime the naive recipe cannot represent — drawn hollow at full width.
- **AC5** — `devtools::test()` 0 failures / 2886 passes; `devtools::check()`
  Status OK (no errors, warnings, or notes); `document()` no diff;
  `pkgdown::check_pkgdown()` clean.

### Consistency gate

`cairn_validate` all checks passed (one weight-cap FAIL during review at 150
plan-owned lines — cap is strict `< 150` — shed from the Work log, now passing).
No DESIGN principle changed, so `cairn_impact` was skipped. Toolchain slot:
`document()` no diff, no hand-edited generated files, no README.Rmd drift,
pkgdown clean, NEWS entry present, `check()` clean.

### Independent review (three lenses + scorer)

- **[O] diff-bug (Opus)** — 3 findings, all reproduced by the orchestrator.
- **[S] blame-history (Sonnet)** — no findings; independently confirmed M33's
  test file byte-identical and each M33 intent (interval anchoring, the
  T10/T2 occasion factoring, contrast-row drop, `na.rm` warn, `drop = FALSE`,
  ungrouped-black) preserved.
- **[S] prior-PR-comments (Sonnet)** — no prior-PR evidence: 2 PRs touched
  these files (#57, #51), both with 0 review comments, verified against REST
  with quota remaining (so a real zero, not a rate-limit artifact). Matches the
  standing LESSONS note that this lens is a clean no-op in this repo.

**Actioned (scored >= 80), both fixed on the branch with regression tests that
were confirmed red before the fix:**

- **F1 (93) — a non-finite estimate silently blanked the rest of the series.**
  `is.na(Inf)` is FALSE, so an `Inf` estimate slipped past the NA-based
  `ssm_has_location()` predicate, hit `Inf %% 360` -> NaN in the unwrap, and
  `cumsum()` propagated the NaN over every later time point. `d_est = c(350,
  Inf, 2, 8, 12)` blanked waves 1-4 with no error and no warning. Fixed by
  making the estimate check symmetric with the bound check (`!is.finite()`,
  never `is.na()` — LESSONS M32); NaN still reads as missing and leaves a gap.
- **F3 (94) — a time column naming a parameter column was clobbered, not
  refused.** The reserved-name guard listed only the output frame's fixed
  names, so `time = "a_est"` passed validation and was then overwritten by the
  amplitude loop, rendering a meaningless diagonal with no error. Fixed by
  extending the guard to every `<p>_est`/`_lci`/`_uci` column and `certified`.

**Logged below threshold (surfaced, not discarded):** 1 finding.

- **F2 (76) — mixed per-row `NA` in `certified` silently drops that point's
  marker.** `show_cert` handles only the all-NA case; a mixed column maps the
  NA row to `shape = NA`, `geom_point(na.rm = TRUE)` drops the glyph, and the
  line draws straight through. Real and reproduced, but scored below threshold
  as a lost visual cue rather than a corrupted statistic — no numeric value is
  wrong and the ribbon/line for that point are still correct. Not actioned.
