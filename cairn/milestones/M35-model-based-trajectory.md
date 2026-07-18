<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M35: Model-based trajectory plotting (`ssm_draws()` tables)

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M33
- **Principles touched:** — (works under the CLAUDE.md angle invariants: LM=360,
  displacement seam unwrap)
- **Branch/PR:** `m35-model-based-trajectory`

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

- [ ] The new entry point returns a ggplot from a per-time-point trajectory
      table with a continuous time axis; happy-path test, plus a table carrying
      only `a_*`/`d_*` (no `e_*`/`x_*`/`y_*`) yielding only the panels it can fill.
- [ ] The seam and certification behavior are **shared with M33, not duplicated**:
      a seam-straddling model trajectory renders continuous and uncertified time
      points render hollow, asserted at the data level, and the displacement
      unwrap has exactly one definition in `R/` (asserted by inspection at review,
      recorded as evidence). *(source: LESSONS M27; D-007.)*
- [ ] Malformed input errors informatively and specifically: missing required
      columns names which; a non-numeric or single-valued time column; a
      non-`data.frame` input. Each branch fired by a test.
- [ ] `vignettes/growth-ssm-analysis.Rmd` builds under `devtools::check()` using
      the new function, with no inline `rel()`/`%% 360` expression left in the
      vignette (grep-asserted), and its displacement figure marks uncertified
      waves.
- [ ] `devtools::test()` and `devtools::check()` clean (0 errors / 0 warnings /
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
- [ ] **T3** — Tests: continuous-axis happy path, partial-column table,
      seam continuity, certification shapes, every error branch.
- [ ] **T4** — Rewrite the growth vignette figure chunk; NEWS entry;
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
  stayed green with zero test edits (the fence held literally). Full suite
  0 failures / 2812 passes.
- 2026-07-18: T2 — pre-implementation gate settled all three open choices
  (S3 generic; explicit `time =`; absent `certified` shows no claim). Built the
  generic + `circumplex_ssm`/`data.frame`/`default` methods and the table
  validator; see M35-D1/M35-D2. Code landed before its tests (T3) — the
  validation surface was settled against a working prototype; all nine error
  branches exercised by hand before commit, then pinned in T3.

## Decisions

### M35-D1 (2026-07-18): the table entry point is an S3 generic, not a constructor

`ssm_plot_trajectory()` becomes an S3 generic with `circumplex_ssm`,
`data.frame`, and `default` methods, rather than shipping an `ssm_trajectory()`
constructor that validates a table into a class the existing function dispatches
on. Settled at the pre-implementation gate (the plan tagged T2 `RB tripwire:
irreversible-api`); Jeff chose the generic, no Fable escalation. Rationale: one
entry point users already know and no new user-facing concept, against a class
with no second use today (no print/summary planned) — the constructor would be
ceremony charged to every caller. Reversible-ish in one direction only: a
constructor could still be added later as an alternative input, but the
`data.frame` method could not be withdrawn without a deprecation cycle.
Consequence: the first formal is renamed `ssm_object` -> `x` (generic
dispatch); safe because M33 is unreleased and no caller passes it by name.

### M35-D2 (2026-07-18): new error conditions use base `stop()`, matching the package

The `r-package` profile's `test-doctrine` slot says new user-facing conditions
use `cli::cli_abort()`/rlang. This milestone uses base `stop(..., call. = FALSE)`
instead: `cli` is not in DESCRIPTION Imports, so adopting it is a dependency
change needing its own question gate and D-entry, and the package has zero
`cli_abort()` call sites — all existing conditions are base `stop()`.
CLAUDE.md's "Base R + minimal deps" and "match existing code style" rules bind
here. A package-wide migration to cli, if wanted, is its own milestone.

## Review
