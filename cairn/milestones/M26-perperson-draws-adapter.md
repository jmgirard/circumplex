# M26: Longitudinal Build B — per-person layer + draws adapter + Bayesian vignette

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Ship the intraindividual descriptive layer (per-person SSM scoring with
circular summaries) and the `ssm_draws()` adapter (parameter- and
profile-draw shapes), finished with the precomputed brms vignette, per the
binding D-013 spec (`devel/longitudinal-ssm-spec.md` §§3, 5, §7 Build B).

## Scope

**In:**
- Exported per-person scoring wrapper (spec §3.2 shape:
  `ssm_parameters_id(data, scales, angles, id = NULL)`; **name final at the
  pre-implementation gate** — an exported name is permanent
  (RB tripwire: irreversible-api)), returning the per-person parameter table
  (e_i, x_i, y_i, a_i, d_i) with the standing degenerate-profile NA
  semantics and NA counts reported as a column, never a silent drop.
- Group-level summaries of per-person parameters: circular mean + resultant
  length for d (never arithmetic angle means), NA d_i stripped with count
  reported before circular aggregation (`angle_mean()` has no `na.rm`);
  aggregation caveat documented (spec §3.1/§3.3).
- `ssm_draws(draws, angles = NULL, interval = 0.95)` per spec §5.1: shape A
  (parameter draws, columns (e, x, y)) and shape B (profile draws + angles);
  dispatch rules incl. the **explicit `type` requirement in the
  ncol = 3 / no-angles cell**; column-mapping message when colnames are
  unrecognized; summaries via the existing `ssm_replicate_intervals()` path
  with d draws classed `circumplex_radian`; the four managed leaks
  (`replicate_label = "posterior draws"`, per-parameter NA semantics
  documented, `t0` = the adapter's own point summaries, shape A synthesizes
  the 6-column `ssm_param_names()` layout with `fit = NA`); point summaries
  = medians + circular mean for d, coherence caveat documented.
- Oracles per spec §3.3 + §5.5 (detailed in the ACs).
- `bayesian_ssm.Rmd`: precomputed (brms never runs on CRAN builders); brms →
  `Suggests` (pre-cleared at the 2026-07-16 plan gate; **D-entry recorded in
  this milestone**); the devel sketch treated as untrusted (its line-43
  derivation comment has swapped atan2 args); mapping derived fresh;
  prior-predictive simulation exhibiting the Rayleigh-shaped induced prior
  on a (spec §5.2/§5.3).
- NEWS + docs: induced-prior footnote, mean-resultant ≤ mean-amplitude
  caveat, circular-mean-of-d_i ≠ group-profile-d documentation.

**Out:**
- Pooled/shrunken per-person estimates → Stan-companion stay-out criteria
  (spec §5.4; ROADMAP candidate).
- Growth helpers, vignette, and the per-t amplitude certification caution →
  M27 (the caution is exercised per-t there; any small adapter-internal
  helper it needs lands with it).
- occasions API → M25.

## Acceptance criteria

- [ ] AC1 — Per-person wrapper reproduces hand-computed closed-form fixtures
      (arithmetic in test comments) for 2–3 synthetic persons incl. one
      exactly-flat and one pure-second-harmonic person (NA semantics); NA
      rate reported as a column.
- [ ] AC2 — Invariants per spec §3.3, test-evidenced on heterogeneous data:
      linearity (mean of per-person (e_i, x_i, y_i) equals the group path's
      (e, x, y) exactly); Jensen (group amplitude ≤ mean per-person
      amplitude, strict under directional dispersion); identical-profiles
      reproduction; circular mean recomputed by hand in the test (atan2 of
      summed sines/cosines, never via `angle_mean()`); the differ-fixture
      (circular mean of d_i ≠ displacement of the group mean profile on a
      heterogeneous sample) as the anti-confusion regression.
- [ ] AC3 — Dispatch contract per spec §5.1, each branch tested: angles
      supplied → shape B requiring `ncol(draws) == length(angles)`;
      `angles = NULL` + ncol ≠ 3 → error naming both shapes;
      `angles = NULL` + ncol == 3 → explicit `type` required (error naming
      the ambiguity); unrecognized colnames → assumed-mapping message.
- [ ] AC4 — Adapter oracles per spec §5.5: feeding the bootstrap replicate
      matrix of an existing `ssm_analyze()` run reproduces that run's
      intervals **exactly**; shape B equals shape A applied to the per-row
      (e, x, y) (exact); hand-computed 4-row fixtures incl. a pole-straddling
      draw pair (CI must wrap) and an all-flat matrix (all-NA contract);
      exact-pole summaries report 360 (D-003/M20).
- [ ] AC5 — Summary honesty, snapshot-evidenced: warnings say "posterior
      draws" (never "bootstrap resamples"); `t0` is the adapter's own point
      summaries; a summarized by median, d by circular mean, with the
      coherence caveat in the docs.
- [ ] AC6 — `bayesian_ssm.Rmd` builds under `devtools::check()` precomputed;
      brms in `Suggests` with the D-entry recorded; the vignette contains
      the prior-predictive Rayleigh exhibit and a fresh (not sketch-copied)
      atan2 derivation pinned by a known-direction fixture.
- [ ] AC7 — NEWS documents both features; `devtools::check()` clean
      (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T3, T5
- AC5 → T4
- AC6 → T6
- AC7 → T7

## Tasks

- [ ] **T1** — Per-person wrapper, tests first (closed-form fixtures);
      exported name settled at the pre-implementation gate
      (RB tripwire: irreversible-api). `ssm_score()`/`ssm_parameters()`
      (`R/ssm_analysis.R:779,854`) are the building blocks.
- [ ] **T2** — Summary layer: circular stats, NA stripping + counts, the
      §3.3 invariant suite (linearity, Jensen, hand-recomputed circular
      mean, differ-fixture).
- [ ] **T3** — `ssm_draws()` core: shape A/B transforms + dispatch rules,
      tests first for every dispatch branch.
- [ ] **T4** — Adapter summary path through `ssm_replicate_intervals()`
      (`R/ssm_bootstrap.R:136-144`) with the managed leaks; print/summary
      snapshots.
- [ ] **T5** — Adapter oracle suite: bootstrap-replicate exact reproduction,
      shape A/B consistency, 4-row fixtures, boundary regressions
      (pole-straddle wrap, all-flat, pole = 360).
- [ ] **T6** — `bayesian_ssm.Rmd` precomputed + brms `Suggests` + D-entry
      (dependency gate satisfied at the 2026-07-16 plan gate); untrusted-
      sketch note honored; prior-predictive exhibit.
- [ ] **T7** — Docs + NEWS; full `devtools::check()`.

## Work log

- 2026-07-16: created by /milestone-plan (Build B of the D-013 contract;
  promoted from the "Longitudinal SSM build family" candidate row). brms
  `Suggests` pre-cleared at the plan gate (D-entry lands with T6).

## Decisions

## Review
