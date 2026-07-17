# M25: Longitudinal Build A — occasions API + paired contrasts

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** `m25-occasions-core`

## Goal

Add wide-format `occasions` analyses to `ssm_analyze()` — per-occasion
mean-based profiles (k ≥ 2, crossable with grouping) and the paired
two-occasion contrast through both inference engines — oracle-validated and
output-surface complete, per the binding D-013 spec
(`devel/longitudinal-ssm-spec.md` §§1–2, §7 Build A).

## Scope

**In:**
- `occasions` argument per spec §1.1: named list of same-length blocks, wide
  person-rows, mutually exclusive with `scales` (`stopifnot(is_var(scales))`
  at `R/ssm_analysis.R:208` becomes conditional); k ≥ 2 (plan gate
  2026-07-16), default labels `T1..Tk`.
- Stem-matching cross-occasion alignment validation + one-time positional
  message when no stem is detectable (spec §1.1, the rotation channel).
- Listwise-only for occasions (pairwise + occasions errors with the estimand
  message); dropped-persons count messaged; selection-bias docs caution
  (spec §1.3).
- Composition cells (spec §1.2): occasions × mean-based × {1 group,
  grouping}; occasions × `contrast = TRUE` (exactly 2 occasions, 1 group).
  Contrast order = `names(occasions)` list order (temporal), never
  alphabetical; occasion-major row order; conditional-presence `Occasion`
  column; `details` occasions metadata.
- Bootstrap engine: wide person-rows through the existing row resampler,
  contrast via `param_diff(occ2, occ1)` (spec §2.1). Monte Carlo engine:
  stacked k·p mean vector, sample covariance of stacked person vectors / n,
  joint MVN draws, per-block transform, contrast (spec §2.2).
- Full oracle battery per spec §2.3, incl. the ρ > 0 / Δd ≈ 135° reversal
  cell measuring the conditional efficiency identities, small-n (≈25–50) and
  k = 3 cells, degenerate-dependence invariant (re-paired persons reproduce
  the independent contrast), pre-registered SE-based boot-vs-MC tolerance,
  closed-form paired Δe check.
- Output surface (RR06 R12): `print.circumplex_ssm` + snapshots for occasions
  `details`; `ssm_table()` and `ssm_plot_*()` support or cleanly reject
  occasions objects; `ssm_ci_accuracy()` errors informatively (spec §1.4).
- DESIGN.md: occasions RNG reproducibility row + the oracle-registry pointer
  declaration (spec §6 gap — this is the first build adding a longitudinal
  oracle). CLAUDE.md: occasion-contrast-order clause (M23 review F2 carry).
  NEWS entry incl. the `Occasion` schema note.

**Out:**
- occasions × measures (correlation path) → ROADMAP candidate (spec §1.2).
- occasions × contrast × grouping (difference-of-differences) → ROADMAP
  candidate (spec §1.2).
- Pairwise-deletion occasions semantics → ROADMAP candidate (spec §1.3).
- `ssm_ci_accuracy()` occasions extension → ROADMAP candidate (spec §1.4).
- `ssm_analyze_long()` sugar → ROADMAP candidate (spec §1.1).
- Per-person layer + draws adapter → M26; growth support → M27.

## Acceptance criteria

- [ ] AC1 — Intake contract per spec §1.1/§1.3, test-evidenced: `scales`/
      `occasions` mutual exclusivity; equal-length block validation;
      stem-order mismatch errors naming the offending block; no-stem
      positional message fires (fixtures both ways); pairwise + occasions
      errors with the estimand message; listwise drop count messaged.
- [ ] AC2 — Occasions profiles correct: occasion-major rows within group,
      conditional-presence `Occasion` column, k = 3 supported; on
      complete-case data each occasion's profile row equals the equivalent
      single-`scales` run (exact invariant, fixture-tested).
- [ ] AC3 — Paired contrast is second-listed minus first-listed by
      `names(occasions)` list order (a `T10`/`T2` name pair regression test
      proves alphabetical sorting would flip it), through **both** engines;
      the contrast validation rule admits exactly the (1 group, 0 measures,
      2 occasions) triple and errors on > 2 occasions with contrast.
- [ ] AC4 — Oracle battery per spec §2.3 green: paired-contrast CI coverage
      at nominal (band pre-registered in the script before the run, M19
      precedent) across the named cells (Δd near 0° and ±180°, truths
      straddling 0/360, small-n ≈ 25–50, k = 3, and the ρ > 0 / Δd ≈ 135°
      reversal cell); measured efficiency matches the conditional identities
      (Δe: ρ_e sign; Δa/Δd: ∇g₂ᵀC∇g₁, reversal observed past 90°);
      degenerate-dependence invariant; closed-form paired Δe agreement;
      boot-vs-MC within the pre-registered tolerance. Results rds + seeded
      regeneration script committed as `devel/m25-*` (level-indexed seeds).
      ≥ 2 independent oracle types per numeric result.
- [ ] AC5 — CLAUDE.md boundary battery for the new paths: contrast near
      ±180°, CIs straddling 0°/360° (wrap asserted), flat/zero-variance
      occasion, one occasion degenerate — tested on both engines.
- [ ] AC6 — Output surface: print snapshots cover occasions `details`;
      `ssm_table()` and each `ssm_plot_*()` either supports occasions
      objects or rejects with an informative error (tested);
      `ssm_ci_accuracy()` errors informatively on occasions objects.
- [ ] AC7 — Docs/NEWS carry only the conditional efficiency statement (grep
      evidence: no unconditional "paired is narrower" anywhere), plus the
      selection-bias and paired-interpretability cautions; DESIGN.md gains
      the occasions RNG row + oracle-registry pointer; CLAUDE.md gains the
      occasion-order clause; `devtools::check()` clean
      (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4, T5, T6
- AC5 → T4, T5
- AC6 → T7
- AC7 → T8

## Tasks

- [x] **T1** — Intake/validation layer, tests first: `occasions` parsing,
      mutual exclusivity, block checks, stem-matching alignment (+ no-stem
      message), listwise-only enforcement + drop-count message
      (`R/ssm_analysis.R:197-208`, `is_*()` helpers).
- [x] **T2** — Scoring + result assembly: per-occasion per-group scoring,
      `build_result_labels()` occasion dimension (`R/ssm_analysis.R:301`),
      occasion-major order, conditional `Occasion` column, `details`
      metadata; exact single-`scales` equivalence fixture.
- [x] **T3** — Contrast rule extension + list-order convention (T10/T2
      regression; `ssm_by_group()` slice `R/ssm_bootstrap.R:162` reached
      only in the 2-occasion/1-group cell).
- [x] **T4** — Bootstrap engine path (existing row resampler; `param_diff`
      contrast; quantile/branch machinery untouched) + its boundary
      regressions.
- [x] **T5** — MC engine: stacked k·p covariance, joint draws, per-block
      transform, contrast (`R/ssm_montecarlo.R:119-149`) + its boundary
      regressions; docs note n_g ≫ k·p.
- [ ] **T6** — Coverage oracle: seeded script (smoke-first, level-indexed
      seeds, pre-registered band/tolerances written before the run), full
      run, committed `devel/m25-*` rds + analysis; degenerate-dependence
      invariant, closed-form Δe, boot-vs-MC tolerance tests into testthat
      where deterministic.
- [ ] **T7** — Output surfaces: print/snapshots, `ssm_table()`,
      `ssm_plot_circle/curve/contrast()` support-or-reject,
      `ssm_ci_accuracy()` informative error.
- [ ] **T8** — Docs + NEWS + DESIGN.md (RNG row, oracle-registry pointer) +
      CLAUDE.md occasion-order clause; full `devtools::check()`.

## Work log

- 2026-07-16: created by /milestone-plan (Build A of the D-013 contract;
  promoted from the "Longitudinal SSM build family" candidate row). Plan-gate
  decisions: full k ≥ 2 ships (contrast stays exactly-2); all builds precede
  M7 in work order (priority high).
- 2026-07-16: T1 done — occasions intake validation (mutual exclusivity,
  block shape, labels, listwise-only estimand error, stem-matching alignment
  incl. rotation error + positional-fallback message path) + contrast rule
  extension in `ssm_analyze`; 12 new tests; full suite green (2141 pass).
  Implement question gate skipped: plan/spec pin all substantive choices.
- 2026-07-16: T2 done — `occ_scores()` + `ssm_analyze_occasions()` +
  `build_result_labels()` occasion branch (conditional Occasion column,
  group-major/occasion-minor); AC2 exact single-`scales` equivalence
  fixtures (k = 3, and per-cell under grouping); drop-count message +
  no-stem positional message tests. Includes the MC `occ_k` plumbing
  (stacked-draw split), engine-validated at T5. Suite green (2164).
- 2026-07-16: T3 done — contrast conventions locked: second-listed minus
  first-listed with the T10/T2 alphabetical-flip regression; hand-computed
  Δe exactness; Δd = angle_dist of the profile rows; composition errors
  (k = 3, grouping x contrast); both engines agree on point estimates and
  cover the construction truths.
- 2026-07-16: T4+T5 done (one commit — engines were built in T2; these are
  their boundary batteries): pole-straddling CI wraps (lci > uci), ±175°
  contrast keeps sign/branch/coverage, flat occasion → NA d + warning with
  healthy occasion untouched, k = 3 engine agreement — all parametrized
  over both engines. No engine fixes needed. n_g ≫ k·p docs note pending
  in T8 roxygen.

## Decisions

## Review
