# M61: Single-item scale positions for `axes_reliability()` — dropping ζ1

- **Status:** blocked
- **Priority:** normal
- **Depends on:** M60
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m61-axes-reliability-single-item`

## Goal

Let `axes_reliability()` estimate an instrument with one item per scale position
by dropping the scale-specificity component ζ1, as Strack's types e and f do.

## Scope

**In:**
- A scale-specificity switch threaded through `axes_syntax()`
  (`R/axes_reliability.R:84,100-102,111-115,122-124`), `axes_fit()` and
  `axes_fit_cormat()`. The rule: ζ1 is dropped exactly when **no** same-scale
  item pair exists anywhere — the condition under which it is unidentified and
  under which the OLS shadow's same-scale design column is empty. A mixed
  configuration with at least one multi-item scale still fits ζ1.
- The downstream generalizations that rule forces: a two-column OLS shadow
  (`:137-147`, currently singular without same-scale pairs), the hardcoded
  `SS1` extractions (`:644`, `:741`), the boundary test (`:650`), and the fixed
  four-row components frame (`:737-743`).
- Nunnally–Bernstein returns `NA` with a stated reason on the ζ1-dropped path —
  α is undefined at one item per scale and `cronbach_alpha()` (`:229-233`)
  divides by `m - 1`. Strack Table 3 leaves col 14 blank for types e and f,
  corroborating.
- Fractional item_n end to end (SYMLOG's 8.67).
- Oracles: the six single-item Table 3 rows (Layer A) plus population /
  synthetic / cross-engine cells at a single-item configuration (Layer B).
- `details` records whether ζ1 was fitted; roxygen, `man/`, vignette, NEWS.

**Out:**
- Blockwise ζ2 (Strack type d) → ROADMAP candidate row.
- FIML on items → ROADMAP candidate row.
- Unequal spacing / quasi-circumplex — stays refused (RR09 §4).
- The equal-spacing relaxation itself → M60, which this depends on because both
  rewrite the same refusal block.

## Acceptance criteria

- [ ] AC1: `axes_reliability()` estimates a configuration with exactly one item
      per scale position — no error, finite per-axis reliability, a three-row
      components frame with no scale-specificity row, and `details` recording
      that ζ1 was not fitted.
- [ ] AC2: the drop rule is "no same-scale item pair exists"; a mixed
      configuration carrying at least one multi-item scale still fits ζ1. Both
      branches tested.
- [ ] AC3: Nunnally–Bernstein is `NA` with a stated reason on the ζ1-dropped
      path — never `NaN`, never a number — surfaced in print/summary on the
      house pattern the cormat path already uses (`:723-725`).
- [ ] AC4: fractional item_n works end to end (8.67), and the OLS shadow returns
      a two-component seed instead of erroring when the same-scale design column
      is empty.
- [ ] AC5: Layer A — all six single-item Strack (2013) Table 3 rows reproduce
      within ±.01: COC %axes 2.8 / 3.2 / 1.9 at item_n 8 → .19 / .21 / .13, and
      SYMLOG 27.2 / 30.3 / 28.1 at item_n 8.67 → .76 / .79 / .77 (p. 7).
- [ ] AC6: Layer B at a single-item configuration — population-matrix recovery
      exact to numerical tolerance with no ζ1 term in either the generating Σ or
      the model, synthetic recovery, and cross-engine lavaan/OpenMx agreement.
- [ ] AC7: roxygen, regenerated `man/`, vignette and NEWS state the drop rule
      and the N–B unavailability; the six oracle rows are banked in
      `cairn/references/strack2013.md`.
- [ ] AC8: `devtools::check()` clean and the PDF manual actually built
      (`R CMD Rd2pdf`; `check()` skips it by default — M7/M57).

## Coverage

- AC1 → T1, T2, T4, T6
- AC2 → T3, T6
- AC3 → T5
- AC4 → T3, T7
- AC5 → T7
- AC6 → T8
- AC7 → T9
- AC8 → T9

## Tasks

- [ ] T1: tests first — a single-item configuration currently errors at the
      ≥ 2-items refusal (`R/axes_reliability.R:507-510`); pin that fence and
      pin `cronbach_alpha()`'s `NaN` at `m = 1`.
- [ ] T2: thread the scale-specificity switch through `axes_syntax()` — latent
      names (`:84`), measurement block (`:111-115`), variance block
      (`:122-124`), and the `st("zeta1")` start lookup (`:100-102`), which must
      emit no modifier rather than a `NULL` start.
- [ ] T3: two-column OLS shadow (`:137-147`) when no same-scale pair exists;
      the returned seed drops `zeta1` (`:146`, consumed at `:617`, stored at
      `:755`).
- [ ] T4: generalize the `SS1` extractions (`:644`, `:741`), the boundary test
      (`:650`) and the four-row components frame (`:737-743`) to a
      variable-length component set.
- [ ] T5: N–B `NA`-with-reason on the ζ1-dropped path (`:707-725`) plus the
      print/summary message (`R/axes_reliability_oop.R:102-110`).
- [ ] T6: relax the ≥ 2-items refusal to the drop rule; keep an informative
      error for anything still unsupported.
- [ ] T7: bank the six COC/SYMLOG rows in `cairn/references/strack2013.md` and
      add the ±.01 sweep test, which is also the fractional-item_n fence.
- [ ] T8: Layer-B cells at a single-item configuration — population Σ without
      the ζ1 block (`:201-208` takes ζ1 as required; needs a no-ζ1 path),
      synthetic recovery, and the OpenMx cross-check without its `zeta1*B` term
      (`tests/testthat/test-axes-reliability.R:234-311`).
- [ ] T9: roxygen, `man/`, vignette, NEWS; full check plus the PDF manual.

## Work log

- 2026-07-25: created by /milestone-plan.
- 2026-07-26: implementation question gate. Drop switch is *inferred* from the item map by one shared predicate, not threaded as an argument through `axes_syntax()`/`axes_fit()`/`axes_fit_cormat()` — the model and the results table then cannot disagree about whether ζ1 was fitted. N–B `NA` breadth → M61-D1. AC4's fractional-item_n question escalated to a Review Brief (Jeff's call at the gate).
- 2026-07-26: read Strack Table 3 p. 7 in the shelf PDF for the six single-item rows. Types e/f print `—` for BOTH scale- and block-specificity (the paper drops ζ1 itself), col 14 is blank for all six, and all six component rows sum to exactly 100.0 — so they carry a sum guard the type-c row could not. AC4's `8.67` is unreachable end to end: at equal spacing with one item per position per-axis item_n is exactly k/2, so 8.67 needs k = 17.34; Table 1 lists SYMLOG with 26 items and no scales, and 26/3 = 8.667 (SYMLOG is a three-axis system). COC checks out: 16 items → item_n 8.
- 2026-07-26: blocked on RB11 (`cairn/reviews/RB11-axes-reliability-fractional-item-n.md`) — whether AC4's `8.67` is reachable at all, whether the three SYMLOG Table 3 rows are a legitimate Layer-A oracle, and the replacement wording for AC4.

## Decisions

- **M61-D1 (2026-07-26): the Nunnally–Bernstein `NA` rule is "any scale with
  fewer than 2 items", a superset of AC3's "ζ1-dropped path".** AC3 forbids
  `NaN`, and `cronbach_alpha()` divides by `m - 1`, so a *mixed* configuration
  (at least one multi-item scale, so ζ1 is still fitted, plus at least one
  single-item scale) would return `NaN` under AC3's literal reading — exactly
  the branch AC2 requires be tested. The broader rule satisfies AC3 strictly and
  closes that hole. Strack corroborates the wider reading: Table 3 col 14 is
  blank for every single-item row, and p. 5 states the formula "was not applied
  for analyzing instruments with a single item per spatial position". Decided at
  the M61 implementation question gate (Jeff).

## Review
