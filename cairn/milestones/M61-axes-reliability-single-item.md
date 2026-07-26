# M61: Single-item scale positions for `axes_reliability()` — dropping ζ1

- **Status:** review
- **Priority:** normal
- **Depends on:** M60
- **Driving RR:** RR11
- **Principles touched:** —
- **Branch/PR:** `m61-axes-reliability-single-item`

## Goal

Let `axes_reliability()` estimate an instrument with one item per scale position
by dropping the scale-specificity component ζ1, as Strack's types e and f do.

## Scope

**In:** single-item scale positions with ζ1 dropped — dropped exactly when **no** same-scale
item pair exists anywhere (unidentified there, and the OLS shadow's same-scale design column
is empty); a mixed configuration with ≥ 1 multi-item scale still fits ζ1, and the drop is
inferred from the item map rather than threaded as an argument. The generalizations that
forces across `axes_syntax()`, the OLS shadow, the `SS1` extractions, the boundary test and
the components frame; N–B `NA`-with-reason whenever any scale has < 2 items (M61-D1);
fractional item_n end to end (odd single-item k/2 and mixed unequal counts — SYMLOG's 8.67 is
a sphere-model value, formula layer only); the Layer-A and Layer-B oracles; `details`
recording whether ζ1 was fitted; roxygen, `man/`, vignette, NEWS.

**Out:** blockwise ζ2 (type d) and FIML on items → ROADMAP candidate rows; unequal spacing /
quasi-circumplex (RR09 §4) and three-axis sphere designs such as SYMLOG (RR11 Q2) stay
refused; the equal-spacing relaxation itself → M60.

## Acceptance criteria

- [ ] AC1: `axes_reliability()` estimates a configuration with exactly one item per scale
      position — no error, finite per-axis reliability, a three-row components frame with no
      scale-specificity row, and `details` recording that ζ1 was not fitted.
- [ ] AC2: the drop rule is "no same-scale item pair exists"; a mixed configuration carrying
      at least one multi-item scale still fits ζ1. Both branches tested.
- [ ] AC3: Nunnally–Bernstein is `NA` with a stated reason on the ζ1-dropped path — never
      `NaN`, never a number — surfaced in print/summary on the house pattern the cormat path
      already uses (`:824-826`).
- [ ] AC4: fractional item_n works end to end on both reachable shapes: (i) an odd
      all-single-item configuration (k = 5 → item_n 2.5/2.5) on the ζ1-dropped path, and
      (ii) a mixed unequal-count configuration at a non-octant rotation (e.g. angles
      22.5/112.5/202.5/292.5, counts 2/3/2/2 → item_n 4.14645/4.85355) on the ζ1-fitted
      path; in both, the results-frame item_n equals an independently coded analytic
      per-axis Σ nᵢwᵢ² within 1e-8 and is stored as double, and in (ii) the two axes differ.
      SYMLOG's printed 8.67 = 26/3 is a three-axis (sphere-model) value unreachable under
      the input contract and is asserted only at the formula layer (AC5).
- [ ] AC5: Layer A is a formula-layer sweep of the six single-item Table 3 rows, never an
      end-to-end path; its numbers, sum guard and discrimination check are AC11 (BC3).
- [ ] AC6: Layer B at a single-item configuration — population-matrix recovery
      exact to numerical tolerance with no ζ1 term in either the generating Σ or
      the model, synthetic recovery, and cross-engine lavaan/OpenMx agreement.
- [ ] AC7: roxygen, regenerated `man/`, vignette and NEWS state the drop rule
      and the N–B unavailability; the six oracle rows are banked in
      `cairn/references/strack2013.md`.
- [ ] AC8: `devtools::check()` clean and the PDF manual actually built
      (`R CMD Rd2pdf`; `check()` skips it by default — M7/M57).
- [ ] AC9 (BC1): M61's AC4 is replaced verbatim by the two criteria in RR11 Q6 (AC4a,
      AC4b), and the Coverage map reads `AC4a → T7; AC4b → T3`.
- [ ] AC10 (BC2): no test anywhere in M61 calls `axes_reliability()` (either input path)
      with a configuration presented as SYMLOG or asserted to yield per-axis item_n 8.67;
      the three SYMLOG Table 3 rows enter the suite only through direct
      `axis_reliability_sb()` calls.
- [ ] AC11 (BC3): the Layer-A sweep asserts, each within ±.01: SB(.028, 8) → .19,
      SB(.032, 8) → .21, SB(.019, 8) → .13, SB(.272, 8.67) → .76, SB(.303, 8.67) → .79,
      SB(.281, 8.67) → .77; asserts each of the six rows' printed components sum to 100.0
      within ±.05; and asserts the three SYMLOG rows each miss their printed reliability by
      MORE than .01 at item_n 32.
- [ ] AC12 (BC4): the end-to-end fixtures of AC4a compare the results-frame `item_n`
      against an analytic per-axis sum coded independently in the test (not via
      `axis_item_n()`), with `expect_equal(tolerance = 1e-8)`, assert `is.double()` on the
      frame column, and on the mixed fixture assert the two axes' item_n differ; no new
      `expect_identical()` on any non-octant item_n, and the existing octant-exactness
      assertions are not weakened.
- [ ] AC13 (BC5): `cairn/references/strack2013.md` banks the six type-e/f rows carrying
      (i) the sphere standing fact quoting pp. 2, 5, and 9, (ii) the `8.67 = 26/3`
      identity, and (iii) a dated (`— observed YYYY-MM-DD`) observation that the
      configuration is unreachable under the accepted input contract (citing D-031); the
      sweep test carries the never-promote comment; the `axes_reliability()` roxygen states
      the two-dimensional scope naming spherical designs (SYMLOG) as out of scope.
- [ ] AC14: the OLS shadow returns a two-component seed instead of erroring when the
      same-scale design column is empty.

**Deviations from RR11**

| BC | Departure | Why |
|---|---|---|
| BC1 | The two split criteria are labelled `AC4` and `AC14`, and Coverage reads `AC4 → T7; AC14 → T3`, not `AC4a`/`AC4b` | `cairn_validate`'s coverage check counts AC checkboxes positionally, so suffixed labels red the gate (M107). Substance unchanged: AC4 is RR11 Q6's first criterion verbatim and AC14 its second verbatim, mapped to T7 and T3 respectively. RR11's own cross-references to "AC4a"/"AC4b" are left verbatim inside the ingested BC text. |

## Coverage

- AC1 → T1, T2, T4, T6
- AC2 → T3, T6
- AC3 → T5
- AC4 → T7
- AC5 → T7
- AC6 → T8
- AC7 → T9
- AC8 → T9
- AC9 → T7, T9
- AC10 → T7
- AC11 → T7
- AC12 → T7
- AC13 → T7, T9
- AC14 → T3

## Tasks

Line refs are post-M60: `R/axes_reliability.R`, or `test-` = `tests/testthat/test-axes-reliability.R`.

- [x] T1: tests first — a single-item configuration currently errors at the ≥ 2-items refusal (`:609`); pin that fence and `cronbach_alpha()`'s `NaN` at `m = 1`.
- [x] T2: the drop predicate + `axes_syntax()` (`:125-172`) — latent names, measurement and variance blocks, and the `st()` start lookup, which must emit no modifier rather than a `NULL` start.
- [x] T3: two-column OLS shadow (`:182-192`) when no same-scale pair exists; the seed drops `zeta1` (consumed `:718`, stored `:856`).
- [x] T4: generalize the `SS1` extractions (`:745`, `:842`), the boundary test (`:751`) and the four-row components frame (`:838-844`) to a variable-length component set.
- [x] T5: N–B `NA`-with-reason (`:802-826`) plus the print/summary message (`R/axes_reliability_oop.R:102-110`).
- [x] T6: relax the ≥ 2-items refusal to the drop rule; keep an informative error for anything still unsupported.
- [x] T7: bank the six COC/SYMLOG rows in `cairn/references/strack2013.md`; add the ±.01 formula-layer sweep plus the AC4 end-to-end fractional-item_n fence (odd-k single-item and mixed unequal-count fixtures).
- [x] T8: Layer-B cells at a single-item configuration, incl. a k = 5 cell (item_n 2.5) — population Σ with no ζ1 term (`axes_population_cor()` at `n_items = 1` puts the ζ1 block on the diagonal, so `zeta1 = 0` suffices), synthetic recovery, and OpenMx without its `zeta1*B` term (`test-:247-315`).
- [x] T9: roxygen (incl. the two-dimensional scope sentence), `man/`, vignette, NEWS; fix the two SYMLOG-shape comment mislabels (`:73-75`, `test-:1166-1168`); full check plus the PDF manual.

## Work log

- 2026-07-25: created by /milestone-plan.
- 2026-07-26: implementation question gate. Drop switch is *inferred* from the item map by one shared predicate, not threaded as an argument through `axes_syntax()`/`axes_fit()`/`axes_fit_cormat()` — the model and the results table then cannot disagree about whether ζ1 was fitted. N–B `NA` breadth → M61-D1. AC4's fractional-item_n question escalated to a Review Brief (Jeff's call at the gate).
- 2026-07-26: read Strack Table 3 p. 7 in the shelf PDF for the six single-item rows. Types e/f print `—` for BOTH scale- and block-specificity (the paper drops ζ1 itself), col 14 is blank for all six, and all six component rows sum to exactly 100.0 — so they carry a sum guard the type-c row could not. AC4's `8.67` is unreachable end to end: at equal spacing with one item per position per-axis item_n is exactly k/2, so 8.67 needs k = 17.34; Table 1 lists SYMLOG with 26 items and no scales, and 26/3 = 8.667 (SYMLOG is a three-axis system). COC checks out: 16 items → item_n 8.
- 2026-07-26: blocked on RB11 (`cairn/reviews/RB11-axes-reliability-fractional-item-n.md`) — whether AC4's `8.67` is reachable at all, whether the three SYMLOG Table 3 rows are a legitimate Layer-A oracle, and the replacement wording for AC4.

- 2026-07-26: ingested RR11 (→ M61-D2). Applied recs 1–5 (AC4 → AC4a/AC4b, AC5 re-scoped to the formula layer, Scope and T7 reworded, BC1–BC5 ingested verbatim as AC9–AC13 with Coverage lines, `Driving RR: RR11`) and both "consider" recs (rec 6 → a k = 5 population cell in T8; rec 7 → the two SYMLOG-shape comment mislabels folded into T9). Rejected as advised: rec 8 (SYMLOG SEm cross-checks — the Self row misses by ~.04) and rec 9 (an 8.67-tuned end-to-end fixture). RR11 Beyond 2 (ξ1 ≥ 1 → NaN SEm) absorbed into the existing ROADMAP infrastructure-refactor candidate row rather than M61's scope, per RR11; Beyond 3 (item_n print width) and Beyond 4 (the paper's 28-vs-29 subsample quirk) rejected as cosmetic / not this repo's. Re-verified RR11's arithmetic independently in R before ingesting: k = 5 item_n measures 2.4999999999999996, the mixed fixture 4.1464466/4.8535534, `2·1 + 8·½ + 8·⅓ = 26/3` exactly, all six SB rows within ±.005, and the three sphere quotes are on pp. 2, 5, 9. No RR11 finding contradicts a standing D-entry (D-031 width and RR09 §4 both stand), so nothing is superseded.
- 2026-07-26: RB11/RR11 archived; AC block compressed in one pass to fit the ingested criteria — Scope condensed and Tasks re-pointed to post-M60 line numbers (the plan's refs were pre-M60 and stale). Plan-owned body 149/149.

- 2026-07-26: T1 done — two characterization tests pin `cronbach_alpha()`'s `NaN` at `m = 1` (the arithmetic M61-D1 rests on) and pin that the ≥ 2-items line is the *only* gate refusing a 16-position single-item COC-shaped set, so T6's relaxation shows up in the diff. `devtools::test()` clean.

- 2026-07-26: T2 done — `axes_fits_zeta1()` (any scale with ≥ 2 items) is the single source of the drop decision; `axes_syntax()` omits the SS latents and the `zeta1` label on the all-single-item map and keeps them for a mixed one, including the single-item scales' own SS latents, which the shared label identifies. The `st()` start lookup now tests the key by name — a two-element seed emits no modifier rather than a subscript error. Full `devtools::test()` clean (3772 pass, 0 fail).

- 2026-07-26: T3 done — `axes_ols_shadow()` drops its same-scale column when that indicator is identically zero off the diagonal, returning a two-element `(xi2, xi1)` seed instead of failing in `qr.solve()`; both it and `axes_fits_zeta1()` read the item map, so seed and model parameter set cannot disagree. Recovery on the single-item population matrix is exact to 1e-10, and a test pins that zeta1 is genuinely unrecoverable there (any zeta1 yields the identical matrix). Axes test file clean (622 pass).

- 2026-07-26: T4 + T6 done. One checkpoint (minor reorder — T4's variable-length component set is only testable end to end once T6 relaxes the refusal, so they ship together). Component extraction reads `axes_fits_zeta1()` rather than probing for an `SS1` row; the boundary test drops its zeta1 term instead of defaulting it (a `logical(0)` in `||` is an error in R >= 4.3); the components frame is assembled from a row list so a dropped component leaves NO row rather than an NA one; `details$zeta1_fitted` records the drop. The refusal is now "every scale needs >= 1 item", naming the offending scales. Three pre-existing tests that pinned the old ">= 2 items" contract were updated to the new one — that contract change is the milestone's point, and T1's characterization test is what makes it visible in the diff.
- 2026-07-26: T7 banking half done — the six type-e/f rows are banked in `cairn/references/strack2013.md` with the sphere block (pp. 2, 5, 9 quotes, the 26/3 identity, and the dated unreachability observation citing D-031), and the stale "not banked, no verification claim" sentence in the Provenance block is struck through and superseded in place. Two channels actually run on p. 7: the `pdftotext -layout` text layer and a 200-dpi `pdftoppm` page-image render, agreeing on all six rows digit-for-digit including the `—` specificity cells and the blank N–B column.
- 2026-07-26: full `devtools::test()` clean after T4/T6 — 3799 pass, 0 fail (3772 before M61; the 4 warnings are pre-existing and outside the axes file). T4/T6 ticked.
- 2026-07-26: T5 done (full `devtools::test()` clean, 3814 pass, 0 fail). N–B reports `NA` with a `details$nb_reason` of `"cormat"` or `"single_item"`, the latter fired by *any* scale with fewer than two items — so the mixed map, where ζ1 is still fitted, no longer leaks alpha's `NaN`. print() carries a distinct note per reason and falls back to the old cormat note for pre-M61 objects. Tests assert NOT-NaN separately from NA, since `is.na(NaN)` is TRUE and the criterion's "never NaN" clause would otherwise go untested.
- 2026-07-26: T7 done (full `devtools::test()` clean, 3829 pass, 0 fail). Formula-layer sweep of all six type-e/f rows within ±.01, a ±.05 component-sum guard (all six sum to 100.0), and a discrimination check at the DISTANT item_n 32 — RR11 measured that ±.01 cannot separate 8.67 from 8.5 (≈.0035 apart), so a near-miss check would be worthless. The SYMLOG block carries the never-promote comment. End-to-end fractional fixtures: k = 5 single-item (item_n 2.5/2.5, ζ1 dropped) and 22.5°-rotated counts 2/3/2/2 (item_n 4.1464466/4.8535534, ζ1 fitted, axes differ), both compared against an analytic sum coded independently of `axis_item_n()` at 1e-8 with an `is.double()` check. No new `expect_identical()` on any non-octant item_n; the four octant-exactness assertions are untouched.
- 2026-07-26: T9 partial — the two SYMLOG-shape comment mislabels (RR11 Beyond 1) corrected in `R/axes_reliability.R` and the M60 item_n test; both had attributed Table 3's 8.67 to a two-axis unbalanced set, when it is a three-axis sphere model's 26/3. Comment-only, no behaviour change.
- 2026-07-26: T8 done. Population recovery exact (<1e-4, chisq <1e-6) at four single-item cells including k = 5 (fractional item_n 2.5) and k = 16 (the COC shape), each also asserting NO `SS` latent exists rather than reading a value from one; Monte-Carlo recovery within 2 MC-SEs at k = 12; and a lavaan/OpenMx cross-check agreeing to <1e-3 at k = 12 and k = 5 with the `zeta1*B` term removed — at one item per position B is the identity, so keeping zeta1 would confound it perfectly with the residuals.

- 2026-07-26: T9 done — roxygen (drop rule, mixed case, N–B condition, the new `details` fields, and the two-dimensional scope naming spherical designs out of scope), `man/axes_reliability.Rd` regenerated by `document()`, the vignette's closing section extended with two paragraphs, and the NEWS text folded into the existing `axes_reliability()` bullet under 2.0.0 per D-031 (no milestone numbers in user-facing text). `pkgdown::check_pkgdown()` clean.
- 2026-07-26: AC8 evidence — `devtools::check(args = "--no-manual")` **Status: OK, 0 errors / 0 warnings / 0 notes** (6m 21s, tests 256s), and the PDF manual actually built via `R CMD Rd2pdf` (exit 0, 346 KB); its pdfTeX `dest` warnings are pre-existing cross-package link references (ggplot2, lavaan, boot), not M61's. Status → review.

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
- **M61-D2 (2026-07-26): RR11 ingested.** SYMLOG is a three-axis *sphere* model (Strack
  pp. 2, 5, 9), so its Table 3 `item_n` 8.67 = 26/3 is unreachable under this estimator's
  two-axis input contract; the six type-e/f rows are banked as formula-layer
  Spearman–Brown anchors only, never an `axes_reliability()` fixture. AC4 split into
  AC4a/AC4b, AC5 re-scoped to the formula layer, BC1–BC5 ingested verbatim as AC9–AC13.
  Reasoning and the verified arithmetic:
  `cairn/reviews/archive/RR11-axes-reliability-fractional-item-n.md`.

## Review
