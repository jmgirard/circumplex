# M91: Stop NA-ing computable numbers when only the raw arm refuses

- **Status:** review
- **Priority:** normal
- **Depends on:** M90
- **Driving RR:** —
- **Principles touched:** GP2, GP4
- **Branch/PR:** m91-naive-arm-decoupling

## Goal

When `axes_corrected_se()`'s raw arm trips the degeneracy criterion and its
cov2cor arm does not, stop NA-ing `corrected` and `fiml_ratio` — quantities
priced exactly at the correlation matrix — NA only `naive` with its own
carried reason, and update every surface that documents the unit-refusal
contract.

Lineage: RR18 rec 7, which M89's Deviations table records as a retained cost
and D-044's Consequences route onward ("RR18 rec 7 … are M90's"); split out
of M90 at the 2026-08-16 replan gate on the size tripwire. `Driving RR` stays
`—` because rec 7 is a recommendation, not a Binding criterion, and RR18's BC
bullets would not match the binding-criteria check (the M82 precedent).

## Scope

Surface tier: **user-facing** — this changes `details$se_correction_failed`
(NULL where non-NULL before), the reported `corrected`/`fiml_ratio` vectors
(numbers where NA before), and the printed SE-failure note.

**In:** the decoupled return shape; where `naive_reason` surfaces to users
(decided at implementation, recorded as a milestone-local decision); the
roxygen contract rewrite at `R/axes_reliability.R:714-735`; a dated
annotation on D-044's "three vectors refuse as a unit" sentence; NEWS; the
counterexample-A regression; a non-rescaling member of the raw-degenerate
family.

**Out:** the vocabulary split → **M90**, this milestone's dependency. The
degeneracy criterion itself → M89/D-044 stand untouched. The τ calibration →
candidate row.

## Acceptance criteria

- [ ] **AC1 (RR18 rec 7)** — When the raw arm trips the degeneracy criterion
      and the cov2cor arm does not, `axes_corrected_se()` reports `corrected`
      and `fiml_ratio` computed from the cov2cor arm, NAs only `naive`, and
      `reason` (hence `details$se_correction_failed`) is NULL; the raw-arm
      refusal is carried in a new named return field (`naive_reason`), and
      where it surfaces to users is decided at implementation and recorded as
      a milestone-local decision. On M89's counterexample-A
      diagonal-rescaling construction the corrected SEs match the unscaled
      matrix's to within 1e-9 relative rather than being NA; and on a
      non-rescaling member of the raw-degenerate family (a huge-but-finite
      single variance, the M71 inflation route) the same decoupled state is
      asserted, so one exemplar does not stand in for the family.
- [ ] **AC2** — When the cov2cor arm trips, all three vectors are NA with one
      reason, exactly as before — pinned by re-running the post-M90
      nestedness grid with its raw-arm-only cells re-expected under AC1
      (reason NULL, `naive_reason` set) and its cov2cor-arm cells unchanged
      (all three vectors NA with one reason).
- [ ] **AC3 (procedure-based)** — A repo-wide grep for the unit-refusal
      contract's phrases ("as a unit", "all three vectors", "never the
      reverse", "three vectors refuse") over `R/`, `man/`, `NEWS.md`, and
      `vignettes/` enumerates the surfaces documenting the old contract;
      every hit is updated or verified already-correct. Known today
      (illustrative, not load-bearing): the roxygen at
      `R/axes_reliability.R:714-735`; the comments at
      `R/axes_corrected_se.R:259-263` and `R/axes_scaled_fit.R:135-138`;
      regenerated man pages; a NEWS entry; and a dated annotation on D-044's
      "three vectors refuse as a unit" sentence (the repo's D-entry
      annotation precedent).
- [ ] **AC4 (GP4)** — The printed SE-failure note
      (`R/axes_reliability_oop.R:284-290`) does not print in the
      raw-arm-only regime; a regression asserts the printed output there
      shows the corrected SEs.
- [ ] **AC5** — The raw-arm-only state is reached end-to-end through the
      assembly seam: a test injects a matrix degenerate in the raw metric and
      clean in cov2cor via the fitted-matrix seam and asserts the decoupled
      return surfaces through `axes_reliability()`'s details.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T5
- AC4 → T4
- AC5 → T3

## Tasks

- [x] **T1** — Test-first: the counterexample-A end-to-end regression plus
      the non-rescaling (inflated-variance) probe, red against post-M90 code
      (which NAs all three vectors).
- [x] **T2** — The decoupled return in `axes_corrected_se()` with
      `naive_reason`; the `naive_reason` routing decision recorded.
- [x] **T3** — The post-M90 nestedness grid re-expected per AC2, plus the
      assembly-seam injection test (AC5).
- [x] **T4** — The print-method regime (AC4) and its regression.
- [x] **T5** — AC3's grep sweep: roxygen rewrite, sibling comments, D-044
      annotation, `devtools::document()`, NEWS.
- [x] **T6** — Full `devtools::check()`.

## Work log

- 2026-08-16: created by /milestone-plan at the M90 replan gate — RR18 rec 7 split out of M90 on the size tripwire. Criteria audited in the same full-mode pass ([O] fresh-context reader, round 2): its blocker on AC2's "grid unchanged" wording and its one-exemplar note on AC1 are repaired in this file's wording; AC3/AC4/AC5 passed as written.

- 2026-08-16: /milestone-implement session start — status in-progress, branch m91-naive-arm-decoupling cut from pushed master (408c5cef). Gate: naive_reason surfaces as a silent details field (no warning); the raw arm keeps the shared refusal vocabulary with the band rationale rescoped (M90 F11).
- 2026-08-16: T1 — the two AC1 regressions (counterexample-A congruence at the helper; huge-finite single variance, the non-rescaling member) written and confirmed red against post-M90 code: unit refusal with warning, no naive_reason field. AC1's "end-to-end" numeric pin sits at the helper where the 1e-9 comparison is clean; the through-`axes_reliability()` assertion is T3's assembly-seam test (AC5).

- 2026-08-16: T2+T3 (one checkpoint; the suite is green only with both) — decoupled return shipped in `axes_corrected_se()` (`naive_reason` beside `reason`; raw-arm criterion trip or pricing failure NAs `naive` alone, silently; cov2cor-arm refusals unchanged as unit refusals), `details$naive_reason` wired through `axes_reliability()`; M91-D1/M91-D2 recorded; nestedness grid, counterexample-A scaling test, AC10 shape pin and the assembly-seam injection test re-expected (the M89 AC2 assembly test is now the AC5 test); the Wc citation range in axes_scaled_fit.R re-anchored (176-184) after comment growth; F11 rationale rescope at axes_sigma_degenerate(). Full suite FAIL 0 / PASS 8291.

- 2026-08-16: T4 — AC4 regression added (end-to-end seam injection through summary()): no failure note, the calibrated-SE claim and both-sides opening print, every SE-carrying component row renders its number, and nothing printed names the naive arm. No print-method code change needed: the note keys on `se_correction_failed`, NULL in this regime by T2. File green.

- 2026-08-16: T5 — AC3 sweep run over R/, man/, NEWS.md, vignettes/: 6 hits enumerated (roxygen contract passage + man mirror, NEWS entry, and the three comments T2 had already rewritten); the roxygen contract and @return details doc rewritten, the details assembly comment updated, NEWS's M89 entry corrected in place (unreleased dev line) naming `details$naive_reason`, D-044's unit-refusal clause given the dated superseding annotation (D-023 precedent), `devtools::document()` clean (axes_reliability.Rd regenerated, no link warnings). Post-sweep re-grep: only M91-correct text remains. Suite FAIL 0 / PASS 8300.

- 2026-08-16: T6 — full `devtools::check(args = "--no-manual")`: 0 errors / 0 warnings / 0 notes. All tasks done; status → review.

- 2026-08-16: routed from the M90 review (diff-lens F11): the partition's convergence-noise rationale is derived in the correlation metric but the criterion also runs on the SE helper's raw arm; a raw-arm-only "indefinite" was not empirically reachable at M90, and this milestone's decoupling reopens that surface — address the rationale's scope (or the raw arm's own band) when restructuring the arms.

## Decisions

- **M91-D1 (2026-08-16): `naive_reason` surfaces as a silent `details` field.** The raw-arm refusal is carried as `details$naive_reason` (NULL when the raw arm computed), with no warning and no printed note: the refused quantity — the helper's `naive` vector, the uncorrected normal-theory pricing kept only as the tie to lavaan's own SE (D-037) — is never user-reported, and in the raw-arm-only regime every reported number is present and correct, so a warning would flag numbers that are fine. Gate-approved 2026-08-16.
- **M91-D2 (2026-08-16): the raw arm keeps the shared refusal vocabulary.** A raw-arm-only trip reports the same criterion literals (`"indefinite"`/`"ill_conditioned"`/`"singular"`/`"unidentified"`) in `naive_reason`, not a collapsed arm-specific literal. The M90 F11 concern — the indefinite band's written rationale was derived for the correlation matrix — is closed by rescoping the rationale comment at `axes_sigma_degenerate()`: the band's argument rests on the optimizer's own error (entrywise relative error of order sqrt(tol) near a quadratic optimum), not on the `cov2cor()` rounding, so it covers the raw arm too. Gate-approved 2026-08-16.


## Review
