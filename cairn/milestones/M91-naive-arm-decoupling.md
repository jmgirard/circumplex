# M91: Stop NA-ing computable numbers when only the raw arm refuses

- **Status:** in-progress
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

- [ ] **T1** — Test-first: the counterexample-A end-to-end regression plus
      the non-rescaling (inflated-variance) probe, red against post-M90 code
      (which NAs all three vectors).
- [ ] **T2** — The decoupled return in `axes_corrected_se()` with
      `naive_reason`; the `naive_reason` routing decision recorded.
- [ ] **T3** — The post-M90 nestedness grid re-expected per AC2, plus the
      assembly-seam injection test (AC5).
- [ ] **T4** — The print-method regime (AC4) and its regression.
- [ ] **T5** — AC3's grep sweep: roxygen rewrite, sibling comments, D-044
      annotation, `devtools::document()`, NEWS.
- [ ] **T6** — Full `devtools::check()`.

## Work log

- 2026-08-16: created by /milestone-plan at the M90 replan gate — RR18 rec 7 split out of M90 on the size tripwire. Criteria audited in the same full-mode pass ([O] fresh-context reader, round 2): its blocker on AC2's "grid unchanged" wording and its one-exemplar note on AC1 are repaired in this file's wording; AC3/AC4/AC5 passed as written.

- 2026-08-16: /milestone-implement session start — status in-progress, branch m91-naive-arm-decoupling cut from pushed master (408c5cef).

- 2026-08-16: routed from the M90 review (diff-lens F11): the partition's convergence-noise rationale is derived in the correlation metric but the criterion also runs on the SE helper's raw arm; a raw-arm-only "indefinite" was not empirically reachable at M90, and this milestone's decoupling reopens that surface — address the rationale's scope (or the raw arm's own band) when restructuring the arms.

## Decisions

## Review
