# M91: Stop NA-ing computable numbers when only the raw arm refuses

**Status:** done (2026-08-16, PR #119 https://github.com/jmgirard/circumplex/pull/119)

**Goal:** When `axes_corrected_se()`'s raw arm trips the degeneracy criterion and its cov2cor arm does not, stop NA-ing `corrected` and `fiml_ratio` — NA only `naive` with its own carried reason — and update every surface documenting the unit-refusal contract.

**Outcome:** The decoupled return in `axes_corrected_se()` (RR18 rec 7, routed by D-044): the cov2cor arm — criterion then pricing — resolves first and still refuses all three vectors as a unit with one warning; a raw-arm-only criterion trip or pricing failure NAs `naive` alone, silently, its literal carried in the new `naive_reason` return field and surfaced as `details$naive_reason`. Regressions: counterexample-A congruence (corrected/fiml_ratio within 1e-9 relative of the unscaled matrix), the inflated-variance non-rescaling member, the assembly-seam injection, and the print regime (no failure note, corrected SEs rendered). AC3 sweep updated the roxygen contract, man page, NEWS, and sibling comments; D-044's unit-refusal clause carries a dated superseding annotation; M90 review F11 closed by rescoping the indefinite band's rationale (optimizer error, not cov2cor rounding) to cover the raw arm.

**Decisions:** M91-D1 — `naive_reason` surfaces as a silent details field (no warning; the refused quantity is never user-reported). M91-D2 — the raw arm keeps the shared refusal vocabulary.

**Review:** three-lens fan-out. Blame-history and prior-review: no findings. Diff-bug: logic confirmed correct; 9 findings, none floor-qualifying — 8 fixed at the gate (two-surface agreement claim scoped to the shared criterion; `@return` documents the pricing-failure route; arm reorder removes the wasted raw sandwich and the naive_reason discard; AC10 retitle; band-rationale unsplice; `empty` hoist; M89 AC2 lineage named), 1 rejected (AC1 names no test venue). Nothing graduated or retired at hygiene.
