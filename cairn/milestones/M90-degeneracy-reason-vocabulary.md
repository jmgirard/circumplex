# M90: Say which degeneracy happened, and stop saying it when it didn't

- **Status:** planned
- **Priority:** normal
- **Depends on:** M89
- **Driving RR:** RR18
- **Principles touched:** GP2, GP4
- **Branch/PR:** —

## Goal

Split the fitted-matrix refusal vocabulary so an indefinite model-implied
matrix -- a statement about the user's model -- stops sharing one word with
mere ill-conditioning, give a saturated model its own refusal instead of
letting it reach `cval = Inf` and report `"indefinite"`, and stop the
`cval <= 0` branch from claiming indefiniteness it can never actually
diagnose.

## Scope

Surface tier: **user-facing** -- these are the literals `axes_reliability()`
prints and documents in `details$se_correction_failed` and
`details$fit_scaling_failed`.

**In:** the `df == 0` guard and its `"saturated"` literal; the
`"indefinite"`/`"ill_conditioned"` partition of the eigenvalue refusal; the
`cval <= 0` relabel; the documentation every one of those user-visible
literals owes; and RR18 rec 7's decoupling of `naive`, which M89 deferred
here at the 2026-08-16 plan gate.

**Out:** the metric move and the tau floor -> **M89**, this milestone's
dependency, where RR18's BC1/BC2/BC3/BC7/BC8 are met. The a-posteriori
cancellation guard on `cval` (RR18 rec 6, "consider") -> candidate row, since
M89's tightened floor closes every measured case and RR18 asks for its
constant to be calibrated against the oracle first.

## Acceptance criteria

- [ ] **AC1 (BC4)** — `axes_scaling_factor()` refuses `df == 0` with the literal
      `"saturated"`, checked after the two df-consistency guards and before any
      matrix computation; the brief's deterministic p = 3 saturated construction
      (S = {1,.5,.3;.5,1,.4;.3,.4,1}, scales A/A/B, `fit_zeta1 = TRUE`, df = 0)
      returns `"saturated"`, and no path reaches `cval = Inf`.
- [ ] **AC2 (BC5)** — Within the refusal region the eigenvalue criterion returns
      `"indefinite"` iff λmin < −λmax·sqrt(p·ε), else `"ill_conditioned"`;
      measured anchors: the M89 T9 indefinite probe (λmin = −0.382) returns
      `"indefinite"` on both surfaces, and the exactly/near-singular probe
      (λmin = −9.32e-16) returns `"ill_conditioned"` on both.
- [ ] **AC3 (BC6)** — The `cval ≤ 0` (or non-finite) refusal at the end of
      `axes_scaling_factor()` no longer returns `"indefinite"`; it returns
      `"ill_conditioned"`, with the tr(UΓ) ≥ 0 rationale recorded in a comment
      beside it.
- [ ] **AC4** — Every literal a user could previously have seen and what it
      becomes is documented wherever the existing literals are — the roxygen
      reason enumerations in `R/axes_reliability.R`, the regenerated
      `man/axes_reliability.Rd`, and a NEWS entry. That is all three of this
      milestone's changes: AC1's new `"saturated"`, AC2's partition, and
      **AC3's `"indefinite"` → `"ill_conditioned"` relabel**, which is a
      printed-output change under GP4. (Ingest audit, findings 7 and 12.)
- [ ] **AC5** — AC3's branch is either fired or shown unreachable, on evidence
      this criterion fixes rather than on preference. Either a test reaches it
      with AC1's guard live and asserts `"ill_conditioned"`; or, if a search
      finds no such input, the search that licenses that conclusion is recorded
      in the milestone (the family swept, the draw count, and the matrix sizes,
      at minimum ≥1e4 draws spanning p ∈ {3, 8, 24}), the branch is marked
      unreachable-by-construction in code naming the two gates that make it so,
      and a test asserts the guard immediately upstream fires instead. The
      comment AC3 requires covers that branch's whole predicate — `cb` and the
      two non-finite arms as well as `cval ≤ 0` — since the tr(UΓ) ≥ 0
      rationale speaks to only one of the four. (Ingest audit, finding 6.)
- [ ] **AC6** — AC2's partition is exercised *at* its threshold, not only far
      from it. At each of p = 24 and at least one of p = 12 / p = 8, the test
      constructs matrices whose λmin sits just inside and just outside
      `−λmax·sqrt(p·ε)` (scale factors ≈ 0.5 and ≈ 2 of the threshold, computed
      per p) and asserts the literal flips across that boundary. Far-field
      anchors alone cannot detect a missing or squared `p`, which is what this
      criterion exists to catch. (Ingest audit, finding 5.)
- [ ] **AC7** — The cross-surface contract M89 established survives this
      milestone's vocabulary split. Because the partition threshold in AC2 is
      not congruence-invariant, the SE helper's two arms can classify one
      matrix differently; so whenever both surfaces refuse, the literal
      `axes_corrected_se()` reports is the one its `cov2cor` arm produces —
      the arm `axes_scaling_factor()` also uses — and M89's AC2/AC8 nestedness
      grid is re-run here and still passes. (Ingest audit, finding 2, which
      found M89's contract otherwise dying silently in its dependent.)

- [ ] **AC8** — `axes_corrected_se()` stops NA-ing computable numbers. When the
      raw arm trips but the `cov2cor` arm does not, `corrected` and
      `fiml_ratio` are still reported and only `naive` is NA, carrying its own
      reason field; when the `cov2cor` arm trips, all three are NA as before.
      On the counterexample-A construction (M89 AC1) the corrected SEs match
      the unscaled matrix's to within 1e-9 relative rather than being NA.
      Takes RR18 rec 7, which M89's Deviations table records as a retained
      cost; the helper's documented return contract is updated with it.

### Deviations from RR18

| BC | Departure | Why |
|---|---|---|
| BC1 | Met in M89 | The metric move is M89's Goal; this milestone depends on it. |
| BC2 | Met in M89; its retained cost closed here | AC2 here changes the literal BC2 asserts agreement on, which is why it sequences after; AC8 removes the all-three-vectors coupling M89 accepted as a cost. |
| BC3 | Met in M89 | The τ floor is M89's. |
| BC7 | Met in M89 | The oracle is M89's evidence base for τ. |
| BC8 | Met in M89 | M89 documents the metric contract; AC4 here documents this milestone's own literals. |

## Coverage

- AC1 → T1, T2
- AC2 → T3, T4
- AC3 → T5
- AC4 → T6
- AC5 → T5
- AC6 → T4
- AC7 → T4, T5
- AC8 → T5

## Tasks

- [ ] **T1** — Test-first: the deterministic saturated construction, red
      against M89's merged code (which reports `"indefinite"` via `cval = Inf`).
- [ ] **T2** — The `df == 0` guard and its `"saturated"` literal.
- [ ] **T3** — Test-first: the indefinite/near-singular partition at the octant
      probe, red where the two currently share one literal.
- [ ] **T4** — The partition in `axes_sigma_degenerate()`, plus the AC6 anchors
      at a second matrix size.
- [ ] **T5** — The `cval ≤ 0` relabel, and either its reaching test or the
      unreachability record AC5 permits — decided by measurement, not by
      preference.
- [ ] **T6** — Roxygen enumerations, `devtools::document()`, NEWS.
- [ ] **T7** — Full `devtools::check()`.

## Work log

- 2026-08-16: created by /milestone-plan, in the same run as M89's re-cut. Takes RR18's BC4-BC6, which M89's Deviations table defers here, plus three ingest-audit findings that RR18's own criteria left uncovered.

## Decisions

## Review
