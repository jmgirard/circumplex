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
`details$fit_scaling_failed`. One reachability nuance recorded rather than
implied: AC1's `"saturated"` is helper-boundary-only today (df = 0 needs
p = 3 with ζ1 fitted; `axes_reliability()` refuses fewer than four scales),
like the documented siblings at `R/axes_reliability.R:1951`.

**In:** the `df == 0` guard and its `"saturated"` literal; the
`"indefinite"`/`"ill_conditioned"` partition of the eigenvalue refusal; the
`cval <= 0` relabel; the cross-surface literal-precedence contract (cov2cor
arm evaluated first, with the raw-matrix finiteness hoist that requires);
and the documentation every user-visible literal owes, enumerated by AC4's
grep procedure.

**Out:** the metric move and the tau floor -> **M89**, this milestone's
dependency, where RR18's BC1/BC2/BC3/BC7/BC8 are met. RR18 rec 7's
decoupling of `naive` -> **M91**, split out at the 2026-08-16 replan gate
(size tripwire); D-044 routes rec 7 onward and M91 carries its lineage. The
a-posteriori cancellation guard on `cval` (RR18 rec 6, "consider") ->
candidate row, since M89's tightened floor closes every measured case and
RR18 asks for its constant to be calibrated against the oracle first.

## Acceptance criteria

- [ ] **AC1 (BC4, narrowed)** — `axes_scaling_factor()` refuses `df == 0` with the
      literal `"saturated"`, after the two df-consistency guards and before any matrix
      computation; the deterministic p = 3 construction (S = {1,.5,.3;.5,1,.4;.3,.4,1},
      scales A/A/B, `fit_zeta1 = TRUE`, df = 0) returns `"saturated"`; the guard's
      position upstream of the `cval` division — asserted by code order and the
      construction's literal, not a claim over all paths — makes `df` nonzero wherever
      `cval` is computed.
- [ ] **AC2 (BC5, anchors restated)** — Within the refusal region the criterion returns
      `"indefinite"` iff λmin < −λmax·sqrt(p·ε), else `"ill_conditioned"`; the comment
      beside the partition states its rationale as a convergence-noise band
      (fitted-matrix entries carry optimizer error ~sqrt(ε), so eigenvalues within
      ~λmax·sqrt(p·ε) of zero are not confident statements about the user's model) — if
      implementation cannot defend it, escalate rather than silently change the constant
      (RB tripwire: no-oracle). Anchors by construction and metric: the M89
      nestedness-grid indefinite probe (`dd %*% sigma %*% dd`, cov2cor, p = 24,
      λmin = −0.5) returns `"indefinite"` on both surfaces; the near-singular probe
      (cov2cor, p = 24, λmin = −9.32e-16) returns `"ill_conditioned"` on both.
- [ ] **AC3 (BC6)** — The `cval ≤ 0` (or non-finite) refusal at the end of
      `axes_scaling_factor()` no longer returns `"indefinite"`; it returns
      `"ill_conditioned"`, with the tr(UΓ) ≥ 0 rationale recorded in a comment beside it.
- [ ] **AC4 (procedure-based)** — A repo-wide grep for each pre-M90 refusal literal
      (`"indefinite"`, `"ill_conditioned"`, `"saturated"`) over `R/`, `man/`, `NEWS.md`,
      `vignettes/` enumerates the doc surfaces; every hit either already describes the
      post-M90 vocabulary or is updated here — including the inline comments at
      `R/axes_reliability.R:1897-1950`, whose df = 0 claim (`"unidentified"`) the replan
      audit falsified (measured: `"indefinite"`), the roxygen enumerations,
      `man/axes_reliability.Rd`, and NEWS. Documented: the three printed-output changes —
      AC1's `"saturated"`, AC2's partition, AC3's relabel (GP4).
- [ ] **AC5** — The `cval ≤ 0` branch's four-arm predicate is dispositioned arm by arm
      on recorded evidence. The two `cb` arms are settled by argument in the comment
      (cb = Σ(1−ρ²)²/baseline_df ≥ 0, equality only at |ρ| = 1, refused upstream). For
      the two `cval` arms: either a test reaches the branch with AC1's guard live and
      asserts `"ill_conditioned"`, or the recorded search — near-floor draws plus
      adversarial near-cancellation constructions targeting tr_vg − sum(acov·bmat) ≈ 0,
      ≥1e4 draws spanning p ∈ {3, 8, 24} — finds no reaching input, the branch is marked
      a defensive backstop (not "unreachable"), and a test asserts the upstream guard
      fires on the search's nearest miss. AC3's comment covers all four arms.
- [ ] **AC6 (probe family widened)** — At p = 24 and at least one of p ∈ {8, 12}, tests
      construct matrices with λmin just inside and just outside −λmax·sqrt(p·ε) (factors
      ≈ 0.5 and ≈ 2, computed per p), at two λmax scales (≈ 1, ≈ 1e3) and two
      construction forms (rank-one negative perturbation; eigen-recomposition
      Q diag(λ) Qᵀ), asserting the literal flips across the boundary in every cell; the
      drop-p, squared-p, and drop-λmax partition mutants each verifiably redden. p = 3
      excluded deliberately: there the ×2 factor no longer separates the squared-p mutant.
- [ ] **AC7 (reworded at the replan)** — Whenever both of `axes_corrected_se()`'s arms
      refuse, the reported literal is the one its cov2cor arm produces — requiring the
      arm order at `R/axes_corrected_se.R:264-268` inverted (cov2cor first) and the
      raw-matrix finiteness check `!all(is.finite(sigma))` hoisted ahead of both arms
      (mirroring `R/axes_scaled_fit.R:149-154`), so `cov2cor()` never runs on an NA/NaN
      diagonal and M71's one-warning-per-refusal contract holds — and M89's nestedness
      grid is re-run with expected literals updated to the new vocabulary, asserting the
      nesting relation pointwise: every cell where the scaling surface refuses, the SE
      helper refuses with the same literal.

### Deviations from RR18

| BC/rec | Departure | Why |
|---|---|---|
| BC1 | Met in M89 | The metric move is M89's Goal; this milestone depends on it. |
| BC2 | Met in M89 | AC2 here changes the literal BC2 asserts agreement on, which is why it sequences after. Its retained cost (unit refusal) is M91's. |
| BC3 | Met in M89 | The τ floor is M89's. |
| BC4 | "no path reaches `cval = Inf`" narrowed to a guard-order claim | Bounded-promise rule: no named procedure enumerates all paths; the guard's position plus the construction's literal is what a procedure settles. |
| BC5 | Anchor restated by construction and metric (λmin = −0.5, cov2cor, p = 24 — not the brief's −0.382, which matches no committed probe); partition constant kept, rationale demanded in-code | Replan audit: the bare number is irreproducible; the constant's original rationale (the refusal floor) was superseded by M89's τ floor. |
| BC6 | Met as AC3 | — |
| BC7 | Met in M89 | The oracle is M89's evidence base for τ. |
| BC8 | Met in M89 | M89 documents the metric contract; AC4 here documents this milestone's own literals. |
| rec 6 | Deferred to the τ-calibration candidate row | "Consider"-level; M89's floor closes every measured case; RR18 asks for oracle calibration first. |
| rec 7 | Split to M91 (planned 2026-08-16, depends on M90) | Size tripwire at the replan gate; D-044 routes it onward. |

## Coverage

- AC1 → T1, T2
- AC2 → T3, T4
- AC3 → T6
- AC4 → T7
- AC5 → T6
- AC6 → T4
- AC7 → T5

## Tasks

- [ ] **T1** — Test-first: the deterministic saturated construction, red
      against M89's merged code (measured at the replan audit: it returns
      `"indefinite"` today).
- [ ] **T2** — The `df == 0` guard and its `"saturated"` literal.
- [ ] **T3** — Test-first: the indefinite/near-singular partition at the
      nestedness-grid probes, red where the two currently share one literal.
- [ ] **T4** — The partition in `axes_sigma_degenerate()`, plus the AC6
      battery: two p values, two λmax scales, two construction forms, three
      partition mutants verified to redden.
- [ ] **T5** — The cov2cor-arm-first inversion and finiteness hoist in
      `axes_corrected_se()`; M89's nestedness grid re-expected under the new
      vocabulary and re-run (AC7).
- [ ] **T6** — The `cval ≤ 0` relabel, the four-arm comment, and AC5's
      arm-by-arm record (`cb` by argument; `cval` by reaching test or
      recorded search).
- [ ] **T7** — AC4's grep sweep: roxygen enumerations, the
      `R/axes_reliability.R:1897-1950` comment block (falsified df = 0
      claim), `devtools::document()`, NEWS.
- [ ] **T8** — Full `devtools::check()`.

## Work log

- 2026-08-16: created by /milestone-plan, in the same run as M89's re-cut. Takes RR18's BC4-BC6, which M89's Deviations table defers here, plus three ingest-audit findings that RR18's own criteria left uncovered.
- 2026-08-16: replanned under the current rulebook (/milestone-plan). Full-mode criteria audit ([O] fresh-context reader, two passes): round 1 returned 1 blocker (AC7's "grid still passes" unsatisfiable — the grid pins the literals AC2/AC8 change) plus 10 findings; round 2 on the final wordings returned clean except the AC7 finiteness hoist, added. AC1/AC4/AC5 narrowed to named procedures, AC2 anchors restated by construction+metric, AC6 gains the λmax/form axes and the drop-λmax mutant, the arm-order task added; AC8 split to M91 at the gate.
- 2026-08-16: alternative rejected at the gate — keeping AC8 (naive decoupling) in M90; lost to the size tripwire (8 criteria, ~12 tasks after repairs); falsified if the split forces `axes_corrected_se()`'s return shape to be reopened twice across the M90/M91 seam.
- 2026-08-16: alternative rejected at the gate — tightening the AC2 partition to the eigensolver-noise band (~p·ε); the reviewed BC5 constant kept with a demanded convergence-noise rationale; falsified by implementation finding no defensible rationale, in which case escalate via RB (no-oracle) rather than silently change the constant.

## Decisions

## Review
