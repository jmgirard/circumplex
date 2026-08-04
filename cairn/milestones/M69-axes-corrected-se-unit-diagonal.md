# M69: Correlation-metric pricing for `axes_reliability()`'s corrected component SEs

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** RR15
- **Principles touched:** —
- **Branch/PR:** `m69-axes-corrected-se-unit-diagonal`

## Goal

Price `axes_corrected_se()`'s corrected branch at `cov2cor(Sigma-hat)` — the
matrix `axes_scaling_factor()` already uses — so both halves of the
correlation-metric correction are computed on one metric.

## Scope

**In:** the `corrected` branch repriced at `cov2cor(Sigma-hat)`, `fiml_ratio`
returned with both sides priced there (RR15 §3), RR15 B2's nonpositive-diagonal
refusal, the moved assertions and fixtures, M66's calibration re-measured, the
affected prose, and the stale cross-reference at `R/axes_scaled_fit.R:135`.

**Out:** the scaled *difference* test and a Swain/Bartlett correction (ROADMAP
rows); `p*`/`N` and the lavaan-variant naming (M70); `sample.cov.rescale =
FALSE` (RR15 rec 8); a fresh 200-replicate FIML simulation (D2). `naive` stays
raw — the only independent tie of the derivative set to lavaan.

## Acceptance criteria

- [ ] **AC1** — Every assertion or fixture whose value moves is enumerated in
      this file with old value, new value, and re-pinned-or-regenerated — at
      minimum `test-axes-corrected-se.R:203`, `:204` (RR15 §7: *will* redden),
      the live-vs-stored arm over `fixtures/m66-corrected-se-cells.rds`, and
      the bootstrap fixture's `analytic` column. Regenerated fixtures record
      new provenance; none keeps pre-M69 values. Lavaan fence: AC8.
- [ ] **AC2** — A vech-space oracle building Delta, V and the standardization
      Jacobian as literal matrices at `cov2cor(Sigma-hat)` agrees with
      `$corrected` to under 1e-6 relative on the analytic probe maps, at the
      same fitted `Sigma-hat` with no refit. Bar from the discrimination
      required: the superseded raw pricing differs by 1.05e-3 on its closest
      component at n = 600 (RR15 measurements), fencing it 1000x.
- [ ] **AC3** — M66's calibration is re-run at all three cells and the
      regenerated cells replace the fixture with new provenance. Every passage
      in `R/axes_reliability.R`'s roxygen, `R/axes_reliability_oop.R`'s printed
      output, `vignettes/axes-reliability.Rmd` and `NEWS.md` characterizing the
      correction's size, direction or sign is then **read for meaning, not
      grepped for literals** (the shipped statements are rounded percentages
      and bare sign claims), each listed with old and new wording or
      "unchanged, rounds the same way", and framed per RR15 B4 as a coherence
      fix rather than a material recalibration.
- [ ] **AC4** — `R/axes_scaled_fit.R`'s Wc comment cites a range of
      `R/axes_corrected_se.R` spanning at most 15 lines containing the
      diagonal-fold assignment, and states each side's pricing; a test parses
      the cited range and asserts both, so a later edit reddens.
- [ ] **AC5** — The profile's `verify` slot is clean: `document()`, `test()`,
      and `check()` before review.
- [ ] **AC6 (BC1)** — `axes_corrected_se()` returns `naive` priced at the raw realigned Sigma-hat, `corrected` priced at `stats::cov2cor()` of that matrix, and a per-component `fiml_ratio` equal to the corrected SE divided by the normal-theory SE **both evaluated at `cov2cor(Sigma-hat)`**; the FIML composition in `R/axes_reliability.R` consumes `fiml_ratio`, and no code in `R/` forms a ratio of `corrected` to `naive`. Enumeration procedure for that universal: `grep -rn "axes_corrected_se" R/` lists every call site (one at review time); each hit and its surrounding function is inspected for ratio formation.
- [ ] **AC7 (BC2)** — On the probe fits of `test-axes-corrected-se.R`, for a seeded random positive diagonal D (entries in [exp(-0.3), exp(0.3)]) and the scalar 2: `corrected` and `fiml_ratio` computed from `D %*% Sigma-hat %*% D` and from `2 * Sigma-hat` each equal their values at Sigma-hat within 1e-6, and `naive` at `2 * Sigma-hat` equals 2 times `naive` at Sigma-hat within 1e-6. Tolerance derivation: the superseded raw/mixed pricing violates these identities by O(1) factors (measured 1.538-2.114 at scalar 2), ≥6 orders above the tolerance; the measured floating-point drift of the cov2cor path is 4.4e-16 and the repository's worst observed instrumentation drift is 1.3e-8, ≥2 orders below. No bit-identity assertions.
- [ ] **AC8 (BC3)** — `tests/testthat/test-axes-corrected-se.R` lines 67-69 and 191-194 pass with their assertion lines unedited: `naive` reproduces lavaan's own component SEs within 1e-7 absolute.
- [ ] **AC9 (BC4')** — The FIML path's corrected SE is the observed-information SE multiplied by the per-parameter ratio of correlation-metric SE to normal-theory SE, **both evaluated at the implied correlation matrix `cov2cor(Sigma-hat)` of that fit**. Against the committed 200-replicate fixture at 2, 5, and 10% MCAR, mean corrected FIML SE(xi1) / empirical SD ∈ [0.90, 1.10] in every cell, re-run under the new pricing. Numeric projection: the reported FIML SE moves by less than 1% relative to the shipped value in every cell (measured -0.062% on the cormat probe's xi1; the 1% bound allows the FIML diagonal profile, sd 0.030), so the band's verdict cannot flip — but the re-run, not the projection, is the evidence.
- [ ] **AC10 (BC5)** — With any nonpositive diagonal entry in the realigned Sigma-hat, `naive`, `corrected`, and `fiml_ratio` are all NA under one named `reason`, refused **before** `cov2cor()` executes; and in every failure return of the function the three vectors are NA together with no fallback. Enumeration procedure: the `na_out()` calls in `axes_corrected_se()` are the function's only non-success returns; list and check each.
- [ ] **AC11 (BC6)** — Every statement of the mixed-ratio artifact's direction or factor in `cairn/milestones/M69-axes-corrected-se-unit-diagonal.md` and in the full diff M69 merges states inflation by N/(N-1) (equivalently, division by (N-1)/N), never shrinkage by (N-1)/N. Enumeration procedure: read the milestone file in full and read the complete `git diff` of the M69 branch against its base — bounded sets that catch prose stating the claim without a searchable literal.

### Deviations from RR15

Agreed at the 2026-08-03 ingest gate after a fresh-context [O] audit of the set.

| # | Criterion | Deviation |
|---|---|---|
| D1 | AC7 (BC2) | Applied to `D %*% Sigma-hat %*% D` with the original dimnames re-attached — the bare product drops them and `axes_corrected_se()` refuses dimnames-free input by design (pinned at `test-axes-corrected-se.R:104`), so the literal recipe errors. "The probe fits" reads as the canonical octant probe used at `:30`/`:56` plus the zeta2 probe at `:180`. RR15 §5(b)'s wiring assertion — reported FIML SE equals `se_uncorrected * fiml_ratio` component-wise — is added here, uncovered by any BC and now the primary evidence the repricing reaches the FIML surface. |
| D2 | AC9 (BC4') | Verified on two arms. (a) The committed-fixture band arm re-runs unchanged and is acknowledged **insensitive** to the repricing: the fixture stores the *uncorrected* `fiml.se` and no Sigma-hat, and the proxy constant 1.4412 is a unit-diagonal quantity same-matrix pricing leaves fixed — a non-regression check, not evidence. (b) The evidence is the live arm at `test-axes-corrected-se.R:256-298`, re-measured, plus D1's wiring assertion. No fresh 200-replicate FIML simulation is in scope. |
| D3 | AC11 (BC6) | Domain restricted to the *forward-looking* prose M69 authors — Goal/Scope/AC/Task sections and the non-`cairn/reviews/` portion of the merged diff. Excludes (i) the append-only work log and Decisions entries, superseded by a dated entry rather than edited; (ii) RB15 and RR15 themselves, records of what was asked and answered; (iii) negated mentions of the superseded direction inside the criteria. Without this, BC6 is satisfiable only by editing history, which IP4 forbids. |
| D4 | AC8 (BC3) | Anchored by content, not line number: the three `naive`-vs-`lav_se()` assertions and the four `naive`-vs-`lav()` assertions, in whatever lines they occupy after T3's oracle lands, with their surrounding fixture construction also unchanged. Verified jointly with AC1. |
| D5 | AC10 (BC5) | The three `na_out()` calls are the only non-success *returns*; the function additionally has two *error* exits (the dimnames `stop()` and the realignment subscript) which raise conditions rather than return a value and are outside the NA-together contract. |

## Coverage

- AC1 → T4
- AC2 → T1, T2
- AC3 → T5, T6
- AC4 → T7
- AC5 → T2, T4, T7
- AC6 → T2
- AC7 → T3
- AC8 → T4
- AC9 → T3, T4
- AC10 → T2
- AC11 → T6

## Tasks

- [x] **T0** — Escalate the ratio-pricing question. Done 2026-08-03: RB15 →
      RR15, ingested with five deviations. `(RB tripwire: no-oracle —
      discharged)`
- [x] **T1** — Test-first: vech-space oracle at `cov2cor(Sigma-hat)`, confirmed
      RED against the shipped raw pricing before T3.
- [x] **T2** — Reprice: one realignment, two pricings — `naive` raw,
      `corrected` and `fiml_ratio` at `cov2cor(Sigma-hat)`
      (`R/axes_corrected_se.R:156-158`); add RR15 B2's nonpositive-diagonal
      refusal before `cov2cor()` with the NA-together contract extended to
      `fiml_ratio`; rewire `R/axes_reliability.R:1691` to consume `fiml_ratio`;
      header states each value's pricing. Keep the extracted helper's matrix
      parameter named `sigma`, or AC4's literal stops matching for a reason
      unrelated to what it guards.
- [x] **T3** — Pin the invariances (diagonal and scalar at 1e-6), the
      raw-`naive` scaling companion, and D1's wiring assertion.
- [ ] **T4** — Re-pin `:203`/`:204`, regenerate both fixtures with provenance
      per AC1, re-measure the live FIML arm at `:256-298` (D2's evidence arm).
- [ ] **T5** — Re-run M66's calibration at all three cells; regenerate it.
- [ ] **T6** — Read the four prose surfaces in full, update every size,
      direction or sign statement, commit the ledger, then run AC11's direction
      sweep within D3's domain.
- [x] **T7** — Repair `R/axes_scaled_fit.R:135`; add AC4's parsed-range guard.

## Work log

- 2026-08-03: created by /milestone-plan.
- 2026-08-03: criteria audit ([O], fresh context, authored none of the drafts) returned findings on all six drafted criteria. Fixed in the wording before this file was written: an unreproducible "about 3e-3" discrimination figure (measured live at 1.05e-3 on the governing component); an AC1 whose "unedited" framing hid four assertions and fixtures that actually break; and a bounded-promise failure whose grep-for-literals procedure provably misses "about 40%", "about 7% below" and a bare sign claim on shipped surfaces. Not fixed here: the FIML ratio's mixed-matrix (N-1)/N artifact, escalated at the gate and now AC4/T1.
- 2026-08-03: plan gate chose escalating the FIML ratio pricing to a Fable review over settling it in session as same-matrix pricing via a third return value, because it changes an exported number by a sample-size-dependent factor and is estimator math where a plausible-but-wrong answer survives ordinary review; falsified by an RR resting only on evidence this session already held.
- 2026-08-03: blocked on RB15 (metric pricing of the corrected component SEs and the FIML ratio), which T1 escalates and AC4 is verified against.
- 2026-08-03: plan gate chose re-running all three calibration cells over re-running the two fast cells and arguing the M1 MAR cell unchanged, because the figures are documented user-facing claims; falsified by a measured demonstration that the repricing is exactly scale-only in that cell.

- 2026-08-03: RR15 ingested. Answers Q1-Q7 recorded in Decisions below; BC1-BC6 ingested verbatim as AC6-AC11 with five deviations agreed at the gate (D1-D5). Status blocked → planned (it was parked from `planned`, never started, so `planned` is the honest un-park).
- 2026-08-03: ingest audit ([O], fresh context, authored none of the criteria) audited BC1-BC6 individually and as a set. Returned: BC6 unsatisfiable without editing an append-only work log (IP4); BC4' satisfiable as a no-op because the named fixture stores the uncorrected `fiml.se` and no Sigma-hat; BC2's literal `D %*% S %*% D` recipe errors on dropped dimnames; BC3's line anchors will shift when T1's oracle lands and duplicate AC1; BC5's "only non-success returns" false for two `stop()` exits; and RR15 rec 4's wiring assertion covered by no criterion. All five became deviations D1-D5; the wiring assertion joined D1. It also found the pre-ingest AC4 loophole recorded on the next line.
- 2026-08-03: **correction, superseding this file's 2026-08-03 entries and T1 wording that state the mixed-ratio artifact as "(N-1)/N" or "shrinking".** RR15 B1 measured the direction inverted: the mixed-matrix ratio **inflates** the reported FIML SE by N/(N-1) (1.0016694 exactly on every component), it does not shrink it by (N-1)/N. Magnitudes stand (~0.17% at n = 600, ~1% at n = 100). History is superseded, never edited (D-045); AC11/BC6 governs the forward-looking prose.
- 2026-08-03: pre-ingest AC4 carried a live loophole — "invariance under same-matrix pricing, **or the recorded (N-1)/N factor otherwise**" — which a mixed-pricing implementation could satisfy while violating BC1 and BC4'. Cut at the ingest gate rather than reworded; BC4' (AC9) now settles the pricing. The remaining criteria renumbered, old AC5 → AC4 and old AC6 → AC5.
- 2026-08-03: RR15 B3, new measurement worth carrying: on a shipped-path 5% MCAR FIML fit the fitted diagonal ranges **0.9433-1.0723, sd 0.0303** — nowhere near a constant (N-1)/N, and the fact that makes the mixed ratio un-pinnable on the only path that uses it. Alongside M68's misspecification range of 0.951-1.026.

- 2026-08-03: T1 done. Vech-space oracle appended at the END of `test-axes-corrected-se.R` (deviation D4: nothing may be inserted above BC3's anchors at `:67-69`/`:191-194`). Deliberately RED, as test-first requires: octant map 1.718e-3, blockwise/crossed map 1.731e-3, against the 1e-6 bar. The octant figure independently reproduces RR15's measured 1.7209e-3 on zeta1 by a route sharing no arithmetic with either the shipped code or the review's probe. The oracle's own three self-checks pass — `V %*% Gamma_S = I`, the Pearson-Filon `(1-rho^2)^2` diagonal of Gamma_R, and the Gamma_S sandwich collapsing exactly to the bread — so it is internally validated before it disagrees with anything. First blockwise draft used a contiguous layout; corrected to `axes_crossed_blocks()` per the M63 lesson before running.

- 2026-08-03: T2 done. `axes_se_pricing()` extracted (matrix parameter kept named `sigma` per AC4's coupling note) and called twice — raw for `naive`, `cov2cor()` for `corrected` and the new `fiml_ratio`; RR15 B2's `nonpositive_diagonal` refusal added before `cov2cor()` runs, with the NA-together contract extended to all three vectors; `R/axes_reliability.R:1691` now consumes `fiml_ratio` instead of forming `corrected/naive`. Both AC2 oracle tests go green. AC6/BC1's enumeration run: `grep -rn "axes_corrected_se" R/` gives one production call site (`:1679`), which consumes `fiml_ratio`; the only remaining quotient in `R/` is the helper's own same-matrix `std$corrected / std$naive`.
- 2026-08-03: minor amendment — T4's two literal re-pins were done ahead of T3, because the suite cannot reach green without them and T2's check-off needs it. `:203` 0.0042646 → 0.0042719; `:204` re-pinned onto `fiml_ratio` at 1.0022604 rather than the old `naive/corrected` 0.9978, since after M69 that quotient straddles two matrices and is not a meaningful quantity to fence (D-037). Task order otherwise unchanged.
- 2026-08-03: full `devtools::test()` carries exactly three known failures, all fixture staleness, none a regression: `test-axes-corrected-se.R:401`/`:406` (the `m66-corrected-se-cells.rds` live-vs-stored arm, 0.01173 vs stored 0.01171 and 0.01184 vs 0.01182) and `:472` (the bootstrap fixture's `analytic` column, 0.01186 vs 0.01184; `NOT_CRAN` only). T4 regenerates the bootstrap fixture, T5 the cells fixture. The ingest audit predicted all five breakages and no others; all five occurred and nothing else did.

- 2026-08-03: T3 partially done, deliberately left unticked. The invariance pins are in and green: `corrected` and `fiml_ratio` invariant to `D Sigma-hat D` for a seeded positive diagonal and to the scalar 2 at 1e-6, with the companion that `naive` scales by exactly 2 under the scalar (RR15's measured 2.000000) and visibly does NOT hold still under the diagonal — so the invariances cannot be the trivial consequence of normalizing everything, and the companion says from inside the same test which matrix `naive` sits on. Two self-inflicted bugs caught by running rather than by eye: the scalar companion was written against sqrt(2) where `D = sqrt(2)I` gives `2*Sigma-hat`, and the first blockwise oracle map used a contiguous block layout. **Still owed on T3: deviation D1's wiring assertion** (reported FIML SE equals `se_uncorrected * fiml_ratio` component-wise). It needs the FIML fit's own Sigma-hat, which `details` does not expose, and reconstructing the fit test-side would build both sides from the same code and catch nothing common-mode (the M65 (j) trap). Moved to T4; recorded in the test file as a named gap rather than stubbed, since a skipping stub reads as coverage.

- 2026-08-03: T3 completed and T7 done. D1's wiring assertion landed after all, via a CAPTURING mock: the real `axes_corrected_se()` is called through and its return recorded, so the test proves the branch wiring without rebuilding either side from the estimator's own code (the M65 (j) trap it was deferred over). Mutation-verified — reverting `:1691` to `corrected/naive` reddens it three times, once per component, and nothing else. T7's citation now reads `R/axes_corrected_se.R:172-178` and is held by a parsing guard in `test-axes-scaled-fit.R`; mutation-verified twice, a stale range failing 2 assertions and an over-wide range failing the 15-line span check.
- 2026-08-03: T4 bootstrap half done — `m66-bootstrap-oracle.rds` regenerated (0.6 min, B = 1000, three draws) with fresh provenance. **Unrequested independent evidence the repricing is right:** the pipeline bootstrap re-standardizes per resample and shares no arithmetic with the analytic formula, and the repriced SE is closer to it on all three draws — seed 1001 |old-boot| 0.000239 → |new-boot| 0.000221, 1002 0.000257 → 0.000240, 1003 0.000290 → 0.000276. The correction moved toward an empirical oracle, not merely toward internal consistency. T4's remaining piece is the `m66-corrected-se-cells.rds` regeneration, which is T5's compute.

## Decisions

- **2026-08-03 (RR15 Q1) — M69's premise is affirmed, on stronger ground than the plan had.** The Wc fold's compression `Sigma_ij = rho_ij` holds only at a unit diagonal, so the raw evaluation is not the derived formula at any scale — measured by non-homogeneity: scaling Sigma-hat by 2 scales the corrected SEs by 1.538/2.009/2.114 where any coherent variance-metric quantity gives exactly 2. RR13's own reproduction appendix derives both branches at the unit-diagonal population matrix, so the shipped raw pricing was plug-in drift from RR13's derivation, not a choice RR13 made. M68-D2 is affirmed on the same ground.
- **2026-08-03 (RR15 Q2/Q3) — both sides of the FIML ratio are priced at `cov2cor(Sigma-hat)`, and the ratio is returned rather than composed at the call site.** A mixed ratio fails the metric-only property the code documents, and would be a *regression* against shipped code: the both-raw ratio is already within 6.2e-4 of metric-only because the (N-1)/N is common-mode and cancels, where mixed pricing injects 1.7e-3. Returning `fiml_ratio` rather than a bare denominator keeps the mixed expression from being one plausible-looking line away at any future call site.
- **2026-08-03 (RR15 Q4) — RR13's BC4 is explicitly superseded, not re-satisfied.** Its operative phrase "evaluated at Sigma-hat" no longer describes the computation. What survives untouched is the missing-information pricing, which lives entirely in the `se_uncorrected` factor. Promoted cross-cutting as D-037. Same-matrix pricing *restores* agreement with RR13's published 1.441229 (1/0.6938522 = 1.44124, against the shipped both-raw 1.44034).
- **2026-08-03 (RR15 Q5/Q6/Q7) — the regression pin is positive-*diagonal* invariance, not scalar.** Exact, and strictly sharper: a scalar-only pin stays green under a "divide by the mean diagonal" pseudo-fix, which the non-constant fitted diagonal shows is materially wrong on real fits. The `n` divisor is unchanged — it cancels entirely in the FIML ratio and double-counts nothing. Non-FIML paths need only the repricing plus the shared B2 guard.

## Review
