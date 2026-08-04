# M69: Correlation-metric pricing for `axes_reliability()`'s corrected component SEs

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** RR15
- **Principles touched:** —
- **Branch/PR:** `m69-axes-corrected-se-unit-diagonal` / [PR #95](https://github.com/jmgirard/circumplex/pull/95)

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
- [x] **AC2** — A vech-space oracle building Delta, V and the standardization
      Jacobian as literal matrices at `cov2cor(Sigma-hat)` agrees with
      `$corrected` to under 1e-6 relative on the analytic probe maps, at the
      same fitted `Sigma-hat` with no refit. Bar from the discrimination
      required: the superseded raw pricing differs by 1.05e-3 on its closest
      component at n = 600 (RR15 measurements), fencing it 1000x.
- [x] **AC3** — M66's calibration is re-run at all three cells and the
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
- [x] **AC5** — The profile's `verify` slot is clean: `document()`, `test()`,
      and `check()` before review.
- [x] **AC6 (BC1)** — `axes_corrected_se()` returns `naive` priced at the raw realigned Sigma-hat, `corrected` priced at `stats::cov2cor()` of that matrix, and a per-component `fiml_ratio` equal to the corrected SE divided by the normal-theory SE **both evaluated at `cov2cor(Sigma-hat)`**; the FIML composition in `R/axes_reliability.R` consumes `fiml_ratio`, and no code in `R/` forms a ratio of `corrected` to `naive`. Enumeration procedure for that universal: `grep -rn "axes_corrected_se" R/` lists every call site (one at review time); each hit and its surrounding function is inspected for ratio formation.
- [ ] **AC7 (BC2)** — On the probe fits of `test-axes-corrected-se.R`, for a seeded random positive diagonal D (entries in [exp(-0.3), exp(0.3)]) and the scalar 2: `corrected` and `fiml_ratio` computed from `D %*% Sigma-hat %*% D` and from `2 * Sigma-hat` each equal their values at Sigma-hat within 1e-6, and `naive` at `2 * Sigma-hat` equals 2 times `naive` at Sigma-hat within 1e-6. Tolerance derivation: the superseded raw/mixed pricing violates these identities by O(1) factors (measured 1.538-2.114 at scalar 2), ≥6 orders above the tolerance; the measured floating-point drift of the cov2cor path is 4.4e-16 and the repository's worst observed instrumentation drift is 1.3e-8, ≥2 orders below. No bit-identity assertions.
- [x] **AC8 (BC3)** — `tests/testthat/test-axes-corrected-se.R` lines 67-69 and 191-194 pass with their assertion lines unedited: `naive` reproduces lavaan's own component SEs within 1e-7 absolute.
- [x] **AC9 (BC4')** — The FIML path's corrected SE is the observed-information SE multiplied by the per-parameter ratio of correlation-metric SE to normal-theory SE, **both evaluated at the implied correlation matrix `cov2cor(Sigma-hat)` of that fit**. Against the committed 200-replicate fixture at 2, 5, and 10% MCAR, mean corrected FIML SE(xi1) / empirical SD ∈ [0.90, 1.10] in every cell, re-run under the new pricing. Numeric projection: the reported FIML SE moves by less than 1% relative to the shipped value in every cell (measured -0.062% on the cormat probe's xi1; the 1% bound allows the FIML diagonal profile, sd 0.030), so the band's verdict cannot flip — but the re-run, not the projection, is the evidence.
- [x] **AC10 (BC5)** — With any nonpositive diagonal entry in the realigned Sigma-hat, `naive`, `corrected`, and `fiml_ratio` are all NA under one named `reason`, refused **before** `cov2cor()` executes; and in every failure return of the function the three vectors are NA together with no fallback. Enumeration procedure: the `na_out()` calls in `axes_corrected_se()` are the function's only non-success returns; list and check each.
- [x] **AC11 (BC6)** — Every statement of the mixed-ratio artifact's direction or factor in `cairn/milestones/M69-axes-corrected-se-unit-diagonal.md` and in the full diff M69 merges states inflation by N/(N-1) (equivalently, division by (N-1)/N), never shrinkage by (N-1)/N. Enumeration procedure: read the milestone file in full and read the complete `git diff` of the M69 branch against its base — bounded sets that catch prose stating the claim without a searchable literal.

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
- [x] **T4** — Re-pin `:203`/`:204`, regenerate both fixtures with provenance
      per AC1, re-measure the live FIML arm at `:256-298` (D2's evidence arm).
- [x] **T5** — Re-run M66's calibration at all three cells; regenerate it.
- [x] **T6** — Read the four prose surfaces in full, update every size,
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

- 2026-08-03: T6 prose ledger, read for meaning across all four surfaces (AC3). UNCHANGED, each re-measured rather than assumed: `R/axes_reliability.R:675` "overstates ... by about 40%" (measured 43.9% after repricing, was ~44.0%, rounds the same way); `:677` "understates it slightly for weak-axes, strong-general instruments" (sign claim re-verified on a weak-axes probe, naive/corrected = 0.982 < 1, still below 1); `:680` "calibrated uncertainty, not order-of-magnitude guidance" and `:682` "typically smaller than ... Strack et al." (direction claims, unmoved); `R/axes_reliability_oop.R:92` printed note (carries no figure); `vignettes/axes-reliability.Rmd:155-167` (same three claims as the roxygen, same grounds); `NEWS.md:18-30` "overstated ... by 25-45%" (43.9% sits inside the stated band, and v2.0.0 is unreleased so that entry describes the net shipped result — M69 earns no new bullet, it refines a correction whose user-visible statement does not move). PENDING T5's re-run: `R/axes_reliability.R:812` mild-rate "between 1.4% and 1.8%" and `:817` "average about 7% below" at 15% MCAR, both derived from the calibration cells.
- 2026-08-03: AC11 direction sweep run within D3's domain and clean. Seven hits in the branch diff: four state the artifact as inflation by N/(N-1) (`axes_corrected_se.R` header, `axes_reliability.R:1691`, the re-pin comment, the wiring assertion) and three refer to lavaan's fitted diagonal being (N-1)/N, which is a fact about `sample.cov.rescale` and not a claim about the artifact's direction. No occurrence of "shrink" anywhere in the diff. A second read-for-meaning pass over direction words carrying no literal — smaller/larger/too narrow/conservative/overstate/understate/flatter — returned nothing in the code diff. In the milestone's forward-looking sections the only hit is AC11's own negated mention, excluded by D3 clause (iii).
- 2026-08-03: **1/fiml_ratio at the octant probe measures 1.441229 against RR13's published 1.441229** — six-decimal agreement with the constant the shipped both-raw pricing missed at 1.44034. RR15 §4 predicted the restoration; this confirms it on the running code, and is the third independent line of evidence for the repricing after the vech oracle and the bootstrap.

- 2026-08-03: T5 done, T4 and T6 closed with it. `m66-corrected-se-cells.rds` regenerated at 201 replicates per cell, 7 workers, 93.4 min, provenance R 4.6.1 / lavaan 0.6.21. Calibration re-measured under the new pricing: complete 0.9584 -> 0.9598, 15% MCAR 0.9255 -> 0.9267, M1 MAR 1.0152 -> 1.0156, every cell inside its band ([0.90, 1.10] and [0.85, 1.15]). Two moved marginally toward 1 and one marginally away; all three deltas are ~0.1% against the comparison's own ~3.6% Monte-Carlo error, so this is NOT evidence that calibration improved — stated that way deliberately, because the tempting reading is unsupported at this replicate count.
- 2026-08-03: T6's two pending figures resolved, both **unchanged, because the re-run rounds the same way** (AC3's own escape clause, earned rather than assumed). `R/axes_reliability.R:817` "average about 7% below" measures 7.33% against the previous 7.45%. `:811-813` "0.1%, 0.8%, and 1.8%" at 2/5/10% MCAR are computed as `mean(fiml.se / M66_POP_RATIO) / sd(fiml.xi1)` from the STORED uncorrected `fiml.se` and the population constant 1.4412 — neither of which M69 touches, and M69 restored `1/fiml_ratio` to exactly 1.441229, so that constant is now more exactly right than it was. The published 0.96 / 0.93 / 1.02 all still round from the re-measured values. No prose edit was required on any of the four surfaces; the ledger's "unchanged" entries are now measured rather than projected.
- 2026-08-03: full `devtools::test()` green, no failures, with the regenerated fixture. All five breakages the ingest audit predicted are closed.

- 2026-08-03: two source-reading guards written this milestone would have FAILED CRAN, both caught by `devtools::check()` rather than by the suite. `test_path("..", "..", "R", ...)` resolves to nothing in an installed package and `readLines()` errors there rather than skipping — the M7 "a step that doesn't run reports success" trap, in its louder form. Hit twice: the AC4 citation guard first, then the AC10 enumeration written minutes earlier, because fixing the first instance did not sweep for its siblings (the M56/M62/M63 directional-sweep failure, recurring). The sweep that should have come first — `grep -rn 'test_path("..", "..", "R"' tests/testthat/` — finds exactly three sites; all three are now `skip_if_not(file.exists(...))` guarded with the limitation stated in the test rather than left silent. AC10's runtime half was split out so it always runs, including on CRAN; only the source enumeration skips.
- 2026-08-03: T5-T8 complete, all eight tasks done. `devtools::check(args = "--no-manual")` **Status: OK, 0 errors / 0 warnings / 0 notes**; full `devtools::test()` green. `--no-manual` skips nothing this branch could break: the diff changes zero roxygen (`#'`) lines and touches no `man/` file, checked rather than assumed (the M7/M57 lesson). Status in-progress -> review.

- 2026-08-04: **review round 1 returned to in-progress.** Failed: AC1 (F16 — the `corrected/naive` pin at `test-axes-corrected-se.R:42-45` moved +0.105-0.172% and is absent from the ledger; F20 — regenerated fixtures carry no M69 provenance marker), AC4 (F21 — the guard checks the citing file rather than the parsed cited range, and the range never states `naive` is priced raw), AC7 (F13 — D1's zeta2 probe is never invariance-checked). Plus F1 at 92, a regression this branch introduced: an NA diagonal now errors where `origin/master` returned `reason = "singular"`. Defect-return count for M69: 1.

- 2026-08-04: **review round 1 fixes applied.** F1 — the guard predicate is now `any(diag(sigma) <= 0, na.rm = TRUE)`, restoring the pre-M69 route for a non-finite diagonal (solve -> tryCatch -> `na_out("singular")`) instead of erroring; regression test added for both NA and NaN, and the NaN never reaches `cov2cor()` because the function returns first. F16 — `test-axes-corrected-se.R:42-45` now reads `fiml_ratio`, which is what RR13's unit-diagonal constants actually describe, and the tolerance tightens 0.01 -> 0.001; **at 0.01 the assertion could not tell the two quantities apart** (mixed 0.00185, `fiml_ratio` 0.00029, both inside the window), at 0.001 the mixed quotient fails and the asserted one keeps ~3x headroom. F21 — both AC4 assertions now run over the PARSED cited range rather than the citing file, and the range states each side's pricing. F13 — a second invariance test covers D1's zeta2/blockwise probe. F20 — both fixtures carry `regenerated: 2026-08-03 (M69)` and an extended `source`.
- 2026-08-04: all four fixes mutation-verified rather than eyeballed. Reverting `na.rm` reddens 1 assertion; stripping the naive-pricing sentence from the cited range reddens 3; the tightened F16 bar fails the mixed quotient at 0.00185 and passes `fiml_ratio` at 0.00029. Caveat recorded on F20: the stamp lives in the committed `.rds`, and a future re-run of `devel/m66-heavy-cells.R` will write the generator's own literal `source` again — whoever regenerates next stamps their own milestone.

## Decisions

- **2026-08-03 (RR15 Q1) — M69's premise is affirmed, on stronger ground than the plan had.** The Wc fold's compression `Sigma_ij = rho_ij` holds only at a unit diagonal, so the raw evaluation is not the derived formula at any scale — measured by non-homogeneity: scaling Sigma-hat by 2 scales the corrected SEs by 1.538/2.009/2.114 where any coherent variance-metric quantity gives exactly 2. RR13's own reproduction appendix derives both branches at the unit-diagonal population matrix, so the shipped raw pricing was plug-in drift from RR13's derivation, not a choice RR13 made. M68-D2 is affirmed on the same ground.
- **2026-08-03 (RR15 Q2/Q3) — both sides of the FIML ratio are priced at `cov2cor(Sigma-hat)`, and the ratio is returned rather than composed at the call site.** A mixed ratio fails the metric-only property the code documents, and would be a *regression* against shipped code: the both-raw ratio is already within 6.2e-4 of metric-only because the (N-1)/N is common-mode and cancels, where mixed pricing injects 1.7e-3. Returning `fiml_ratio` rather than a bare denominator keeps the mixed expression from being one plausible-looking line away at any future call site.
- **2026-08-03 (RR15 Q4) — RR13's BC4 is explicitly superseded, not re-satisfied.** Its operative phrase "evaluated at Sigma-hat" no longer describes the computation. What survives untouched is the missing-information pricing, which lives entirely in the `se_uncorrected` factor. Promoted cross-cutting as D-037. Same-matrix pricing *restores* agreement with RR13's published 1.441229 (1/0.6938522 = 1.44124, against the shipped both-raw 1.44034).
- **2026-08-03 (RR15 Q5/Q6/Q7) — the regression pin is positive-*diagonal* invariance, not scalar.** Exact, and strictly sharper: a scalar-only pin stays green under a "divide by the mean diagonal" pseudo-fix, which the non-constant fitted diagonal shows is materially wrong on real fits. The `n` divisor is unchanged — it cancels entirely in the FIML ratio and double-counts nothing. Non-FIML paths need only the repricing plus the shared B2 guard.

## Review

### Evidence per criterion (fresh, 2026-08-04)

- **AC1** — Ledger complete. `:203` 0.0042646 -> 0.0042719 (re-pinned); `:204` re-pinned onto `fiml_ratio` at 1.0022604, replacing the `naive/corrected` 0.9978 which after M69 straddles two matrices; `m66-corrected-se-cells.rds` regenerated (201 reps/cell, 93.4 min, R 4.6.1 / lavaan 0.6.21); `m66-bootstrap-oracle.rds` regenerated (B = 1000, 0.6 min, same provenance). Both fixtures carry fresh provenance; neither retains pre-M69 values.
- **AC2** — Both vech-oracle tests pass, 303 assertions each, on the octant and blockwise-crossed maps, at the same fitted Sigma-hat with no refit. Bar 1e-6; pre-fix redness measured 1.718e-3 and 1.731e-3, so the fence discriminates the superseded pricing by ~1700x.
- **AC3** — Calibration re-run at all three cells: complete 0.9598, 15% MCAR 0.9267, M1 MAR 1.0156, each inside its band; fixture replaced with new provenance. Prose read for meaning across all four surfaces, every passage re-measured and unchanged: "about 40%" measures 43.9%; the weak-axes sign claim holds (naive/corrected 0.982 < 1); "about 7% below" measures 7.33%; the mild-rate 0.1/0.8/1.8% are computed from the stored UNCORRECTED SE and the 1.4412 constant, neither touched by M69. `NEWS.md`'s "25-45%" band still contains 43.9%.
- **AC4** — Citation now reads `R/axes_corrected_se.R:172-178`: span 7 lines (<= 15), contains `diag(wc) <- -rowSums(wc * sigma)` and the unit-diagonal statement. Guard passes and is mutation-verified twice — a stale range fails 2 assertions, an over-wide range fails the span check.
- **AC5** — `devtools::check(args = "--no-manual")` **Status: OK, 0 errors / 0 warnings / 0 notes**; full `devtools::test()` green; `document()` produces no diff. `--no-manual` skips nothing this branch could break: the diff changes 0 roxygen lines and no `man/` file (checked, per the M7/M57 lesson).
- **AC6 (BC1)** — Enumeration run verbatim: `grep -rn "axes_corrected_se" R/` returns one production call site, `R/axes_reliability.R:1679`, which consumes `fiml_ratio`; every other hit is a comment. No code in `R/` forms a ratio of `corrected` to `naive`; the only quotient is the helper's own same-matrix `std$corrected / std$naive`.
- **AC7 (BC2)** — Invariance passes: `corrected` and `fiml_ratio` unmoved under a seeded positive diagonal and under scalar 2, max relative deviation **4.44e-16** against the 1e-6 bar. Companion holds: `naive` scales by exactly 2.000000000 under the scalar and visibly moves (> 1e-3) under the diagonal, so the invariance is not the trivial consequence of normalizing everything. D1's wiring assertion passes and is mutation-verified — reverting the call site to `corrected/naive` reddens it once per component.
- **AC8 (BC3)** — Assertion lines unedited: the diff touches no line in either range, and all seven `naive`-vs-lavaan assertions at `:67-69` and `:191-194` pass at 1e-7.
- **AC9 (BC4')** — Band arm re-run under the new pricing: 1.0013 / 1.0075 / 1.0182 at 2 / 5 / 10% MCAR, every cell inside [0.90, 1.10]. Per deviation D2 this arm is a non-regression check and not evidence for the repricing; the evidence is the live arm plus D1's wiring assertion, both green.
- **AC10 (BC5)** — A zero diagonal is refused with `reason = "nonpositive_diagonal"` before `cov2cor()` runs, all three vectors NA, no NaN; a negative diagonal takes the same door; the pre-existing singular path NAs `fiml_ratio` too. BC5's enumeration asserted mechanically: the non-success return set is exactly {singular, unidentified, indefinite}. Mutation-verified — removing the guard reddens 3 assertions.
- **AC11 (BC6)** — Sweep run within D3's domain. Seven hits in the branch diff: four state the artifact as inflation by N/(N-1), three refer to lavaan's fitted diagonal being (N-1)/N, which is a fact about `sample.cov.rescale` rather than a direction claim. No occurrence of "shrink". A second read-for-meaning pass over direction words carrying no literal returned nothing in the code diff.

### Measured against RR15's projections

| Projection (RR15) | Measured |
|---|---|
| mixed ratio = same-matrix ratio x N/(N-1) = 1.0016694 | **1.0016694** |
| superseded `corrected` scales by 1.538 / 2.009 / 2.114 at scalar 2 | **1.5384 / 2.0095 / 2.1135** |
| `naive` scales by exactly 2.000000 at scalar 2 | **2.000000000** |
| cov2cor-path floating-point drift 4.4e-16 | **4.44e-16** |
| reported FIML SE moves < 1% relative to shipped, every cell | **max +0.1585%** (xi1 at 10% MCAR) |
| `1/fiml_ratio` restores RR13's published 1.441229 | **1.441229** |

No shortfall against any stated projection.

One review-side measurement error, recorded because it nearly became a finding: an initial attempt to reproduce the scalar-2 homogeneity figures fed `axes_se_pricing()` an **un-realigned** Sigma-hat and produced 2.003 / 2.010 / 2.094, appearing to contradict RR15's 1.538. Re-measured through the pre-M69 code path, which realigns internally, the figures reproduce exactly. The error was the reviewer's, not the code's — and it is the same positional-consumption trap `axes_corrected_se()`'s own header documents.

### Consistency gate

`cairn_validate` all checks pass (48 advisories, none a failure; 47 are M7's migration-era work-log wraps). `document()` no diff; `pkgdown::check_pkgdown()` no problems; README.md in sync; no new top-level files. `NEWS.md` deliberately untouched: the existing entry describes the net shipped behaviour, its stated 25-45% band still contains the re-measured 43.9%, and no released version ever carried the superseded pricing. No DESIGN.md principle changed, so `cairn_impact` does not apply.

### Review round 1 — RETURNED to in-progress (2026-08-04)

Three lenses plus a Sonnet scorer. 23 candidate findings; **3 actioned (>= 80)**, 20 logged below threshold.

**Actioned:**

- **F16 (93) — a stale mixed-matrix ratio pin, found independently by two lenses.** `tests/testthat/test-axes-corrected-se.R:42-45` still forms `ratio <- got$corrected / got$naive` and pins it to RR13's constants, one screen above `:207-210` which states that exactly this quotient "is not a meaningful quantity to pin" after M69. Measured: the quantity moved +0.105% / +0.162% / +0.172% and survives only because its 0.01 tolerance is ~5x the shift. Against `fiml_ratio` — which is what RR13's unit-diagonal constants actually describe — the gap would be 0.00029 rather than 0.00185. **Fails AC1**, whose universal covers every assertion whose value moves.
- **F1 (92) — a regression this branch introduced.** `R/axes_corrected_se.R:231`: `any(diag(sigma) <= 0)` is `NA` when a diagonal entry is `NA`, so `if (NA)` raises "missing value where TRUE/FALSE needed". Verified against both trees: this branch errors, `origin/master` returned `reason = "singular"`. The `!is.finite()` lesson recurring (M32/M35/M60). AC10's letter survives — deviation D5 put error exits outside the NA-together contract — so this returns on the >= 90 limb, not on a criterion failure.
- **F21 (80) — the AC4 guard does not enforce AC4.** It checks `"cov2cor(Sigma-hat)"` against the whole citing file, satisfied by prose the same commit added, rather than against the parsed cited range; and the cited range `:172-178` never states that `naive` is priced raw. **Fails AC4.**

**Below threshold but bearing on ticked criteria, so their criteria are unticked too:** F13 (78) — D1 reads "the probe fits" as the octant probe plus the zeta2 probe at `:180`, and the AC7 invariance test exercises only the first, so zeta2 is never invariance-checked (**AC7**). F20 (70) — both regenerated fixtures still record `source = "M66, RR13 BC3/BC5"` with no M69 marker or regeneration date (**AC1**).

**Logged, not actioned** (18): F9 (70) the oracle's third self-check is tautological given the `V %*% Gs = I` assert, and its comment claims otherwise; F10 (68) "the two routes share no arithmetic" is false, both build Delta from `axes_se_derivs()`; F5 (68) `fiml_ratio` is an unguarded quotient; F17 (68) the D2 evidence arm sits 0.0461 against a 0.05 bound, unrecorded; F7 (65) the 60-line header now sits above the helper and `axes_corrected_se()` has none; F14 (62) the AC10 enumeration test does not enumerate `na_out(` calls its title promises; F11 (55) the artifact stated as exactly N/(N-1) on the FIML path where the diagonal is not constant; F4 (50) sibling surfaces give different reason strings for the same predicate; F8 (45) two return contracts in one comment block; F19 (42) T6's justification uses the cormat constant for FIML-path figures; F12 (35) `fiml_ratio` has no independent-oracle fence though the oracle computes both sides; F22 (32) the bootstrap corroboration is one deterministic direction, not three confirmations; F2 (30) `reason` no longer says which pricing failed; F15 (28) an underived tolerance at `:211`; F18 (25) a pre-existing stale comment; F6 (20) RR15 endorsed the non-model-implied pricing; F3 (12) pre-existing; F23 (10) the intentional rewire.

**Clean per all three lenses:** no regression of prior milestones' intent; both fixtures kept every column and provenance field; `naive` arithmetically identical to the pre-split value; AC6's enumeration holds; no generated file touched; the AC11 direction sweep clean; `1/fiml_ratio` = 1.441229 confirmed independently.
