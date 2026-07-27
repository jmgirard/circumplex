# M66: Corrected component standard errors for `axes_reliability()`

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** RR13
- **Principles touched:** —
- **Branch/PR:** `m66-axes-reliability-corrected-se` / [PR #92](https://github.com/jmgirard/circumplex/pull/92)

## Goal

Replace `axes_reliability()`'s correlation-as-covariance component standard
errors with the Browne/Cudeck corrected asymptotic covariance, on every input
path, so the reported SEs are calibrated rather than approximate.

## Scope

**In:** the corrected asymptotic covariance derived in RR13, implemented in base
R and evaluated at the fitted Σ̂; applied to every SE the `components` table
reports (ξ1, ξ2, ζ1, and ζ2 when fitted) on the raw-data, `cormat`, and FIML
paths; deterministic anchors, complete-data and FIML empirical calibration, a
beyond-mild-missingness check, an independent pipeline-bootstrap oracle; and the
doc/NEWS surfaces the correction falsifies.

**Out:**
- The exact-route escalation (Γ_R from the saturated fit's observed-information
  acov, delta-transformed) → its own milestone, planned only if AC5 measures a
  cell outside its band. The band is never widened (RR13 BC5).
- A Satorra–Bentler-style scaled test statistic for the global χ² → ROADMAP
  candidate (RR13 B-1); this milestone quantifies the χ² direction in prose
  only and leaves the statistic uncorrected.
- The seven M65 review fold-ins (EM-stall predicate, fit-measure guard, thin
  overlap, `@return`, OLS-shadow assertion) → **M67**.
- Any change to point estimates, reliabilities, SEm, df, or the model class.
  `correlation = TRUE`, robust/sandwich SEs, and unit-variance refitting stay
  rejected on RR13's measured grounds.

## Acceptance criteria

- [ ] **AC1 (BC1, the correction).** Corrected component SEs are computed as
  SE_corr = √(2·tr(W_c Σ̂ W_c Σ̂)/n), where W = ½ Σ̂⁻¹(Σ_s c_s M_s)Σ̂⁻¹ with
  {M_s} the model's derivative matrices {C, J, B (, K), E_11…E_pp}, c the
  target parameter's row of (Δ′VΔ)⁻¹, and W_c equal to W off the diagonal
  with diag(W_c)_i = −Σ_{j≠i} W_ij Σ̂_ij. Implemented in base R (no new
  dependency; lavaan/OpenMx stay Suggests), evaluated at the fitted Σ̂,
  identical code on the data and cormat paths, and applied to every SE the
  `components` table reports (ξ1, ξ2, ζ1 and ζ2 when fitted).
- [ ] **AC2 (BC2, deterministic anchors).** Fitting the exact probe population
  matrix (8 scales × 3 items, ξ1=.35, ξ2=.10, ζ1=.08) at n = 600: the
  uncorrected SE(ξ1) must equal 0.01677 within 2e-4 and the corrected SE(ξ1)
  must equal 0.01164 within 2e-4; the corrected/uncorrected ratio for
  (ξ1, ξ2, ζ1) must equal (1/1.441, 1/1.067, 1/0.997) within 0.01 each.
- [ ] **AC3 (BC3, empirical calibration, complete data).** Over ≥200
  complete-data replicates at the probe population (the fixture seeds may be
  reused), mean corrected SE(ξ1) / empirical SD(ξ̂1) ∈ [0.90, 1.10] (band
  ≈ ±2.8 MC SEs; measured 1.005 over 50 replicates in this review).
- [ ] **AC4 (BC4, FIML composition).** The FIML path's corrected SE is the
  observed-information SE divided by the same per-parameter ratio evaluated
  at Σ̂. Against the committed 200-replicate fixture at 2, 5, and 10% MCAR,
  mean corrected FIML SE(ξ1) / empirical SD ∈ [0.90, 1.10] in every cell
  (measured 1.001/1.008/1.018).
- [ ] **AC5 (BC5, beyond-mild-missingness check).** One cell each at 15% MCAR
  (the BC14 headline fixture population) and mechanism M1 MAR with enough
  replicates that the MC SE of the SD is ≤ 5%: corrected SE / empirical SD
  ∈ [0.85, 1.15]. If either cell fails, escalate to the exact route
  (Γ_R from the saturated fit's observed-information acov, delta-transformed)
  or the pipeline bootstrap — the band must not be widened.
- [ ] **AC6 (BC6, independent oracle).** On ≥2 complete-data draws, a pipeline
  bootstrap (≥200 resamples, re-computing the correlation matrix per
  resample) must agree with the corrected SE(ξ1) within 15% relative; the
  test must not use lavaan's `se = "bootstrap"`.
- [ ] **AC7 (docs state the corrected contract).** The printed
  `axes_se_caveat` drops its standard-error clause and keeps its global-fit
  sentence unchanged; `axes_fiml_se_caveat` is unchanged; the roxygen passages
  the correction falsifies are rewritten — both the `@details`
  correlation-as-covariance block (`R/axes_reliability.R:663-682`) and the
  `# Missing data` sentence claiming the FIML SEs "remain approximate for the
  same correlation-as-covariance reason" (`R/axes_reliability.R:744-745`) —
  with RR13 B-1's χ² figures (E[T] = 261.1 against df = 273) stated in roxygen;
  and `NEWS.md` names the change, its direction, that point estimates,
  reliabilities and SEm are unchanged, and that Strack et al.'s printed LISREL
  SEs carry the uncorrected approximation. Verified against rendered `print()`
  output and the built Rd, never the source strings alone.
- [ ] **AC8 (gate clean).** `devtools::test()` and
  `devtools::check(args = "--no-manual")` clean, plus a built PDF manual
  (`R CMD Rd2pdf --no-preview --force`), since AC7 changes roxygen.

### Deviations from RR13

| BC | Disposition |
|---|---|
| BC7 | Not ingested. It binds M65, not this milestone — RR13's own preamble says "BC1–BC6 bind the corrective milestone; BC7 binds M65 if it ships first". M65 shipped 2026-07-27 and discharged it, recording the RR12 BC13 replacement in its own Deviations table. |

## Coverage

- AC1 → T1, T2, T3, T4
- AC2 → T1
- AC3 → T5
- AC4 → T4, T5
- AC5 → T5
- AC6 → T6
- AC7 → T7
- AC8 → T7

## Tasks

- [x] **T1 — the corrected-covariance helper, tests-first.** New base-R helper
  building {C, J, B, K, E_ii}, the information matrix, W and W_c per AC1,
  returning corrected SEs for every fitted component. Write the AC2 anchor test
  first. **The helper must realign Σ̂ to the item-map order before use** —
  lavaan reorders model variables (`fitted(fit)$cov` came back starting at
  `item_04` at the plan gate) and the misaligned computation returns 0.0046
  where 0.01677 is right, with no error. Pin that with a test that would redden
  if the realignment were dropped.
- [x] **T2 — wire into the components table, all three paths.** Replace the
  `comp_ses` vector at `R/axes_reliability.R:1542-1547`: raw and `cormat` take
  the corrected SE directly, FIML takes the multiplicative composition
  (observed-information SE × corrected/naive at Σ̂). Amended from "raw and
  cormat only" at implement — see the work log; the paths cannot be migrated
  separately.
- [x] **T3 — the ζ2 anchor.** Derive the block-component (`K`) row and pin a
  deterministic anchor on the crossed-blocks layout (`axes_crossed_blocks()`,
  `R/axes_reliability.R:453`), the way T1 pins ξ1/ξ2/ζ1. RR13's reproduction
  code never exercises `K`, so this is the one piece of AC1 with no worked
  value behind it.
- [x] **T4 — FIML calibration evidence (AC4).** The composition itself moved to
  T2. Bridge probe: re-fit ~20 of the fixture's stored seeds, show the per-Σ̂
  and population-constant (1.4412) corrections agree within Monte-Carlo noise,
  then discharge AC4's [0.90, 1.10] check over all 600 stored replicates via
  the constant.
- [x] **T5 — heavy cells: generator script + committed fixture.** Following
  M65's pattern (`devel/m65-fiml-heavy-cells.R` → committed `.rds` → live-smoke
  harness). Three new cells the existing fixture does not carry: 200 complete-
  data replicates (AC3, ~30 s), one 15% MCAR cell (AC5, ~15 min), one M1 MAR
  cell at ≥201 replicates (AC5; RR13 prices a structured MAR fit at 18–68 s, so
  1–4 h offline). Store the per-replicate corrected SE so AC5 is checkable
  without refitting.
- [x] **T6 — pipeline-bootstrap oracle.** ≥2 complete-data draws, ≥200
  resamples each, re-computing the correlation matrix per resample; assert
  agreement within 15% relative. Must not use lavaan's `se = "bootstrap"`,
  which does not re-standardize (AC6).
- [x] **T7 — docs, caveats, NEWS, gate.** AC7's four surfaces, then AC8's
  checks including the PDF manual.

## Work log

- 2026-07-27: created by /milestone-plan. `Driving RR: RR13`; BC1–BC6 ingested verbatim as AC1–AC6.
- 2026-07-27: plan-gate probe — RR13's reproduction appendix reproduces exactly (naive 0.16881764, ratio 1.441229), and the shipped `axes_fit_cormat()` path hits AC2's anchors with 8–14× tolerance margin (naive 0.0167459, equal to lavaan's own reported SE to 7 digits; corrected 0.0116264; ratios 1.4403/1.0673/0.9969). The residual gap to RR13's population-evaluated figures is lavaan's (N−1)/N input rescale.
- 2026-07-27: plan-gate hazard recorded for T1 — lavaan reorders model variables, so `fitted(fit)$cov` is not in item-map order; the misaligned computation returns a plausible wrong number (0.0046 vs 0.01677) with no error and no warning.
- 2026-07-27: criteria audit (fresh-context [O]) fixed four findings before AC1–AC8 were written: AC7 gained the `# Missing data` roxygen sentence it had missed, stopped asking `axes_fiml_se_caveat` to drop a clause it does not contain, stopped re-quantifying a χ² figure M65 already shipped, and the Coverage/tasks now state that AC3 and AC5 need new fixture cells. `DESIGN.md` carries no IP/GP block, so `Principles touched: —`.
- 2026-07-27: plan gate chose the delta-method corrected asymptotic covariance over lavaan `correlation = TRUE`, robust/sandwich SEs, and unit-variance refitting because RR13 measured each of the three to fail (different model class moving ξ̂1 ≈5 empirical SDs; no fix; foreclosed by RR12 §9); falsified by AC3 or AC5 measuring a calibration outside its band.
- 2026-07-27: plan gate chose the ~20-seed bridge probe for AC4 over full 600-replicate regeneration and over the population constant alone, because the fixture stores no Σ̂ and the probe buys the per-Σ̂ claim for minutes rather than ~45 min of refitting; falsified by the probe measuring per-Σ̂ and constant corrections disagreeing by more than Monte-Carlo noise, which would force the regeneration.
- 2026-07-27: plan gate chose to add a ζ2 deterministic anchor (T3) over shipping the blockwise branch on its existing structural unit tests, because AC1 binds ζ2 while RR13's worked code omits `K` entirely; falsified by nothing — the rejected alternative was strictly weaker evidence.
- 2026-07-27: plan gate chose to spin the AC5 exact-route escalation out as its own milestone over taking it inside M66, because its size is conditional on a measurement not yet taken; falsified by an AC5 cell missing [0.85, 1.15], which triggers that plan.
- 2026-07-27: started (/milestone-implement). Branch `m66-axes-reliability-corrected-se` cut from master at 704adba3; no dependencies to verify, no other milestone in-progress. Status planned→in-progress.
- 2026-07-27: implement gate settled three open API choices — `details$se_uncorrected` retains lavaan's reported SEs (auditability without a supported opt-out); a Σ̂ the correction cannot invert gives NA SEs with a warning and a named reason, never a silent fallback to the uncorrected number; and no `se =` argument, since an opt-out would make a value the package documents as miscalibrated into a permanent exported surface (D-035 already rules the change a fix, not an interface change).
- 2026-07-27: T1 done. `R/axes_corrected_se.R` (new): `axes_se_derivs()` builds {C, J, B, K, E_11..E_pp}; `axes_corrected_se()` returns naive + corrected SEs per component and a `reason`. Anchors: naive SE(ξ1) 0.0167459, corrected 0.0116264, ratios 1.4403/1.0673/0.9969 — all inside BC2, and the naive branch matches lavaan's own reported SE to <1e-7, which fences the derivative structure independently. `devtools::test()` 0 failures, 4263 passing; the 4 warnings are the pre-existing test-ci_accuracy.R diagnostic cautions (0 warnings across both axes test files).
- 2026-07-27: T2+T3 verified and ticked — `test-axes-fiml.R` 193 assertions clean, `devtools::test()` 0 failures / 4290 passing (the 4 warnings are the pre-existing test-ci_accuracy.R cautions). The open question the checkpoint left — whether `fiml_se < lw_se` survives the correction, which is not preserved by construction since each path divides by a ratio at its own Σ̂ — is answered YES, measured replicate by replicate on both metrics.
- 2026-07-27: T4 done (AC4). Fixture arm, all 200 replicates per cell: corrected FIML SE / empirical SD = **1.0013 / 1.0075 / 1.0182** at 2/5/10% MCAR, reproducing RR13's 1.001/1.008/1.018 to the digit, all inside [0.90, 1.10]. RR13 B-4's listwise-under-deletion columns give the same check across an order of magnitude of effective N — **0.9864 / 0.9974 / 1.0197** at n_complete ≈ 370 / 175 / 48 — at no simulation cost; one of 200 replicates at 10% leaves 32 complete cases for 24 items and does not fit, so that arm is `na.rm` with a ≥195 usable floor.
- 2026-07-27: T4 bridge probe — **the two corrections do not simply agree, and the record says so.** Over 20 re-fits per rate the per-Σ̂ ratio the shipped code uses runs **1.4499 / 1.4501 / 1.4507** against the population constant **1.4412**: systematically above it, same sign and size at all three rates, so a finite-sample offset rather than scatter (~0.6% in the ratio, ~2.0% in the corrected SE). The plan's falsifier was disagreement beyond Monte-Carlo noise; the MC SE of an empirical SD over 200 replicates is ≈3.6%, so the offset sits below the noise of the very statistic AC4 is computed from and cannot change its verdict at this replicate count. **Not** triggered, so no 600-replicate regeneration. Direction recorded because it matters: the constant is the CONSERVATIVE proxy — the shipped composition reports a slightly smaller SE and calibrates marginally nearer 1 than the fixture-arm numbers above.
- 2026-07-27: minor plan amendment — T2 now wires **all three** paths and T4 keeps only the FIML calibration *evidence*. The split shipped an inconsistent intermediate state: `test-axes-fiml.R`'s live-smoke harness compares the listwise SE against a stored fixture value and asserts `fiml_se < lw_se` ACROSS paths, so correcting one path and not the other fails tests that are right to fail. Criteria and Coverage unchanged.
- 2026-07-27: T2 kept M65's fixture rather than regenerating it. Its `lw_se`/`fiml.se` columns hold pre-correction SEs, which is exactly what `details$se_uncorrected` still carries, so the live-smoke harness now reads that field and goes on catching drift in lavaan's own observed-information SEs. Regenerating to corrected values would have left that check comparing this package's output against itself. M65's `expect_lt(fiml_se, lw_se)` is now asserted on BOTH metrics, because the two paths divide by ratios evaluated at their own Σ̂ and the ordering is not preserved by construction.
- 2026-07-27: T3 ζ2 anchor derived on the crossed-blocks layout (8 scales × 3 items, ξ1=.35, ξ2=.10, ζ1=.08, ζ2=.05, n=600): naive SE(ζ2) 0.0042551 — equal to lavaan's own `BS1` SE to 7 decimals, which is the independent fence on the `K` derivative matrix RR13's appendix never exercises — corrected 0.0042646, ratio 0.9978. All four naive SEs match lavaan at this population. Swapping `K` for the same-scale indicator reddens 6 assertions. The corrected ζ2 literal is recorded in-test as a regression pin, not an oracle, since it comes from this implementation.
- 2026-07-27: T5 generator written (`devel/m66-heavy-cells.R`, M65's script→`.rds`→live-smoke pattern) and the full 201-replicate run launched; 6-replicate smoke took 2.3 min on 6 workers, so the full run projects to ~77 min. The smoke's calibrations (0.888 / 1.334 / 1.415) are NOT evidence of miscalibration: the MC SE of an SD over 6 replicates is 32%, and all three sit within ~1.3 MC SEs of 1.0. What the smoke does establish is that the correction reaches every cell — `se/se_naive` = 0.698/0.699/0.692 against the expected 1/1.44 — which is the thing a 6-rep run can actually settle. The new fixture stores BOTH SEs per replicate, closing the gap that forced T4 to bridge with a population constant.
- 2026-07-27: T5 done (AC3, AC5). 201 replicates × 3 cells, 100.2 min on 6 workers, 201/201 usable in every cell and ξ̂1 unbiased in each, so no selection effect stands behind any number. **BC3 complete data 0.9584** ∈ [0.90, 1.10]; **BC5a 15% MCAR 0.9255** and **BC5b M1 MAR 1.0152**, both ∈ [0.85, 1.15]. No cell missed its band, so RR13's exact-route escalation is NOT triggered and nothing spins out. Fixture `tests/testthat/fixtures/m66-corrected-se-cells.rds` (15K) stores xi1, corrected SE and naive SE per replicate.
- 2026-07-27: T5 — the complete-data cell's calibration is 0.9584 where RR13 measured 1.005 over 50 replicates, and the gap is in the DENOMINATOR, not the correction: mean corrected SE 0.011620 against the closed-form 0.011639 (agreement 0.2%), while the empirical SD came out 0.012124 against RR13's 0.01158. Two independent ~200-replicate SDs differing by 4.7% is 1.3 MC SEs — ordinary. Pinned in-test against the closed form as well as the band, since a band alone would tolerate a systematically wrong SE paired with a coincidentally matching SD.
- 2026-07-27: T5 — **a real limitation, recorded rather than absorbed by the band's width.** At 15% cellwise MCAR the correction runs ANTI-CONSERVATIVE: calibration 0.9255, i.e. reported SEs understate true variability by ~7.5%, which is 2.1 MC SEs below 1 and not readable as noise. It reverses the direction RR13 measured at 2/5/10% (1.001/1.008/1.018, conservative), so it is a trend reversal rather than a continuation, and it is the standardization-constant residual growing faster than the metric error shrinks. Inside BC5's band — which RR13 set wider for exactly this regime — so AC5 passes as written. Documented in the roxygen (heavy missingness named as the least trustworthy regime, with a resampling interval recommended there) and pinned in-test at (0.88, 1.0) so a future change that worsens it is caught instead of hidden by the band.
- 2026-07-27: T6 done (AC6). Stored oracle `tests/testthat/fixtures/m66-bootstrap-oracle.rds` (3 draws, B = 1000, `devel/m66-bootstrap-oracle.R`): bootstrap vs corrected SE **2.02% / 2.21% / 2.52%**, against **32.4% / 30.0% / 26.7%** vs the uncorrected value — so the oracle discriminates the two decisively, which is the claim BC6 exists to make, not merely that it lands under 15%.
- 2026-07-27: T6 — **the first BC6 run FAILED (15.06% at seed 1001) and the correction was not the cause.** The analytic value 0.011841 reproduces RR13's published 0.01184 for that same draw exactly; what was short was the yardstick. A bootstrap SD over B resamples carries ~1/√(2B) noise — ~5% at B = 200 — and the running SD showed 0.013625 at B = 200 against 0.012967 by B = 1000, so noise alone moved a genuine gap past the bar. Fixed by raising B to 1000 (noise ~2.2%), which BC6 permits outright ("≥ 200 resamples"); **the 15% bar was never touched** — a criterion adjusted to fit the result it exists to test is worthless.
- 2026-07-27: T6 — reseeding audited before trusting the fixture, because the generator seeds each resample as `seed*1000 + b` and consecutive-integer seeds could correlate resamples and bias the SD downward, which would make AC6 pass for the wrong reason. Measured against a continuing stream at B = 1000: **+2.03% (seed 1002) and −2.20% (seed 1003)** — opposite signs, both ≈ the 2.2% MC SE, so no systematic bias. Seed 1001's two B = 1000 estimates differ by 7.3% (~3 MC SEs) with no directional pattern across the three, so BC6's true agreement sits in roughly 2–9% depending on the resample stream; every measurement taken is inside the bar and none is near it.
- 2026-07-27: T6 — a near-miss worth the line: the first T6 run reported `FAIL 0 | PASS 87` **while the new test never executed**. `skip_on_cran()` skips whenever `NOT_CRAN` is unset, which a bare `testthat::test_file()` does not set; only the `SKIP 1` and an assertion count identical to the pre-T6 run gave it away. Fourth instance of the repo's "green because it never looked" family (M31 vdiffr auto-skip, M7 `--no-manual`, M39 CI-skipped baseline). `devtools::test()` does set it, so the suite runs the test — verified by running it rather than assumed.
- 2026-07-27: T7 gate — `R CMD check` **Status OK, 0 errors / 0 warnings / 0 notes** (15m 13s, vignettes rebuilt clean). PDF manual built directly (`R CMD Rd2pdf --no-preview --force`), 78 pages, exit 0; the pdfTeX destination warnings (`set.seed`, `summary`, `ggplot2::coord_radial`) are pre-existing cross-references, none from M66's additions. Grepped the check log for `checking PDF version of manual`: **0 hits, confirming the step did not run inside `check()`** — the M7/M57 lesson applied as a measurement rather than recalled, which is why the manual is built separately.
- 2026-07-27: T7 — **the AC7 Rd guard was the M7 trap, written fresh.** `readLines(test_path("..","..","man",...))` cannot open the file under `R CMD check`, because installed packages carry `help/` not `man/`, and it errored the entire check on its first run. The repo already had the dual-source pattern (`man/` in the dev tree, `tools::Rd_db()` once installed) in two other test files with this very lesson written above it; it is now used here, with an `nchar() > 1000` floor so neither source can pass vacuously. M7's original SKIPPED silently on that path — erroring was the lucky half.
- 2026-07-27: T7 — **fresh-context [O] guard review found five real weaknesses my own mutation testing could not**, which is the point of the rule that an author never certifies their own guard. A mutation only ever tests the REVERT; it says nothing about a reworded reintroduction or a revert-by-deletion. Findings, all applied: (F1) the absence asserts pinned the OLD vocabulary — old text said "component SEs overstate", new text says "standard errors", so the same false claim rewritten in current words passed all four; now pinned at the verb stems `overstat`/`understat`, absent from every printed note. (F2) `"order-of-magnitude guidance"` was never in the PRINTED caveat (only the roxygen), so it could not fail on any reversion of the string it guards, while the comment above it claimed every falsified phrase was pinned; kept for forward value, now labelled honestly, and a phrase the old caveat did carry added beside it. (F3, F5) two asserts pinned noun phrases without their predicates and stayed green under a rewrite that INVERTED the claim — 261.1 and 273 are the same numbers whether the χ² is corrected or not. (F4) **the one true hole:** the FIML section's positive claim was unpinned, so deleting it satisfied the absence assert BY DELETION while the next sentence kept the residual assert green — an Rd silent on whether the FIML path is corrected, suite green. Verified closed by mutation: deleting that sentence now reddens.
- 2026-07-27: T7 — coverage NOT claimed for two AC7 clauses, per the same review: `axes_fiml_se_caveat`'s unchanged-ness is covered by the pre-existing `test-axes-fiml.R:941`, not by these guards, and the `NEWS.md` clause has no assert anywhere — it is read at the review gate, matching AC7's own "verified against rendered print() output and the built Rd" scoping.
- 2026-07-27: T7 — a failed mutation reported the unmutated suite's result (`107 PASS`) because its anchor string never matched; only an explicit `assert` in the mutation script exposed it. Second instance this milestone of a probe whose own validity had to be established before its output meant anything (after the `skip_on_cran` silent skip at T6).
- 2026-07-27: all tasks done; status in-progress→review. Final gates: `devtools::test()` 0 failures / 4362 passing / 0 skips (4 warnings, all the pre-existing test-ci_accuracy.R cautions); `R CMD check` Status OK, 0/0/0; PDF manual 78 pages. Acceptance-criteria boxes deliberately left UNTICKED — they are review's to tick against fresh evidence recorded in the Review section (AC fencing).
- 2026-07-27: T7 doc content done (box unticked — AC8's `check()` + PDF manual run at the end). `axes_se_caveat` drops its SE clause and keeps the global-fit sentence verbatim; `axes_fiml_se_caveat` unchanged as AC7 requires; both falsified roxygen passages rewritten, including the `# Missing data` sentence claiming the FIML SEs "remain approximate for the same correlation-as-covariance reason", which is now false in both halves. RR13 B-1's E[T] = 261.1 against df = 273 stated in roxygen. NEWS entry under Breaking changes. Verified against rendered `print()` output and the built Rd, not source strings — and each falsified phrase is pinned as an ABSENCE assertion, the directional half the M56/M63 stale-claim lesson says the sweep keeps missing. The `cpm_gradient` link warning from `document()` is pre-existing (R/cpm_fit.R is byte-untouched by this branch).
- 2026-07-27: T1 mutation record. Reddening: dropping the Σ̂ realignment fails 8 assertions, cos(2Δ) for cos(Δ) fails 8, dropping the W_c diagonal fails 3. NOT reddening, and correctly so: stripping C's diagonal leaves both SEs bit-identical to 15 decimals, because the diagonal direction is spanned by the free {E_ii} and the change is a unit-triangular reparameterization of nuisance parameters. The code comment claiming that diagonal was load-bearing was false and is corrected in place (the M36/M60 lesson family); the null is recorded there so a later session does not re-chase it.

## Decisions

## Review
