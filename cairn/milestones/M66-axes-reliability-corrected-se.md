# M66: Corrected component standard errors for `axes_reliability()`

- **Status:** planned
- **Priority:** high
- **Depends on:** —
- **Driving RR:** RR13
- **Principles touched:** —
- **Branch/PR:** —

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

- [ ] **T1 — the corrected-covariance helper, tests-first.** New base-R helper
  building {C, J, B, K, E_ii}, the information matrix, W and W_c per AC1,
  returning corrected SEs for every fitted component. Write the AC2 anchor test
  first. **The helper must realign Σ̂ to the item-map order before use** —
  lavaan reorders model variables (`fitted(fit)$cov` came back starting at
  `item_04` at the plan gate) and the misaligned computation returns 0.0046
  where 0.01677 is right, with no error. Pin that with a test that would redden
  if the realignment were dropped.
- [ ] **T2 — wire into the components table.** Replace the `comp_ses` vector at
  `R/axes_reliability.R:1542-1547` on the raw-data and `cormat` paths. Same
  code both paths (AC1).
- [ ] **T3 — the ζ2 anchor.** Derive the block-component (`K`) row and pin a
  deterministic anchor on the crossed-blocks layout (`axes_crossed_blocks()`,
  `R/axes_reliability.R:453`), the way T1 pins ξ1/ξ2/ζ1. RR13's reproduction
  code never exercises `K`, so this is the one piece of AC1 with no worked
  value behind it.
- [ ] **T4 — FIML composition + bridge probe.** Corrected FIML SE = the
  observed-information SE ÷ the per-parameter ratio at that fit's own Σ̂.
  Bridge probe: re-fit ~20 of the fixture's stored seeds, show the per-Σ̂ and
  population-constant (1.4412) corrections agree within Monte-Carlo noise, then
  discharge AC4's [0.90, 1.10] check over all 600 stored replicates via the
  constant.
- [ ] **T5 — heavy cells: generator script + committed fixture.** Following
  M65's pattern (`devel/m65-fiml-heavy-cells.R` → committed `.rds` → live-smoke
  harness). Three new cells the existing fixture does not carry: 200 complete-
  data replicates (AC3, ~30 s), one 15% MCAR cell (AC5, ~15 min), one M1 MAR
  cell at ≥201 replicates (AC5; RR13 prices a structured MAR fit at 18–68 s, so
  1–4 h offline). Store the per-replicate corrected SE so AC5 is checkable
  without refitting.
- [ ] **T6 — pipeline-bootstrap oracle.** ≥2 complete-data draws, ≥200
  resamples each, re-computing the correlation matrix per resample; assert
  agreement within 15% relative. Must not use lavaan's `se = "bootstrap"`,
  which does not re-standardize (AC6).
- [ ] **T7 — docs, caveats, NEWS, gate.** AC7's four surfaces, then AC8's
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

## Decisions

## Review
