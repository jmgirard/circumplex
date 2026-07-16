# M21/T1: T_diag-vs-T_free calibration — analysis summary

**Question (D-009 item 3, spec §9):** is the free-scaling family's test
statistic `T = (N−1)·F̂` better calibrated to its nominal χ²_df than the
shipped unit-scaling family's — i.e., should the free family become the
preferable inference default for the CPM model test?

**Generator:** `devel/m21-t-calibration.R` (deterministic; seeds
`BASE_SEED + 12e7 + 1e6·cfg_idx + 1e4·N_idx + i`, disjoint from oracle
stages 1–3). Full results with per-replicate T vectors:
`devel/m21-t-calibration-results.rds` (500 reps × 12 cells, 2026-07-16;
run reproduced bit-identically twice).

**Design:** paired — each replicate draws one `X ~ N(0, P0)` at the stage-1
correlation truths (boundary/interior configs, N ∈ {250, 1000, 2000, 5000,
20000, 50000}), computes `R = cor(X)` once, fits **both** engines
(`scaling = "unit"` and `"free"`, variant A, m = 3) to the same `R`. df is
identical by construction (spec §4), so the per-replicate contrast
`T_free − T_unit` is exact. Kept: both accepted, both unpolished, equal df
(stage-1 KS convention). The correlation-input contract forces σ_pop = 1
(M19 lesson) — these are the only well-posed truths for this comparison.

## Result: the families are calibration-indistinguishable

| Cell | df | n | unit mean/df | free mean/df | unit rej@.05 | free rej@.05 | unit KS p | free KS p | paired ΔT̄ | cor |
|---|---|---|---|---|---|---|---|---|---|---|
| boundary_N250 | 10 | 412 | .908 | .904 | .024 | .022 | .000 | .000 | −0.04 | .9999 |
| boundary_N1000 | 10 | 488 | .855 | .850 | .029 | .029 | .000 | .000 | −0.04 | .9980 |
| boundary_N2000 | 10 | 492 | .896 | .895 | .028 | .028 | .000 | .000 | −0.02 | .9981 |
| boundary_N5000 | 10 | 496 | .892 | .889 | .036 | .034 | .000 | .000 | −0.03 | .9999 |
| boundary_N20000 | 10 | 500 | .937 | .935 | .042 | .042 | .003 | .002 | −0.02 | 1.0000 |
| boundary_N50000 | 10 | 500 | .998 | .997 | .046 | .046 | .996 | .992 | −0.02 | 1.0000 |
| interior_N250 | 10 | 378 | .884 | .881 | .026 | .026 | .000 | .000 | −0.03 | .9996 |
| interior_N1000 | 10 | 485 | .949 | .947 | .043 | .043 | .016 | .014 | −0.01 | 1.0000 |
| interior_N2000 | 10 | 500 | .995 | .994 | .048 | .048 | .998 | .996 | −0.01 | 1.0000 |
| interior_N5000 | 10 | 500 | .999 | .998 | .042 | .042 | .901 | .921 | −0.01 | 1.0000 |
| interior_N20000 | 10 | 500 | 1.000 | .999 | .054 | .054 | .604 | .542 | −0.01 | 1.0000 |
| interior_N50000 | 10 | 500 | .997 | .996 | .056 | .056 | .702 | .685 | −0.01 | 1.0000 |

- **Paired difference is negligible everywhere:** mean `T_free − T_unit`
  ∈ [−0.044, −0.011] on a df = 10 scale (≤ 0.5% of df); paired correlation
  ≥ .998 in every cell. The free family's p extra σ nuisances sit at ≈1
  under the correlation-input contract and absorb essentially none of the
  finite-N misfit.
- **Identical calibration regime:** both families are mildly conservative
  at small/mid N (mean/df ≈ .85–.95; rejection at α = .05 runs .02–.04;
  KS rejects χ² at boundary N ≤ 20000, interior N ≤ 1000) and both reach
  nominal together (boundary N = 50000: mean/df ≈ .998, rej ≈ .046, KS p
  ≈ .99; interior from N = 2000). No cell separates the families in any
  metric at 500 reps.
- **Nesting check:** free nests unit (σ = 1), so `T_free ≤ T_unit` up to
  optimizer tolerance; 3 of 5,751 used replicates (0.05%) violate by an
  optimizer tail (max +5.5, boundary_N2000) — noted, immaterial to every
  cell summary.
- **Cross-reference:** the committed one-family summaries
  (`m19-free-coverage-results.rds` ks_T; `m4-coverage-oracle-results.rds`
  ks_T, df = 10) show the same regime; stage-1 diag KS values are not
  directly comparable cell-to-cell (bootstrap-path fits, N ≤ 1000) but
  agree in direction.

## Implication for the decision (RB input, not the decision itself)

The measured evidence gives **no calibration basis for preferring the free
family** as the model-test inference default: T is the same statistic in
practice at every well-posed truth. Against that null benefit stand the
free family's known costs: bordered-Hessian SE singularity in ~52–55% of
N = 250 fits (M19/D-010), p extra parameters, and analytic σ² CIs that are
never trustworthy (D-009). The natural reading is **keep the unit/diag
family as the inference default**; the free family remains what D-009
admitted it for — exact reproduction of published CIRCUM/CircE output —
with its D-010 caution ladder unchanged. Final call: Fable RB (M21/T2).

**Scope caveat for the RB:** this comparison is only defined at correlation
truths with σ_pop = 1 (there is no covariance-input path; `cor()` discards
variances). If a covariance-matrix input ever ships (deferred candidate,
D-009 item 4), the free family's T at non-unit σ truths is unmeasured and
this decision should be revisited there.
