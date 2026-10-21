# M167: CPM interval coverage per component, the record

This file records the runs of `devel/m167-interval-oracle.R`. The design and
the pass rule are the pre-registration block at the top of that script. The
figures below come from `devel/m167-interval-results.rds` (the full run),
`devel/m167-interval-control.rds` (the control run) and
`devel/m167-interval-confirm.rds` (the confirmation run).

## What ran

- The full run started from commit 908ed678 on 2026-10-21. It took 97.5
  minutes on 17 cores, with 500 replicates per cell and 1000 resamples per
  bootstrap.
- The results file records commit 8a2c6fb2, because the script reads `HEAD`
  when it saves. Between the two commits only roxygen comment lines in
  `R/cpm_fit.R` changed (`git diff 908ed678 8a2c6fb2 -- devel R src`). So the
  run used the code of 908ed678.
- The control run scored B1 and B2 only, at the six raw-data cells, under the
  M4 seeds. It ran before the design review and took 33 minutes. The full run
  reproduces its B1 and B2 figures at five of the six cells. At interior
  N = 250, two replicates errored in the full run (404 accepted fits against
  406), and the pooled B2 figures there differ by up to .006.
- The confirmation run used fresh seeds (20261022) at the raw-data cells where
  a pass needed confirming. Its results are under "Confirmation" below.
- The prototypes pass all 11 checks in `devel/m167-interval-checks.R`.
- Run times are the elapsed times each run printed. The .rds files do not
  store them.

## Path each figure measured

- Raw-data cells (N = 250, 500, 1000) draw rows with `cpm_simulate()` from the
  exact truth fit. That function draws multivariate-normal factors and
  uniquenesses. B1, F3 and Bb resample those rows. B2, F1, F2p, F2bc, F2b and
  P1 use the rows' correlation matrix, as a correlation-matrix user would.
- Correlation-matrix cells (N = 2000, 5000, 20000) draw one sample correlation
  per replicate as `cov2cor()` of a Wishart(N - 1, P) draw. That is the exact
  distribution of `cor()` of N multivariate-normal rows.
- So every figure is a normal-data figure for p = 8 octant scales, equal
  zeta = .75, m = 3, variant A, and the two beta truths. Nothing here measures
  non-normal data, unequal communalities, other p or m, other variants, or the
  free-scaling family.
- Coverage is conditional on the engine accepting the fit. At the boundary
  truth the run excluded 40, 12 and 9 of 500 fits at N = 250, 500 and 1000,
  and 3, 0 and 0 above that. At the interior truth it excluded 94 (2 more
  errored), 52 and 10, then 0.

## AC2 control

Pooled as M4 pooled it, every B1 and B2 entry at the six M4 cells lies within
.021 of the M4 record (.020 at three decimals). The largest difference is B2 beta at boundary N = 500,
.869 against .849. The B1 differences are at most .007. The accepted-fit
counts differ slightly from M4's, for example 448 against 452 at interior
N = 500. This run did not look into why.

## The shipped bootstrap (B1) per weight

The table gives coverage per beta weight. In parentheses is the share of
replicates whose interval lies below or above the truth, for the side that
carries the misses.

| Cell | beta_0 | beta_1 | beta_2 | beta_3 |
|---|---|---|---|---|
| boundary N=250 | .741 (below .26) | .928 | .785 (above .22) | .741 (above .22) |
| boundary N=500 | .674 (below .33) | .930 | .732 (above .27) | .740 (above .26) |
| boundary N=1000 | .656 (below .34) | .949 | .756 (above .24) | .731 (above .27) |
| interior N=250 | .844 (below .15) | .953 | .906 | .869 (above .13) |
| interior N=500 | .806 (below .19) | .949 | .884 (above .11) | .893 (above .11) |
| interior N=1000 | .814 (below .19) | .959 | .896 (above .10) | .857 (above .14) |

Against the band [.90, .98], before the Monte Carlo allowance:

- At the boundary truth, beta_0, beta_2 and beta_3 are outside the band at
  every N.
- At the interior truth, beta_0 and beta_3 are outside it at every N, and
  beta_2 at N = 500 and 1000.
- beta_1 is inside the band at every raw-data cell.

The largest shortfall is on the largest weight, beta_0 (tied with beta_3 at
boundary N = 250). Nearly all its misses fall below the truth: 0.5% of its
intervals lie above it at interior N = 250, and none elsewhere. The misses of
beta_2 and beta_3 fall above it.

B1 communalities miss above the truth. At N = 250 they cover .79 to .85 per
scale at the boundary truth and .73 to .80 at the interior truth. Of their
intervals 15 to 26% lie above the truth and 0 to 2% below. At N = 500 they
cover .85 to .91, with 9 to 15% above.

B1 angles pass at N = 250 and 1000 under the pass rule. At boundary N = 500
the LM angle covers .871 and fails.

## Why: the diagnostics

- The estimator is biased. At the boundary truth the bias of beta_0-hat is
  .7 to 1.05 of its SD from N = 250 to 2000, and still .75 SD at N = 20000.
  At the interior truth it is .65 to .80 SD at N <= 1000 and .08 SD at
  N = 20000.
- The bootstraps see part of that bias. At the boundary truth the mean B1
  replicate shift of beta_0 is -.011 to -.012, against an estimator bias of
  -.023 to -.030. At the interior truth it is -.004 to -.008, against -.012
  to -.021. The F2 shift is similar.
- The variant-B arm refits the same data with the angles fixed at theory. It
  removes the beta_0 bias: under .07 of its SD in every raw-data cell. Its
  Heywood rate is 0 to 3% at the boundary truth. So the pre-registered reading
  confirms the angle and communality mode as the cause at all three N there.
  At the interior truth its Heywood rate is 37%, 8% and 1% at N = 250, 500
  and 1000, so the reading confirms the mode only at N = 1000.
- At N = 250, 500 and 1000, 78%, 59% and 25% of accepted fits are Heywood
  fits at the boundary truth, and 91%, 61% and 21% at the interior truth.
- The Wald standard error is defined on 40%, 56% and 85% of accepted fits at
  the boundary truth, and on 27%, 60% and 87% at the interior truth.

## Verdict under the pre-registered rule

In the strict form, no (method, path, parameter family) passes at every cell
listed for its path. In the ladder form ("passes from N"):

| Path | Method | Family | Passes from N | Unconditional | Cells passed |
|---|---|---|---|---|---|
| raw data | B1 | angles | 1000 | 1000 | 250, 1000 |
| raw data | P1 | communalities | 1000 | 1000 | 1000 |
| correlation matrix | F2bc | weights | 5000 | 5000 | 5000, 20000 |

Every other entry fails at its largest listed N, so its ladder reads "never".
Some entries pass at cells below the top of the ladder:

- F2p angles and communalities pass at N = 2000 and 5000.
- F2bc and P1 communalities pass at N = 1000, 2000 and 5000.
- F1 communalities pass at N = 2000.

None of these passes at boundary N = 20000. There the weak angle and
communality mode of the estimator remains: the angle SDs are 3 to 8 degrees
and the beta_0 bias is .75 of its SD.

The per-component tables give these reasons for the other failures:

- F1 communality intervals cover .90 to .98 at N <= 1000 on the fits with a
  defined SE. But up to 44% (boundary) and 84% (interior) of them are vacuous
  (width over .5), so they fail the vacuous-interval rule. Up to 60% and 73%
  of fits have no F1 interval.
- P1 communality intervals are vacuous in up to 14% (boundary) and 54%
  (interior) of fits at N = 250.
- The basic bootstrap (Bb, F2b) covers communalities at only .47 to .79 at
  N <= 1000.
- At the boundary truth, F2bc is the only method whose beta_0 passes
  unconditionally at any cell: .894 at N = 5000 and .888 at N = 20000, both
  inside the allowance and below .90. B2 and F1 pass beta_0 only
  conditionally, at N = 250 and 1000, where 60% and 16% of fits have no
  interval. P1, B2 and F1 cover beta_0 at .83 to .87 from N = 2000 to 20000.

## Confirmation

The confirmation run (`M167_CONFIRM=1`, 44 minutes) re-ran the raw-data
cells where the main run passed an entry: N = 250 and 1000, both truths,
fresh seeds 20261022, 500 replicates.

| Path | Method | Family | N | Confirmed |
|---|---|---|---|---|
| raw data | B1 | angles | 250 | no |
| raw data | B1 | angles | 1000 | yes, unconditional |
| raw data | P1 | communalities | 1000 | yes, unconditional |
| correlation matrix | F2bc | communalities | 1000 | yes, unconditional |
| correlation matrix | P1 | communalities | 1000 | yes, unconditional |

B1 angles at boundary N = 250 cover .86 to .92 per scale on the fresh seeds,
and 2 of the 7 angles fail. So the N = 250 pass of the main run does not
hold up. The ladder entry "B1 angles pass from N = 1000" does not rest on
N = 250 and stands.
