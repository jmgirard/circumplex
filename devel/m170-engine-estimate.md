# M170: the CPM engine's missed optima, and which solution it reports

This record reports the study in `devel/m170-engine-estimate.R`. The
pre-registration block at the top of that script fixes the study design.
It defines the truths, the cells, the seeds and the three estimators. It
also defines every figure below, the saturation rule and the decision rule.
The script `devel/m170-engine-estimate-summary.R` writes every table in the
generated part below, from `devel/m170-engine-estimate-results.rds`. The
prose in this section cites those tables. To regenerate them, run the
summary script.

## Provenance

- Commit 2c0dfbd7 added the estimator helpers and their checks
  (`devel/m170-engine-estimate-checks.R`).
- Commit 47836901 added the pre-registration block. No pilot file or
  full-run file existed at that commit.
- RB28 (a Fable review) examined the block. Commit a65a356e applied RR28's
  corrections, which the block lists under its CORRECTIONS heading. No pilot
  file or full-run file existed at that commit either.
- The timing pilot ran at a65a356e, and commit 065f6455 added its file. The
  pilot set R = 500 replicates per cell.
- The full run ran at 95120722 with a clean working tree. It used 17
  workers and took 39 minutes. Commit 07ca104f added its results file.

## Estimators in brief

- E is `cpm_engine()` as shipped.
- W is E plus 101 deterministic extra starts. Most of them sit around E's
  estimate: communality-wall starts, one all-wall start, single-angle
  shifts, pairwise angle swaps, and swaps with a wall. Eight more are wall
  starts around g0, the battery's theory start.
- H adds 400 seeded random starts around g0 to W's starts. H is a bounded
  reference search. It is not the global minimum.
- T = (N - 1) F. If a start lies more than 0.01 T units below E's F, it
  replaces E. Otherwise E stands. W and H therefore differ from E exactly on their "lower
  fits".

## What the study found

**A lower discrepancy is common.** W finds one on 16 to 43% of accepted
fits at the boundary truth. At the interior truth the share falls with N.
It is .541 at N = 250, .012 at N = 2000 and .000 at N = 5000.

**No bounded search here finds the lowest point.** The saturation verdict
fails in all ten cells. The independent family of 400 uniform starts went
below H on 2 to 19 of the 50 subsample fits per cell. Doubling H's random
starts still lowered F on up to 14.6% of fits, at interior N = 250. Every
point that the uniform family found below H had a changed cyclic order.
H's shares are therefore lower bounds in every cell.

**The truth decides whether the lower point keeps the configuration.** At
the boundary truth, most of W's lower fits keep E's cyclic order. The
reordered share is .109 of accepted fits at N = 250. From N = 500 up it is
.035 or less. In 46 to 65% of W's lower fits at N <= 1000, the lower point
has more scales at the Heywood wall than E. At the interior truth, 81 to
100% of W's lower fits change the order.

**The gap is typically 1 to 2 T units.** W's median gap runs from 1.10 to
2.32 T units by cell. Its one-sided 95% lower confidence bound runs from
0.73 to 1.69. The largest single gap is 13.90 T units.

**W's and H's lower points are worse estimates of the truth, on average.**
The study measured the quality of W's and H's points only. It did not
measure the quality of the points that the uniform family found below H.
Take every cell where W or H differs from E. There, the angle RMSE and the weight RMSE are
larger for that estimator than for E. The communality RMSE is larger too,
with one exception: H at interior N = 5000, at .04239 against E's .04243.
Wald coverage is further from .95 for W and H in every such cell. Below
N = 5000, a smaller share of W and H fits than of E fits gets a finite
standard error. At
boundary N = 250, the angle RMSE is 18.71 degrees for E, 29.89 for W and
39.66 for H. At interior N = 250 it is 11.84, 43.34 and 55.26. No cell
passes `no_worse()` for W or H. The one exception is interior N = 5000 for
W, where W never differs from E. RR27 Q2 reported this direction from 60
replicates per cell. This study measures it at 500.

The table "Estimate quality on the lower fits only" repeats the comparison
on the fits where W or H differs from E. There too, W's and H's mean angle
error and mean weight error are larger than E's in every cell. The
direction does not hold on every fit. W's angles are closer to the truth
than E's on up to 31% of W's lower fits (boundary N = 250). W's weights
are closer on up to 19% (interior N = 250).

**E's fit test is conservative where lower points are common.** Take the
cells at N <= 1000. There, E's T exceeds its .95 chi-square quantile on 1.6 to 3.9% of
accepted fits. At boundary N = 2000 and 5000 the share is 1.8%. At interior
N = 2000 and 5000 it is 5.0% and 4.8%. In the results, W's and H's T
never exceeds the quantile on a fit where E's does not. Their shares are
therefore at or below E's in every cell, and W's equals E's at interior
N = 5000. This is a result of the run, not a property of the estimators:
W's or H's polish can leave a model with a different df from E's.

**A W-based diagnostic misses some lower points.** Consider the cells where
H found more than 10 lower fits. In each of them, W finds 77.5 to 100% of
H's lower fits. The uniform family also ran on a subsample of 50 fits per
cell. There, H or the family found a lower point that W missed on 2 to 26%
of fits. The 26% is at boundary N = 250. At interior N = 5000, H found a
lower point on 4 fits and W found none.

**Three of the four real fits have no lower point.** W and H find none on
the jz2017 octants or on the vocational-interest matrix. They also find
none on the vignette's `demo` sample. The verbal matrix is the exception (p = 6, m = 2, df = 2).
There, W reaches T = 0.836 against E's 3.994, a gap of 3.158. H reaches
0.366, a gap of 3.628. Both points reorder the scales, as RR27 reported for
this matrix.

## The decision

The pre-registered rule reads its branches in order.

- Branch 0 does not fire. No figure it reads is NA.
- Branches 1 and 2 do not fire. Saturation fails in every cell, and
  `no_worse()` fails for both W and H.
- Branch 3 does not fire. W's share is .05 or more in eight cells.
- Branch 4 does not fire. In the pilot, W costs 0.68 to 1.07 CPU-seconds
  per fit. The limit is 10. The figure covers E and W's refits, but not the
  finish step (at most one polish refit and the canonicalization).
- Branch 5 does not fire. The largest lower bound on W's median gap is 1.69,
  at interior N = 500. The threshold is 2.0. Two point medians are above
  2.0: 2.02 at interior N = 500 and 2.32 at interior N = 2000. The bound
  keeps such medians from deciding the branch on Monte Carlo noise.

The default, branch 6, selects **keep E and add a diagnostic for a lower
optimum outside the theoretical configuration**. The results file stores
the same output, under `decision`.

## Scope and limits

- The data are normal, at two truths: p = 8 octant scales, communality .75,
  m = 3, variant A, unit scaling, N = 250 to 5000.
- The real and published fits extend the reach to those four data sets only.
- The `demo` sample came from the full run on an Apple M5 Pro Mac
  (Mac17,9, macOS 27.0.1). That machine had R 4.6.1 with R's reference BLAS
  and LAPACK (`libRblas.0.dylib`, `libRlapack.dylib`), read on the same day.
  The vignette marks this sample volatile across BLAS builds.
- E's exclusion and Heywood rates match M167's for the same cells and
  generator. M170 excluded 41 and 95 fits at N = 250, against M167's 40 and
  94. Heywood rates are .82 and .93, against .78 and .91. E's SE-defined
  shares differ from M167's by up to .065 at N = 250, on different seeds.

<!-- generated:begin (devel/m170-engine-estimate-summary.R) -->

### Run

- Results file written 2026-10-27 at commit 95120722 on arm64. Uncommitted paths at start: 0.
- R = 500 replicates per cell (pilot rule), K = 100. H's random starts: 2K around g0 at sd .7, 2K around g0 at sd 1.2.
- Pilot: commit a65a356e, projected 8.80 CPU-hours.
- Metrics and decision recomputed 2026-10-27 at commit 6d6e6399 from the stored replicates (the block's CORRECTIONS after the full run). Decision before the recompute: branch 6.
- Decision rule output: keep E and add a diagnostic for a lower optimum outside the theoretical configuration (branch 6).

### Shares of lower fits and the saturation verdict (AC2)

H's figures describe H, the study's bounded reference search, never the global minimum of F. A ">=" marks H's total lower share in a cell whose saturation verdict is a fail: a lower bound.

| cell | accepted | excluded | errored | H_K lower | H lower | S1 share | S2 | family hit rate | family reordered | family more Heywood | saturation |
|---|---|---|---|---|---|---|---|---|---|---|---|
| boundary N = 250 | 459 | 41 | 0 | >= 0.538 | >= 0.553 | 0.057 | 7 of 50 | 0.0025 | 1.000 | 0.286 | fail |
| boundary N = 500 | 482 | 18 | 0 | >= 0.421 | >= 0.423 | 0.010 | 5 of 50 | 0.0075 | 1.000 | 0.400 | fail |
| boundary N = 1000 | 492 |  8 | 0 | >= 0.362 | >= 0.362 | 0.000 | 3 of 50 | 0.0075 | 1.000 | 0.000 | fail |
| boundary N = 2000 | 491 |  9 | 0 | >= 0.297 | >= 0.297 | 0.002 | 2 of 50 | 0.0125 | 1.000 | 0.000 | fail |
| boundary N = 5000 | 496 |  4 | 0 | >= 0.161 | >= 0.161 | 0.000 | 4 of 50 | 0.0150 | 1.000 | 0.000 | fail |
| interior N = 250 | 405 | 95 | 0 | >= 0.657 | >= 0.689 | 0.146 | 19 of 50 | 0.0025 | 1.000 | 0.105 | fail |
| interior N = 500 | 466 | 34 | 0 | >= 0.354 | >= 0.391 | 0.073 | 7 of 50 | 0.0025 | 1.000 | 0.429 | fail |
| interior N = 1000 | 493 |  7 | 0 | >= 0.101 | >= 0.112 | 0.018 | 5 of 50 | 0.0025 | 1.000 | 0.000 | fail |
| interior N = 2000 | 499 |  1 | 0 | >= 0.014 | >= 0.020 | 0.006 | 2 of 50 | 0.0025 | 1.000 | 0.000 | fail |
| interior N = 5000 | 500 |  0 | 0 | >= 0.004 | >= 0.008 | 0.004 | 2 of 50 | 0.0050 | 1.000 | 0.000 | fail |

### Failed starts, finish errors and gradient failures

| cell | failed starts W | failed starts H | finish errors W | finish errors H | gradient failures W | gradient failures H |
|---|---|---|---|---|---|---|
| boundary N = 250 | 0 | 0 | 0 | 0 | 3 | 1 |
| boundary N = 500 | 0 | 0 | 0 | 0 | 1 | 0 |
| boundary N = 1000 | 0 | 0 | 0 | 0 | 0 | 0 |
| boundary N = 2000 | 0 | 0 | 0 | 0 | 0 | 0 |
| boundary N = 5000 | 0 | 0 | 0 | 0 | 0 | 0 |
| interior N = 250 | 0 | 0 | 0 | 0 | 0 | 2 |
| interior N = 500 | 0 | 0 | 0 | 0 | 1 | 0 |
| interior N = 1000 | 0 | 0 | 0 | 0 | 0 | 0 |
| interior N = 2000 | 0 | 0 | 0 | 0 | 0 | 0 |
| interior N = 5000 | 0 | 0 | 0 | 0 | 0 | 0 |

### W against E (AC3)

| cell | lower | same order | reordered | gap median | gap median LCB | gap max | more Heywood | order changed | recall | missed (H) | missed (H or family) | CPU W (s) | CPU E (s) |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| boundary N = 250 | 0.431 | 0.322 | 0.109 | 1.43 | 1.18 | 9.39 | 0.646 | 0.253 | 0.780 | 0.122 | 0.260 | 0.68 | 0.048 |
| boundary N = 500 | 0.417 | 0.382 | 0.035 | 1.67 | 1.44 | 9.69 | 0.632 | 0.085 | 0.985 | 0.006 | 0.040 | 0.73 | 0.049 |
| boundary N = 1000 | 0.360 | 0.350 | 0.010 | 1.61 | 1.39 | 12.08 | 0.458 | 0.028 | 0.994 | 0.002 | 0.060 | 0.77 | 0.041 |
| boundary N = 2000 | 0.297 | 0.297 | 0.000 | 1.81 | 1.52 | 9.81 | 0.212 | 0.000 | 1.000 | 0.000 | 0.020 | 0.73 | 0.043 |
| boundary N = 5000 | 0.161 | 0.161 | 0.000 | 1.10 | 0.74 | 8.17 | 0.000 | 0.000 | 1.000 | 0.000 | 0.080 | 1.00 | 0.053 |
| interior N = 250 | 0.541 | 0.040 | 0.501 | 1.82 | 1.47 | 10.35 | 0.507 | 0.927 | 0.785 | 0.148 | 0.180 | 0.93 | 0.068 |
| interior N = 500 | 0.303 | 0.058 | 0.245 | 2.02 | 1.69 | 11.02 | 0.525 | 0.809 | 0.775 | 0.088 | 0.100 | 1.06 | 0.071 |
| interior N = 1000 | 0.089 | 0.014 | 0.075 | 1.89 | 1.06 | 13.90 | 0.568 | 0.841 | 0.800 | 0.022 | 0.100 | 1.07 | 0.056 |
| interior N = 2000 | 0.012 | 0.000 | 0.012 | 2.32 | 0.73 | 6.23 | 0.833 | 1.000 | 0.600 | 0.008 | 0.060 | 1.06 | 0.050 |
| interior N = 5000 | 0.000 | 0.000 | 0.000 | NA | NA | NA | NA | NA | 0.000 | 0.008 | 0.060 | 1.06 | 0.048 |

### H against E (AC3)

H's figures describe H, the study's bounded reference search, never the global minimum of F. A ">=" marks H's total lower share in a cell whose saturation verdict is a fail: a lower bound.

| cell | lower | same order | reordered | gap median | gap median LCB | gap max | more Heywood | order changed |
|---|---|---|---|---|---|---|---|---|
| boundary N = 250 | >= 0.553 | 0.292 | 0.261 | 1.52 | 1.30 | 19.41 | 0.551 | 0.472 |
| boundary N = 500 | >= 0.423 | 0.369 | 0.054 | 1.72 | 1.46 | 9.69 | 0.618 | 0.127 |
| boundary N = 1000 | >= 0.362 | 0.346 | 0.016 | 1.62 | 1.39 | 12.08 | 0.461 | 0.045 |
| boundary N = 2000 | >= 0.297 | 0.295 | 0.002 | 1.84 | 1.52 | 9.81 | 0.212 | 0.007 |
| boundary N = 5000 | >= 0.161 | 0.161 | 0.000 | 1.10 | 0.74 | 8.17 | 0.000 | 0.000 |
| interior N = 250 | >= 0.689 | 0.022 | 0.667 | 2.71 | 2.24 | 12.28 | 0.337 | 0.968 |
| interior N = 500 | >= 0.391 | 0.045 | 0.345 | 2.49 | 2.10 | 12.22 | 0.423 | 0.885 |
| interior N = 1000 | >= 0.112 | 0.014 | 0.097 | 1.99 | 1.10 | 13.90 | 0.509 | 0.873 |
| interior N = 2000 | >= 0.020 | 0.000 | 0.020 | 1.90 | 0.79 | 9.84 | 0.500 | 1.000 |
| interior N = 5000 | >= 0.008 | 0.000 | 0.008 | 4.65 | NA | 7.09 | 0.000 | 1.000 |

### Estimate quality by family (AC4)

H's figures describe H, the study's bounded reference search, never the global minimum of F. A ">=" marks H's total lower share in a cell whose saturation verdict is a fail: a lower bound.

| cell | estimator | RMSE angle (deg) | RMSE zeta | RMSE beta | cov dev | SE defined | T exceed |
|---|---|---|---|---|---|---|---|
| boundary N = 250 | E | 18.71 | 0.1411 | 0.0376 | 0.1042 | 0.338 | 0.017 |
| boundary N = 250 | W | 29.89 | 0.1534 | 0.0552 | 0.2019 | 0.235 | 0.004 |
| boundary N = 250 | H | 39.66 | 0.1542 | 0.0768 | 0.2731 | 0.235 | 0.002 |
| boundary N = 500 | E | 16.33 | 0.1207 | 0.0297 | 0.0920 | 0.568 | 0.031 |
| boundary N = 500 | W | 21.72 | 0.1354 | 0.0394 | 0.1699 | 0.432 | 0.015 |
| boundary N = 500 | H | 23.41 | 0.1355 | 0.0435 | 0.1561 | 0.396 | 0.012 |
| boundary N = 1000 | E | 13.89 | 0.0955 | 0.0218 | 0.1019 | 0.837 | 0.016 |
| boundary N = 1000 | W | 16.75 | 0.1077 | 0.0275 | 0.1762 | 0.738 | 0.014 |
| boundary N = 1000 | H | 18.43 | 0.1080 | 0.0302 | 0.1721 | 0.722 | 0.014 |
| boundary N = 2000 | E | 12.16 | 0.0749 | 0.0163 | 0.1478 | 0.957 | 0.018 |
| boundary N = 2000 | W | 13.31 | 0.0845 | 0.0188 | 0.2139 | 0.916 | 0.006 |
| boundary N = 2000 | H | 14.80 | 0.0845 | 0.0215 | 0.2129 | 0.912 | 0.006 |
| boundary N = 5000 | E | 9.69 | 0.0514 | 0.0107 | 0.1612 | 1.000 | 0.018 |
| boundary N = 5000 | W | 10.47 | 0.0553 | 0.0117 | 0.2071 | 1.000 | 0.010 |
| boundary N = 5000 | H | 10.47 | 0.0553 | 0.0117 | 0.2071 | 1.000 | 0.010 |
| interior N = 250 | E | 11.84 | 0.1800 | 0.0315 | 0.1559 | 0.338 | 0.035 |
| interior N = 250 | W | 43.34 | 0.1877 | 0.0424 | 0.2456 | 0.264 | 0.010 |
| interior N = 250 | H | 55.26 | 0.1811 | 0.0519 | 0.3053 | 0.306 | 0.000 |
| interior N = 500 | E | 9.27 | 0.1465 | 0.0231 | 0.0521 | 0.534 | 0.039 |
| interior N = 500 | W | 32.00 | 0.1523 | 0.0316 | 0.0990 | 0.459 | 0.017 |
| interior N = 500 | H | 41.38 | 0.1495 | 0.0378 | 0.1303 | 0.474 | 0.009 |
| interior N = 1000 | E | 6.69 | 0.1102 | 0.0163 | 0.0260 | 0.846 | 0.039 |
| interior N = 1000 | W | 19.36 | 0.1141 | 0.0209 | 0.0385 | 0.805 | 0.020 |
| interior N = 1000 | H | 23.20 | 0.1143 | 0.0228 | 0.0436 | 0.797 | 0.016 |
| interior N = 2000 | E | 4.49 | 0.0712 | 0.0100 | 0.0128 | 0.994 | 0.050 |
| interior N = 2000 | W | 9.60 | 0.0722 | 0.0119 | 0.0147 | 0.988 | 0.042 |
| interior N = 2000 | H | 14.75 | 0.0721 | 0.0147 | 0.0170 | 0.988 | 0.042 |
| interior N = 5000 | E | 2.72 | 0.0424 | 0.0061 | 0.0089 | 1.000 | 0.048 |
| interior N = 5000 | W | 2.72 | 0.0424 | 0.0061 | 0.0089 | 1.000 | 0.048 |
| interior N = 5000 | H | 11.20 | 0.0424 | 0.0106 | 0.0120 | 1.000 | 0.048 |

### Estimate quality on the lower fits only

H's figures describe H, the study's bounded reference search, never the global minimum of F. A ">=" marks H's total lower share in a cell whose saturation verdict is a fail: a lower bound.

X is W or H. Errors are each fit's root mean square error over the 7 free angles or the 4 weights, averaged over X's lower fits.

| cell | estimator | lower fits | angle error E (deg) | angle error X (deg) | X closer on angles | weight error E | weight error X | X closer on weights |
|---|---|---|---|---|---|---|---|---|
| boundary N = 250 | W | 198 | 16.90 | 32.57 | 0.313 | 0.0339 | 0.0618 | 0.141 |
| boundary N = 250 | H | 254 | 17.65 | 43.57 | 0.197 | 0.0340 | 0.0857 | 0.087 |
| boundary N = 500 | W | 201 | 15.12 | 22.65 | 0.254 | 0.0270 | 0.0441 | 0.159 |
| boundary N = 500 | H | 204 | 15.15 | 24.66 | 0.240 | 0.0271 | 0.0484 | 0.147 |
| boundary N = 1000 | W | 177 | 11.97 | 18.12 | 0.260 | 0.0174 | 0.0305 | 0.102 |
| boundary N = 1000 | H | 178 | 11.99 | 19.32 | 0.253 | 0.0174 | 0.0324 | 0.101 |
| boundary N = 2000 | W | 146 | 10.73 | 15.11 | 0.247 | 0.0136 | 0.0228 | 0.062 |
| boundary N = 2000 | H | 146 | 10.73 | 15.97 | 0.240 | 0.0136 | 0.0242 | 0.062 |
| boundary N = 5000 | W |  80 | 7.40 | 12.92 | 0.138 | 0.0090 | 0.0155 | 0.050 |
| boundary N = 5000 | H |  80 | 7.40 | 12.92 | 0.138 | 0.0090 | 0.0155 | 0.050 |
| interior N = 250 | W | 219 | 10.47 | 55.83 | 0.041 | 0.0282 | 0.0467 | 0.187 |
| interior N = 250 | H | 279 | 10.80 | 63.45 | 0.022 | 0.0292 | 0.0571 | 0.108 |
| interior N = 500 | W | 141 | 8.71 | 54.08 | 0.092 | 0.0220 | 0.0436 | 0.149 |
| interior N = 500 | H | 182 | 8.72 | 62.73 | 0.055 | 0.0220 | 0.0522 | 0.093 |
| interior N = 1000 | W |  44 | 6.06 | 59.31 | 0.023 | 0.0159 | 0.0445 | 0.045 |
| interior N = 1000 | H |  55 | 6.06 | 64.07 | 0.018 | 0.0159 | 0.0483 | 0.036 |
| interior N = 2000 | W |   6 | 3.14 | 88.06 | 0.000 | 0.0112 | 0.0578 | 0.000 |
| interior N = 2000 | H |  10 | 3.79 | 103.26 | 0.000 | 0.0104 | 0.0764 | 0.000 |
| interior N = 5000 | H |   4 | 2.54 | 126.43 | 0.000 | 0.0056 | 0.1077 | 0.000 |

### Decision-rule inputs

| cell | sat | no_worse W | improves W | no_worse H | improves H | share_lower W | gap LCB W | CPU W ok |
|---|---|---|---|---|---|---|---|---|
| boundary N = 250 | fail | FALSE | FALSE | FALSE | FALSE | 0.431 | 1.18 | TRUE |
| boundary N = 500 | fail | FALSE | FALSE | FALSE | FALSE | 0.417 | 1.44 | TRUE |
| boundary N = 1000 | fail | FALSE | FALSE | FALSE | FALSE | 0.360 | 1.39 | TRUE |
| boundary N = 2000 | fail | FALSE | FALSE | FALSE | FALSE | 0.297 | 1.52 | TRUE |
| boundary N = 5000 | fail | FALSE | FALSE | FALSE | FALSE | 0.161 | 0.74 | TRUE |
| interior N = 250 | fail | FALSE | FALSE | FALSE | FALSE | 0.541 | 1.47 | TRUE |
| interior N = 500 | fail | FALSE | FALSE | FALSE | FALSE | 0.303 | 1.69 | TRUE |
| interior N = 1000 | fail | FALSE | FALSE | FALSE | FALSE | 0.089 | 1.06 | TRUE |
| interior N = 2000 | fail | FALSE | FALSE | FALSE | FALSE | 0.012 | 0.73 | TRUE |
| interior N = 5000 | fail | TRUE | FALSE | FALSE | FALSE | 0.000 | NA | TRUE |

### Estimate quality by parameter (AC4)

H's figures describe H, the study's bounded reference search, never the global minimum of F. A ">=" marks H's total lower share in a cell whose saturation verdict is a fail: a lower bound.

#### boundary N = 250

| parameter | E bias | E RMSE | E SE def | E Wald cov | W bias | W RMSE | W SE def | W Wald cov | H bias | H RMSE | H SE def | H Wald cov |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| angle_BC | 0.58 | 12.98 | 0.338 | 0.871 | 0.45 | 25.09 | 0.235 | 0.778 | 1.65 | 39.06 | 0.235 | 0.630 |
| angle_DE | 1.17 | 19.56 | 0.338 | 0.813 | 2.18 | 28.05 | 0.235 | 0.713 | 3.81 | 37.04 | 0.235 | 0.583 |
| angle_FG | 1.36 | 21.88 | 0.338 | 0.819 | 4.88 | 33.19 | 0.235 | 0.648 | 4.56 | 39.95 | 0.235 | 0.519 |
| angle_HI | 0.82 | 22.74 | 0.338 | 0.781 | 2.00 | 31.12 | 0.235 | 0.574 | 0.37 | 41.73 | 0.235 | 0.454 |
| angle_JK | 1.58 | 21.56 | 0.338 | 0.781 | -1.33 | 30.95 | 0.235 | 0.565 | -4.68 | 40.96 | 0.235 | 0.444 |
| angle_LM | 1.35 | 19.08 | 0.338 | 0.819 | -2.89 | 32.34 | 0.235 | 0.639 | -7.49 | 39.16 | 0.235 | 0.546 |
| angle_NO | 1.04 | 13.14 | 0.338 | 0.845 | -0.60 | 28.49 | 0.235 | 0.796 | -3.14 | 39.71 | 0.235 | 0.667 |
| zeta_PA | 0.0367 | 0.1397 | 0.338 | 0.839 | 0.0527 | 0.1501 | 0.235 | 0.769 | 0.0529 | 0.1527 | 0.235 | 0.787 |
| zeta_BC | 0.0515 | 0.1478 | 0.338 | 0.826 | 0.0643 | 0.1585 | 0.235 | 0.787 | 0.0616 | 0.1550 | 0.235 | 0.741 |
| zeta_DE | 0.0435 | 0.1403 | 0.338 | 0.884 | 0.0632 | 0.1547 | 0.235 | 0.815 | 0.0586 | 0.1544 | 0.235 | 0.778 |
| zeta_FG | 0.0444 | 0.1395 | 0.338 | 0.903 | 0.0652 | 0.1508 | 0.235 | 0.852 | 0.0614 | 0.1546 | 0.235 | 0.796 |
| zeta_HI | 0.0419 | 0.1453 | 0.338 | 0.852 | 0.0595 | 0.1572 | 0.235 | 0.787 | 0.0560 | 0.1574 | 0.235 | 0.787 |
| zeta_JK | 0.0446 | 0.1429 | 0.338 | 0.845 | 0.0604 | 0.1520 | 0.235 | 0.824 | 0.0570 | 0.1485 | 0.235 | 0.852 |
| zeta_LM | 0.0396 | 0.1377 | 0.338 | 0.839 | 0.0578 | 0.1503 | 0.235 | 0.815 | 0.0591 | 0.1545 | 0.235 | 0.824 |
| zeta_NO | 0.0343 | 0.1353 | 0.338 | 0.865 | 0.0609 | 0.1539 | 0.235 | 0.796 | 0.0630 | 0.1569 | 0.235 | 0.769 |
| beta0 | -0.0295 | 0.0503 | 0.338 | 0.897 | -0.0438 | 0.0577 | 0.235 | 0.787 | -0.0466 | 0.0592 | 0.235 | 0.769 |
| beta1 | -0.0017 | 0.0312 | 0.338 | 0.903 | -0.0223 | 0.0553 | 0.235 | 0.815 | -0.0485 | 0.0863 | 0.235 | 0.648 |
| beta2 | 0.0198 | 0.0338 | 0.338 | 0.871 | 0.0331 | 0.0566 | 0.235 | 0.833 | 0.0413 | 0.0743 | 0.235 | 0.769 |
| beta3 | 0.0115 | 0.0350 | 0.338 | 0.819 | 0.0330 | 0.0511 | 0.235 | 0.620 | 0.0538 | 0.0873 | 0.235 | 0.500 |

#### boundary N = 500

| parameter | E bias | E RMSE | E SE def | E Wald cov | W bias | W RMSE | W SE def | W Wald cov | H bias | H RMSE | H SE def | H Wald cov |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| angle_BC | 0.01 | 10.22 | 0.568 | 0.891 | 1.02 | 16.95 | 0.432 | 0.822 | 1.83 | 22.13 | 0.396 | 0.838 |
| angle_DE | -0.64 | 16.84 | 0.568 | 0.810 | 1.13 | 22.49 | 0.432 | 0.678 | 1.91 | 24.68 | 0.396 | 0.712 |
| angle_FG | -0.95 | 19.51 | 0.568 | 0.748 | 1.11 | 23.66 | 0.432 | 0.601 | 0.79 | 23.97 | 0.396 | 0.623 |
| angle_HI | -1.48 | 19.94 | 0.568 | 0.726 | -0.03 | 25.02 | 0.432 | 0.620 | -0.17 | 26.84 | 0.396 | 0.628 |
| angle_JK | -1.05 | 20.13 | 0.568 | 0.723 | -0.66 | 24.26 | 0.432 | 0.582 | -0.70 | 25.23 | 0.396 | 0.602 |
| angle_LM | -0.61 | 17.20 | 0.568 | 0.788 | -1.01 | 21.69 | 0.432 | 0.683 | -0.43 | 22.23 | 0.396 | 0.665 |
| angle_NO | -0.09 | 10.51 | 0.568 | 0.861 | -1.00 | 17.96 | 0.432 | 0.793 | -0.33 | 18.78 | 0.396 | 0.785 |
| zeta_PA | 0.0360 | 0.1198 | 0.568 | 0.901 | 0.0558 | 0.1338 | 0.432 | 0.856 | 0.0574 | 0.1363 | 0.396 | 0.874 |
| zeta_BC | 0.0354 | 0.1197 | 0.568 | 0.883 | 0.0550 | 0.1354 | 0.432 | 0.865 | 0.0559 | 0.1359 | 0.396 | 0.895 |
| zeta_DE | 0.0356 | 0.1226 | 0.568 | 0.891 | 0.0563 | 0.1376 | 0.432 | 0.851 | 0.0546 | 0.1373 | 0.396 | 0.859 |
| zeta_FG | 0.0382 | 0.1185 | 0.568 | 0.894 | 0.0602 | 0.1376 | 0.432 | 0.856 | 0.0583 | 0.1358 | 0.396 | 0.885 |
| zeta_HI | 0.0429 | 0.1195 | 0.568 | 0.901 | 0.0610 | 0.1362 | 0.432 | 0.875 | 0.0624 | 0.1369 | 0.396 | 0.880 |
| zeta_JK | 0.0401 | 0.1205 | 0.568 | 0.920 | 0.0510 | 0.1319 | 0.432 | 0.889 | 0.0511 | 0.1318 | 0.396 | 0.901 |
| zeta_LM | 0.0433 | 0.1231 | 0.568 | 0.901 | 0.0540 | 0.1347 | 0.432 | 0.856 | 0.0534 | 0.1349 | 0.396 | 0.869 |
| zeta_NO | 0.0432 | 0.1218 | 0.568 | 0.931 | 0.0589 | 0.1361 | 0.432 | 0.904 | 0.0567 | 0.1350 | 0.396 | 0.911 |
| beta0 | -0.0306 | 0.0433 | 0.568 | 0.894 | -0.0424 | 0.0501 | 0.432 | 0.774 | -0.0425 | 0.0502 | 0.396 | 0.812 |
| beta1 | -0.0021 | 0.0222 | 0.568 | 0.901 | -0.0110 | 0.0334 | 0.432 | 0.865 | -0.0140 | 0.0397 | 0.396 | 0.848 |
| beta2 | 0.0178 | 0.0256 | 0.568 | 0.883 | 0.0244 | 0.0370 | 0.432 | 0.803 | 0.0256 | 0.0410 | 0.396 | 0.812 |
| beta3 | 0.0149 | 0.0279 | 0.568 | 0.854 | 0.0290 | 0.0374 | 0.432 | 0.649 | 0.0309 | 0.0430 | 0.396 | 0.686 |

#### boundary N = 1000

| parameter | E bias | E RMSE | E SE def | E Wald cov | W bias | W RMSE | W SE def | W Wald cov | H bias | H RMSE | H SE def | H Wald cov |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| angle_BC | -0.24 | 8.84 | 0.837 | 0.852 | 0.57 | 12.53 | 0.738 | 0.788 | 0.88 | 14.85 | 0.722 | 0.786 |
| angle_DE | -0.48 | 14.03 | 0.837 | 0.784 | 0.60 | 17.49 | 0.738 | 0.667 | 0.80 | 18.59 | 0.722 | 0.668 |
| angle_FG | -0.53 | 16.04 | 0.837 | 0.782 | 0.18 | 19.03 | 0.738 | 0.634 | -0.18 | 20.78 | 0.722 | 0.637 |
| angle_HI | -0.40 | 17.37 | 0.837 | 0.718 | -0.65 | 20.41 | 0.738 | 0.587 | -0.46 | 20.91 | 0.722 | 0.589 |
| angle_JK | -0.28 | 16.95 | 0.837 | 0.718 | -0.79 | 20.39 | 0.738 | 0.595 | -1.28 | 22.36 | 0.722 | 0.594 |
| angle_LM | -0.14 | 14.88 | 0.837 | 0.728 | -0.56 | 17.43 | 0.738 | 0.612 | -0.54 | 17.52 | 0.722 | 0.617 |
| angle_NO | -0.08 | 9.15 | 0.837 | 0.806 | -0.22 | 9.95 | 0.738 | 0.700 | -0.77 | 14.04 | 0.722 | 0.701 |
| zeta_PA | 0.0268 | 0.0967 | 0.837 | 0.893 | 0.0405 | 0.1080 | 0.738 | 0.868 | 0.0401 | 0.1076 | 0.722 | 0.879 |
| zeta_BC | 0.0300 | 0.0940 | 0.837 | 0.913 | 0.0445 | 0.1085 | 0.738 | 0.882 | 0.0445 | 0.1087 | 0.722 | 0.885 |
| zeta_DE | 0.0294 | 0.0909 | 0.837 | 0.913 | 0.0425 | 0.1061 | 0.738 | 0.868 | 0.0424 | 0.1061 | 0.722 | 0.870 |
| zeta_FG | 0.0296 | 0.0974 | 0.837 | 0.888 | 0.0395 | 0.1100 | 0.738 | 0.851 | 0.0395 | 0.1104 | 0.722 | 0.851 |
| zeta_HI | 0.0324 | 0.0964 | 0.837 | 0.930 | 0.0431 | 0.1074 | 0.738 | 0.901 | 0.0437 | 0.1078 | 0.722 | 0.907 |
| zeta_JK | 0.0336 | 0.0965 | 0.837 | 0.910 | 0.0426 | 0.1070 | 0.738 | 0.876 | 0.0429 | 0.1076 | 0.722 | 0.876 |
| zeta_LM | 0.0293 | 0.0930 | 0.837 | 0.932 | 0.0397 | 0.1010 | 0.738 | 0.923 | 0.0397 | 0.1014 | 0.722 | 0.921 |
| zeta_NO | 0.0323 | 0.0993 | 0.837 | 0.922 | 0.0463 | 0.1140 | 0.738 | 0.882 | 0.0462 | 0.1146 | 0.722 | 0.876 |
| beta0 | -0.0233 | 0.0327 | 0.837 | 0.854 | -0.0319 | 0.0383 | 0.738 | 0.744 | -0.0319 | 0.0384 | 0.722 | 0.763 |
| beta1 | -0.0028 | 0.0143 | 0.837 | 0.927 | -0.0065 | 0.0195 | 0.738 | 0.898 | -0.0077 | 0.0254 | 0.722 | 0.904 |
| beta2 | 0.0128 | 0.0184 | 0.837 | 0.842 | 0.0163 | 0.0245 | 0.738 | 0.788 | 0.0168 | 0.0262 | 0.722 | 0.806 |
| beta3 | 0.0132 | 0.0217 | 0.837 | 0.801 | 0.0221 | 0.0276 | 0.738 | 0.642 | 0.0227 | 0.0309 | 0.722 | 0.651 |

#### boundary N = 2000

| parameter | E bias | E RMSE | E SE def | E Wald cov | W bias | W RMSE | W SE def | W Wald cov | H bias | H RMSE | H SE def | H Wald cov |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| angle_BC | 0.23 | 7.15 | 0.957 | 0.770 | 0.33 | 8.05 | 0.916 | 0.676 | 0.67 | 11.39 | 0.912 | 0.676 |
| angle_DE | 0.19 | 12.18 | 0.957 | 0.723 | 0.31 | 13.63 | 0.916 | 0.636 | 0.28 | 13.61 | 0.912 | 0.638 |
| angle_FG | 0.15 | 15.09 | 0.957 | 0.657 | 0.23 | 16.70 | 0.916 | 0.573 | -0.16 | 18.56 | 0.912 | 0.576 |
| angle_HI | -0.15 | 15.98 | 0.957 | 0.626 | -0.14 | 17.58 | 0.916 | 0.544 | -0.15 | 17.58 | 0.912 | 0.547 |
| angle_JK | -0.14 | 14.97 | 0.957 | 0.677 | -0.19 | 16.05 | 0.916 | 0.569 | -0.55 | 17.94 | 0.912 | 0.569 |
| angle_LM | -0.12 | 12.24 | 0.957 | 0.719 | -0.15 | 13.08 | 0.916 | 0.631 | -0.13 | 13.08 | 0.912 | 0.632 |
| angle_NO | -0.16 | 7.52 | 0.957 | 0.757 | -0.22 | 8.07 | 0.916 | 0.689 | -0.56 | 11.42 | 0.912 | 0.690 |
| zeta_PA | 0.0238 | 0.0755 | 0.957 | 0.874 | 0.0328 | 0.0877 | 0.916 | 0.833 | 0.0325 | 0.0877 | 0.912 | 0.835 |
| zeta_BC | 0.0227 | 0.0737 | 0.957 | 0.877 | 0.0309 | 0.0851 | 0.916 | 0.829 | 0.0307 | 0.0849 | 0.912 | 0.830 |
| zeta_DE | 0.0188 | 0.0712 | 0.957 | 0.872 | 0.0257 | 0.0808 | 0.916 | 0.827 | 0.0257 | 0.0808 | 0.912 | 0.828 |
| zeta_FG | 0.0235 | 0.0775 | 0.957 | 0.868 | 0.0314 | 0.0882 | 0.916 | 0.822 | 0.0313 | 0.0882 | 0.912 | 0.819 |
| zeta_HI | 0.0202 | 0.0748 | 0.957 | 0.891 | 0.0259 | 0.0813 | 0.916 | 0.856 | 0.0260 | 0.0813 | 0.912 | 0.857 |
| zeta_JK | 0.0214 | 0.0749 | 0.957 | 0.849 | 0.0300 | 0.0867 | 0.916 | 0.816 | 0.0301 | 0.0867 | 0.912 | 0.817 |
| zeta_LM | 0.0202 | 0.0766 | 0.957 | 0.881 | 0.0273 | 0.0849 | 0.916 | 0.844 | 0.0273 | 0.0849 | 0.912 | 0.844 |
| zeta_NO | 0.0233 | 0.0745 | 0.957 | 0.891 | 0.0301 | 0.0811 | 0.916 | 0.867 | 0.0301 | 0.0811 | 0.912 | 0.868 |
| beta0 | -0.0180 | 0.0250 | 0.957 | 0.806 | -0.0236 | 0.0291 | 0.916 | 0.702 | -0.0235 | 0.0291 | 0.912 | 0.705 |
| beta1 | -0.0019 | 0.0105 | 0.957 | 0.932 | -0.0034 | 0.0110 | 0.916 | 0.911 | -0.0040 | 0.0175 | 0.912 | 0.908 |
| beta2 | 0.0093 | 0.0135 | 0.957 | 0.821 | 0.0108 | 0.0148 | 0.916 | 0.767 | 0.0108 | 0.0148 | 0.912 | 0.768 |
| beta3 | 0.0106 | 0.0161 | 0.957 | 0.749 | 0.0161 | 0.0202 | 0.916 | 0.596 | 0.0167 | 0.0247 | 0.912 | 0.596 |

#### boundary N = 5000

| parameter | E bias | E RMSE | E SE def | E Wald cov | W bias | W RMSE | W SE def | W Wald cov | H bias | H RMSE | H SE def | H Wald cov |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| angle_BC | 0.32 | 5.54 | 1.000 | 0.740 | 0.31 | 5.88 | 1.000 | 0.690 | 0.31 | 5.88 | 1.000 | 0.690 |
| angle_DE | 0.23 | 9.73 | 1.000 | 0.679 | 0.42 | 10.56 | 1.000 | 0.619 | 0.42 | 10.56 | 1.000 | 0.619 |
| angle_FG | -0.02 | 12.37 | 1.000 | 0.639 | 0.42 | 13.42 | 1.000 | 0.587 | 0.42 | 13.42 | 1.000 | 0.587 |
| angle_HI | -0.20 | 12.97 | 1.000 | 0.669 | 0.38 | 14.07 | 1.000 | 0.599 | 0.38 | 14.07 | 1.000 | 0.599 |
| angle_JK | -0.44 | 12.20 | 1.000 | 0.688 | 0.10 | 13.27 | 1.000 | 0.605 | 0.10 | 13.27 | 1.000 | 0.605 |
| angle_LM | -0.70 | 9.65 | 1.000 | 0.718 | -0.32 | 10.36 | 1.000 | 0.657 | -0.32 | 10.36 | 1.000 | 0.657 |
| angle_NO | -0.45 | 5.36 | 1.000 | 0.752 | -0.27 | 5.73 | 1.000 | 0.692 | -0.27 | 5.73 | 1.000 | 0.692 |
| zeta_PA | 0.0166 | 0.0525 | 1.000 | 0.839 | 0.0183 | 0.0542 | 1.000 | 0.837 | 0.0183 | 0.0542 | 1.000 | 0.837 |
| zeta_BC | 0.0138 | 0.0504 | 1.000 | 0.851 | 0.0176 | 0.0565 | 1.000 | 0.810 | 0.0176 | 0.0565 | 1.000 | 0.810 |
| zeta_DE | 0.0124 | 0.0523 | 1.000 | 0.827 | 0.0173 | 0.0571 | 1.000 | 0.806 | 0.0173 | 0.0571 | 1.000 | 0.806 |
| zeta_FG | 0.0114 | 0.0489 | 1.000 | 0.863 | 0.0160 | 0.0553 | 1.000 | 0.821 | 0.0160 | 0.0553 | 1.000 | 0.821 |
| zeta_HI | 0.0129 | 0.0519 | 1.000 | 0.859 | 0.0160 | 0.0547 | 1.000 | 0.833 | 0.0160 | 0.0547 | 1.000 | 0.833 |
| zeta_JK | 0.0109 | 0.0501 | 1.000 | 0.881 | 0.0126 | 0.0531 | 1.000 | 0.843 | 0.0126 | 0.0531 | 1.000 | 0.843 |
| zeta_LM | 0.0137 | 0.0525 | 1.000 | 0.855 | 0.0151 | 0.0554 | 1.000 | 0.821 | 0.0151 | 0.0554 | 1.000 | 0.821 |
| zeta_NO | 0.0164 | 0.0522 | 1.000 | 0.849 | 0.0177 | 0.0561 | 1.000 | 0.808 | 0.0177 | 0.0561 | 1.000 | 0.808 |
| beta0 | -0.0116 | 0.0162 | 1.000 | 0.782 | -0.0138 | 0.0177 | 1.000 | 0.730 | -0.0138 | 0.0177 | 1.000 | 0.730 |
| beta1 | -0.0021 | 0.0073 | 1.000 | 0.921 | -0.0025 | 0.0076 | 1.000 | 0.911 | -0.0025 | 0.0076 | 1.000 | 0.911 |
| beta2 | 0.0060 | 0.0086 | 1.000 | 0.847 | 0.0068 | 0.0091 | 1.000 | 0.808 | 0.0068 | 0.0091 | 1.000 | 0.808 |
| beta3 | 0.0076 | 0.0108 | 1.000 | 0.730 | 0.0096 | 0.0123 | 1.000 | 0.639 | 0.0096 | 0.0123 | 1.000 | 0.639 |

#### interior N = 250

| parameter | E bias | E RMSE | E SE def | E Wald cov | W bias | W RMSE | W SE def | W Wald cov | H bias | H RMSE | H SE def | H Wald cov |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| angle_BC | -0.61 | 8.31 | 0.338 | 0.883 | -7.52 | 56.08 | 0.264 | 0.710 | -11.62 | 63.62 | 0.306 | 0.500 |
| angle_DE | -1.31 | 13.01 | 0.338 | 0.803 | 2.65 | 25.25 | 0.264 | 0.729 | 10.26 | 42.15 | 0.306 | 0.629 |
| angle_FG | -0.52 | 12.15 | 0.338 | 0.796 | 12.08 | 51.53 | 0.264 | 0.664 | 6.42 | 62.70 | 0.306 | 0.484 |
| angle_HI | -0.54 | 15.04 | 0.338 | 0.774 | 0.46 | 29.39 | 0.264 | 0.654 | 1.58 | 44.18 | 0.306 | 0.524 |
| angle_JK | -0.38 | 11.84 | 0.338 | 0.788 | -9.85 | 56.91 | 0.264 | 0.570 | -14.20 | 66.06 | 0.306 | 0.435 |
| angle_LM | -0.50 | 14.06 | 0.338 | 0.781 | -3.49 | 29.64 | 0.264 | 0.692 | -10.30 | 44.01 | 0.306 | 0.629 |
| angle_NO | -0.39 | 8.49 | 0.338 | 0.818 | 9.43 | 54.57 | 0.264 | 0.636 | 1.30 | 64.10 | 0.306 | 0.411 |
| zeta_PA | 0.0535 | 0.1768 | 0.338 | 0.723 | 0.0557 | 0.1841 | 0.264 | 0.701 | 0.0479 | 0.1820 | 0.306 | 0.742 |
| zeta_BC | 0.0496 | 0.1863 | 0.338 | 0.693 | 0.0586 | 0.1915 | 0.264 | 0.673 | 0.0433 | 0.1787 | 0.306 | 0.766 |
| zeta_DE | 0.0566 | 0.1828 | 0.338 | 0.759 | 0.0720 | 0.1931 | 0.264 | 0.682 | 0.0586 | 0.1842 | 0.306 | 0.806 |
| zeta_FG | 0.0718 | 0.1768 | 0.338 | 0.752 | 0.0732 | 0.1847 | 0.264 | 0.701 | 0.0534 | 0.1806 | 0.306 | 0.766 |
| zeta_HI | 0.0490 | 0.1823 | 0.338 | 0.737 | 0.0533 | 0.1901 | 0.264 | 0.664 | 0.0449 | 0.1840 | 0.306 | 0.734 |
| zeta_JK | 0.0499 | 0.1785 | 0.338 | 0.730 | 0.0510 | 0.1866 | 0.264 | 0.645 | 0.0415 | 0.1827 | 0.306 | 0.661 |
| zeta_LM | 0.0659 | 0.1791 | 0.338 | 0.672 | 0.0778 | 0.1868 | 0.264 | 0.636 | 0.0733 | 0.1816 | 0.306 | 0.718 |
| zeta_NO | 0.0511 | 0.1776 | 0.338 | 0.774 | 0.0612 | 0.1845 | 0.264 | 0.776 | 0.0507 | 0.1752 | 0.306 | 0.782 |
| beta0 | -0.0251 | 0.0395 | 0.338 | 0.876 | -0.0286 | 0.0424 | 0.264 | 0.850 | -0.0261 | 0.0401 | 0.306 | 0.798 |
| beta1 | -0.0002 | 0.0293 | 0.338 | 0.920 | -0.0193 | 0.0460 | 0.264 | 0.785 | -0.0374 | 0.0620 | 0.306 | 0.532 |
| beta2 | 0.0114 | 0.0284 | 0.338 | 0.920 | 0.0120 | 0.0297 | 0.264 | 0.879 | 0.0108 | 0.0335 | 0.306 | 0.887 |
| beta3 | 0.0138 | 0.0287 | 0.338 | 0.891 | 0.0358 | 0.0515 | 0.264 | 0.738 | 0.0527 | 0.0720 | 0.306 | 0.444 |

#### interior N = 500

| parameter | E bias | E RMSE | E SE def | E Wald cov | W bias | W RMSE | W SE def | W Wald cov | H bias | H RMSE | H SE def | H Wald cov |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| angle_BC | -0.11 | 7.45 | 0.534 | 0.912 | -11.63 | 44.55 | 0.459 | 0.813 | -8.45 | 55.04 | 0.474 | 0.683 |
| angle_DE | -0.21 | 8.78 | 0.534 | 0.884 | 1.41 | 13.49 | 0.459 | 0.813 | 4.15 | 26.75 | 0.474 | 0.796 |
| angle_FG | 0.21 | 10.55 | 0.534 | 0.859 | 10.72 | 41.08 | 0.459 | 0.780 | 7.04 | 50.36 | 0.474 | 0.674 |
| angle_HI | 0.70 | 10.52 | 0.534 | 0.827 | 1.86 | 19.69 | 0.459 | 0.780 | -0.07 | 27.26 | 0.474 | 0.769 |
| angle_JK | 1.10 | 10.92 | 0.534 | 0.835 | -11.50 | 46.67 | 0.459 | 0.743 | -12.50 | 54.90 | 0.474 | 0.656 |
| angle_LM | 0.54 | 9.33 | 0.534 | 0.908 | -1.68 | 18.10 | 0.459 | 0.850 | -2.85 | 25.51 | 0.474 | 0.842 |
| angle_NO | 0.36 | 7.34 | 0.534 | 0.920 | 6.27 | 40.42 | 0.459 | 0.860 | 0.30 | 49.86 | 0.474 | 0.751 |
| zeta_PA | 0.0311 | 0.1500 | 0.534 | 0.888 | 0.0289 | 0.1549 | 0.459 | 0.879 | 0.0318 | 0.1514 | 0.474 | 0.882 |
| zeta_BC | 0.0416 | 0.1485 | 0.534 | 0.904 | 0.0506 | 0.1532 | 0.459 | 0.897 | 0.0425 | 0.1478 | 0.474 | 0.932 |
| zeta_DE | 0.0375 | 0.1397 | 0.534 | 0.908 | 0.0447 | 0.1463 | 0.459 | 0.879 | 0.0426 | 0.1429 | 0.474 | 0.882 |
| zeta_FG | 0.0490 | 0.1471 | 0.534 | 0.880 | 0.0436 | 0.1523 | 0.459 | 0.864 | 0.0368 | 0.1515 | 0.474 | 0.882 |
| zeta_HI | 0.0473 | 0.1444 | 0.534 | 0.916 | 0.0414 | 0.1493 | 0.459 | 0.893 | 0.0393 | 0.1484 | 0.474 | 0.914 |
| zeta_JK | 0.0351 | 0.1447 | 0.534 | 0.936 | 0.0466 | 0.1521 | 0.459 | 0.925 | 0.0454 | 0.1486 | 0.474 | 0.946 |
| zeta_LM | 0.0447 | 0.1535 | 0.534 | 0.876 | 0.0532 | 0.1577 | 0.459 | 0.879 | 0.0523 | 0.1579 | 0.474 | 0.896 |
| zeta_NO | 0.0316 | 0.1442 | 0.534 | 0.936 | 0.0357 | 0.1526 | 0.459 | 0.916 | 0.0334 | 0.1479 | 0.474 | 0.937 |
| beta0 | -0.0192 | 0.0320 | 0.534 | 0.904 | -0.0209 | 0.0333 | 0.459 | 0.864 | -0.0201 | 0.0330 | 0.474 | 0.869 |
| beta1 | -0.0018 | 0.0197 | 0.534 | 0.928 | -0.0120 | 0.0346 | 0.459 | 0.841 | -0.0219 | 0.0469 | 0.474 | 0.697 |
| beta2 | 0.0094 | 0.0209 | 0.534 | 0.920 | 0.0094 | 0.0215 | 0.459 | 0.893 | 0.0093 | 0.0217 | 0.474 | 0.905 |
| beta3 | 0.0116 | 0.0198 | 0.534 | 0.924 | 0.0235 | 0.0369 | 0.459 | 0.799 | 0.0326 | 0.0495 | 0.474 | 0.661 |

#### interior N = 1000

| parameter | E bias | E RMSE | E SE def | E Wald cov | W bias | W RMSE | W SE def | W Wald cov | H bias | H RMSE | H SE def | H Wald cov |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| angle_BC | 0.13 | 4.81 | 0.846 | 0.906 | -3.10 | 25.85 | 0.805 | 0.877 | -2.96 | 29.36 | 0.797 | 0.860 |
| angle_DE | 0.18 | 6.91 | 0.846 | 0.894 | 0.77 | 10.40 | 0.805 | 0.874 | 0.80 | 15.63 | 0.797 | 0.870 |
| angle_FG | 0.12 | 7.97 | 0.846 | 0.854 | 3.89 | 26.45 | 0.805 | 0.839 | 2.98 | 31.04 | 0.797 | 0.822 |
| angle_HI | 0.26 | 8.43 | 0.846 | 0.873 | -0.06 | 12.52 | 0.805 | 0.866 | 0.21 | 12.99 | 0.797 | 0.870 |
| angle_JK | 0.10 | 7.65 | 0.846 | 0.887 | -4.12 | 26.16 | 0.805 | 0.869 | -3.74 | 28.65 | 0.797 | 0.850 |
| angle_LM | -0.32 | 6.54 | 0.846 | 0.916 | -0.81 | 9.18 | 0.805 | 0.899 | -1.72 | 15.39 | 0.797 | 0.891 |
| angle_NO | -0.25 | 4.48 | 0.846 | 0.930 | 3.90 | 24.93 | 0.805 | 0.912 | 3.14 | 29.39 | 0.797 | 0.891 |
| zeta_PA | 0.0288 | 0.1139 | 0.846 | 0.942 | 0.0269 | 0.1157 | 0.805 | 0.937 | 0.0283 | 0.1165 | 0.797 | 0.931 |
| zeta_BC | 0.0244 | 0.1064 | 0.846 | 0.952 | 0.0285 | 0.1117 | 0.805 | 0.947 | 0.0283 | 0.1116 | 0.797 | 0.947 |
| zeta_DE | 0.0237 | 0.1055 | 0.846 | 0.952 | 0.0246 | 0.1103 | 0.805 | 0.947 | 0.0251 | 0.1104 | 0.797 | 0.952 |
| zeta_FG | 0.0262 | 0.1149 | 0.846 | 0.942 | 0.0254 | 0.1173 | 0.805 | 0.937 | 0.0266 | 0.1172 | 0.797 | 0.939 |
| zeta_HI | 0.0258 | 0.1109 | 0.846 | 0.954 | 0.0269 | 0.1126 | 0.805 | 0.957 | 0.0274 | 0.1147 | 0.797 | 0.947 |
| zeta_JK | 0.0199 | 0.1122 | 0.846 | 0.935 | 0.0235 | 0.1174 | 0.805 | 0.924 | 0.0224 | 0.1159 | 0.797 | 0.929 |
| zeta_LM | 0.0225 | 0.1076 | 0.846 | 0.962 | 0.0246 | 0.1119 | 0.805 | 0.962 | 0.0236 | 0.1120 | 0.797 | 0.957 |
| zeta_NO | 0.0290 | 0.1105 | 0.846 | 0.945 | 0.0337 | 0.1160 | 0.805 | 0.932 | 0.0330 | 0.1163 | 0.797 | 0.931 |
| beta0 | -0.0125 | 0.0235 | 0.846 | 0.933 | -0.0134 | 0.0241 | 0.805 | 0.937 | -0.0135 | 0.0240 | 0.797 | 0.941 |
| beta1 | 0.0000 | 0.0138 | 0.846 | 0.935 | -0.0038 | 0.0215 | 0.805 | 0.897 | -0.0055 | 0.0253 | 0.797 | 0.883 |
| beta2 | 0.0048 | 0.0143 | 0.846 | 0.945 | 0.0050 | 0.0144 | 0.805 | 0.942 | 0.0050 | 0.0144 | 0.797 | 0.939 |
| beta3 | 0.0077 | 0.0134 | 0.846 | 0.938 | 0.0122 | 0.0237 | 0.805 | 0.902 | 0.0139 | 0.0277 | 0.797 | 0.891 |

#### interior N = 2000

| parameter | E bias | E RMSE | E SE def | E Wald cov | W bias | W RMSE | W SE def | W Wald cov | H bias | H RMSE | H SE def | H Wald cov |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| angle_BC | -0.06 | 3.33 | 0.994 | 0.933 | -0.95 | 16.44 | 0.988 | 0.927 | -0.59 | 20.72 | 0.988 | 0.919 |
| angle_DE | -0.28 | 4.66 | 0.994 | 0.935 | -0.20 | 4.89 | 0.988 | 0.929 | 0.52 | 12.30 | 0.988 | 0.925 |
| angle_FG | -0.51 | 5.37 | 0.994 | 0.931 | 0.06 | 10.08 | 0.988 | 0.927 | 0.42 | 16.13 | 0.988 | 0.923 |
| angle_HI | -0.29 | 5.20 | 0.994 | 0.931 | -0.30 | 5.20 | 0.988 | 0.931 | -0.29 | 5.17 | 0.988 | 0.931 |
| angle_JK | -0.20 | 5.03 | 0.994 | 0.923 | -1.76 | 16.73 | 0.988 | 0.917 | -2.12 | 20.97 | 0.988 | 0.909 |
| angle_LM | -0.24 | 4.61 | 0.994 | 0.915 | -0.16 | 4.78 | 0.988 | 0.909 | -0.17 | 12.32 | 0.988 | 0.905 |
| angle_NO | -0.09 | 3.20 | 0.994 | 0.931 | 0.47 | 9.11 | 0.988 | 0.927 | 0.10 | 15.62 | 0.988 | 0.919 |
| zeta_PA | 0.0116 | 0.0694 | 0.994 | 0.964 | 0.0108 | 0.0694 | 0.988 | 0.963 | 0.0109 | 0.0694 | 0.988 | 0.963 |
| zeta_BC | 0.0031 | 0.0691 | 0.994 | 0.946 | 0.0048 | 0.0720 | 0.988 | 0.943 | 0.0046 | 0.0720 | 0.988 | 0.941 |
| zeta_DE | 0.0085 | 0.0740 | 0.994 | 0.956 | 0.0089 | 0.0743 | 0.988 | 0.955 | 0.0089 | 0.0741 | 0.988 | 0.955 |
| zeta_FG | 0.0093 | 0.0707 | 0.994 | 0.952 | 0.0091 | 0.0717 | 0.988 | 0.947 | 0.0091 | 0.0719 | 0.988 | 0.945 |
| zeta_HI | 0.0135 | 0.0710 | 0.994 | 0.946 | 0.0129 | 0.0713 | 0.988 | 0.943 | 0.0129 | 0.0713 | 0.988 | 0.943 |
| zeta_JK | 0.0114 | 0.0707 | 0.994 | 0.960 | 0.0132 | 0.0728 | 0.988 | 0.957 | 0.0132 | 0.0726 | 0.988 | 0.957 |
| zeta_LM | 0.0133 | 0.0724 | 0.994 | 0.948 | 0.0132 | 0.0731 | 0.988 | 0.947 | 0.0129 | 0.0730 | 0.988 | 0.947 |
| zeta_NO | 0.0160 | 0.0719 | 0.994 | 0.956 | 0.0161 | 0.0727 | 0.988 | 0.953 | 0.0162 | 0.0725 | 0.988 | 0.953 |
| beta0 | -0.0058 | 0.0138 | 0.994 | 0.948 | -0.0060 | 0.0140 | 0.988 | 0.945 | -0.0060 | 0.0140 | 0.988 | 0.945 |
| beta1 | 0.0000 | 0.0088 | 0.994 | 0.966 | -0.0008 | 0.0119 | 0.988 | 0.961 | -0.0019 | 0.0177 | 0.988 | 0.955 |
| beta2 | 0.0021 | 0.0088 | 0.994 | 0.948 | 0.0022 | 0.0089 | 0.988 | 0.945 | 0.0021 | 0.0089 | 0.988 | 0.943 |
| beta3 | 0.0037 | 0.0087 | 0.994 | 0.923 | 0.0046 | 0.0126 | 0.988 | 0.923 | 0.0057 | 0.0183 | 0.988 | 0.917 |

#### interior N = 5000

| parameter | E bias | E RMSE | E SE def | E Wald cov | W bias | W RMSE | W SE def | W Wald cov | H bias | H RMSE | H SE def | H Wald cov |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| angle_BC | 0.10 | 2.02 | 1.000 | 0.944 | 0.10 | 2.02 | 1.000 | 0.944 | 1.16 | 12.79 | 1.000 | 0.936 |
| angle_DE | 0.09 | 2.87 | 1.000 | 0.934 | 0.09 | 2.87 | 1.000 | 0.934 | 0.09 | 11.72 | 1.000 | 0.930 |
| angle_FG | 0.07 | 3.18 | 1.000 | 0.928 | 0.07 | 3.18 | 1.000 | 0.928 | -0.28 | 13.06 | 1.000 | 0.920 |
| angle_HI | -0.06 | 3.29 | 1.000 | 0.938 | -0.06 | 3.29 | 1.000 | 0.938 | -0.07 | 3.27 | 1.000 | 0.940 |
| angle_JK | -0.08 | 3.07 | 1.000 | 0.946 | -0.08 | 3.07 | 1.000 | 0.946 | 0.29 | 13.09 | 1.000 | 0.938 |
| angle_LM | -0.04 | 2.67 | 1.000 | 0.948 | -0.04 | 2.67 | 1.000 | 0.948 | 0.68 | 11.59 | 1.000 | 0.944 |
| angle_NO | -0.01 | 1.95 | 1.000 | 0.936 | -0.01 | 1.95 | 1.000 | 0.936 | -0.36 | 12.84 | 1.000 | 0.928 |
| zeta_PA | 0.0048 | 0.0439 | 1.000 | 0.944 | 0.0048 | 0.0439 | 1.000 | 0.944 | 0.0045 | 0.0438 | 1.000 | 0.942 |
| zeta_BC | 0.0050 | 0.0439 | 1.000 | 0.928 | 0.0050 | 0.0439 | 1.000 | 0.928 | 0.0050 | 0.0438 | 1.000 | 0.926 |
| zeta_DE | 0.0020 | 0.0418 | 1.000 | 0.952 | 0.0020 | 0.0418 | 1.000 | 0.952 | 0.0022 | 0.0417 | 1.000 | 0.952 |
| zeta_FG | 0.0033 | 0.0422 | 1.000 | 0.954 | 0.0033 | 0.0422 | 1.000 | 0.954 | 0.0032 | 0.0421 | 1.000 | 0.954 |
| zeta_HI | 0.0020 | 0.0409 | 1.000 | 0.950 | 0.0020 | 0.0409 | 1.000 | 0.950 | 0.0020 | 0.0408 | 1.000 | 0.950 |
| zeta_JK | 0.0035 | 0.0412 | 1.000 | 0.938 | 0.0035 | 0.0412 | 1.000 | 0.938 | 0.0034 | 0.0411 | 1.000 | 0.936 |
| zeta_LM | 0.0041 | 0.0436 | 1.000 | 0.950 | 0.0041 | 0.0436 | 1.000 | 0.950 | 0.0041 | 0.0436 | 1.000 | 0.950 |
| zeta_NO | 0.0046 | 0.0422 | 1.000 | 0.952 | 0.0046 | 0.0422 | 1.000 | 0.952 | 0.0047 | 0.0422 | 1.000 | 0.950 |
| beta0 | -0.0018 | 0.0073 | 1.000 | 0.964 | -0.0018 | 0.0073 | 1.000 | 0.964 | -0.0018 | 0.0073 | 1.000 | 0.964 |
| beta1 | -0.0004 | 0.0063 | 1.000 | 0.934 | -0.0004 | 0.0063 | 1.000 | 0.934 | -0.0017 | 0.0152 | 1.000 | 0.926 |
| beta2 | 0.0009 | 0.0054 | 1.000 | 0.950 | 0.0009 | 0.0054 | 1.000 | 0.950 | 0.0009 | 0.0054 | 1.000 | 0.950 |
| beta3 | 0.0013 | 0.0052 | 1.000 | 0.934 | 0.0013 | 0.0052 | 1.000 | 0.934 | 0.0025 | 0.0144 | 1.000 | 0.926 |

### Real and published fits (AC5)

#### jz2017 (N = 1166, m = 3)

E: accepted TRUE, T = 81.169, df = 10. Gaps: W 0.000, H 0.000 T units.

| estimator | T | df | angles | zeta | beta | order | order changed |
|---|---|---|---|---|---|---|---|
| E | 81.169 | 10 | 90.0, 125.1, 170.4, 195.4, 250.7, 269.5, 294.2, 11.3 | 0.767, 0.931, 0.780, 0.861, 0.956, 0.942, 0.806, 1.000 | 0.450, 0.440, 0.074, 0.036 | 1 2 3 4 5 6 7 8 | - |

#### voc (N = 175, m = 1)

E: accepted TRUE, T = 16.697, df = 7. Gaps: W 0.000, H 0.000 T units.

| estimator | T | df | angles | zeta | beta | order | order changed |
|---|---|---|---|---|---|---|---|
| E | 16.697 | 7 | 360.0, 53.3, 112.3, 123.0, 192.1, 210.0, 268.4 | 0.929, 0.812, 0.979, 0.779, 0.808, 0.939, 0.831 | 0.639, 0.361 | 1 2 3 4 5 6 7 | - |

#### verbal (N = 1046, m = 2)

E: accepted TRUE, T = 3.994, df = 2. Gaps: W 3.158, H 3.628 T units.

| estimator | T | df | angles | zeta | beta | order | order changed |
|---|---|---|---|---|---|---|---|
| E | 3.994 | 2 | 360.0, 38.7, 67.5, 166.3, 221.6, 245.7 | 0.801, 0.859, 0.915, 0.983, 0.850, 0.784 | 0.737, 0.195, 0.068 | 1 2 3 4 5 6 | - |
| W | 0.836 | 2 | 360.0, 22.2, 202.0, 145.2, 112.4, 293.4 | 0.727, 0.906, 0.940, 0.865, 0.902, 0.814 | 0.749, 0.065, 0.186 | 1 2 5 4 3 6 | TRUE |
| H | 0.366 | 2 | 360.0, 337.5, 174.4, 210.2, 243.8, 42.0 | 0.700, 0.958, 0.855, 0.816, 0.999, 0.726 | 0.727, 0.027, 0.246 | 1 6 3 4 5 2 | TRUE |

#### demo (N = 2500, m = 3)

E: accepted TRUE, T = 2.229, df = 10. Gaps: W 0.000, H 0.000 T units.

| estimator | T | df | angles | zeta | beta | order | order changed |
|---|---|---|---|---|---|---|---|
| E | 2.229 | 10 | 90.0, 125.1, 167.8, 193.3, 251.0, 269.5, 293.6, 12.7 | 0.785, 0.881, 0.799, 0.878, 0.958, 0.948, 0.803, 0.929 | 0.442, 0.453, 0.068, 0.037 | 1 2 3 4 5 6 7 8 | - |

<!-- generated:end -->
