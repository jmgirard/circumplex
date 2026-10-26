# M167: CPM interval coverage per component, the record

This file records the runs of `devel/m167-interval-oracle.R`. The design and
the pass rule are the pre-registration block at the top of that script. The
figures below come from `devel/m167-interval-results.rds` (the full run),
`devel/m167-interval-control.rds` (the control run) and
`devel/m167-interval-confirm.rds` (the confirmation run).

## What ran

- The full run in the results file ran from commit b90b6ddd on 2026-10-21.
  It took 117.0 minutes on 17 cores, with 500 replicates per cell and 1000
  resamples per bootstrap. The file records that commit, read when the run
  started.
- It replaces a first full run from commit 908ed678. That run's results
  file stores 8a2c6fb2, the commit when it saved, and the two differ only in
  roxygen lines of `R/cpm_fit.R`. The review of that run
  found three defects in the harness: P1 gave false endpoints near a Heywood
  wall, an error inside one method dropped the replicate from every method,
  and the confirmation seeds collided with the main run's seeds. The block's
  "CORRECTIONS AFTER THE FIRST FULL RUN" heading lists the fixes. None
  changes the truths, cells, methods, scoring rule, band or pass rule. Git
  holds the first run's files.
- The control run scored B1 and B2 only, at the six raw-data cells, under the
  M4 seeds. It ran before the design review and took 33 minutes.
- The confirmation run ran from commit 2fdfadc4 and took 53.8 minutes.
  Between b90b6ddd and 2fdfadc4 only comment lines in `R/cpm_fit.R` and
  `R/cpm_oop.R` changed (`git diff b90b6ddd 2fdfadc4 -- devel R src`).
- The prototypes at b90b6ddd passed the 12 checks then in
  `devel/m167-interval-checks.R`. The third review replaced check 4d, and
  the rebuilt P1 fails the new check (next section).
- Run times are the elapsed times each run printed. The .rds files do not
  store them.

## P1 is not established

The third review found that P1 still returned endpoints that were too
narrow: in Heywood-regime fits a lower basin existed that its two starts
per evaluation missed. RR27 gave a rebuilt P1 and a stop rule fixed before
any code (M167-D6, M167-D7, and the block's "CORRECTIONS AFTER THE THIRD
REVIEW" heading). The rebuilt P1 failed both of the rule's gates:

- The endpoint check re-profiles each endpoint from its own starts. The
  statistic it found there was below 3.79 at 10 of 141 endpoints and above
  3.89 at none, and 12 of 120 components were unverified. The plant, the
  two-start P1, failed the check with 63 endpoints below and 30 above.
- The 10-fit timing pilot (`devel/m167-p1-pilot.R`,
  `devel/m167-p1-pilot.rds`) gave a median of 8.71 CPU-seconds per fit
  against a limit of 6. The rule asks for an unloaded core. At the pilot's
  end the load averages were 8.58, 6.51 and 6.26 (1, 5 and 15 minutes). The endpoint check fails on its own, so
  the outcome does not depend on the pilot.

So P1 is out of the verdict, and every P1 figure below is reported as not
established. They come from the P1 of b90b6ddd, whose endpoints the third
review showed too narrow. Nothing was re-run after the third review. The
review's two other script changes change no scored figure. A removed weight
now keeps [0, 0] after a method error, but no replicate errored in the full
run. The run now saves `git status`, which scores nothing.

## Which estimator the figures measure

Every figure scores the estimate `cpm_fit()` ships. That estimate is the
maximum-likelihood solution in the basin the engine's theory-centred starts
reach, which is not always the global minimum (D-071). The third review
refit 37 boundary N = 250 fits from random starts. In 6 of them a restart
found a lower discrepancy, by 0.3 to 2.0 in N times F. The study-wide share of
such fits was to come from P1's anchor pass, so with P1 removed it is not
measured here.

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
  and 3, 0 and 0 above that. At the interior truth it excluded 94, 52 and 10,
  then 0. No replicate errored.

## AC2 control

Pooled as M4 pooled it, every B1 and B2 entry at the six M4 cells lies within
.021 of the M4 record (.020 at three decimals). The largest difference is B2
beta at boundary N = 500, .869 against .849. The B1 differences are at most
.007. The accepted-fit counts differ slightly from M4's, for example 448
against 452 at interior N = 500. This run did not look into why.

The full run reproduces the control run's pooled B1 and B2 figures exactly,
except B2 beta at boundary N = 250 (.005 lower). There a removed weight whose
standard error is undefined now gets the interval [0, 0], as the block's
scoring rule says, where the control run left it with no interval.

## The shipped bootstrap (B1) per weight

The table gives coverage per beta weight. In parentheses is the share of
replicates whose interval lies below or above the truth, for the side that
carries the misses.

| Cell | beta_0 | beta_1 | beta_2 | beta_3 |
|---|---|---|---|---|
| boundary N=250 | .741 (below .26) | .928 | .785 (above .22) | .741 (above .22) |
| boundary N=500 | .674 (below .33) | .930 | .732 (above .27) | .740 (above .26) |
| boundary N=1000 | .656 (below .34) | .949 | .756 (above .24) | .731 (above .27) |
| interior N=250 | .842 (below .15) | .953 | .906 | .869 (above .13) |
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
beta_2 and beta_3 fall mostly above it.

B1 communalities miss above the truth. At N = 250 they cover .79 to .85 per
scale at the boundary truth and .73 to .80 at the interior truth. Of their
intervals 15 to 26% lie above the truth and 0 to 2% below. At N = 500 they
cover .85 to .91, with 9 to 15% above.

B1 angles pass at N = 250 and 1000 under the pass rule. At boundary N = 500
the LM angle covers .871 and fails.

B2's figures are conditional on a defined Wald SE. The B1 figures on those
same fits follow, for N = 250, 500 and 1000. At the boundary truth B1
covers beta_0 at .83, .75 and .70 there, and at .74, .67 and .66 on all
accepted fits. At the interior truth the figures are .90, .88 and .86
there, and .84, .81 and .81 on all fits. B1 communalities on
those fits cover .87 to .97 at the boundary truth and .78 to .97 at the
interior truth.

## Why: the diagnostics

- The estimator is biased. At the boundary truth the bias of beta_0-hat is
  .7 to 1.05 of its SD from N = 250 to 2000, and still .75 SD at N = 20000.
  At the interior truth it is .65 to .80 SD at N <= 1000 and .08 SD at
  N = 20000.
- The bootstraps see part of that bias. At the boundary truth the mean B1
  replicate shift of beta_0 is -.011 to -.012, against an estimator bias of
  -.023 to -.030. At the interior truth it is -.004 to -.008, against -.012
  to -.022. The F2 shift is similar.
- The variant-B arm refits the same data with the angles fixed at theory.
  Every variant-B fit was accepted. It removes the beta_0 bias: under .07 of
  its SD in every raw-data cell. Its Heywood rate is 0 to 3% at the boundary
  truth, so the pre-registered reading confirms the angle and communality
  mode as the cause at all three N there. At the interior truth its Heywood
  rate is 37%, 8% and 1% at N = 250, 500 and 1000, so the reading confirms
  the mode only at N = 1000.
- At N = 250, 500 and 1000, 78%, 59% and 25% of accepted fits are Heywood
  fits at the boundary truth, and 91%, 61% and 21% at the interior truth.
- The Wald standard error is defined on 40%, 56% and 85% of accepted fits at
  the boundary truth, and on 27%, 60% and 87% at the interior truth.

## Verdict under the pre-registered rule

In the strict form, no (method, path, parameter family) passes at every cell
listed for its path. In the ladder form ("passes from N"), two entries
pass:

| Path | Method | Family | Passes from N | Unconditional | Cells passed |
|---|---|---|---|---|---|
| raw data | B1 | angles | 1000 | 1000 | 250, 1000 |
| correlation matrix | F2bc | weights | 5000 | 5000 | 5000, 20000 |

The results file also scores P1 communalities on the raw-data path as
passing from N = 1000. That figure is not established, because P1 is out
of the verdict (above).

The raw-data path's largest cell is N = 1000. So the B1 entry means a pass
at N = 1000 and says nothing about larger N there. At the boundary truth,
F2bc weights pass at N = 5000 and 20000
only through the Monte Carlo allowance: beta_0 covers .894 and .888, and
beta_3 .890 at N = 20000. At the interior truth every weight is in the band.

Every other entry fails at its largest listed N, so its ladder reads "never".
Some entries pass at cells below the top of the ladder:

- F2p angles and communalities pass at N = 2000 and 5000.
- F2bc communalities pass at N = 1000, 2000 and 5000. So do P1
  communalities, a figure that is not established.
- F1 communalities pass at N = 2000.

None of these passes at boundary N = 20000. There the weak angle and
communality mode of the estimator remains: the angle SDs are 3 to 8 degrees
and the beta_0 bias is .75 of its SD.

The per-component tables give these reasons for the other failures:

- F1 communality intervals cover .89 to .98 at N <= 1000 on the fits with a
  defined SE. But up to 44% (boundary) and 85% (interior) of them are vacuous
  (width over .5), so they fail the vacuous-interval rule. Up to 60% and 73%
  of fits have no F1 interval.
- P1 is out of the verdict, so its figures are not established. Its
  communality intervals cover .90 to .97 at N <= 1000, from endpoints the
  third review showed too narrow.
- The basic bootstrap (Bb, F2b) covers communalities at only .47 to .79 at
  N <= 1000.
- At the boundary truth, F2bc is the only method whose beta_0 passes
  unconditionally at any cell, .894 at N = 5000 and .888 at N = 20000, both
  inside the allowance and below .90. B2 and F1 pass beta_0 only
  conditionally, at N = 250 and 1000, where 60% and 15% of fits have no
  interval. B2 and F1 cover beta_0 at .83 to .87 from N = 2000 to 20000.
- At the boundary truth, no candidate covers beta_0 inside the band
  unconditionally at N <= 1000. F3 and F2bc cover it at .80 to .86 there.
  At the interior truth, F3's beta_0 passes at N = 1000.

## Confirmation

The confirmation run (`M167_CONFIRM=1`) re-ran the raw-data cells where the
main run passed an entry: N = 250 and 1000, both truths, 500 replicates, on
the seed base 30261022, which no seed of the main run reaches.

The block asks to confirm each entry that passes, under either verdict
form, at a raw-data cell. The run took every raw-data cell where any entry
passed at the cell level, which is wider. For example, it re-ran F2bc
communalities at N = 1000, an entry whose ladder reads "never". The P1 rows
below confirm figures that are not established.

| Path | Method | Family | N | Confirmed |
|---|---|---|---|---|
| raw data | B1 | angles | 250 | no |
| raw data | B1 | angles | 1000 | yes, unconditional |
| raw data | P1 (not established) | communalities | 1000 | yes, unconditional |
| correlation matrix | F2bc | communalities | 1000 | yes, unconditional |
| correlation matrix | P1 (not established) | communalities | 1000 | yes, unconditional |

B1 angles at boundary N = 250 cover .87 to .92 per scale on the fresh seeds,
and 3 of the 7 angles fail. So the N = 250 pass of the main run does not
hold up. The ladder entry "B1 angles pass from N = 1000" does not rest on
N = 250 and stands.

## Seeds

The correlation-matrix cells use the base 20261021, which sits 315 above the
M4 base. So a correlation-matrix replicate can start from the RNG state of a
raw-data replicate. The two draw different things (a Wishart matrix against
simulated rows) at different N, and each cell is scored on its own. The
block records this and leaves it as it is.
