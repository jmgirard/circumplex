# Fit Browne's circular stochastic process model (circumplex fit statistics)

Estimate Browne's (1992) circular stochastic process model (CPM) for the
correlational structure of a set of circumplex scales or items, the
native replacement for the archived CircE package. Each variable is
modeled as a point on a circle at an estimated angle, with a communality
index and a shared correlation function; the fit of that structure is
summarized with the usual covariance-structure indices (chi-square,
RMSEA, SRMR, CFI, TLI).

## Usage

``` r
cpm_fit(
  data = NULL,
  scales = NULL,
  angles = octants(),
  cormat = NULL,
  n = NULL,
  m = 3,
  model = c("quasi-circumplex", "constrained-angles", "equal-communality", "circulant"),
  scaling = c("unit", "free"),
  reference = 1,
  interval = 0.95,
  ci_method = c("bootstrap", "analytic"),
  boots = 2000,
  listwise = TRUE
)
```

## Arguments

- data:

  A data frame or matrix containing the circumplex scales (raw-data
  path). Supply exactly one of `data` or `cormat`.

- scales:

  For the raw-data path, a character vector of column names (or a
  numeric vector of column indexes) selecting the circumplex scales. For
  the `cormat` path, optional labels for the variables (defaults to the
  matrix dimnames, or `V1`, `V2`, ...).

- angles:

  A numeric vector of the theoretical angular displacement of each
  scale, in degrees, used both as the reference/identifying angle and as
  optimization start values (default =
  [`octants()`](http://circumplex.jmgirard.com/dev/reference/octants.md)).
  Its length must match the number of scales.

- cormat:

  A correlation matrix (the matrix-input path, CircE-style). Supply
  exactly one of `data` or `cormat`. Must be symmetric with a unit
  diagonal and positive definite.

- n:

  For the `cormat` path, the sample size (number of observations) the
  correlation matrix was computed from. The test statistic uses `N - 1`
  (the Wishart degrees of freedom); pass the raw sample size here.

- m:

  The number of harmonics in the correlation function (default = 3, the
  octant-scale convention). Capped at `floor((p - 1) / 2)` for the
  free-angle variants and `floor(p / 2)` for the fixed-angle variants.

- model:

  The model variant (design of Browne 1992): `"quasi-circumplex"`
  (default; free angles and communalities), `"constrained-angles"`
  (angles fixed at their theoretical values), `"equal-communality"` (a
  single shared communality), or `"circulant"` (both constraints).

- scaling:

  The covariance-scaling family, orthogonal to `model`: `"unit"`
  (default) fits the correlation structure with a unit model-implied
  diagonal (the historical behaviour); `"free"` fits Browne's covariance
  structure \\\Sigma = D\_\sigma P D\_\sigma\\ with `p` free variance
  scales, the parameterization CIRCUM and CircE use, so `cpm_fit()` can
  reproduce their published output exactly. Free scaling adds `p`
  parameters but leaves the degrees of freedom unchanged (it fits the
  `p` extra diagonal covariance moments). The fitted \\\hat\sigma^2\\
  are reported as a `VarRatio` column (the ratio of reproduced to input
  variance); they carry no confidence interval, and the analytic
  intervals for the remaining parameters are not yet coverage-validated
  for this family ([`summary()`](https://rdrr.io/r/base/summary.html)
  cautions). The input is still a correlation matrix (unit diagonal).

- reference:

  The index into `scales` of the variable whose angle is fixed at its
  theoretical value to identify the rotation (default = 1).

- interval:

  The confidence level for the parameter intervals (default = 0.95). The
  RMSEA interval is always the conventional 90 percent.

- ci_method:

  How to construct the parameter confidence intervals: `"bootstrap"`
  (the default on the raw-data path) resamples rows, recomputes the
  correlation matrix, and refits the model warm-started from the
  reported solution; `"analytic"` uses Wald intervals from the
  information matrix. On the `cormat` path only `"analytic"` is
  available (there is no raw data to resample), and it is the default
  there.

- boots:

  The number of bootstrap resamples for `ci_method = "bootstrap"`
  (default = 2000).

- listwise:

  Whether to handle missing values by listwise deletion. Only listwise
  deletion is supported in this release (default = TRUE).

## Value

A `circumplex_cpm` object: a list with `results` (a data frame of
estimated angles and communality indices with confidence intervals),
`betas` (the correlation-function weights), `fit` (the fit indices),
`corfun` (the estimated correlation function), `matrices` (the sample
and model-implied matrices and residuals), and `details` (model,
diagnostics, and settings). See
[`print.circumplex_cpm()`](http://circumplex.jmgirard.com/dev/reference/print.circumplex_cpm.md)
and
[`summary.circumplex_cpm()`](http://circumplex.jmgirard.com/dev/reference/summary.circumplex_cpm.md).

## Confidence intervals

The bootstrap (the raw-data default) refits the model to each resampled
correlation matrix, warm-started from the reported solution, and forms
percentile intervals; angle replicates are pooled with the package's
circular quantile machinery, so an angle interval that straddles the
0/360 boundary is reported wrapped (its lower limit numerically exceeds
its upper limit, as with displacement intervals in
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)).
Resamples with a degenerate (non-positive-definite) correlation matrix
or a refit failing the convergence acceptance criterion are excluded
with a warning reporting how many; the intervals are then conditional on
estimability. Analytic (Wald) intervals are asymptotically valid but can
materially mis-cover at field-typical sample sizes;
[`summary()`](https://rdrr.io/r/base/summary.html) prints a caution
below `n = 2000`. Analytic angle intervals are reported on the unwrapped
branch of the estimate (endpoints may fall outside \[0, 360) near the
boundary).

## Reproducibility

Only the bootstrap consumes R's random number stream; the engine's point
estimates, fit indices, and the analytic intervals are deterministic, so
the estimates are identical across seeds and the default `cormat`-path
fit never touches the stream. Call
[`set.seed()`](https://rdrr.io/r/base/Random.html) immediately before
`cpm_fit()` for reproducible bootstrap intervals. All resample indices
are drawn in one block before any refitting, so a given seed yields the
same intervals regardless of how many replicates are later excluded.

## References

Browne, M. W. (1992). Circumplex models for correlation matrices.
*Psychometrika, 57*(4), 469-497.

## See also

Other analysis functions:
[`cpm_simulate()`](http://circumplex.jmgirard.com/dev/reference/cpm_simulate.md),
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md),
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md),
[`ssm_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters.md),
[`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md),
[`ssm_sem()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem.md),
[`ssm_sem_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_parameters.md)

## Examples

``` r
# Raw-data path on the eight IIP-SC octant scales (bootstrap CIs; a small
# `boots` keeps the example fast -- the default is 2000)
data("jz2017")
scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
set.seed(12345)
fit <- cpm_fit(jz2017, scales = scales, boots = 100)
#> Warning: CPM Hessian is ill-conditioned (condition number 1.83e+14): angles may be clustered or parameters weakly determined.
#> Warning: 2 of 100 bootstrap resamples were excluded (0 with a degenerate or non-positive-definite correlation matrix, 2 failing the convergence acceptance criterion); the confidence intervals are based on the remaining 98 replicates and are conditional on estimability.
fit
#> 
#> Circular Process Model (Browne, 1992) 
#> Model:             quasi-circumplex 
#> Harmonics (m):     3 
#> Sample size (N):   1166 
#> Reference scale:   PA 
#> 
#>  Scale Angle_theory   Angle Angle_lci Angle_uci  Zeta Zeta_lci Zeta_uci
#>     PA           90  90.000    90.000    90.000 0.767    0.679    0.888
#>     BC          135 125.074   112.759   136.068 0.931    0.861    1.000
#>     DE          180 170.353   156.422   183.533 0.780    0.738    0.836
#>     FG          225 195.425   185.156   205.763 0.861    0.829    0.902
#>     HI          270 250.721   243.600   257.894 0.956    0.940    0.977
#>     JK          315 269.491   261.621   277.776 0.942    0.930    0.956
#>     LM          360 294.230   285.726   302.375 0.806    0.757    0.854
#>     NO           45  11.305     1.719    21.025 1.000    1.000    1.000
#>  Communality
#>        0.589
#>        0.868
#>        0.608
#>        0.741
#>        0.914
#>        0.888
#>        0.650
#>        1.000
#> 
#> Fit: χ²(10) = 81.169, p = <1e-04; RMSEA = 0.078 [0.063, 0.094]; SRMR = 0.042; CFI = 0.984
#>   Note: a communality index reached its upper boundary (ζ > 0.995, a Heywood-type solution).
#>   Note: 2 of 100 bootstrap resamples were excluded (0 degenerate, 2 non-convergent); the intervals are based on 98 replicates and are conditional on estimability.

# Matrix-input path (supply the sample size; analytic CIs)
R <- cor(jz2017[scales])
cpm_fit(cormat = R, scales = scales, n = nrow(jz2017))
#> Warning: CPM Hessian is ill-conditioned (condition number 1.83e+14): angles may be clustered or parameters weakly determined.
#> 
#> Circular Process Model (Browne, 1992) 
#> Model:             quasi-circumplex 
#> Harmonics (m):     3 
#> Sample size (N):   1166 
#> Reference scale:   PA 
#> 
#>  Scale Angle_theory   Angle Angle_lci Angle_uci  Zeta Zeta_lci Zeta_uci
#>     PA           90  90.000        NA        NA 0.767       NA       NA
#>     BC          135 125.074        NA        NA 0.931       NA       NA
#>     DE          180 170.353        NA        NA 0.780       NA       NA
#>     FG          225 195.425        NA        NA 0.861       NA       NA
#>     HI          270 250.721        NA        NA 0.956       NA       NA
#>     JK          315 269.491        NA        NA 0.942       NA       NA
#>     LM          360 294.230        NA        NA 0.806       NA       NA
#>     NO           45  11.305        NA        NA 1.000       NA       NA
#>  Communality
#>        0.589
#>        0.868
#>        0.608
#>        0.741
#>        0.914
#>        0.888
#>        0.650
#>        1.000
#> 
#> Fit: χ²(10) = 81.169, p = <1e-04; RMSEA = 0.078 [0.063, 0.094]; SRMR = 0.042; CFI = 0.984
#>   Note: a communality index reached its upper boundary (ζ > 0.995, a Heywood-type solution).
```
