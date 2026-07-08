# Perform analyses using the Structural Summary Method

Calculate SSM parameters with confidence intervals (bootstrapped by
default, or Monte Carlo via `method`) for a variety of different
analysis types. Depending on what arguments are supplied, either
mean-based or correlation-based analyses will be performed, one or more
groups will be used to stratify the data, and contrasts between groups
or measures will be calculated.

## Usage

``` r
ssm_analyze(
  data,
  scales,
  angles = octants(),
  measures = NULL,
  grouping = NULL,
  contrast = FALSE,
  boots = 2000,
  interval = 0.95,
  listwise = TRUE,
  measures_labels = NULL,
  parallel = "no",
  ncpus = 1,
  method = "bootstrap"
)
```

## Arguments

- data:

  Required. A data frame or matrix containing at least circumplex
  scales.

- scales:

  Required. A character vector of column names, or a numeric vector of
  column indexes, from `data` that contains the circumplex scale scores
  to be analyzed.

- angles:

  Optional. A numeric vector containing the angular displacement of each
  circumplex scale included in `scales` (in degrees). (default =
  [`octants()`](http://circumplex.jmgirard.com/dev/reference/octants.md)).
  The closed-form SSM estimator used here equals the
  ordinary-least-squares cosine fit for equally spaced `angles` (e.g.,
  octants at 45-degree intervals) – more generally, for any angle set
  satisfying first- and second-harmonic balance. For angle sets
  violating that balance (generic unequally spaced sets), it is the
  conventional Gurtman estimator, not a least-squares fit, and the
  reported model fit is then no longer a bounded R-squared in `[0, 1]`
  (it can fall below 0).

- measures:

  Optional. Either `NULL` or a character vector of column names from
  `data` that contains one or more variables to be correlated with the
  circumplex scales and analyzed using correlation-based SSM analyses.

- grouping:

  Optional. Either `NULL` or a string that contains the column name from
  `data` of the variable that indicates the group membership of each
  observation.

- contrast:

  Optional. A logical indicating whether to output the difference
  between two measures' or two groups' SSM parameters. Can only be set
  to TRUE when there are exactly two measures and one group, one measure
  and two groups, or no measures and two groups (default = FALSE). The
  contrast is always the second level minus the first. For two groups,
  this is the second level of `grouping` alphabetically, unless
  `grouping` is already a factor with an explicit level order, in which
  case that order is used. For two measures, this is simply the second
  entry of `measures` as given (no reordering). The direction is shown
  in the result's Label (e.g., "Male - Female").

- boots:

  Optional. A single positive whole number indicating how many bootstrap
  resamples (or, when `method = "montecarlo"`, Monte Carlo draws) to use
  when estimating the confidence intervals (default = 2000).

- interval:

  Optional. A single positive number between 0 and 1 (exclusive) that
  indicates what confidence level to use when estimating the confidence
  intervals (default = 0.95).

- listwise:

  Optional. A logical indicating whether missing values should be
  handled by listwise deletion (TRUE) or pairwise deletion (FALSE). Note
  that pairwise deletion may result in different missing data patterns
  in each bootstrap resample and is slower to compute (default = TRUE).

- measures_labels:

  Optional. Either `NULL` or a character vector providing a label for
  each measure provided in `measures` (in the same order) to appear in
  the results as well as tables and plots derived from the results.

- parallel:

  Optional. A string indicating whether to distribute the bootstrap
  computation across multiple CPU cores: "no" (default), "multicore"
  (process forking; available on macOS and Linux, ignored on Windows),
  or "snow" (a local PSOCK cluster; available on all platforms). Passed
  to [`boot`](https://rdrr.io/pkg/boot/man/boot.html). Because the
  bootstrap resample indices are drawn in the main R process before any
  work is distributed, results for a given
  [`set.seed()`](https://rdrr.io/r/base/Random.html) are identical
  regardless of the `parallel` and `ncpus` settings.

- ncpus:

  Optional. A single positive whole number indicating how many CPU cores
  to use when `parallel` is not "no" (default = 1).

- method:

  Optional. A string indicating how to estimate the confidence
  intervals: "bootstrap" (default) resamples the data, whereas
  "montecarlo" draws parameter replicates from the asymptotic sampling
  distribution of the group mean vector (mean-based analyses) or the
  measure-scale correlation vector (correlation-based analyses) – a
  multivariate normal with empirically estimated covariance – and
  propagates them through the SSM parameter transformation. The Monte
  Carlo method is much faster for large samples but relies on the
  asymptotic normality of the means or correlations, so prefer the
  bootstrap for small samples; it also requires listwise-complete data.
  Correlations are drawn jointly across measures within each group on
  the Fisher z scale and back-transformed. The `parallel` and `ncpus`
  arguments apply only to the bootstrap.

## Value

A list containing the results and description of the analysis.

- results:

  A data frame with the SSM parameter estimates

- details:

  A list with the number of bootstrap resamples or Monte Carlo draws
  (boots), the confidence interval percentage level (interval), the
  angular displacement of scales (angles), and the interval estimation
  method (method)

- call:

  A language object containing the function call that created this
  object

- scores:

  A data frame containing the mean scale scores

- type:

  A string indicating what type of SSM analysis was done

The profile displacement parameter is reported in the half-open interval
`[0, 360)` degrees. A profile that peaks exactly at the 0/360 degree
boundary is reported as approximately 360 (equivalently 0, the same
direction); which of the two appears is a floating-point detail and both
denote the same pole. Contrast displacements are instead reported as a
signed difference in `(-180, 180]` degrees (see the "Contrast" block in
the printed output).

Degenerate profiles (flat or zero-amplitude) have undefined displacement
(and fit, if flat), which is reported as `NA` with a warning. Bootstrap
resamples that produce degenerate profiles (e.g., a resampled measure
with zero variance) are excluded from the confidence intervals with a
warning reporting how many were dropped; the intervals are then
conditional on estimability.

\[0,
360)`degrees. A profile that peaks exactly at the 0/360 degree boundary is reported as approximately 360 (equivalently 0, the same direction); which of the two appears is a floating-point detail and both denote the same pole. Contrast displacements are instead reported as a signed difference in`(-180,
180\]:
R:0,%20360)%60%20degrees.%20A%20profile%20that%20peaks%20exactly%20at%20the%200/360%20degree%0A%20%20boundary%20is%20reported%20as%20approximately%20360%20(equivalently%200,%20the%20same%0A%20%20direction);%20which%20of%20the%20two%20appears%20is%20a%20floating-point%20detail%20and%20both%0A%20%20denote%20the%20same%20pole.%20Contrast%20displacements%20are%20instead%20reported%20as%20a%0A%20%20signed%20difference%20in%20%60(-180,%20180

## Reproducibility

This function consumes R's random number stream (so do
`cpm_fit(ci_method = "bootstrap")`,
[`cpm_simulate()`](http://circumplex.jmgirard.com/dev/reference/cpm_simulate.md),
and
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md);
[`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md)/[`ssm_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters.md)
and the tidying functions are deterministic). Call
[`set.seed()`](https://rdrr.io/r/base/Random.html) immediately before
`ssm_analyze()` for reproducible confidence intervals:

- **Bootstrap** (`method = "bootstrap"`, the default): the same seed
  gives byte-identical `results`, *regardless of* the `parallel`/`ncpus`
  settings (see their descriptions below), because
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html) draws all
  resample indices from the seed before any work is parallelized.

- **Monte Carlo** (`method = "montecarlo"`): the same seed gives
  byte-identical `results`. Adding a group or measure, or reordering
  `scales`/`measures`, changes the random draw sequence, so results are
  reproducible for a fixed call but will not match after such structural
  edits even with the same seed.

- The two methods are **not** expected to agree numerically for the same
  seed – they consume the random stream in unrelated ways. Their
  statistical agreement (validated on real data; see
  [`vignette("introduction-to-ssm-analysis")`](http://circumplex.jmgirard.com/dev/articles/introduction-to-ssm-analysis.md))
  is a separate property from RNG reproducibility.

- Increasing `boots` changes the CI by design (more resamples/draws
  should tighten Monte Carlo error), so results are not expected to be
  stable across different `boots` values, only within a fixed call.

## See also

Other ssm functions:
[`plot.circumplex_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/plot.circumplex_ci_accuracy.md),
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md),
[`ssm_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters.md),
[`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md),
[`ssm_sem()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem.md),
[`ssm_sem_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_parameters.md),
[`ssm_table()`](http://circumplex.jmgirard.com/dev/reference/ssm_table.md)

Other analysis functions:
[`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md),
[`cpm_simulate()`](http://circumplex.jmgirard.com/dev/reference/cpm_simulate.md),
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md),
[`ssm_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters.md),
[`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md),
[`ssm_sem()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem.md),
[`ssm_sem_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_parameters.md)

## Examples

``` r
# Load example data
data("jz2017")

# Single-group mean-based SSM
ssm_analyze(
  jz2017,
  scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
)
#> 
#> # Profile [All]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.917      0.889      0.946
#> X-Value           0.351      0.324      0.378
#> Y-Value          -0.252     -0.281     -0.224
#> Amplitude         0.432      0.402      0.461
#> Displacement    324.292    320.751    327.676
#> Model Fit         0.878                      
#> 

# Single-group correlation-based SSM
ssm_analyze(
  jz2017,
  scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  measures = c("NARPD", "ASPD")
)
#> 
#> # Profile [NARPD]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.202      0.169      0.237
#> X-Value          -0.062     -0.094     -0.028
#> Y-Value           0.179      0.145      0.214
#> Amplitude         0.189      0.155      0.226
#> Displacement    108.967     99.002    118.680
#> Model Fit         0.957                      
#> 
#> 
#> # Profile [ASPD]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.124      0.090      0.157
#> X-Value          -0.099     -0.134     -0.064
#> Y-Value           0.203      0.168      0.240
#> Amplitude         0.226      0.190      0.266
#> Displacement    115.927    107.286    124.541
#> Model Fit         0.964                      
#> 

# Monte Carlo confidence intervals (faster for large samples)
ssm_analyze(
  jz2017,
  scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  method = "montecarlo"
)
#> 
#> # Profile [All]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.917      0.889      0.945
#> X-Value           0.351      0.323      0.379
#> Y-Value          -0.252     -0.282     -0.223
#> Amplitude         0.432      0.403      0.463
#> Displacement    324.292    320.840    327.907
#> Model Fit         0.878                      
#> 
# \donttest{
# Multiple-group mean-based SSM
ssm_analyze(
  jz2017,
  scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  grouping = "Gender"
)
#> 
#> # Profile [Female]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.946      0.908      0.983
#> X-Value           0.459      0.419      0.497
#> Y-Value          -0.310     -0.353     -0.265
#> Amplitude         0.554      0.509      0.598
#> Displacement    325.963    322.036    329.958
#> Model Fit         0.889                      
#> 
#> 
#> # Profile [Male]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.884      0.843      0.926
#> X-Value           0.227      0.194      0.261
#> Y-Value          -0.186     -0.227     -0.149
#> Amplitude         0.294      0.259      0.331
#> Displacement    320.685    313.535    327.870
#> Model Fit         0.824                      
#> 

# Multiple-group mean-based SSM with contrast
ssm_analyze(
  jz2017,
  scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  grouping = "Gender",
  contrast = TRUE
)
#> 
#> # Profile [Female]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.946      0.906      0.985
#> X-Value           0.459      0.419      0.499
#> Y-Value          -0.310     -0.354     -0.269
#> Amplitude         0.554      0.509      0.601
#> Displacement    325.963    322.085    329.738
#> Model Fit         0.889                      
#> 
#> 
#> # Profile [Male]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.884      0.843      0.926
#> X-Value           0.227      0.190      0.262
#> Y-Value          -0.186     -0.224     -0.149
#> Amplitude         0.294      0.255      0.332
#> Displacement    320.685    313.633    327.912
#> Model Fit         0.824                      
#> 
#> 
#> # Contrast [Male - Female]:
#> 
#>                  Estimate   Lower CI   Upper CI
#> Δ Elevation        -0.062     -0.117     -0.004
#> Δ X-Value          -0.232     -0.285     -0.179
#> Δ Y-Value           0.124      0.066      0.180
#> Δ Amplitude        -0.261     -0.318     -0.200
#> Δ Displacement     -5.278    -13.287      2.978
#> Δ Model Fit        -0.066                      
#> 

# Single-group correlation-based SSM with contrast
ssm_analyze(
  jz2017,
  scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  measures = c("NARPD", "ASPD"),
  contrast = TRUE
)
#> 
#> # Profile [NARPD]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.202      0.170      0.233
#> X-Value          -0.062     -0.097     -0.029
#> Y-Value           0.179      0.145      0.213
#> Amplitude         0.189      0.154      0.225
#> Displacement    108.967     99.181    119.113
#> Model Fit         0.957                      
#> 
#> 
#> # Profile [ASPD]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.124      0.090      0.159
#> X-Value          -0.099     -0.134     -0.064
#> Y-Value           0.203      0.165      0.239
#> Amplitude         0.226      0.189      0.265
#> Displacement    115.927    107.395    124.309
#> Model Fit         0.964                      
#> 
#> 
#> # Contrast [ASPD - NARPD]:
#> 
#>                  Estimate   Lower CI   Upper CI
#> Δ Elevation        -0.079     -0.114     -0.042
#> Δ X-Value          -0.037     -0.074      0.002
#> Δ Y-Value           0.024     -0.014      0.061
#> Δ Amplitude         0.037     -0.001      0.075
#> Δ Displacement      6.960     -3.617     17.108
#> Δ Model Fit         0.007                      
#> 

# Multiple-group correlation-based SSM
ssm_analyze(
  jz2017,
  scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  measures = "NARPD",
  grouping = "Gender"
)
#> 
#> # Profile [NARPD: Female]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.172      0.128      0.217
#> X-Value          -0.080     -0.126     -0.034
#> Y-Value           0.202      0.152      0.249
#> Amplitude         0.217      0.166      0.266
#> Displacement    111.669     99.779    123.161
#> Model Fit         0.972                      
#> 
#> 
#> # Profile [NARPD: Male]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.244      0.192      0.296
#> X-Value          -0.029     -0.075      0.014
#> Y-Value           0.146      0.098      0.191
#> Amplitude         0.149      0.105      0.196
#> Displacement    101.248     84.330    119.449
#> Model Fit         0.902                      
#> 

# Multiple-group correlation-based SSM with contrast
ssm_analyze(
  jz2017,
  scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  measures = "NARPD",
  grouping = "Gender",
  contrast = TRUE
)
#> 
#> # Profile [NARPD: Female]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.172      0.128      0.217
#> X-Value          -0.080     -0.128     -0.038
#> Y-Value           0.202      0.152      0.249
#> Amplitude         0.217      0.167      0.269
#> Displacement    111.669    100.674    123.270
#> Model Fit         0.972                      
#> 
#> 
#> # Profile [NARPD: Male]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.244      0.192      0.296
#> X-Value          -0.029     -0.074      0.013
#> Y-Value           0.146      0.099      0.192
#> Amplitude         0.149      0.103      0.196
#> Displacement    101.248     85.069    118.602
#> Model Fit         0.902                      
#> 
#> 
#> # Contrast [NARPD: Male - Female]:
#> 
#>                  Estimate   Lower CI   Upper CI
#> Δ Elevation         0.072      0.003      0.140
#> Δ X-Value           0.051     -0.011      0.114
#> Δ Y-Value          -0.056     -0.127      0.011
#> Δ Amplitude        -0.068     -0.139     -0.001
#> Δ Displacement    -10.421    -30.566     10.325
#> Δ Model Fit        -0.071                      
#> 
# }
```
