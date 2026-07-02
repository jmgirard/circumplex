# Perform analyses using the Structural Summary Method

Calculate SSM parameters with bootstrapped confidence intervals for a
variety of different analysis types. Depending on what arguments are
supplied, either mean-based or correlation-based analyses will be
performed, one or more groups will be used to stratify the data, and
contrasts between groups or measures will be calculated.

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
  measures_labels = NULL
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
  [`octants()`](http://circumplex.jmgirard.com/reference/octants.md)).
  The closed-form SSM estimator used here equals the
  ordinary-least-squares cosine fit only when `angles` are equally
  spaced around the circle (e.g., octants at 45-degree intervals); for
  unequally spaced angles it is the conventional Gurtman estimator, not
  a least-squares fit.

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
  resamples to use when estimating the confidence intervals (default =
  2000).

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

## Value

A list containing the results and description of the analysis.

- results:

  A data frame with the SSM parameter estimates

- details:

  A list with the number of bootstrap resamples (boots), the confidence
  interval percentage level (interval), and the angular displacement of
  scales (angles)

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

## See also

Other ssm functions:
[`ssm_parameters()`](http://circumplex.jmgirard.com/reference/ssm_parameters.md),
[`ssm_score()`](http://circumplex.jmgirard.com/reference/ssm_score.md),
[`ssm_table()`](http://circumplex.jmgirard.com/reference/ssm_table.md)

Other analysis functions:
[`ssm_parameters()`](http://circumplex.jmgirard.com/reference/ssm_parameters.md),
[`ssm_score()`](http://circumplex.jmgirard.com/reference/ssm_score.md)

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
#> Elevation         0.917      0.888      0.946
#> X-Value           0.351      0.324      0.378
#> Y-Value          -0.252     -0.282     -0.222
#> Amplitude         0.432      0.402      0.462
#> Displacement    324.292    320.907    327.921
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
#> Elevation         0.202      0.169      0.236
#> X-Value          -0.062     -0.094     -0.030
#> Y-Value           0.179      0.145      0.214
#> Amplitude         0.189      0.153      0.226
#> Displacement    108.967     99.334    118.620
#> Model Fit         0.957                      
#> 
#> 
#> # Profile [ASPD]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.124      0.089      0.159
#> X-Value          -0.099     -0.133     -0.063
#> Y-Value           0.203      0.167      0.237
#> Amplitude         0.226      0.190      0.263
#> Displacement    115.927    107.451    124.435
#> Model Fit         0.964                      
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
#> Elevation         0.946      0.907      0.983
#> X-Value           0.459      0.420      0.499
#> Y-Value          -0.310     -0.355     -0.268
#> Amplitude         0.554      0.509      0.599
#> Displacement    325.963    322.240    329.833
#> Model Fit         0.889                      
#> 
#> 
#> # Profile [Male]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.884      0.842      0.925
#> X-Value           0.227      0.192      0.262
#> Y-Value          -0.186     -0.225     -0.148
#> Amplitude         0.294      0.258      0.332
#> Displacement    320.685    313.267    327.988
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
#> Elevation         0.946      0.908      0.986
#> X-Value           0.459      0.420      0.496
#> Y-Value          -0.310     -0.355     -0.267
#> Amplitude         0.554      0.512      0.598
#> Displacement    325.963    321.951    329.924
#> Model Fit         0.889                      
#> 
#> 
#> # Profile [Male]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.884      0.842      0.929
#> X-Value           0.227      0.192      0.263
#> Y-Value          -0.186     -0.224     -0.148
#> Amplitude         0.294      0.258      0.333
#> Displacement    320.685    313.849    328.047
#> Model Fit         0.824                      
#> 
#> 
#> # Contrast [Male - Female]:
#> 
#>                  Estimate   Lower CI   Upper CI
#> Δ Elevation        -0.062     -0.118     -0.004
#> Δ X-Value          -0.232     -0.286     -0.181
#> Δ Y-Value           0.124      0.069      0.179
#> Δ Amplitude        -0.261     -0.317     -0.204
#> Δ Displacement     -5.278    -13.507      2.770
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
#> Elevation         0.202      0.168      0.236
#> X-Value          -0.062     -0.095     -0.029
#> Y-Value           0.179      0.146      0.212
#> Amplitude         0.189      0.154      0.224
#> Displacement    108.967     99.381    118.797
#> Model Fit         0.957                      
#> 
#> 
#> # Profile [ASPD]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.124      0.089      0.158
#> X-Value          -0.099     -0.134     -0.066
#> Y-Value           0.203      0.168      0.236
#> Amplitude         0.226      0.189      0.262
#> Displacement    115.927    107.782    124.564
#> Model Fit         0.964                      
#> 
#> 
#> # Contrast [ASPD - NARPD]:
#> 
#>                  Estimate   Lower CI   Upper CI
#> Δ Elevation        -0.079     -0.116     -0.041
#> Δ X-Value          -0.037     -0.077      0.000
#> Δ Y-Value           0.024     -0.014      0.060
#> Δ Amplitude         0.037     -0.001      0.075
#> Δ Displacement      6.960     -3.385     18.057
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
#> Elevation         0.172      0.128      0.218
#> X-Value          -0.080     -0.125     -0.033
#> Y-Value           0.202      0.153      0.250
#> Amplitude         0.217      0.168      0.268
#> Displacement    111.669     99.626    122.752
#> Model Fit         0.972                      
#> 
#> 
#> # Profile [NARPD: Male]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.244      0.194      0.295
#> X-Value          -0.029     -0.074      0.017
#> Y-Value           0.146      0.097      0.192
#> Amplitude         0.149      0.102      0.195
#> Displacement    101.248     83.457    119.191
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
#> Elevation         0.172      0.127      0.215
#> X-Value          -0.080     -0.127     -0.034
#> Y-Value           0.202      0.152      0.250
#> Amplitude         0.217      0.167      0.269
#> Displacement    111.669     99.844    123.191
#> Model Fit         0.972                      
#> 
#> 
#> # Profile [NARPD: Male]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.244      0.192      0.295
#> X-Value          -0.029     -0.072      0.014
#> Y-Value           0.146      0.098      0.191
#> Amplitude         0.149      0.105      0.195
#> Displacement    101.248     84.228    119.107
#> Model Fit         0.902                      
#> 
#> 
#> # Contrast [NARPD: Male - Female]:
#> 
#>                  Estimate   Lower CI   Upper CI
#> Δ Elevation         0.072      0.005      0.139
#> Δ X-Value           0.051     -0.012      0.113
#> Δ Y-Value          -0.056     -0.124      0.010
#> Δ Amplitude        -0.068     -0.138     -0.004
#> Δ Displacement    -10.421    -30.538     10.798
#> Δ Model Fit        -0.071                      
#> 
# }
```
