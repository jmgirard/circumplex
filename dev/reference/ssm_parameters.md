# Calculate Structural Summary Method parameters for a set of scores

Calculate SSM parameters (without confidence intervals) for a set of
scores and generate a data frame with customizable labels for each
parameter value. This function requires the input to be a numeric vector
(or coercable to one) and returns only the parameters. See
[`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md)
for a similar function that calculates SSM parameters for each row of a
data frame.

## Usage

``` r
ssm_parameters(
  scores,
  angles = octants(),
  prefix = "",
  suffix = "",
  e_label = "Elev",
  x_label = "Xval",
  y_label = "Yval",
  a_label = "Ampl",
  d_label = "Disp",
  f_label = "Fit"
)
```

## Arguments

- scores:

  Required. A numeric vector (or single row data frame) containing one
  score for each of a set of circumplex scales.

- angles:

  Required. A numeric vector containing the angular displacement of each
  circumplex scale included in `scores` (in degrees). The closed-form
  SSM estimator used here equals the ordinary-least-squares cosine fit
  for equally spaced `angles` (e.g., octants at 45-degree intervals) –
  more generally, for any angle set satisfying first- and
  second-harmonic balance. For angle sets violating that balance
  (generic unequally spaced sets), it is the conventional Gurtman
  estimator, not a least-squares fit, and the reported fit is then no
  longer a bounded R-squared in `[0, 1]` (it can fall below 0).

- prefix:

  Optional. A string to append to the beginning of all of the SSM
  parameters' variable names (default = "").

- suffix:

  Optional. A string to append to the end of all of the SSM parameters'
  variable names (default = "").

- e_label:

  Optional. A string representing the variable name of the SSM elevation
  parameter (default = "Elev").

- x_label:

  Optional. A string representing the variable name of the SSM x-value
  parameter (default = "Xval").

- y_label:

  Optional. A string representing the variable name of the SSM y-value
  parameter (default = "Yval").

- a_label:

  Optional. A string representing the variable name of the SSM amplitude
  parameter (default = "Ampl").

- d_label:

  Optional. A string representing the variable name of the SSM
  displacement parameter (default = "Disp").

- f_label:

  Optional. A string representing the variable name of the SSM fit or
  R-squared value (default = "Fit"). This value is a bounded R-squared
  in `[0, 1]` when the closed form coincides with the least-squares fit
  (equally spaced or otherwise harmonic-balanced `angles`; see
  `angles`).

## Value

A data frame containing the SSM parameters calculated from `scores`. For
degenerate profiles the undefined parameters are returned as `NA` with a
warning: a flat profile (zero variance) has undefined displacement and
fit, and a profile with real variance but zero amplitude (i.e., no
first-harmonic component) has undefined displacement and a fit of 0.
Note that this applies only to amplitudes that are zero up to machine
precision; small real amplitudes are always estimated, and their
uncertainty is expressed through confidence intervals (see
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)).

## See also

Other ssm functions:
[`plot.circumplex_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/plot.circumplex_ci_accuracy.md),
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md),
[`ssm_analyze_long()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze_long.md),
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md),
[`ssm_draws()`](http://circumplex.jmgirard.com/dev/reference/ssm_draws.md),
[`ssm_parameters_id()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters_id.md),
[`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md),
[`ssm_sem()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem.md),
[`ssm_sem_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_parameters.md),
[`ssm_table()`](http://circumplex.jmgirard.com/dev/reference/ssm_table.md),
[`summary.circumplex_ssm_id()`](http://circumplex.jmgirard.com/dev/reference/summary.circumplex_ssm_id.md)

Other analysis functions:
[`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md),
[`cpm_simulate()`](http://circumplex.jmgirard.com/dev/reference/cpm_simulate.md),
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md),
[`ssm_analyze_long()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze_long.md),
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md),
[`ssm_draws()`](http://circumplex.jmgirard.com/dev/reference/ssm_draws.md),
[`ssm_parameters_id()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters_id.md),
[`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md),
[`ssm_sem()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem.md),
[`ssm_sem_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_parameters.md),
[`summary.circumplex_ssm_id()`](http://circumplex.jmgirard.com/dev/reference/summary.circumplex_ssm_id.md)

## Examples

``` r
# Manually enter octant scores
scores <- c(0.55, 0.58, 0.62, 0.76, 1.21, 1.21, 1.48, 0.90)
ssm_parameters(scores)
#>      Elev      Xval       Yval      Ampl     Disp       Fit
#> 1 0.91375 0.3511181 -0.2516206 0.4319685 324.3736 0.8781155

# Customize several of the labels
ssm_parameters(scores, x_label = "LOV", y_label = "DOM")
#>      Elev       LOV        DOM      Ampl     Disp       Fit
#> 1 0.91375 0.3511181 -0.2516206 0.4319685 324.3736 0.8781155

# Add a prefix to all labels
ssm_parameters(scores, prefix = "IIP_")
#>   IIP_Elev  IIP_Xval   IIP_Yval  IIP_Ampl IIP_Disp   IIP_Fit
#> 1  0.91375 0.3511181 -0.2516206 0.4319685 324.3736 0.8781155
```
