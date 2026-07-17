# Summarize posterior draws as SSM parameters

Transform posterior draws from a user-fitted Bayesian model (e.g., a
brms cosine regression) into Structural Summary Method parameter draws
and summarize them with the package's circular-statistics machinery. Two
draw shapes are accepted, distinguished explicitly (never guessed):

## Usage

``` r
ssm_draws(draws, angles = NULL, interval = 0.95, type = NULL)
```

## Arguments

- draws:

  Required. A numeric matrix or data frame of posterior draws: one row
  per draw, columns per the shape rules above.

- angles:

  Optional. A numeric vector of angular displacements (in degrees) for
  profile draws, one per column of `draws`; `NULL` (default) for
  parameter draws.

- interval:

  Optional. A single number between 0 and 1 giving the credible level
  for the equal-tailed intervals (default = 0.95).

- type:

  Optional. `"parameters"` or `"profiles"`, required only where the
  shape is ambiguous (`angles = NULL` with exactly 3 columns); when
  given elsewhere it must not contradict `angles`.

## Value

An object of class `"circumplex_ssm_draws"` holding `draws` (the SSM
parameter draws, one row per posterior draw, columns e, x, y, a, d, fit,
displacement in degrees \[0, 360\], pole reported as 360), `results`
(the point summaries and credible bounds), and `details`. Printing shows
the summary table; [`summary()`](https://rdrr.io/r/base/summary.html)
adds the analysis details.

## Details

- **Parameter draws** (`angles = NULL`, `type = "parameters"`): a
  numeric matrix or data frame with exactly three columns interpreted
  **in column order** as (e, x, y) – elevation (intercept), the cosine
  coefficient (x), and the sine coefficient (y). Each row is mapped to
  amplitude `a = sqrt(x^2 + y^2)` and displacement `d = atan2(y, x)`
  wrapped to \[0, 360\] (the 0/360 pole is reported as 360, the
  package's LM = 360 convention). Column names are not used to reorder:
  when names are present but do not look (intercept, cos, sin)-like, a
  message states the assumed mapping. A row with exactly zero amplitude
  has undefined displacement (`NA`); model fit is undefined for
  parameter draws (`fit = NA`) because no profile is available to
  measure it against.

- **Profile draws** (`angles` supplied): a numeric matrix with one
  column per circumplex scale (`ncol(draws) == length(angles)`); each
  row goes through the closed-form SSM transform exactly as a bootstrap
  replicate would, inheriting the standing degenerate-profile NA
  semantics.

With `angles = NULL` and a column count other than 3 the input matches
neither shape and an error explains both. With `angles = NULL` and
exactly 3 columns the shape is ambiguous (a p = 3 instrument's profile
draws look like parameter draws), so `type = "parameters"` is required.

Point estimates are posterior medians for e, x, y, a, and fit (amplitude
is right-skewed, so a mean would be biased upward), and the circular
mean for displacement. Marginal summaries are not jointly coherent: the
reported a is not `sqrt(x^2 + y^2)` of the reported (x, y), and the
reported d is not their direction – each is the honest marginal summary
of its own posterior. Intervals are equal-tailed credible intervals
(percentile quantiles of the draws), with displacement handled by the
package's circular quantile machinery (centered on the circular mean, so
intervals straddling 0/360 wrap correctly). Draws with undefined
displacement are excluded from the displacement summaries only, which
are therefore conditional on estimability (measure-zero for continuous
parameter-draw posteriors; can bind for profile draws). A diffuse
posterior with zero circular resultant has an undefined circular mean,
reported as `NA` rather than invented.

Note that independent priors on (x, y) induce a non-uniform prior on (a,
d) – roughly Rayleigh-shaped on amplitude, with mass pushed away from a
= 0 – so the prior on the SSM scale should be inspected (e.g., by
prior-predictive simulation) rather than assumed flat; see the package's
Bayesian SSM vignette.

## See also

Other ssm functions:
[`plot.circumplex_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/plot.circumplex_ci_accuracy.md),
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md),
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md),
[`ssm_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters.md),
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
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md),
[`ssm_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters.md),
[`ssm_parameters_id()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters_id.md),
[`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md),
[`ssm_sem()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem.md),
[`ssm_sem_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_parameters.md),
[`summary.circumplex_ssm_id()`](http://circumplex.jmgirard.com/dev/reference/summary.circumplex_ssm_id.md)

## Examples

``` r
# Parameter draws (e.g., brms fixed-effect draws b_Intercept, b_cos, b_sin)
set.seed(1)
draws <- cbind(rnorm(500, 0.4, 0.1), rnorm(500, 0.9, 0.1),
               rnorm(500, -0.3, 0.1))
ssm_draws(draws, type = "parameters")
#> 
#> # Posterior Summary:
#> 
#>                Estimate   Lower CrI   Upper CrI
#> Elevation         0.396       0.200       0.603
#> X-Value           0.898       0.676       1.100
#> Y-Value          -0.305      -0.484      -0.095
#> Amplitude         0.947       0.731       1.157
#> Displacement    341.475     330.470     354.036
#> Model Fit                                      
#> 
```
