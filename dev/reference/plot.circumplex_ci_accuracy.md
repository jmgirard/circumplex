# Plot SSM CI accuracy across the amplitude ladder

Draw the empirical coverage from an
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md)
run against its amplitude-ladder conditions: one panel per SSM parameter
(including displacement conditional on guardrail certification), one
line per profile row, with 95% Wilson score intervals as error bars,
Bradley's (1978) liberal robustness band shaded, and the nominal
confidence level as a dashed line. Amplitude rungs whose coverage is
structurally zero (a percentile interval of strictly positive amplitude
replicates cannot contain a zero truth; see
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md))
are drawn as open symbols. This is a Cartesian diagnostic plot, not a
circumplex figure.

## Usage

``` r
# S3 method for class 'circumplex_ci_accuracy'
plot(x, ...)
```

## Arguments

- x:

  A `circumplex_ci_accuracy` object from
  [`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md).

- ...:

  Currently ignored.

## Value

A ggplot2 object.

## See also

Other ssm functions:
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md),
[`ssm_analyze_long()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze_long.md),
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md),
[`ssm_draws()`](http://circumplex.jmgirard.com/dev/reference/ssm_draws.md),
[`ssm_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters.md),
[`ssm_parameters_id()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters_id.md),
[`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md),
[`ssm_sem()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem.md),
[`ssm_sem_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_parameters.md),
[`ssm_table()`](http://circumplex.jmgirard.com/dev/reference/ssm_table.md),
[`summary.circumplex_ssm_id()`](http://circumplex.jmgirard.com/dev/reference/summary.circumplex_ssm_id.md)

Other visualization functions:
[`ssm_plot_trajectory()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_trajectory.md)

## Examples

``` r
# \donttest{
data("jz2017")
set.seed(12345)
res <- ssm_analyze(
  jz2017[1:200, ],
  scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  boots = 100
)
set.seed(23456)
acc <- ssm_ci_accuracy(res, reps = 25)
#> Warning: CPM Hessian is ill-conditioned (condition number 3.53e+17): angles may be clustered or parameters weakly determined.
plot(acc)

# }
```
