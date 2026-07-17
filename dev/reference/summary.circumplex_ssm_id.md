# Summarize per-person SSM parameters at the group level

Aggregate a per-person SSM parameter table (from
[`ssm_parameters_id()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters_id.md))
into group-level summaries, using circular statistics for displacement:
arithmetic means are meaningless for angles, so displacement is
summarized by its circular mean (the direction of the summed unit
vectors) and the mean resultant length (a 0 to 1 measure of directional
concentration).

## Usage

``` r
# S3 method for class 'circumplex_ssm_id'
summary(object, ...)
```

## Arguments

- object:

  Required. An object of class `"circumplex_ssm_id"` created by
  [`ssm_parameters_id()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters_id.md).

- ...:

  Ignored (S3 consistency).

## Value

A one-row data frame with columns `n` (persons), `n_na_d` (persons with
undefined displacement, excluded from the circular summaries), `e_mean`,
`x_mean`, `y_mean`, `a_mean` (arithmetic means), `d_mean` (circular mean
of displacement, degrees in \[0, 360\], the 0/360 pole reported as 360),
and `d_res` (mean resultant length in \[0, 1\]; `NA` when no
displacement is defined, and undefined direction at zero resultant
reports `d_mean = NA`).

## Details

Persons with undefined (`NA`) displacement are stripped before the
circular aggregation – `n_na_d` reports how many – while the arithmetic
means of the other parameters use all persons with defined values. Two
aggregation caveats apply. (1) The circular mean of per-person
displacements weights every person's direction equally; it is a
*different quantity* from the displacement of the group mean profile
(e.g., from
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)),
which weights persons by amplitude – on heterogeneous samples the two
can differ substantially. (2) By the triangle inequality, the amplitude
of the group mean profile is at most the mean per-person amplitude
(`a_mean`), strictly smaller when directions disperse; relatedly, the
mean resultant length `d_res` falls below 1 as directions disperse.

## See also

Other ssm functions:
[`plot.circumplex_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/plot.circumplex_ci_accuracy.md),
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md),
[`ssm_analyze_long()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze_long.md),
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md),
[`ssm_draws()`](http://circumplex.jmgirard.com/dev/reference/ssm_draws.md),
[`ssm_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters.md),
[`ssm_parameters_id()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters_id.md),
[`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md),
[`ssm_sem()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem.md),
[`ssm_sem_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_parameters.md),
[`ssm_table()`](http://circumplex.jmgirard.com/dev/reference/ssm_table.md)

Other analysis functions:
[`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md),
[`cpm_simulate()`](http://circumplex.jmgirard.com/dev/reference/cpm_simulate.md),
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md),
[`ssm_analyze_long()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze_long.md),
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md),
[`ssm_draws()`](http://circumplex.jmgirard.com/dev/reference/ssm_draws.md),
[`ssm_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters.md),
[`ssm_parameters_id()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters_id.md),
[`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md),
[`ssm_sem()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem.md),
[`ssm_sem_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_parameters.md)

## Examples

``` r
data("aw2009")
res <- ssm_parameters_id(
  aw2009,
  scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
)
summary(res)
#>   n n_na_d e_mean    x_mean     y_mean   a_mean   d_mean     d_res
#> 1 5      0  0.423 0.9445214 -0.2644691 1.164276 344.4394 0.8429474
```
