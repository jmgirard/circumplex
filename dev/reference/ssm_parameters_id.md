# Calculate SSM parameters for each person

Score each person's own circumplex profile through the closed-form SSM
transform and return a per-person parameter table. When `id` is `NULL`,
every row of `data` is treated as one person's profile (like
[`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md),
but returning a fresh table rather than appending columns). When `id`
names a column, rows sharing an id (e.g., occasions of intensive
longitudinal data) are first averaged within person – each scale's mean
uses that person's available (non-missing) rows – and the within-person
mean profile is scored.

## Usage

``` r
ssm_parameters_id(data, scales, angles = octants(), id = NULL)
```

## Arguments

- data:

  Required. A data frame or matrix containing at least circumplex
  scales, with one row per person or (with `id`) per person-occasion.

- scales:

  Required. The variable names or column numbers for the variables in
  `data` that contain circumplex scales to be analyzed.

- angles:

  Optional. A numeric vector containing the angular displacement of each
  circumplex scale included in `scales`, in degrees (default =
  [`octants()`](http://circumplex.jmgirard.com/dev/reference/octants.md)).
  The closed-form SSM estimator used here equals the
  ordinary-least-squares cosine fit for equally spaced `angles` – more
  generally, for any angle set satisfying first- and second-harmonic
  balance; see
  [`ssm_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters.md).

- id:

  Optional. A single variable name or column number identifying persons.
  If `NULL` (default), each row is scored as its own person; otherwise
  rows sharing an id are averaged within person before scoring. Missing
  id values are an error (a person cannot be silently dropped).

## Value

A data frame of class `"circumplex_ssm_id"` with one row per person, in
order of first appearance: the id column (named after `id`, or `id` when
`NULL`), `n_obs` (rows contributing to that person), `na_rate`
(proportion of missing scale cells among those rows), and the SSM
parameters `Elev`, `Xval`, `Yval`, `Ampl`, `Disp` (degrees in \[0,
360\], with the 0/360 pole reported as 360 per the package's LM = 360
convention), and `Fit`. Use
[`summary.circumplex_ssm_id()`](http://circumplex.jmgirard.com/dev/reference/summary.circumplex_ssm_id.md)
for group-level summaries with circular statistics for displacement.

## Details

Degenerate profiles keep their row and are reported as `NA`, never
silently dropped: a flat (zero-variance) profile has undefined
displacement and fit, a profile with real variance but zero
first-harmonic amplitude has undefined displacement and a fit of 0, and
a person with a completely missing scale has an undefined profile (all
parameters `NA`). The `na_rate` column exposes each person's share of
missing scale cells so missingness is visible alongside its
consequences.

## See also

Other ssm functions:
[`plot.circumplex_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/plot.circumplex_ci_accuracy.md),
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md),
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md),
[`ssm_draws()`](http://circumplex.jmgirard.com/dev/reference/ssm_draws.md),
[`ssm_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters.md),
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
[`ssm_draws()`](http://circumplex.jmgirard.com/dev/reference/ssm_draws.md),
[`ssm_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters.md),
[`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md),
[`ssm_sem()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem.md),
[`ssm_sem_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_parameters.md),
[`summary.circumplex_ssm_id()`](http://circumplex.jmgirard.com/dev/reference/summary.circumplex_ssm_id.md)

## Examples

``` r
data("aw2009")
ssm_parameters_id(
  aw2009,
  scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
)
#>   id n_obs na_rate    Elev      Xval       Yval      Ampl      Disp       Fit
#> 1  1     1       0  0.4325 1.2514177 -1.3091258 1.8110374 313.70892 0.9706769
#> 2  2     1       0  0.2325 1.4193555  0.5073528 1.5073079  19.66958 0.9172710
#> 3  3     1       0 -0.0700 0.7822361  0.4476346 0.9012602  29.78035 0.7137698
#> 4  4     1       0  0.6250 0.8313567 -0.5560749 1.0001866 326.22237 0.8784837
#> 5  5     1       0  0.8950 0.4382412 -0.4121320 0.6015880 316.75861 0.9674101
```
