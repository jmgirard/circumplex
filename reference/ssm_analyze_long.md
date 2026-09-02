# Perform SSM analyses on long-format repeated-measures data

A convenience wrapper around the `occasions` interface of
[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md)
for data stored in long format (one row per person per occasion). It
reshapes the data into the wide, one-row-per-person layout that
[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md)
consumes and then delegates to it; all estimation is performed by
[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md)
unchanged. See the `occasions` argument of
[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md)
for the analysis semantics – per-occasion profiles, paired within-person
contrasts, and the listwise-only handling of missing waves (a person
missing any occasion is dropped from all occasions).

## Usage

``` r
ssm_analyze_long(
  data,
  scales,
  angles = octants(),
  id,
  occasion,
  grouping = NULL,
  contrast = FALSE,
  boots = 2000,
  interval = 0.95,
  parallel = "no",
  ncpus = 1,
  method = "bootstrap"
)
```

## Arguments

- data:

  Required. A data frame (or matrix) in long format containing an
  identifier column, an occasion column, and the circumplex scale scores
  (one set of score columns, repeated across occasions in different
  rows).

- scales:

  Required. A character vector of column names, or a numeric vector of
  column indexes, giving the circumplex scale scores in `data` (the same
  scales measured at every occasion).

- angles:

  Optional. A numeric vector containing the angular displacement of each
  circumplex scale included in `scales` (in degrees) (default =
  [`octants()`](http://circumplex.jmgirard.com/reference/octants.md)).

- id:

  Required. A single column name or index identifying the person that
  each row belongs to.

- occasion:

  Required. A single column name or index identifying the occasion
  (wave) that each row belongs to. Occasion order – which governs the
  second-minus-first direction of a paired contrast – is taken from the
  factor levels of this column when it is a factor, and otherwise from
  the order in which the occasions first appear in `data`. It is never
  sorted alphabetically, so a `T10`/`T2` pair keeps its supplied order.

- grouping:

  Optional. A single column name or index giving a time-invariant
  grouping variable (one value per person; an error is raised if a
  person's grouping value varies across occasions).

- contrast:

  Optional. A logical value; if `TRUE` (and the data contain exactly two
  occasions in a single group), the paired within-person contrast
  (second occasion minus first) is calculated (default = FALSE).

- boots, interval, parallel, ncpus, method:

  Optional. Passed through to
  [`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md);
  see its documentation.

## Value

A list containing the results and description of the analysis, as
returned by
[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md)
(with an `Occasion` column). See
[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md).

## See also

[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md)
for the wide-format interface and the analysis semantics this wrapper
delegates to.

Other ssm functions:
[`plot.circumplex_ci_accuracy()`](http://circumplex.jmgirard.com/reference/plot.circumplex_ci_accuracy.md),
[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md),
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/reference/ssm_ci_accuracy.md),
[`ssm_draws()`](http://circumplex.jmgirard.com/reference/ssm_draws.md),
[`ssm_parameters()`](http://circumplex.jmgirard.com/reference/ssm_parameters.md),
[`ssm_parameters_id()`](http://circumplex.jmgirard.com/reference/ssm_parameters_id.md),
[`ssm_score()`](http://circumplex.jmgirard.com/reference/ssm_score.md),
[`ssm_sem()`](http://circumplex.jmgirard.com/reference/ssm_sem.md),
[`ssm_sem_parameters()`](http://circumplex.jmgirard.com/reference/ssm_sem_parameters.md),
[`ssm_table()`](http://circumplex.jmgirard.com/reference/ssm_table.md),
[`summary.circumplex_ssm_id()`](http://circumplex.jmgirard.com/reference/summary.circumplex_ssm_id.md)

Other analysis functions:
[`cpm_fit()`](http://circumplex.jmgirard.com/reference/cpm_fit.md),
[`cpm_simulate()`](http://circumplex.jmgirard.com/reference/cpm_simulate.md),
[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md),
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/reference/ssm_ci_accuracy.md),
[`ssm_draws()`](http://circumplex.jmgirard.com/reference/ssm_draws.md),
[`ssm_parameters()`](http://circumplex.jmgirard.com/reference/ssm_parameters.md),
[`ssm_parameters_id()`](http://circumplex.jmgirard.com/reference/ssm_parameters_id.md),
[`ssm_score()`](http://circumplex.jmgirard.com/reference/ssm_score.md),
[`ssm_sem()`](http://circumplex.jmgirard.com/reference/ssm_sem.md),
[`ssm_sem_parameters()`](http://circumplex.jmgirard.com/reference/ssm_sem_parameters.md),
[`summary.circumplex_ssm_id()`](http://circumplex.jmgirard.com/reference/summary.circumplex_ssm_id.md)

## Examples

``` r
# `boots` is lowered from its default of 2000 throughout these examples so
# they run quickly; a reported analysis should use the default.

# Build a small two-occasion dataset in long format (one row per person per
# occasion). In practice `data` already stores the repeated occasions this
# way; here we stack two copies of jz2017 as an illustration.
data("jz2017")
scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
t1 <- jz2017[, scales]
t1$id <- seq_len(nrow(t1))
t1$occasion <- "T1"
t2 <- t1
t2$occasion <- "T2"
long <- rbind(t1, t2)

# \donttest{
# Per-occasion SSM profiles from long-format data
ssm_analyze_long(
  long, scales = scales, id = "id", occasion = "occasion",
  boots = 200
)
#> 
#> # Profile [T1]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.917      0.891      0.944
#> X-Value           0.351      0.321      0.374
#> Y-Value          -0.252     -0.277     -0.220
#> Amplitude         0.432      0.397      0.459
#> Displacement    324.292    321.175    327.287
#> Model Fit         0.878                      
#> 
#> 
#> # Profile [T2]:
#> 
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.917      0.891      0.944
#> X-Value           0.351      0.321      0.374
#> Y-Value          -0.252     -0.277     -0.220
#> Amplitude         0.432      0.397      0.459
#> Displacement    324.292    321.175    327.287
#> Model Fit         0.878                      
#> 
# }
```
