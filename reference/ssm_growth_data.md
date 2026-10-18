# Build the long table for a growth model on SSM coordinates

Score every row of `data` as its own circumplex profile and stack the
three SSM coordinates, elevation `e`, `x` and `y`, into the long table
that the package's growth recipe fits. One input row gives three output
rows, one per coordinate, and the output holds each row's id and time
value beside the coordinate's name and value.
[`ssm_growth_formula()`](http://circumplex.jmgirard.com/reference/ssm_growth_formula.md)
gives the fit call that takes this table.

## Usage

``` r
ssm_growth_data(data, scales, angles = octants(), id, time)
```

## Arguments

- data:

  Required. A data frame or matrix with one row per person per time
  point, containing the circumplex scales, an id column and a time
  column. Two rows with the same id and the same time are an error. Time
  values are compared exactly, with no rounding. Ids are compared after
  conversion to a factor, which matches them by
  [`as.character()`](https://rdrr.io/r/base/character.html) (15
  significant digits), so two numeric ids that differ only beyond that
  precision count as one person. A column name that occurs more than
  once is an error when `id`, `time` or a name in `scales` refers to it.

- scales:

  Required. The variable names or column numbers for the variables in
  `data` that contain circumplex scales. They may not select the `id` or
  `time` column.

- angles:

  Optional. A numeric vector containing the angular displacement of each
  circumplex scale included in `scales`, in degrees (default =
  [`octants()`](http://circumplex.jmgirard.com/reference/octants.md)).

- id:

  Required. The name of the column in `data` identifying persons. A
  column name, not a number. Missing values are an error.

- time:

  Required. The name of the numeric column in `data` holding each row's
  time point. A column name, not a number. A column that is not numeric
  (a `Date`, factor or character column, among others) is an error, as
  are missing and infinite values: the growth model fits time as a
  number, so the caller chooses its origin and unit. `id` and `time`
  must differ from each other and from `dv` and `value`, the two column
  names the output reserves.

## Value

A data frame with `3 * nrow(data)` rows and four columns, named `<id>`
(a factor), `<time>` (numeric), `dv` (a factor with levels `e`, `x` and
`y`) and `value`. Rows are ordered by input row, then by `dv`. Each
`value` is the `Elev`, `Xval` or `Yval` that
[`ssm_parameters_id()`](http://circumplex.jmgirard.com/reference/ssm_parameters_id.md)
with `id = NULL` gives that input row.

## Details

Growth in displacement is modeled through `x` and `y`, never through the
angle itself, so the table carries no amplitude or displacement. A flat
profile has `x` and `y` at zero to floating-point precision and a
defined `e`, and a profile with a scale entirely missing has `NA` on all
three coordinates. Both keep their rows. Each engine's fit call then
drops a row whose `value` is `NA`: glmmTMB and brms do so by default,
and the nlme call that
[`ssm_growth_formula()`](http://circumplex.jmgirard.com/reference/ssm_growth_formula.md)
prints sets `na.action = na.omit` for the same effect.

## See also

Other growth functions:
[`ssm_growth_formula()`](http://circumplex.jmgirard.com/reference/ssm_growth_formula.md),
[`ssm_trajectory()`](http://circumplex.jmgirard.com/reference/ssm_trajectory.md)

## Examples

``` r
data("simulated_growth")
long <- ssm_growth_data(
  simulated_growth,
  scales = PANO(),
  id = "person",
  time = "wave"
)
head(long, 6)
#>   person wave dv      value
#> 1      1    0  e  0.4595363
#> 2      1    0  x  0.5697631
#> 3      1    0  y -0.1920113
#> 4      2    0  e  0.8644617
#> 5      2    0  x  0.9896316
#> 6      2    0  y  0.2426116
```
