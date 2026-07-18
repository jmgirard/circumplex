# Angle-labeled x-axis scale for circumplex plots

A ggplot2 continuous position scale for the angle axis of a linear
circumplex plot, such as the score-by-angle curve drawn by
[`ssm_plot_curve()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_curve.md).
It places axis breaks at the circumplex scale angles and labels them, by
default, with their angular position in degrees. Custom text labels or a
`circumplex_instrument` can be supplied instead, using the same
conventions as
[`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md),
so the linear axis and the circular canvas label their scales
consistently.

## Usage

``` r
scale_x_circumplex(angles = octants(), labels = NULL, instrument = NULL, ...)
```

## Arguments

- angles:

  Optional. A numeric vector of the angular position (in degrees) of
  each circumplex scale (default =
  [`octants()`](http://circumplex.jmgirard.com/dev/reference/octants.md)).
  Ignored if `instrument` is supplied.

- labels:

  Optional. Either `NULL` or a character vector of axis labels, one per
  angle and in the same order (default = `NULL`, which labels each break
  with its angle in degrees, or with the instrument's scale
  abbreviations when `instrument` is supplied).

- instrument:

  Optional. Either `NULL` or a `circumplex_instrument` object (see
  `instrument()`) from which to take the scale angles and (unless
  `labels` is given) abbreviations (default = `NULL`).

- ...:

  Additional arguments passed to
  [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html),
  such as `name` or `limits`.

## Value

A ggplot2 scale object that can be added to a plot with `+`.

## See also

Other circumplex layers:
[`coord_circumplex()`](http://circumplex.jmgirard.com/dev/reference/coord_circumplex.md),
[`geom_ssm_arc()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_arc.md),
[`geom_ssm_point()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_point.md),
[`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md),
[`theme_circumplex()`](http://circumplex.jmgirard.com/dev/reference/theme_circumplex.md)

## Examples

``` r
# Degree-labeled angle axis
scale_x_circumplex(octants())
#> <ScaleContinuousPosition>
#>  Range:  
#>  Limits:    0 --    1

# Label the axis with an instrument's scale abbreviations
scale_x_circumplex(instrument = csip)
#> <ScaleContinuousPosition>
#>  Range:  
#>  Limits:    0 --    1
```
