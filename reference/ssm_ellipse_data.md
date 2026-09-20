# Centre and covariance of the Cartesian SSM coordinates for an ellipse layer

Compute the five columns
[`geom_ssm_ellipse()`](http://circumplex.jmgirard.com/reference/geom_ssm_ellipse.md)
draws from: the centre `(x0, y0)` of a profile's Cartesian coordinates
and the elements `var_x`, `var_y`, `cov_xy` of their 2 by 2 covariance
matrix. This is an S3 generic; the method for
[`ssm_draws()`](http://circumplex.jmgirard.com/reference/ssm_draws.md)
objects takes the centre from the object's point summaries
(`results$x_est`, `results$y_est`, the posterior medians of `x` and `y`)
and the covariance from
[`stats::cov()`](https://rdrr.io/r/stats/cor.html) of the `x` and `y`
columns of the object's draws. The point
[`geom_ssm_point()`](http://circumplex.jmgirard.com/reference/geom_ssm_point.md)
draws from the same object is `a_est` at `d_est`, which need not
coincide with those medians. Objects that retain no draws, such as the
result of
[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md),
have no method and are refused.

## Usage

``` r
ssm_ellipse_data(x, ...)

# S3 method for class 'circumplex_ssm_draws'
ssm_ellipse_data(x, ...)

# Default S3 method
ssm_ellipse_data(x, ...)
```

## Arguments

- x:

  An object of class `"circumplex_ssm_draws"`.

- ...:

  Passed to methods; unused by the shipped method.

## Value

A one-row data frame with columns `x0`, `y0`, `var_x`, `var_y`, and
`cov_xy`, in the score metric of the coordinates.

## See also

[`geom_ssm_ellipse()`](http://circumplex.jmgirard.com/reference/geom_ssm_ellipse.md),
which draws the ellipse, and
[`ssm_draws()`](http://circumplex.jmgirard.com/reference/ssm_draws.md),
which produces the input.

## Examples

``` r
set.seed(1)
draws <- cbind(rnorm(500, 0.4, 0.1), rnorm(500, 0.9, 0.1),
               rnorm(500, -0.3, 0.1))
ssm_ellipse_data(ssm_draws(draws, type = "parameters"))
#>          x0         y0      var_x      var_y        cov_xy
#> 1 0.8978743 -0.3053591 0.01117892 0.01019377 -0.0008221293
```
