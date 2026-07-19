# Print a circular process model fit

Compact display of a
[`cpm_fit()`](http://circumplex.jmgirard.com/reference/cpm_fit.md)
object: the estimated angles and communality indices with confidence
intervals, a one-line fit summary, and any boundary/convergence notes.

## Usage

``` r
# S3 method for class 'circumplex_cpm'
print(x, digits = 3, ...)
```

## Arguments

- x:

  A `circumplex_cpm` object.

- digits:

  The number of decimal places to display (default = 3).

- ...:

  Not used.

## Value

`x`, invisibly.
