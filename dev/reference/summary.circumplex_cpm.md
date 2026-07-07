# Summarize a circular process model fit

Fuller display of a
[`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md)
object: adds the correlation-function weights, the full set of fit
indices, a residual summary (the largest absolute residual and the pair
it belongs to), and all boundary/identification diagnostics in plain
language. When the confidence intervals are analytic, prints a coverage
caution calibrated by simulation: unconditionally when the sample size
is modest (N \< 2000, where Wald intervals mis-covered for every
configuration studied), and up to N = 50000 when the fitted solution
shows a boundary or weak-identification marker (Heywood communality,
removed harmonic, small correlation-function weight, ill-conditioning,
or competing near-tied optima), the regime where they mis-covered even
at large N (see
[`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md)).

## Usage

``` r
# S3 method for class 'circumplex_cpm'
summary(object, digits = 3, ...)
```

## Arguments

- object:

  A `circumplex_cpm` object.

- digits:

  The number of decimal places to display (default = 3).

- ...:

  Not used.

## Value

`object`, invisibly.
