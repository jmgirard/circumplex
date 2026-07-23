# Reliability of the circumplex axes (Strack, Jacobs & Grosse Holtforth, 2013)

Estimate the reliability (and standard error of measurement) of the two
circumplex axes of an octant instrument with the item-level restricted
tau-equivalent CFA of Strack, Jacobs, and Grosse Holtforth (2013). The
model decomposes each item's variance into orthogonal components – a
general factor, the two circumplex axes, scale specificity, and item
specificity – and reads the axes' reliability off the isolated
axes-variance component with the Spearman-Brown formula. It is a
confirmatory, item-level complement to
[`fit_structure()`](http://circumplex.jmgirard.com/reference/fit_structure.md)'s
exploratory scale-level criteria.

## Usage

``` r
axes_reliability(data, items, angles = NULL, instrument = NULL, sd = "std")
```

## Arguments

- data:

  A data frame (or matrix) containing the circumplex items.

- items:

  Item selection. With `instrument`, a character vector of column names
  (or numeric indices) giving **all** items in item-number order, as in
  [`score()`](http://circumplex.jmgirard.com/reference/score.md).
  Without `instrument`, a list with one element per scale, each a
  character vector (or numeric indices) of that scale's item columns.

- angles:

  A numeric vector of the scales' angles in degrees (one per scale),
  required for the explicit map and forbidden with `instrument` (which
  supplies its own). Use
  [`octants()`](http://circumplex.jmgirard.com/reference/octants.md).

- instrument:

  Optional. A `circumplex_instrument` object supplying the scale angles
  and item membership (`Scales$Angle`, `Scales$Items`).

- sd:

  The scale for the standard error of measurement: `"std"` (the default)
  reports the z-standardized SEm `sqrt(1 - reliability)`; `"raw"` uses
  each axis composite's observed raw SD; or a numeric vector (length 1,
  recycled, or length 2 for the X and Y axes) of axis SDs.

## Value

An object of class `circumplex_axes_reliability` with
[`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) methods: `results`
(one row per axis: the axes variance, item_n, reliability, SEm,
Nunnally-Bernstein reliability, and boundary flag), `components` (the
estimated variance components with SEs), `fit` (global fit indices), and
`details`.

## Details

The model is fit to the item **correlation** matrix (the items are
z-standardized) as a flat fixed-links CFA: every item loads on the two
axes with fixed cosine weights, on a general factor with weight one, and
on its scale's specificity factor with weight one; the two axis
variances are held equal (the circumplex "no preferred rotation" axiom)
and every scale-specificity variance shares one value, while item errors
stay free (tau-equivalent). Only the axes-variance component feeds
reliability.

The Nunnally-Bernstein axis reliability (`nb_reliability`) is reported
alongside for comparison: it **overestimates** axis reliability when
scale specificity is large, because it charges scale-specificity
variance to the axis rather than isolating it (Strack et al. 2013,
Figure 3).

Because the model is fit to the item **correlation** matrix as if it
were a covariance matrix (the paper's own practice), the component point
estimates and the reliabilities are correct, but the component standard
errors and the global chi-square are **approximate** (Cudeck, 1989).
Results are reported **per axis** (X and Y): for a balanced octant
instrument the two axes carry the same axes-variance estimate and differ
only through `item_n`.

Missing data are handled by **listwise deletion only** (a message
reports the complete-case count); pairwise correlation input is never
used. A boundary fit (a non-positive estimated axes variance, or any
negative estimated variance) returns `NA` reliability and SEm with a
warning and a boundary flag rather than a clipped or negative value.

## References

Strack, S., Jacobs, K. A., & Grosse Holtforth, M. (2013). The
reliability of circumplex axes. *SAGE Open*, 3(2).
[doi:10.1177/2158244013486115](https://doi.org/10.1177/2158244013486115)

Cudeck, R. (1989). Analysis of correlation matrices using covariance
structure models. *Psychological Bulletin*, 105(2), 317-327.

## See also

[`fit_structure()`](http://circumplex.jmgirard.com/reference/fit_structure.md)
for exploratory circumplex-structure criteria.

## Examples

``` r
# A simulated 32-item octant dataset (four items per octant scale).
data("simulated_items")

# Map the item columns to their eight scales (four items each), in the
# octants() angle order, then estimate the axes reliability.
items <- split(names(simulated_items), rep(1:8, each = 4))
res <- axes_reliability(simulated_items, items = items, angles = octants())
#> axes_reliability(): 500 complete case(s) used.
res
#> 
#> Circumplex Axes Reliability (Strack, Jacobs & Grosse Holtforth, 2013)
#> Items:        32 (8 scales)
#> Complete N:   500
#> SEm scale:    std
#> 
#> # Per-axis reliability
#> 
#>  Axis item_n Reliability SEm   NB_Reliability
#>  X    16     0.773       0.476 0.822         
#>  Y    16     0.773       0.476 0.823         
#> 
#>   Note: the two axes share one axes-variance estimate and, with equal
#>   items per axis, carry the same reliability -- expected, not an error.
#> 
#>   Note: the model is fit to the item correlation matrix, so the point
#>   estimates are exact but the standard errors and global fit are
#>   approximate (Cudeck, 1989).
summary(res)
#> 
#> Circumplex Axes Reliability (Strack, Jacobs & Grosse Holtforth, 2013)
#> Items:        32 (8 scales)
#> Complete N:   500
#> SEm scale:    std
#> 
#> # Per-axis reliability
#> 
#>  Axis item_n Reliability SEm   NB_Reliability
#>  X    16     0.773       0.476 0.822         
#>  Y    16     0.773       0.476 0.823         
#> 
#>   Note: the two axes share one axes-variance estimate and, with equal
#>   items per axis, carry the same reliability -- expected, not an error.
#> 
#>   Note: the model is fit to the item correlation matrix, so the point
#>   estimates are exact but the standard errors and global fit are
#>   approximate (Cudeck, 1989).
#> 
#> # Variance components
#> 
#>  Component         Estimate SE   
#>  general           0.051    0.005
#>  axes              0.175    0.011
#>  scale_specificity 0.093    0.008
#>  item              0.680    --   
#> 
#> # Global fit
#> 
#>   chi-square(493) = 479.19,  RMSEA = 0.000,  CFI = 1.000
```
