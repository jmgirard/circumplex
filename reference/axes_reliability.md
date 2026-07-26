# Reliability of the circumplex axes (Strack, Jacobs & Grosse Holtforth, 2013)

Estimate the reliability (and standard error of measurement) of the two
circumplex axes of an instrument with the item-level restricted
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
axes_reliability(
  data = NULL,
  items,
  angles = NULL,
  instrument = NULL,
  cormat = NULL,
  n = NULL,
  sd = "std"
)
```

## Arguments

- data:

  A data frame (or matrix) containing the circumplex items. Supply
  exactly one of `data` or `cormat`.

- items:

  Item selection. With `instrument`, a character vector of column names
  (or numeric indices) giving **all** items in item-number order, as in
  [`score()`](http://circumplex.jmgirard.com/reference/score.md).
  Without `instrument`, a list with one element per scale, each a
  character vector (or numeric indices) of that scale's item columns.

- angles:

  A numeric vector of the scales' angles in degrees (one per scale),
  required for the explicit map and forbidden with `instrument` (which
  supplies its own). Must be equally spaced around the circle, at any
  rotation, with at least four scales;
  [`octants()`](http://circumplex.jmgirard.com/reference/octants.md)
  gives the canonical eight. Angles outside `[0, 360)` are reduced onto
  their circumplex positions, so 0 and 360 name the same position.

- instrument:

  Optional. A `circumplex_instrument` object supplying the scale angles
  and item membership (`Scales$Angle`, `Scales$Items`).

- cormat:

  An item correlation matrix (the matrix-input path), symmetric with a
  unit diagonal and positive definite, with dimnames naming the items.
  Supply exactly one of `data` or `cormat`.

- n:

  For the `cormat` path, the sample size (number of observations) the
  correlation matrix was computed from. Required with `cormat`, and not
  accepted with `data` (which carries its own).

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
estimated variance components with SEs – four rows, or three when scale
specificity was dropped), `fit` (global fit indices), and `details`
(including `zeta1_fitted`, whether scale specificity was in the model,
and `nb_reason`, why the Nunnally-Bernstein comparison is `NA`).

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
Figure 3). It needs each scale's coefficient alpha, which is undefined
for a scale carrying a single item, so it is reported as `NA` with a
stated reason whenever any scale has fewer than two items – as Strack et
al. themselves do, leaving it blank for such instruments.

Because the model is fit to the item **correlation** matrix as if it
were a covariance matrix (the paper's own practice), the component point
estimates and the reliabilities are correct, but the component standard
errors and the global chi-square are **approximate** (Cudeck, 1989).
Results are reported **per axis** (X and Y): for a balanced instrument
the two axes carry the same axes-variance estimate and differ only
through `item_n`.

## Which instruments this accepts

Any set of **equally spaced** scale angles, at any rotation: the
canonical octants, an interstitial set rotated 22.5 degrees off the
axes, or a non-octant count such as six or twelve scales. What matters
is equal spacing, not the count or the starting angle – for any equally
spaced set of `k` scales, each axis draws the same effective test length
(`k / 2` per item), which is what keeps the equal-axis-variance
restriction as innocuous as it is for octants.

Scales may carry **one item each**, as Strack et al.'s types e and f do.
With a single item at every position no two items share a scale, so the
scale-specificity component is not identified and is dropped from the
model rather than estimated: the components table then has three rows
instead of four, and `details$zeta1_fitted` is `FALSE`. A *mixed*
instrument still estimates it – one multi-item scale supplies the
information, and the shared-value restriction carries it to the rest.

Two limits. At least **four** scales are required: with three, every
pair of scales sits the same angular distance apart, and the general,
axes, and scale-specificity variances are then not separately
identified. And spacing must be equal, not merely close – a
quasi-circumplex is refused rather than approximated, since Strack et
al. (2013) excluded such instruments from the model's validation. Every
scale needs at least one item.

The model is two-dimensional. Instruments whose items span three
dimensions – spherical designs such as SYMLOG (Strack et al.'s type f) –
are out of scope, even though Strack et al. (2013) analyze one; their
Table 3 SYMLOG rows arise from a three-axis sphere model, not from any
configuration this function accepts.

Missing data are handled by **listwise deletion only** (a message
reports the complete-case count); pairwise correlation input is never
used. A boundary fit (a non-positive estimated axes variance, or any
negative estimated variance) returns `NA` reliability and SEm with a
warning and a boundary flag rather than a clipped or negative value.

## Supplying a correlation matrix instead of raw data

Give `cormat` and `n` in place of `data` to estimate from an item
correlation matrix that someone else published, with no raw data in
hand. The matrix must be symmetric, positive definite, and carry a unit
diagonal (the model assumes unit-variance items); `items` selects and
orders its rows by name, so the matrix's own column order does not
matter. Estimates are identical to those the raw-data path would give
for the same matrix.

Two results are unavailable on this path, because both need the
respondents' own item scores rather than their correlations: the
Nunnally-Bernstein comparison is reported as `NA` (it needs each scale's
alpha and the axis composite's variance), and `sd = "raw"` is refused
(there are no scale scores to take an observed SD from). Supply the axis
SDs numerically if you want SEm on a raw scale.

## Blockwise instruments

Some circumplex instruments are administered in **blocks** (items
grouped by something other than their scale), which contributes a
block-specificity variance component of its own. This model has no such
component, and the package's instrument objects carry no block
structure, so a blockwise instrument analyzed here folds its block
variance into the general and scale-specificity components – inflating
them and, in turn, deflating the share attributed to the axes. Strack et
al. (2013, Table 3) report block-specificity as high as 6.7%, so treat
axes reliability from a blockwise instrument as approximate.

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
#> Input:        item data
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
#> Input:        item data
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

# The same estimates from the item correlation matrix alone, as when
# reanalyzing a matrix published without its raw data.
axes_reliability(
  cormat = cor(simulated_items), items = items, angles = octants(),
  n = nrow(simulated_items)
)
#> 
#> Circumplex Axes Reliability (Strack, Jacobs & Grosse Holtforth, 2013)
#> Input:        correlation matrix
#> Items:        32 (8 scales)
#> Sample N:     500
#> SEm scale:    std
#> 
#> # Per-axis reliability
#> 
#>  Axis item_n Reliability SEm   NB_Reliability
#>  X    16     0.773       0.476 --            
#>  Y    16     0.773       0.476 --            
#> 
#>   Note: the two axes share one axes-variance estimate and, with equal
#>   items per axis, carry the same reliability -- expected, not an error.
#> 
#>   Note: the Nunnally-Bernstein comparison needs the raw item scores
#>   (scale alphas and the axis-composite variance), so it is NA on the
#>   correlation-matrix path.
#> 
#>   Note: the model is fit to the item correlation matrix, so the point
#>   estimates are exact but the standard errors and global fit are
#>   approximate (Cudeck, 1989).
```
