# Axes Reliability

``` r

library(circumplex)
```

## 1. What is axis reliability?

A circumplex instrument places its scales around a circle and summarizes
a person (or a profile of correlations) by their position on two
orthogonal **axes** — here communion (the X axis, at 0°) and agency (the
Y axis, at 90°). Because those axis scores drive everything downstream —
a person’s projected location, the displacement and amplitude of a
Structural Summary Method profile — it is worth asking how *reliably*
the instrument measures each axis.

[`axes_reliability()`](http://circumplex.jmgirard.com/reference/axes_reliability.md)
answers that question with the estimator of Strack, Jacobs, and Grosse
Holtforth (2013). It fits an item-level measurement model that
decomposes each item’s variance into four pieces — a general factor
common to all items, the two circumplex **axes**, a scale-specificity
component, and item error — and reads axis reliability off the **axes**
component alone. Reliability is then the Spearman–Brown “list-length”
reliability of a composite of that length built from items that share
only their axes variance.

This is a different question from the other reliability-adjacent tools
in the package.
[`ssm_sem()`](http://circumplex.jmgirard.com/reference/ssm_sem.md)
disattenuates a *scale-level* SSM profile for measurement error;
[`fit_structure()`](http://circumplex.jmgirard.com/reference/fit_structure.md)
evaluates whether a correlation matrix has circumplex *structure* at
all.
[`axes_reliability()`](http://circumplex.jmgirard.com/reference/axes_reliability.md)
instead reports a single, interpretable number per axis: how well the
instrument measures communion and agency.

## 2. A worked example

The package ships `simulated_items`, a synthetic dataset of 1–7 Likert
responses from 500 respondents on 32 items — four items on each of the
eight octant scales, in the order that
[`octants()`](http://circumplex.jmgirard.com/reference/octants.md)
returns. The items were drawn from a five-component population (a
general factor, two equal axes with axes variance .18, a shared
scale-specificity component of .10, and free item error) that implies an
axis reliability of about .78.

[`axes_reliability()`](http://circumplex.jmgirard.com/reference/axes_reliability.md)
needs three things: the data, a map from items to scales, and the
scales’ angles. The map is a list with one character vector of item
names per scale, in the same angle order as the angles you pass:

``` r

data("simulated_items")

# Four items per octant scale, in octants() order (PA, BC, DE, ..., NO).
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
```

If your items belong to one of the package’s built-in instruments, you
can pass the `instrument` object instead of `items` and `angles` — it
supplies both the scale angles and the item membership, exactly as
[`score()`](http://circumplex.jmgirard.com/reference/score.md) does.
(The example above uses the explicit map because `simulated_items` is
not a registered instrument.)

The header confirms how many complete cases were used, and the per-axis
table reports, for each axis, the effective test length (`item_n`), the
Strack axis `Reliability`, its standard error of measurement (`SEm`),
and the Nunnally–Bernstein reliability (`NB_Reliability`) for
comparison. For a balanced octant instrument the two axes share one
axes-variance estimate and carry equal `item_n`, so they report the same
reliability — expected, not an error.

The recovered reliability (about .77) lands close to the .78 built into
the simulated population, and the axes-variance estimate (below)
recovers the population value of .18.

## 3. Reading the components

[`summary()`](https://rdrr.io/r/base/summary.html) adds the estimated
variance components and the model’s global fit:

``` r

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

The **variance components** show the decomposition the reliability rests
on: the `axes` component is the only one that feeds reliability, while
`general`, `scale_specificity`, and item error are isolated from it.
This is precisely why the Nunnally–Bernstein figure printed alongside
runs **higher** than the Strack reliability: N–B charges
scale-specificity variance to the axis rather than isolating it, so it
**overestimates** axis reliability whenever scale specificity is
non-trivial (Strack et al., 2013, Figure 3). The gap between the two
numbers is a direct read-out of how much scale-specific variance the
simpler formula would have miscredited to the axes.

## 4. Caveats to keep in mind

Three properties of the method shape how its output should be read.

**The standard errors and global fit are approximate.** Following the
paper’s own practice, the model is fit to the item **correlation**
matrix as though it were a covariance matrix. The component point
estimates and the reliabilities are correct, but the component standard
errors and the global chi-square are only approximate (Cudeck, 1989).
Read the fit indices as a rough guide, not as an exact test.

**Missing data are handled by listwise deletion only.** The header
reports the complete-case count so you can see how many rows
contributed; pairwise correlation input is never used. If listwise
deletion discards a large share of your sample, address the missingness
before interpreting the estimate.

**A boundary fit returns `NA`, not a clipped value.** If the model
estimates a non-positive axes variance (or any negative variance
component), the reliability and SEm are reported as `NA` with a warning
and a boundary flag, rather than a clipped or negative number. An `NA`
here is a signal that the model did not identify a usable axes-variance
component in your data — not a defect to be worked around.

Finally, a note on the `SEm`. The standard error of measurement supports
a location interval for a single profile (Strack et al., 2013, use
±1.65·SEm). By default
[`axes_reliability()`](http://circumplex.jmgirard.com/reference/axes_reliability.md)
reports the z-standardized SEm, `sqrt(1 - reliability)`; pass
`sd = "raw"` (or your own axis SDs) to put the SEm on the raw axis-score
scale. Such an interval describes the measurement imprecision of one
profile’s axis position; it is not a significance test of that position
against any particular value.

## 5. Wrap-up

[`axes_reliability()`](http://circumplex.jmgirard.com/reference/axes_reliability.md)
gives a compact, per-axis answer to “how reliably does this instrument
measure communion and agency?”, isolating the axes variance from the
general and scale-specific components that a simpler reliability formula
would conflate. Use it to characterize an octant circumplex instrument
before leaning on its axis scores, and read its output with the
correlation-as-covariance, listwise-deletion, and boundary caveats in
mind.

## References

- Cudeck, R. (1989). Analysis of correlation matrices using covariance
  structure models. *Psychological Bulletin, 105*(2), 317–327.

- Strack, S., Jacobs, K. A., & Grosse Holtforth, M. (2013). The
  reliability of circumplex axes. *SAGE Open, 3*(2).
  <https://doi.org/10.1177/2158244013486115>
