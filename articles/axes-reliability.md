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
decomposes each item’s variance into orthogonal pieces — a general
factor common to all items, the two circumplex **axes**, a
scale-specificity component, item error, and, for an instrument
administered in blocks, a block-specificity component — and reads axis
reliability off the **axes** component alone. Reliability is then the
Spearman–Brown “list-length” reliability of a composite of that length
built from items that share only their axes variance.

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
scale-specificity component of .10, no block specificity — the
instrument is not blockwise, so that component is zero here — and free
item error) that implies an axis reliability of about .78.

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
#>   Note: the model is fit to the item correlation matrix as if it were a
#>   covariance matrix (Cudeck, 1989), and both sides of that mismatch are
#>   corrected -- so these numbers differ from LISREL's, and from lavaan's
#>   own, by design.
#>   The component standard errors are adjusted to the correlation metric
#>   and are calibrated; they are typically smaller than the values printed
#>   by Strack et al. (2013), whose LISREL output carries no correction.
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
comparison. For a balanced instrument the two axes share one
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
#>   Note: the model is fit to the item correlation matrix as if it were a
#>   covariance matrix (Cudeck, 1989), and both sides of that mismatch are
#>   corrected -- so these numbers differ from LISREL's, and from lavaan's
#>   own, by design.
#>   The component standard errors are adjusted to the correlation metric
#>   and are calibrated; they are typically smaller than the values printed
#>   by Strack et al. (2013), whose LISREL output carries no correction.
#> 
#> # Variance components
#> 
#>  Component         Estimate SE   
#>  general           0.051    0.005
#>  axes              0.175    0.009
#>  scale_specificity 0.093    0.008
#>  item              0.680    --   
#> 
#> # Global fit
#> 
#>   chi-square(493) = 488.27,  RMSEA = 0.000,  CFI = 1.000
#> 
#>   The global fit statistics chisq, pvalue, rmsea and cfi are scaled to
#>   that metric (Satorra & Bentler, 1994), which removes a distortion that
#>   flatters fit; df and srmr are unchanged. The scaled test can modestly
#>   over-reject at typical sample sizes -- it over-flags misfit rather than
#>   flattering it; see ?axes_reliability for the measured rates.
```

The **variance components** show the decomposition the reliability rests
on: the `axes` component is the only one that feeds reliability, while
`general`, `scale_specificity`, `block_specificity` (when blocks were
supplied), and item error are isolated from it. This is precisely why
the Nunnally–Bernstein figure printed alongside runs **higher** than the
Strack reliability: N–B charges scale-specificity variance to the axis
rather than isolating it, so it **overestimates** axis reliability
whenever scale specificity is non-trivial (Strack et al., 2013, Figure
3). The gap between the two numbers is a direct read-out of how much
scale-specific variance the simpler formula would have miscredited to
the axes.

## 4. Starting from a published correlation matrix

You do not always have the raw data. A paper may print an item
correlation matrix and nothing else, and that matrix is enough: pass it
as `cormat` together with the sample size it was computed from, in place
of `data`.

``` r

R <- cor(simulated_items)
axes_reliability(
  cormat = R, items = items, angles = octants(), n = nrow(simulated_items)
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
#>   Note: the model is fit to the item correlation matrix as if it were a
#>   covariance matrix (Cudeck, 1989), and both sides of that mismatch are
#>   corrected -- so these numbers differ from LISREL's, and from lavaan's
#>   own, by design.
#>   The component standard errors are adjusted to the correlation metric
#>   and are calibrated; they are typically smaller than the values printed
#>   by Strack et al. (2013), whose LISREL output carries no correction.
```

The estimates are identical to the raw-data run above — the raw-data
path builds exactly this matrix internally and fits it the same way.
`items` selects and orders the matrix’s rows by name, so its own column
ordering does not matter, and it must be symmetric, positive definite,
and have a unit diagonal (the model assumes unit-variance items).

Two things are unavailable here, and both for the same reason: they are
properties of the respondents, not of their correlations. The
Nunnally–Bernstein comparison is reported as `NA` — it needs each
scale’s alpha and the axis composite’s variance, neither of which a
correlation matrix carries. And `sd = "raw"` is refused, because there
are no scale scores to take an observed SD from; supply the axis SDs
numerically if you want SEm on a raw scale. Both are reported rather
than silently omitted, so a matrix-based result cannot be mistaken for a
raw-data one.

## 5. Caveats to keep in mind

Four properties of the method shape how its output should be read.

**Both the standard errors and the global fit statistics are corrected
for the correlation metric.** Following the paper’s own practice, the
model is fit to the item **correlation** matrix as though it were a
covariance matrix. That metric mismatch leaves the component point
estimates and the reliabilities correct, but it misprices anything
computed from the input’s sampling variability (Cudeck, 1989).

The component standard errors are **corrected** for it. Normal-theory
maximum likelihood prices its standard errors for a sample
**covariance** input, while this estimator consumes a sample
**correlation** matrix, whose diagonal cannot vary at all. Left
uncorrected that mismatch **overstates** sampling variability by about
40% for an instrument whose axes carry a lot of variance, and
**understates** it slightly for weak-axes, strong-general instruments —
it changes sign across the range of instruments the function accepts,
which is why it could not be left to a caveat. The reported SEs are
adjusted to the correlation metric and are calibrated uncertainty. They
are typically **smaller** than the standard errors printed in Strack et
al. (2013), whose LISREL values carry the uncorrected approximation.
What the estimator reported before the correction is kept in
`details$se_uncorrected`.

The **global fit statistics are corrected too**, for the same mismatch
running the other way. Sample correlations vary *less* than the
covariances the reference chi-square distribution is derived for, so an
uncorrected test statistic comes out too small and fit is flattered.
`chisq`, `pvalue`, `rmsea` and `cfi` are therefore reported as
Satorra–Bentler-type **scaled** values — the chi-square divided by a
factor computed at the fitted matrix, with `cfi` also scaling its own
baseline model (Satorra & Bentler, 1994). The factor is recomputed for
every fit rather than being a constant, which is the whole point: how
much fit was flattered depends on the instrument. `df` and `srmr` are
unchanged, being a count of restrictions and a residual summary rather
than test statistics with a reference distribution. What the estimator
reported before the scaling is kept in `details$fit_uncorrected`, and
the factors themselves in `details$scaling_factor`.

Read the scaled statistic for what it is. The correction makes the test
statistic match its reference distribution *in expectation*; it is a
calibration, not an exactness guarantee, and it will not rescue a model
that is badly wrong. The usual cutoffs (Hu & Bentler, 1999) are no more
or less appropriate than before — what has changed is that the number
being compared to them is no longer systematically kind.

**How well calibrated is the test, and at what sample size?** The
scaling fixes the metric error, and the χ² test built on it is
asymptotically exact: its rejection rate approaches the nominal α as the
number of distinct moments, p\* = p(p+1)/2, falls relative to N.
Simulated at one population (eight octant scales, three items each, axes
variance .35), the rejection rate at α = .05 runs

| p\*/N          | 0.50 | 0.25 | 0.12 | 0.06 |
|----------------|------|------|------|------|
| rejection rate | .092 | .079 | .062 | .054 |

reaching the nominal band by a p\*/N of about 0.06. That is a sweep at a
single population, not a general threshold.

At **N = 600** the test **over-rejects** — measured .06 to .11 at three
populations chosen to bracket the range of instruments the function
accepts. The uncorrected statistic under-rejects over the same range, at
.02 to .03, and moves *further* from nominal as N grows, because its
error is asymptotic while the scaled statistic’s is a finite-sample one
that shrinks away. The over-rejection at a fixed N grows with instrument
size (larger `df`) and shrinks with N.

Two practical consequences. A p-value near whatever threshold you are
using deserves caution at moderate N with many items — but note the
direction: the scaled test **over-flags** misfit rather than flattering
it, which is the safer error, and the opposite of what the uncorrected
statistic did. And all of this evidence is **complete-data**: under
`missing = "fiml"` the scaled statistic is calibrated in mean, but its
rejection rate has not been measured, so none of these rates should be
read as applying to that path.

If you check the implied matrix, you will also find that it does not
reproduce the unit diagonal exactly. That too is expected: with the
loadings fixed, the condition a free item error satisfies is the
*weighted* diagonal rather than the raw one, so off-diagonal sampling
misfit leaks into the implied diagonal at roughly the sampling standard
error of a correlation.

**Missing data: listwise by default, FIML on request.** The default,
`missing = "listwise"`, uses complete cases only and reports how many
there were. `missing = "fiml"` instead estimates from every respondent
who answered at least one item, and reports the total N with the
complete-case count beside it. Pairwise-deletion correlations are never
used on either setting.

The choice is a trade of assumptions, not a free upgrade. FIML requires
the data to be **missing at random** — missingness may depend on values
you observed, but not on the unobserved values themselves — **and
multivariate normal**. Under MCAR, where missingness is unrelated to
anything, listwise deletion is already consistent and merely wasteful,
so FIML buys precision rather than correctness; under MAR, listwise
deletion is genuinely biased and FIML is not. Under FIML the standard
errors are observed-information standard errors on the standardized
metric, conditional on the standardization constants. They carry the
same correlation-metric correction as every other path, applied
multiplicatively so the observed information’s own pricing of the
missing data survives it. What the correction does not reach is the
uncertainty in those standardization constants, which stays small at
mild missingness but grows with the rate — at 15% cellwise missingness
the reported SEs average about 7% *below* the estimator’s actual
sampling variability, so treat heavy missingness as the regime where
they are least trustworthy. Two results become unavailable, both needing
items observed by every respondent: the Nunnally-Bernstein comparison is
`NA` with a stated reason, and `sd = "raw"` is refused in favour of
numeric axis SDs.

Note the provenance, because it differs from the rest of this vignette:
Strack et al. (2013) report no missing-data analyses at all, so nothing
about the FIML path rests on their results. It is certified against this
package’s own synthetic oracle, where the true variance components are
known by construction.

**A boundary fit returns `NA`, not a clipped value.** If the model
estimates an axes variance outside the interval (0, 1) — at or below
zero the axes carry no variance to be reliable, at or above one they
carry all of it — or any negative variance component, the reliability
and SEm are reported as `NA` with a warning and a boundary flag, rather
than a clipped, negative, or missing number. An `NA` here is a signal
that the model did not identify a usable axes-variance component in your
data — not a defect to be worked around.

**A blockwise instrument needs its blocks declared.** Some circumplex
instruments are administered in blocks — items grouped by something
other than their scale — which carries a block-specificity component of
its own, reported as high as 6.7% by Strack et al. (2013, Table 3). Pass
`blocks` (a list of item columns, one element per block, exactly as
`items` is a list per scale) and that component is estimated too: the
component table gains a `zeta2` row and `details$zeta2_fitted` is
`TRUE`. Nothing in the bundled instrument objects records block
membership, so the map has to come from you.

The component is only estimable when the blocks say something the model
does not already know. Blocks that coincide with the scales, a single
block holding every item, or one block per item all leave `zeta2`
unidentified, and it is dropped with `details$zeta2_fitted` set to
`FALSE` — the same treatment scale specificity gets on a single-item
instrument.

What it costs to *ignore* real blocks depends on their geometry. The
general factor never hands block variance back, so `xi2` is inflated
under most layouts and unchanged under a few — never deflated. The axes
variance, the quantity reliability is actually read from, moves only
when block membership carries information about the *angular distance*
between items, beyond what sharing a scale already tells you.

One case is clean enough to rely on: when **each block draws exactly one
item from every scale**, every within-block pair is a different-scale
pair and the blocks cover every pair of scale positions equally often.
Membership then says nothing about angular distance, and `xi1`, the
reliability, and the SEm are untouched — worth estimating for its own
sake, but costless to omit.

Elsewhere, be careful: *“the blocks are spread evenly around the circle”
is not the test.* Blocks pairing diametrically opposite scales are as
evenly spread as blocks get — their angles average to the centre of the
circle — and at eight scales they still pull `xi1` about 9% *below*
truth, because every within-block pair sits exactly half a turn apart,
which is very much information about angular distance. Blocks covering
contiguous arcs pull about 12% the other way. Unless each block takes
one item per scale, estimate the component rather than reasoning about
the picture.

Finally, a note on the `SEm`. The standard error of measurement supports
a location interval for a single profile (Strack et al., 2013, use
±1.65·SEm). By default
[`axes_reliability()`](http://circumplex.jmgirard.com/reference/axes_reliability.md)
reports the z-standardized SEm, `sqrt(1 - reliability)`; pass
`sd = "raw"` (or your own axis SDs) to put the SEm on the raw axis-score
scale. Such an interval describes the measurement imprecision of one
profile’s axis position; it is not a significance test of that position
against any particular value.

## 6. Wrap-up

[`axes_reliability()`](http://circumplex.jmgirard.com/reference/axes_reliability.md)
gives a compact, per-axis answer to “how reliably does this instrument
measure communion and agency?”, isolating the axes variance from the
general and scale-specific components that a simpler reliability formula
would conflate. Use it to characterize a circumplex instrument before
leaning on its axis scores, and read its output with the
correlation-as-covariance, missing-data, and boundary caveats in mind.

The examples above all use the canonical eight octant scales, but
nothing in the model requires them: any **equally spaced** set of angles
works, at any rotation and at any count from four scales upward. Equal
spacing is what matters, and it is required rather than merely preferred
— a quasi-circumplex, whose scales sit at slightly unequal intervals, is
refused rather than approximated, because Strack et al. (2013) excluded
such instruments when validating the model. Three scales are refused for
a different reason: at that count every pair of scales sits the same
angular distance apart, so the general, axes, and scale-specificity
variances can no longer be told apart.

Scales may also carry a **single item** each, as Strack et al.’s
single-item circumplex types do. With one item at every position no two
items ever share a scale, so nothing in the data distinguishes scale
specificity from item specificity: the scale-specificity component is
not identified, and
[`axes_reliability()`](http://circumplex.jmgirard.com/reference/axes_reliability.md)
drops it from the model rather than estimating a quantity the data
cannot support. The components table then carries three rows instead of
four, and `details$zeta1_fitted` records the drop. An instrument that
*mixes* single-item and multi-item scales still estimates the component
— one multi-item scale supplies the information, and the shared-value
restriction carries it to the remaining scales. One consequence follows
for the comparison figure: coefficient alpha is undefined for a one-item
scale, so the Nunnally–Bernstein reliability is reported as `NA` with
its reason whenever any scale has fewer than two items, exactly as
Strack et al. leave that column blank for such instruments.

One further boundary is worth stating plainly, because the paper crosses
it and this function does not: the model implemented here is
**two**-dimensional. Strack et al. also analyze a *spherical*
instrument, SYMLOG, whose items span three orthogonal dimensions; the
figures they publish for it come from that three-axis model, and no
configuration
[`axes_reliability()`](http://circumplex.jmgirard.com/reference/axes_reliability.md)
accepts will reproduce them.

## References

- Cudeck, R. (1989). Analysis of correlation matrices using covariance
  structure models. *Psychological Bulletin, 105*(2), 317–327.

- Satorra, A., & Bentler, P. M. (1994). Corrections to test statistics
  and standard errors in covariance structure analysis. In *Latent
  variables analysis: Applications for developmental research*
  (pp. 399–419).

- Strack, S., Jacobs, K. A., & Grosse Holtforth, M. (2013). The
  reliability of circumplex axes. *SAGE Open, 3*(2).
  <https://doi.org/10.1177/2158244013486115>
