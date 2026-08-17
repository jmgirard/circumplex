# Reliability of the circumplex axes (Strack, Jacobs & Grosse Holtforth, 2013)

Estimate the reliability (and standard error of measurement) of the two
circumplex axes of an instrument with the item-level restricted
tau-equivalent CFA of Strack, Jacobs, and Grosse Holtforth (2013). The
model decomposes each item's variance into orthogonal components – a
general factor, the two circumplex axes, scale specificity, block
specificity for a blockwise instrument, and item specificity – and reads
the axes' reliability off the isolated axes-variance component with the
Spearman-Brown formula. It is a confirmatory, item-level complement to
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
  blocks = NULL,
  sd = "std",
  missing = c("listwise", "fiml")
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

- blocks:

  Optional. For a **blockwise** instrument, a list with one element per
  administration block, each a character vector (or numeric indices) of
  that block's item columns – the same shape `items` takes for scales.
  The blocks must partition the items: every item in exactly one block.
  Supplying them adds the block-specificity component to the model; see
  "Blockwise instruments" below.

- sd:

  The scale for the standard error of measurement: `"std"` (the default)
  reports the z-standardized SEm `sqrt(1 - reliability)`; `"raw"` uses
  each axis composite's observed raw SD; or a numeric vector (length 1,
  recycled, or length 2 for the X and Y axes) of axis SDs. A supplied
  numeric SD must be finite and positive; anything else is refused
  rather than carried into the reported SEm.

- missing:

  How item-level missing data are handled on the `data` path:
  `"listwise"` (the default; complete cases only) or `"fiml"`
  (full-information maximum likelihood, via lavaan's `missing = "ml"`),
  which uses every respondent who answered at least one item. Not
  available with `cormat`, which carries no missing cells.

## Value

An object of class `circumplex_axes_reliability` with
[`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) methods: `results`
(one row per axis: the axes variance, item_n, reliability, SEm,
Nunnally-Bernstein reliability, and boundary flag), `components` (the
estimated variance components with SEs – four rows by default, three
when scale specificity was dropped, five when block specificity was
fitted), `fit` (global fit indices), and `details` (including
`zeta1_fitted` and `zeta2_fitted`, whether scale and block specificity
were in the model, `blocks`, the block labels when a block map was
supplied, `nb_reason`, why the Nunnally-Bernstein comparison is `NA`,
`missing`, which missing-data treatment lavaan actually used,
`n_complete`, the complete-case count, `min_coverage`, the fewest
respondents behind any item pair, `se_uncorrected`, the component
standard errors as normal-theory maximum likelihood reports them before
the correlation-structure correction, `se_correction_failed`, `NULL`
when that correction succeeded or a string naming why the reported SEs
are `NA` – notably the shared degeneracy criterion's two literals
(smallest eigenvalue relative to largest at or below
`sqrt(p * .Machine$double.eps / 1e-6)`, evaluated on
[`cov2cor()`](https://rdrr.io/r/stats/cor.html) of the fitted covariance
matrix): `"indefinite"` for a decisively negative smallest eigenvalue
(below `-lambda_max * sqrt(p * .Machine$double.eps)`), a statement about
the model, and `"ill_conditioned"` for roundoff-level negativity,
singularity, or ill-conditioning, a numerical caution – which also sets
`fit_scaling_failed` when the shared criterion is what tripped it –
`naive_reason`, `NULL` unless the internal uncorrected arm (the
diagnostic tie to lavaan's own standard errors) was refused while every
reported number computed, whether by the same criterion evaluated on the
raw fitted matrix or by that arm's own pricing (`"singular"`,
`"unidentified"`, `"indefinite"`); it carries the same reason vocabulary
and is deliberately silent – no warning or printed note accompanies it –
`fit_uncorrected`, the six fit statistics as lavaan reports them before
the correlation-metric scaling, `scaling_factor`, the two
Satorra-Bentler factors (`model` and `baseline`), and
`fit_scaling_failed`, `NULL` when the scaling succeeded or a string
naming why `chisq`, `pvalue`, `rmsea` and `cfi` are `NA`). `details`
also carries `n_moments`, the number of distinct analyzed moments \\p^\*
= p(p+1)/2\\, and `baseline`, the independence model's **unscaled**
`chisq` and `df`. Those two, with `fit$chisq`, `fit$df` and the
`baseline` element of `scaling_factor` – five inputs, since the baseline
chi-square must be scaled by its own factor before it is used –
reproduce the reported `cfi`. Note that `details$baseline` and the
`baseline` element of `details$scaling_factor` are different quantities
that share a name: the first is a chi-square and df pair, the second a
scaling factor. `fit` carries `chisq`, `df`, `pvalue`, `rmsea`, `cfi`
and `srmr`; the four chi-square-derived values are scaled and `df` and
`srmr` are not. Three sample sizes sit beside each other in `details`
and are not interchangeable. `n` is the one the fit was priced at – the
number of rows the estimator was actually handed, after listwise
deletion or after dropping rows with no observed item, and the `n` you
supplied on the correlation-matrix path. It is the N to divide
`n_moments` by when locating a fit on the calibration table in
[`vignette("axes-reliability")`](http://circumplex.jmgirard.com/articles/axes-reliability.md).
`n_total` is the number of rows supplied before any of that, and
`n_complete` the number answering every item. `n_complete` and
`min_coverage` are present on every path so that a caller can read them
unconditionally, and are `NA` where they carry no information:
`min_coverage` outside `missing = "fiml"`, and both of them when a
correlation matrix was supplied in place of raw data.

## Details

The model is fit to the item **correlation** matrix (the items are
z-standardized) as a flat fixed-links CFA: every item loads on the two
axes with fixed cosine weights, on a general factor with weight one, on
its scale's specificity factor with weight one, and – when `blocks` are
supplied and identified – on its block's specificity factor with weight
one; the two axis variances are held equal (the circumplex "no preferred
rotation" axiom), every scale-specificity variance shares one value and
every block-specificity variance shares one value, while item errors
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
estimates and the reliabilities are correct, and both the component
standard errors and the global test statistic are **corrected** for that
metric (Cudeck, 1989; Satorra & Bentler, 1994). Results are reported
**per axis** (X and Y): for a balanced instrument the two axes carry the
same axes-variance estimate and differ only through `item_n`.

What the correction does. Normal-theory maximum likelihood prices its
standard errors for a sample **covariance** input, while this estimator
consumes a sample **correlation** matrix, whose diagonal cannot vary at
all and whose off-diagonal cells are less variable than the
corresponding covariances. Left uncorrected, that mismatch
**overstates** sampling variability by about 40% for an instrument whose
axes carry a lot of variance (an axes variance of .35), and
**understates** it slightly for weak-axes, strong-general instruments –
so it could not be stated honestly by any fixed caveat, because it
changes sign across the range of instruments this function accepts. The
reported SEs are therefore adjusted to the correlation metric and are
calibrated uncertainty, not order-of-magnitude guidance. They are
typically **smaller** than the standard errors printed in Strack et al.
(2013), whose LISREL values carry the uncorrected approximation. Point
estimates, reliabilities, and SEm are unchanged by the correction. What
lavaan reported before it is kept in `details$se_uncorrected`.

The global test statistic carries the same mismatch in the other
direction, and is corrected too. Left alone it is too **small** – fit is
flattered, because the sample correlations the estimator consumes vary
less than the covariances the reference chi-square is derived for.
`chisq`, `pvalue`, `rmsea` and `cfi` are therefore reported as
Satorra-Bentler-type **scaled** values, `chisq` divided by a factor
computed at the fitted matrix (Satorra & Bentler, 1994), with `cfi`
additionally scaling its own baseline model. The factor is not a
constant: it is recomputed for every fit, which is the point – the size
of the distortion depends on the instrument. `df` and `srmr` are
**unchanged**, being a count of restrictions and a residual summary
rather than test statistics with a reference distribution. What lavaan
reported before the scaling is kept in `details$fit_uncorrected`, and
the two factors in `details$scaling_factor`.

One thing the scaling is **not**: it is not a robustness correction for
non-normal data. The factor is computed under normal theory throughout,
and it corrects one thing only – that the estimator consumes a sample
correlation matrix where normal-theory maximum likelihood prices a
sample covariance matrix. It does not license the model against skewed
or heavy-tailed items, and it is unrelated to the Satorra-Bentler scaled
statistics reported by
[`ssm_sem()`](http://circumplex.jmgirard.com/reference/ssm_sem.md)'s
robust estimators, which correct for non-normality on the covariance
metric.

The scaled statistic matches its reference chi-square in **mean**; it is
not exact, and it does not make a badly misspecified model fit. If the
factor cannot be computed, all four are `NA` with the reason in
`details$fit_scaling_failed` – never the uncorrected value in their
place. One refusal is shared with the component-SE correction, under one
stated criterion evaluated in the metric the reported numbers are
computed in: a fitted covariance matrix whose correlation form
[`cov2cor()`](https://rdrr.io/r/stats/cor.html) is degenerate –
indefinite, singular, or so ill-conditioned that its smallest
eigenvalue, relative to its largest, falls at or below
`sqrt(p * .Machine$double.eps / 1e-6)`, where the `1e-6` is a stated
accuracy target: past that floor the corrected standard errors could
carry relative error above it – is refused by both surfaces, and the
refusal says which degeneracy happened: `"indefinite"` when the smallest
eigenvalue is decisively negative (below
`-lambda_max * sqrt(p * .Machine$double.eps)` – beyond the fit's own
numerical noise band, so it is a statement about the model),
`"ill_conditioned"` for roundoff-level negativity, exact singularity, or
mere ill-conditioning (a numerical caution). Either way the corrected
standard errors and the four scaled statistics go `NA` together (each
with its own warning naming that reason) rather than one surface
refusing while the other silently scales. The standard-error surface
additionally applies the same criterion to the raw fitted matrix, which
one internal arm of its computation – the uncorrected normal-theory
pricing, kept only as a diagnostic tie to lavaan's own standard errors –
inverts. A matrix degenerate only in the raw metric (wildly unequal
fitted variances over a well-conditioned correlation structure) refuses
that internal arm alone: the reported standard errors and scaled fit
statistics all compute – `details$se_correction_failed` and
`details$fit_scaling_failed` are both `NULL`, and no warning or note
fires, because every reported number is present and priced in the metric
the criterion cleared – and the internal refusal is recorded, silently,
in `details$naive_reason` under the same reason vocabulary. Under the
shared criterion the two surfaces' user-facing refusals therefore agree
exactly: whatever the criterion refuses for the scaled statistics it
refuses for the standard errors with the same reason, and nothing the
raw metric alone refuses touches either. (Each surface retains its own
refusals outside the criterion – the saturated-model door below touches
only the fit statistics, and either surface's internal computation can
still refuse on its own.) On a unit-diagonal fitted matrix the two
metrics coincide. Separately, a saturated model (`df = 0`) is refused as
`"saturated"` before any scaling arithmetic runs – a refusal that
touches only the four scaled statistics; the corrected standard errors
still compute. (A `df = 0` fit is reachable today only at the internal
helpers' documented contract boundary: `axes_reliability()` itself
refuses the three-item maps that could saturate.) `df` and `srmr` still
report.

If you cross-check against lavaan, match the variant. The scaled
`chisq`, `pvalue`, `rmsea` and `cfi` here are built with the definitions
lavaan calls `chisq.scaled`, `pvalue.scaled`, `rmsea.scaled` and
`cfi.scaled` – the mean-adjusted Satorra-Bentler forms, `cfi` scaling
its baseline term as well as its model term. They are **not** the
`*.robust` forms (`cfi.robust`, `rmsea.robust`), which apply the
Brosseau-Liard/Savalei adjustment and give different numbers from the
same inputs. And because the model is estimated with plain maximum
likelihood, the scaling being applied here rather than by lavaan,
`fitMeasures()` on an equivalent fit reports the **uncorrected** values
– those in `details$fit_uncorrected` – under the bare names `chisq`,
`pvalue`, `rmsea` and `cfi`. It reports no `*.scaled` or `*.robust`
measure at all on such a fit: lavaan supplies those only for a genuinely
scaled estimator such as `"MLM"` or `"MLR"`, and silently returns a
shorter vector when asked for one it does not have. So a cross-check
against lavaan's bare `cfi` will disagree with `$fit$cfi`, a request for
`cfi.robust` will come back empty rather than disagreeing, and neither
outcome is a defect. `details$baseline` and `details$scaling_factor`
carry what you need to rebuild the reported values yourself.

## How well calibrated is the test, and at what sample size

The scaling fixes the metric error, and the \\\chi^2\\ test built on it
is asymptotically exact: its rejection rate approaches the nominal
\\\alpha\\ as the number of distinct moments p\\^\*\\ = p(p+1)/2 falls
relative to N. Measured by simulation at one population (8 octant
scales, 3 items each, axes variance .35), the rejection rate at
\\\alpha\\ = .05 runs .092, .079, .062, .054 at p\\^\*\\/N = 0.50, 0.25,
0.12, 0.06 – reaching the nominal band by p\\^\*\\/N of about 0.06. That
is a sweep at a single population, not a general threshold.

At **N = 600** the \\\chi^2\\ test **over-rejects**: measured .06 to .11
at three populations chosen to bracket the range of instruments this
function accepts. The uncorrected statistic under-rejects over the same
range, at .02 to .03, and – unlike the scaled one – moves *further* from
nominal as N grows, because its error is asymptotic while the scaled
statistic's is a finite-sample one that shrinks away.

The over-rejection at a fixed N grows with instrument size (larger `df`)
and shrinks with N. So a p-value near whatever threshold you are using
deserves caution at moderate N and a large item count – but note the
direction: the scaled test **over-flags** misfit rather than flattering
it, which is the safer error and the opposite of what the uncorrected
statistic did.

All of that evidence is **complete-data**. Under `missing = "fiml"` the
scaled statistic is calibrated in mean, but its rejection rate has not
been measured, so none of the rates above should be read as applying to
that path.

A related detail, in case you check: the fitted model does **not**
reproduce the correlation matrix's unit diagonal exactly, and that is
expected rather than a defect. With the loadings fixed, the stationarity
condition available for a free item error is the *weighted* diagonal,
not the raw one, so off-diagonal sampling misfit leaks into the implied
diagonal at roughly the sampling standard error of a correlation.

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

## Missing data

`missing = "listwise"` is the default: only complete cases are used, and
a message reports how many there were. Pairwise-deletion correlations
are never used on either setting.

`missing = "fiml"` instead estimates from every respondent who answered
at least one item, by full-information maximum likelihood. Two
assumptions come with it, and both are stronger than listwise
deletion's: the data must be **missing at random** (missingness may
depend on values you observed, but not on the unobserved values
themselves) **and multivariate normal**. Under MCAR — the special case
where missingness is unrelated to anything — listwise deletion is
*consistent*, merely inefficient, so FIML buys precision there and not
correctness. Under MAR listwise deletion is genuinely biased and FIML is
not, which is when the switch is worth its assumptions.

The items are standardized by the saturated model's own FIML means and
SDs, never by the means and SDs of whichever cells happen to be
observed, and those standardized columns feed a single FIML fit. The
reported standard errors are observed-information standard errors on
that standardized metric, conditional on the standardization constants
(they do not propagate the uncertainty in the constants themselves).
They carry the same correlation-metric correction as every other path,
applied multiplicatively so that the observed information's own pricing
of the missing data survives it. What the correction does not reach is
the uncertainty in the standardization constants above. At mild rates
that residual is too small to pin down: at 2%, 5%, and 10% cellwise
missingness it measures 0.1%, 0.8%, and 1.8%, all well inside the
Monte-Carlo error of the comparison itself (about 3.6% over 200
replicates), so its size is bounded but its direction at those rates is
not established.

It becomes measurable, and **anti-conservative**, as missingness grows.
Over 201 replicates at 15% cellwise MCAR the reported standard errors
average about 7% **below** the estimator's actual sampling variability,
so at that rate a confidence interval built from them is slightly too
narrow. Note the direction reverses: the mild-rate figures above, such
as they are, sit on the conservative side. Treat heavy missingness as
the regime where these standard errors are least trustworthy, and prefer
a resampling interval there if the uncertainty matters to your
conclusion.

The global fit statistics are scaled on this path too, by the same
factor the complete-data paths use rather than one rebuilt from the FIML
fit's own saturated stage. That is deliberate and follows the standard
errors above: lavaan's FIML chi-square is already referenced against the
FIML saturated loglikelihood, so it already prices the missing
information, while the scaling factor's normal-theory reference is
exactly 1 – which makes the factor a metric-only ratio. A factor that
priced missingness a second time would double-count it.

Two results are unavailable under `missing = "fiml"`, both because they
need items observed by every respondent: the Nunnally-Bernstein
comparison is `NA` with a stated reason, and `sd = "raw"` is refused —
supply the axis SDs numerically instead.

A note on provenance: Strack et al. (2013) report no missing-data
analyses, so nothing about the FIML path rests on their results. It is
certified against this package's own synthetic oracle, where the true
variance components are known by construction.

## Boundary solutions

This contract governs every input path – raw items on either `missing`
setting, and a supplied correlation matrix alike. A boundary fit returns
`NA` reliability and SEm with a warning and a boundary flag rather than
a clipped, negative, or missing value. A fit counts as a boundary when
the estimated axes variance falls outside `(0, 1)` – at or below zero
the axes carry no variance to be reliable, and at or above one they
carry all of it, which drives the Spearman-Brown reliability to one or
beyond, leaving the standard error of measurement at zero or undefined –
or when any estimated variance is negative.

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

Some circumplex instruments are administered in **blocks** – items
grouped by something other than their scale – which carries a
block-specificity variance of its own (Strack et al. 2013 report it as
high as 6.7%). Supply `blocks` to estimate it as a fifth component: the
`components` table then carries a `zeta2` row and `details$zeta2_fitted`
is `TRUE`. The package's instrument objects record no block structure,
so the map comes from you.

Block specificity is estimable only when the blocks are not a
relabelling of something the model already has. If every block coincides
with a scale, or all items share one block, or every item sits in its
own block, the component explains nothing the others do not; it is
dropped from the model, `details$zeta2_fitted` is `FALSE`, and the
component table keeps its four rows. That decision is read off the
data's own moment structure rather than from a rule of thumb about how
the blocks look, so it also catches maps whose redundancy is not obvious
by eye.

**What omitting `blocks` costs depends on the block geometry**, and it
is not a uniform penalty. The general factor never gives block variance
back, so `xi2` is inflated under most layouts and unchanged under a few;
it is never deflated. The axes variance – the one reliability is read
from – moves only when block membership carries information about the
angular distance between items, over and above what sharing a scale
already says.

The clean case is worth stating exactly, because it is both common and
checkable: **when each block draws exactly one item from every scale**,
every within-block pair is a different-scale pair and the blocks span
every pair of scale positions equally often. Block membership then says
nothing about angular distance, and `xi1`, the reliability, and the SEm
are unaffected – the component is worth estimating for its own sake, but
ignoring it costs the reliability nothing.

Away from that case the bias runs in either direction and **"the blocks
are spread evenly around the circle" is not the test.** Blocks that pair
diametrically opposite scales are as dispersed as a block can be – their
angles average to the centre of the circle – and at eight scales they
still pull `xi1` about 9% below truth, because every within-block pair
sits half a turn apart and that is emphatically information about
angular distance. Blocks covering contiguous arcs pull it the other way,
about 12% above. When the blocks are neither one item per scale nor
obviously arbitrary, estimate the component rather than reasoning about
the geometry.

## References

Strack, S., Jacobs, K. A., & Grosse Holtforth, M. (2013). The
reliability of circumplex axes. *SAGE Open*, 3(2).
[doi:10.1177/2158244013486115](https://doi.org/10.1177/2158244013486115)

Cudeck, R. (1989). Analysis of correlation matrices using covariance
structure models. *Psychological Bulletin*, 105(2), 317-327.

Satorra, A., & Bentler, P. M. (1994). Corrections to test statistics and
standard errors in covariance structure analysis. In *Latent variables
analysis: Applications for developmental research* (pp. 399-419).

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
#>   Note: the model is fit to the item correlation matrix as if it were a
#>   covariance matrix (Cudeck, 1989), and both sides of that mismatch are
#>   corrected -- so these numbers differ from LISREL's, and from lavaan's
#>   own, by design.
#>   The component standard errors are adjusted to the correlation metric
#>   and are calibrated; they are typically smaller than the values printed
#>   by Strack et al. (2013), whose LISREL output carries no correction.
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
#>   They follow lavaan's *.scaled definitions, not its *.robust ones, and
#>   differ from what fitMeasures() reports for an equivalent ML fit.

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
#>   Note: the model is fit to the item correlation matrix as if it were a
#>   covariance matrix (Cudeck, 1989), and both sides of that mismatch are
#>   corrected -- so these numbers differ from LISREL's, and from lavaan's
#>   own, by design.
#>   The component standard errors are adjusted to the correlation metric
#>   and are calibrated; they are typically smaller than the values printed
#>   by Strack et al. (2013), whose LISREL output carries no correction.
```
