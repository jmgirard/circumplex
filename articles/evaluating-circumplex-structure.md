# Evaluating Circumplex Structure

``` r

library(circumplex)
```

**Level:** Intermediate. Read “Intermediate SSM Analysis” first.

## 1. Overview

This vignette asks whether an instrument fits a circumplex, the first of
the two conditions that an SSM interpretation rests on. Section 2, “Two
questions to ask before interpreting an SSM analysis”, states both
conditions. Section 3, “Does the instrument fit a circumplex?”, fits the
circular process model with
[`cpm_fit()`](http://circumplex.jmgirard.com/reference/cpm_fit.md),
reads its fit indices, and compares its constrained variants. The
Wrap-up lists what the page covered and names the next pages, and the
References list the sources cited.

## 2. Two questions to ask before interpreting an SSM analysis

The Structural Summary Method (SSM) condenses a circumplex profile into
a few interpretable parameters: elevation, amplitude, displacement, and
fit.
[`vignette("introduction-to-ssm-analysis")`](http://circumplex.jmgirard.com/articles/introduction-to-ssm-analysis.md)
defines each of them. That condensation rests on two assumptions. Both
are testable, but they are often left untested:

1.  **Does the instrument actually have circumplex structure?** The SSM
    locates a profile on a circle, at positions given by the scales’
    theoretical angles. If the scales do not really form a circumplex,
    the summary is distorted before any profile is computed. The same
    holds if they sit at very different angles than the theory says.

2.  **Are the confidence intervals trustworthy at your sample size and
    profile?** Zimmermann and Wright (2017) showed that the accuracy of
    bootstrap SSM intervals depends on the sample size and the
    instrument’s correlation structure. For amplitude and displacement,
    it also depends on how differentiated the profile really is in the
    population. Intervals that are accurate for one construct in your
    table can be inaccurate for the construct printed directly below it.

This page and the two after it show how to answer both questions.
[`cpm_fit()`](http://circumplex.jmgirard.com/reference/cpm_fit.md) and
[`fit_structure()`](http://circumplex.jmgirard.com/reference/fit_structure.md)
answer question 1, and
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/reference/ssm_ci_accuracy.md)
answers question 2. The examples use the `jz2017` dataset. It holds the
same sample of 1,166 undergraduates that Zimmermann and Wright (2017,
Study 5) analyzed. It has octant scores on the IIP-SC, and personality
disorder (PD) scale scores from the PDQ-4+.

## 3. Does the instrument fit a circumplex?

### Fitting the circular process model

[`cpm_fit()`](http://circumplex.jmgirard.com/reference/cpm_fit.md)
estimates Browne’s (1992) circular process model (CPM), the confirmatory
model behind the CIRCUM and CircE programs. Each scale gets an estimated
angle on the circle and a *communality index* $`\zeta`$. The communality
index is the correlation between the scale and the common circumplex
“factor”. Its square, $`\zeta^2`$, is the scale’s *communality*, the
share of its variance that the circumplex explains. The model also
yields the usual covariance-structure fit indices.

[`cpm_fit()`](http://circumplex.jmgirard.com/reference/cpm_fit.md)
accepts raw data or a correlation matrix. On the raw-data path, the
default confidence intervals are bootstrapped. We lower `boots` from its
default of 2000 to keep this vignette quick to build. In practice, use
the default.

This chunk prints two warnings: the CPM Hessian is ill-conditioned, and
some bootstrap resamples were excluded. Both are expected for these
data. The page “CPM Fits at a Boundary” explains the first. The second
means that the intervals rest only on the resamples that were kept.

``` r

data("jz2017")
set.seed(12345)
cpm <- cpm_fit(jz2017, scales = PANO(), angles = octants(), boots = 500)
#> Warning: CPM Hessian is ill-conditioned (condition number 1.83e+14): angles
#> may be clustered or parameters weakly determined.
#> Warning: 11 of 500 bootstrap resamples were excluded (0 with a degenerate or
#> non-positive-definite correlation matrix, 11 failing the convergence
#> acceptance criterion); the confidence intervals are based on the remaining
#> 489 replicates and are conditional on estimability.
summary(cpm)
#> 
#> Circular Process Model (Browne, 1992) 
#> Model:             quasi-circumplex 
#> Harmonics (m):     3 
#> Sample size (N):   1166 
#> Reference scale:   PA 
#> CI method:         bootstrap 
#> Confidence level:  0.95 
#> 
#> # Estimated angles and communality indices
#> 
#>  Scale Theory   Angle     lci     uci  Zeta   lci   uci Communality
#>     PA     90  90.000  90.000  90.000 0.767 0.684 0.860       0.589
#>     BC    135 125.074 113.548 137.069 0.931 0.872 1.000       0.868
#>     DE    180 170.353 156.943 185.174 0.780 0.733 0.826       0.608
#>     FG    225 195.425 185.811 206.523 0.861 0.822 0.910       0.741
#>     HI    270 250.721 244.660 258.758 0.956 0.935 0.977       0.914
#>     JK    315 269.491 261.813 279.412 0.942 0.929 0.957       0.888
#>     LM    360 294.230 286.626 303.019 0.806 0.761 0.850       0.650
#>     NO     45  11.305   2.426  20.913 1.000 1.000 1.000       1.000
#> 
#> # Correlation-function weights
#> 
#>  k  Beta Beta_lci Beta_uci
#>  0 0.450    0.418    0.481
#>  1 0.440    0.411    0.466
#>  2 0.074    0.065    0.087
#>  3 0.036    0.015    0.057
#> 
#> # Fit indices
#> 
#>   χ²(10) = 81.169, p = <1e-04
#>   RMSEA = 0.078 [0.063, 0.094] (90% CI)
#>   SRMR  = 0.042
#>   CFI   = 0.984    TLI = 0.956
#>   AIC   = 117.169    BIC = 208.273
#> 
#> # Residuals
#> 
#>   Largest absolute residual: 0.134 (PA – HI)
#> 
#> # Diagnostics
#> 
#>   Note: a communality index reached its upper boundary (ζ > 0.995, a
#>   Heywood-type solution).
#>   Note: 11 of 500 bootstrap resamples were excluded (0 degenerate, 11
#>   non-convergent); the intervals are based on 489 replicates and are
#>   conditional on estimability.
#> 
#>   Note: boundary/weak-identification markers fired: Heywood communality;
#>   small correlation-function weight; ill-conditioned Hessian.
#>   What has been measured about these markers covers analytic intervals only,
#>   and not every marker was measured; they are not validated as predictors of
#>   the bootstrap intervals shown here (see the vignette section 'When a fit
#>   sits at a boundary').
```

Two parts of this output matter most for evaluating structure:

- **The estimated angles.** Compare them with the theoretical angles,
  but read the comparison carefully. One scale is held fixed to identify
  the configuration (PA here, at 90°). So every other scale’s departure
  is measured from that anchor, and a different anchor would
  redistribute the departures. Two things are worth separating. The
  *ordering* around the circle is preserved: the estimated angles run
  through the octants in the same cyclic order that the instrument
  assigns them. The *spacing* is not preserved. The gaps between
  circularly adjacent estimates run from under 20° to nearly 80°,
  against a theoretical 45°. The largest departure from a theoretical
  position is about 66°. Departures from perfect structure are common in
  well-validated circumplex instruments (Gurtman & Pincus, 2000). The
  model comparison below quantifies what this pattern costs: it is why
  forcing equal spacing fits these data poorly.
- **The communality indices.** Scales with low $`\zeta`$ are poorly
  described by the circle. A profile peak at such a scale’s angle means
  less than the same peak at the angle of a well-explained scale.

The estimated configuration is easiest to read on the circle itself. The
plot draws each scale at its estimated angle, at a radius equal to its
communality ($`\zeta^2`$). It adds a joint confidence region where one
is estimable.

``` r

plot(cpm)
```

![plot of chunk
cpm_plot](figures/evaluating-circumplex-structure-cpm_plot-1.png)

### Reading the fit indices

The [`summary()`](https://rdrr.io/r/base/summary.html) output reports
the standard indices, and the standard benchmarks apply:

- RMSEA at or below about .08 suggests adequate approximate fit, and
  above about .10 suggests poor fit (Browne & Cudeck, 1993).
- SRMR at or below about .08 suggests acceptable average residuals (Hu &
  Bentler, 1999).
- CFI and TLI near .95 or above suggest good comparative fit (Hu &
  Bentler, 1999).

Treat these as conventions from the broader covariance-structure
literature, not circumplex-specific laws.

Two caveats come from the benchmark sources themselves. First, Browne
and Cudeck call their own RMSEA thresholds “based on subjective
judgment.” They caution that such a figure “cannot be regarded as
infallible or correct” (Browne & Cudeck, 1993). So read the cutoffs as
conventions, not decision rules. Second, these benchmarks are least
dependable at small samples. Hu and Bentler (1999) found that the
ML-based TLI and RMSEA tend to *overreject* true-population models when
the sample is small. (CFI is not among the indices they flag.)
Circumplex analyses are often run at modest sample sizes. The SSM
accuracy thresholds in “Confidence Interval Accuracy” span roughly
$`n = 50`$ to $`200`$. At small $`n`$, a TLI or RMSEA that falls short
of its benchmark can reflect the index’s small-sample behavior as much
as the model’s fit.

Two further cautions are circumplex-specific, and both come from this
package’s own validation simulations:

- **Boundary solutions are common at realistic sample sizes**, including
  in the fit above. The page “CPM Fits at a Boundary” says what that
  means and what to do about it.
- **Do not lean hard on the chi-square p value.** In simulations at
  field-typical sample sizes with octant-like population structures, the
  test statistic did not follow its nominal chi-square reference
  distribution. This held even for correctly specified models. Read the
  chi-square descriptively (bigger is worse), and prefer RMSEA/SRMR/CFI
  for judging approximate fit.

For the same reasons, [`summary()`](https://rdrr.io/r/base/summary.html)
prints a caution for analytic (Wald) intervals below $`N = 2000`$. When
a boundary marker (defined in “CPM Fits at a Boundary”) is present, the
analytic caution prints up to very large $`N`$. In validation, analytic
intervals mis-covered in exactly those regimes. Analytic intervals are
the only option on the correlation-matrix input path. Prefer the
bootstrap (the raw-data default) when you have raw data.

### Comparing model variants

Constrained variants make the structural question sharp. Is the
instrument consistent with *equally spaced* scales? Is it consistent
with *equal* communalities? The correlation-matrix path is convenient
for this comparison, because its point estimates and fit indices are
deterministic. So no bootstrap is needed. The first of these refits
(`quasi-circumplex`) prints the same ill-conditioned Hessian warning
that “CPM Fits at a Boundary” explains.

``` r

R <- cpm$matrices$R # the sample correlation matrix stored by cpm_fit()
fit_quasi <- cpm_fit(
  cormat = R, scales = PANO(), angles = octants(),
  n = nrow(jz2017), model = "quasi-circumplex"
)
#> Warning: CPM Hessian is ill-conditioned (condition number 1.83e+14): angles
#> may be clustered or parameters weakly determined.
fit_equal <- cpm_fit(
  cormat = R, scales = PANO(), angles = octants(),
  n = nrow(jz2017), model = "equal-communality"
)
fit_circulant <- cpm_fit(
  cormat = R, scales = PANO(), angles = octants(),
  n = nrow(jz2017), model = "circulant"
)
```

The table below has one row for each of the three fits. It shows the
model name from `$details$model`, and the degrees of freedom and four
fit indices from `$fit`. The indices are rounded to three decimals. (The
code that builds the table is omitted.)

    #>               model df rmsea  srmr   cfi   tli
    #> 1  quasi-circumplex 10 0.078 0.042 0.984 0.956
    #> 2 equal-communality 17 0.100 0.063 0.956 0.928
    #> 3         circulant 24 0.185 0.130 0.790 0.755

The pattern reproduces what Zimmermann and Wright (2017, p. 14) reported
for these data with CircE. The fully constrained model (equal spacing
*and* equal communality, the `"circulant"` variant) fits poorly.
Relaxing the equal-spacing constraint improves fit to the edge of the
conventional benchmarks: marginal by RMSEA, and acceptable by SRMR and
CFI. They reported CFI = .824, TLI = .795 and RMSEA = .169 for the fully
constrained model. For the model with the equal-spacing constraint
relaxed, they reported CFI = .958, TLI = .931 and RMSEA = .098.

Do not expect the default fit’s indices to match theirs to the digit.
CIRCUM/CircE fit a covariance version of the model with free scaling
constants. By default,
[`cpm_fit()`](http://circumplex.jmgirard.com/reference/cpm_fit.md) fits
the correlation structure directly, and the two versions differ slightly
at finite sample sizes. Pass `scaling = "free"` to fit their covariance
parameterization and reproduce published CIRCUM/CircE output exactly.

For the model test, the scaling choice does not matter here. With
correlation input, the two families’ test statistics are
calibration-indistinguishable. In paired simulation at sample sizes
250–50,000, they differed by well under 1% of the model degrees of
freedom. The free statistic never exceeds the default’s on the same
input, beyond numerical tolerance. This is because the free family nests
the default and is also started from its solution. The default is
recommended for routine inference, because free scaling adds parameters
whose analytic standard errors are often undefined at small-to-moderate
samples. The *conclusion* is the same: ordered octants with unequal
spacing, and adequate approximate fit once equal spacing is not forced.

A poor CPM fit does not make SSM output uncomputable. It makes the
summary less meaningful, because the profile is referred to scale
positions that the data contradict. If the ordering itself fails (scales
out of sequence around the circle), SSM parameters should not be
interpreted.

## Wrap-up

Evaluating circumplex structure has two layers. The first is whether the
instrument behaves like a circumplex in your sample
([`cpm_fit()`](http://circumplex.jmgirard.com/reference/cpm_fit.md)).
The second is whether the inferential machinery of the SSM can be
trusted at your sample size and profile
([`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/reference/ssm_ci_accuracy.md)).
Both are one function call, and both change what you should claim more
often than users expect. In particular, amplitude and displacement
intervals earn their trust only when the profile is genuinely
differentiated relative to the precision that your sample size affords.

Two pages follow this one. “CPM Fits at a Boundary” explains the
boundary note that the fit above printed and what to do when a marker
fires. “Confidence Interval Accuracy” asks whether the intervals can be
trusted at your sample size and profile.

## References

- Browne, M. W. (1992). Circumplex models for correlation matrices.
  *Psychometrika, 57*(4), 469–497.

- Browne, M. W., & Cudeck, R. (1993). Alternative ways of assessing
  model fit. In K. A. Bollen & J. S. Long (Eds.), *Testing structural
  equation models* (pp. 136–162). Newbury Park, CA: Sage.

- Grassi, M., Luccio, R., & Di Blas, L. (2010). CircE: An R
  implementation of Browne’s circular stochastic process model.
  *Behavior Research Methods, 42*(1), 55–73.

- Gurtman, M. B., & Pincus, A. L. (2000). Interpersonal Adjective
  Scales: Confirmation of circumplex structure from multiple
  perspectives. *Personality and Social Psychology Bulletin, 26*(3),
  374–384.

- Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in
  covariance structure analysis: Conventional criteria versus new
  alternatives. *Structural Equation Modeling, 6*(1), 1–55.

- Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in
  interpersonal construct validation: Methodological advances in the
  circumplex Structural Summary Approach. *Assessment, 24*(1), 3–23.
