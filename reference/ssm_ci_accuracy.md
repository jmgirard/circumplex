# Assess the accuracy of SSM confidence intervals by simulation

Estimate, by simulation, whether the confidence intervals of a fitted
[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md)
object would cover the true SSM parameters at their nominal rate if the
population looked like the fitted estimates, at the observed sample
size(s). Following Zimmermann & Wright (2017), the population's scale
intercorrelation structure is characterized by fitting Browne's (1992)
circular process model
([`cpm_fit()`](http://circumplex.jmgirard.com/reference/cpm_fit.md)) to
the pooled within-group scale correlations; each of `reps` datasets is
then simulated from that plug-in population at the object's exact group
sizes and the object's own interval procedure (same engine, same
`boots`, same `interval`) is rerun on it. Coverage is reported per
profile row, parameter, and amplitude condition, with 95% Wilson score
intervals classified against Bradley's (1978) liberal robustness band at
the as-estimated condition.

## Usage

``` r
ssm_ci_accuracy(
  ssm_object,
  reps = 1000,
  amplitude_factors = c(1, 0.5, 0.25, 0),
  structure = c("cpm", "observed"),
  m = NULL,
  cpm = NULL,
  data = NULL,
  parallel = "no",
  ncpus = 1
)
```

## Arguments

- ssm_object:

  Required. A `circumplex_ssm` object from
  [`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md).

- reps:

  Optional. The number of simulated datasets per amplitude condition
  (default = 1000, binomial SE about 0.7 percentage points; 500 is a
  reasonable quick-look floor).

- amplitude_factors:

  Optional. Numeric vector of amplitude scaling factors in `[0, 1]`
  defining the ladder conditions (default = `c(1, 0.5, 0.25, 0)`). Must
  include 1, the as-estimated condition that the verdict is keyed to.
  Applied to every profile row jointly.

- structure:

  Optional. `"cpm"` (default) simulates from the Browne model's
  model-implied scale correlations; `"observed"` bypasses the model and
  uses the pooled within-group correlation matrix directly (a
  sensitivity switch: if the two verdicts differ, structure uncertainty
  is itself material). Not accepted for occasions analyses, whose
  population is always the observed stacked cross-occasion covariance
  (no model is fit).

- m:

  Optional. The number of harmonics passed to
  [`cpm_fit()`](http://circumplex.jmgirard.com/reference/cpm_fit.md)
  (default `NULL` uses `min(3, floor((p - 1) / 2))`).

- cpm:

  Optional. A pre-fitted `circumplex_cpm` object to reuse for the
  population structure instead of refitting (its scales must match the
  ssm object's). Must be a unit-scaling fit (the default
  [`cpm_fit()`](http://circumplex.jmgirard.com/reference/cpm_fit.md)
  scaling); a free-scaling fit models a covariance structure and is not
  a valid population correlation structure for this diagnostic. Ignored
  when `structure = "observed"`; not accepted for occasions analyses.

- data:

  Optional. The original data set, required only for ssm objects created
  before sufficient statistics were stored at analysis time; the
  statistics are then recomputed and checked against the stored profile
  vectors.

- parallel:

  Optional. `"no"` (default), `"multicore"`, or `"snow"`; distributes
  the simulation replicates across `ncpus` cores. Results are identical
  for a given seed regardless of these settings (see Reproducibility).

- ncpus:

  Optional. Number of cores when `parallel` is not `"no"` (default = 1).

## Value

A `circumplex_ci_accuracy` object: a list with `coverage` (per Profile x
Parameter x Condition: coverage, its Monte Carlo SE, the one-sided miss
rates, median CI width, and for displacement the
certification-conditional coverage with the number of certified
replicates behind it – for a contrast row this conditional column is
retained as a joint-certification descriptive that no display consumes;
`Structural` flags the amplitude rows whose zero coverage is a theorem
rather than a measurement), `guardrail` (per Profile x Condition:
certification rate with its 95% Wilson score interval, the
user-expectation benchmark `(1 - interval) / 2`, the stored
false-certification caution decision at the `c = 0` rung (`Caution`,
true when the Wilson lower bound exceeds the benchmark; `NA` off that
rung, and `NA` for a contrast row, which `print.circumplex_ssm()` never
gates), fit-pass rate, and the branch-pathology rate – the rate at which
a displacement point estimate falls geometrically outside its own
interval), `verdict` (Wilson-vs-Bradley classification of elevation,
amplitude, and displacement coverage at the as-estimated condition – a
profile's displacement is classified certification-conditionally
(`Parameter` `"d_conditional"`), a contrast's unconditionally
(`Parameter` `"d"`) – plus an overall worst-of row per profile; note the
printed verdict headline additionally elevates to CAUTION whenever the
guardrail `Caution` fired, so it can read worse than the overall
coverage class), `cpm` (the embedded
[`cpm_fit()`](http://circumplex.jmgirard.com/reference/cpm_fit.md)
object, or `NULL` when `structure = "observed"`), `population` (per
profile row: the population profile vectors, truth parameters, and any
positive-semidefiniteness repair magnitude, by condition), and
`details`. The [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
method draws coverage against the amplitude ladder with the Bradley band
shaded; [`summary()`](https://rdrr.io/r/base/summary.html) adds a
plain-language verdict (see
[`summary.circumplex_ci_accuracy()`](http://circumplex.jmgirard.com/reference/summary.circumplex_ci_accuracy.md)).

## Details

Displacement coverage is angular: the truth is inside the reported
interval as an arc (membership modulo 360 degrees), so populations
peaking at the 0/360 boundary and contrast intervals reported beyond
+/-180 degrees are handled without special-casing. Because displacement
is only interpreted when the printed amplitude guardrail certifies it,
displacement coverage is also reported conditional on certification
under the shipped decision rule `a_lci / (a_uci - a_lci) >= 0.35` (the
rule the printed
[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md)
output applies): the amplitude CI's lower bound must sit at least 0.35
CI widths above zero. The rule is scale-free (invariant to the score
metric) and print-independent, so no scale-dependent threshold is
reported; the 0.35 constant is calibrated for the default 95% confidence
interval. A contrast row is a signed difference, not a prototypicality
measure, so `print.circumplex_ssm()` never certification-gates it; its
displacement verdict and printed coverage are therefore reported
unconditionally (matching that profiles-only stance). Its
certification-conditional coverage – where "certified" means both
profile rows were certified – is still computed and retained in the
returned object as a descriptive that no display consumes.

The `amplitude_factors` ladder manufactures populations whose
closed-form amplitude is scaled toward zero (the regime where percentile
amplitude intervals are theoretically weakest) while keeping the
residual profile content fixed. The ladder is defined through the
estimator functional (a 3x3 solve on the images of 1, cos, and sin), so
the condition-`c` truths are exact for any angle spacing: elevation is
unchanged and the amplitude is exactly `c` times the estimate. Truths
are nevertheless recomputed from each condition's population profile. At
`c = 0` amplitude coverage is structurally zero (a percentile interval
of strictly positive amplitude replicates cannot contain 0; such rows
are flagged in the `Structural` column) and displacement truth is
undefined (reported `NA`); the guardrail certification rate carries the
inferential weight there, and the informative rungs for amplitude
coverage are the small `c > 0` ones.

When a profile row's amplitude estimate is itself below half its
observed CI width, the relative ladder degenerates: the analysis already
sits in the near-zero regime. One absolute rung is then added at the
certification margin (`c` chosen so `c` times the amplitude estimate
equals the observed amplitude-CI half-width, the largest such `c` across
affected rows) and [`summary()`](https://rdrr.io/r/base/summary.html)
notes the regime. On the correlation path this rung is dropped with a
warning if it would push a population correlation to +/-1.

## Reproducibility

This function is stochastic: call
[`set.seed()`](https://rdrr.io/r/base/Random.html) immediately before
it. It draws one [`sample.int()`](https://rdrr.io/r/base/sample.html)
value from the caller's random number stream to seed an internal
L'Ecuyer-CMRG stream, gives every simulated dataset its own
deterministic substream, and then restores the caller's `.Random.seed`
and generator kind on exit, so results for a given seed are identical
regardless of `parallel`/`ncpus` and the caller's stream is advanced by
exactly that one draw.

## Limitations

Coverage is evaluated at the fitted structure, not the unknown truth
("would the procedure work in a population like your estimates", not
"did your interval cover"). Simulated populations are multivariate
normal with the fitted correlation structure; heavy tails or skew in the
real data can degrade coverage further than reported. The diagnostic
assesses the complete-data procedure (missing data are not simulated),
and groups are assumed to share one circumplex structure. When the
Browne model fits poorly, the simulated population may misrepresent the
data; the embedded fit and its diagnostics are returned for inspection.

For an occasions (repeated-measures) analysis the population is instead
a multivariate normal with the *observed* stacked cross-occasion
covariance (no circular-model idealization): the within-person
dependence across occasions is carried directly, and no
`structure`/`cpm` alternative is offered. When the stacked covariance is
rank-deficient (per-group sample size at or below the number of
occasions times scales) the simulated population is a proper degenerate
normal, so the reported coverage and width remain valid but the
fit-statistic pass rate is descriptive only. For a paired contrast, the
joint-certification rate (both occasions certified) is reported as a
descriptive caveat on the contrast's interpretability. Assessing the
occasions object per occasion via `scales =` one occasion's columns
instead uses the default `"cpm"` structure and can give slightly
different per-occasion verdicts – a structure-sensitivity fact, not a
bug.

## References

Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in
interpersonal construct validation: Methodological advances in the
circumplex Structural Summary Approach. *Assessment, 24*(1), 3-23.

Browne, M. W. (1992). Circumplex models for correlation matrices.
*Psychometrika, 57*(4), 469-497.

Bradley, J. V. (1978). Robustness? *British Journal of Mathematical and
Statistical Psychology, 31*(2), 144-152.

## See also

Other ssm functions:
[`plot.circumplex_ci_accuracy()`](http://circumplex.jmgirard.com/reference/plot.circumplex_ci_accuracy.md),
[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md),
[`ssm_analyze_long()`](http://circumplex.jmgirard.com/reference/ssm_analyze_long.md),
[`ssm_draws()`](http://circumplex.jmgirard.com/reference/ssm_draws.md),
[`ssm_parameters()`](http://circumplex.jmgirard.com/reference/ssm_parameters.md),
[`ssm_parameters_id()`](http://circumplex.jmgirard.com/reference/ssm_parameters_id.md),
[`ssm_score()`](http://circumplex.jmgirard.com/reference/ssm_score.md),
[`ssm_sem()`](http://circumplex.jmgirard.com/reference/ssm_sem.md),
[`ssm_sem_parameters()`](http://circumplex.jmgirard.com/reference/ssm_sem_parameters.md),
[`ssm_table()`](http://circumplex.jmgirard.com/reference/ssm_table.md),
[`summary.circumplex_ssm_id()`](http://circumplex.jmgirard.com/reference/summary.circumplex_ssm_id.md)

Other analysis functions:
[`cpm_fit()`](http://circumplex.jmgirard.com/reference/cpm_fit.md),
[`cpm_simulate()`](http://circumplex.jmgirard.com/reference/cpm_simulate.md),
[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md),
[`ssm_analyze_long()`](http://circumplex.jmgirard.com/reference/ssm_analyze_long.md),
[`ssm_draws()`](http://circumplex.jmgirard.com/reference/ssm_draws.md),
[`ssm_parameters()`](http://circumplex.jmgirard.com/reference/ssm_parameters.md),
[`ssm_parameters_id()`](http://circumplex.jmgirard.com/reference/ssm_parameters_id.md),
[`ssm_score()`](http://circumplex.jmgirard.com/reference/ssm_score.md),
[`ssm_sem()`](http://circumplex.jmgirard.com/reference/ssm_sem.md),
[`ssm_sem_parameters()`](http://circumplex.jmgirard.com/reference/ssm_sem_parameters.md),
[`summary.circumplex_ssm_id()`](http://circumplex.jmgirard.com/reference/summary.circumplex_ssm_id.md)

## Examples

``` r
# \donttest{
data("jz2017")
set.seed(12345)
res <- ssm_analyze(
  jz2017[1:200, ],
  scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
  boots = 100
)
# Small reps/boots keep the example fast; use the defaults in practice
set.seed(23456)
acc <- ssm_ci_accuracy(res, reps = 25, amplitude_factors = c(1, 0.25))
#> Warning: CPM Hessian is ill-conditioned (condition number 3.53e+17): angles may be clustered or parameters weakly determined.
acc
#> 
#> SSM CI accuracy, simulated at your n and settings (25 replications per condition; bootstrap intervals with 100 replicates at level 0.95)
#> 
#>   # Profile [All] (n = 200; 95% bootstrap CIs, 100 replicates):
#>     Elevation      coverage 92.0% -- borderline
#>     Amplitude      coverage 88.0% -- borderline
#>     Displacement   coverage 92.0% when certified -- borderline
#>   Verdict: BORDERLINE -- elevation, amplitude, and certified displacement
#>   coverage rates are borderline at this number of replications; a larger
#>   `reps` would sharpen the verdict.
summary(acc)
#> 
#> Statistical Basis:    Mean Scores 
#> Assessed Engine:  bootstrap with 100 replicates 
#> Confidence Level:     0.95 
#> Simulation Reps:  25 per condition 
#> Amplitude Ladder:     1 0.25 
#> Population Structure:     Browne circular model (CPM) 
#> Group Sizes:      All = 200 
#> Certification Rule:   a_lci / (a_uci - a_lci) >= 0.35 (scale-free, print-independent) 
#> Elapsed:      0.3 s
#> 
#> Structure note: population simulated from a Browne circular model fit (m = 3,
#> RMSEA = 0.049, SRMR = 0.035).
#>   The structure fits adequately (RMSEA <= 0.08, Browne & Cudeck, 1993; SRMR
#>   <= 0.08, Hu & Bentler, 1999), so the simulated population is a reasonable
#>   stand-in for yours.
#>   Boundary markers: Heywood communality; small correlation-function weight;
#>   ill-conditioned Hessian.
#> 
#> CI trustworthiness at the as-estimated condition (c = 1), classified
#> against Bradley's (1978) liberal band via 95% Wilson intervals:
#> 
#>   # Profile [All] (n = 200; 95% bootstrap CIs, 100 replicates):
#>     Elevation      coverage 92.0% -- borderline
#>     Amplitude      coverage 88.0% -- borderline
#>     Displacement   coverage 92.0% when certified -- borderline
#>   Verdict: BORDERLINE -- elevation, amplitude, and certified displacement
#>   coverage rates are borderline at this number of replications; a larger
#>   `reps` would sharpen the verdict.
#> 
#> Coverage by profile, parameter, and amplitude condition:
#>  Profile Parameter Condition Coverage MC_se Left_miss Right_miss Median_width
#>      All         e      1.00     0.92 0.054      0.04       0.04        0.138
#>      All         x      1.00     0.92 0.054      0.08       0.00        0.109
#>      All         y      1.00     1.00 0.000      0.00       0.00        0.111
#>      All         a      1.00     0.88 0.065      0.12       0.00        0.116
#>      All         d      1.00     0.92 0.054      0.00       0.08       15.104
#>      All         e      0.25     0.96 0.039      0.04       0.00        0.139
#>      All         x      0.25     0.96 0.039      0.00       0.04        0.105
#>      All         y      0.25     0.92 0.054      0.04       0.04        0.108
#>      All         a      0.25     1.00 0.000      0.00       0.00        0.108
#>      All         d      0.25     0.96 0.039      0.04       0.00       58.050
#>  Coverage_conditional N_conditional Structural N_reps
#>                    NA            NA      FALSE     25
#>                    NA            NA      FALSE     25
#>                    NA            NA      FALSE     25
#>                    NA            NA      FALSE     25
#>                 0.920            25      FALSE     25
#>                    NA            NA      FALSE     25
#>                    NA            NA      FALSE     25
#>                    NA            NA      FALSE     25
#>                    NA            NA      FALSE     25
#>                 0.957            23      FALSE     25
#> 
#> Guardrail operating characteristics:
#>  Profile Condition Cert_rate Cert_lci Cert_uci Benchmark Caution Fit_pass_rate
#>      All      1.00      1.00    0.867    1.000     0.025      NA             1
#>      All      0.25      0.92    0.750    0.978     0.025      NA             0
#>  Branch_pathology_rate N_reps
#>                      0     25
#>                      0     25
# }
```
