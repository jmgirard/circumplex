# Simulate data from a fitted circular process model

Draw `n` standardized observations from the model-implied correlation
matrix \\\hat{P}\\ of a fitted
[`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md)
object, using the low-rank factor representation \\P =
\Lambda\Lambda^\top + (I - D\_\zeta^2)\\ (Browne, 1992; design sec.
1.3/sec. 5.4). Each observation is generated as \\x = \Lambda z + (I -
D\_\zeta^2)^{1/2}\varepsilon\\ with independent standard-normal
common-factor scores \\z\\ and unique deviates \\\varepsilon\\, so the
draws are exactly positive semidefinite by construction (no eigenvalue
clamping) and their population correlation matrix is \\\hat{P}\\
exactly.

## Usage

``` r
cpm_simulate(object, n)
```

## Arguments

- object:

  A `circumplex_cpm` object from
  [`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md).

- n:

  The number of observations (rows) to simulate; a positive whole
  number.

## Value

A numeric matrix with `n` rows and one column per fitted scale, columns
in the fitted scale order with `colnames` set to the scale names
(`rownames` are `NULL`). The population margins are zero-mean and
unit-variance, so [`cor()`](https://rdrr.io/r/stats/cor.html) of the
returned matrix converges to `object$matrices$Phat` as `n` grows.

## Details

This is the mean-based simulation path of the SSM CI-trustworthiness
diagnostic
([`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md);
the M4 Brief-B contract): a caller draws standardized data here and
rescales it to a group's means and SDs. The correlation-based (augmented
scales-plus-measures) path is **not** produced here – it reduces to the
returned population block `object$matrices$Phat`, from which the caller
assembles and repairs its own joint matrix.

## Reproducibility

`cpm_simulate()` consumes R's global random number stream (the
common-factor scores then the unique deviates, drawn in that fixed
order), so it follows the package's
[`set.seed()`](https://rdrr.io/r/base/Random.html)-immediately-before
convention: a given seed reproduces the draw exactly. It is one of the
package's stochastic entry points (alongside
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
and the bootstrap path of
[`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md));
the fit itself is deterministic.

## References

Browne, M. W. (1992). Circumplex models for correlation matrices.
*Psychometrika, 57*(4), 469-497.

## See also

[`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md)

Other analysis functions:
[`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md),
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md),
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md),
[`ssm_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters.md),
[`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md),
[`ssm_sem()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem.md),
[`ssm_sem_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_parameters.md)

## Examples

``` r
data("jz2017")
scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
fit <- cpm_fit(cormat = cor(jz2017[scales]), scales = scales,
               n = nrow(jz2017))
#> Warning: CPM Hessian is ill-conditioned (condition number 1.83e+14): angles may be clustered or parameters weakly determined.
set.seed(1)
x <- cpm_simulate(fit, n = 500)
round(cor(x) - fit$matrices$Phat, 2)
#>       PA    BC    DE    FG    HI    JK    LM    NO
#> PA  0.00 -0.03 -0.05  0.05  0.00 -0.02 -0.01 -0.07
#> BC -0.03  0.00  0.01  0.01 -0.03 -0.04 -0.03 -0.08
#> DE -0.05  0.01  0.00  0.03 -0.01 -0.01  0.03 -0.06
#> FG  0.05  0.01  0.03  0.00  0.00 -0.04 -0.03 -0.03
#> HI  0.00 -0.03 -0.01  0.00  0.00 -0.01 -0.04  0.00
#> JK -0.02 -0.04 -0.01 -0.04 -0.01  0.00 -0.02  0.01
#> LM -0.01 -0.03  0.03 -0.03 -0.04 -0.02  0.00 -0.02
#> NO -0.07 -0.08 -0.06 -0.03  0.00  0.01 -0.02  0.00
```
