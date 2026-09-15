# Summarize the accuracy of SSM confidence intervals

Print the full report of an
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/reference/ssm_ci_accuracy.md)
run: the assessed configuration, a structure note describing the
simulated population (with cautions when the structural model converged
badly or fits poorly – benchmarks per Browne & Cudeck, 1993, and Hu &
Bentler, 1999), the per-profile verdict blocks (coverage of elevation,
amplitude, and certification-conditional displacement classified against
Bradley's liberal band; the guardrail false-certification caution), and
a compact table with one row per profile and amplitude condition. That
table shows the coverage of each parameter, the displacement coverage
when certified, the certification rate and the `Structural` flag. On a
contrast row, "certified" means that both profiles were certified. The
Monte Carlo standard errors, the one-sided miss rates, the interval
widths, the conditional and replication counts, and the other guardrail
columns are not printed; read them from the `coverage` and `guardrail`
elements of the object.

## Usage

``` r
# S3 method for class 'circumplex_ci_accuracy'
summary(object, digits = 3, ...)
```

## Arguments

- object:

  A `circumplex_ci_accuracy` object from
  [`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/reference/ssm_ci_accuracy.md).

- digits:

  Number of digits to which table entries are rounded (default = 3).

- ...:

  Currently ignored.

## Value

The object, invisibly.

## References

Browne, M. W., & Cudeck, R. (1993). Alternative ways of assessing model
fit. In K. A. Bollen & J. S. Long (Eds.), *Testing structural equation
models* (pp. 136-162). Sage.

Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in
covariance structure analysis: Conventional criteria versus new
alternatives. *Structural Equation Modeling, 6*(1), 1-55.
