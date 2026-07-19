# Evaluate circumplex structure (Acton & Revelle, 2004)

Run the exploratory circumplex-structure criteria of Acton and Revelle
(2004) on a set of scales and return one object bundling all of the
tests. Four criteria are computed from the first two unrotated
principal-axis factors of the scales' correlation matrix – the **Fisher
Test** of equal axes, the **Gap Test** of equal spacing, the **Variance
Test** (VT2) and **Rotation Test** of interstitiality – and each
statistic is classified against simulation-derived, scoring- and
scale-count-specific cutoffs. A fifth test, **RANDALL** (Hubert &
Arabie, 1987; Tracey, 1997), evaluates the hypothesised circular *order*
of the scales with a randomization test that yields a genuine p-value.

## Usage

``` r
fit_structure(
  data,
  scales,
  scoring = c("deviation", "raw"),
  ridge = 0,
  n_perm = NULL,
  listwise = TRUE
)
```

## Arguments

- data:

  A data frame (or matrix) containing the circumplex scales.

- scales:

  A character vector of column names (or a numeric vector of column
  indexes) selecting the circumplex scales, **in hypothesised circular
  order** (the order is RANDALL's order hypothesis). At least four
  scales are required.

- scoring:

  Either `"deviation"` (the default; row-mean-center the scales before
  analysis) or `"raw"` (analyze the scores as given). Selects the
  matching interpretive cutoffs.

- ridge:

  A non-negative ridge added to the diagonal of the correlation matrix
  (then rescaled to a unit diagonal) to repair a non-positive-definite
  matrix before factoring; default `0`, which matches the cutoff
  calibration. Raise it only if factoring fails, noting that a nonzero
  ridge moves the statistics off the calibrated scale.

- n_perm:

  `NULL` (the default) to compute RANDALL's p-value by exact
  enumeration, available for up to nine scales; otherwise a single
  positive whole number of Monte Carlo relabelings (required for ten or
  more scales). The Monte Carlo path draws from the global RNG stream,
  so set a seed with [`set.seed()`](https://rdrr.io/r/base/Random.html)
  beforehand for reproducibility. Ten or more scales require `n_perm`
  (exact enumeration is infeasible); supplying it is validated up front,
  before any criteria are computed.

- listwise:

  A logical indicating whether missing values are handled by listwise
  deletion (`TRUE`, the default) or pairwise deletion (`FALSE`),
  matching
  [`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md).
  Listwise deletion gives all five tests one complete-case correlation
  matrix, which is the metric the interpretive cutoffs were calibrated
  on; pairwise deletion can yield a non-positive- definite matrix and
  moves the statistics off that calibrated scale.

## Value

An object of class `circumplex_structure` with
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html), and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods. Its
components are `results` (a data frame with one row per factor-analytic
criterion: statistic, cutoffs, and interpretive category), `randall`
(the RANDALL index, p-value, and method), `loadings` (the two unrotated
principal-axis factors), and `details`.

## Details

The four factor-analytic criteria have the most power when there is no
large general factor, which *deviation scoring* (centering each
respondent on their own mean across the scales, exactly what
[`ipsatize()`](http://circumplex.jmgirard.com/reference/ipsatize.md)
does) approximates by removing it (Acton & Revelle, 2004, p. 9).
Deviation scoring is therefore the default and is applied to all five
tests; pass `scoring = "raw"` to leave the scores untouched. The two
scorings carry different cutoffs, matched automatically.

The interpretive cutoffs are **heuristic likelihood classifications read
off simulated distributions, not significance tests**, and they are
specific to the number of scales. Only eight scales (the canonical
octant instrument) are calibrated; with any other count the statistics
are still reported but no interpretation is attached (see
[`print()`](https://rdrr.io/r/base/print.html)/[`summary()`](https://rdrr.io/r/base/summary.html)).
The cutoffs were re-derived under Acton and Revelle's own generating
model at eight scales; see
[`vignette("evaluating-circumplex-structure")`](http://circumplex.jmgirard.com/articles/evaluating-circumplex-structure.md).
RANDALL needs no cutoffs: with up to nine scales its null distribution
is enumerated exactly, so its p-value is available at any scale count of
four or more.

## References

Acton, G. S., & Revelle, W. (2004). Evaluation of ten psychometric
criteria for circumplex structure. *Methods of Psychological Research
Online*, 9(1), 1-27.

Hubert, L., & Arabie, P. (1987). Evaluating order hypotheses within
proximity matrices. *Psychological Bulletin*, 102(1), 172-178.

Tracey, T. J. G. (1997). RANDALL: A Microsoft FORTRAN program for a
randomization test of hypothesized order relations. *Educational and
Psychological Measurement*, 57(1), 164-168.

## See also

[`cpm_fit()`](http://circumplex.jmgirard.com/reference/cpm_fit.md) for a
confirmatory circumplex model;
[`ipsatize()`](http://circumplex.jmgirard.com/reference/ipsatize.md) for
deviation scoring.

## Examples

``` r
data("jz2017")
scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
res <- fit_structure(jz2017, scales = scales)
res
#> 
#> Circumplex Structure Tests (Acton & Revelle, 2004)
#> Scales (nv):  8
#> Scoring:      deviation (row-mean centered)
#> 
#> # Exploratory criteria
#> 
#>  Test     Statistic Interpretation                                           
#>  Fisher   0.102     equal axes: at least 3x as likely as the alternative     
#>  Gap      0.152     equal spacing: at least 3x as likely as the alternative  
#>  Variance 0.180     interstitiality: almost certain                          
#>  Rotation 0.325     interstitiality: at least 3x as likely as the alternative
#> 
#> # Order hypothesis (RANDALL)
#> 
#>   Correspondence index = 0.868, p = 0.000397 (exact, 5040 relabelings)
#> 
#>   Interpretations are heuristic likelihood classifications from simulation, not
#>   significance tests (Acton & Revelle, 2004). RANDALL's p-value is exact.
summary(res)
#> 
#> Circumplex Structure Tests (Acton & Revelle, 2004)
#> Scales (nv):  8
#> Scoring:      deviation (row-mean centered)
#> Ridge:        0
#> 
#> # Exploratory criteria
#> 
#>  Test     Statistic Almost Thrice Twice Verdict       
#>  Fisher   0.102     0.07   0.12   0.15  3x+ likely    
#>  Gap      0.152     0.15   0.40   0.46  3x+ likely    
#>  Variance 0.180     0.19   0.59   0.64  almost certain
#>  Rotation 0.325     0.32   0.64   0.67  3x+ likely    
#> 
#> # Estimated scale geometry
#> 
#>  Scale Angle   Communality
#>  PA    339.635 0.642      
#>  BC    359.858 0.571      
#>  DE     48.584 0.500      
#>  FG     81.337 0.522      
#>  HI    161.662 0.690      
#>  JK    183.339 0.713      
#>  LM    215.503 0.388      
#>  NO    287.119 0.474      
#> 
#> # Order hypothesis (RANDALL)
#> 
#>   Correspondence index = 0.868, p = 0.000397 (exact, 5040 relabelings)
#> 
#>   Interpretations are heuristic likelihood classifications from simulation, not
#>   significance tests (Acton & Revelle, 2004). RANDALL's p-value is exact.
#> 
```
