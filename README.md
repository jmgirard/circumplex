# circumplex

[![Build Status](https://travis-ci.org/jmgirard/circumplex.svg?branch=master)](https://travis-ci.org/jmgirard/circumplex) [![Coverage status](https://codecov.io/gh/jmgirard/circumplex/branch/master/graph/badge.svg)](https://codecov.io/github/jmgirard/circumplex?branch=master)

## Overview

The goal of *circumplex* is to provide a powerful, flexible, and user-friendly way to analyze and visualize circumplex data. This package has been authored by [Jeffrey Girard](http://jmgirard.com/), [Johannes Zimmermann](https://psychologische-hochschule.de/prof-dr-johannes-zimmermann/), and [Aidan Wright](http://personalityprocesses.com/). It is maintained by [Jeffrey Girard](http://jmgirard.com/).

## References

Girard, J. M., Zimmermann, J., & Wright, A. G. C. (2018). New tools for circumplex data analysis and visualization in R. _Meeting of the Society for Interpersonal Theory and Research._ Montreal, Canada.

Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in interpersonal construct validation: Methodological advances in the circumplex Structural Summary Approach. _Assessment, 24_(1), 3–23.

Wright, A. G. C., Pincus, A. L., Conroy, D. E., & Hilsenroth, M. J. (2009). Integrating methods to optimize circumplex description and comparison of groups. _Journal of Personality Assessment, 91_(4), 311–322.

## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jmgirard/circumplex")
```

## Examples of the Structural Summary Method (SSM)

Calculate mean-based SSM parameters for a single group/sample

``` r
data("aw2009")
results <- ssm_analzye(aw2009, PA:NO, octants())
ssm_table(results)
ssm_plot(results)
```

Calculate mean-based SSM parameters for multiple groups/samples

``` r
data("jz2017")
jz2017s <- standardize(jz2017, PA:NO)
results <- ssm_analyze(jz2017s, PA:NO, octants(), grouping = Gender)
ssm_table(results)
ssm_plot(results)
```

Calculate correlation-based SSM parameters for multiple measures

``` r
data("jz2017")
results <- ssm_analyze(jz2017, PA:NO, octants(), measures = c(NARPD, ASPD))
ssm_table(results)
ssm_plot(results)
```

Constrast correlation-based SSM parameters for two measures

``` r
data("jz2017")
results <- ssm_analyze(jz2017, PA:NO, octants(), measures = c(NARPD, ASPD),
  contrasts = "test")
ssm_table(results)
ssm_plot(results)
```

## Examples of verbose and abbreviated syntax

Verbose call with variable names

``` r
data("jz2017")
jz2017s <- standardize(
  .data = jz2017,
  .vars = c(PA, BC, DE, FG, HI, JK, LM, NO)
)
ssm_analyze(
  .data = jz2017s,
  scales = c(PA, BC, DE, FG, HI, JK, LM, NO),
  angles = c(90, 135, 180, 225, 270, 315, 360, 45),
  grouping = Gender,
  contrasts = "none",
  boots = 2000,
  interval = 0.95
)
```

Verbose call with column numbers

``` r
data("jz2017")
jz2017s <- standardize(
  .data = jz2017,
  .vars = c(2, 3, 4, 5, 6, 7, 8, 9)
)
ssm_analyze(
  .data = jz2017s,
  scales = c(2, 3, 4, 5, 6, 7, 8, 9),
  angles = c(90, 135, 180, 225, 270, 315, 360, 45),
  grouping = 1,
  contrasts = "none",
  boots = 2000,
  interval = 0.95
)
```

Abbreviated call with variable names and defaults

``` r
data("jz2017")
jz2017s <- standardize(jz2017, PA:NO)
ssm_analyze(jz2017s, PA:NO, octants(), grouping = Gender)
```

Abbreviated call with column numbers and defaults

``` r
data("jz2017")
jz2017s <- standardize(jz2017, 2:9)
ssm_analyze(jz2017s, 2:9, octants(), grouping = 1)
```
