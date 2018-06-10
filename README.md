# circumplex

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
## Examples

Calculate mean-based SSM parameters for a single group/sample

``` r
data('aw2009')
ssm_profiles(.data = wright2009, scales = PA:NO, angles = octants())
```

Calculate mean-based SSM parameters for multiple groups/samples

``` r
data('jz2017')
data('iipsc')
jz2017s <- ssm_standardize(.data = jz2017, scales = PA:NO, angles = octants(), norms = iipsc)
ssm_profiles(.data = jz2017s, scales = PA:NO, angles = octants(), grouping = Gender)
```

Calculate correlation-based SSM parameters for multiple measures

``` r
data('jz2017')
ssm_measures(.data = jz2017, scales = PA:NO, angles = octants(), measures = PARPD:AVPD)
```