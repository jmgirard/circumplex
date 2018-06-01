# ssm

The goal of ssm is to provide a powerful, flexible, and user-friendly way to analyze and visualize circumplex data using the Structural Summary Method with bootstrapped confidence intervals. 

## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jmgirard/ssm")
```
## Examples

Calculate mean-based SSM parameters for a single group/sample

``` r
data('wright2009')
data('octants')
ssm_profiles(.data = wright2009, scales = PA:NO, angles = octants)
```

Calculate mean-based SSM parameters for multiple groups/samples

``` r
data('zimmermann2017')
data('octants')
data('Norms_IIPSC_Hopwood2008')
zimmermann2017s <- ssm_standardize(.data = zimmermann2017, scales = PA:NO, angles = octants, norms = Norms_IIPSC_Hopwood2008)
ssm_profiles(.data = zimmermann2017s, scales = PA:NO, angles = octants, grouping = Gender)
```

Calculate correlation-based SSM parameters for multiple measures

``` r
data('zimmermann2017')
data('octants')
ssm_measures(.data = zimmermann2017, scales = PA:NO, angles = octants, measures = PARPD:OCPD)
```