
<!-- README.md is generated from README.Rmd. Please edit that file -->

# circumplex <img src="man/figures/logo.png" align="right" alt="" />

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/circumplex)](https://cran.r-project.org/package=circumplex)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Codecov test
coverage](https://codecov.io/gh/jmgirard/circumplex/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jmgirard/circumplex?branch=master)
[![R-CMD-check](https://github.com/jmgirard/circumplex/workflows/R-CMD-check/badge.svg)](https://github.com/jmgirard/circumplex/actions)
[![R-CMD-check](https://github.com/jmgirard/circumplex/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jmgirard/circumplex/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of *circumplex* is to provide a powerful, flexible, and
user-friendly way to analyze and visualize circumplex data. It was
created and is maintained by [Jeffrey Girard](https://jmgirard.com/); it
was inspired by work from and was developed under advisement from
[Johannes
Zimmermann](https://www.uni-kassel.de/fb01/institute/institut-fuer-psychologie/fachgebiete/differentielle-psychologie/prof-dr-johannes-zimmermann)
and [Aidan Wright](https://www.personalityprocesses.com/). You can learn
more about using this package through the vignette articles available on
the [package website](https://circumplex.jmgirard.com/) or through
`?circumplex`.

## Installation

``` r
# Install release version from CRAN
install.packages("circumplex")

# Install development version from GitHub
devtools::install_github("jmgirard/circumplex")
```

## Usage

``` r
data("jz2017")
results <- ssm_analyze(
  .data = jz2017, 
  scales = c(PA, BC, DE, FG, HI, JK, LM, NO), 
  angles = c(90, 135, 180, 225, 270, 315, 360, 45), 
  measures = c(NARPD, ASPD),
  measures_labels = c("Narcissistic PD", "Antisocial PD")
)
summary(results)
#> Call:
#> ssm_analyze(.data = jz2017, scales = c(PA, BC, DE, FG, HI, JK, 
#>     LM, NO), angles = c(90, 135, 180, 225, 270, 315, 360, 45), 
#>     measures = c(NARPD, ASPD), measures_labels = c("Narcissistic PD", 
#>         "Antisocial PD"))
#> 
#> Statistical Basis:    Correlation Scores 
#> Bootstrap Resamples:  2000 
#> Confidence Level:     0.95 
#> Listwise Deletion:    TRUE 
#> Scale Displacements:  90 135 180 225 270 315 360 45 
#> 
#> Profile [Narcissistic PD]:
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.202      0.169      0.238
#> X-Value          -0.062     -0.094     -0.029
#> Y-Value           0.179      0.145      0.213
#> Amplitude         0.189      0.154      0.227
#> Displacement    108.967     98.633    118.537
#> Model Fit         0.957                      
#> 
#> Profile [Antisocial PD]:
#>                Estimate   Lower CI   Upper CI
#> Elevation         0.124      0.087      0.158
#> X-Value          -0.099     -0.133     -0.064
#> Y-Value           0.203      0.170      0.239
#> Amplitude         0.226      0.191      0.264
#> Displacement    115.927    107.327    124.188
#> Model Fit         0.964
```

``` r
ssm_table(results, xy = FALSE)
```

<table class="table" style="font-size: 12px; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">
Correlation-based Structural Summary Statistics with 95% CIs
</caption>
<thead>
<tr>
<th style="text-align:left;">
Profile
</th>
<th style="text-align:left;">
Elevation
</th>
<th style="text-align:left;">
Amplitude
</th>
<th style="text-align:left;">
Displacement
</th>
<th style="text-align:left;">
Fit
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Narcissistic PD
</td>
<td style="text-align:left;">
0.20 (0.17, 0.24)
</td>
<td style="text-align:left;">
0.19 (0.15, 0.23)
</td>
<td style="text-align:left;">
109.0 (98.6, 118.5)
</td>
<td style="text-align:left;">
0.957
</td>
</tr>
<tr>
<td style="text-align:left;">
Antisocial PD
</td>
<td style="text-align:left;">
0.12 (0.09, 0.16)
</td>
<td style="text-align:left;">
0.23 (0.19, 0.26)
</td>
<td style="text-align:left;">
115.9 (107.3, 124.2)
</td>
<td style="text-align:left;">
0.964
</td>
</tr>
</tbody>
</table>

``` r
ssm_plot(results)
```

![](man/figures/README-plot-1.png)<!-- -->

## Code of Conduct

Please note that the ‘circumplex’ project is released with a
[Contributor Code of
Conduct](https://circumplex.jmgirard.com/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

## References

Girard, J. M., Zimmermann, J., & Wright, A. G. C. (2018). New tools for
circumplex data analysis and visualization in R. *Meeting of the Society
for Interpersonal Theory and Research.* Montreal, Canada.

Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in
interpersonal construct validation: Methodological advances in the
circumplex Structural Summary Approach. *Assessment, 24*(1), 3–23.

Wright, A. G. C., Pincus, A. L., Conroy, D. E., & Hilsenroth, M. J.
(2009). Integrating methods to optimize circumplex description and
comparison of groups. *Journal of Personality Assessment, 91*(4),
311–322.
