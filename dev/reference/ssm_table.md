# Create HTML table from SSM results or contrasts

Take in the results of an SSM analysis and return an HTML table with the
desired formatting.

## Usage

``` r
ssm_table(ssm_object, caption = NULL, drop_xy = FALSE, render = TRUE)
```

## Arguments

- ssm_object:

  Required. The results output of
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md).

- caption:

  A string to be displayed above the table (default = NULL).

- drop_xy:

  A logical indicating whether the x-value and y-value parameters should
  be omitted from the output (default = FALSE).

- render:

  A logical indicating whether the table should be displayed in the
  RStudio viewer or web browser (default = TRUE).

## Value

A data frame containing the information for the HTML table. As a
side-effect, may also output the HTML table to the web viewer.

## See also

Other ssm functions:
[`plot.circumplex_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/plot.circumplex_ci_accuracy.md),
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md),
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md),
[`ssm_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters.md),
[`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md)

Other table functions:
[`html_render()`](http://circumplex.jmgirard.com/dev/reference/html_render.md)

## Examples

``` r
# \donttest{
# Load example data
data("jz2017")

# Create table of profile results
res <- ssm_analyze(
  jz2017,
  scales = 2:9,
  measures = c("NARPD", "ASPD")
)
ssm_table(res)
#> <table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;' >
#> <thead>
#> <tr><td colspan='7' style='text-align: left;'>
#> Correlation-based Structural Summary Statistics with 95% CIs</td></tr>
#> <tr>
#> <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>Profile</th>
#> <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>Elevation</th>
#> <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>X.Value</th>
#> <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>Y.Value</th>
#> <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>Amplitude</th>
#> <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>Displacement</th>
#> <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>Fit</th>
#> </tr>
#> </thead>
#> <tbody>
#> <tr>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>NARPD</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>0.20 (0.17, 0.24)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>-0.06 (-0.10, -0.03)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>0.18 (0.14, 0.21)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>0.19 (0.16, 0.23)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>109.0 (99.0, 119.1)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>0.957</td>
#> </tr>
#> <tr>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>ASPD</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>0.12 (0.09, 0.16)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>-0.10 (-0.13, -0.06)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>0.20 (0.17, 0.24)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>0.23 (0.19, 0.26)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>115.9 (107.5, 124.5)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>0.964</td>
#> </tr>
#> </tbody>
#> </table>
#>   Profile         Elevation              X.Value           Y.Value
#> 1   NARPD 0.20 (0.17, 0.24) -0.06 (-0.10, -0.03) 0.18 (0.14, 0.21)
#> 2    ASPD 0.12 (0.09, 0.16) -0.10 (-0.13, -0.06) 0.20 (0.17, 0.24)
#>           Amplitude         Displacement   Fit
#> 1 0.19 (0.16, 0.23)  109.0 (99.0, 119.1) 0.957
#> 2 0.23 (0.19, 0.26) 115.9 (107.5, 124.5) 0.964

# Create table of contrast results
res <- ssm_analyze(
  jz2017,
  scales = 2:9,
  measures = c("NARPD", "ASPD"), 
  contrast = TRUE
)
ssm_table(res)
#> <table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;' >
#> <thead>
#> <tr><td colspan='7' style='text-align: left;'>
#> Correlation-based Structural Summary Statistic Contrasts with 95% CIs</td></tr>
#> <tr>
#> <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>Contrast</th>
#> <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>Elevation</th>
#> <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>X.Value</th>
#> <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>Y.Value</th>
#> <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>Amplitude</th>
#> <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>Displacement</th>
#> <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>Fit</th>
#> </tr>
#> </thead>
#> <tbody>
#> <tr>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>NARPD</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>0.20 (0.17, 0.24)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>-0.06 (-0.10, -0.03)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>0.18 (0.14, 0.21)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>0.19 (0.16, 0.22)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>109.0 (99.0, 118.6)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>0.957</td>
#> </tr>
#> <tr>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>ASPD</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>0.12 (0.09, 0.16)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>-0.10 (-0.13, -0.06)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>0.20 (0.17, 0.24)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>0.23 (0.19, 0.26)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>115.9 (107.3, 124.2)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; text-align: left;'>0.964</td>
#> </tr>
#> <tr>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>ASPD - NARPD</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>-0.08 (-0.12, -0.04)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>-0.04 (-0.07, 0.00)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>0.02 (-0.01, 0.06)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>0.04 (-0.00, 0.08)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>7.0 (-3.4, 17.2)</td>
#> <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>0.007</td>
#> </tr>
#> </tbody>
#> </table>
#>       Contrast            Elevation              X.Value            Y.Value
#> 1        NARPD    0.20 (0.17, 0.24) -0.06 (-0.10, -0.03)  0.18 (0.14, 0.21)
#> 2         ASPD    0.12 (0.09, 0.16) -0.10 (-0.13, -0.06)  0.20 (0.17, 0.24)
#> 3 ASPD - NARPD -0.08 (-0.12, -0.04)  -0.04 (-0.07, 0.00) 0.02 (-0.01, 0.06)
#>            Amplitude         Displacement   Fit
#> 1  0.19 (0.16, 0.22)  109.0 (99.0, 118.6) 0.957
#> 2  0.23 (0.19, 0.26) 115.9 (107.3, 124.2) 0.964
#> 3 0.04 (-0.00, 0.08)     7.0 (-3.4, 17.2) 0.007
# }
```
