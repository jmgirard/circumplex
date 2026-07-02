# Calculate SSM parameters by row and add results as new columns

Calculate the SSM parameters for each row of a data frame and add the
results as additional columns. This can be useful when the SSM is being
used for the description or visualization of individual data points
rather than for statistical inference on groups of data points.

## Usage

``` r
ssm_score(data, scales, angles = octants(), append = TRUE, ...)
```

## Arguments

- data:

  Required. A data frame or matrix containing at least circumplex
  scales.

- scales:

  Required. The variable names or column numbers for the variables in
  `.data` that contain circumplex scales to be analyzed.

- angles:

  Required. A numeric vector containing the angular displacement of each
  circumplex scale included in `scales` (in degrees).

- append:

  Optional. A logical indicating whether to append the output to `data`
  or simply return the output (default = "TRUE").

- ...:

  Optional. Additional parameters to pass to
  [`ssm_parameters()`](http://circumplex.jmgirard.com/reference/ssm_parameters.md),
  such as `prefix` and `suffix`.

## Value

A data frame containing `.data` plus six additional columns containing
the SSM parameters (calculated rowwise).

## See also

Other ssm functions:
[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md),
[`ssm_parameters()`](http://circumplex.jmgirard.com/reference/ssm_parameters.md),
[`ssm_table()`](http://circumplex.jmgirard.com/reference/ssm_table.md)

Other analysis functions:
[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md),
[`ssm_parameters()`](http://circumplex.jmgirard.com/reference/ssm_parameters.md)

## Examples

``` r
data("aw2009")
ssm_score(
  aw2009,
  scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
)
#>      PA    BC    DE    FG    HI    JK   LM   NO    Elev      Xval       Yval
#> 1 -1.09 -1.04 -0.97  0.61  1.41  2.49 1.78 0.27  0.4325 1.2514177 -1.3091258
#> 2  1.13 -1.04 -0.97 -0.79 -0.56  0.79 1.78 1.52  0.2325 1.4193555  0.5073528
#> 3  0.91 -0.65 -0.80 -0.96 -0.23 -0.34 1.24 0.27 -0.0700 0.7822361  0.4476346
#> 4  0.47 -0.45 -0.29  0.26  1.57  1.36 1.60 0.48  0.6250 0.8313567 -0.5560749
#> 5  0.45  0.32  0.43  0.96  1.25  1.41 1.49 0.85  0.8950 0.4382412 -0.4121320
#>        Ampl      Disp       Fit
#> 1 1.8110374 313.70892 0.9706769
#> 2 1.5073079  19.66958 0.9172710
#> 3 0.9012602  29.78035 0.7137698
#> 4 1.0001866 326.22237 0.8784837
#> 5 0.6015880 316.75861 0.9674101
```
