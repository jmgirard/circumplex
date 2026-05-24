# Standardize circumplex scales using sample data

Take in a data frame containing circumplex scales (or items) and return
that same data frame with each specified variable transformed into
standard scores (i.e., z-scores) based on observed means and SDs.

## Usage

``` r
self_standardize(
  data,
  scales,
  na.rm = TRUE,
  prefix = "",
  suffix = "_z",
  append = TRUE
)
```

## Arguments

- data:

  Required. A data frame or matrix containing at least circumplex
  scales.

- scales:

  Required. A character vector containing the column names, or a numeric
  vector containing the column indexes, for the variables (scale scores)
  to be standardized.

- na.rm:

  Optional. A logical that determines whether to remove missing values
  from scales when calculating the means and SDs used for
  standardization (default = TRUE).

- prefix:

  Optional. A string to include at the beginning of the newly calculated
  scale variables' names, before the scale name and `suffix` (default =
  "").

- suffix:

  Optional. A string to include at the end of the newly calculated scale
  variables' names, after the scale name and `prefix` (default = "\_z").

- append:

  Optional. A logical that determines whether the calculated
  standardized scores should be added as columns to `data` in the output
  or the standardized scores alone should be output (default = TRUE).

## Value

A data frame that contains the self-standardized versions of `scales`.

## See also

Other tidying functions:
[`ipsatize()`](http://circumplex.jmgirard.com/dev/reference/ipsatize.md),
[`norm_standardize()`](http://circumplex.jmgirard.com/dev/reference/norm_standardize.md),
[`score()`](http://circumplex.jmgirard.com/dev/reference/score.md)

## Examples

``` r
self_standardize(aw2009, scales = 1:8)
#>      PA    BC    DE    FG    HI    JK   LM   NO        PA_z       BC_z
#> 1 -1.09 -1.04 -0.97  0.61  1.41  2.49 1.78 0.27 -1.68567863 -0.8356210
#> 2  1.13 -1.04 -0.97 -0.79 -0.56  0.79 1.78 1.52  0.87047339 -0.8356210
#> 3  0.91 -0.65 -0.80 -0.96 -0.23 -0.34 1.24 0.27  0.61716103 -0.1392702
#> 4  0.47 -0.45 -0.29  0.26  1.57  1.36 1.60 0.48  0.11053630  0.2178328
#> 5  0.45  0.32  0.43  0.96  1.25  1.41 1.49 0.85  0.08750791  1.5926794
#>         DE_z       FG_z       HI_z       JK_z        LM_z       NO_z
#> 1 -0.7503127  0.6969306  0.7206249  1.3067256  0.89429570 -0.7742758
#> 2 -0.7503127 -0.9456668 -1.2456231 -0.3412221  0.89429570  1.5978927
#> 3 -0.4668612 -1.1451251 -0.9162516 -1.4366226 -1.49639577 -0.7742758
#> 4  0.3834932  0.2862813  0.8803202  0.2113251  0.09739854 -0.3757515
#> 5  1.5839935  1.1075800  0.5609296  0.2597941 -0.38959417  0.3264104
```
