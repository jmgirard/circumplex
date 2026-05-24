# Score circumplex scales from item responses

Calculate mean scores on circumplex scales from item responses by using
a set of scoring instructions, which may be loaded from the package or
created as a custom data frame.

## Usage

``` r
score(
  data,
  items,
  instrument,
  na.rm = TRUE,
  prefix = "",
  suffix = "",
  append = TRUE
)
```

## Arguments

- data:

  Required. A data frame containing at least circumplex scales.

- items:

  Required. The variable names or column numbers for the variables in
  `.data` that contain all the circumplex items from a single circumplex
  measure, in ascending order from item 1 to item N.

- instrument:

  Required. An instrument object from the package. To see the available
  circumplex instruments, use
  [`instruments()`](http://circumplex.jmgirard.com/dev/reference/instruments.md).

- na.rm:

  Optional. A logical that determines if missing values should be
  omitted from the calculation of scores (default = TRUE). When set to
  TRUE, scales with missing data are essentially calculated with mean
  imputation.

- prefix:

  Optional. A string to include at the beginning of the newly calculated
  scale variables' names, before `Abbrev` from `key` and `suffix`
  (default = "").

- suffix:

  Optional. A string to include at the end of the newly calculated scale
  variables' names, after `Abbrev` from `key` and `prefix` (default =
  "").

- append:

  Optional. A logical that determines whether the calculated score
  variables will be appended to `data` or returned on their own (default
  = TRUE).

## Value

A data frame that matches `.data` except that new variables are appended
that contain mean scores on each variable included in `key`.

## See also

Other tidying functions:
[`ipsatize()`](http://circumplex.jmgirard.com/dev/reference/ipsatize.md),
[`norm_standardize()`](http://circumplex.jmgirard.com/dev/reference/norm_standardize.md),
[`self_standardize()`](http://circumplex.jmgirard.com/dev/reference/self_standardize.md)

## Examples

``` r
data("raw_iipsc")
score(raw_iipsc, items = 1:32, instrument = iipsc, prefix = "IIPSC_")
#>    IIP01 IIP02 IIP03 IIP04 IIP05 IIP06 IIP07 IIP08 IIP09 IIP10 IIP11 IIP12
#> 1      0     0     0     0     1     0     1     0     2     1     0     0
#> 2      1     1     0     0     3     2     2     1     0     1     0     1
#> 3      1     0     1     0     1     1     1     3     0     1     0     0
#> 4      3     2     3    NA     2     3     2     3     2     3     2     4
#> 5      0     0     0     1     0     0     1     1     0     1     0     2
#> 6      0     0     0     0     0     0     1     1     0     0     0     0
#> 7      1     0     0     0     2     1     1     0     1     0     0     0
#> 8      1     0     1     0     1     1     2     1     1     0     0     0
#> 9      0     0     2     2     0     1     3     0     1     0     1     1
#> 10     0     0     0     0     0     0     2     0     0     0     0     0
#>    IIP13 IIP14 IIP15 IIP16 IIP17 IIP18 IIP19 IIP20 IIP21 IIP22 IIP23 IIP24
#> 1      0     1     4     3     2     4     2     0     1     0     0     0
#> 2      4     3     3     1     0     0     1     0     1     2     0     0
#> 3      2     3     3     2     2     1     1     0     3     2     3     1
#> 4      2     1     2     3     1     2     2     1     3     2     3     2
#> 5      1     1     3     1     0     1     0     1     1     0     1     1
#> 6      0     0     2     1     1     0     0     0     0     0     1     1
#> 7      1     1     1     0     1     0     0     0     0     1     1     1
#> 8      1    NA     2     1     1     0     1     0     0     0     1     1
#> 9      0     2     2     2     1     2     2     0     0     0     3     0
#> 10     0     2     2     1     0     0     0     0     0     0     0     0
#>    IIP25 IIP26 IIP27 IIP28 IIP29 IIP30 IIP31 IIP32 IIPSC_PA IIPSC_BC IIPSC_DE
#> 1      3     3     3     0     0     0     1     0     1.75     2.00     1.25
#> 2      0     0     0     1     0     0     0     2     0.25     0.50     0.25
#> 3      1     1     1     0     3     2     3     2     1.00     0.75     0.75
#> 4      1     2     3     2     3     2     3     2     1.75     2.25     2.50
#> 5      2     1     0     0     0     0     0     0     0.50     0.75     0.00
#> 6      0     0     0     0     0     0     0     1     0.25     0.00     0.00
#> 7      1     0     0     0     1     1     0     0     1.00     0.00     0.00
#> 8      1     1     1     0     0     1     2     1     1.00     0.25     0.75
#> 9      1     0     1     0     0     1     3     0     0.75     0.50     1.50
#> 10     0     0     0     0     0     0     0     0     0.00     0.00     0.00
#>    IIPSC_FG IIPSC_HI  IIPSC_JK IIPSC_LM IIPSC_NO
#> 1  0.000000     0.50 0.2500000     1.50     0.75
#> 2  0.500000     2.00 1.7500000     1.25     1.00
#> 3  0.000000     2.25 2.0000000     2.50     2.00
#> 4  2.333333     2.50 2.0000000     2.50     2.50
#> 5  1.000000     0.50 0.2500000     1.25     0.75
#> 6  0.000000     0.00 0.0000000     1.00     1.00
#> 7  0.000000     1.00 1.0000000     0.75     0.25
#> 8  0.000000     0.50 0.6666667     1.75     1.00
#> 9  0.750000     0.00 1.0000000     2.75     0.50
#> 10 0.000000     0.00 0.5000000     1.00     0.25
```
