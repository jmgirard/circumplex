# Two-letter abbreviations for octant circumplex scales

Return a vector of abbreviations for octant circumplex scales, from PA
to NO.

## Usage

``` r
PANO(case = "upper")
```

## Arguments

- case:

  An optional string the determines whether the abbreviations should be
  in uppercase or lowercase. (default = "upper")

## Value

A character vector with eight elements, each corresponding to the
abbreviation of an octant subscale: PA, BC, DE, FG, HI, JK, LM, NO.

## Examples

``` r
PANO()
#> [1] "PA" "BC" "DE" "FG" "HI" "JK" "LM" "NO"
PANO(case = "lower")
#> [1] "pa" "bc" "de" "fg" "hi" "jk" "lm" "no"
```
