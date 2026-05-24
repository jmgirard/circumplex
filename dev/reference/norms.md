# Display the norms for a circumplex instrument

Display the norms for a circumplex instrument including the total number
of normative data sets available and each data set's number, sample
size, population, and source reference and hyperlink. If another
normative data set exists that is not yet included in the package,
please let us know.

## Usage

``` r
norms(x)
```

## Arguments

- x:

  Required. An object of the instrument class.

## Value

The same input object. Prints text to console.

## See also

Other instrument functions:
[`anchors()`](http://circumplex.jmgirard.com/dev/reference/anchors.md),
[`instruments()`](http://circumplex.jmgirard.com/dev/reference/instruments.md),
[`items()`](http://circumplex.jmgirard.com/dev/reference/items.md),
[`scales()`](http://circumplex.jmgirard.com/dev/reference/scales.md)

## Examples

``` r
norms(csip)
#> The CSIP currently has 1 normative data set(s):
#> 1. 712 American college students
#> Boudreaux, Ozer, Oltmanns, & Wright (2018)
#> <https://doi.org/10.1037/pas0000505>
```
