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

## Details

The population is a short standardized label chosen by this package so
that samples can be compared across instruments; it is deliberately
broader than the description the original source gives. Several
instruments normed on students at a single named university, in a stated
period or region, are all labelled "American college students" here.
Consult the reference and hyperlink printed alongside it for the
source's own description of the sample before treating a normative
sample as representative of a population.

## See also

Other instrument functions:
[`anchors()`](http://circumplex.jmgirard.com/reference/anchors.md),
[`instruments()`](http://circumplex.jmgirard.com/reference/instruments.md),
[`items()`](http://circumplex.jmgirard.com/reference/items.md),
[`scales()`](http://circumplex.jmgirard.com/reference/scales.md)

## Examples

``` r
norms(csip)
#> The CSIP currently has 1 normative data set(s):
#> 1. 712 American college students
#> Boudreaux, Ozer, Oltmanns, & Wright (2018)
#> <https://doi.org/10.1037/pas0000505>
```
