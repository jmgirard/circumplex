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

For most samples the label names the group they were drawn from rather
than a frame they were drawn to represent – but not for all of them, and
which is which is recorded per sample in the `Kind` column and printed
as the sample's reference kind:

- standardization sample:

  The sample was drawn to represent a defined population, so its mean
  and standard deviation estimate that population's. Only the IIP-32 and
  IIP-64 samples are of this kind.

- identified published source:

  The sample's octant statistics are printed in an identified source – a
  study report or an author's norms page – and describe that group of
  people rather than any wider frame.

- no identified source:

  The sample's octant statistics appear in no source that has been
  identified, whatever is known about the sample itself, and should be
  treated as unverified.

See
[`vignette("using-instruments")`](http://circumplex.jmgirard.com/articles/using-instruments.md)
for what the shipped reference samples are and how to choose among them.

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
#> Reference kind: identified published source
#> Boudreaux, Ozer, Oltmanns, & Wright (2018)
#> <https://doi.org/10.1037/pas0000505>
```
