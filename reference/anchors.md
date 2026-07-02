# Display the anchors of a circumplex instrument

Display the anchors of a circumplex instrument including the total
number of anchors and each anchor's numerical value and text label.
Anchors are the response options that respondants select from (e.g., 0 =
No, 1 = Yes).

## Usage

``` r
anchors(x)
```

## Arguments

- x:

  Required. An object of the instrument class.

## Value

The same input object. Prints text to console.

## See also

Other instrument functions:
[`instruments()`](http://circumplex.jmgirard.com/reference/instruments.md),
[`items()`](http://circumplex.jmgirard.com/reference/items.md),
[`norms()`](http://circumplex.jmgirard.com/reference/norms.md),
[`scales()`](http://circumplex.jmgirard.com/reference/scales.md)

## Examples

``` r
anchors(csip)
#> The CSIP is rated using the following 4-point scale.
#> 0. Not a problem
#> 1. Minor problem
#> 2. Moderate problem
#> 3. Serious problem
```
