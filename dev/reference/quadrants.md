# Angular displacements for quadrant circumplex scales

Return a vector of angular displacements, in degrees, for four equally
spaced circumplex scales corresponding to the circumplex quadrants. Can
be passed to the `angles` parameter of other functions in this package.

## Usage

``` r
quadrants()
```

## Value

A numeric vector with eight elements, each corresponding to the angular
displacement (in degrees) of a subscale, in the following order: BC, FG,
JK, NO.

## Examples

``` r
quadrants()
#> 135 225 315 45 
#> Degrees
```
