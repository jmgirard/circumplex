# Angular displacements for octant circumplex scales

Return a vector of angular displacements, in degrees, for eight equally
spaced circumplex scales corresponding to the circumplex octants. Can be
passed to the `angles` parameter of other functions in this package.

## Usage

``` r
octants()
```

## Value

A numeric vector with eight elements, each corresponding to the angular
displacement (in degrees) of a subscale, in the following order: PA, BC,
DE, FG, HI, JK, LM, NO.

## Examples

``` r
octants()
#> 90 135 180 225 270 315 360 45 
#> Degrees
```
