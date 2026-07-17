# Unwrap a sequence of angles onto a continuous branch

Unwrap a temporally ordered sequence of angular displacements (e.g., one
displacement per measurement wave) onto a continuous numeric branch, so
that a trajectory drifting across the 0/360 boundary becomes a smooth
sequence suitable for linear growth modeling. Each input is first
wrapped to \[0, 360) (any real numbers are accepted); the output then
starts at the first wave's wrapped value and accumulates the shortest
signed rotation between successive waves, so successive values never
differ by more than 180 degrees. For example, `c(350, 10, 30)` unwraps
to `c(350, 370, 390)`.

## Usage

``` r
angle_unwrap(x)
```

## Arguments

- x:

  A numeric vector of angles in degrees, in temporal order. Any real
  values are accepted and are wrapped to \[0, 360) first.

## Value

A plain numeric vector of the same length: the unwrapped angles in
degrees on a continuous branch anchored at the first wave's wrapped
value. Values may legitimately fall outside \[0, 360); the LM = 360
reporting convention applies to displacements, not to the unwrapped
branch (an input of 360 anchors at 0).

## Details

Two conventions are pinned. An exact 180-degree step is directionally
ambiguous; it is resolved as +180 (ascending), matching the package's
contrast convention of reporting an exact half-turn as +180. A missing
wave makes every subsequent step branch-ambiguous, so `NA` propagates
from the missing wave onward rather than silently bridging the gap.

Unwrapping assumes the sequence really does move by less than a
half-turn between successive waves; when the truth moves faster than the
sampling (or persons occupy heterogeneous locations with no common
branch), the unwrapped branch is wrong without warning. See the
package's growth modeling vignette for these failure modes and the
bivariate (x, y) alternative that avoids them.

## Examples

``` r
angle_unwrap(c(350, 10, 30))
#> [1] 350 370 390
angle_unwrap(c(10, 350, 330))
#> [1]  10 -10 -30
angle_unwrap(c(350, NA, 30))
#> [1] 350  NA  NA
```
