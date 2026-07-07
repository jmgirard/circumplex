# Plot a circumplex-structure configuration

Draw the two-factor loading configuration of a
[`fit_structure()`](http://circumplex.jmgirard.com/dev/reference/fit_structure.md)
object on the circular canvas from
[`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md).
Each scale is placed at its estimated angle (`atan2` of its two
principal-axis loadings) and at a radius given by its communality (the
share of its variance on the first two factors), so a clean circumplex
shows the scales spread evenly around a ring of similar radius, unequal
axes show scales at differing radii (what the Fisher Test measures), and
simple structure shows scales bunched near a few angles (what the Gap
and interstitiality tests measure). The canvas spokes mark the same
estimated angles, labelled by scale.

## Usage

``` r
# S3 method for class 'circumplex_structure'
plot(x, amax = 1, legend = TRUE, ...)
```

## Arguments

- x:

  A `circumplex_structure` object from
  [`fit_structure()`](http://circumplex.jmgirard.com/dev/reference/fit_structure.md).

- amax:

  A single positive number giving the communality represented by the
  canvas's outer ring (default = 1). Principal-axis communalities can
  exceed 1 in a Heywood case; when any scale's communality exceeds
  `amax` the ring is expanded to contain it, so no point is ever drawn
  outside the canvas.

- legend:

  A logical: draw a legend keying the colours to the scale names
  (default = `TRUE`).

- ...:

  Not used. Supplying an unrecognized argument produces a warning.

## Value

A ggplot2 object.

## See also

[`fit_structure()`](http://circumplex.jmgirard.com/dev/reference/fit_structure.md),
[`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md)

## Examples

``` r
data("jz2017")
scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
plot(fit_structure(jz2017, scales = scales))
```
