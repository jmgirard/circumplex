# Draw SSM confidence-region arcs in circumplex space

A ggplot2 layer that draws, for each profile, the wedge spanning its
amplitude confidence interval (radially) and its displacement confidence
interval (angularly), on the canvas produced by
[`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md).
The amplitude/displacement-to-canvas transform – including the
wrap-around when a displacement interval crosses the 0/360 degree
boundary – is handled internally, so the bounds are supplied directly in
SSM units.

## Usage

``` r
geom_ssm_arc(
  mapping = NULL,
  data = NULL,
  stat = StatSsmArc,
  position = "identity",
  ...,
  amax = 0.5,
  n = 360,
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, show.legend, inherit.aes, ...:

  Standard ggplot2 layer arguments. `mapping` must supply the
  `amplitude_min`, `amplitude_max`, `displacement_min`, and
  `displacement_max` aesthetics.

- amax:

  A single positive number giving the amplitude represented by the
  canvas's outer ring; must match the `amax` used for
  [`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md)
  (default = 0.5).

- n:

  The number of points used to draw each arc's curved edges (default =
  360).

- na.rm:

  Ignored; profiles with a missing displacement or amplitude bound
  (degenerate profiles) are always dropped.

## Value

A ggplot2 layer.

## See also

[`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md),
[`geom_ssm_point()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_point.md)

## Examples

``` r
data("jz2017")
res <- ssm_analyze(jz2017, scales = 2:9, measures = "NARPD")
amax <- 0.5
ggcircumplex(octants(), amax = amax) +
  geom_ssm_arc(
    data = res$results,
    mapping = ggplot2::aes(
      amplitude_min = a_lci, amplitude_max = a_uci,
      displacement_min = d_lci, displacement_max = d_uci
    ),
    amax = amax, alpha = 0.4
  )
```
