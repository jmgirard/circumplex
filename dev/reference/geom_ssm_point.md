# Draw SSM profile points in circumplex space

A ggplot2 layer that places a point for each profile at its amplitude
and displacement, on the canvas produced by
[`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md).
The amplitude/displacement-to-canvas transform is handled internally, so
the `amplitude` and `displacement` aesthetics are supplied directly in
SSM units (amplitude in the score metric, displacement in degrees).

## Usage

``` r
geom_ssm_point(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  amax = 0.5,
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, show.legend, inherit.aes, ...:

  Standard ggplot2 layer arguments. `mapping` must supply the
  `amplitude` and `displacement` aesthetics.

- amax:

  A single positive number giving the amplitude represented by the
  canvas's outer ring; must match the `amax` used for
  [`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md)
  so the points align with the amplitude gridlines (default = 0.5).

- na.rm:

  Ignored; profiles with a missing displacement or amplitude (degenerate
  profiles) are always dropped, since they have no location.

## Value

A ggplot2 layer.

## See also

[`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md),
[`geom_ssm_arc()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_arc.md)

## Examples

``` r
data("jz2017")
res <- ssm_analyze(jz2017, scales = 2:9, measures = "NARPD")
amax <- 0.5
ggcircumplex(octants(), amax = amax) +
  geom_ssm_point(
    data = res$results,
    mapping = ggplot2::aes(amplitude = a_est, displacement = d_est),
    amax = amax
  )
```
