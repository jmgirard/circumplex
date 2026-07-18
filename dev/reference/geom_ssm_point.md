# Draw SSM profile points in circumplex space

A ggplot2 layer that places a point for each profile at its amplitude
and displacement on a circumplex canvas built with
[`coord_circumplex()`](http://circumplex.jmgirard.com/dev/reference/coord_circumplex.md)
(for example the canvas from
[`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md)).
The amplitude and displacement are supplied directly in SSM units
(amplitude in the score metric, displacement in degrees); the coordinate
system performs the polar transform.

## Usage

``` r
geom_ssm_point(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  amax = NULL,
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

  (Deprecated) The amplitude represented by the outer ring is now owned
  by
  [`coord_circumplex()`](http://circumplex.jmgirard.com/dev/reference/coord_circumplex.md);
  a value supplied here is ignored with a one-time note.

- na.rm:

  If `FALSE`, warn (with the dropped-row count) before removing profiles
  with a missing displacement or amplitude, since they have no location
  on the circle; if `TRUE` (the default) remove them silently.

## Value

A ggplot2 layer.

## See also

[`coord_circumplex()`](http://circumplex.jmgirard.com/dev/reference/coord_circumplex.md),
[`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md),
[`geom_ssm_arc()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_arc.md)

## Examples

``` r
data("jz2017")
res <- ssm_analyze(jz2017, scales = 2:9, measures = "NARPD")
ggcircumplex(octants(), amax = 0.5) +
  geom_ssm_point(
    data = res$results,
    mapping = ggplot2::aes(amplitude = a_est, displacement = d_est)
  )
```
