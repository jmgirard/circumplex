# Create a circumplex plotting canvas

Build an empty circular canvas – the amplitude rings, displacement
spokes, and scale labels that circumplex figures are drawn on – as a
ggplot2 object. Additional layers (points, arcs, annotations) can be
added to it with `+`, so it serves as the reusable foundation for custom
circumplex visualizations. The package's own
[`ssm_plot_circle()`](http://circumplex.jmgirard.com/reference/ssm_plot_circle.md)
draws on the same canvas.

## Usage

``` r
ggcircumplex(
  angles = octants(),
  labels = NULL,
  amax = 0.5,
  font_size = 12,
  instrument = NULL
)
```

## Arguments

- angles:

  Optional. A numeric vector of the angular position (in degrees) of
  each circumplex scale, going counterclockwise from the right (default
  = [`octants()`](http://circumplex.jmgirard.com/reference/octants.md)).
  Ignored if `instrument` is supplied.

- labels:

  Optional. Either `NULL` or a character vector of text labels to draw
  around the circle, one per angle and in the same order (default =
  `NULL`, which draws the numeric angles). If `instrument` is supplied,
  `NULL` uses the instrument's scale abbreviations.

- amax:

  Optional. A single positive number giving the amplitude at the outer
  ring, which sets the amplitude-axis labels; the center of the circle
  is fixed at amplitude 0 (default = 0.5).

- font_size:

  Optional. A single positive number giving the size (in pt) of the
  scale and amplitude labels (default = 12).

- instrument:

  Optional. Either `NULL` or a `circumplex_instrument` object (see
  `instrument()`). When supplied, the scale `angles` and (unless
  `labels` is given) the scale abbreviations are taken from the
  instrument (default = `NULL`).

## Value

A ggplot2 object containing the empty circumplex canvas.

## See also

[`coord_circumplex()`](http://circumplex.jmgirard.com/reference/coord_circumplex.md),
which owns the transform this canvas is built on;
[`ssm_plot_circle()`](http://circumplex.jmgirard.com/reference/ssm_plot_circle.md),
which draws SSM results on this canvas.

Other circumplex layers:
[`coord_circumplex()`](http://circumplex.jmgirard.com/reference/coord_circumplex.md),
[`geom_ssm_arc()`](http://circumplex.jmgirard.com/reference/geom_ssm_arc.md),
[`geom_ssm_path()`](http://circumplex.jmgirard.com/reference/geom_ssm_path.md),
[`geom_ssm_point()`](http://circumplex.jmgirard.com/reference/geom_ssm_point.md),
[`scale_x_circumplex()`](http://circumplex.jmgirard.com/reference/scale_x_circumplex.md),
[`theme_circumplex()`](http://circumplex.jmgirard.com/reference/theme_circumplex.md)

## Examples

``` r
# A default octant canvas
ggcircumplex()


# Label the scales with their circumplex pole abbreviations
ggcircumplex(octants(), labels = PANO())


# Derive the angles and labels from a circumplex instrument
ggcircumplex(instrument = csip)
```
