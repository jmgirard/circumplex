# Create a Circular Plot of SSM Results

Take in the results of a Structural Summary Method analysis and plot the
point and interval estimate for each row (e.g., group or measure) in a
circular space quantified by displacement and amplitude.

## Usage

``` r
ssm_plot_circle(
  ssm_object,
  amax = NULL,
  legend_font_size = 12,
  scale_font_size = 12,
  drop_lowfit = FALSE,
  repel = FALSE,
  angle_labels = NULL,
  palette = "Set2",
  vary_shapes = FALSE,
  path = FALSE,
  ...
)
```

## Arguments

- ssm_object:

  Required. The output of
  [`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md).

- amax:

  A positive real number corresponding to the radius of the circle. It
  is used to scale the amplitude values and will determine which
  amplitude labels are drawn.

- legend_font_size:

  A positive real number corresponding to the size (in pt) of the text
  labels in the legend (default = 12).

- scale_font_size:

  A positive real number corresponding to the size (in pt) of the text
  labels for the amplitude and displacement scales (default = 12).

- drop_lowfit:

  A logical determining whether profiles with low model fit (\<.70)
  should be omitted or plotted with dashed borders (default = FALSE).

- repel:

  A logical determining whether each profile is labelled with a repelled
  text label (placed on the circumplex canvas by
  [`coord_circumplex()`](http://circumplex.jmgirard.com/reference/coord_circumplex.md),
  so labels avoid overlapping each other and the points) instead of
  distinguished by colour and a legend (default = FALSE). Requires the
  ggrepel package.

- angle_labels:

  A character vector specifying text labels to plot around the circle
  for each scale. Can also specify NULL to default to numerical angle
  labels or a vector of empty strings ("") to hide the labels. If not
  NULL, must have the same length and ordering as the `angles` argument
  to
  [`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md).
  (default = NULL)

- palette:

  A string corresponding to the palette to be used from ColorBrewer for
  the color and fill aesthetics. If set to NULL, all points will appear
  blue and no legend will be there (useful for showing the coverage of a
  high number of variables).

- vary_shapes:

  A logical determining whether profiles should each get their own shape
  or vary only by fill color. This only works when the number of
  profiles is five or less. (default = FALSE)

- path:

  A logical determining whether each series' movement across occasions
  is drawn as an arrowed path on the circle (default = `FALSE`).
  Requires an SSM object with occasions, from
  [`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md)
  with the `occasions` argument or from
  [`ssm_analyze_long()`](http://circumplex.jmgirard.com/reference/ssm_analyze_long.md);
  supplying `TRUE` for any other object is an error. Occasions are
  connected in the order they were supplied, never alphabetically, and
  the path is drawn the short way across the 0/360 boundary. An occasion
  whose displacement is undefined (a flat or zero-amplitude profile)
  breaks the path rather than being interpolated through. See
  [`geom_ssm_path()`](http://circumplex.jmgirard.com/reference/geom_ssm_path.md)
  for the underlying layer.

- ...:

  Not used. Supplying an unrecognized argument produces a warning.

## Value

A ggplot variable containing a completed circular plot.

## See also

Other visualization functions:
[`plot.circumplex_ci_accuracy()`](http://circumplex.jmgirard.com/reference/plot.circumplex_ci_accuracy.md),
[`ssm_plot_contrast()`](http://circumplex.jmgirard.com/reference/ssm_plot_contrast.md),
[`ssm_plot_curve()`](http://circumplex.jmgirard.com/reference/ssm_plot_curve.md),
[`ssm_plot_trajectory()`](http://circumplex.jmgirard.com/reference/ssm_plot_trajectory.md)

## Examples

``` r
# `boots` is lowered from its default of 2000 throughout these examples so
# they run quickly; a reported analysis should use the default.

# \donttest{
data("jz2017")
res <- ssm_analyze(
  jz2017,
  scales = 2:9,
  measures = c("NARPD", "ASPD"),
  boots = 200
)
ssm_plot_circle(res)

# }
```
