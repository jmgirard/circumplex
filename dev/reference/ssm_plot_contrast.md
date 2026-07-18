# Create a Difference Plot of SSM Contrast Results

Take in the results of a Structural Summary Method analysis with
pairwise contrasts and plot the point and interval estimates for each
parameter's contrast (e.g., between groups or measures).

## Usage

``` r
ssm_plot_contrast(
  ssm_object,
  drop_xy = FALSE,
  sig_color = "#fc8d62",
  ns_color = "white",
  linesize = 1.25,
  fontsize = 12,
  ...
)
```

## Arguments

- ssm_object:

  Required. The results output of
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md).

- drop_xy:

  A logical determining whether the X-Value and Y-Value parameters
  should be removed from the plot (default = FALSE).

- sig_color:

  Optional. A string corresponding to the color to use to denote
  significant contrasts (default = "#fc8d62").

- ns_color:

  Optional. A string corresponding to the color to use to denote
  non-significant contrasts (default = "white").

- linesize:

  Optional. A positive number corresponding to the size of the point
  range elements in mm (default = 1.5).

- fontsize:

  Optional. A positive number corresponding to the size of the axis
  labels, numbers, and facet headings in pt (default = 12).

- ...:

  Not used. Supplying an unrecognized argument produces a warning.

## Value

A ggplot variable containing difference point-ranges faceted by SSM
parameter. An interval that does not contain the value of zero has
p\<.05.

## See also

Other visualization functions:
[`plot.circumplex_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/plot.circumplex_ci_accuracy.md),
[`ssm_plot_circle()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_circle.md),
[`ssm_plot_curve()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_curve.md),
[`ssm_plot_trajectory()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_trajectory.md)

## Examples

``` r
# \donttest{
data("jz2017")
res <- ssm_analyze(
  jz2017,
  scales = 2:9,
  measures = c("NARPD", "ASPD"),
  contrast = TRUE
)
ssm_plot_contrast(res)

# }
```
