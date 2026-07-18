# Create a Curve Plot of SSM Results

Take in the results of a Structural Summary Method analysis and plot the
scores by angle and the estimated SSM curve.

## Usage

``` r
ssm_plot_curve(
  ssm_object,
  angle_labels = NULL,
  base_size = 11,
  drop_lowfit = FALSE,
  ...
)
```

## Arguments

- ssm_object:

  Required. The results output of
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md).

- angle_labels:

  Optional. Either NULL or a character vector that determines the x-axis
  labels. If NULL, the labels will be the angle numbers. If a character
  vector, must be the same length and in the same order as the `angles`
  argument to
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
  (default = NULL).

- base_size:

  Optional. A positive number corresponding to the base font size in pts
  (default = 11).

- drop_lowfit:

  Optional. A logical indicating whether to omit profiles with low fit
  (\<.70) or include them with dashed lines (default = FALSE).

- ...:

  Not used. Supplying an unrecognized argument produces a warning.

## Value

A ggplot object depicting the SSM curve(s) of each profile.

## See also

Other visualization functions:
[`plot.circumplex_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/plot.circumplex_ci_accuracy.md),
[`ssm_plot_circle()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_circle.md),
[`ssm_plot_contrast()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_contrast.md),
[`ssm_plot_trajectory()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_trajectory.md)

## Examples

``` r
# \donttest{
data("jz2017")
res <- ssm_analyze(
  jz2017,
  scales = 2:9,
  measures = 10:13
)
ssm_plot_curve(res)

ssm_plot_curve(res, angle_labels = PANO())

# }
```
