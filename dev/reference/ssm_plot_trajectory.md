# Create a Trajectory Plot of SSM Results Across Occasions

Plot each Structural Summary Method parameter against occasion, one
facet per parameter, with its confidence interval as a band. This is a
Cartesian diagnostic plot, not a circumplex figure: the horizontal axis
is time, not angle.

## Usage

``` r
ssm_plot_trajectory(
  ssm_object,
  drop_xy = FALSE,
  base_size = 11,
  na.rm = TRUE,
  ...
)
```

## Arguments

- ssm_object:

  An SSM results object produced by
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
  with the `occasions` argument, or by
  [`ssm_analyze_long()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze_long.md).

- drop_xy:

  A logical determining whether the X-value and Y-value panels should be
  omitted (default = `FALSE`), leaving elevation, amplitude, and
  displacement.

- base_size:

  A positive number determining the base font size of the plot (default
  = 11).

- na.rm:

  A logical determining whether occasions that cannot be plotted (no
  defined displacement) are dropped silently (default = `TRUE`) or with
  a warning naming how many were removed (`FALSE`).

- ...:

  Not used. Supplying an unrecognized argument produces a warning.

## Value

A ggplot object depicting each SSM parameter's trajectory across
occasions, with confidence bands.

## Details

The displacement panel is drawn on an *unwrapped* branch, so a profile
whose displacement crosses the 0/360 boundary renders as one continuous
path rather than jumping a full turn. Values on that panel may therefore
fall outside \[0, 360); each confidence bound is placed at its signed
angular distance from its own estimate. Unwrapping assumes the profile
rotates less than a half-turn between consecutive occasions at which its
displacement is defined – no data can verify this, so occasions that are
far apart in time, or a series with a gap, should be read with that in
mind.

Occasions appear in the order they were supplied to
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
(or in the occasion factor's level order for
[`ssm_analyze_long()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze_long.md)),
never in alphabetical order.

On the displacement panel, an occasion whose amplitude confidence
interval is too close to zero for its displacement to be interpretable
is drawn as a hollow point; see
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
for the certification rule. A profile with no defined displacement at
all (a flat profile) leaves a gap in that panel.

A contrast row is never plotted as an occasion – it is a difference, not
a time point. Use
[`ssm_plot_contrast()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_contrast.md)
for it.

## See also

Other visualization functions:
[`plot.circumplex_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/plot.circumplex_ci_accuracy.md)

## Examples

``` r
# \donttest{
data("jz2017")
scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
t1 <- jz2017[, scales]
t1$id <- seq_len(nrow(t1))
t1$occasion <- "T1"
t2 <- t1
t2$occasion <- "T2"
res <- ssm_analyze_long(rbind(t1, t2),
  scales = scales, id = "id", occasion = "occasion"
)
ssm_plot_trajectory(res)

ssm_plot_trajectory(res, drop_xy = TRUE)

# }
```
