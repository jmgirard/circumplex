# Circumplex canvas theme

The ggplot2 theme applied to the circumplex canvas built by
[`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md).
It is built on
[`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
so that the amplitude rings, displacement spokes, and labels drawn by
[`coord_circumplex()`](http://circumplex.jmgirard.com/dev/reference/coord_circumplex.md)
are themed panel furniture that respond to further theming. Apply it to
a custom circumplex plot, and add `+ theme_*()` or `+ theme()` on top to
restyle the canvas.

## Usage

``` r
theme_circumplex(base_size = 12)
```

## Arguments

- base_size:

  A single positive number giving the base font size (in pt) for the
  theme (default = 12).

## Value

A ggplot2 theme object, to be added to a plot with `+`.

## See also

Other circumplex layers:
[`coord_circumplex()`](http://circumplex.jmgirard.com/dev/reference/coord_circumplex.md),
[`geom_ssm_arc()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_arc.md),
[`geom_ssm_point()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_point.md),
[`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md),
[`scale_x_circumplex()`](http://circumplex.jmgirard.com/dev/reference/scale_x_circumplex.md)

## Examples

``` r
# Restyle the canvas with a larger base font
ggcircumplex(octants()) + theme_circumplex(base_size = 16)
```
