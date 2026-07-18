# Circumplex ggproto classes

These are the ggplot2
[`ggplot2::ggproto()`](https://ggplot2.tidyverse.org/reference/ggproto.html)
classes that back the circumplex layers and coordinate system:
`GeomSsmPoint` (the profile-point geom), `GeomSsmArc` (the
confidence-region arc geom), `GeomSsmPath` (the movement-path geom), and
`CoordCircumplex` (the coordinate system). They are exported so that
downstream packages can subclass them to build custom circumplex layers;
most users should use the
[`geom_ssm_point()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_point.md),
[`geom_ssm_arc()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_arc.md),
[`geom_ssm_path()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_path.md),
and
[`coord_circumplex()`](http://circumplex.jmgirard.com/dev/reference/coord_circumplex.md)
constructors instead.

## See also

[`geom_ssm_point()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_point.md),
[`geom_ssm_arc()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_arc.md),
[`geom_ssm_path()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_path.md),
[`coord_circumplex()`](http://circumplex.jmgirard.com/dev/reference/coord_circumplex.md)
