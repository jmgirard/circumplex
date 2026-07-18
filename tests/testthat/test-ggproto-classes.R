# M32 T1: the layer/coord ggproto generators are exported so downstream
# packages can subclass them (DESIGN.md "circumplex-ggproto"; the two-tier API,
# D-018). These tests fence the NAMESPACE export AND prove a trivial subclass of
# each renders on the circumplex canvas -- an unexported object would still
# subclass under load_all(), so the export assertion is the load-bearing fence.

test_that("the layer/coord ggproto generators are exported (T1)", {
  exports <- getNamespaceExports("circumplex")
  expect_true(all(
    c("GeomSsmPoint", "GeomSsmArc", "CoordCircumplex") %in% exports
  ))
})

test_that("a downstream subclass of GeomSsmPoint renders (T1)", {
  SubPoint <- ggplot2::ggproto("SubSsmPoint", GeomSsmPoint)
  df <- data.frame(a_est = c(0.2, 0.3), d_est = c(45, 120))
  lay <- ggplot2::layer(
    geom = SubPoint, stat = "identity", position = "identity", data = df,
    mapping = ggplot2::aes(amplitude = .data$a_est, displacement = .data$d_est),
    params = list(na.rm = TRUE)
  )
  p <- ggcircumplex(octants(), amax = 0.5) + lay
  expect_no_error(ggplot2::ggplot_build(p))
  expect_s3_class(SubPoint, "GeomSsmPoint")
})

test_that("a downstream subclass of GeomSsmArc renders (T1)", {
  SubArc <- ggplot2::ggproto("SubSsmArc", GeomSsmArc)
  df <- data.frame(a_lci = 0.2, a_uci = 0.3, d_lci = 40, d_uci = 60)
  lay <- ggplot2::layer(
    geom = SubArc, stat = "identity", position = "identity", data = df,
    mapping = ggplot2::aes(
      amplitude_min = .data$a_lci, amplitude_max = .data$a_uci,
      displacement_min = .data$d_lci, displacement_max = .data$d_uci
    ),
    params = list(na.rm = TRUE)
  )
  p <- ggcircumplex(octants(), amax = 0.5) + lay
  expect_no_error(ggplot2::ggplot_build(p))
  expect_s3_class(SubArc, "GeomSsmArc")
})

test_that("a downstream subclass of CoordCircumplex renders (T1)", {
  SubCoord <- ggplot2::ggproto("SubCoordCircumplex", CoordCircumplex)
  # A downstream subclass writes its own constructor; here we re-home a stock
  # coord_circumplex() instance under the subclass parent (the same field-copy
  # coord_circumplex() itself uses) to prove the exported parent is usable.
  base <- coord_circumplex(amax = 0.5)
  sub <- ggplot2::ggproto(
    NULL, SubCoord,
    limits = base$limits, theta = base$theta, r = base$r, arc = base$arc,
    expand = base$expand, reverse = base$reverse,
    r_axis_inside = base$r_axis_inside, rotate_angle = base$rotate_angle,
    inner_radius = base$inner_radius, clip = base$clip,
    amax = base$amax, center = base$center
  )
  expect_s3_class(sub, "CoordCircumplex")
  df <- data.frame(a_est = 0.25, d_est = 90)
  p <- ggplot2::ggplot(df) + sub +
    geom_ssm_point(ggplot2::aes(amplitude = .data$a_est, displacement = .data$d_est))
  expect_no_error(ggplot2::ggplot_build(p))
})
