# The polar-native geoms must reproduce, layer for layer, the coordinates that
# ssm_plot_circle() computes inline. We compare the built (post-transform) x/y
# of the arc and point layers rather than rendering, so the equivalence is
# exact and device-independent. circle_base()/ggcircumplex() contribute the
# same five canvas layers in both plots, so the arc is layer 6 and the points
# are layer 7 in each.

# Rebuild the plotting data frame the way ssm_plot_circle() does, so the
# extension plot receives identical inputs.
ext_plot <- function(res, amax) {
  df <- res$results
  if (res$details$contrast) df <- df[1:2, ]
  ggcircumplex(
    angles = as.integer(round(res$details$angles)),
    amax = amax
  ) +
    geom_ssm_arc(
      data = df,
      mapping = ggplot2::aes(
        amplitude_min = .data$a_lci,
        amplitude_max = .data$a_uci,
        displacement_min = .data$d_lci,
        displacement_max = .data$d_uci,
        group = .data$Label
      ),
      amax = amax
    ) +
    geom_ssm_point(
      data = df,
      mapping = ggplot2::aes(
        amplitude = .data$a_est,
        displacement = .data$d_est,
        group = .data$Label
      ),
      amax = amax
    )
}

arc_xy <- function(p) {
  d <- ggplot2::ggplot_build(p)$data[[6]]
  d[order(d$x, d$y), c("x", "y")]
}
point_xy <- function(p) {
  d <- ggplot2::ggplot_build(p)$data[[7]]
  d[order(d$x, d$y), c("x", "y")]
}

test_that("geom_ssm_arc/point reproduce ssm_plot_circle geometry", {
  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(jz2017, scales = 2:9, measures = "PARPD")
  amax <- pretty_max(res$results$a_uci)

  p_ssm <- ssm_plot_circle(res)
  p_ext <- ext_plot(res, amax)

  expect_equal(arc_xy(p_ext), arc_xy(p_ssm), ignore_attr = TRUE)
  expect_equal(point_xy(p_ext), point_xy(p_ssm), ignore_attr = TRUE)
})

test_that("geom_ssm_* reproduce geometry with multiple profiles", {
  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(jz2017, scales = 2:9, measures = 10:13)
  amax <- pretty_max(res$results$a_uci)

  p_ssm <- ssm_plot_circle(res)
  p_ext <- ext_plot(res, amax)

  expect_equal(arc_xy(p_ext), arc_xy(p_ssm), ignore_attr = TRUE)
  expect_equal(point_xy(p_ext), point_xy(p_ssm), ignore_attr = TRUE)
})

test_that("an arc straddling the 0/360 seam renders as one contiguous arc", {
  data("jz2017")
  set.seed(12345)
  dat <- jz2017[sample(seq_len(nrow(jz2017)), size = 100), ]
  res <- ssm_analyze(dat, 2:9, measures = 19)
  amax <- pretty_max(res$results$a_uci)

  # Must match ssm_plot_circle's cross-zero handling exactly
  expect_equal(arc_xy(ext_plot(res, amax)), arc_xy(ssm_plot_circle(res)),
               ignore_attr = TRUE)

  # A synthetic interval that crosses the seam (350 -> 10 deg, a 20 deg span)
  # must render the short way, producing an arc of the same angular width as an
  # equivalent non-crossing interval (170 -> 190 deg). arcPaths() emits points
  # proportional to angular span, so a mishandled wrap (spanning 340 deg the
  # long way) would produce far more vertices; equal counts prove containment.
  one_arc <- function(dlo, dhi) {
    p <- ggcircumplex(octants(), amax = 0.5) +
      geom_ssm_arc(
        data = data.frame(a_lci = 0.2, a_uci = 0.3, d_lci = dlo, d_uci = dhi),
        mapping = ggplot2::aes(
          amplitude_min = a_lci, amplitude_max = a_uci,
          displacement_min = d_lci, displacement_max = d_uci
        ),
        amax = 0.5
      )
    nrow(ggplot2::ggplot_build(p)$data[[6]])
  }
  # Equal up to arcPaths() discretization rounding; a long-way (340 deg) wrap
  # would differ by hundreds of vertices, not a couple.
  expect_lt(abs(one_arc(350, 10) - one_arc(170, 190)), 5)
})

test_that("geoms drop NA-displacement (degenerate) rows without error", {
  # A flat profile has NA displacement and fit
  dat <- as.data.frame(matrix(1, nrow = 20, ncol = 8))
  colnames(dat) <- PANO()
  res <- suppressWarnings(ssm_analyze(dat, scales = 1:8, boots = 20))

  p <- ggcircumplex(octants(), amax = 0.5) +
    geom_ssm_arc(
      data = res$results,
      mapping = ggplot2::aes(
        amplitude_min = .data$a_lci, amplitude_max = .data$a_uci,
        displacement_min = .data$d_lci, displacement_max = .data$d_uci
      ),
      amax = 0.5
    ) +
    geom_ssm_point(
      data = res$results,
      mapping = ggplot2::aes(
        amplitude = .data$a_est, displacement = .data$d_est
      ),
      amax = 0.5
    )

  expect_no_error(ggplot2::ggplot_build(p))
  # The degenerate row contributes no point vertices
  expect_equal(nrow(ggplot2::ggplot_build(p)$data[[7]]), 0)
})

test_that("a canvas-plus-geoms plot renders (visual regression)", {
  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(jz2017, scales = 2:9, measures = "PARPD")
  amax <- pretty_max(res$results$a_uci)
  vdiffr::expect_doppelganger("ggcircumplex with ssm geoms", ext_plot(res, amax))
})
