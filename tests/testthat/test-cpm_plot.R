# plot.circumplex_cpm draws the estimated item configuration on the
# ggcircumplex() canvas: a point per scale at its estimated angle and a radius
# given by its communality, with a joint angle x communality CI wedge where the
# interval is estimable.

# A clean, model-implied correlation matrix so the fit recovers exactly and the
# analytic CIs are well defined -- a deterministic (cormat-path, no RNG) fixture
# for the visual snapshot.
clean_cpm_fit <- function() {
  th <- octants() * pi / 180
  zeta <- rep(0.92, 8)
  beta <- c(0.6, 0.3, 0.08, 0.02)
  P <- cpm_implied_cor(th, zeta, beta)
  dimnames(P) <- list(PANO(), PANO())
  cpm_fit(cormat = P, angles = octants(), n = 1000)
}

# Locate a layer's built data by its geom class (the canvas is now the coord's
# panel furniture, not drawn layers, so indices shifted; find by content).
cpm_layer <- function(b, p, geom_class) {
  idx <- which(vapply(p$layers, function(l) inherits(l$geom, geom_class),
                      logical(1)))
  if (length(idx) == 0) return(NULL)
  b$data[[idx[[1]]]]
}

test_that("plot.circumplex_cpm builds a circular canvas with points and wedges", {
  fit <- clean_cpm_fit()
  p <- plot(fit)
  expect_true(ggplot2::is_ggplot(p))

  b <- ggplot2::ggplot_build(p)
  arc <- cpm_layer(b, p, "GeomSsmArc")
  pts <- cpm_layer(b, p, "GeomSsmPoint")
  expect_gt(nrow(arc), 0)
  expect_equal(nrow(pts), 8)  # one point per scale, all locations defined

  # The coord owns the polar transform, so the point layer carries communality
  # as the radial (y) aesthetic and the estimated angle as the angular (x) one.
  expect_equal(sort(pts$y), sort(fit$results$Communality), tolerance = 1e-6)
  expect_setequal(round(pts$x, 6), round(fit$results$Angle, 6))
})

test_that("plot.circumplex_cpm is a stable visual", {
  skip_if_not_installed("vdiffr")
  skip_on_ci() # vdiffr snapshots are platform-dependent (fonts/rendering)
  fit <- clean_cpm_fit()
  vdiffr::expect_doppelganger("cpm circle plot", plot(fit))
  vdiffr::expect_doppelganger("cpm circle plot no legend", plot(fit, legend = FALSE))
})

test_that("plot.circumplex_cpm names scales whose CI wedge is inestimable", {
  # The raw jz2017 octants give an ill-conditioned Hessian -> analytic CIs are
  # all NA. Every scale then draws as a point with no wedge, and the plot must
  # name them rather than let the wedges vanish silently (the unified
  # plottability predicate at the plot level).
  data("jz2017")
  fit <- suppressWarnings(
    cpm_fit(jz2017, scales = 2:9, angles = octants(), ci_method = "analytic")
  )
  expect_true(all(is.na(fit$results$Angle_lci)))  # confirm the fixture

  expect_warning(p <- plot(fit), "wedge omitted")
  expect_true(ggplot2::is_ggplot(p))

  b <- ggplot2::ggplot_build(p)
  # No arc layer is added when nothing is drawable; every scale still draws a
  # point.
  expect_null(cpm_layer(b, p, "GeomSsmArc"))
  expect_equal(nrow(cpm_layer(b, p, "GeomSsmPoint")), 8L)
})

test_that("plot.circumplex_cpm validates its arguments", {
  fit <- clean_cpm_fit()
  expect_error(plot(fit, amax = c(1, 2)))
  expect_error(plot(fit, amax = -1))
  expect_error(plot(fit, legend = "yes"))
  expect_error(plot(fit, angle_labels = c("A", "B")))  # wrong length
  expect_warning(plot(fit, bogus_arg = 1), "disregarded")
})
