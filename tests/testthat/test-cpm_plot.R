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
  skip_on_cran()
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
  skip_on_cran()
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

# ---- zero-width interval marks ----------------------------------------------
# A zero-width interval names no wedge, so the arc layer drops it. The plot must
# still show it: a line along its nonzero side, or a short cap when both widths
# are zero. These tests edit one scale's bounds in a clean fit and measure the
# mark that scale gets on the canvas.

# Set scale `scale`'s angle bounds and zeta bounds (communality = zeta^2).
with_bounds <- function(fit, scale, angle = NULL, zeta = NULL, comm = NULL) {
  i <- match(scale, as.character(fit$results$Scale))
  if (!is.null(angle)) {
    fit$results$Angle_lci[i] <- angle[[1]]
    fit$results$Angle_uci[i] <- angle[[2]]
  }
  if (!is.null(zeta)) {
    fit$results$Zeta_lci[i] <- zeta[[1]]
    fit$results$Zeta_uci[i] <- zeta[[2]]
  }
  if (!is.null(comm)) fit$results$Communality[i] <- comm
  fit
}

# The zero-width mark layer's rows for one scale, with the chord length between
# each mark's two ends measured in canvas coordinates after the coord transform.
# A chord above 0 means the drawn line has a length above 0.
zero_width_marks <- function(p, scale) {
  b <- ggplot2::ggplot_build(p)
  idx <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomSegment"),
                      logical(1)))
  if (length(idx) == 0) return(NULL)
  idx <- idx[[1]]
  rows <- which(as.character(p$layers[[idx]]$data$Scale) == scale)
  d <- b$data[[idx]][rows, , drop = FALSE]
  if (nrow(d) == 0) return(d)
  pp <- b$layout$panel_params[[1]]
  from <- b$layout$coord$transform(data.frame(x = d$x, y = d$y), pp)
  to <- b$layout$coord$transform(data.frame(x = d$xend, y = d$yend), pp)
  d$canvas_length <- sqrt((to$x - from$x)^2 + (to$y - from$y)^2)
  d
}

expect_visible_mark <- function(fit, scale) {
  p <- expect_no_warning(plot(fit))
  m <- zero_width_marks(p, scale)
  expect_false(is.null(m))
  expect_gt(nrow(m), 0)
  expect_true(all(m$linewidth > 0))
  expect_true(all(m$canvas_length > 1e-3))
  invisible(m)
}

test_that("a zero-width angle interval draws a radial line (AC1)", {
  skip_on_cran()
  fit <- clean_cpm_fit()
  # The reference scale's angle is fixed, so its interval already has zero width.
  expect_equal(fit$results$Angle_lci[1], fit$results$Angle_uci[1])
  m <- expect_visible_mark(fit, "PA")
  expect_equal(m$x, m$xend)
  expect_equal(m$y, fit$results$Zeta_lci[1]^2)
  expect_equal(m$yend, fit$results$Zeta_uci[1]^2)
  # Stored at the seam in each of its three forms.
  for (ang in list(c(0, 0), c(360, 360), c(360, 0))) {
    expect_visible_mark(with_bounds(fit, "LM", angle = ang), "LM")
  }
})

test_that("a zero-width communality interval draws an arc (AC1)", {
  skip_on_cran()
  fit <- clean_cpm_fit()
  for (cm in c(0.5, 1)) {
    # Crossing the seam, and with a negative lower bound.
    for (ang in list(c(350, 10), c(-10.85758, 10.85758))) {
      f <- with_bounds(fit, "LM", angle = ang, zeta = sqrt(c(cm, cm)))
      m <- expect_visible_mark(f, "LM")
      expect_equal(m$y, cm)
      expect_equal(m$yend, cm)
      expect_equal(m$xend - m$x, ssm_arc_span(ang[[1]], ang[[2]]))
    }
  }
})

test_that("a both-zero interval draws a cap centered on the interval (AC1)", {
  skip_on_cran()
  fit <- clean_cpm_fit()
  for (ang in list(c(0, 0), c(360, 360))) {
    # The communality estimate (0.8464) sits away from the zero-width bound.
    f <- with_bounds(fit, "LM", angle = ang, zeta = c(0.8, 0.8))
    m <- expect_visible_mark(f, "LM")
    expect_equal(m$y, 0.64)
    expect_equal(m$yend, 0.64)
    expect_equal((m$x + m$xend) / 2, ang[[1]])
  }
})

test_that("a zero-width angle interval from communality 0 draws a line (AC1)", {
  skip_on_cran()
  fit <- clean_cpm_fit()
  f <- with_bounds(fit, "PA", zeta = c(-0.1, 0.9))
  m <- expect_visible_mark(f, "PA")
  expect_equal(m$y, 0)
  expect_equal(m$yend, 0.81)
})

test_that("a scale with nonzero widths gets a wedge and no line mark (AC1)", {
  skip_on_cran()
  fit <- clean_cpm_fit()
  p <- plot(fit)
  m <- zero_width_marks(p, "BC")
  expect_true(is.null(m) || nrow(m) == 0)
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(cpm_layer(b, p, "GeomSsmArc")), 7L)
})

test_that("a communality interval with upper bound 0 warns and draws a point (AC1)", {
  skip_on_cran()
  fit <- clean_cpm_fit()
  f <- with_bounds(fit, "BC", zeta = c(-0.2, -0.05))
  expect_warning(p <- plot(f), "BC \\(communality interval at 0\\)")
  m <- zero_width_marks(p, "BC")
  expect_true(is.null(m) || nrow(m) == 0)
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(cpm_layer(b, p, "GeomSsmArc")), 6L)
  expect_equal(nrow(cpm_layer(b, p, "GeomSsmPoint")), 8L)
})

test_that("plot.circumplex_cpm validates its arguments", {
  skip_on_cran()
  fit <- clean_cpm_fit()
  expect_error(plot(fit, amax = c(1, 2)))
  expect_error(plot(fit, amax = -1))
  expect_error(plot(fit, legend = "yes"))
  expect_error(plot(fit, angle_labels = c("A", "B")))  # wrong length
  expect_warning(plot(fit, bogus_arg = 1), "disregarded")
})
