# The "Advanced Circumplex Visualization" vignette's chunk `ellipse-lines`
# computes, from a one-row ssm_ellipse_data() result, the two angles of the
# tangents from the origin to the confidence ellipse and the nearest and
# farthest distances from the origin to its boundary (Nagy, Etzel & Lüdtke
# 2019, Figure 4 and Appendices B and C). The chunk is purled from the
# .Rmd.orig and evaluated here, so the code the page echoes is the code
# tested. Two oracles:
#
# (i) The shipped outline: GeomSsmEllipse's own vertices at n = 3600. The
#     tangent angles are the extreme polar angles over the vertices and the
#     distances the extreme vertex radii. This oracle is a discretization of
#     the geom the figure draws, so it shares the geom's parameterization and
#     is not independent of it.
# (ii) Circular ellipses (var_x = var_y, cov_xy = 0), where the tangents are
#      the centre's displacement +/- asin(r / c) and the distances c - r and
#      c + r, in closed form and independent of GeomSsmEllipse.
# (iii) Non-circular ellipses whose major axis points at the origin, where
#       the nearest and farthest boundary points sit on that axis at
#       c -/+ the semi-major length, in closed form and independent of
#       GeomSsmEllipse. Oracle (ii) never exercises the cross terms of the
#       covariance, so this one carries the distances on a real ellipse.
#
# The .orig is .Rbuildignore'd, so R CMD check and covr skip.

ellipse_row <- function(cen, d, var_x, var_y, cov_xy) {
  data.frame(
    x0 = cen * cos(d * pi / 180), y0 = cen * sin(d * pi / 180),
    var_x = var_x, var_y = var_y, cov_xy = cov_xy
  )
}

ellipse_lines_chunk <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    code <- vignette_chunk("advanced-visualization.Rmd.orig", "ellipse-lines")
    rds <- testthat::test_path("..", "..", "vignettes", "bayesian_ssm_draws.rds")
    if (is.null(code) || !file.exists(rds)) return(NULL)
    env <- new.env(parent = globalenv())
    env$post <- ssm_draws(readRDS(rds), type = "parameters")
    env$ellipse <- ssm_ellipse_data(env$post)
    eval(parse(text = code), envir = env)
    cache <<- env
    cache
  }
})

# Absolute tolerances: every bound in the acceptance criteria is absolute. A
# failure reports the observed gap, so a marginal miss shows its size.
expect_within <- function(got, want, tol, info = NULL) {
  err <- max(abs(got - want))
  expect_true(
    err < tol,
    info = paste0(
      if (!is.null(info)) paste0(info, ": "),
      "max abs error ", signif(err, 3), " against tolerance ", tol
    )
  )
}

skip_if_no_chunk <- function() {
  if (is.null(ellipse_lines_chunk())) {
    skip("vignette .Rmd.orig source or draws fixture unavailable in this build")
  }
}

# Oracle (i): the extremes of the shipped outline at n = 3600.
outline_extremes <- function(ellipse, level) {
  data <- cbind(ellipse, PANEL = factor(1), group = -1L)
  out <- GeomSsmEllipse$setup_data(data, list(level = level, n = 3600L, na.rm = TRUE))
  list(
    angles = c(min(out$x), max(out$x)) %% 360,
    winds = (max(out$x) - min(out$x)) >= 360 - 1e-6,
    radii = c(min(out$y), max(out$y))
  )
}

test_that("the chunk defines ellipse_lines() and runs it on the vignette's ellipse", {
  skip_if_no_chunk()
  env <- ellipse_lines_chunk()
  expect_true(is.function(env$ellipse_lines))
  lines <- env$four_lines
  expect_s3_class(lines, "data.frame")
  expect_identical(lines$line, c("tangent_1", "tangent_2", "nearest", "farthest"))
  expect_true(all(lines$displacement >= 0 & lines$displacement < 360))
  expect_true(all(lines$amplitude[3:4] > 0))
  expect_lt(lines$amplitude[3], lines$amplitude[4])
})

test_that("oracle (i): tangents and distances match the shipped outline at n = 3600", {
  skip_if_no_chunk()
  env <- ellipse_lines_chunk()
  cases <- list(
    vignette = list(ellipse = env$ellipse, level = 0.95),
    d45 = list(ellipse = ellipse_row(0.5, 45, 0.010, 0.004, 0.003), level = 0.95),
    d200 = list(ellipse = ellipse_row(0.4, 200, 0.003, 0.012, -0.002), level = 0.90),
    # A tangent pair straddling 0/360: the half-angle exceeds 2 degrees.
    d358 = list(ellipse = ellipse_row(0.5, 358, 0.006, 0.006, 0.002), level = 0.95)
  )
  for (name in names(cases)) {
    case <- cases[[name]]
    got <- env$ellipse_lines(case$ellipse, level = case$level)
    want <- outline_extremes(case$ellipse, case$level)
    expect_false(want$winds, info = name)
    expect_within(got$displacement[1:2], want$angles, 0.1, info = name)
    expect_within(got$amplitude[3:4], want$radii, 1e-3, info = name)
    # The point at the nearest distance lies on the outline at that radius.
    v <- got$amplitude[3:4] * cbind(cos(got$displacement[3:4] * pi / 180),
                                    sin(got$displacement[3:4] * pi / 180))
    S <- with(case$ellipse, matrix(c(var_x, cov_xy, cov_xy, var_y), 2, 2))
    centre <- c(case$ellipse$x0, case$ellipse$y0)
    q <- apply(v, 1, function(p) drop(t(p - centre) %*% solve(S) %*% (p - centre)))
    expect_within(q, rep(stats::qchisq(case$level, 2), 2), 1e-6, info = name)
  }
  # The straddling case: tangent_1 sits below 360 and tangent_2 above 0.
  got <- env$ellipse_lines(cases$d358$ellipse, level = 0.95)
  expect_gt(got$displacement[1], 300)
  expect_lt(got$displacement[2], 60)
})

test_that("oracle (i): an origin-containing ellipse has NA tangents and the outline's radii", {
  skip_if_no_chunk()
  env <- ellipse_lines_chunk()
  ellipse <- ellipse_row(0.05, 120, 0.010, 0.004, 0.003)
  got <- env$ellipse_lines(ellipse, level = 0.95)
  want <- outline_extremes(ellipse, 0.95)
  expect_true(want$winds)
  expect_identical(got$displacement[1:2], c(NA_real_, NA_real_))
  expect_within(got$amplitude[3:4], want$radii, 1e-3)
})

test_that("oracle (ii): circular ellipses match the closed form", {
  skip_if_no_chunk()
  env <- ellipse_lines_chunk()
  cen <- 0.6
  for (ratio in c(0.2, 0.5, 0.9)) {
    for (d in c(30, 350)) {
      for (level in c(0.5, 0.95)) {
        r <- ratio * cen
        s2 <- r^2 / stats::qchisq(level, 2)
        ellipse <- ellipse_row(cen, d, s2, s2, 0)
        got <- env$ellipse_lines(ellipse, level = level)
        half <- asin(r / cen) * 180 / pi
        info <- sprintf("ratio %s, d %s, level %s", ratio, d, level)
        expect_within(got$displacement[1:2], c((d - half) %% 360, (d + half) %% 360),
                      1e-6, info = info)
        expect_within(got$amplitude[3:4], c(cen - r, cen + r), 1e-6, info = info)
        # The nearest and farthest points lie on the centre's own direction.
        # optimize() locates a stationary point only to about sqrt(eps) in
        # its argument, so the direction is held to 1e-4 degrees; the
        # distances above, flat there, are held to 1e-6.
        expect_within(got$displacement[3:4], c(d, d), 1e-4, info = info)
      }
    }
  }
})

test_that("oracle (iii): an ellipse whose major axis points at the origin has its extremes on that axis", {
  skip_if_no_chunk()
  env <- ellipse_lines_chunk()
  cen <- 0.6
  major <- 0.02
  minor <- 0.005
  for (d in c(30, 200, 350)) {
    for (level in c(0.5, 0.95)) {
      th <- d * pi / 180
      R <- matrix(c(cos(th), sin(th), -sin(th), cos(th)), 2, 2)
      S <- R %*% diag(c(major, minor)) %*% t(R)
      ellipse <- ellipse_row(cen, d, S[1, 1], S[2, 2], S[1, 2])
      semi <- sqrt(major * stats::qchisq(level, 2))
      expect_lt(semi, cen)
      got <- env$ellipse_lines(ellipse, level = level)
      info <- sprintf("d %s, level %s", d, level)
      expect_within(got$amplitude[3:4], c(cen - semi, cen + semi), 1e-6, info = info)
      expect_within(got$displacement[3:4], c(d, d), 1e-4, info = info)
      # The tangents straddle the centre direction symmetrically.
      gap <- (got$displacement[2] - got$displacement[1]) %% 360
      expect_within((got$displacement[1] + gap / 2) %% 360, d, 1e-4, info = info)
    }
  }
})

test_that("the ellipse figure draws the four lines from the chunk's values", {
  skip_if_no_chunk()
  env <- ellipse_lines_chunk()
  code <- vignette_chunk("advanced-visualization.Rmd.orig", "ellipse-figure")
  fig_env <- new.env(parent = asNamespace("ggplot2"))
  for (v in c("post", "ellipse", "level", "four_lines")) {
    assign(v, get(v, env), envir = fig_env)
  }
  p <- eval(parse(text = code), envir = fig_env)
  expect_s3_class(p, "ggplot")
  expect_identical(p$coordinates$grid, "cartesian")
  built <- ggplot2::ggplot_build(p)
  expect_equal(built$layout$panel_scales_x[[1]]$get_breaks(), as.numeric(octants()),
               ignore_attr = TRUE)
  geoms <- unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))
  expect_identical(
    geoms,
    c("GeomBlank", "GeomSsmArc", "GeomSsmPath", "GeomSsmPath", "GeomSsmEllipse",
      "GeomSsmPoint", "GeomText")
  )
  lines <- env$four_lines
  amax <- p$coordinates$amax
  # The wedge, light.
  expect_lte(p$layers[[2]]$aes_params$alpha, 0.15)
  expect_identical(p$layers[[2]]$data, env$post$results)
  # Dashed tangents from the origin to the rim at the chunk's angles.
  tang <- p$layers[[3]]
  expect_identical(tang$aes_params$linetype, "dashed")
  for (i in 1:2) {
    rows <- tang$data[tang$data$line == lines$line[i], ]
    expect_equal(sort(rows$amplitude), c(0, amax))
    expect_equal(unique(rows$displacement), lines$displacement[i])
  }
  # Dotted distances from the origin to the nearest and farthest points.
  dist <- p$layers[[4]]
  expect_identical(dist$aes_params$linetype, "dotted")
  for (i in 3:4) {
    rows <- dist$data[dist$data$line == lines$line[i], ]
    expect_equal(sort(rows$amplitude), c(0, lines$amplitude[i]))
    expect_equal(unique(rows$displacement), lines$displacement[i])
  }
  # The ellipse from ssm_ellipse_data(post), the point and one label.
  expect_identical(p$layers[[5]]$data, env$ellipse)
  expect_identical(rlang::as_label(p$layers[[6]]$mapping$amplitude), "a_est")
  expect_identical(rlang::as_label(p$layers[[6]]$mapping$displacement), "d_est")
  expect_equal(nrow(p$layers[[7]]$data), 1L)
})

test_that("oracle (ii): a circle containing the origin has NA tangents and nearest r - c", {
  skip_if_no_chunk()
  env <- ellipse_lines_chunk()
  cen <- 0.2
  r <- 0.3
  s2 <- r^2 / stats::qchisq(0.95, 2)
  got <- env$ellipse_lines(ellipse_row(cen, 30, s2, s2, 0), level = 0.95)
  expect_identical(got$displacement[1:2], c(NA_real_, NA_real_))
  expect_within(got$amplitude[3:4], c(r - cen, r + cen), 1e-6)
})
