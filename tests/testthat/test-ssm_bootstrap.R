test_that("Quantile for circular radians works", {
  a <- as_degree(0:180)
  b <- as_radian(a)
  qb <- stats::quantile(b)
  expect_s3_class(qb, "circumplex_radian")
  expect_equal(qb, as_radian(as_degree(c(0, 45, 90, 135, 180))),
               ignore_attr = TRUE)

  a <- as_degree(180:360)
  b <- as_radian(a)
  qb <- quantile(b)
  expect_s3_class(qb, "circumplex_radian")
  expect_equal(qb, as_radian(as_degree(c(180, 225, 270, 315, 0))),
               ignore_attr = TRUE)

  a <- as_degree(c(270:360, 1:90))
  b <- as_radian(a)
  qb <- quantile(b)
  expect_equal(qb, as_radian(as_degree(c(270, 315, 0, 45, 90))),
               ignore_attr = TRUE)

  a <- as_degree(c(NA_real_, NA_real_, NA_real_))
  b <- as_radian(a)
  qb <- stats::quantile(b)
  expect_true(is.na(qb))

  a <- as_degree(c(0, 0, 30, 90, NA_real_))
  b <- as_radian(a)
  c <- as_degree(c(0, 0, 30, 90))
  d <- as_radian(c)
  expect_equal(stats::quantile(b), stats::quantile(d))
})

library(testthat)

test_that("bootstrap with some degenerate replicates does not error", {
  # A rare binary measure: some resamples are constant, giving NaN
  # correlations and hence degenerate replicate profiles
  set.seed(42)
  n <- 100
  dat <- data.frame(matrix(rnorm(n * 8), ncol = 8))
  colnames(dat) <- PANO()
  dat$rare <- c(rep(0, 97), rep(1, 3))

  expect_warning(
    res <- ssm_analyze(dat, scales = 1:8, measures = "rare", boots = 200),
    "resamples"
  )
  # Point estimates and CIs for the observed (non-degenerate) profile exist
  expect_true(is.finite(res$results$e_est))
  expect_true(is.finite(res$results$d_lci) && is.finite(res$results$d_uci))
  expect_true(is.finite(res$results$a_lci) && is.finite(res$results$a_uci))
})

test_that("fully flat data yields NA estimates without erroring", {
  dat <- as.data.frame(matrix(1, nrow = 20, ncol = 8))
  colnames(dat) <- PANO()
  w <- capture_warnings(res <- ssm_analyze(dat, scales = 1:8, boots = 20))
  expect_true(any(grepl("flat", w)))
  expect_equal(res$results$e_est, 1)
  expect_true(is.na(res$results$d_est))
  expect_true(is.na(res$results$fit_est))
})

test_that("contrast displacement estimate and CI share a branch at +/-180", {
  # Two groups with displacements ~180 degrees apart: the bootstrap
  # distribution of the contrast straddles the +/-180 boundary. The estimate
  # (from angle_dist, principal branch) and the CI (from circular-mean
  # centering, its own branch) must be reported on the same branch so the
  # estimate lies numerically inside its interval. This seed reproduces a
  # branch disagreement in the pre-fix code (est +179.4, CI (-196.6, -159.0)).
  rad <- as.numeric(as_radian(octants()))
  set.seed(70)
  A <- t(sapply(1:12, function(i) 1 + 2 * cos(rad - 90 * pi / 180) + rnorm(8, 0, 1.5)))
  B <- t(sapply(1:12, function(i) 1 + 2 * cos(rad - 270 * pi / 180) + rnorm(8, 0, 1.5)))
  dat <- as.data.frame(rbind(A, B))
  colnames(dat) <- PANO()
  dat$Group <- rep(c("A", "B"), each = 12)

  set.seed(5070)
  res <- suppressWarnings(ssm_analyze(
    dat, scales = 1:8, grouping = "Group", contrast = TRUE, boots = 250
  ))
  r <- res$results[nrow(res$results), ]

  # Estimate stays on the principal branch (-180, 180]
  expect_true(r$d_est > -180 && r$d_est <= 180)
  # Estimate lies numerically inside its own CI
  expect_true(r$d_lci <= r$d_est && r$d_est <= r$d_uci)
  # The interval is a sane width (not a wrapped-around artifact)
  expect_lt(r$d_uci - r$d_lci, 90)
  # And it is geometrically near the true contrast of 180 degrees
  expect_lt(abs(abs(r$d_est) - 180), 10)
})

test_that("quantile.circumplex_contrast_radian handles 0/360 boundary crossings cleanly", {
  # Create mock bootstrap replicates that straddle the 0/2*pi boundary.
  # 0.01 and 0.02 rad are just above 0° (~0.57° and ~1.15°).
  # 6.26 and 6.27 rad are just below 360° (~358.67° and ~359.24°).
  # Geometrically, all 4 points sit inside a tight ~2.5° arc.
  replicates <- c(0.01, 0.02, 6.26, 6.27)

  # Assign the custom contrast class
  class(replicates) <- c("circumplex_contrast_radian", "numeric")

  # Calculate confidence interval bounds (e.g., 25% and 75% for simple validation)
  res_quantiles <- quantile(replicates, probs = c(0.25, 0.75))

  # CRITICAL FAILURE CHECK (Old Behavior):
  # Standard linear quantiles would treat 6.26 as a massive number,
  # erroneously yielding a huge spread: [0.0175, 6.2625] (a 357.8° span).

  # CORRECT BEHAVIOR CHECK (New Behavior):
  # The new method unwraps the phase around the circular mean.
  # The resulting interval should be tight and tightly bounded near 0.
  expect_lt(res_quantiles[["75%"]], 0.1)
  expect_gt(res_quantiles[["25%"]], -0.1)

  # The lower quantile should correctly cross into negative space
  expect_true(res_quantiles[["25%"]] < 0)
})

test_that("SSM class conversions preserve negative degrees for contrasts", {
  # Ensure that passing a negative contrast radian through the pipeline
  # cleanly converts to a negative degree value instead of wrapping back to +350°+
  neg_contrast <- structure(-0.05, class = c("circumplex_contrast_radian", "numeric"))

  # Emulate the conversion sequence executed inside ssm_bootstrap()
  radial_conversion <- as_radian(neg_contrast)
  degree_conversion <- as_degree(radial_conversion)

  # The underlying value must remain safely negative
  expect_equal(as.numeric(degree_conversion), -0.05 * (180 / pi))
  expect_s3_class(degree_conversion, "circumplex_degree")
})
