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

test_that("internal parameter layout is name-driven, not positional", {
  # Contract with the C++ group_parameters()/ssm_parameters_cpp(): six
  # parameters per group in this fixed order. The bootstrap assembly relies on
  # these names (not positional arithmetic) to locate displacement, so pin them.
  expect_identical(ssm_param_names(), c("e", "x", "y", "a", "d", "fit"))
  expect_identical(which(ssm_param_names() == "d"), 5L)

  # reshape_params() lays one group per row with parameter_suffix column names,
  # derived from ssm_param_names() rather than a hardcoded six-block.
  two_groups <- reshape_params(as.numeric(1:12), suffix = "est")
  expect_identical(
    colnames(two_groups),
    c("e_est", "x_est", "y_est", "a_est", "d_est", "fit_est")
  )
  expect_equal(nrow(two_groups), 2)
  expect_equal(two_groups$d_est, c(5, 11)) # 5th value of each six-block
})

test_that("param_diff() generalizes to matrices row-wise (C1)", {
  # The Monte Carlo engine contrasts an R x 6 draw matrix; param_diff() must
  # give the same result row-by-row as the length-6 vector form (which the
  # bootstrap path uses), so both engines share one contrast convention.
  set.seed(1)
  second <- matrix(rnorm(18, sd = 2), nrow = 3) # 3 draws x 6 params
  first <- matrix(rnorm(18, sd = 2), nrow = 3)
  mres <- param_diff(second, first)
  expect_true(is.matrix(mres))
  for (i in seq_len(nrow(second))) {
    expect_equal(
      unname(mres[i, ]),
      unname(param_diff(second[i, ], first[i, ]))
    )
  }
  # Displacement column (5) is an angular distance, not a plain difference
  d <- which(ssm_param_names() == "d")
  expect_false(isTRUE(all.equal(mres[, d], second[, d] - first[, d])))
})

test_that("displacement is classed by name across >2 groups (non-contrast)", {
  # Three groups exercise the multi-block name-based displacement selection
  # beyond the one/two-group cases covered elsewhere: every group's
  # displacement must land on [0, 360) in degrees and be finite.
  rad <- as.numeric(as_radian(octants()))
  set.seed(2017)
  mk <- function(peak) {
    t(sapply(1:15, function(i) 1 + 2 * cos(rad - peak * pi / 180) + rnorm(8, 0, 1)))
  }
  dat <- as.data.frame(rbind(mk(45), mk(180), mk(315)))
  colnames(dat) <- PANO()
  dat$Group <- rep(c("A", "B", "C"), each = 15)

  set.seed(1)
  res <- ssm_analyze(dat, scales = 1:8, grouping = "Group", boots = 50)
  d <- res$results$d_est
  expect_length(d, 3)
  expect_true(all(is.finite(d)))
  expect_true(all(d >= 0 & d < 360))
})

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

test_that("pairwise-deletion bootstrap tolerates an all-NA resampled column (F1)", {
  # F1 (Brief C audit): under listwise = FALSE a resample can draw only NA rows
  # for one scale, leaving an empty column for col_means(). Pre-fix this aborted
  # the whole ssm_analyze() call with "mean(): object has no elements". The mean
  # path should now degrade like the correlation path (pairwise_r): return NA for
  # the empty column and let the degenerate-replicate exclusion + warning absorb
  # it. Deterministic repro from the audit (seed 123, 4/6-missing scale).
  set.seed(123)
  df <- data.frame(
    S1 = c(1, 2, NA, NA, NA, NA),
    S2 = rnorm(6), S3 = rnorm(6), S4 = rnorm(6),
    S5 = rnorm(6), S6 = rnorm(6), S7 = rnorm(6), S8 = rnorm(6)
  )
  expect_warning(
    res <- ssm_analyze(df, scales = paste0("S", 1:8), boots = 500,
                       listwise = FALSE),
    "resamples"
  )
  # The observed profile is well-defined; only some resamples are degenerate.
  expect_true(is.finite(res$results$e_est))
  expect_true(is.finite(res$results$a_est))
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

test_that("exactly opposite profiles report a +180 contrast inside its CI (F3)", {
  # Exactly sign-flipped group profiles: group b's mean vector is the exact
  # negation of group a's, so the two displacements are atan2(-y, -x) vs
  # atan2(y, x) -- a float-exact (or wrap-absorbed) half-turn. The (-180, 180]
  # convention requires the contrast to be reported as +180, not -180
  # (pre-fix it was exactly -180), and the CI branch-alignment shift must
  # follow the estimate to the +180 branch so the estimate stays numerically
  # inside its interval.
  set.seed(42)
  base <- matrix(rnorm(50 * 8), 50, 8) %*% diag(1:8 / 4)
  dat <- data.frame(rbind(base, -base))
  names(dat) <- paste0("S", 1:8)
  dat$G <- rep(c("a", "b"), each = 50)

  set.seed(24)
  res <- suppressWarnings(ssm_analyze(
    dat, scales = paste0("S", 1:8), grouping = "G", contrast = TRUE,
    boots = 200
  ))
  r <- res$results[nrow(res$results), ]

  # Contract: strictly inside (-180, 180] (pre-fix: exactly -180, failing this)
  expect_true(r$d_est > -180 && r$d_est <= 180)
  # The exact half-turn atom reports the +180 branch. NB: this seed lands on the
  # bit-exact atom (remapped to +180); other seeds can leave the half-turn 1-2
  # ulp off and report -179.9999...deg, so this +sign assertion is seed-specific.
  expect_equal(abs(r$d_est), 180)
  expect_gt(r$d_est, 0)
  # Estimate lies numerically inside its own CI at the atom
  expect_true(r$d_lci <= r$d_est && r$d_est <= r$d_uci)
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

test_that("quantile.circumplex_* return NA_real_ on an all-NA column (M13)", {
  # A flat / zero-variance displacement column arrives all-NA; the circular
  # quantile methods must return a numeric NA (NA_real_), not a logical NA, so
  # downstream numeric CI assembly stays type-stable (sapply/rbind, cpm_fit's
  # q[1]/q[2]).
  r <- new_radian(rep(NA_real_, 4))
  cr <- new_contrast_radian(rep(NA_real_, 4))
  expect_identical(quantile(r), NA_real_)
  expect_identical(quantile(cr), NA_real_)
  # length-1 return is preserved (the ssm_ci_accuracy.R length==1 guard depends
  # on it)
  expect_length(quantile(r), 1L)
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
