# Pole-VALUE guards (M120 AC4 iii).
#
# The suite's other 0/360 blocks assert relational properties -- an interval
# wraps, an interval contains its estimate, a path takes the short arc. Those
# all stay true when EVERY displacement is shifted onto atan2's (-pi, pi]
# branch, so none of them fails under that defect; planting it reddens only
# core-estimator blocks that happen to pin a number. The blocks below name the
# expected angle at the pole instead, so the branch shift fails them, and they
# deliberately run on CRAN -- they are the CRAN-visible half of that guard.
#
# Each profile is the exact first harmonic e + a*cos(theta - d), so the closed
# form recovers (e, a, d) exactly and the expected displacement is the d that
# built it, not a value read back off the implementation.

pole_profile <- function(d_deg, angles = octants(), e = 2, a = 0.5) {
  theta <- as.numeric(angles) * pi / 180
  e + a * cos(theta - d_deg * pi / 180)
}

# Raw data whose SAMPLE covariance is exactly `sigma`, in base R -- MASS is not
# a dependency of this package and must not become one for a test. Whiten a
# random matrix against its own covariance, then colour it with sigma's
# Cholesky factor; the result is exact to floating point, so the fit below is
# a population fit rather than a sampling draw.
exact_cov_sample <- function(n, sigma) {
  p <- ncol(sigma)
  stopifnot(n > p)
  set.seed(20260902)
  x <- matrix(stats::rnorm(n * p), n, p)
  x <- sweep(x, 2, colMeans(x))
  x %*% solve(chol(stats::cov(x))) %*% chol(sigma)
}

# 350 sits just below the pole and 5 just above it; on atan2's own branch the
# first reports -10 instead of 350, which is the failure these blocks exist to
# see. 180 is the half-turn, where the branch convention is (-180, 180].
POLE_CASES <- c(350, 359, 5, 180)

test_that("ssm_parameters reports the displacement that built the profile at the 0/360 pole", {
  for (d in POLE_CASES) {
    out <- ssm_parameters(pole_profile(d), angles = octants())
    expect_equal(as.numeric(out$Disp), d,
                 tolerance = 1e-8,
                 info = paste("built at", d, "degrees"))
    expect_equal(as.numeric(out$Ampl), 0.5, tolerance = 1e-8)
  }
})

test_that("ssm_analyze reports the displacement that built the profile at the 0/360 pole", {
  for (d in POLE_CASES) {
    scores <- pole_profile(d)
    # Two identical rows: the mean profile is the profile, so the point
    # estimate is the closed form of it and needs no resampling to check.
    dat <- as.data.frame(rbind(scores, scores))
    names(dat) <- PANO()
    res <- ssm_analyze(dat, scales = PANO(), angles = octants(), boots = 10)
    expect_equal(res$results$d_est, d, tolerance = 1e-6,
                 info = paste("built at", d, "degrees"))
    expect_equal(res$results$a_est, 0.5, tolerance = 1e-6)
  }
})

test_that("ssm_draws reports the displacement that built each profile draw at the 0/360 pole", {
  draws <- do.call(rbind, lapply(POLE_CASES, pole_profile))
  res <- ssm_draws(draws, angles = octants())
  expect_equal(as.numeric(res$draws[, "d"]), POLE_CASES, tolerance = 1e-8)
  expect_equal(as.numeric(res$draws[, "a"]), rep(0.5, length(POLE_CASES)),
               tolerance = 1e-8)
})

test_that("ssm_sem_parameters reports a latent displacement past the 0/360 pole", {
  skip_if_not_installed("lavaan")
  # A measure whose direction sits at 350 degrees, so the latent profile's
  # displacement lands just BELOW the pole -- the region where leaving the
  # transform on atan2's own branch reports about -10 instead.
  oct <- as.numeric(octants())   # test-ssm_sem.R's `oct`, local to this file
  a <- seq(0.5, 0.8, length.out = 8)
  cc <- seq(0.7, 0.5, length.out = 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  delta <- 350 * pi / 180
  sigma_m <- cbind(c(0.2, 0.4 * cos(delta), 0.4 * sin(delta)))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)
  fit <- sem_pop_fit(pop)

  res <- ssm_sem_parameters(
    fit,
    scales = pop$scales, angles = oct, measures = pop$measures,
    boots = 10
  )
  # Truth from the package's own validated closed form on the population
  # latent profile -- a different code path from the SEM transform under test.
  truth <- ssm_parameters(as.numeric(pop$rho0), oct)
  expect_gt(as.numeric(truth$Disp), 180)   # the case really is past the branch
  expect_equal(as.numeric(res$results$d_est), as.numeric(truth$Disp),
               tolerance = 1e-2)
  expect_equal(res$results$a_est, truth$Ampl, tolerance = 1e-4)
})

test_that("ssm_analyze_long reports the displacement that built each occasion at the 0/360 pole", {
  # Same construction as the wide case, entered through the long-format
  # wrapper, so the wrapper's own reshaping cannot quietly rotate a profile.
  built <- c(T1 = 350, T2 = 5)
  long <- do.call(rbind, lapply(names(built), function(occ) {
    scores <- pole_profile(built[[occ]])
    d <- as.data.frame(rbind(scores, scores))
    names(d) <- PANO()
    d$id <- seq_len(2)
    d$occasion <- occ
    d
  }))
  res <- ssm_analyze_long(long, scales = PANO(), angles = octants(),
                          id = "id", occasion = "occasion", boots = 10)
  expect_equal(res$results$d_est, unname(built), tolerance = 1e-6)
  expect_equal(res$results$a_est, rep(0.5, 2), tolerance = 1e-6)
})

test_that("ssm_sem reports a latent displacement past the 0/360 pole", {
  skip_if_not_installed("lavaan")
  # ssm_sem() fits the measurement model itself, where ssm_sem_parameters()
  # above is handed a fit; both reach the same transform, and this block is
  # what keeps the fitting entry point covered on CRAN.
  oct <- as.numeric(octants())
  a <- seq(0.5, 0.8, length.out = 8)
  cc <- seq(0.7, 0.5, length.out = 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  delta <- 350 * pi / 180
  sigma_m <- cbind(c(0.2, 0.4 * cos(delta), 0.4 * sin(delta)))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)
  dat <- as.data.frame(exact_cov_sample(400, pop$sigma))
  names(dat) <- colnames(pop$sigma)
  res <- ssm_sem(dat, scales = pop$scales, angles = oct,
                 measures = pop$measures, boots = 10)
  truth <- ssm_parameters(as.numeric(pop$rho0), oct)
  expect_gt(as.numeric(truth$Disp), 180)
  expect_equal(as.numeric(res$results$d_est), as.numeric(truth$Disp),
               tolerance = 1e-1)
  expect_equal(res$results$a_est, truth$Ampl, tolerance = 1e-2)
})
