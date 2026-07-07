test_that("method argument is validated and defaults to bootstrap", {
  data("aw2009")
  expect_error(
    ssm_analyze(aw2009, scales = 1:8, boots = 10, method = "bogus")
  )
})

# Compare CI endpoints across engines on the scale of the interval width;
# both engines carry Monte Carlo error of the same order, so exact equality
# is not expected but endpoints must agree closely at n = 1200
expect_ci_agreement <- function(mc, bt, params = c("e", "x", "y", "a"),
                                tol = 0.15) {
  for (p in params) {
    w <- bt$results[[paste0(p, "_uci")]] - bt$results[[paste0(p, "_lci")]]
    for (side in c("_lci", "_uci")) {
      expect_lt(
        max(abs(mc$results[[paste0(p, side)]] - bt$results[[paste0(p, side)]])),
        max(tol * w)
      )
    }
  }
}

# Circular difference in degrees, reported in (-180, 180]
deg_dist <- function(x, y) {
  ((x - y + 180) %% 360) - 180
}

test_that("Monte Carlo matches bootstrap: mean-based, single group (jz2017)", {
  skip_on_cran()
  data("jz2017")

  set.seed(111)
  bt <- ssm_analyze(jz2017, scales = PANO(), boots = 2000)
  set.seed(222)
  mc <- ssm_analyze(jz2017, scales = PANO(), boots = 2000,
                    method = "montecarlo")

  # Point estimates use the same closed-form estimator: identical
  expect_equal(mc$results$e_est, bt$results$e_est, tolerance = 1e-12)
  expect_equal(mc$results$a_est, bt$results$a_est, tolerance = 1e-12)
  expect_equal(mc$results$d_est, bt$results$d_est, tolerance = 1e-12)

  expect_ci_agreement(mc, bt)
  d_w <- deg_dist(bt$results$d_uci, bt$results$d_lci)
  expect_lt(abs(deg_dist(mc$results$d_lci, bt$results$d_lci)), 0.15 * d_w)
  expect_lt(abs(deg_dist(mc$results$d_uci, bt$results$d_uci)), 0.15 * d_w)

  # Method recorded and displayed
  expect_identical(mc$details$method, "montecarlo")
  expect_output(summary(mc), "Monte Carlo Draws")
  expect_output(summary(bt), "Bootstrap Resamples")
})

test_that("Monte Carlo matches bootstrap: correlation-based with measure
           contrast (jz2017)", {
  skip_on_cran()
  data("jz2017")

  # Two measures in one group share the sample, so their correlation vectors
  # are dependent; the measure contrast CI is only right if the Monte Carlo
  # draws preserve that joint distribution (independent draws would inflate it)
  set.seed(333)
  bt <- ssm_analyze(jz2017, scales = PANO(), measures = c("NARPD", "ASPD"),
                    contrast = TRUE, boots = 2000)
  set.seed(444)
  mc <- ssm_analyze(jz2017, scales = PANO(), measures = c("NARPD", "ASPD"),
                    contrast = TRUE, boots = 2000, method = "montecarlo")

  expect_equal(mc$results$e_est, bt$results$e_est, tolerance = 1e-12)
  expect_ci_agreement(mc, bt)

  # Contrast displacement: estimate must sit inside its own CI
  i <- nrow(mc$results)
  expect_true(mc$results$d_lci[i] <= mc$results$d_est[i] &&
                mc$results$d_est[i] <= mc$results$d_uci[i])
})

test_that("Monte Carlo matches bootstrap: mean-based group contrast (jz2017)", {
  skip_on_cran()
  data("jz2017")

  set.seed(555)
  bt <- ssm_analyze(jz2017, scales = PANO(), grouping = "Gender",
                    contrast = TRUE, boots = 2000)
  set.seed(666)
  mc <- ssm_analyze(jz2017, scales = PANO(), grouping = "Gender",
                    contrast = TRUE, boots = 2000, method = "montecarlo")

  expect_equal(mc$results$e_est, bt$results$e_est, tolerance = 1e-12)
  expect_ci_agreement(mc, bt)
})

test_that("Monte Carlo displacement CI straddles the 0/360 boundary cleanly", {
  skip_on_cran()
  rad <- as.numeric(as_radian(octants()))
  set.seed(77)
  dat <- as.data.frame(t(sapply(1:100, function(i) {
    1 + 2 * cos(rad) + rnorm(8, 0, 1.5) # true displacement 0 == 360
  })))
  colnames(dat) <- PANO()

  set.seed(78)
  mc <- ssm_analyze(dat, scales = PANO(), boots = 2000, method = "montecarlo")
  r <- mc$results

  # Estimate near the boundary, on [0, 360)
  expect_lt(min(abs(deg_dist(r$d_est, 0))), 15)
  # Estimate inside its CI, circularly
  span <- (r$d_uci - r$d_lci) %% 360
  offset <- (r$d_est - r$d_lci) %% 360
  expect_lte(offset, span)
  # And the interval is tight, not a wrapped-around artifact
  expect_lt(span, 90)
})

test_that("Monte Carlo contrast displacement near +/-180 stays on the
           estimate's branch", {
  skip_on_cran()
  rad <- as.numeric(as_radian(octants()))
  set.seed(70)
  A <- t(sapply(1:12, function(i) 1 + 2 * cos(rad - 90 * pi / 180) + rnorm(8, 0, 1.5)))
  B <- t(sapply(1:12, function(i) 1 + 2 * cos(rad - 270 * pi / 180) + rnorm(8, 0, 1.5)))
  dat <- as.data.frame(rbind(A, B))
  colnames(dat) <- PANO()
  dat$Group <- rep(c("A", "B"), each = 12)

  set.seed(5070)
  mc <- suppressWarnings(ssm_analyze(
    dat, scales = 1:8, grouping = "Group", contrast = TRUE, boots = 2000,
    method = "montecarlo"
  ))
  r <- mc$results[nrow(mc$results), ]

  expect_true(r$d_est > -180 && r$d_est <= 180)
  expect_true(r$d_lci <= r$d_est && r$d_est <= r$d_uci)
  expect_lt(r$d_uci - r$d_lci, 120)
  expect_lt(abs(abs(r$d_est) - 180), 15)
})

test_that("Monte Carlo handles flat data and singular covariance", {
  # Fully flat data: zero covariance, all draws degenerate
  dat <- as.data.frame(matrix(1, nrow = 20, ncol = 8))
  colnames(dat) <- PANO()
  w <- capture_warnings(
    res <- ssm_analyze(dat, scales = 1:8, boots = 20, method = "montecarlo")
  )
  expect_true(any(grepl("flat|degenerate", w)))
  expect_equal(res$results$e_est, 1)
  expect_true(is.na(res$results$d_est))

  # Ipsatized scales are sum-constrained (singular covariance); the sampler
  # must be PSD-safe rather than relying on a Cholesky factor
  skip_on_cran()
  data("jz2017")
  ips <- ipsatize(jz2017, items = PANO())
  set.seed(88)
  res_ips <- ssm_analyze(ips, scales = paste0(PANO(), "_i"), boots = 500,
                         method = "montecarlo")
  expect_true(all(is.finite(res_ips$results$e_lci)))
  expect_true(all(is.finite(res_ips$results$a_uci)))
})

test_that("Monte Carlo with missing data requires listwise deletion", {
  data("jz2017")
  jz <- jz2017[1:100, ]
  jz$PA[3] <- NA

  # Pairwise deletion + missing values: no valid asymptotic covariance
  expect_error(
    ssm_analyze(jz, scales = PANO(), boots = 50, listwise = FALSE,
                method = "montecarlo"),
    "listwise"
  )
  # Listwise deletion works (NA rows removed upstream)
  set.seed(99)
  expect_no_error(
    ssm_analyze(jz, scales = PANO(), boots = 50, method = "montecarlo")
  )
  # And pairwise without any actual missingness is fine
  set.seed(100)
  expect_no_error(
    ssm_analyze(jz2017[1:100, ], scales = PANO(), boots = 50,
                listwise = FALSE, method = "montecarlo")
  )
})

test_that("seeded Monte Carlo output matches the pre-Z1 pin (engine refactor guard)", {
  # Captured 2026-07-07, immediately before the Z1 MC-engine efficiency
  # refactor (vectorized psi, batched group_parameters, name-driven block
  # indexing). Those changes must not alter any value: same draws, same
  # arithmetic per element. Tolerance covers cross-platform BLAS ulp noise
  # only; on the capture machine the refactor is byte-identical.
  fixture <- readRDS(test_path("fixtures", "mc-seeded-pins.rds"))
  data("jz2017")
  num <- function(x) unlist(x$results[sapply(x$results, is.numeric)])

  set.seed(2026)
  mc1 <- ssm_analyze(jz2017[1:150, ], scales = PANO(), boots = 100,
                     method = "montecarlo")
  expect_equal(num(mc1), fixture$mc1, tolerance = 1e-10)

  set.seed(2026)
  mc2 <- ssm_analyze(jz2017[1:150, ], scales = PANO(),
                     measures = c("NARPD", "ASPD"), contrast = TRUE,
                     boots = 100, method = "montecarlo")
  expect_equal(num(mc2), fixture$mc2, tolerance = 1e-10)

  set.seed(2026)
  mc3 <- ssm_analyze(jz2017[1:150, ], scales = PANO(), grouping = "Gender",
                     contrast = TRUE, boots = 100, method = "montecarlo")
  expect_equal(num(mc3), fixture$mc3, tolerance = 1e-10)
})

test_that("Monte Carlo results are reproducible with a seed", {
  data("aw2009")
  set.seed(42)
  r1 <- ssm_analyze(aw2009, scales = 1:8, boots = 100, method = "montecarlo")
  set.seed(42)
  r2 <- ssm_analyze(aw2009, scales = 1:8, boots = 100, method = "montecarlo")
  expect_identical(r1$results, r2$results)
})

test_that("Monte Carlo contrast at an exact half-turn reports +180 inside its CI (F3)", {
  # Same exact-half-turn construction as the bootstrap F3 test: the Monte
  # Carlo engine shares param_diff()/angle_dist() for its observed contrast,
  # so the +180 convention and the estimate<->CI branch alignment must hold
  # there too.
  set.seed(42)
  base <- matrix(rnorm(50 * 8), 50, 8) %*% diag(1:8 / 4)
  dat <- data.frame(rbind(base, -base))
  names(dat) <- paste0("S", 1:8)
  dat$G <- rep(c("a", "b"), each = 50)

  set.seed(24)
  res <- suppressWarnings(ssm_analyze(
    dat, scales = paste0("S", 1:8), grouping = "G", contrast = TRUE,
    boots = 500, method = "montecarlo"
  ))
  r <- res$results[nrow(res$results), ]

  expect_true(r$d_est > -180 && r$d_est <= 180)
  expect_equal(abs(r$d_est), 180)
  # This seed lands on the bit-exact atom (+180); the +sign is seed-specific
  # (other seeds can leave the half-turn 1-2 ulp off, reporting -179.9999...deg).
  expect_gt(r$d_est, 0)
  expect_true(r$d_lci <= r$d_est && r$d_est <= r$d_uci)
})
