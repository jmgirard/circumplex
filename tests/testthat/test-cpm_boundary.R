# Boundary-condition suite for the free-scaling CPM path (M18).
#
# The four angular/boundary invariant classes (cairn/boundary-coverage.md):
#   A -- profile peaking exactly at 0/360 (angle lands on the pole)
#   B -- angle CI straddling 0/360
#   C -- contrasts near +/-180 (branch-cut agreement)
#   D -- flat / zero-variance profiles (graceful refusal, no crash)
#
# For the CPM free-scaling path (scaling = "free"), the applicable classes are
# A (pole) and D (flat/degenerate). Class C has no CPM estimand (there are no
# angular contrasts in a CPM fit). Class B is a property of the shared circular
# quantile machinery (quantile.circumplex_radian) that free scaling reuses
# unchanged -- the sigma block is orthogonal to the angle block -- so it is
# covered once on the unit path and not re-proved here (boundary-coverage.md
# "shared engine" convention).
#
# The last section is on the unit path: the start battery that the
# cpm_fit() help section "Which solution is reported" describes, and a lower
# discrepancy that battery does not reach but the lower-discrepancy check flags.
#
# Oracle rule unchanged: expected values are closed-form / construction /
# invariants, never from memory.

# ---- Class A: an angle exactly at the 0/360 pole, free scaling --------------

test_that("free scaling, class A: an in-family angle at the 0/360 pole recovers", {
  skip_on_cran()
  # Non-reference scale 2 sits at exactly 0 (the reference-relative pole), as in
  # the unit-path pole test. Data are exactly in the correlation family
  # (sigma = 1), so free scaling must recover sigma-hat = 1 AND report the pole
  # angle as ~0 or ~360 (DESIGN G2 / D-003), with the sigma block leaving the
  # angle handling untouched (spec sec. 5, pin 2).
  theta0 <- c(0, 0, 0.9, 1.8, 2.7, 3.6, 4.5, 5.4)
  zeta0 <- rep(0.75, 8)
  beta0 <- c(0.5, 0.3, 0.2)
  P0 <- cpm_implied_cor(theta0, zeta0, beta0)
  fit <- cpm_engine(P0, angles = theta0 * 180 / pi, m = 3, variant = "A",
                    reference = 1, scaling = "free")
  expect_lte(fit$F, 1e-8)
  expect_equal(fit$sigma, rep(1, 8), tolerance = 1e-6)
  a2 <- fit$theta[2]
  expect_true(min(abs(a2 - 0), abs(a2 - 360)) < 1e-3)
})

test_that("free scaling, class A: a genuine sigma != 1 pattern recovers at the pole", {
  skip_on_cran()
  # Inject non-trivial variance scales with a pole angle, then fit the resulting
  # covariance directly (engine level -- the cormat path requires unit-diagonal
  # input; here we test the estimator's pole+scale recovery). Rescale-
  # equivariance guarantees sigma-hat = sigma and the pole angle is unchanged.
  theta0 <- c(0, 0, 0.9, 1.8, 2.7, 3.6, 4.5, 5.4)
  zeta0 <- rep(0.75, 8)
  beta0 <- c(0.5, 0.3, 0.2)
  sigma0 <- c(1.3, 0.8, 1.1, 0.9, 1.2, 0.7, 1.0, 1.15)
  Sigma0 <- cpm_implied_cov(theta0, zeta0, beta0, sigma0)
  fit <- cpm_engine(Sigma0, angles = theta0 * 180 / pi, m = 3, variant = "A",
                    reference = 1, scaling = "free")
  expect_lte(fit$F, 1e-8)
  expect_equal(fit$sigma, sigma0, tolerance = 1e-5)
  a2 <- fit$theta[2]
  expect_true(min(abs(a2 - 0), abs(a2 - 360)) < 1e-3)
})

# ---- Class D: flat / zero-variance input, free scaling ----------------------

test_that("free scaling, class D: a singular correlation matrix is refused", {
  # A rank-deficient (flat) matrix has ln|R| = -Inf; the PD guard refuses it
  # BEFORE the scaling family matters, so free scaling refuses identically to
  # unit. Same clear error.
  Rsing <- matrix(1, 5, 5)
  expect_error(
    cpm_engine(Rsing, angles = c(0, 72, 144, 216, 288), m = 1, variant = "A",
               scaling = "free"),
    regexp = "positive definite|singular|PD"
  )
})

test_that("free scaling, class D: a zero-variance raw column is refused", {
  # A constant (zero-variance) scale column makes cor() undefined (NA); cpm_fit
  # must refuse rather than fit garbage, on the free path as on the unit path.
  set.seed(1)
  df <- as.data.frame(matrix(stats::rnorm(200 * 8), 200, 8))
  names(df) <- paste0("V", 1:8)
  df$V3 <- 5                                   # constant column
  expect_error(
    suppressWarnings(cpm_fit(df, scales = paste0("V", 1:8),
                             angles = octants(), scaling = "free")),
    regexp = "positive definite|singular|PD|NA|missing|constant|variance"
  )
})

test_that("free scaling, class D: a near-flat (tiny-eigenvalue) matrix is refused", {
  # Build an almost-singular correlation matrix (one eigenvalue ~1e-12) and
  # confirm the free path refuses it with the PD message (fail-closed, no crash
  # inside solve(Sigma)).
  p <- 6
  V <- qr.Q(qr(matrix(stats::rnorm(p * p), p)))
  ev <- c(rep(1, p - 1), 1e-12)
  M <- V %*% diag(ev) %*% t(V)
  d <- 1 / sqrt(diag(M))
  R <- d * M * rep(d, each = p)
  R <- (R + t(R)) / 2
  expect_error(
    cpm_engine(R, angles = 360 * (0:5) / 6, m = 1, variant = "A",
               scaling = "free"),
    regexp = "positive definite|singular|PD"
  )
})

# ---- Heywood-wall start: a lower discrepancy outside the start battery -----

test_that("the start battery has the size the help page states per model", {
  theta <- as.numeric(as_radian(as_degree(octants())))
  R <- cpm_implied_cor(theta, rep(0.75, 8), c(0.45, 0.35, 0.15, 0.05))
  sv <- cpm_start_values(R, theta, 3)
  n_starts <- function(variant) {
    spec <- cpm_spec(8, 3, variant)
    spec$theta_ref_val <- theta[1]
    spec$theta_fixed <- theta
    length(cpm_start_set(spec, theta, sv)$starts)
  }
  # theory, mirror and four offset patterns; or one start and three rescalings
  expect_identical(vapply(c("A", "C", "B", "D"), n_starts, integer(1)),
                   c(A = 6L, C = 6L, B = 4L, D = 4L))
})

test_that("at equal df a larger discrepancy gives a smaller p and worse RMSEA and CFI", {
  theta <- as.numeric(as_radian(as_degree(octants())))
  R <- cpm_implied_cor(theta, rep(0.75, 8), c(0.45, 0.35, 0.15, 0.05))
  ix <- function(Fhat) {
    cpm_fit_indices(Fhat, df = 10, p = 8, N = 250, R = R, Phat = R, q = 26)
  }
  lo <- ix(0.05)
  hi <- ix(0.15)
  expect_gt(hi$chisq, lo$chisq)
  expect_lt(hi$pvalue, lo$pvalue)
  expect_gt(hi$rmsea, lo$rmsea)
  expect_lt(hi$cfi, lo$cfi)
})

test_that("the lower-discrepancy check flags the lower F a wall start reaches", {
  # Replicate 30 of the boundary N = 250 cell of devel/m167-interval-oracle.R.
  # Skipped on CRAN: the basin nlminb() reaches can differ by platform.
  skip_on_cran()
  scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  P0 <- cpm_implied_cor(as.numeric(as_radian(as_degree(octants()))),
                        rep(0.75, 8), c(0.45, 0.35, 0.15, 0.05))
  truth <- cpm_fit(cormat = P0, scales = scales, n = 10000, m = 3)
  set.seed(21261736)
  X <- cpm_simulate(truth, 250)
  expect_no_warning(
    fit <- cpm_fit(X, scales = scales, m = 3, ci_method = "analytic")
  )
  expect_true(fit$details$accepted)
  expect_false(cpm_marker_labels()[["multimodal"]] %in%
                 cpm_boundary_markers(fit))

  # A communality-wall start on HI.
  d <- fit$details
  g <- d$par
  g[d$spec$i_zeta[match("HI", scales)]] <- stats::qlogis(0.99)
  lower <- cpm_optimize_one(g, fit$matrices$R, d$spec)
  expect_gt(fit$fit$chisq - fit$fit$n * lower$F, 1)

  expect_true(d$lower_check$found)
  expect_gt(d$lower_check$gap, 1)
})
