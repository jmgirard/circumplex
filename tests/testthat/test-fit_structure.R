# Shared infrastructure for the Acton & Revelle (2004) circumplex structure
# tests: base-R principal-axis loadings and ridge-on-the-correlation-matrix
# repair. See R/fit_structure.R.

octants_jz <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")

# paf2(): psych-independent correctness ---------------------------------------

test_that("paf2 recovers an exact two-factor correlation matrix", {
  # Build a correlation matrix that is exactly rank-2-plus-uniqueness: PAF must
  # recover the true communalities (which are rotation-invariant, so they do
  # not depend on the arbitrary principal-axis orientation) essentially exactly.
  ang <- (0:7) * (2 * pi / 8)
  lambda_true <- cbind(0.8 * cos(ang), 0.8 * sin(ang)) # all communalities 0.64
  u <- 1 - rowSums(lambda_true^2)
  r <- lambda_true %*% t(lambda_true)
  diag(r) <- diag(r) + u

  lambda <- paf2(r)
  expect_equal(rowSums(lambda^2), rep(0.64, 8), tolerance = 1e-3)
  # The two-factor model reproduces the off-diagonal correlations.
  fitted <- lambda %*% t(lambda)
  expect_equal(fitted[upper.tri(fitted)], r[upper.tri(r)], tolerance = 1e-3)
})

test_that("paf2 loadings are self-consistent at the returned solution", {
  data("jz2017")
  r <- stats::cor(as.matrix(jz2017[octants_jz]))
  lambda <- paf2(r)
  # At the PAF fixed point the reconstructed communalities equal the diagonal
  # that generated them, i.e. rowSums(lambda^2) reproduces the reduced-matrix
  # diagonal. This is a tolerance-light correctness property independent of any
  # external factor-analysis implementation.
  reduced <- r
  diag(reduced) <- rowSums(lambda^2)
  e <- eigen(reduced, symmetric = TRUE)
  recon <- e$vectors[, 1:2] %*% diag(sqrt(pmax(e$values[1:2], 0)))
  expect_equal(rowSums(recon^2), rowSums(lambda^2),
    tolerance = 1e-4, ignore_attr = TRUE)
})

# paf2(): psych oracle --------------------------------------------------------

test_that("paf2 matches psych::fa principal-axis loadings on reference data", {
  skip_if_not_installed("psych")
  data("jz2017")
  r <- stats::cor(as.matrix(jz2017[octants_jz]))

  lambda <- paf2(r)
  oracle <- suppressWarnings(
    unclass(psych::fa(r, nfactors = 2, rotate = "none", fm = "pa")$loadings)[, 1:2]
  )
  # Factor sign is arbitrary; align each column before comparing magnitudes.
  for (k in 1:2) {
    if (sum(lambda[, k] * oracle[, k]) < 0) oracle[, k] <- -oracle[, k]
  }
  expect_equal(unname(lambda), unname(oracle), tolerance = 0.01)
})

# structure_loadings(): ridge applied to the correlation matrix ---------------

test_that("structure_loadings with ridge = 0 factors the raw correlation matrix", {
  data("jz2017")
  r <- stats::cor(as.matrix(jz2017[octants_jz]))
  expect_equal(structure_loadings(jz2017, octants_jz, ridge = 0), paf2(r))
})

test_that("ridge is added to the correlation matrix, not the data", {
  data("jz2017")
  r <- stats::cor(as.matrix(jz2017[octants_jz]))
  ridge <- 0.1
  # Correct operation: R + ridge*I rescaled back to unit diagonal, then PAF.
  r_ridged <- r
  diag(r_ridged) <- diag(r_ridged) + ridge
  r_ridged <- stats::cov2cor(r_ridged)
  expect_equal(
    structure_loadings(jz2017, octants_jz, ridge = ridge),
    paf2(r_ridged)
  )
  # The buggy draft perturbed the first p rows of the raw data. Guard against a
  # regression to that behaviour: ridging must not depend on row order or n.
  shuffled <- jz2017[sample(nrow(jz2017)), ]
  expect_equal(
    structure_loadings(jz2017, octants_jz, ridge = ridge),
    structure_loadings(shuffled, octants_jz, ridge = ridge)
  )
})

test_that("ridge repairs a non-positive-definite (ipsatized) correlation matrix", {
  data("jz2017")
  # Deviation scoring (ipsatize) makes the octant scores sum to zero, so their
  # correlation matrix is singular (rank 7). Ridge on R restores definiteness.
  di <- ipsatize(jz2017, items = octants_jz, append = FALSE)
  ipsat <- paste0(octants_jz, "_i")
  r <- stats::cor(as.matrix(di[ipsat]))
  expect_lt(min(eigen(r, only.values = TRUE)$values), 1e-8)

  r_ridged <- stats::cov2cor(`diag<-`(r, diag(r) + 0.1))
  expect_gt(min(eigen(r_ridged, only.values = TRUE)$values), 0)
  # Loadings are finite and well-defined after the repair.
  lambda <- structure_loadings(di, ipsat, ridge = 0.1)
  expect_true(all(is.finite(lambda)))
  expect_equal(dim(lambda), c(8L, 2L))
})

# structure_loadings(): validation --------------------------------------------

test_that("structure_loadings validates its arguments", {
  data("jz2017")
  expect_error(structure_loadings(jz2017, octants_jz, ridge = -1))
  expect_error(structure_loadings(jz2017, octants_jz, ridge = c(0.1, 0.2)))
  expect_error(structure_loadings(jz2017, "PA")) # needs at least two scales
})
