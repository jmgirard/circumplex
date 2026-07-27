# M65 axes_reliability(missing = "fiml"): the FIML correlation metric (T1).
#
# The whole path rests on one claim: the standardizing moments are the
# SATURATED-FIML (EM) means and SDs, never available-case scale() moments,
# which RR12 measured drifting +0.0167 (about one SE at N = 600) under MAR.
# On complete data the two coincide by construction, and that coincidence is
# what these tests pin -- it is the only place the FIML metric has a known
# exact answer to be checked against (RR12 BC2, BC6).
#
# Tolerances are RR12's, not calibrated here: 1e-12 elementwise. RR12 measured
# 8.9e-16 for R-hat; M65-D1's route measures 2.2e-15 / 1.1e-15, so the bar sits
# roughly three orders above the noise -- the M59/M61 discrimination rule, which
# asks for headroom rather than the tightest number one machine happens to print.

# One population, one seed, reused by every cell below. The 8 x 3 octant layout
# is RR12's probe population (BC10), so these fixtures and the evidence bar's
# fixtures are the same object rather than two things that look alike.
fiml_fixture <- function(n = 300L, k = 3L, seed = 7L) {
  oct <- octants()
  set.seed(seed)
  mat <- as.matrix(axes_simulate(n, oct, k, .35, .10, .08))
  colnames(mat) <- sprintf("item_%02d", seq_len(ncol(mat)))
  mat
}

test_that("BC2: on complete data the FIML metric reproduces scale() to 1e-12", {
  skip_if_not_installed("lavaan")
  mat <- fiml_fixture()
  mom <- axes_fiml_moments(mat)
  # The claim is elementwise on the standardized MATRIX, not on the moments --
  # a compensating pair of errors in the mean and the SD would leave both
  # moment vectors wrong and the matrix right, and it is the matrix that is fed
  # to lavaan, so the matrix is what the criterion fences.
  expect_lt(max(abs(mom$z - scale(mat))), 1e-12)
  # Stated separately so a failure says WHICH half moved.
  expect_lt(max(abs(mom$mean - colMeans(mat))), 1e-12)
  expect_lt(max(abs(mom$sd - apply(mat, 2, stats::sd))), 1e-12)
})

test_that("BC6: on complete data R-hat reproduces cor() to 1e-12", {
  skip_if_not_installed("lavaan")
  mat <- fiml_fixture()
  mom <- axes_fiml_moments(mat)
  expect_lt(max(abs(mom$R - stats::cor(mat))), 1e-12)
  # R-hat is a correlation matrix in its own right, not merely close to one.
  expect_lt(max(abs(diag(mom$R) - 1)), 1e-12)
  expect_true(isSymmetric(unname(mom$R), tol = 1e-12))
})

test_that("the N-1 rescaling is the thing that makes BC2 exact", {
  skip_if_not_installed("lavaan")
  # A mutation test written as a test, because the sqrt(N_used/(N_used - 1))
  # convention is a single factor that a refactor could drop silently: without
  # it the SDs are the ML ones and the standardized matrix misses scale() by
  # ~1/(2N), which at n = 300 is ~1.7e-3 -- nine orders above the 1e-12 bar, so
  # the criterion above genuinely detects its absence rather than tolerating it.
  mat <- fiml_fixture()
  mom <- axes_fiml_moments(mat)
  n <- nrow(mat)
  sd_ml <- mom$sd / sqrt(n / (n - 1))
  z_ml <- sweep(sweep(mat, 2, mom$mean, "-"), 2, sd_ml, "/")
  expect_gt(max(abs(z_ml - scale(mat))), 1e-6)
})

test_that("the FIML metric is NOT the available-case scale() metric", {
  skip_if_not_installed("lavaan")
  # The distinction RR12's whole ruling turns on. Under missingness the two
  # metrics must differ; if they ever coincide, the implementation has silently
  # fallen back to available-case moments and every MAR guarantee is void.
  mat <- fiml_fixture()
  set.seed(105)
  m <- mat
  m[runif(length(m)) < 0.10] <- NA
  mom <- axes_fiml_moments(m)
  ac_sd <- apply(m, 2, stats::sd, na.rm = TRUE)
  ac_mean <- colMeans(m, na.rm = TRUE)
  # Not merely unequal -- unequal by more than float noise, on both moments.
  expect_gt(max(abs(mom$mean - ac_mean)), 1e-8)
  expect_gt(max(abs(mom$sd - ac_sd)), 1e-8)
  # ... and R-hat differs from the pairwise-deletion correlation, which RR09
  # BC13 bans and which D-033 was careful to say R-hat is not.
  expect_gt(max(abs(mom$R - stats::cor(m, use = "pairwise.complete.obs"))), 1e-8)
})

test_that("axes_fiml_moments() reports the coverage diagnostics BC8 needs", {
  skip_if_not_installed("lavaan")
  mat <- fiml_fixture()
  set.seed(11)
  m <- mat
  m[runif(length(m)) < 0.10] <- NA
  m[1, ] <- NA_real_ # an all-missing row: dropped, excluded from N_used (BC7)
  mom <- axes_fiml_moments(m)
  expect_identical(mom$n_dropped, 1L)
  expect_identical(mom$n_used, nrow(m) - 1L)
  expect_identical(mom$n_complete, sum(stats::complete.cases(m)))
  # Minimum pairwise joint coverage over item PAIRS, not over items.
  co <- crossprod(!is.na(m[-1, , drop = FALSE]))
  expect_identical(mom$min_coverage, min(co[upper.tri(co)]))
})
