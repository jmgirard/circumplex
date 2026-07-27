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

test_that("axes_fiml_coverage() reports the diagnostics BC8 needs", {
  mat <- fiml_fixture()
  set.seed(11)
  m <- mat
  m[runif(length(m)) < 0.10] <- NA
  m[1, ] <- NA_real_ # an all-missing row: dropped, excluded from N_used (BC7)
  cov <- axes_fiml_coverage(m)
  expect_identical(cov$n_dropped, 1L)
  expect_identical(cov$n_used, nrow(m) - 1L)
  expect_false(cov$keep[[1]])
  expect_identical(cov$n_complete, sum(stats::complete.cases(m)))
  # Minimum pairwise joint coverage over item PAIRS, not over items.
  co <- crossprod(!is.na(m[-1, , drop = FALSE]))
  expect_identical(cov$min_coverage, min(co[upper.tri(co)]))
  # Per-item coverage is the diagonal, and is a DIFFERENT quantity -- with 10%
  # per-cell missingness it sits far above the pairwise minimum, so a test that
  # confused the two would not merely be imprecise, it would report a number
  # roughly twice too large.
  expect_identical(cov$item_n, diag(co))
  expect_gt(min(cov$item_n), cov$min_coverage)
})


# --- T2: the exported `missing =` argument and the one-stage FIML fit ---------
#
# T1 established the metric; T2 puts it behind the user-facing argument and
# feeds it to ONE structured lavaan fit. Three claims are separable and are
# tested separately: the listwise default is untouched (AC1), complete data
# makes the two paths agree (AC3), and the FIML fit is a genuine one-stage
# observed-information fit rather than a two-stage refit of a moment matrix
# (AC4, AC5).

# The 8 x 3 octant map the fixture's columns carry, for the exported function.
fiml_items <- function(mat) split(colnames(mat), rep(1:8, each = 3))

# The fixture with per-cell MCAR punched into it. Seeded separately from the
# population so a change to one does not silently reshuffle the other.
fiml_holes <- function(mat, rate = 0.10, seed = 105L) {
  set.seed(seed)
  mat[runif(length(mat)) < rate] <- NA
  mat
}

test_that("AC1: `missing` defaults to listwise and leaves that path alone", {
  skip_if_not_installed("lavaan")
  mat <- fiml_fixture()
  items <- fiml_items(mat)
  dat <- as.data.frame(mat)
  a <- suppressMessages(axes_reliability(dat, items = items, angles = octants()))
  b <- suppressMessages(
    axes_reliability(dat, items = items, angles = octants(),
                     missing = "listwise")
  )
  # identical(), not a tolerance: the default must not merely agree with the
  # explicit listwise call, it must BE that call. The pre-M65 tests in
  # test-axes-reliability.R carry the shipped numbers themselves, so their
  # passing is what pins bit-identity to what shipped; this pins the argument.
  a$call <- b$call <- NULL
  expect_identical(a, b)
  # match.arg() rather than a hand-rolled check, so a typo names the options.
  expect_error(
    axes_reliability(dat, items = items, angles = octants(), missing = "ml"),
    "should be one of"
  )
})

test_that("AC1: `missing = \"fiml\"` is refused on the cormat path", {
  skip_if_not_installed("lavaan")
  mat <- fiml_fixture()
  # Not one of BC7's six clauses, and deliberately so: a published correlation
  # matrix has no missing cells and no rows to run EM over, so the argument is
  # not merely unhelpful there, it names an estimator that cannot be run.
  expect_error(
    axes_reliability(
      cormat = stats::cor(mat), items = fiml_items(mat), angles = octants(),
      n = nrow(mat), missing = "fiml"
    ),
    "cormat"
  )
})

test_that("AC3: on complete data the two paths agree to 1e-8", {
  skip_if_not_installed("lavaan")
  # The FIML path's only exactly-known answer. Both paths see the same rows, so
  # any disagreement beyond float noise is the metric or the estimator moving,
  # not the data -- which makes this the criterion that would catch a silent
  # fallback to available-case moments on a dataset that has no NAs to hide it.
  mat <- fiml_fixture()
  dat <- as.data.frame(mat)
  items <- fiml_items(mat)
  lw <- suppressMessages(
    axes_reliability(dat, items = items, angles = octants(),
                     missing = "listwise")
  )
  fi <- suppressMessages(
    axes_reliability(dat, items = items, angles = octants(), missing = "fiml")
  )
  expect_equal(fi$components$Estimate, lw$components$Estimate,
               tolerance = 1e-8)
  expect_equal(fi$results$reliability, lw$results$reliability,
               tolerance = 1e-8)
  expect_equal(fi$results$sem, lw$results$sem, tolerance = 1e-8)
  expect_identical(fi$details$n, lw$details$n)
})

test_that("AC3: complete-data agreement holds with a fifth component too", {
  skip_if_not_installed("lavaan")
  # zeta2 is fitted only when the block map adds rank (M63), so the four-row
  # cell above never exercises it; without this the criterion's "(and zeta2 when
  # fitted)" clause would have no evidence at all.
  mat <- fiml_fixture()
  dat <- as.data.frame(mat)
  items <- fiml_items(mat)
  # One item from every scale per block: three blocks of eight.
  blocks <- split(colnames(mat), rep(1:3, times = 8))
  lw <- suppressMessages(
    axes_reliability(dat, items = items, angles = octants(), blocks = blocks,
                     missing = "listwise")
  )
  fi <- suppressMessages(
    axes_reliability(dat, items = items, angles = octants(), blocks = blocks,
                     missing = "fiml")
  )
  expect_true(lw$details$zeta2_fitted)
  expect_equal(fi$components$Estimate, lw$components$Estimate,
               tolerance = 1e-8)
})

test_that("AC4: the FIML fit uses observed information", {
  skip_if_not_installed("lavaan")
  # Asserted on the object axes_reliability() ACTUALLY fitted, captured through
  # the convergence seam, rather than on a parallel fit assembled by the test --
  # a parallel fit would keep passing if the exported function stopped passing
  # `missing` through, which is precisely the wiring this criterion fences.
  mat <- fiml_holes(fiml_fixture())
  captured <- NULL
  local_mocked_bindings(
    axes_converged = function(fit) {
      captured <<- fit
      TRUE
    }
  )
  suppressMessages(
    axes_reliability(as.data.frame(mat), items = fiml_items(mat),
                     angles = octants(), missing = "fiml")
  )
  opts <- lavaan::lavInspect(captured, "options")
  expect_identical(opts$information[1], "observed")
  # ... and the fiml -> "ml" translation went through sem_fit_cfa(), which is
  # the only place in the package that owns it.
  expect_identical(opts$missing, "ml")
  expect_true(lavaan::lavInspect(captured, "converged"))
})

test_that("AC4: the listwise fit is not silently promoted to FIML", {
  skip_if_not_installed("lavaan")
  # The other half of the seam: a mock that only ever sees "observed" would
  # pass above even if every fit used observed information. Fire the listwise
  # branch through the same capture and require the options to DIFFER.
  mat <- fiml_fixture()
  captured <- NULL
  local_mocked_bindings(
    axes_converged = function(fit) {
      captured <<- fit
      TRUE
    }
  )
  suppressMessages(
    axes_reliability(as.data.frame(mat), items = fiml_items(mat),
                     angles = octants(), missing = "listwise")
  )
  expect_identical(lavaan::lavInspect(captured, "options")$missing, "listwise")
})

test_that("AC5: no two-stage refit of R-hat reaches the reported results", {
  skip_if_not_installed("lavaan")
  mat <- fiml_holes(fiml_fixture())
  dat <- as.data.frame(mat)
  items <- fiml_items(mat)

  # (a) The banned route is not merely unused, it is unreachable: the cormat
  # fitter aborts if anything calls it, and the FIML path completes anyway.
  res <- local({
    local_mocked_bindings(
      axes_fit_cormat = function(...) {
        stop("the two-stage cormat fitter was called on the FIML path")
      }
    )
    suppressMessages(
      axes_reliability(dat, items = items, angles = octants(),
                       missing = "fiml")
    )
  })
  expect_false(res$results$boundary[[1]])

  # (b) A mock proves wiring only (the M62 lesson), so the numbers are checked
  # unmocked against the banned construction itself: fit R-hat as a covariance
  # matrix with sample.nobs = the TOTAL N, the two-stage route BC5 names. Its
  # chi-square and its component SEs must not be the reported ones. They differ
  # because that route claims N complete respondents' worth of information for
  # moments estimated from a fraction of that.
  cov <- axes_fiml_coverage(mat)
  mom <- axes_fiml_moments(mat[cov$keep, , drop = FALSE])
  two_stage <- axes_fit_cormat(mom$R, items, octants(), n = nrow(mat))
  expect_gt(
    abs(res$fit$chisq - unname(lavaan::fitMeasures(two_stage, "chisq"))), 1e-4
  )
  pe <- lavaan::parameterEstimates(two_stage)
  ts_se <- pe$se[pe$op == "~~" & pe$lhs == "AX" & pe$rhs == "AX"][[1]]
  ax_se <- res$components$SE[res$components$Symbol == "xi1"]
  expect_gt(abs(ax_se - ts_se), 1e-6)
  # The point estimates, by contrast, are close -- both consume the same R-hat.
  # Stated so the SE difference above reads as an information claim rather than
  # as the two routes simply estimating different things.
  ts_est <- pe$est[pe$op == "~~" & pe$lhs == "AX" & pe$rhs == "AX"][[1]]
  expect_lt(abs(res$results$xi1[[1]] - ts_est), 0.01)
})
