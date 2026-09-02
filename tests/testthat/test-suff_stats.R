# Sufficient-statistics storage for the CI-accuracy diagnostic (spec
# devel/m4-ci-accuracy-spec.md Section 8.3; MILESTONES task Z0). These tests pin
# the details$suff_stats contract and the data = fallback for old objects.

scales8 <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")

test_that("mean-based path stores per-group n, SDs, and scale correlations", {
  skip_on_cran()
  data("jz2017")
  set.seed(1)
  res <- ssm_analyze(jz2017, scales = scales8, grouping = "Gender")

  ss <- res$details$suff_stats
  expect_type(ss, "list")
  expect_named(ss, c("n", "sds", "cormats"))

  groups <- levels(factor(jz2017$Gender))
  expect_named(ss$n, groups)
  expect_named(ss$sds, groups)
  expect_named(ss$cormats, groups)

  for (g in groups) {
    block <- jz2017[jz2017$Gender == g & !is.na(jz2017$Gender), scales8]
    block <- block[stats::complete.cases(block), , drop = FALSE]
    expect_equal(ss$n[[g]], nrow(block))
    expect_equal(ss$sds[[g]], apply(block, 2, stats::sd), ignore_attr = FALSE)
    expect_equal(ss$cormats[[g]], stats::cor(block))
    expect_equal(dim(ss$cormats[[g]]), c(8L, 8L))
    expect_equal(colnames(ss$cormats[[g]]), scales8)
  }
})

test_that("correlation-based path stores joint (scales + measures) matrices", {
  skip_on_cran()
  data("jz2017")
  set.seed(1)
  res <- ssm_analyze(
    jz2017, scales = scales8, measures = c("NARPD", "ASPD")
  )

  ss <- res$details$suff_stats
  expect_named(ss, c("n", "sds", "cormats"))
  # No SDs on the correlation path
  expect_null(ss$sds)

  expect_named(ss$n, "All")
  jointvars <- c(scales8, "NARPD", "ASPD")
  block <- jz2017[, jointvars]
  block <- block[stats::complete.cases(block), , drop = FALSE]
  expect_equal(ss$n[["All"]], nrow(block))
  expect_equal(dim(ss$cormats[["All"]]), c(10L, 10L))
  expect_equal(colnames(ss$cormats[["All"]]), jointvars)
  expect_equal(ss$cormats[["All"]], stats::cor(block))
})

test_that("contrast objects key sufficient stats by the real groups only", {
  skip_on_cran()
  data("jz2017")
  set.seed(1)
  res <- ssm_analyze(
    jz2017, scales = scales8, grouping = "Gender", contrast = TRUE
  )
  ss <- res$details$suff_stats
  # Two real groups, no contrast pseudo-group
  expect_equal(length(ss$n), 2L)
  expect_equal(names(ss$n), levels(factor(jz2017$Gender)))
})

test_that("stored profiles reproduce the object's scores (internal consistency)", {
  skip_on_cran()
  data("jz2017")
  set.seed(1)
  res <- ssm_analyze(jz2017, scales = scales8, grouping = "Gender")
  recomputed <- ssm_compute_suff_stats(
    jz2017, scales = scales8, grouping = "Gender", compute_profiles = TRUE
  )
  stored <- as.matrix(res$scores[, scales8, drop = FALSE])
  expect_equal(recomputed$profiles, stored, ignore_attr = TRUE)
})

test_that("ssm_suff_stats() returns stored stats when present", {
  skip_on_cran()
  data("jz2017")
  set.seed(1)
  res <- ssm_analyze(jz2017, scales = scales8, grouping = "Gender")
  expect_identical(ssm_suff_stats(res), res$details$suff_stats)
})

test_that("data = fallback reconstructs stats for objects predating storage", {
  skip_on_cran()
  data("jz2017")
  set.seed(1)
  res <- ssm_analyze(jz2017, scales = scales8, grouping = "Gender")
  target <- res$details$suff_stats

  # Simulate an old object: drop the stored sufficient statistics
  old <- res
  old$details$suff_stats <- NULL

  # No data supplied: informative error
  expect_error(ssm_suff_stats(old), "predates")

  # With the original data: exact reconstruction
  recovered <- ssm_suff_stats(old, data = jz2017)
  expect_equal(recovered, target)
})

test_that("fallback consistency check rejects the wrong dataset", {
  skip_on_cran()
  data("jz2017")
  set.seed(1)
  res <- ssm_analyze(jz2017, scales = scales8, grouping = "Gender")
  old <- res
  old$details$suff_stats <- NULL

  # Perturb the data so recomputed profiles no longer match the stored scores
  wrong <- jz2017
  wrong$PA <- wrong$PA + 1
  expect_error(ssm_suff_stats(old, data = wrong), "inconsistent")
})

test_that("fallback accepts the correct data under listwise deletion with NAs", {
  skip_on_cran()
  # Regression: ssm_compute_suff_stats() must replicate ssm_analyze()'s up-front
  # listwise na.omit before the C++ estimator, or recomputed profiles go NaN and
  # the consistency check rejects the genuine dataset.
  data("jz2017")
  d <- jz2017
  d$PA[1] <- NA
  d$BC[5] <- NA

  set.seed(1)
  res <- ssm_analyze(d, scales = scales8, grouping = "Gender")
  target <- res$details$suff_stats
  old <- res
  old$details$suff_stats <- NULL

  recovered <- ssm_suff_stats(old, data = d)
  expect_equal(recovered, target)

  # And the correlation path with NAs in a scale and a measure
  set.seed(1)
  resc <- ssm_analyze(d, scales = scales8, measures = c("NARPD", "ASPD"))
  oldc <- resc
  oldc$details$suff_stats <- NULL
  expect_equal(ssm_suff_stats(oldc, data = d), resc$details$suff_stats)
})

test_that("fallback works on the correlation path", {
  skip_on_cran()
  data("jz2017")
  set.seed(1)
  res <- ssm_analyze(
    jz2017, scales = scales8, measures = c("NARPD", "ASPD")
  )
  target <- res$details$suff_stats
  old <- res
  old$details$suff_stats <- NULL
  recovered <- ssm_suff_stats(old, data = jz2017)
  expect_equal(recovered, target)
})
