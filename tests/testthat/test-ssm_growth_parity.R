# Cross-engine parity: nlme against the committed glmmTMB fixture (M151 AC4) -
#
# The fixture tests/testthat/fixtures/growth-fixef.rds holds the glmmTMB fit
# of the joint model on both growth datasets (generator: data-raw/growth-
# fixef.R). nlme's dialect of the same model is fit here on simulated_growth
# only, and its fixed effects and their covariance compared. The two engines maximize
# the same REML criterion, so the fixed effects agree to optimizer precision
# and the structural-zero covariance entries differ only by rounding noise
# in both; the covariance tolerance is therefore absolute, scaled by the
# largest entry.

fixture_names <- c("dve", "dvx", "dvy", "dve:wave", "dvx:wave", "dvy:wave")

test_that("the glmmTMB fixture carries both datasets and its provenance", {
  fx <- readRDS(test_path("fixtures", "growth-fixef.rds"))
  expect_setequal(names(fx), c("simulated_growth", "simulated_growth_origin",
                               "glmmTMB_version", "TMB_version", "provenance"))
  for (d in c("simulated_growth", "simulated_growth_origin")) {
    expect_identical(names(fx[[d]]$coef), fixture_names)
    expect_identical(dimnames(fx[[d]]$vcov), list(fixture_names, fixture_names))
    expect_true(isSymmetric(fx[[d]]$vcov))
  }
  expect_match(fx$provenance, "data-raw/growth-fixef.R")
  expect_match(fx$provenance, "20260716")
  expect_match(fx$glmmTMB_version, "^[0-9]+\\.[0-9]+")
  expect_match(fx$TMB_version, "^[0-9]+\\.[0-9]+")
})

test_that("nlme's fit of the pieces matches the glmmTMB fixture", {
  skip_if_not_installed("nlme")
  fx <- readRDS(test_path("fixtures", "growth-fixef.rds"))$simulated_growth
  data("simulated_growth")
  long <- ssm_growth_data(simulated_growth, scales = PANO(),
                          id = "person", time = "wave")
  gf <- ssm_growth_formula("nlme", time = "wave", id = "person")
  fit <- nlme::lme(
    fixed = gf$fixed,
    random = gf$random,
    weights = nlme::varIdent(form = gf$weights),
    data = long,
    method = "REML"
  )
  coef <- nlme::fixef(fit)
  vcov <- as.matrix(vcov(fit))

  expect_setequal(names(coef), fixture_names)
  coef <- coef[fixture_names]
  vcov <- vcov[fixture_names, fixture_names]

  # Fixed effects: absolute agreement within 1e-6 on every coefficient.
  gap_coef <- max(abs(coef - fx$coef))
  expect_lt(gap_coef, 1e-6)

  # Covariance: absolute agreement within 1e-4 times the largest entry. Two
  # things set the bound. The cross-coordinate slope entries, and the
  # cross-coordinate intercept-by-slope entries of this balanced design, are
  # structural zeros that
  # each engine reports as rounding noise of a different sign and size, so a
  # relative tolerance would fail on nothing. And the cross-coordinate
  # intercept entries sit where the REML criterion is flat: measured
  # 2026-09-24 (glmmTMB 1.1.15, nlme 3.1-171), the largest gap was 7e-6 of
  # the largest entry, at the e-by-x intercept entry, and refitting nlme at
  # tighter tolerances moved that entry by as much again.
  gap_vcov <- max(abs(vcov - fx$vcov))
  expect_lt(gap_vcov, 1e-4 * max(abs(fx$vcov)))

  # The same REML criterion (identical up to optimizer precision).
  expect_equal(as.numeric(stats::logLik(fit)), fx$logLik, tolerance = 1e-8)
})
