# Nesting of the free-scaling family in the unit family (M22; RR05 B2/R5).
#
# The free family nests the unit family (sigma = 1 recovers it, with an
# identical discrepancy value at the same theta/zeta/beta), so the minimized
# discrepancy must satisfy F_free <= F_unit on the same input R. Without the
# unit-solution seed this held only up to multi-start luck: the free engine
# could land on a worse optimum than the unit fit of the same R (3/5,751 used
# replicates in the M21 paired calibration, worst +5.52 T-units). The engine
# seeds the free battery with the accepted unit solution, enforcing nesting
# by construction (top-level fits only; bootstrap replicates keep their
# warm starts).

test_that("free fit never exceeds the unit fit's discrepancy (RR05 worst violator)", {
  skip_on_cran()
  # Provenance: replicate i = 29 of the M21 paired T-calibration cell
  # boundary_N2000 — generator devel/m21-t-calibration.R, seed formula
  # BASE_SEED + OFFSET + 1e6*cfg_idx + 1e4*N_idx + i with BASE_SEED =
  # 20260706, OFFSET = 12e7, cfg_idx = 1 (boundary), N_idx = 3 (N = 2000).
  # This replicate is RR05's recorded worst nesting violation (T_free -
  # T_unit = +5.52 pre-seed): red before the unit-solution seed, green after.
  p <- 8
  angles <- octants()
  angles_rad <- as.numeric(as_radian(as_degree(angles)))
  P0 <- cpm_implied_cor(angles_rad, rep(0.75, p), c(.45, .35, .15, .05))
  N <- 2000
  set.seed(20260706 + 12e7 + 1e6 * 1 + 1e4 * 3 + 29)
  X <- matrix(stats::rnorm(N * p), nrow = N) %*% chol(P0)
  R <- stats::cor(X)

  eu <- suppressWarnings(
    cpm_engine(R, angles = angles, m = 3, variant = "A", scaling = "unit")
  )
  ef <- suppressWarnings(
    cpm_engine(R, angles = angles, m = 3, variant = "A", scaling = "free")
  )

  # Nesting is well-posed only on the same spec: neither family polished.
  expect_length(eu$removed_harmonics, 0)
  expect_length(ef$removed_harmonics, 0)
  expect_identical(ef$df, eu$df)

  expect_lte(ef$F, eu$F + 1e-8)
})

test_that("nesting holds across a small deterministic battery (variants A and C)", {
  skip_on_cran()
  # Interior truth (M21 configs); N small enough to keep the battery quick.
  # Seeds are arbitrary but fixed; verified unpolished at write time so the
  # equal-df comparison stays well-posed.
  p <- 8
  angles <- octants()
  angles_rad <- as.numeric(as_radian(as_degree(angles)))
  P0 <- cpm_implied_cor(angles_rad, rep(0.75, p), c(.35, .30, .20, .15))
  N <- 500
  for (variant in c("A", "C")) {
    for (seed in c(101, 202)) {
      set.seed(seed)
      X <- matrix(stats::rnorm(N * p), nrow = N) %*% chol(P0)
      R <- stats::cor(X)
      eu <- suppressWarnings(
        cpm_engine(R, angles = angles, m = 3, variant = variant, scaling = "unit")
      )
      ef <- suppressWarnings(
        cpm_engine(R, angles = angles, m = 3, variant = variant, scaling = "free")
      )
      expect_length(eu$removed_harmonics, 0)
      expect_length(ef$removed_harmonics, 0)
      expect_identical(ef$df, eu$df)
      expect_lte(ef$F, eu$F + 1e-8)
    }
  }
})
