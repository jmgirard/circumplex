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

# Criterion statistics (A&R Eqs. 2, 6, 8, 9) -----------------------------------
# Oracles are closed forms derived independently in each test; psych::circ.tests
# is *not* used as an oracle because it reproduces the draft bugs these
# functions fix (no wrap-around gap, sign*acos angles, the x[0] indexing no-op,
# a quarter-period rotation grid, and scalar-total VT normalization).

# Loadings matrix for variables at the given angles (degrees) and radii.
loadings_at <- function(deg, radius = 1) {
  rad <- deg * pi / 180
  cbind(radius * cos(rad), radius * sin(rad))
}

test_that("structure_fisher is the CV of vector lengths", {
  # Constant radius: zero scatter regardless of spacing.
  expect_equal(structure_fisher(loadings_at(seq(45, 360, by = 45), 0.8)), 0)
  # Unequal radii: matches the hand-computed CV of the vector lengths
  # sqrt(h2). This is the A&R prose scale, not their Eq. 6 as printed (CV of
  # h2): the T2 sanity gate showed only the vector-length scale reproduces
  # the published .10/.15 cutoffs, so the cutoffs attach to this quantity.
  l <- loadings_at(c(0, 90, 180, 270), radius = c(1, 1, 0.5, 0.5))
  h <- c(1, 1, 0.5, 0.5)
  expect_equal(structure_fisher(l), stats::sd(h) / mean(h))
  # Invariant to rotation of the factor pair (communalities are).
  expect_equal(structure_fisher(rotate_loadings(l, 17 * pi / 180)), structure_fisher(l))
})

test_that("structure_gap includes the wrap-around gap (A&R Eq. 2)", {
  # Equally spaced octants: all gaps equal, so the variance is zero.
  expect_equal(structure_gap(loadings_at(seq(45, 360, by = 45))), 0)
  # All variables crowded into the first quadrant: the wrap-around gap
  # (350 -> 10, crossing 0/360) is by far the largest gap. Code that only
  # takes diff(sort(theta)) sees eight equal 10-degree gaps and returns 0,
  # certifying a quarter-circle as perfectly spaced.
  l <- loadings_at(seq(10, 80, by = 10))
  gaps <- c(rep(10, 7), 290) * pi / 180
  expect_equal(structure_gap(l), stats::var(gaps))
  expect_gt(structure_gap(l), 0.5)
  # Invariant to row order and to rotation of the factor pair. (Fixed
  # permutation: no RNG side effect on the suite's global stream.)
  expect_equal(structure_gap(l[c(5, 3, 8, 1, 7, 2, 6, 4), ]), structure_gap(l))
  expect_equal(structure_gap(rotate_loadings(l, 33 * pi / 180)), structure_gap(l))
})

test_that("structure_gap angles are exact on the axes (sign*acos regression)", {
  # Loadings with EXACT zero coordinates at 0/90/180/270 degrees. This is the
  # binding form of the boundary guard: cos/sin-built loadings carry
  # sin(pi) = 1.2e-16, which the old sign(l2)*acos(l1/h) recovery survives by
  # accident (sign(1.2e-16) = 1), so only exact zeros expose it. On this
  # input the buggy recovery collapses 180 degrees to 0 and yields gap
  # variance 0.176; the correct answer for equally spaced octants is 0.
  s <- sqrt(2) / 2
  l <- rbind(
    c(1, 0), c(s, s), c(0, 1), c(-s, s),
    c(-1, 0), c(-s, -s), c(0, -1), c(s, -s)
  )
  expect_equal(structure_gap(l), 0)
})

test_that("degenerate loadings return NA_real_, never NaN", {
  l <- loadings_at(seq(45, 360, by = 45))
  l[3, ] <- 0 # no defined angle for one variable
  expect_true(is.na(structure_gap(l)))
  expect_true(is.na(structure_vt(l)))
  # Fisher and RT remain defined (neither needs a per-variable angle).
  expect_true(is.finite(structure_fisher(l)))
  expect_true(is.finite(structure_rt(l)))
  # Whole-solution degeneracy: all four are NA (and specifically not NaN).
  flat <- matrix(0, 8, 2)
  for (f in list(structure_fisher, structure_gap, structure_vt, structure_rt)) {
    expect_identical(f(flat), NA_real_)
  }
  # Noise-level communalities (paf2 can clip both eigenvalues to ~0): a CV of
  # floating-point noise must not be reported as a real Fisher value.
  expect_identical(structure_fisher(matrix(1e-9, 8, 2)), NA_real_)
  # Constant Y across variables (a bipolar pair of scales): VT is 0/0.
  expect_identical(structure_vt(rbind(c(-0.975, 0), c(0.975, 0))), NA_real_)
})

test_that("structure_vt matches A&R Eq. 8 closed forms on the full-period grid", {
  # Perfect equally spaced circumplex: var_v(cos^2(theta_v - theta)) is the
  # same for every rotation, so the CV over rotations is exactly zero.
  expect_equal(structure_vt(loadings_at(seq(45, 360, by = 45), 0.7)), 0)
  # Axis-aligned simple structure (two unit variables per axis):
  # Y_vtheta = cos^2(alpha_v - theta) takes value cos^2(theta) for the four
  # variables on the x-axis and sin^2(theta) for the four on the y-axis, so
  # var_v(Y) = 2 cos^2(2 theta) / 7 and the CV over the 0-175 grid follows.
  l <- loadings_at(rep(c(0, 90, 180, 270), each = 2))
  th <- seq(0, 175, by = 5) * pi / 180
  x <- 2 * cos(2 * th)^2 / 7
  expect_equal(structure_vt(l), stats::sd(x) / mean(x))
  # Full-period grid makes the statistic orientation-invariant (the draft's
  # 0-45 degree grid depended on the arbitrary PA orientation). The rotation
  # matrix is built inline deliberately: structure_vt rotates internally via
  # rotate_loadings, so using the same helper here would pass vacuously if
  # rotate_loadings ignored its angle.
  rot <- 13 * pi / 180
  rotmat <- cbind(c(cos(rot), sin(rot)), c(-sin(rot), cos(rot)))
  expect_equal(structure_vt(l %*% rotmat), structure_vt(l))
})

test_that("structure_rt matches A&R Eq. 9 closed forms on the full-period grid", {
  # Perfect equally spaced circumplex: sum_v cos^2(2(alpha_v - theta)) is
  # constant over rotations, so RT is exactly zero.
  expect_equal(structure_rt(loadings_at(seq(45, 360, by = 45), 0.7)), 0)
  # Axis-aligned simple structure: sigma_vtheta^2 = cos^2(2 theta) / 2 for
  # every unit variable on an axis, so X_theta = 4 cos^2(2 theta) for 8
  # variables and the CV over the 0-85 grid follows.
  l <- loadings_at(rep(c(0, 90, 180, 270), each = 2))
  th <- seq(0, 85, by = 5) * pi / 180
  x <- 4 * cos(2 * th)^2
  expect_equal(structure_rt(l), stats::sd(x) / mean(x))
  # Orientation invariance on the full-period grid (inline rotation matrix
  # for the same independence reason as in the structure_vt test).
  rot <- 29 * pi / 180
  rotmat <- cbind(c(cos(rot), sin(rot)), c(-sin(rot), cos(rot)))
  expect_equal(structure_rt(l %*% rotmat), structure_rt(l))
})

test_that("stored nv = 8 cutoffs match the committed derivation record", {
  # The (slimmed) derivation record is committed in data-raw/, which ships in
  # the repository but not in the installed package, so this pin runs for
  # source checkouts (devtools::test) and skips on R CMD check.
  rds <- testthat::test_path("..", "..", "data-raw", "structure-test-cutoffs.rds")
  skip_if(!file.exists(rds), "derivation record not available")
  record <- readRDS(rds)
  expect_equal(as.numeric(record$seed), 20260707)
  expect_identical(record$reading, "standardized")
  for (st in names(structure_cutoffs[["8"]])) {
    for (scoring in c("raw", "deviation")) {
      expect_equal(
        structure_cutoffs[["8"]][[st]][[scoring]],
        round(record$nv8_cutoffs[paste(st, scoring, sep = "."), ], 2),
        info = paste(st, scoring)
      )
    }
  }
})

test_that("criterion statistics separate circumplex from simple structure", {
  circ <- loadings_at(seq(45, 360, by = 45), 0.7)
  simple <- loadings_at(rep(c(0, 90, 180, 270), each = 2), 0.7)
  expect_lt(structure_gap(circ), structure_gap(simple))
  expect_lt(structure_vt(circ), structure_vt(simple))
  expect_lt(structure_rt(circ), structure_rt(simple))
})
