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
  # expect_identical(..., NA_real_) discriminates NA from NaN -- the degeneracy
  # policy promises NA, never NaN, and is.na() alone would pass on a NaN.
  expect_identical(structure_gap(l), NA_real_)
  expect_identical(structure_vt(l), NA_real_)
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

test_that("NA loadings propagate to NA_real_ statistics, never an error", {
  # A zero-variance scale (or a never-jointly-observed pair) makes cor() return
  # NA; structure_loadings/paf2 must hand back NA loadings and every criterion
  # must return NA_real_ rather than crashing in eigen() or an `if (any(NA))`.
  na_l <- matrix(NA_real_, 8, 2)
  for (f in list(structure_fisher, structure_gap, structure_vt, structure_rt)) {
    expect_identical(f(na_l), NA_real_)
  }
  # End to end: a constant scale reaches paf2 through cor() as an NA matrix.
  d <- as.data.frame(matrix(stats::rnorm(80), nrow = 10, ncol = 8))
  d[[3]] <- 5 # zero variance
  L <- suppressWarnings(structure_loadings(d, names(d)))
  expect_true(all(is.na(L)))
  expect_identical(dim(L), c(8L, 2L))
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

test_that("structure_rt is scale-invariant down to weak communalities (loadings^4 guard)", {
  # RT is a coefficient of variation, so rescaling every loading must leave it
  # unchanged. The degeneracy guard used to test the rotation profile on a
  # loadings^4 scale, so a valid-but-weak circumplex -- small communalities that
  # Fisher and VT still define -- was wrongly voided to NA. Guarding on the
  # communalities (loadings^2, the scale the other three tests use) restores the
  # invariance and the cross-test consistency.
  circ <- loadings_at(seq(45, 360, by = 45), 0.7)
  base <- structure_rt(circ)
  weak <- structure_rt(circ * 1e-4) # communalities ~ 5e-9, well above DEGEN_TOL
  expect_false(is.na(weak))
  expect_equal(weak, base, tolerance = 1e-8)
  # Fisher and VT stay defined on the same weak loadings, so RT must too.
  expect_false(is.na(structure_fisher(circ * 1e-4)))
  expect_false(is.na(structure_vt(circ * 1e-4)))
  # A genuinely flat solution is still NA (never NaN), exactly as before.
  expect_identical(structure_rt(matrix(0, 8, 2)), NA_real_)
})

test_that("stored cutoffs are strictly ordered almost < thrice < twice", {
  # structure_interpret walks almost -> thrice -> twice and reports the first
  # band a statistic clears, so a mis-ordered triple would make a band
  # unreachable. Assert the invariant over every stored nv/test/scoring (the
  # derivation script asserts the same before writing these constants).
  for (nv in names(structure_cutoffs)) {
    for (test in names(structure_cutoffs[[nv]])) {
      for (scoring in names(structure_cutoffs[[nv]][[test]])) {
        cuts <- structure_cutoffs[[nv]][[test]][[scoring]]
        expect_true(
          cuts[["almost"]] < cuts[["thrice"]] &&
            cuts[["thrice"]] < cuts[["twice"]],
          info = paste(nv, test, scoring)
        )
      }
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

# Interpretation layer (T3): scoring-keyed, nv-specific cutoff classification --

test_that("structure_interpret classifies against the scoring-keyed cutoffs", {
  # fisher nv = 8: raw = (.10, .13, .15); deviation = (.07, .12, .15).
  expect_identical(structure_interpret(0.05, "fisher", 8, "raw")$category, "almost")
  expect_identical(structure_interpret(0.20, "fisher", 8, "raw")$category, "weak")
  # A statistic of 0.09 is classified differently under the two declared
  # scorings: below the raw "almost" cutoff (.10) but past the deviation one
  # (.07). This is the interpretive trap the method review flags -- the same
  # number means different things depending on whether the data were ipsatized.
  expect_identical(structure_interpret(0.09, "fisher", 8, "raw")$category, "almost")
  expect_identical(structure_interpret(0.09, "fisher", 8, "deviation")$category, "thrice")
  # The returned cutoffs are the ones actually used for the classification.
  expect_identical(
    structure_interpret(0.09, "fisher", 8, "deviation")$cutoffs,
    structure_cutoffs[["8"]]$fisher$deviation
  )
})

test_that("structure_interpret refuses to classify at an uncalibrated nv", {
  # Only nv = 8 is calibrated; A&R's nv effect means the nv = 64/128 cutoffs
  # must NOT be silently applied to another scale count. No cutoffs -> no
  # category, but the statistic itself is untouched by the caller.
  res <- structure_interpret(0.05, "fisher", 6, "raw")
  expect_null(res$cutoffs)
  expect_true(is.na(res$category))
})

test_that("structure_interpret propagates a degenerate (NA) statistic", {
  res <- structure_interpret(NA_real_, "fisher", 8, "raw")
  expect_true(is.na(res$category))
  # Cutoffs are still reported (they exist at nv = 8); only the class is undefined.
  expect_identical(res$cutoffs, structure_cutoffs[["8"]]$fisher$raw)
})

test_that("structure_fisher_test wraps the internal statistic and interpretation", {
  data("jz2017")
  res <- structure_fisher_test(jz2017, octants_jz, scoring = "raw")
  # The statistic is exactly the internal CV of vector lengths on the same
  # loadings -- the wrapper adds interpretation, it does not recompute.
  expect_equal(res$statistic, structure_fisher(structure_loadings(jz2017, octants_jz)))
  expect_identical(res$test, "fisher")
  expect_identical(res$scoring, "raw")
  expect_identical(res$nv, 8L)
  # jz2017 raw octant scales carry a general factor: Fisher ~ .29, well past the
  # raw "twice" cutoff, so equal axes are not clearly supported.
  expect_identical(res$category, "weak")
})

test_that("structure_fisher_test reflects deviation scoring on ipsatized data", {
  data("jz2017")
  di <- ipsatize(jz2017, items = octants_jz, append = FALSE)
  ip <- paste0(octants_jz, "_i")
  res <- structure_fisher_test(di, ip, scoring = "deviation", ridge = 0.1)
  # Removing the general factor by ipsatizing pulls the statistic down into the
  # supported range (~.10), which the deviation cutoffs read as equal axes being
  # at least three times as likely -- the power gain A&R attribute to deviation
  # scoring.
  expect_identical(res$category, "thrice")
})

test_that("structure_fisher_test validates the declared scoring", {
  data("jz2017")
  expect_error(structure_fisher_test(jz2017, octants_jz, scoring = "ipsative"))
})

# Gap test wrapper (T4): equal spacing, not equal axes -------------------------

test_that("structure_gap_test wraps the internal statistic and interpretation", {
  data("jz2017")
  res <- structure_gap_test(jz2017, octants_jz, scoring = "raw")
  # The statistic is exactly the internal gap variance on the same loadings --
  # the wrapper adds interpretation, it does not recompute (and it carries the
  # T2 wrap-around-gap fix, so a general factor is not mistaken for even spacing).
  expect_equal(res$statistic, structure_gap(structure_loadings(jz2017, octants_jz)))
  expect_identical(res$test, "gap")
  expect_identical(res$scoring, "raw")
  expect_identical(res$nv, 8L)
  # jz2017 raw octant scales carry a general factor: gap variance ~ 2.4 rad^2,
  # far past the raw "twice" cutoff, so even spacing is not clearly supported.
  expect_identical(res$category, "weak")
})

test_that("structure_gap_test reflects deviation scoring on ipsatized data", {
  data("jz2017")
  di <- ipsatize(jz2017, items = octants_jz, append = FALSE)
  ip <- paste0(octants_jz, "_i")
  res <- structure_gap_test(di, ip, scoring = "deviation", ridge = 0.1)
  # Ipsatizing collapses the gap variance from ~2.4 to ~.15, which the deviation
  # cutoffs read as even spacing being at least three times as likely. The
  # statistic sits just above the .15 "almost" cutoff, so a flip to "almost"
  # here would signal that the loadings (not the interpretation) changed.
  expect_identical(res$category, "thrice")
})

test_that("structure_gap_test validates the declared scoring", {
  data("jz2017")
  expect_error(structure_gap_test(jz2017, octants_jz, scoring = "ipsative"))
})

# Variance test (VT2) wrapper (T5) --------------------------------------------

test_that("structure_vt_test wraps the internal statistic and interpretation", {
  data("jz2017")
  res <- structure_vt_test(jz2017, octants_jz, scoring = "raw")
  # The statistic is exactly the internal VT2 (effective variant: squared
  # factor-1 loading over own communality, CV over a full-period rotation grid)
  # on the same loadings -- the wrapper adds interpretation, it does not
  # recompute.
  expect_equal(res$statistic, structure_vt(structure_loadings(jz2017, octants_jz)))
  expect_identical(res$test, "vt")
  expect_identical(res$scoring, "raw")
  expect_identical(res$nv, 8L)
  # jz2017 raw octant scales: VT ~ .38, just past the raw "twice" cutoff (.37),
  # so interstitiality is not clearly supported. (Near the cutoff by design:
  # a flip signals the loadings changed, not the interpretation.)
  expect_identical(res$category, "weak")
})

test_that("structure_vt_test reflects deviation scoring on ipsatized data", {
  data("jz2017")
  di <- ipsatize(jz2017, items = octants_jz, append = FALSE)
  ip <- paste0(octants_jz, "_i")
  res <- structure_vt_test(di, ip, scoring = "deviation", ridge = 0.1)
  # Ipsatizing pulls VT below the deviation "almost" cutoff (.19), so
  # interstitiality is almost certain -- A&R "strongly recommend" deviation
  # scoring for VT2 in every case, and this is why.
  expect_identical(res$category, "almost")
})

test_that("structure_vt_test validates the declared scoring", {
  data("jz2017")
  expect_error(structure_vt_test(jz2017, octants_jz, scoring = "ipsative"))
})

# Rotation test wrapper (T5) --------------------------------------------------

test_that("structure_rt_test wraps the internal statistic and interpretation", {
  data("jz2017")
  res <- structure_rt_test(jz2017, octants_jz, scoring = "raw")
  # The statistic is exactly the internal RT (quartimax-like sum, CV over the
  # full-period 0-85 grid -- the grid/label alignment that the draft's
  # criterion[0] indexing bug corrupted is pinned at the statistic level in the
  # closed-form structure_rt test above) on the same loadings.
  expect_equal(res$statistic, structure_rt(structure_loadings(jz2017, octants_jz)))
  expect_identical(res$test, "rt")
  expect_identical(res$scoring, "raw")
  expect_identical(res$nv, 8L)
  # jz2017 raw octant scales: RT ~ .56, well past the raw "twice" cutoff (.35).
  expect_identical(res$category, "weak")
})

test_that("structure_rt_test reflects deviation scoring on ipsatized data", {
  data("jz2017")
  di <- ipsatize(jz2017, items = octants_jz, append = FALSE)
  ip <- paste0(octants_jz, "_i")
  res <- structure_rt_test(di, ip, scoring = "deviation", ridge = 0.1)
  # Ipsatizing pulls RT to ~.33, just past the deviation "almost" cutoff (.32),
  # which reads as interstitiality at least three times as likely. (Near the
  # cutoff by design: a flip signals a loadings change.)
  expect_identical(res$category, "thrice")
})

test_that("structure_rt_test validates the declared scoring", {
  data("jz2017")
  expect_error(structure_rt_test(jz2017, octants_jz, scoring = "ipsative"))
})

# RANDALL correspondence index + randomization test (T6) -----------------------
# Unlike the four A&R criteria there are no simulated cutoffs here: the
# randomization test yields an exact (or Monte Carlo) p-value against the
# random-relabeling null, which is why A&R excluded RANDALL from their
# simulation (footnote 3). Oracles below are the draft's verified counting
# loop (index) and closed forms derived from the dihedral symmetry of the
# circle (p-values) -- nothing enters from memory.

# A data frame of nv scales whose *sample* correlations are exactly
# cos(angular difference): x_k = cos(a_k) f1 + sin(a_k) f2 with f1, f2
# exactly uncorrelated, equal-variance columns. Correlations then decrease
# strictly in circular distance -- a perfect circumplex.
perfect_circumplex_df <- function(nv) {
  f1 <- c(1, -1, 1, -1)
  f2 <- c(1, 1, -1, -1)
  ang <- (seq_len(nv) - 1) * 2 * pi / nv
  dat <- as.data.frame(lapply(ang, function(a) cos(a) * f1 + sin(a) * f2))
  names(dat) <- paste0("V", seq_len(nv))
  dat
}

test_that("structure_randall matches the draft's counting loop on jz2017", {
  data("jz2017")
  r <- stats::cor(as.matrix(jz2017[octants_jz]))
  # Independent oracle: the draft's counting machinery (devel/fit_analysis.R),
  # transcribed verbatim. Method-review §5 verified it computes the Hubert &
  # Arabie correspondence index with ties counted as violations.
  n_away <- function(a, b, nv) {
    s <- rep(1:nv, 2)
    min(abs(outer(which(s == a), which(s == b), "-")))
  }
  m <- matrix(NA, 8, 8)
  for (i in 1:8) for (j in 1:8) m[i, j] <- n_away(i, j, 8)
  hyp <- m[lower.tri(m)]
  vals <- r[lower.tri(r)]
  nc <- 0
  nt <- 0
  for (i in seq_along(vals)) {
    lr <- hyp > hyp[i]
    nc <- nc + sum(vals[i] > vals[lr])
    nt <- nt + sum(lr)
  }
  expect_equal(structure_randall(r), (nc / nt) - ((nt - nc) / nt))
  # nv = 8 has 288 order predictions (pairs of pair-slots with unequal
  # circular distance: 8/8/8/4 slots at distances 1-4).
  expect_identical(nt, 288)
})

test_that("structure_randall closed forms: perfect, anti-ordered, and tied matrices", {
  ang <- (0:7) * 2 * pi / 8
  # Perfect circumplex: correlations strictly decrease in circular distance,
  # so every prediction is correct.
  r_perf <- cos(outer(ang, ang, "-"))
  expect_equal(structure_randall(r_perf), 1)
  # Anti-ordered: correlations strictly *increase* in circular distance, so
  # every prediction is violated. (The index only reads the lower triangle,
  # so the -1 diagonal of -r_perf is immaterial.)
  expect_equal(structure_randall(-r_perf), -1)
  # Constant off-diagonal: every prediction is a tie, and ties count as
  # violations (the draft's convention, measure-zero for continuous data).
  r_tied <- matrix(0.5, 8, 8)
  diag(r_tied) <- 1
  expect_equal(structure_randall(r_tied), -1)
  # NA correlations (e.g. no jointly observed rows) give NA, never NaN.
  r_na <- r_tied
  r_na[2, 1] <- r_na[1, 2] <- NA
  expect_identical(structure_randall(r_na), NA_real_)
  # Fewer than 4 variables have no unequal-distance pairs, hence no
  # predictions: NA, not NaN.
  expect_identical(structure_randall(diag(3)), NA_real_)
})

test_that("structure_randall_test exact p equals the dihedral closed form", {
  # For a perfect circumplex the index is 1, and (by strict monotonicity in
  # distance) a relabeling reaches 1 iff it preserves every circular-distance
  # class; class-preserving relabelings of a cycle are exactly its graph
  # automorphisms, the dihedral group. With variable 1 held at position 1 the
  # enumeration quotients out rotations, leaving {identity, reflection through
  # position 1}: exact p = 2 / (nv - 1)!.
  dat8 <- perfect_circumplex_df(8)
  res8 <- structure_randall_test(dat8, names(dat8))
  expect_equal(res8$statistic, 1)
  expect_equal(res8$p_value, 2 / factorial(7))
  expect_identical(res8$method, "exact")
  expect_identical(res8$nv, 8L)
  # Same closed form at nv = 4 (6 relabelings, hand-checkable).
  dat4 <- perfect_circumplex_df(4)
  res4 <- structure_randall_test(dat4, names(dat4))
  expect_equal(res4$statistic, 1)
  expect_equal(res4$p_value, 2 / factorial(3))
  # Anti-ordered data (reverse the hypothesized order... flipping the sign of
  # every correlation at odd distance is not constructible from data, so use
  # the tied-matrix route instead): constant scales are degenerate, so instead
  # check the always-true lower bound: dihedral relabelings preserve the index
  # for ANY matrix (they permute values only within distance classes), so the
  # exact p can never fall below 2 / (nv - 1)!.
  data("jz2017")
  res_jz <- structure_randall_test(jz2017, octants_jz)
  expect_gte(res_jz$p_value, 2 / factorial(7))
})

test_that("structure_randall_test on jz2017 pins the index and exact p", {
  data("jz2017")
  res <- structure_randall_test(jz2017, octants_jz)
  # Index: 260 of 288 order predictions correct -> (260 - 28)/288.
  expect_equal(res$statistic, (260 - 28) / 288)
  # Regression pin (first derivation run, 2026-07-07): the observed index is
  # high enough that only the two dihedral relabelings that always reproduce
  # it reach it, so the exact p sits at its lower bound.
  expect_equal(res$p_value, 2 / factorial(7))
})

test_that("exact p from the rotation quotient equals full nv! enumeration", {
  # The exact path enumerates only the (nv-1)! relabelings with variable 1
  # held at position 1, claiming rotations of positions preserve the index
  # with uniform multiplicity. Validate that claim wholesale on generic
  # (fixed, unstructured) data at nv = 5 against an independent brute force
  # over ALL 5! = 120 relabelings, using the draft-style counting loop rather
  # than the package's prediction machinery.
  dat <- data.frame(
    a = c(2, 5, 1, 4, 3, 6),
    b = c(4, 1, 3, 5, 6, 2),
    c = c(1, 3, 6, 2, 4, 5),
    d = c(5, 6, 2, 3, 1, 4),
    e = c(3, 2, 4, 6, 5, 1)
  )
  res <- structure_randall_test(dat, names(dat))
  expect_identical(res$n_perm, 24L)

  r <- stats::cor(as.matrix(dat))
  d5 <- outer(1:5, 1:5, function(i, j) pmin(abs(i - j), 5 - abs(i - j)))
  hyp <- d5[lower.tri(d5)]
  brute_index <- function(rp) {
    vals <- rp[lower.tri(rp)]
    nc <- 0
    nt <- 0
    for (i in seq_along(vals)) {
      lr <- hyp > hyp[i]
      nc <- nc + sum(vals[i] > vals[lr])
      nt <- nt + sum(lr)
    }
    (nc / nt) - ((nt - nc) / nt)
  }
  perms <- rbind(
    cbind(1L, all_perms(2:5)), cbind(2L, all_perms(c(1L, 3:5))),
    cbind(3L, all_perms(c(1:2, 4:5))), cbind(4L, all_perms(c(1:3, 5L))),
    cbind(5L, all_perms(1:4))
  )
  ci_full <- vapply(
    seq_len(nrow(perms)),
    function(k) brute_index(r[perms[k, ], perms[k, ]]),
    numeric(1)
  )
  expect_identical(nrow(perms), 120L)
  expect_equal(res$statistic, brute_index(r))
  expect_equal(res$p_value, mean(ci_full >= brute_index(r)))
})

test_that("structure_randall_test Monte Carlo path: add-one p, seed convention", {
  dat <- perfect_circumplex_df(8)
  # Monte Carlo p uses the add-one convention (1 + #{CI* >= CI})/(n_perm + 1),
  # so it can never be zero and is reproducible under a set.seed() call
  # (global-stream RNG contract, DESIGN.md -- no seed argument).
  set.seed(20260707)
  res <- structure_randall_test(dat, names(dat), n_perm = 199)
  expect_identical(res$method, "monte carlo")
  expect_identical(res$n_perm, 199L)
  expect_gte(res$p_value, 1 / 200)
  expect_lt(res$p_value, 0.05)
  set.seed(20260707)
  res2 <- structure_randall_test(dat, names(dat), n_perm = 199)
  expect_identical(res2$p_value, res$p_value)
})

test_that("structure_randall_test RNG contract: exact path leaves the stream untouched", {
  data("jz2017")
  set.seed(42)
  seed_before <- .Random.seed
  invisible(structure_randall_test(jz2017, octants_jz))
  expect_identical(.Random.seed, seed_before)
  # The Monte Carlo path consumes the global stream.
  invisible(structure_randall_test(jz2017, octants_jz, n_perm = 9))
  expect_false(identical(.Random.seed, seed_before))
})

test_that("structure_randall_test exact path does not create .Random.seed", {
  data("jz2017")
  # Stronger than the "stream untouched" test above: with no RNG state present,
  # the exact path must consume no randomness at all -- it must not even bring
  # .Random.seed into existence.
  if (exists(".Random.seed", envir = globalenv())) {
    old_seed <- get(".Random.seed", envir = globalenv())
    rm(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  }
  invisible(structure_randall_test(jz2017, octants_jz))
  expect_false(exists(".Random.seed", envir = globalenv()))
})

test_that("Monte Carlo RANDALL p is reproducible and interior on a marginal circumplex", {
  # A noisy circumplex whose order is only partially recovered, so the Monte
  # Carlo p lands strictly inside (add-one floor, 1) rather than pinned at the
  # 1/(n_perm + 1) floor -- making seed reproducibility a non-trivial check.
  set.seed(101)
  nv <- 8
  ang <- (seq_len(nv) - 1) * 2 * pi / nv
  f1 <- stats::rnorm(120)
  f2 <- stats::rnorm(120)
  dat <- as.data.frame(lapply(ang, function(a) {
    cos(a) * f1 + sin(a) * f2 + 5 * stats::rnorm(120)
  }))
  names(dat) <- paste0("V", seq_len(nv))

  set.seed(2024)
  res <- structure_randall_test(dat, names(dat), n_perm = 999)
  set.seed(2024)
  res2 <- structure_randall_test(dat, names(dat), n_perm = 999)
  expect_identical(res$method, "monte carlo")
  expect_identical(res$p_value, res2$p_value) # reproducible under the seed
  expect_gt(res$p_value, 2 / 1000) # strictly above the add-one floor
  expect_lt(res$p_value, 0.9) # still an interior value (index ~ .04, p ~ .40)
})

test_that("internal helpers select columns correctly from a matrix input", {
  data("jz2017")
  m <- as.matrix(jz2017[octants_jz])
  # `data[scales]` element-indexes a matrix (wrong); `data[, scales]` selects
  # columns. Loadings and RANDALL from a matrix must match the data-frame path,
  # for both name and numeric-index selection.
  expect_equal(
    structure_loadings(m, octants_jz),
    structure_loadings(as.data.frame(m), octants_jz)
  )
  expect_equal(
    structure_loadings(m, 1:8),
    structure_loadings(as.data.frame(m), octants_jz)
  )
  expect_equal(
    structure_randall_test(m, octants_jz)$statistic,
    structure_randall_test(as.data.frame(m), octants_jz)$statistic
  )
})

test_that("structure_randall_test validates its arguments", {
  data("jz2017")
  # RANDALL needs unequal circular distances: at least 4 scales.
  expect_error(structure_randall_test(jz2017, octants_jz[1:3]))
  expect_error(structure_randall_test(jz2017, octants_jz, n_perm = 0))
  expect_error(structure_randall_test(jz2017, octants_jz, n_perm = c(10, 20)))
})
