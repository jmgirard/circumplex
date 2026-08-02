# M68 axes_reliability(): the Satorra-Bentler-type scaling factor.
#
# axes_reliability() fits its model to the item CORRELATION matrix as if it were
# a covariance matrix, so lavaan refers T to a chi-square derived for a
# Wishart-distributed S while the analyzed moments are correlations. The fix is
# satorra1994's eqs. 16.21/16.22 (p. 407): T_s = T / c with c = tr(U Gamma_R)/df
# and U = V - V Delta (Delta' V Delta)^-1 Delta' V (eq. 16.18, p. 406).
#
# The shipped code (R/axes_scaled_fit.R) evaluates that trace through p x p
# identities and never forms a p* x p* matrix. These tests recompute it the
# STUPID way -- literal vech-space Gamma_R, V, Delta and U -- so the two routes
# share no arithmetic. That is the closed-form oracle of AC2; AC3/AC4's
# simulation coverage is the second, independent type.


# ---- the deliberately dumb vech-space oracle --------------------------------
#
# Everything here is built from its definition with explicit matrices. It is
# O(p^4) in memory and is never called on anything larger than the probe maps.

vech_oracle_dup <- function(p) {
  pstar <- p * (p + 1) / 2
  D <- matrix(0, p * p, pstar)
  k <- 0L
  for (j in seq_len(p)) for (i in j:p) {
    k <- k + 1L
    D[(j - 1) * p + i, k] <- 1
    D[(i - 1) * p + j, k] <- 1
  }
  D
}

vech_oracle_vech <- function(M) M[lower.tri(M, diag = TRUE)]

# c and c_b from literal vech-space matrices. `sigma` must already be a
# correlation matrix in the item map's own order.
vech_oracle_factor <- function(sigma, mats, df, baseline_df) {
  p <- nrow(sigma)
  pstar <- p * (p + 1) / 2
  D <- vech_oracle_dup(p)
  Dp <- solve(t(D) %*% D) %*% t(D)          # the Moore-Penrose D^+
  si <- solve(sigma)

  # V, the normal-theory ML weight in vech coordinates (satorra1994 eq. 16.4,
  # p. 402), and Gamma_S, the normal-theory acov of vech(S). They are inverses;
  # the oracle asserts that rather than assuming it.
  V <- 0.5 * t(D) %*% kronecker(si, si) %*% D
  Gs <- 2 * Dp %*% kronecker(sigma, sigma) %*% t(Dp)
  testthat::expect_lt(max(abs(V %*% Gs - diag(pstar))), 1e-9)

  # Gamma_R = J Gamma_S J', with J the row-by-row Jacobian of the
  # covariance-to-correlation map at a unit diagonal:
  #   dr_ij = ds_ij - 0.5 * rho_ij * (ds_ii + ds_jj)   (i != j),  dr_ii = 0.
  idx <- which(lower.tri(matrix(0, p, p), diag = TRUE), arr.ind = TRUE)
  J <- matrix(0, pstar, pstar)
  for (a in seq_len(pstar)) {
    i <- idx[a, 1]
    j <- idx[a, 2]
    if (i == j) next                        # a correlation's diagonal is fixed
    J[a, a] <- 1
    ai <- which(idx[, 1] == i & idx[, 2] == i)
    aj <- which(idx[, 1] == j & idx[, 2] == j)
    J[a, ai] <- J[a, ai] - 0.5 * sigma[i, j]
    J[a, aj] <- J[a, aj] - 0.5 * sigma[i, j]
  }
  Gr <- J %*% Gs %*% t(J)
  # Independent check on Gr itself: under normality the asymptotic variance of
  # sqrt(n) r_ij is (1 - rho_ij^2)^2, which J never sees -- it is the
  # Pearson-Filon value, and agreement says the delta method was assembled
  # right rather than merely consistently.
  for (a in seq_len(pstar)) {
    i <- idx[a, 1]
    j <- idx[a, 2]
    want <- if (i == j) 0 else (1 - sigma[i, j]^2)^2
    testthat::expect_lt(abs(Gr[a, a] - want), 1e-9)
  }

  Delta <- vapply(mats, vech_oracle_vech, numeric(pstar))
  proj <- function(Dl) {
    Dl <- as.matrix(Dl)
    V - V %*% Dl %*% solve(t(Dl) %*% V %*% Dl) %*% t(Dl) %*% V
  }
  U <- proj(Delta)
  # The invariant that validates the ORACLE: priced with the normal-theory
  # Gamma_S instead of Gamma_R the factor is exactly 1, because
  # tr(U Gamma_S) = p* - q by construction. A slip in D, V, Delta or the
  # projection breaks this before it reaches the comparison below.
  testthat::expect_lt(abs(sum(diag(U %*% Gs)) / df - 1), 1e-9)

  # The baseline (independence) model is fitted to a correlation matrix, so its
  # own implied matrix is the identity; only Gamma_R stays at sigma-hat.
  pb <- nrow(sigma)
  Vb <- 0.5 * t(D) %*% kronecker(diag(pb), diag(pb)) %*% D
  Db <- vapply(seq_len(pb), function(i) {
    E <- matrix(0, pb, pb)
    E[i, i] <- 1
    vech_oracle_vech(E)
  }, numeric(pstar))
  Ub <- Vb - Vb %*% Db %*% solve(t(Db) %*% Vb %*% Db) %*% t(Db) %*% Vb

  list(
    scale = sum(diag(U %*% Gr)) / df,
    baseline = sum(diag(Ub %*% Gr)) / baseline_df
  )
}


# ---- probe maps -------------------------------------------------------------

# RR13's probe population: 8 octant scales, 3 items each, xi1 = .35, xi2 = .10,
# zeta1 = .08. This is the map M66's anchors are stated at.
probe_octant <- function() {
  oct <- octants()
  pop <- axes_population_cor(oct, 3L, xi1 = .35, xi2 = .10, zeta1 = .08)
  nm <- sprintf("item_%02d", seq_len(nrow(pop$sigma)))
  dimnames(pop$sigma) <- list(nm, nm)
  list(
    sigma = pop$sigma, names = nm, scale = pop$scale, angles = oct,
    items = unname(split(nm, pop$scale)),
    item_angle = rep(as.numeric(oct), each = 3L), n_items = 3L
  )
}

# Six equally spaced scales, two items each -- a non-octant type (b/c) map.
probe_six <- function() {
  ang <- as_degree(c(60, 120, 180, 240, 300, 360))
  pop <- axes_population_cor(ang, 2L, xi1 = .30, xi2 = .05, zeta1 = .12)
  nm <- sprintf("item_%02d", seq_len(nrow(pop$sigma)))
  dimnames(pop$sigma) <- list(nm, nm)
  list(
    sigma = pop$sigma, names = nm, scale = pop$scale, angles = ang,
    items = unname(split(nm, pop$scale)),
    item_angle = rep(as.numeric(ang), each = 2L), n_items = 2L
  )
}

# One item per scale (M61 type e/f): zeta1 is unidentified and dropped, so the
# parameter set is {xi1, xi2} plus p item errors.
probe_single <- function() {
  oct <- octants()
  pop <- axes_population_cor(oct, 1L, xi1 = .40, xi2 = .08, zeta1 = 0)
  nm <- sprintf("item_%02d", seq_len(nrow(pop$sigma)))
  dimnames(pop$sigma) <- list(nm, nm)
  list(
    sigma = pop$sigma, names = nm, scale = pop$scale, angles = oct,
    items = unname(split(nm, pop$scale)),
    item_angle = as.numeric(oct), n_items = 1L
  )
}

# Fit and hand back exactly what axes_scaling_factor() consumes.
probe_fit <- function(pp, n = 600) {
  fit <- axes_fit_cormat(pp$sigma, pp$items, pp$angles, n = n)
  fm <- lavaan::fitMeasures(fit, c("chisq", "df", "baseline.chisq",
                                   "baseline.df", "ntotal"))
  list(fit = fit, sigma = lavaan::fitted(fit)$cov, fm = fm)
}


test_that("AC2: the shipped factor matches the explicit vech-space oracle (octant map)", {
  skip_if_not_installed("lavaan")
  pp <- probe_octant()
  ff <- probe_fit(pp)

  got <- axes_scaling_factor(
    ff$sigma, pp$names, pp$item_angle, pp$scale,
    fit_zeta1 = TRUE, fit_zeta2 = FALSE,
    df = ff$fm[["df"]], baseline_df = ff$fm[["baseline.df"]]
  )
  expect_null(got$reason)

  d <- axes_se_derivs(pp$item_angle, pp$scale, NULL, TRUE, FALSE)
  want <- vech_oracle_factor(
    stats::cov2cor(ff$sigma[pp$names, pp$names]), d$mats,
    ff$fm[["df"]], ff$fm[["baseline.df"]]
  )

  expect_lt(abs(got$scale - want$scale) / abs(want$scale), 1e-8)
  expect_lt(abs(got$baseline - want$baseline) / abs(want$baseline), 1e-8)
})


test_that("AC2: the shipped factor matches the oracle on a 6-scale map", {
  skip_if_not_installed("lavaan")
  pp <- probe_six()
  ff <- probe_fit(pp)

  got <- axes_scaling_factor(
    ff$sigma, pp$names, pp$item_angle, pp$scale,
    fit_zeta1 = TRUE, fit_zeta2 = FALSE,
    df = ff$fm[["df"]], baseline_df = ff$fm[["baseline.df"]]
  )
  expect_null(got$reason)

  d <- axes_se_derivs(pp$item_angle, pp$scale, NULL, TRUE, FALSE)
  want <- vech_oracle_factor(
    stats::cov2cor(ff$sigma[pp$names, pp$names]), d$mats,
    ff$fm[["df"]], ff$fm[["baseline.df"]]
  )
  expect_lt(abs(got$scale - want$scale) / abs(want$scale), 1e-8)
  expect_lt(abs(got$baseline - want$baseline) / abs(want$baseline), 1e-8)
})


test_that("AC2: the shipped factor matches the oracle with one item per scale", {
  skip_if_not_installed("lavaan")
  pp <- probe_single()
  ff <- probe_fit(pp)

  # zeta1 is not in the fitted model here, which changes Delta's column count
  # and therefore U's projection -- the case that catches a factor built from
  # a hardcoded parameter set.
  got <- axes_scaling_factor(
    ff$sigma, pp$names, pp$item_angle, pp$scale,
    fit_zeta1 = FALSE, fit_zeta2 = FALSE,
    df = ff$fm[["df"]], baseline_df = ff$fm[["baseline.df"]]
  )
  expect_null(got$reason)

  d <- axes_se_derivs(pp$item_angle, pp$scale, NULL, FALSE, FALSE)
  want <- vech_oracle_factor(
    stats::cov2cor(ff$sigma[pp$names, pp$names]), d$mats,
    ff$fm[["df"]], ff$fm[["baseline.df"]]
  )
  expect_lt(abs(got$scale - want$scale) / abs(want$scale), 1e-8)
  expect_lt(abs(got$baseline - want$baseline) / abs(want$baseline), 1e-8)
})


test_that("AC2: the baseline factor is the mean of (1 - rho^2)^2 over item pairs", {
  skip_if_not_installed("lavaan")
  pp <- probe_octant()
  ff <- probe_fit(pp)
  got <- axes_scaling_factor(
    ff$sigma, pp$names, pp$item_angle, pp$scale,
    fit_zeta1 = TRUE, fit_zeta2 = FALSE,
    df = ff$fm[["df"]], baseline_df = ff$fm[["baseline.df"]]
  )

  # A third, fully independent route to c_b, written out as a scalar mean
  # rather than as a trace. It falls out of eq. (16.18) because the
  # independence model's free parameters are the variances, whose sample
  # correlations have zero sampling variability -- so U_b reduces to V_b and
  # the trace collapses to the average of the Pearson-Filon variances.
  rho <- stats::cov2cor(ff$sigma[pp$names, pp$names])
  off <- rho[upper.tri(rho)]
  expect_lt(abs(got$baseline - mean((1 - off^2)^2)), 1e-10)
})


test_that("AC2: 1/c corroborates RR13's measured E[T] = 261.1 at df = 273", {
  skip_if_not_installed("lavaan")
  # RR13 W-A measured E[T] = 261.1 against df = 273 at this population by
  # simulation, and shipped no reproduction code for it -- so this is
  # CORROBORATION, not a gate: the scaled expectation must land within 0.5 of
  # df, and a miss escalates rather than fails (M68 AC2).
  pp <- probe_octant()
  ff <- probe_fit(pp)
  got <- axes_scaling_factor(
    ff$sigma, pp$names, pp$item_angle, pp$scale,
    fit_zeta1 = TRUE, fit_zeta2 = FALSE,
    df = ff$fm[["df"]], baseline_df = ff$fm[["baseline.df"]]
  )
  expect_lt(abs(261.1 / got$scale - 273), 0.5)
})


test_that("AC1: the factor refuses rather than guessing when its inputs are wrong", {
  skip_if_not_installed("lavaan")
  pp <- probe_octant()
  ff <- probe_fit(pp)
  args <- list(
    sigma = ff$sigma, item_names = pp$names,
    item_angle_deg = pp$item_angle, item_scale = pp$scale,
    fit_zeta1 = TRUE, fit_zeta2 = FALSE,
    df = ff$fm[["df"]], baseline_df = ff$fm[["baseline.df"]]
  )

  # Dimnames are load-bearing: sigma arrives in lavaan's own variable order,
  # not the item map's, so a matrix that cannot be realigned is refused rather
  # than consumed in whatever order it arrived (the M66 contract).
  bare <- ff$sigma
  dimnames(bare) <- NULL
  expect_error(
    do.call(axes_scaling_factor, utils::modifyList(args, list(sigma = bare))),
    "dimnames"
  )

  # A singular sigma-hat: both factors NA together, with a named reason, and
  # never a silent fall back to 1 (which would relabel the uncorrected
  # statistic as corrected -- the one failure a user could not detect).
  sing <- ff$sigma
  sing[, 2] <- sing[, 1]
  sing[2, ] <- sing[1, ]
  got <- suppressWarnings(
    do.call(axes_scaling_factor, utils::modifyList(args, list(sigma = sing)))
  )
  expect_identical(got$reason, "singular")
  expect_true(is.na(got$scale))
  expect_true(is.na(got$baseline))

  # A df that does not match the derivative set means Delta is not this
  # model's Delta, so U projects onto the wrong space. Refuse.
  got <- suppressWarnings(
    do.call(axes_scaling_factor, utils::modifyList(args, list(df = 271)))
  )
  expect_identical(got$reason, "df_mismatch")
  expect_true(is.na(got$scale))
})


test_that("AC1: lavaan still forms rmsea, cfi and pvalue the way the scaler assumes", {
  skip_if_not_installed("lavaan")
  # The scaler does not re-derive lavaan's fit indices -- it recomputes them
  # from the SCALED chi-square using the published definitions. That is only
  # sound while lavaan's own uncorrected values follow those same definitions,
  # and lavaan is a Suggests with no version floor. So pin the arithmetic
  # against lavaan's own output on a fit whose chi-square EXCEEDS df (a
  # perfect fit makes rmsea and cfi degenerate and pins nothing).
  pp <- probe_octant()
  bad <- pp$sigma
  bad[1, 5] <- bad[5, 1] <- bad[1, 5] + 0.30
  bad[2, 9] <- bad[9, 2] <- bad[2, 9] - 0.25
  ev <- eigen(bad, symmetric = TRUE)$values
  skip_if(min(ev) <= 0, "perturbed probe matrix is not positive definite")

  fit <- axes_fit_cormat(bad, pp$items, pp$angles, n = 1500)
  fm <- lavaan::fitMeasures(fit, c("chisq", "df", "pvalue", "rmsea", "cfi",
                                   "baseline.chisq", "baseline.df", "ntotal"))
  tval <- fm[["chisq"]]
  df <- fm[["df"]]
  skip_if(tval <= df, "perturbation did not misspecify enough to test rmsea")

  expect_lt(abs(fm[["pvalue"]] - stats::pchisq(tval, df, lower.tail = FALSE)), 1e-12)
  expect_lt(
    abs(fm[["rmsea"]] - sqrt(max(tval - df, 0) / (df * fm[["ntotal"]]))),
    1e-10
  )
  expect_lt(
    abs(fm[["cfi"]] - (1 - max(tval - df, 0) /
                         max(tval - df, fm[["baseline.chisq"]] - fm[["baseline.df"]], 0))),
    1e-10
  )
})


test_that("AC1: scaling the fit measures divides T and leaves df and srmr alone", {
  skip_if_not_installed("lavaan")
  fm <- c(chisq = 300, df = 273, pvalue = 0.1, rmsea = 0.02, cfi = 0.99,
          srmr = 0.05, baseline.chisq = 4000, baseline.df = 276, ntotal = 600)
  cf <- list(scale = 0.95, baseline = 0.87, reason = NULL)
  got <- axes_scale_fit_measures(fm, cf)

  ts <- 300 / 0.95
  tbs <- 4000 / 0.87
  expect_equal(got$fit$chisq, ts)
  expect_equal(got$fit$df, 273)
  expect_equal(got$fit$srmr, 0.05)
  expect_equal(got$fit$pvalue, stats::pchisq(ts, 273, lower.tail = FALSE))
  expect_equal(got$fit$rmsea, sqrt(max(ts - 273, 0) / (273 * 600)))
  expect_equal(
    got$fit$cfi,
    1 - max(ts - 273, 0) / max(ts - 273, tbs - 276, 0)
  )
  # The uncorrected six travel alongside, unmodified.
  expect_equal(got$uncorrected$chisq, 300)
  expect_equal(got$uncorrected$cfi, 0.99)
  expect_equal(got$uncorrected$rmsea, 0.02)

  # A failed factor NAs the four chi-square-derived statistics and nothing
  # else, and never falls back to the unscaled values (M68-D1).
  bad <- axes_scale_fit_measures(fm, list(scale = NA_real_, baseline = NA_real_,
                                          reason = "singular"))
  expect_true(is.na(bad$fit$chisq))
  expect_true(is.na(bad$fit$pvalue))
  expect_true(is.na(bad$fit$rmsea))
  expect_true(is.na(bad$fit$cfi))
  expect_equal(bad$fit$df, 273)
  expect_equal(bad$fit$srmr, 0.05)
  expect_equal(bad$uncorrected$chisq, 300)
})


test_that("AC1: a scaled statistic is never reported beside an unscaled one", {
  skip_if_not_installed("lavaan")
  # c = 1 exactly is the identity case; every scaled statistic must then equal
  # lavaan's own. This is what makes the four-field replacement checkable: if
  # any of the four were passed through unscaled it would ALSO match here, so
  # the test pairs it with a c far from 1, where a passed-through field stands
  # out as unchanged.
  fm <- c(chisq = 400, df = 273, pvalue = 0.5, rmsea = 0.3, cfi = 0.5,
          srmr = 0.05, baseline.chisq = 4000, baseline.df = 276, ntotal = 600)
  ident <- axes_scale_fit_measures(fm, list(scale = 1, baseline = 1, reason = NULL))
  expect_equal(ident$fit$chisq, 400)

  moved <- axes_scale_fit_measures(fm, list(scale = 0.5, baseline = 0.5,
                                            reason = NULL))
  for (nm in c("chisq", "pvalue", "rmsea", "cfi")) {
    expect_false(isTRUE(all.equal(moved$fit[[nm]], ident$fit[[nm]])),
                 info = paste("field passed through unscaled:", nm))
  }
  for (nm in c("df", "srmr")) {
    expect_equal(moved$fit[[nm]], ident$fit[[nm]])
  }
})


# ---- AC1: the wiring, on all three input paths ------------------------------

# One population and one seed feeding all three paths, so "path-dependent" is a
# claim about the code rather than about three different datasets.
wire_fixture <- function(n = 300L, seed = 21L) {
  oct <- octants()
  set.seed(seed)
  mat <- as.matrix(axes_simulate(n, oct, 3L, .35, .10, .08))
  colnames(mat) <- sprintf("item_%02d", seq_len(ncol(mat)))
  list(mat = mat, angles = oct,
       items = unname(split(colnames(mat), rep(seq_len(8), each = 3L))))
}

# Every path's object is checked by the same function, because AC1's claim is
# that the three agree in CONTRACT, and three separately-written checks would
# not notice if one path quietly lost a field.
expect_scaled_contract <- function(res, label) {
  d <- res$details
  expect_false(is.null(d$fit_uncorrected), info = label)
  expect_false(is.null(d$scaling_factor), info = label)
  expect_null(d$fit_scaling_failed, info = label)
  expect_named(d$scaling_factor, c("model", "baseline"), info = label)

  cm <- d$scaling_factor[["model"]]
  cb <- d$scaling_factor[["baseline"]]
  expect_true(is.finite(cm) && cm > 0, info = label)
  expect_true(is.finite(cb) && cb > 0, info = label)

  u <- d$fit_uncorrected
  ts <- u$chisq / cm

  # The four chi-square-derived statistics ARE the scaled ones.
  expect_lt(abs(res$fit$chisq - ts), 1e-10 * max(1, abs(ts)))
  expect_lt(abs(res$fit$pvalue - stats::pchisq(ts, u$df, lower.tail = FALSE)),
            1e-12)
  # RMSEA and CFI are checked for MOVEMENT rather than recomputed here: their
  # arithmetic is pinned against lavaan's own above, and repeating it would
  # only re-assert the implementation against itself.
  if (ts > u$df || u$chisq > u$df) {
    expect_false(isTRUE(all.equal(res$fit$rmsea, u$rmsea)), info = label)
  }

  # df and srmr pass through untouched -- AC1's bit-identity clause.
  expect_identical(res$fit$df, u$df, info = label)
  expect_identical(res$fit$srmr, u$srmr, info = label)

  # Nothing in `$fit` leaks the helper fields the scaler needed.
  expect_setequal(names(res$fit),
                  c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr"))
}

test_that("AC1: listwise, cormat and fiml all report scaled fit statistics", {
  skip_if_not_installed("lavaan")
  fx <- wire_fixture()

  listwise <- axes_reliability(as.data.frame(fx$mat), fx$items, fx$angles)
  expect_scaled_contract(listwise, "listwise")

  cm <- axes_reliability(items = fx$items, angles = fx$angles,
                         cormat = stats::cor(fx$mat), n = nrow(fx$mat))
  expect_scaled_contract(cm, "cormat")

  miss <- fx$mat
  set.seed(99)
  miss[cbind(sample.int(nrow(miss), 40L), sample.int(ncol(miss), 40L, TRUE))] <- NA
  fiml <- axes_reliability(as.data.frame(miss), fx$items, fx$angles,
                           missing = "fiml")
  expect_scaled_contract(fiml, "fiml")

  # The listwise and cormat paths see the same correlation matrix, so their
  # factors must agree -- the M65 SRMR trap was exactly a statistic that
  # differed across paths on data with no missing cells at all.
  expect_lt(
    abs(listwise$details$scaling_factor[["model"]] -
          cm$details$scaling_factor[["model"]]),
    1e-8
  )

  # And no path reports the unscaled statistic: at this population c is far
  # enough from 1 that a passed-through chisq would be visible.
  for (r in list(listwise, cm, fiml)) {
    expect_false(isTRUE(all.equal(r$fit$chisq, r$details$fit_uncorrected$chisq)))
  }
})

test_that("AC1: `$fit$df` and `$fit$srmr` are what lavaan itself reports", {
  skip_if_not_installed("lavaan")
  # Bit-identity against an INDEPENDENT refit, not against the stored
  # uncorrected copy -- the stored copy comes from the same fitMeasures() call
  # the scaler read, so agreeing with it says nothing about the wiring.
  fx <- wire_fixture()
  res <- axes_reliability(as.data.frame(fx$mat), fx$items, fx$angles)
  # Standardized, because axes_reliability() fits the STANDARDIZED matrix; a
  # refit on the raw scores is a different model's srmr (0.0521 against
  # 0.0495 here) and would fail this for the wrong reason.
  fit <- axes_fit(as.data.frame(scale(fx$mat)), fx$items, fx$angles)
  fm <- lavaan::fitMeasures(fit, c("df", "srmr_bentler_nomean"))
  expect_identical(res$fit$df, fm[["df"]])
  # srmr agrees to 1e-8 rather than bit-exactly, and the gap is the refit's,
  # not M68's: axes_reliability() passes OLS starting values to the optimizer
  # (`start = ols`) and this refit does not, so the two land on the same optimum
  # from different directions and differ in the 9th digit. The BIT-identity AC1
  # asks for is asserted where it is actually meaningful -- against the
  # uncorrected copy taken from the very fitMeasures() call the scaler read, in
  # expect_scaled_contract() above. What this refit adds is that the reported
  # quantity is still lavaan's covariance-only SRMR at all.
  expect_lt(abs(res$fit$srmr - fm[["srmr_bentler_nomean"]]), 1e-8)
})
