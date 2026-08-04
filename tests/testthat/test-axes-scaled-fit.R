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
  # right rather than merely consistently. The DIAGONAL only; the entrywise
  # check is vech_oracle_gamma_r_closed() below, and RR14 found that the
  # diagonal alone left most of tr{U Gamma_R} validated by nothing but the two
  # delta-method routes agreeing with each other.
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


# Gamma_R built a SECOND, wholly different way: entry by entry from the closed
# normal-theory expression for the asymptotic covariance of two sample
# correlations. The delta-method route above never sees this formula -- it
# composes a Jacobian with Gamma_S -- so agreement is a genuine cross-check
# rather than two spellings of one derivation.
#
# For i != j and k != l,
#
#   n*cov(r_ij, r_kl) =
#     0.5*rho_ij*rho_kl*(rho_ik^2 + rho_il^2 + rho_jk^2 + rho_jl^2)
#     + rho_ik*rho_jl + rho_il*rho_jk
#     - rho_ij*(rho_ik*rho_il + rho_jk*rho_jl)
#     - rho_kl*(rho_ik*rho_jk + rho_il*rho_jl)
#
# and any cell touching a variance (i == j or k == l) is zero, because a sample
# correlation's diagonal does not vary. The identity is Olkin & Siotani's; it is
# written out here rather than cited because nothing SHIPPED relies on it -- it
# is a test-side recomputation whose own correctness is established by the
# agreement this function is used to assert (RR14 BC7).
vech_oracle_gamma_r_closed <- function(sigma) {
  p <- nrow(sigma)
  idx <- which(lower.tri(matrix(0, p, p), diag = TRUE), arr.ind = TRUE)
  pstar <- nrow(idx)
  out <- matrix(0, pstar, pstar)
  r <- sigma
  for (a in seq_len(pstar)) {
    i <- idx[a, 1]
    j <- idx[a, 2]
    if (i == j) next
    for (b in seq_len(pstar)) {
      k <- idx[b, 1]
      l <- idx[b, 2]
      if (k == l) next
      out[a, b] <-
        0.5 * r[i, j] * r[k, l] *
          (r[i, k]^2 + r[i, l]^2 + r[j, k]^2 + r[j, l]^2) +
        r[i, k] * r[j, l] + r[i, l] * r[j, k] -
        r[i, j] * (r[i, k] * r[i, l] + r[j, k] * r[j, l]) -
        r[k, l] * (r[i, k] * r[j, k] + r[i, l] * r[j, l])
    }
  }
  out
}

# The delta-method Gamma_R on its own, so the two routes can be compared without
# running the whole factor.
vech_oracle_gamma_r_delta <- function(sigma) {
  p <- nrow(sigma)
  D <- vech_oracle_dup(p)
  Dp <- solve(t(D) %*% D) %*% t(D)
  pstar <- p * (p + 1) / 2
  Gs <- 2 * Dp %*% kronecker(sigma, sigma) %*% t(Dp)
  idx <- which(lower.tri(matrix(0, p, p), diag = TRUE), arr.ind = TRUE)
  J <- matrix(0, pstar, pstar)
  for (a in seq_len(pstar)) {
    i <- idx[a, 1]
    j <- idx[a, 2]
    if (i == j) next
    J[a, a] <- 1
    ai <- which(idx[, 1] == i & idx[, 2] == i)
    aj <- which(idx[, 1] == j & idx[, 2] == j)
    J[a, ai] <- J[a, ai] - 0.5 * sigma[i, j]
    J[a, aj] <- J[a, aj] - 0.5 * sigma[i, j]
  }
  J %*% Gs %*% t(J)
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


test_that("M68 review F2: CFI is 1, not NaN, when neither model nor baseline misfits", {
  skip_if_not_installed("lavaan")
  # Both excesses zero => 0/0. The shipped formula returned NaN, which a user
  # reads as a broken computation rather than as perfect fit; lavaan's own
  # lav_fit_cfi() returns 1 in the same state, and so does the definition's
  # limit. Reachable whenever a well-fitting model sits on a near-independence
  # correlation matrix: the baseline chi-square is then small too.
  fm <- c(chisq = 200, df = 273, pvalue = 0.9, rmsea = 0, cfi = 1,
          srmr = 0.02, baseline.chisq = 260, baseline.df = 276, ntotal = 600)
  cf <- list(scale = 0.95, baseline = 0.95, reason = NULL)
  got <- axes_scale_fit_measures(fm, cf)
  expect_false(is.nan(got$fit$cfi))
  expect_equal(got$fit$cfi, 1)

  # Only the 0/0 corner is special-cased: a misfitting model still gets the
  # ratio.
  fm2 <- fm
  fm2[["chisq"]] <- 400
  fm2[["baseline.chisq"]] <- 4000
  got2 <- axes_scale_fit_measures(fm2, cf)
  expect_lt(got2$fit$cfi, 1)
  expect_equal(got2$fit$cfi, 1 - (400 / 0.95 - 273) /
                 (4000 / 0.95 - 276))

  # Against lavaan's own function on the same two scaled statistics, so the
  # agreement is with the reference implementation and not with our reading of
  # it. `lav_fit_cfi()` is UNEXPORTED, so neither its existence nor its argument
  # names are a contract: an earlier lavaan takes them positionally under
  # different names, and calling it by name errored the whole test on CI while
  # passing locally (M68 review round 2 -- the existence probe checked that the
  # symbol resolved, not that it accepted these arguments). The call itself is
  # therefore what is probed, and any failure to reach it SKIPS: the assertions
  # above already pin the behaviour without lavaan's help, and this is
  # corroboration against the reference implementation, not the check itself.
  ref <- tryCatch({
    f <- get("lav_fit_cfi", envir = asNamespace("lavaan"))
    c(f(X2 = 200 / 0.95, df = 273, X2.null = 260 / 0.95, df.null = 276),
      f(X2 = 400 / 0.95, df = 273, X2.null = 4000 / 0.95, df.null = 276))
  }, error = function(e) NULL, warning = function(w) NULL)
  skip_if(is.null(ref) || length(ref) != 2L || anyNA(ref),
          "lavaan::lav_fit_cfi is not callable with these arguments")
  expect_equal(got$fit$cfi, ref[[1]])
  expect_equal(got2$fit$cfi, ref[[2]])
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
  if (ts > u$df || u$chisq > u$df) {
    expect_false(isTRUE(all.equal(res$fit$rmsea, u$rmsea)), info = label)
  }

  # CFI, read and recomputed on EVERY path (M68 review, F4: no test in this file
  # read `$fit$cfi` at all, so a wiring regression that assigned lavaan's
  # unscaled cfi to the reported field would have passed the whole suite -- the
  # one of the four statistics with no standing check).
  #
  # `details` does not store the baseline chi-square, so it is recovered by
  # inverting lavaan's OWN uncorrected cfi:
  #     cfi = 1 - (T - df) / (T_b - df_b)      when T > df and T_b - df_b is the
  #                                            larger of the two excesses
  # with df_b = p(p-1)/2, the independence model's own count. The inversion is
  # exact and uses only the uncorrected six plus the two factors, so it is
  # independent of the value under test.
  pn <- res$details$n_items
  dfb <- pn * (pn - 1) / 2
  if (u$chisq > u$df && u$cfi < 1 - 1e-8) {
    tsb <- ((u$chisq - u$df) / (1 - u$cfi) + dfb) / cb
    t1 <- max(ts - u$df, 0)
    t2 <- max(ts - u$df, tsb - dfb, 0)
    want_cfi <- if (t2 == 0) 1 else 1 - t1 / t2
    # The check must be capable of failing: the scaled and unscaled CFIs differ
    # here, so passing it by reporting lavaan's value is not available.
    expect_gt(abs(want_cfi - u$cfi), 1e-4, label = paste("cfi moves", label))
    expect_lt(abs(res$fit$cfi - want_cfi), 1e-8,
              label = paste("cfi recomputation", label))
    expect_false(isTRUE(all.equal(res$fit$cfi, u$cfi)), info = label)
  }
  # ... and whatever the fit, the reported CFI is a number in range and never
  # NaN (M68 review, F2).
  expect_false(is.nan(res$fit$cfi), info = label)
  expect_true(res$fit$cfi >= 0 && res$fit$cfi <= 1, info = label)

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


test_that("AC13: Gamma_R agrees entrywise with the closed normal-theory formula", {
  skip_if_not_installed("lavaan")
  # RR14's "Beyond the brief" finding 1: the oracle validated Gamma_R only on
  # its DIAGONAL, while the off-diagonal cells carry most of tr{U Gamma_R} and
  # were pinned by nothing but the two delta-method routes agreeing with each
  # other. This closes it in the suite.
  pp <- probe_octant()
  ff <- probe_fit(pp)
  sigma <- stats::cov2cor(ff$sigma[pp$names, pp$names])

  got <- vech_oracle_gamma_r_delta(sigma)
  want <- vech_oracle_gamma_r_closed(sigma)
  expect_identical(dim(got), dim(want))
  # ALL cells, not the diagonal only. Absolute rather than relative, because
  # most cells are near zero and a relative bar there is meaningless.
  expect_lt(max(abs(got - want)), 1e-12)

  # The comparison must be capable of failing: perturbing one correlation moves
  # the closed-form matrix well outside the bar, so agreement above is not two
  # all-zero matrices matching. Guards the M60 green-for-the-wrong-reason trap.
  bad <- sigma
  bad[1, 2] <- bad[2, 1] <- bad[1, 2] + 0.05
  expect_gt(max(abs(vech_oracle_gamma_r_closed(bad) - want)), 1e-6)

  # And a non-model matrix, so the check is not resting on the fitted matrix's
  # own structure (RR14 verified its route at 3.3e-16 this way).
  set.seed(4)
  z <- matrix(stats::rnorm(40 * 6), 40, 6)
  free <- stats::cor(z)
  expect_lt(
    max(abs(vech_oracle_gamma_r_delta(free) - vech_oracle_gamma_r_closed(free))),
    1e-12
  )
})


# ---- AC7-AC10, AC14: the committed calibration evidence ---------------------
#
# RR14 BC8: the regression evidence stands in the SUITE, not only in the work
# log. Without these, AC7-AC10 could be satisfied by a narrative and the package
# would ship with no standing check on the calibration numbers at all -- the
# gap the binding-criteria audit called the single most consequential omission.
#
# Regenerate the fixture with `Rscript devel/m68-scaled-fit-cells.R` (2000
# replicates, ~5 min on 8 cores).

m68_cells <- function() {
  readRDS(test_path("fixtures", "m68-scaled-fit-cells.rds"))
}

# The three N = 600 populations and their committed rates (AC9's fences).
M68_POPS <- c("strong", "weak", "antic")
M68_RATE_SCALED <- c(strong = .0790, weak = .0630, antic = .1070)
M68_RATE_UNSCALED <- c(strong = .0270, weak = .0200, antic = .0215)

m68_ok <- function(cell) cell[stats::complete.cases(cell), , drop = FALSE]


test_that("AC7: mean(T_s)/df is in [0.97, 1.03] at all three populations", {
  fx <- m68_cells()
  expect_gte(fx$provenance$reps, 2000L)
  for (nm in M68_POPS) {
    cell <- m68_ok(fx$cells[[nm]])
    expect_gte(nrow(cell), 2000L, label = nm)
    got <- mean(cell[, "chisq_scaled"]) / cell[1, "df"]
    expect_gt(got, 0.97, label = paste("mean(T_s)/df", nm))
    expect_lt(got, 1.03, label = paste("mean(T_s)/df", nm))
  }

  # The paired negative: the UNSCALED ratio must be outside the same band at
  # every population, or the criterion above would be satisfied by a factor
  # that did nothing. This is the whole claim of the milestone in one line.
  for (nm in M68_POPS) {
    cell <- m68_ok(fx$cells[[nm]])
    expect_lt(mean(cell[, "chisq"]) / cell[1, "df"], 0.985, label = nm)
  }
})


test_that("AC8: the rejection rate is nominal where the asymptotics hold", {
  fx <- m68_cells()
  cell <- m68_ok(fx$sweep[["4800"]])
  expect_gte(nrow(cell), 2000L)
  rej <- mean(cell[, "p"] < .05)
  expect_gt(rej, .036)
  expect_lt(rej, .064)

  # ... and the unscaled test is NOT, in the flattering direction, at the same
  # sample size. The pair is the argument for shipping: at the N where the
  # reference distribution is trustworthy, the scaled test is right and the
  # uncorrected one rejects at roughly a third of nominal.
  expect_lt(mean(cell[, "p_unscaled"] < .05), .030)
})


test_that("AC9: the N = 600 rates hold their committed fences", {
  fx <- m68_cells()
  # Regression fences, NOT calibration claims -- the scaled rates here are
  # deliberately outside the nominal band, and that is the documented
  # small-sample behaviour (AC11), not a defect. A breach escalates rather
  # than being re-fenced, and would have to move AC11's published numbers too.
  for (nm in M68_POPS) {
    cell <- m68_ok(fx$cells[[nm]])
    expect_lt(abs(mean(cell[, "p"] < .05) - M68_RATE_SCALED[[nm]]), .021,
              label = paste("scaled rate", nm))
    expect_lt(abs(mean(cell[, "p_unscaled"] < .05) - M68_RATE_UNSCALED[[nm]]),
              .021, label = paste("unscaled rate", nm))
  }
})


test_that("AC10: the tail excess is not factor-estimation noise", {
  fx <- m68_cells()
  # RR14 Q2's decomposition, re-run from the stored columns: replacing each
  # replicate's own c-hat with the population factor barely moves the rejection
  # rate, so the excess is a mean shift in T and not noise the estimated factor
  # introduced. This is the load-bearing evidence for documenting the residual
  # (AC11) rather than treating it as a defect in the scaling.
  for (nm in M68_POPS) {
    cell <- m68_ok(fx$cells[[nm]])
    df <- cell[1, "df"]
    c_pop <- fx$population_diagnostics[[nm]]$cfactor
    rej_hat <- mean(stats::pchisq(cell[, "chisq"] / cell[, "cfactor"], df,
                                  lower.tail = FALSE) < .05)
    rej_pop <- mean(stats::pchisq(cell[, "chisq"] / c_pop, df,
                                  lower.tail = FALSE) < .05)
    expect_lt(abs(rej_hat - rej_pop), .005, label = paste("rej gap", nm))
    # The per-fit factor barely varies, which is why the two agree.
    expect_lt(stats::sd(cell[, "cfactor"]) / mean(cell[, "cfactor"]), .01,
              label = paste("rel sd(c-hat)", nm))
  }
})


test_that("AC14: the live smoke cell reproduces the committed harness", {
  skip_if_not_installed("lavaan")
  # The stored numbers above are a pin; this runs the same path end-to-end at
  # 12 replicates so a regression in the WIRING is caught without the 5-minute
  # full run. The M65 harness pattern: a stored fixture is never the only thing
  # between a broken estimator and a green suite.
  #
  # It runs the GENERATOR'S OWN replicate function -- m68_one_rep() in
  # helper-m68-cells.R, which devel/m68-scaled-fit-cells.R source()s and calls
  # for every row of the fixture -- on the generator's own population and its
  # own first 12 seeds. The earlier version re-implemented the replicate inline
  # on unrelated seeds, which cannot catch the drift between the harness and the
  # package that is the only reason a smoke cell exists (M68 review, F7).
  fx <- m68_cells()
  seeds <- m68_seeds("strong", 12L)
  got <- vapply(seeds, function(s) m68_one_rep(m68_pops$strong, s), numeric(6))
  expect_false(anyNA(got))

  # Same seeds as the fixture's first 12 rows, so this is not merely the same
  # DISTRIBUTION -- it is the same draws, and the committed numbers must come
  # back. This is AC9's exact-reproduction arm applied where it is cheap.
  #
  # The BIT-EXACT half is gated on the environment (m68_env_matches), the same
  # gate AC9's replay uses; without it this errors rather than skips on any
  # platform whose BLAS/LAPACK or lavaan differs, which on CRAN is most of them
  # (M68 review round 2, F1 -- the gate was on the sibling test only). What runs
  # everywhere is the loose agreement below: it still catches a harness that has
  # drifted from the package, which is what AC14 asks the cell for, without
  # asserting last-bit determinism the platform does not owe.
  stored <- t(fx$cells$strong[seq_along(seeds), , drop = FALSE])
  expect_identical(rownames(got), rownames(stored))
  expect_lt(max(abs(got["chisq", ] - stored["chisq", ])), 1e-4)
  expect_lt(max(abs(got["cfactor", ] - stored["cfactor", ])), 1e-6)
  if (m68_env_matches(fx)) {
    expect_lt(max(abs(got - stored)), 1e-12)
  }

  # Direction, not calibration: 12 replicates cannot resolve a rejection rate,
  # and applying the fixture's bar to a sample this small would be theatre. The
  # factor, by contrast, is a population quantity and barely moves, so 12 draws
  # pin it tightly against the fixture's own c_pop.
  expect_lt(abs(mean(got["cfactor", ]) -
                  fx$population_diagnostics$strong$cfactor), .005)
  # Scaling moves the statistic UP, on every single replicate, because c < 1.
  expect_true(all(got["chisq_scaled", ] > got["chisq", ]))
})


test_that("AC9: the committed rates reproduce exactly in the same environment", {
  skip_if_not_installed("lavaan")
  fx <- m68_cells()
  # AC9 has two arms and the drift fence above is only the second one. The first
  # is exact reproduction under an UNCHANGED environment, which is a different
  # claim entirely: it says the pinned seeds still determine the numbers, so a
  # regeneration is a re-derivation rather than a fresh sample. It was
  # unimplemented (M68 review, F6).
  #
  # (1) The rates ARE the committed constants, to 1e-12 rather than to the
  # rounding the fence tolerates. A rate is a deterministic function of the
  # stored per-replicate column, so this is the arm's claim about the rates
  # exactly.
  #
  # This arm is UNGUARDED, deliberately: it is arithmetic over a frozen .rds and
  # cannot be moved by an R or lavaan upgrade, so gating it on the environment
  # would retire the tightest fence in the file the day either one ships a new
  # version, leaving only the +/- .021 drift fence -- which tolerates a 2.5x
  # move in the small rates (M68 review round 2, F2). Only the live replay below
  # depends on the environment.
  for (nm in M68_POPS) {
    cell <- m68_ok(fx$cells[[nm]])
    expect_lt(abs(mean(cell[, "p"] < .05) - M68_RATE_SCALED[[nm]]), 1e-12,
              label = paste("exact scaled rate", nm))
    expect_lt(abs(mean(cell[, "p_unscaled"] < .05) - M68_RATE_UNSCALED[[nm]]),
              1e-12, label = paste("exact unscaled rate", nm))
  }

  # (2) ... and the stored column itself is reproducible from its own seeds, so
  # (1) is a live property of the harness and not an arithmetic tautology over a
  # frozen file. Two replicates per population -- the whole 6000-fit
  # regeneration is the generator's `verify` mode, run by hand.
  #
  # THIS is what the environment gate belongs on: same seeds reproduce the same
  # numbers only under the same R and the same lavaan.
  skip_if_not(m68_env_matches(fx),
              "fixture was generated under a different R or lavaan version")
  for (nm in M68_POPS) {
    idx <- c(1L, 2L)
    seeds <- m68_seeds(nm, max(idx))[idx]
    got <- vapply(seeds, function(s) m68_one_rep(m68_pops[[nm]], s), numeric(6))
    stored <- t(fx$cells[[nm]][idx, , drop = FALSE])
    expect_false(anyNA(got), label = nm)
    expect_lt(max(abs(got - stored)), 1e-12, label = paste("replay", nm))
  }
})


# ---- AC11: the small-sample behaviour is documented, on three surfaces ------

test_that("AC11: the Rd states the calibration sweep, the direction, and the FIML scope", {
  rd <- if (file.exists(test_path("..", "..", "man", "axes_reliability.Rd"))) {
    paste(readLines(test_path("..", "..", "man", "axes_reliability.Rd"),
                    warn = FALSE), collapse = " ")
  } else {
    db <- tools::Rd_db("circumplex")
    paste(as.character(db[["axes_reliability.Rd"]]), collapse = "")
  }
  expect_gt(nchar(rd), 1000L)
  rd <- gsub("\\s+", " ", rd)

  # (i) the sweep, with its numbers and its own scope disclaimer. The
  # disclaimer is pinned because without it the page states a general
  # threshold that one population's sweep does not support.
  expect_match(rd, ".092, .079, .062, .054", fixed = TRUE)
  expect_match(rd, "0.50, 0.25, 0.12, 0.06", fixed = TRUE)
  expect_match(rd, "sweep at a single population, not a general threshold",
               fixed = TRUE)
  # (ii) the N = 600 behaviour, both directions.
  expect_match(rd, ".06 to .11", fixed = TRUE)
  expect_match(rd, ".02 to .03", fixed = TRUE)
  expect_match(rd, "moves \\emph{further} from nominal as N grows",
               fixed = TRUE)
  # (iii) the direction of the error, which is the part a user acts on.
  expect_match(rd, "over-flags", fixed = TRUE)
  # (iv) the FIML scoping. Without it the rates above read as covering a path
  # where no rejection rate has ever been measured.
  expect_match(rd, "rejection rate has not been measured", fixed = TRUE)
})


test_that("AC11: the vignette carries the same four claims", {
  vig <- test_path("..", "..", "vignettes", "axes-reliability.Rmd")
  skip_if_not(file.exists(vig))
  txt <- gsub("\\s+", " ", paste(readLines(vig, warn = FALSE), collapse = " "))
  expect_gt(nchar(txt), 1000L)

  expect_match(txt, "| .092 | .079 | .062 | .054 |", fixed = TRUE)
  expect_match(txt, "sweep at a single population, not a general threshold",
               fixed = TRUE)
  expect_match(txt, "measured .06 to .11 at three populations chosen to bracket",
               fixed = TRUE)
  expect_match(txt, "**over-flags** misfit rather than flattering it",
               fixed = TRUE)
  expect_match(txt, "rejection rate has not been measured", fixed = TRUE)
})


test_that("AC11: the printed note gives direction and a pointer, and no rates", {
  skip_if_not_installed("lavaan")
  pp <- probe_octant()
  res <- axes_reliability(cormat = pp$sigma, items = pp$items,
                          angles = pp$angles, n = 600)
  out <- gsub("\\s+", " ", paste(capture.output(summary(res)), collapse = " "))

  # BC5 asks for the note BESIDE the chi-square/RMSEA/CFI line, so its position
  # is asserted and not just its presence: nothing may come between the fit line
  # and the note (M68 review, F16 -- it used to sit two blocks above, printed by
  # print() before summary() had emitted the components table or the fit line).
  lines <- capture.output(summary(res))
  fit_at <- grep("^  chi-square\\(", lines)
  note_at <- grep("The global fit statistics chisq, pvalue, rmsea and cfi",
                  lines, fixed = TRUE)
  expect_length(fit_at, 1L)
  expect_length(note_at, 1L)
  expect_true(note_at > fit_at)
  expect_true(all(!nzchar(trimws(lines[seq(fit_at + 1L, note_at - 1L)]))))
  # ... and print() alone no longer carries it, so it appears exactly once.
  pr <- gsub("\\s+", " ", paste(capture.output(print(res)), collapse = " "))
  expect_no_match(pr, "The global fit statistics chisq, pvalue, rmsea and cfi",
                  fixed = TRUE)

  expect_match(out, "can modestly over-reject at typical sample sizes",
               fixed = TRUE)
  expect_match(out, "over-flags misfit rather than flattering it", fixed = TRUE)
  expect_match(out, "?axes_reliability for the measured rates", fixed = TRUE)

  # And deliberately NO rates here. A number printed on this surface would
  # drift out of agreement with the Rd and the vignette the first time the
  # fixture is regenerated, because nothing ties it to them (BC5).
  for (bad in c(".06 to .11", ".092", ".054", "p*/N")) {
    expect_no_match(out, bad, fixed = TRUE)
  }
})


test_that("AC11: the documented rates are the fixture's rates", {
  # BC5's tie: the published numbers are the committed fixture's, rounded, and
  # move only with it. Without this the documentation and the evidence are two
  # independent records of the same quantity, which is how they drift.
  fx <- m68_cells()
  rates <- vapply(M68_POPS, function(nm) {
    cell <- m68_ok(fx$cells[[nm]])
    mean(cell[, "p"] < .05)
  }, numeric(1))
  # The Rd and vignette say ".06 to .11" at these three populations.
  expect_gte(min(rates), .06 - .005)
  expect_lte(max(rates), .11 + .005)

  unscaled <- vapply(M68_POPS, function(nm) {
    cell <- m68_ok(fx$cells[[nm]])
    mean(cell[, "p_unscaled"] < .05)
  }, numeric(1))
  # ... and ".02 to .03" for the uncorrected statistic.
  expect_gte(min(unscaled), .02 - .005)
  expect_lte(max(unscaled), .03 + .005)

  # The sweep numbers the Rd prints, against the sweep cells they came from.
  want <- c("600" = .092, "1200" = .079, "2400" = .062, "4800" = .054)
  for (nn in names(want)) {
    cell <- m68_ok(fx$sweep[[nn]])
    expect_lt(abs(mean(cell[, "p"] < .05) - want[[nn]]), .006, label = nn)
  }
})


test_that("AC12: the Rd fences the scaling against the robustness misreading", {
  rd <- if (file.exists(test_path("..", "..", "man", "axes_reliability.Rd"))) {
    paste(readLines(test_path("..", "..", "man", "axes_reliability.Rd"),
                    warn = FALSE), collapse = " ")
  } else {
    db <- tools::Rd_db("circumplex")
    paste(as.character(db[["axes_reliability.Rd"]]), collapse = "")
  }
  rd <- gsub("\\s+", " ", rd)

  # A Satorra-Bentler scaled statistic is best known as the fix for
  # NON-NORMALITY, which this one is not: the factor is normal-theory
  # throughout and corrects the correlation-versus-covariance metric only. The
  # same package reports genuine robust scaled statistics from ssm_sem(), so
  # the two are one `?` away from each other and the confusion is live.
  expect_match(rd, "not a robustness correction for non-normal data",
               fixed = TRUE)
  expect_match(rd, "computed under normal theory throughout", fixed = TRUE)
  # The paired positive on the other half: it must also say what the factor
  # DOES correct, or the fence could be satisfied by a bare denial.
  expect_match(rd, "corrects one thing only", fixed = TRUE)
  # And the cross-reference that keeps a reader from importing ssm_sem()'s
  # meaning of the same author names.
  expect_match(rd, "unrelated to the Satorra-Bentler scaled statistics reported by",
               fixed = TRUE)
})


# ---- AC3: the FIML path's mean calibration ----------------------------------
#
# This is the ONLY oracle behind M68-D1 -- the decision to scale the FIML path
# with the complete-data Gamma_R at Sigma-hat rather than one rebuilt from the
# FIML fit's own saturated stage. No complete-data reference value covers that
# choice, so a miss here falsifies the decision rather than needing a fix.
#
# Regenerate with `Rscript devel/m68-fiml-scaled-cells.R` (~1 h; the M1 MAR
# cell fits at N = 2400).

test_that("AC3: mean(T_s)/df is in [0.95, 1.05] in every FIML cell", {
  fx <- readRDS(test_path("fixtures", "m68-fiml-scaled-cells.rds"))
  # The seeds are the M65 and M66 fixtures' own, so these are the SAME draws
  # those two measured point estimates and standard errors on -- not a fresh
  # sample that merely looks comparable. Assert the provenance, because that
  # sharing is the reason the three sets of evidence can be read together.
  m65 <- readRDS(test_path("fixtures", "m65-heavy-cells.rds"))
  m66 <- readRDS(test_path("fixtures", "m66-corrected-se-cells.rds"))
  expect_identical(fx$provenance$seeds$mcar, m65$provenance$seeds$mcar)
  expect_identical(fx$provenance$seeds$m1, m66$provenance$seeds$m1)

  for (nm in c("0.02", "0.05", "0.10", "m1")) {
    cell <- fx$cells[[nm]]
    ok <- stats::complete.cases(cell)
    expect_gte(sum(ok), 190L, label = paste("survivors", nm))
    got <- mean(cell[ok, "chisq_scaled"]) / cell[which(ok)[1], "df"]
    expect_gt(got, 0.95, label = paste("mean(T_s)/df", nm))
    expect_lt(got, 1.05, label = paste("mean(T_s)/df", nm))
  }
})


test_that("AC3: the FIML factor is the complete-data one, per M68-D1", {
  fx <- readRDS(test_path("fixtures", "m68-fiml-scaled-cells.rds"))
  cells <- readRDS(test_path("fixtures", "m68-scaled-fit-cells.rds"))
  c_pop <- cells$population_diagnostics$strong$cfactor

  # M68-D1 chose the complete-data Gamma_R at Sigma-hat over a saturated-stage
  # reconstruction, on the ground that lavaan's FIML chi-square already prices
  # the missing information. The observable consequence: the factor is a
  # property of the POPULATION matrix, so it must not drift with the
  # missingness rate. A saturated-information factor would.
  facs <- vapply(c("0.02", "0.05", "0.10"), function(nm) {
    cell <- fx$cells[[nm]]
    mean(cell[stats::complete.cases(cell), "cfactor"])
  }, numeric(1))
  expect_lt(max(abs(facs - c_pop)), .01)
  # And no trend across a fivefold change in the missingness rate.
  expect_lt(abs(facs[["0.10"]] - facs[["0.02"]]), .005)
})


# ---- M69 / AC4: the cross-file citation cannot rot silently ------------------

test_that("AC4: axes_scaled_fit's Wc citation still lands on the Wc fold", {
  # An INSTALLED package carries no R/ sources, so under R CMD check these paths
  # do not exist and readLines() errors outright. Guarded the way this file's
  # other source-reading guards are. Stated plainly because a silent skip is
  # false coverage (the M7 lesson): this guard runs under devtools::test() and
  # is SKIPPED under R CMD check and on CRAN, so it fences the citation in
  # development only -- which is where a citation rots.
  cite_path <- test_path("..", "..", "R", "axes_scaled_fit.R")
  target_path <- test_path("..", "..", "R", "axes_corrected_se.R")
  skip_if_not(file.exists(cite_path) && file.exists(target_path),
              "package R/ sources absent (installed package)")
  cite_src <- readLines(cite_path)
  target <- readLines(target_path)
  expect_gt(length(target), 100L)

  # The citation, parsed rather than assumed: "(R/axes_corrected_se.R:A-B)".
  hits <- regmatches(
    cite_src,
    regexpr("R/axes_corrected_se\\.R:[0-9]+-[0-9]+", cite_src)
  )
  hits <- hits[nzchar(hits)]
  expect_length(hits, 1L)

  rng <- as.integer(strsplit(sub(".*:", "", hits[[1]]), "-", fixed = TRUE)[[1]])
  expect_lt(rng[[2]] - rng[[1]], 15L)      # AC4: at most a 15-line span
  expect_lte(rng[[2]], length(target))

  # It must actually contain the fold it claims to point at. A bare "the file
  # exists" check would pass over any range at all -- the loophole the M69
  # criteria audit flagged in the first draft of this criterion.
  cited <- target[rng[[1]]:rng[[2]]]
  expect_true(any(grepl("diag(wc) <- -rowSums(wc * sigma)", cited, fixed = TRUE)))

  # AC4 requires the cited range to state EACH SIDE's pricing. Both assertions
  # are made over `cited` — the parsed range — and never over the citing file
  # at large: an earlier draft checked "cov2cor(Sigma-hat)" anywhere in
  # axes_scaled_fit.R, which its own prose satisfied, so the guard passed
  # without the cited range saying anything about pricing at all (M69 review
  # round 1, F21).
  cited_txt <- paste(cited, collapse = " ")
  expect_true(grepl("unit diagonal", cited_txt, fixed = TRUE))
  expect_true(grepl("cov2cor(Sigma-hat)", cited_txt, fixed = TRUE))
  expect_true(grepl("naive", cited_txt, fixed = TRUE) &&
                grepl("raw", cited_txt, fixed = TRUE))
})


# ---- M70 AC4: which CFI variant the reported value actually is --------------

test_that("AC4: the reported cfi IS the cfi.scaled definition, not cfi.robust", {
  skip_if_not_installed("lavaan")
  pp <- probe_octant()
  bad <- pp$sigma
  bad[1, 5] <- bad[5, 1] <- bad[1, 5] + 0.30
  bad[2, 9] <- bad[9, 2] <- bad[2, 9] - 0.25
  skip_if(min(eigen(bad, symmetric = TRUE)$values) <= 0,
          "perturbed probe matrix is not positive definite")

  res <- suppressMessages(axes_reliability(cormat = bad, items = pp$items,
                                           angles = pp$angles, n = 1500))
  skip_if(!is.null(res$details$fit_scaling_failed),
          "the scaling failed on this probe")

  # Every input is read off the object. That IS the claim under test: a reader
  # can settle which variant they are looking at without refitting anything.
  t_unscaled <- res$details$fit_uncorrected$chisq
  tb_unscaled <- unname(res$details$baseline[["chisq"]])
  df <- res$fit$df
  df_b <- unname(res$details$baseline[["df"]])
  cc <- unname(res$details$scaling_factor[["model"]])
  cb <- unname(res$details$scaling_factor[["baseline"]])

  # Both excesses strictly positive. At perfect fit both definitions truncate
  # to 1 and the comparison below would pass while distinguishing nothing.
  ex_scaled <- t_unscaled / cc - df
  ex_b_scaled <- tb_unscaled / cb - df_b
  expect_gt(ex_scaled, 0)
  expect_gt(ex_b_scaled, 0)

  # cfi.scaled -- the excesses of the SCALED statistics over the UNCHANGED df.
  cfi_scaled <- 1 - max(ex_scaled, 0) / max(ex_scaled, ex_b_scaled, 0)
  expect_equal(res$fit$cfi, cfi_scaled, tolerance = 1e-10)

  # cfi.robust (Brosseau-Liard & Savalei) -- the excesses of the UNSCALED
  # statistics over each df multiplied by its OWN scaling factor. The two
  # definitions coincide exactly when the model and baseline factors are equal,
  # so the probe pins that they are not: without that, a passing difference
  # would be an accident of this matrix rather than a property of the
  # definitions.
  expect_gt(abs(cc - cb), 1e-6)
  ex_rob <- t_unscaled - cc * df
  ex_b_rob <- tb_unscaled - cb * df_b
  cfi_robust <- 1 - max(ex_rob, 0) / max(ex_rob, ex_b_rob, 0)
  expect_gt(abs(cfi_scaled - cfi_robust), 1e-4)

  # Corroboration only, against lavaan's own implementation of the definition
  # the assertions above already pin. `lav_fit_cfi()` is UNEXPORTED, so neither
  # its existence nor its argument names are a contract -- reached via get(),
  # and any failure to call it skips rather than reddening (the M68 round-2
  # lesson).
  ref <- tryCatch({
    f <- get("lav_fit_cfi", envir = asNamespace("lavaan"))
    f(X2 = t_unscaled / cc, df = df, X2.null = tb_unscaled / cb, df.null = df_b)
  }, error = function(e) NULL, warning = function(w) NULL)
  skip_if(is.null(ref) || length(ref) != 1L || anyNA(ref),
          "lavaan::lav_fit_cfi is not callable with these arguments")
  expect_equal(res$fit$cfi, unname(ref), tolerance = 1e-10)
})


# ---- M70 AC5: the nonpositive-diagonal guard is NA-safe ---------------------

test_that("AC5: a non-finite fitted diagonal refuses cleanly instead of erroring", {
  # Deliberately no skip_if_not_installed("lavaan"): unlike the rest of this
  # file, nothing here fits a model. `df` and `baseline_df` are computed from
  # the map so the guards above line 103 pass, and the diagonal check is
  # reached with no estimator involved.
  pp <- probe_octant()
  p <- nrow(pp$sigma)
  df <- p * (p + 1) / 2 - length(
    axes_se_derivs(pp$item_angle, pp$scale, NULL, TRUE, FALSE)$mats
  )
  baseline_df <- p * (p - 1) / 2

  call_it <- function(sigma) {
    axes_scaling_factor(sigma, pp$names, pp$item_angle, pp$scale,
                        fit_zeta1 = TRUE, fit_zeta2 = FALSE,
                        df = df, baseline_df = baseline_df)
  }

  # An NA and a NaN entry each used to error out of `if (NA)` with "missing
  # value where TRUE/FALSE needed", instead of the named-reason NA this
  # function's own header promises. With the predicate NA-safe they fall
  # through to cov2cor() and are caught by the solve()/is.finite pair below it
  # -- which is why the reason is "singular" and not "nonpositive_diagonal".
  # Two warnings arrive on these paths, R's own cov2cor() non-finite-diagonal
  # warning first and this function's refusal second; the refusal is the one
  # asserted, and the first is expected rather than suppressed.
  for (bad_value in list(NA_real_, NaN)) {
    sig <- pp$sigma
    sig[2L, 2L] <- bad_value
    w <- testthat::capture_warnings(got <- call_it(sig))
    # Both are asserted rather than one asserted and one left to escape as a
    # test warning: cov2cor()'s arrival is a fact about the route this input
    # takes, and the route is what the fix changed.
    expect_true(any(grepl("could not be computed", w, fixed = TRUE)))
    expect_true(any(grepl("diag", w, fixed = TRUE)))
    expect_identical(got$reason, "singular")
    expect_identical(got$scale, NA_real_)
    expect_identical(got$baseline, NA_real_)
  }

  # The unchanged control: a zero variance still takes the guard's own door, so
  # the fix rejects nothing it did not reject before.
  sig0 <- pp$sigma
  sig0[3L, 3L] <- 0
  expect_warning(got0 <- call_it(sig0), "could not be computed")
  expect_identical(got0$reason, "singular")
  expect_identical(got0$scale, NA_real_)
  expect_identical(got0$baseline, NA_real_)
})
