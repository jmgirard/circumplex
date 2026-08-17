# Correlation-metric-calibrated global fit statistics -------------------------
#
# axes_reliability() fits its model to the item CORRELATION matrix as if it were
# a covariance matrix (Strack et al.'s own LISREL practice). M66 corrected the
# component standard errors for that; this file corrects the OTHER side, the
# global test statistic, which carries the same mismatch in the opposite
# direction -- lavaan refers T to a chi-square derived for a Wishart-distributed
# sample covariance matrix, while the analyzed moments are correlations, whose
# diagonal has zero sampling variability and whose off-diagonal is LESS variable
# than the corresponding covariance (var(sqrt(n) r_ij) = (1 - rho^2)^2 against
# (1 + rho^2)). Referred to the wrong reference distribution, T comes out too
# small and fit is flattered -- measured at RR13 as E[T] = 261.1 against
# df = 273 at the probe population, about 4%.
#
# The correction is Satorra & Bentler's scaled statistic, satorra1994 (p. 407):
#
#   T_s = T / c        [eq. 16.21]        c = trace{U Gamma / r}   [eq. 16.22]
#
# with r = p* - q the degrees of freedom and, satorra1994 (p. 406),
#
#   U = H - H Delta (Delta' H Delta)^-1 Delta' H                   [eq. 16.18]
#
# taking H = V, the normal-theory ML weight matrix (their eq. 16.4). What makes
# this legitimate on correlations rather than covariances is the chapter's own
# scope sentence at satorra1994 (p. 401): Gamma is the asymptotic covariance of
# WHATEVER moment vector is analyzed, not specifically of covariances. So the
# whole correction is the substitution of Gamma_R for Gamma_S. The problem being
# fixed is the one cudeck1989 (pp. 322-323) states; note that Cudeck's own
# "Error (b)" is a DIFFERENT error (see cairn/references/cudeck1989.md).
#
# Strength of the claim, stated exactly because it is easy to overstate: the
# scaled statistic agrees with the reference chi-square in MEAN always, and is
# exactly chi-square only when the eigenvalues of U Gamma are all equal
# satorra1994 (p. 407), cases a and b. This model is in the chapter's case (c),
# where a better approximation is a supported conjecture, not a theorem. AC3's
# simulation is what establishes that it holds here.
#
# Rejected route: Satorra & Bentler's ADJUSTED (Satterthwaite-type) statistic,
# which matches mean AND variance by moving to noninteger degrees of freedom
# satorra1994 (p. 409). It would change `$fit$df`, a documented return field,
# from an integer count of overidentifying restrictions into a fitted quantity.


# The scaling factor at the fitted Sigma-hat, and the independence model's own
# factor for CFI.
#
# `sigma` is the model-implied covariance from the fitted lavaan object and
# `item_names` the item map's own order; THE TWO ARE NOT THE SAME ORDER, for
# exactly the reason axes_corrected_se() documents at length -- lavaan orders
# variables by first appearance in the syntax. Realignment happens here, off the
# matrix's own dimnames, and a matrix carrying none is refused.
#
# `sigma` is then normalized with cov2cor(). Two things make that necessary
# rather than cosmetic. First, lavaan's `sample.cov.rescale` multiplies the
# analyzed matrix by (N-1)/N, so the fitted diagonal comes back at 0.9983 rather
# than 1 on a well-fitting n = 600 probe; Gamma_R's entries are functions of
# CORRELATIONS and (1 - rho^2)^2 is meaningless at rho = 1.03. Second, under
# misspecification the implied diagonal is not even constant (measured range
# 0.951-1.026 on a deliberately perturbed probe), so no single scalar undoes it.
# The normalization is exact rather than approximate: T itself is invariant to a
# scalar rescaling of both matrices, and pricing U and Gamma_R at the same
# implied CORRELATION matrix is the coherent reading of an estimand that is
# defined on the correlation metric to begin with.
#
# `df` and `baseline_df` come from lavaan rather than being recomputed, because
# they are the divisors the statistic is actually referred against. They are
# CHECKED against this function's own derivative set: a mismatch means Delta is
# not this model's Delta, so U would project onto the wrong space and c would be
# a plausible-looking wrong number.
#
# Returns `scale` (c), `baseline` (c_b), and `reason` -- NULL on success, or a
# string naming why both factors are NA. The two are NA together and neither
# ever falls back to 1: reporting the uncorrected statistic as the corrected one
# is the single failure a user could not detect (the M66 contract).
axes_scaling_factor <- function(sigma, item_names, item_angle_deg, item_scale,
                                item_block = NULL, fit_zeta1, fit_zeta2,
                                df, baseline_df) {
  if (is.null(rownames(sigma)) || is.null(colnames(sigma))) {
    stop(
      "`sigma` must carry dimnames so it can be realigned to the item map.",
      call. = FALSE
    )
  }
  sigma <- sigma[item_names, item_names, drop = FALSE]

  d <- axes_se_derivs(item_angle_deg, item_scale, item_block,
                      fit_zeta1, fit_zeta2)
  na_out <- function(reason) {
    warning(
      "The scaled fit statistics could not be computed (", reason,
      "); they are reported as NA.",
      call. = FALSE
    )
    list(scale = NA_real_, baseline = NA_real_, reason = reason)
  }

  p <- nrow(sigma)
  q <- length(d$mats)
  if (!isTRUE(p * (p + 1) / 2 - q == df)) return(na_out("df_mismatch"))
  if (!isTRUE(p * (p - 1) / 2 == baseline_df)) {
    return(na_out("baseline_df_mismatch"))
  }
  # A saturated model has no chi-square to scale: T = 0 on 0 degrees of
  # freedom, and the cval line below divides by df. Before M90 this reached
  # cval = Inf and fell out of the final refusal as "indefinite" -- a claim
  # about the user's model that df = 0 never licenses. Checked AFTER the two
  # df-consistency guards (a df = 0 that does not match the derivative set is
  # a df_mismatch, not a saturation) and before any matrix computation; this
  # guard is also what keeps df nonzero at the cval division (M90 AC1).
  if (df == 0) return(na_out("saturated"))
  # `na.rm = TRUE` is load-bearing, not defensive. Without it a single NA or
  # NaN on the fitted diagonal makes the predicate NA and `if (NA)` ERRORS with
  # "missing value where TRUE/FALSE needed" -- in place of the named-reason NA
  # this function's header promises, and which every other refusal here honors.
  # With na.rm an NA or NaN entry falls THROUGH this guard and is refused as
  # "singular" by axes_sigma_degenerate()'s finiteness arm below (before M89 it
  # traveled further, to the solve()/tryCatch/is.finite pair, same literal).
  # This guard exists for a diagonal that is finite but not positive, which
  # cov2cor() would silently turn into NaN correlations, and `<= 0` still
  # catches every such entry with na.rm in place.
  #
  # NA and NaN, deliberately, and not "non-finite" in general. The three
  # non-finite entries take three different doors here, and the ORDER of the
  # two guards below is what keeps them apart:
  #
  #   -Inf   caught by `<= 0`, which is true of it        -> "singular"
  #   NA/NaN falls through both (na.rm above, and is.infinite() is FALSE on
  #          both), and is caught by axes_sigma_degenerate()'s finiteness
  #          arm below                                    -> "singular"
  #   +Inf   fails `<= 0`, so the second guard is the only thing that refuses
  #          it                                           -> "infinite_diagonal"
  #
  # +Inf needs its own guard because it is the one entry cov2cor() launders
  # rather than propagates: it maps that row/column to zeros with a unit
  # diagonal, which solve() inverts happily and is.finite() accepts, so before
  # M71 a factor was computed from a corrupted matrix and returned with reason
  # NULL (measured 0.9579017 at the M70 review) -- an uncorrected-from-corrupted
  # number in a field documented as corrected, the one failure a user could not
  # detect. Testing `!is.finite()` here instead, or ahead of the `<= 0` guard,
  # would refuse the same input but silently relabel the -Inf and NA/NaN routes
  # above, which are correct as they stand; tests pin all three.
  #
  # The sibling guards in axes_corrected_se() have named the same literals on
  # every one of these routes since M89 unified the two surfaces' reason
  # vocabulary (its `<= 0` door said "nonpositive_diagonal" before, and +Inf
  # fell out of its raw-matrix pricing as "unidentified").
  if (any(diag(sigma) <= 0, na.rm = TRUE)) return(na_out("singular"))
  if (any(is.infinite(diag(sigma)))) return(na_out("infinite_diagonal"))

  # The stated degeneracy criterion (M89), shared with axes_corrected_se() --
  # see axes_sigma_degenerate() in R/axes_corrected_se.R for the criterion and
  # its rationale. It prices cov2cor(sigma) deliberately (M89 re-cut, RR18):
  # every quantity this surface computes is a function of the correlation
  # matrix alone, so raw-metric conditioning is not the error model of
  # anything computed here -- the first cut priced the raw matrix and refused
  # pure diagonal rescalings this surface prices exactly. The criterion's
  # finiteness arm is evaluated on the RAW matrix, ahead of the cov2cor() its
  # conditioning arm prices: cov2cor() of an NA/NaN diagonal emits its own
  # warning first, and the M71 contract is exactly one warning per refusal --
  # this function's own, with no cov2cor() noise. Finiteness is metric-blind,
  # so hoisting it moves no refusal across the two arms' boundary.
  if (!all(is.finite(sigma))) return(na_out("singular"))
  sigma <- stats::cov2cor(sigma)
  degenerate <- axes_sigma_degenerate(sigma)
  if (!is.null(degenerate)) return(na_out(degenerate))

  si <- tryCatch(solve(sigma), error = function(e) NULL)
  if (is.null(si) || !all(is.finite(si))) return(na_out("singular"))

  # --- trace{V Gamma_R}, in closed form ---------------------------------------
  #
  # The p* x p* product is never formed. Writing the operators on symmetric
  # matrices -- V: E -> 0.5 Sigma^-1 E Sigma^-1, Gamma_S: E -> 2 Sigma E Sigma,
  # and the standardization Jacobian J: E -> E - 0.5 (diag(E) Sigma + Sigma
  # diag(E)) with Gamma_R = J Gamma_S J' -- and taking the trace over an
  # orthonormal basis of symmetric matrices collapses to one sum over item
  # pairs:
  #
  #   trace{V Gamma_R} = sum_{k<l} [ 1 - (Sigma^-1)_kl rho_kl (1 - rho_kl^2) ]
  #
  # Two things check it. Substituting Gamma_S for Gamma_R gives p* exactly (the
  # operators are inverses), and the identity reduces to tr(Sigma^-1 Sigma) = p
  # when the two derivations of it are equated. The vech-space oracle in
  # tests/testthat/test-axes-scaled-fit.R recomputes it from literal matrices
  # and agrees to 1e-15 on the probe maps.
  up <- upper.tri(sigma)
  rho <- sigma[up]
  tr_vg <- sum(1 - si[up] * rho * (1 - rho^2))

  # --- the projection term ----------------------------------------------------
  #
  # trace{V Delta (Delta'V Delta)^-1 Delta'V Gamma_R} = sum_st A_st B_st with
  # A = (Delta'V Delta)^-1 -- the same information matrix axes_se_pricing()
  # builds -- and B_st = 2 tr(Wc_s Sigma Wc_t Sigma), where Wc is W with the
  # covariance-to-correlation Jacobian folded in exactly as it is there
  # (R/axes_corrected_se.R:171-179): off the diagonal W is unchanged, and the
  # diagonal absorbs the standardization because a sample correlation's diagonal
  # does not vary at all.
  #
  # "Exactly as it is there" is now literally true on BOTH sides. Until M69 it
  # was not: this file normalized with cov2cor() at line 104 while
  # axes_corrected_se() folded at the raw Sigma-hat, so the two surfaces priced
  # the same construction at different matrices. Both are at cov2cor(Sigma-hat)
  # now (D-037). A guard in tests/testthat/test-axes-scaled-fit.R parses the
  # line range out of this comment and asserts it still lands on the fold, so
  # this citation reddens rather than rotting the next time the code moves --
  # which is how it came to be stale in the first place.
  sim <- lapply(d$mats, function(m) si %*% m)
  info <- matrix(0, q, q)
  for (s in seq_len(q)) {
    for (t in s:q) {
      info[s, t] <- info[t, s] <- 0.5 * sum(sim[[s]] * t(sim[[t]]))
    }
  }
  acov <- tryCatch(solve(info), error = function(e) NULL)
  if (is.null(acov) || !all(is.finite(acov))) return(na_out("unidentified"))

  ys <- lapply(sim, function(sm) {
    w <- 0.5 * sm %*% si
    diag(w) <- diag(w) - diag(sigma %*% w)
    w %*% sigma
  })
  bmat <- matrix(0, q, q)
  for (s in seq_len(q)) {
    for (t in s:q) {
      bmat[s, t] <- bmat[t, s] <- 2 * sum(ys[[s]] * t(ys[[t]]))
    }
  }

  cval <- (tr_vg - sum(acov * bmat)) / df

  # --- the baseline (independence) model's factor -----------------------------
  #
  # Same construction with Delta_b the p unit-variance derivatives and the
  # implied matrix the identity (the baseline fitted to a correlation matrix).
  # It collapses to a scalar mean: the free parameters are the variances, whose
  # sample correlations have zero sampling variability, so the whole projection
  # term vanishes and U_b reduces to V_b. What remains is the average of the
  # Pearson-Filon variances over item pairs. Gamma_R stays at sigma-hat -- it
  # describes the data, not the model, and the independence model's own implied
  # matrix is a grossly inconsistent estimate of it.
  cb <- sum((1 - rho^2)^2) / baseline_df

  # Defensive backstop, not a diagnosis (M90 AC3/AC5). Four arms:
  #   cb <= 0 and !is.finite(cb): analytically unreachable -- cb =
  #     sum((1 - rho^2)^2) / baseline_df over finite rho (sigma passed the
  #     finiteness gate above), so cb is finite and >= 0, with equality only
  #     if every |rho| = 1, a matrix the degeneracy criterion refuses first.
  #     baseline_df > 0 whenever this line runs: the baseline_df guard pins
  #     baseline_df = p(p - 1)/2, which is 0 only at p = 1 -- where the
  #     information matrix (q x q from a single moment, rank <= 1 < q) is
  #     singular, so acov's solve refuses "unidentified" before this line
  #     (measured at the M90 round-2 review).
  #   cval <= 0 and !is.finite(cval): tr(U Gamma) >= 0 in exact arithmetic
  #     (U is the psd projection residual of V, Gamma_R is psd) and df is
  #     nonzero past the saturation guard, so a nonpositive or non-finite cval here
  #     is double-precision cancellation, never evidence about the user's
  #     model -- which is why this door stopped saying "indefinite" at M90.
  #     The criterion's tau floor caps the cancellation error a computed
  #     cval can carry (RR18), and the one matrix measured to produce
  #     cval < 0 in doubles (exemplar B, true cval +0.0556) is refused
  #     upstream. Recorded search (M90 AC5, work log): 30,000
  #     criterion-accepted draws spanning p in {3, 8, 24} plus adversarial
  #     hill-climbs reached this branch 0 times; the nearest miss, an
  #     adversarial p = 3 / df = 1 draw at cval = +1.2e-5, still computes.
  #     Retained because "not reached by the search" is not "unreachable":
  #     the p = 3 / df = 1 family approaches 0+ continuously, so a deeper
  #     adversary could land inside the cancellation noise.
  if (!is.finite(cval) || !is.finite(cb) || cval <= 0 || cb <= 0) {
    return(na_out("ill_conditioned"))
  }
  list(scale = cval, baseline = cb, reason = NULL)
}


# Rebuild the reported fit statistics from the scaled chi-square.
#
# `fm` is lavaan's own vector, carrying the six reported measures plus the three
# the recomputation needs (baseline.chisq, baseline.df, ntotal). `cf` is
# axes_scaling_factor()'s return.
#
# The four chi-square-derived statistics are recomputed from T_s = T/c using the
# published definitions rather than re-asked of lavaan, which has no idea the
# statistic moved. `df` and `srmr` pass through untouched: df is a count of
# overidentifying restrictions and srmr is a residual summary, and neither is a
# test statistic with a reference distribution to recalibrate.
#
# The definitions are pinned against lavaan's own uncorrected output by a test
# on a fit whose chi-square exceeds df -- lavaan is a Suggests with no version
# floor, so a future change to how it forms rmsea or cfi would otherwise leave
# the scaled values silently disagreeing with the unscaled ones beside them.
#
# On a failed factor the four go NA together and `df`/`srmr` still report. There
# is deliberately no fallback to lavaan's unscaled values: a warned-about
# uncorrected number in a field documented as corrected is the one failure mode
# a user could not detect (M68-D1).
axes_scale_fit_measures <- function(fm, cf) {
  want <- c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr")
  uncorrected <- as.list(fm[want])

  df <- fm[["df"]]
  if (is.null(cf$reason) && is.finite(cf$scale)) {
    ts <- fm[["chisq"]] / cf$scale
    tbs <- fm[["baseline.chisq"]] / cf$baseline
    # CFI's 0/0 case, handled exactly as lavaan's own lav_fit_cfi() handles it:
    # when the model and the baseline BOTH fit at or under their degrees of
    # freedom, both excesses are zero and the ratio is undefined. lavaan returns
    # 1 there; the published definition's limit is 1; and the arithmetic
    # otherwise returns NaN, which reads as a computation failure rather than as
    # perfect fit. The state is reachable -- an over-identified model on a
    # near-independence correlation matrix hits it (M68 review, F2).
    t1 <- max(ts - df, 0)
    t2 <- max(ts - df, tbs - fm[["baseline.df"]], 0)
    cfi <- if (isTRUE(all.equal(t1, 0)) && isTRUE(all.equal(t2, 0))) {
      1
    } else {
      1 - t1 / t2
    }
    scaled <- list(
      chisq = ts,
      df = df,
      pvalue = stats::pchisq(ts, df, lower.tail = FALSE),
      rmsea = sqrt(max(ts - df, 0) / (df * fm[["ntotal"]])),
      cfi = cfi,
      srmr = fm[["srmr"]]
    )
  } else {
    scaled <- list(
      chisq = NA_real_, df = df, pvalue = NA_real_,
      rmsea = NA_real_, cfi = NA_real_, srmr = fm[["srmr"]]
    )
  }
  list(fit = scaled, uncorrected = uncorrected)
}
