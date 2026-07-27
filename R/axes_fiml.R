# The FIML correlation metric for axes_reliability(missing = "fiml") -- M65.
#
# RR12's load-bearing holding: under MAR, standardizing the items by their
# AVAILABLE-CASE means and SDs is dishonest. The standardized columns then carry
# k_i * k_j * rho_ij, and the model has no free per-item parameter OFF the
# diagonal to absorb an item-specific multiplicative distortion, so the
# distortion lands in the estimated components (measured +0.0167 on xi1 -- about
# one full SE at N = 600 -- against +0.0008 for the two metric-correct routes).
#
# The honest construction standardizes by the SATURATED-model FIML (EM) moments
# instead, and feeds those columns to one structured FIML fit. Both stages see
# the same information, so nothing is estimated twice on different grounds.


# The saturated (h1) stage, isolated as its own function for two reasons: it is
# the mockable seam BC7 clause (iv) refuses on, and it is the single place the
# EM estimator is chosen, so M65-D1's route is stated once.
#
# Why lavCor(output = "fit") rather than an explicit saturated model: BC2/BC6
# demand 1e-12 elementwise agreement with scale()/cor() on complete data, and a
# saturated lavaan::sem() reaches only ~1.3e-07 -- its general optimizer's
# convergence tolerance, five orders short, and not a thing model tuning fixes.
# lavaan's EM routine reaches ~1e-15 and is ~90x faster (M65-D1, measured).
#
# NOTE the convergence predicate is this function's own, NOT axes_converged():
# lavInspect(fit, "converged") describes the STRUCTURED optimizer, and this
# stage never runs one -- it reports FALSE on a perfectly healthy saturated fit
# (measured: FALSE with all 324 saturated parameters recovered to 1e-15). Using
# it here would refuse every dataset. What actually fails at this stage is the
# EM loop hitting its iteration cap, which lavaan reports as a warning and then
# returns its last iterate anyway -- silently usable-looking, so it is caught by
# listening for the warning rather than by inspecting the returned object.
axes_fiml_h1 <- function(dat) {
  stalled <- FALSE
  # `ordered = character(0)` pins every column as continuous. Ablated rather
  # than assumed (the M36 lesson): on a 5-point integer Likert fixture it
  # changes the returned moments by exactly 0 -- lavaan already treats integer
  # columns as continuous, and axes_reliability() refuses non-numeric input
  # upstream, so no factor can reach this call. It is retained as a PIN, not a
  # fix: a polychoric correlation is a different estimand that would arrive
  # looking like a valid correlation matrix, so the day lavaan's detection
  # default changes should be a test failure somewhere, not a silent estimand
  # swap here. Stated so a later reader does not credit it with current work.
  fit <- withCallingHandlers(
    lavaan::lavCor(
      dat,
      ordered = character(0),
      missing = "ml",
      output = "fit",
      meanstructure = TRUE
    ),
    warning = function(w) {
      if (grepl("iteration|converg", conditionMessage(w), ignore.case = TRUE)) {
        stalled <<- TRUE
      }
      invokeRestart("muffleWarning")
    }
  )
  h1 <- lavaan::lavInspect(fit, "h1")
  list(
    mean = h1$mean,
    cov = h1$cov,
    # Finiteness is checked beside the warning because the two failures are
    # independent: EM can hit its cap with finite iterates, and can return a
    # non-finite moment without warning when an item is degenerate.
    converged = !stalled && all(is.finite(h1$mean)) && all(is.finite(h1$cov))
  )
}


# Saturated-FIML moments, the standardized item matrix, and the coverage
# diagnostics BC8 reports. `mat` is the numeric item matrix in item-map order,
# missing cells as NA. Rows with NO observed item are dropped here (BC7): they
# carry no information for any moment, and leaving them in would inflate every
# denominator that N_used feeds.
axes_fiml_moments <- function(mat) {
  obs <- !is.na(mat)
  keep <- rowSums(obs) > 0L
  n_dropped <- sum(!keep)
  mat <- mat[keep, , drop = FALSE]
  obs <- obs[keep, , drop = FALSE]
  n_used <- nrow(mat)

  # Pairwise joint coverage: how many respondents answered BOTH items of a
  # pair. crossprod() on the observed-indicator matrix gives every pair at once;
  # the diagonal is per-ITEM coverage, which is a different quantity, so the
  # minimum is taken over the off-diagonal only.
  co <- crossprod(obs)
  min_coverage <- if (ncol(co) > 1L) min(co[upper.tri(co)]) else NA_real_

  h1 <- axes_fiml_h1(as.data.frame(mat))
  if (!isTRUE(h1$converged)) {
    stop(
      "The saturated (EM) stage did not converge, so the standardizing ",
      "moments and the item correlation matrix cannot be estimated.",
      call. = FALSE
    )
  }

  # The ML covariance divides by N, the sample SD by N - 1. Rescaling by
  # sqrt(N_used/(N_used - 1)) is what makes this metric reduce EXACTLY to
  # scale() on complete data (BC2) rather than merely closely -- the property
  # that lets the FIML path be checked against a known answer at all.
  mu <- h1$mean
  sdv <- sqrt(diag(h1$cov)) * sqrt(n_used / (n_used - 1))
  z <- sweep(sweep(mat, 2L, mu, "-"), 2L, sdv, "/")

  # R-hat is read off the same saturated fit, NOT recomputed from `z`: a
  # correlation of the standardized columns would be an available-case
  # correlation wearing the FIML metric's clothes -- exactly the quantity RR09
  # BC13 bans and D-033 was careful to say R-hat is not.
  list(
    z = z,
    mean = mu,
    sd = sdv,
    R = stats::cov2cor(h1$cov),
    n_used = n_used,
    n_dropped = n_dropped,
    n_complete = sum(stats::complete.cases(mat)),
    min_coverage = min_coverage
  )
}
