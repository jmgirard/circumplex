# Corrected asymptotic covariance for the axes-reliability components ----------
#
# axes_reliability() fits its model to the item CORRELATION matrix as if it were
# a covariance matrix (Strack et al.'s own LISREL practice). The point estimates
# are correct, but lavaan's normal-theory standard errors price the variability
# of a sample COVARIANCE matrix while the estimator consumes a sample
# CORRELATION matrix -- whose diagonal does not vary at all, and whose
# off-diagonal cells are less variable than the corresponding covariances
# (var(sqrt(n) r_ij) = (1 - rho^2)^2 against (1 + rho^2)). That is Cudeck's
# Error (c), cudeck1989 (p. 323): "If a model that is not scale invariant is
# applied to a correlation matrix with most computer programs, all of the
# estimated standard errors will be wrong." This model is not scale invariant
# (the derivation is in cairn/references/cudeck1989.md, not in the article --
# Cudeck never treats a circumplex).
#
# The size of that mismatch was first measured at M65 and derived in closed form
# at RR13: x1.44 at the probe population, and a ratio running [0.81, 1.97] over
# the accepted input space -- sign-unstable, so no static caveat could state it
# honestly (D-035 supersedes RR09 section 2 and D-026 holding (5), which had
# ruled "document, don't fix" without ever measuring the magnitude).
#
# The correction is exact for this model rather than approximate, because the
# implied covariance is LINEAR in the parameters: Sigma = xi1*C + xi2*J +
# zeta1*B + zeta2*K + diag(eps). The ML estimator therefore linearizes exactly
# as theta_hat - theta ~ tr(W(input - Sigma)), and both pricings of `input` are
# available in closed form.
#
# Attribution, stated exactly because it is easy to overstate: cudeck1989
# (p. 323) says a correction is needed and points at Browne (1982, section 1.6)
# for one -- "Formulas are available that give correct standard errors ... but
# as of this writing, these corrections have not been included in most computer
# programs." He prints no such formula in that article, and the Browne pages he
# points at are not on this repo's shelf. So the formula below is DERIVED here
# from the model's linear structure and validated by this repo's own oracles;
# cudeck1989 licenses the premise, not the algebra. The companion page for the
# test-statistic side of the same mismatch is satorra1994 (pp. 406-407), used in
# R/axes_scaled_fit.R.
#
# Rejected routes, each MEASURED rather than argued (RR13 section 3): lavaan's
# `correlation = TRUE` fits a different model class (npar 3, item errors
# determined) and moves xi1_hat by about 5 empirical SDs; robust/sandwich SEs
# are structurally blind to the in-sample standardization and measured no fix;
# refitting on the covariance metric with unit-variance constraints stays
# rejected on RR12 section 9's standing holding.


# The model's derivative matrices dSigma/dtheta, in the parameter order the
# information matrix and its inverse are indexed by. The COMPONENT derivatives
# come first, so row r of (Delta'V Delta)^-1 is component r's row; the p free
# item-error derivatives follow.
#
# Each component matrix carries its own DIAGONAL (C's, J's, B's and K's are all
# 1 there), because the fitted diagonal is xi1 + xi2 + zeta1 + zeta2 + eps_i and
# every component contributes to it. That is the faithful derivative, but it is
# NOT load-bearing here, and the difference matters to anyone mutating this
# function: stripping C's diagonal leaves both returned SEs BIT-IDENTICAL to 15
# decimals (measured at M66), so the whole suite stays green. The null is
# correct rather than a coverage hole -- the diagonal direction is spanned by
# the free item errors {E_ii}, so shifting a component's derivative diagonal is
# a unit-triangular reparameterization of nuisance parameters, and both the
# asymptotic variance and the influence representation this function returns are
# invariant under it. Recorded so a later session does not re-chase the green
# (the M60 lesson). Mutations that DO discriminate this matrix: cos(2*delta) for
# cos(delta) reddens 8 assertions, and dropping the W_c diagonal reddens 3.
axes_se_derivs <- function(item_angle_deg, item_scale, item_block,
                           fit_zeta1, fit_zeta2) {
  p <- length(item_scale)
  th <- as.numeric(item_angle_deg) * pi / 180
  d <- list(
    xi1 = cos(outer(th, th, "-")),
    xi2 = matrix(1, p, p)
  )
  if (fit_zeta1) d$zeta1 <- outer(item_scale, item_scale, `==`) * 1
  if (fit_zeta2) d$zeta2 <- outer(item_block, item_block, `==`) * 1
  n_comp <- length(d)
  # Free item residual variances: one E_ii per item.
  errs <- lapply(seq_len(p), function(i) {
    e <- matrix(0, p, p)
    e[i, i] <- 1
    e
  })
  list(mats = c(d, errs), components = names(d), n_comp = n_comp)
}


# Corrected (and, for comparison, uncorrected) asymptotic standard errors for
# every fitted variance component, evaluated at the fitted Sigma-hat.
#
# `sigma` is the model-implied covariance matrix from the fitted lavaan object;
# `item_names` is the item map's own order. THE TWO ARE NOT THE SAME ORDER.
# lavaan orders a model's variables by first appearance in the syntax, and
# axes_syntax() drops zero-weight loading terms from the AX line, so a pole
# scale's items appear later than the item map puts them: measured at the M66
# plan gate, `fitted(fit)$cov` on the canonical octant probe came back starting
# at item_04. Consuming it positionally pairs each item with ANOTHER item's
# angle and scale and returns SE(xi1) = 0.0046 where 0.01677 is right -- a 3.6x
# error with no error condition and a perfectly plausible-looking number. So the
# realignment happens HERE, once, off the matrix's own dimnames, rather than at
# each call site where one caller could forget it. A matrix carrying no
# dimnames cannot be realigned and is refused rather than consumed in whatever
# order it arrived.
#
# Returns a list of three named vectors plus `reason`. EACH IS PRICED AT A
# STATED MATRIX, and which one is not cosmetic (M69, RR15):
#
#   `naive`      normal-theory covariance ML, at the RAW realigned Sigma-hat.
#                Raw because this is the value that reproduces lavaan's own
#                reported SE to 1e-7, the only independent tie between this
#                derivative set and lavaan's implementation.
#   `corrected`  the correlation-structure value, at `cov2cor(Sigma-hat)`.
#   `fiml_ratio` `corrected` divided by the normal-theory SE, BOTH at
#                `cov2cor(Sigma-hat)` -- the metric-only conversion the FIML
#                path multiplies lavaan's observed-information SE by.
#
# Why the corrected side is normalized: the W_c fold below compresses the
# standardization differential using `Sigma_ij` in place of `rho_ij`, which is
# the same number ONLY when the diagonal is exactly 1. lavaan's
# `sample.cov.rescale` leaves the fitted diagonal at (N-1)/N, and under
# misspecification it is not even constant (measured 0.943-1.072 on a FIML fit,
# RR15 B3). Evaluated off the unit diagonal the fold is not the derived quantity
# at any scale -- decisively, scaling Sigma-hat by 2 scales the corrected SEs by
# 1.538/2.009/2.114 where a coherent variance-metric quantity gives exactly 2.
# RR13's own reproduction appendix derives both branches at the unit-diagonal
# population matrix, so this is fidelity to that derivation, not merely
# consistency with R/axes_scaled_fit.R (D-037).
#
# Why `fiml_ratio` is returned rather than composed at the call site: with only
# `naive` and `corrected` exposed, the mixed-matrix ratio
# `corrected$corrected / corrected$naive` is one plausible-looking expression
# away at every future call site, and it INFLATES the reported FIML SE by
# N/(N-1) -- 0.17% at n = 600, 1% at n = 100. Returning the ratio makes the
# same-matrix invariant a property of this function, testable once.
#
# `reason` is NULL on success, or a string naming why all three vectors are NA.
# They are NA together and never fall back to each other: relabelling the
# uncorrected number as corrected is the one failure a user could not detect.
# One pricing of the whole sandwich at one matrix. Called twice by
# axes_corrected_se() -- once at the raw Sigma-hat, once at cov2cor(Sigma-hat)
# -- because the two returned quantities are defined at different matrices and
# the duplicated linear algebra is 24x24 with q ~ 28, i.e. free.
#
# The parameter is named `sigma` deliberately: R/axes_scaled_fit.R's Wc comment
# cites the W_c fold below by line range and a guard asserts the citation still
# lands on it, so renaming this would redden that guard for a reason unrelated
# to what it guards.
#
# Returns a list of the two SE vectors, or a single string naming the failure.
axes_se_pricing <- function(sigma, d, n) {
  si <- tryCatch(solve(sigma), error = function(e) NULL)
  if (is.null(si) || !all(is.finite(si))) return("singular")

  sim <- lapply(d$mats, function(m) si %*% m)
  q <- length(sim)
  # The ML information matrix Delta'V Delta, exploiting
  # 0.5 * tr(Sigma^-1 M_s Sigma^-1 M_t) = 0.5 * sum((Sigma^-1 M_s) * t(Sigma^-1 M_t)).
  info <- matrix(0, q, q)
  for (s in seq_len(q)) {
    for (t in s:q) {
      info[s, t] <- info[t, s] <- 0.5 * sum(sim[[s]] * t(sim[[t]]))
    }
  }
  acov <- tryCatch(solve(info), error = function(e) NULL)
  if (is.null(acov) || !all(is.finite(acov))) return("unidentified")

  out <- vapply(seq_len(d$n_comp), function(r) {
    # W for component r: the derivative structure weighted by that component's
    # row of (Delta'V Delta)^-1, sandwiched by Sigma^-1.
    w <- 0.5 * si %*% Reduce(`+`, Map(`*`, d$mats, acov[r, ])) %*% si
    ws <- w %*% sigma
    naive <- 2 * sum(ws * t(ws))
    # W_c is W with the covariance->correlation Jacobian folded in: the sample
    # correlation's diagonal has ZERO sampling variance, and its off-diagonal
    # carries dr_ij = ds_ij - 0.5*rho_ij*(ds_ii + ds_jj). Off the diagonal W is
    # unchanged; the diagonal absorbs the standardization. The substitution of
    # `sigma` for rho is exact only at a unit diagonal, so `corrected` and
    # `fiml_ratio` are priced at cov2cor(Sigma-hat) while `naive` is priced raw.
    wc <- w
    diag(wc) <- 0
    diag(wc) <- -rowSums(wc * sigma)
    wcs <- wc %*% sigma
    corrected <- 2 * sum(wcs * t(wcs))
    c(sqrt(naive / n), sqrt(corrected / n))
  }, numeric(2))

  # Belt-and-braces on the contract stated above, NOT a fixed bug -- and the
  # distinction is recorded because the evidence is thinner than it looks.
  # In principle a nonsingular Sigma-hat need not be POSITIVE DEFINITE, and
  # solve() succeeding does not rule the indefinite case out; there
  # 2*tr(W_c S W_c S) could come out negative and sqrt() return NaN, leaving a
  # vector numeric in one component and NaN in another with `reason` still
  # NULL -- the mixed state the header forbids, and NaN rather than NA besides
  # (the M62 doctrine). An M66 reviewer reported measuring that on 96 of 300
  # indefinite draws. **Re-running it over 3822 indefinite matrices at this
  # layout reproduced it zero times, and this guard has never fired**, so the
  # reported case is unconfirmed and no end-to-end axes_reliability() call
  # reaches here at all (its positive-definiteness gate refuses such input
  # first). Kept anyway: it is one comparison, it costs nothing, and a header
  # that states a contract the code does not enforce is worse than no contract.
  if (!all(is.finite(out))) return("indefinite")

  list(naive = out[1, ], corrected = out[2, ])
}


axes_corrected_se <- function(sigma, item_names, item_angle_deg, item_scale,
                              item_block = NULL, n, fit_zeta1, fit_zeta2) {
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
    empty <- stats::setNames(rep(NA_real_, d$n_comp), d$components)
    warning(
      "The corrected component standard errors could not be computed (",
      reason, "); they are reported as NA.",
      call. = FALSE
    )
    list(naive = empty, corrected = empty, fiml_ratio = empty, reason = reason)
  }

  # Refused BEFORE cov2cor() runs, which is what makes this reachable at all:
  # cov2cor() of a nonpositive diagonal returns NaN rows rather than erroring,
  # so without this the failure would surface as "indefinite" or as raw NaN
  # instead of as an honest refusal. The sibling surface has carried the same
  # guard since M68 (R/axes_scaled_fit.R); this one did not (RR15 B2).
  #
  # `na.rm = TRUE` is load-bearing, not defensive. Without it a single NA or
  # NaN on the diagonal makes the predicate NA and `if (NA)` ERRORS, which
  # breaks the NA-together contract on an input the pre-M69 code handled: it
  # fell through to solve() -> tryCatch -> na_out("singular"). With na.rm the
  # same input takes that same route again, and the NaN never reaches cov2cor()
  # because this function returns before it. `<= 0` still catches every
  # genuinely nonpositive variance. (M69 review round 1, F1 -- the `is.finite`
  # family recurring: M32, M35, M60.)
  if (any(diag(sigma) <= 0, na.rm = TRUE)) {
    return(na_out("nonpositive_diagonal"))
  }

  raw <- axes_se_pricing(sigma, d, n)
  if (is.character(raw)) return(na_out(raw))
  std <- axes_se_pricing(stats::cov2cor(sigma), d, n)
  if (is.character(std)) return(na_out(std))

  list(
    naive = stats::setNames(raw$naive, d$components),
    corrected = stats::setNames(std$corrected, d$components),
    fiml_ratio = stats::setNames(std$corrected / std$naive, d$components),
    reason = NULL
  )
}
