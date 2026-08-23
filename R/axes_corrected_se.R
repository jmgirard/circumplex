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
# `reason` is NULL unless a failure touched the REPORTED vectors (`corrected`
# and `fiml_ratio`, both functions of cov2cor(Sigma-hat)); when it is set,
# all three vectors are NA under that one reason. A failure confined to the
# raw metric -- the `naive` arm is the one place the raw matrix is inverted
# -- NAs `naive` alone and is carried in `naive_reason` instead, with
# `reason` NULL (M91; RR18 rec 7). The vectors never fall back to each
# other: relabelling the uncorrected number as corrected is the one failure
# a user could not detect. One pricing of the whole sandwich at one matrix.
# Called twice by
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
  # The one definition of the all-NA component vector, shared by the unit
  # refusals below and the decoupled naive-only refusal at the bottom.
  empty <- stats::setNames(rep(NA_real_, d$n_comp), d$components)
  # A unit refusal: something touching the REPORTED vectors failed, so one
  # reason speaks for all three and `naive_reason` is deliberately NULL --
  # never a second label beside it. Every na_out() call sits ahead of the
  # raw arm below, so no determined raw-arm refusal is ever discarded here.
  na_out <- function(reason) {
    warning(
      "The corrected component standard errors could not be computed (",
      reason, "); they are reported as NA.",
      call. = FALSE
    )
    list(naive = empty, corrected = empty, fiml_ratio = empty, reason = reason,
         naive_reason = NULL)
  }

  # Refused BEFORE cov2cor() runs, which is what makes this reachable at all:
  # cov2cor() of a nonpositive diagonal returns NaN rows rather than erroring,
  # so without this the failure would surface as "indefinite" or as raw NaN
  # instead of as an honest refusal. The sibling surface has carried the same
  # guard since M68 (R/axes_scaled_fit.R); this one did not (RR15 B2). Until
  # M89 this door said "nonpositive_diagonal" where the sibling's says
  # "singular": the two surfaces price the same fitted matrix and their reasons
  # are read side by side, so they now share one vocabulary, and the sibling's
  # literal is the one M71's boundary tests pinned.
  #
  # `na.rm = TRUE` is load-bearing, not defensive. Without it a single NA or
  # NaN on the diagonal makes the predicate NA and `if (NA)` ERRORS, which
  # breaks the NA-together contract on an input the pre-M69 code handled. With
  # na.rm the same input is still refused as "singular" -- since M89 by
  # axes_sigma_degenerate()'s finiteness arm below, before any pricing runs.
  # `<= 0` still catches every genuinely nonpositive variance. (M69 review
  # round 1, F1 -- the `is.finite` family recurring: M32, M35, M60.)
  if (any(diag(sigma) <= 0, na.rm = TRUE)) {
    return(na_out("singular"))
  }
  # +Inf fails `<= 0`, so it needs its own door, exactly as in the sibling
  # (M71). Before M89 this surface refused it too, but by accident rather than
  # contract: pricing the raw matrix, solve() of an infinite variance zeroes
  # that row/column of the inverse, and the rank-deficient information matrix
  # fell out as "unidentified" -- a different literal than the sibling prints
  # for the same input. This surface adopts the sibling's literal (M89 AC2).
  if (any(is.infinite(diag(sigma)))) return(na_out("infinite_diagonal"))

  # The stated degeneracy criterion (M89) -- see axes_sigma_degenerate() at the
  # end of this file for the criterion and its rationale. Checked before any
  # pricing so that what refuses a degenerate matrix is a stated contract, not
  # whichever solve() call happens to give up first on this platform's LAPACK.
  # Evaluated on BOTH matrices this helper prices: cov2cor(Sigma-hat) (the
  # corrected arm, and the only matrix anything user-reported depends on) and
  # the raw realigned Sigma-hat (the `naive` arm below inverts it). The two
  # arms no longer refuse as a unit (M91; RR18 rec 7): the cov2cor arm
  # tripping refuses all three vectors under `reason` -- relabelling one arm's
  # number as the other's stays the undetectable failure -- while a failure
  # confined to the raw metric NAs `naive` alone, its literal carried in
  # `naive_reason` with `reason` NULL, because `corrected` and `fiml_ratio`
  # are functions of cov2cor(Sigma-hat) the raw metric's conditioning says
  # nothing about (RR18 measured them invariant to <= 6.4e-16 across eight
  # decades of diagonal inflation).
  #
  # ORDER (M90 AC7, restated under the M91 decoupling): the cov2cor arm is
  # consulted FIRST and short-circuits, so whenever both arms would refuse,
  # the literal reported is the cov2cor arm's -- the arm
  # axes_scaling_factor() also prices. M90's indefinite/ill_conditioned
  # partition is not congruence-invariant (the eigenvalue RATIO moves under
  # cov2cor even though the signs do not), so the two arms can label one
  # matrix differently; this precedence is what keeps the reported `reason`
  # in pointwise literal agreement with the scaling surface (M89's
  # nestedness contract). The raw arm's own label surfaces only in
  # `naive_reason`, and only when the cov2cor arm is clean.
  # The finiteness hoist just below exists for that order: the raw
  # matrix is checked finite before cov2cor() runs on it, because cov2cor()
  # of an NA/NaN diagonal emits its own warning and the M71 contract is
  # exactly one warning per refusal (the same trap the sibling documents at
  # R/axes_scaled_fit.R; finiteness is metric-blind, so hoisting it moves no
  # refusal across the arms' boundary).
  if (!all(is.finite(sigma))) return(na_out("singular"))
  degenerate <- axes_sigma_degenerate(stats::cov2cor(sigma))
  if (!is.null(degenerate)) return(na_out(degenerate))

  # The whole cov2cor arm -- criterion above, pricing here -- resolves before
  # the raw arm runs: a matrix destined for a unit refusal never pays for a
  # raw sandwich whose result would be dropped, and by the time the raw arm
  # can record a refusal no na_out() exit remains to discard it.
  std <- axes_se_pricing(stats::cov2cor(sigma), d, n)
  if (is.character(std)) return(na_out(std))

  # The raw arm, decoupled (M91): a criterion trip or a pricing failure here
  # touches `naive` alone. No warning is emitted for it -- the refused
  # quantity is never user-reported (it exists as the lavaan tie, D-037) and
  # every reported number is present and correct; the refusal is carried in
  # `naive_reason` (surfaced as details$naive_reason; M91-D1) under the same
  # vocabulary every other refusal uses (M91-D2).
  naive_reason <- axes_sigma_degenerate(sigma)
  raw <- NULL
  if (is.null(naive_reason)) {
    raw <- axes_se_pricing(sigma, d, n)
    if (is.character(raw)) {
      naive_reason <- raw
      raw <- NULL
    }
  }

  list(
    naive = if (is.null(raw)) empty else stats::setNames(raw$naive, d$components),
    corrected = stats::setNames(std$corrected, d$components),
    fiml_ratio = stats::setNames(std$corrected / std$naive, d$components),
    reason = NULL,
    naive_reason = naive_reason
  )
}


# The single stated degeneracy criterion for lavaan's fitted covariance matrix
# (M89), shared by its two consumers -- axes_corrected_se() above and
# axes_scaling_factor() in R/axes_scaled_fit.R -- so a matrix too degenerate to
# price is refused by BOTH surfaces with one literal, instead of a user
# receiving NA corrected SEs beside silently scaled fit statistics derived
# from the same matrix.
#
# THE CRITERION: the smallest eigenvalue of the priced matrix, relative to its
# largest, must exceed sqrt(p * eps / tau) (tau below); at or below that the
# matrix is refused. One inequality carries three spectral cases -- a
# negative smallest eigenvalue (lambda_min <= 0 < lambda_max), exact
# singularity (lambda_min = 0), and mere ill-conditioning
# (lambda_max/lambda_min >= sqrt(tau/(p*eps)), about 4.3e4 at p = 24) --
# and since M90 the refusal's LITERAL is decided by depth, not by case:
# "indefinite" only where the negativity is decisive, "ill_conditioned" for
# everything else including roundoff-level negativity; the partition and
# its rationale live at axes_sigma_degenerate() below.
#
# WHICH MATRIX (M89 re-cut, RR18): each consumer prices the matrix it actually
# computes with. Every quantity the scaling surface computes -- and every
# number axes_reliability() reports -- is a function of cov2cor(Sigma-hat)
# alone, so that surface evaluates the criterion on cov2cor(Sigma-hat). The SE
# helper evaluates it on BOTH cov2cor(Sigma-hat) and the raw Sigma-hat: its
# `naive` arm is the one place the raw matrix is inverted (the lavaan tie,
# D-037). A cov2cor-arm trip refuses all three of its vectors under `reason`;
# a raw-arm-only trip NAs `naive` alone, carried in `naive_reason` (M91;
# RR18 rec 7). Under THIS CRITERION the two surfaces' reported refusals
# therefore agree exactly -- whatever the criterion refuses at the scaling
# surface, the SE helper's `reason` names with the same literal, and nothing
# the raw metric alone refuses touches either surface's reported numbers.
# (Each surface keeps its own non-criterion refusals -- the SE helper's
# pricing backstops, the scaling surface's df doors -- which were never part
# of the nestedness contract.) Pricing the raw matrix everywhere, as the first cut did,
# refused pure diagonal rescalings the estimand is exactly invariant under:
# RR18 measured corrected/fiml_ratio invariant to <= 6.4e-16 across eight
# decades of diagonal inflation that move kappa(raw) to 2.1e8. No
# model-statement content is lost in the move: cov2cor() is a congruence, so
# by Sylvester's law of inertia it preserves eigenvalue signs exactly --
# indefiniteness and exact singularity are metric-invariant, and only the
# scale nuisance the reported quantities never depend on is normalized away.
#
# WHY THIS CUTOFF: the corrected branch builds the information matrix
# Delta'V Delta from the priced matrix's INVERSE twice, so its entries carry a
# relative error growing like p * kappa^2 * eps; the floor is where that bound
# reaches the accuracy target. The shipped sqrt(p*eps) floor -- no target at
# all, in these terms -- was a hundred thousand times looser: it accepted the
# committed exemplar B (kappa = 6.65e6 in BOTH metrics, unit diagonal) on which
# the reported corrected SEs were wrong by 3.4% with reason NULL, the package's
# first measured silent wrong number in this subsystem (RR18).
#
# THE TARGET AND THE CEILING (M106, RR19). Two documented quantities stand
# behind one shipped constant. Keeping them apart is the point: M89 defined tau
# as the largest tolerated reported error and then enforced a cap ten times
# looser, so the stated definition and the enforced behaviour disagreed by
# exactly the slack factor.
#
#   delta_star = 1e-4 -- THE ACCURACY TARGET: the largest relative error a
#     reported corrected SE may carry. Derived from the SE's own sampling
#     variability rather than from machine epsilon (RR19 section 1). The
#     corrected SE is a smooth plug-in functional of Sigma-hat, so its relative
#     sampling SD is of order 1/sqrt(2*(n-1)): for (n-1)s^2/sigma^2 ~
#     chi^2_{n-1} the relative SD of s^2 is sqrt(2/(n-1)), which the delta
#     method halves for the square root. Holding the numerical bias to one
#     tenth of that statistical noise -- the conventional "numerical error much
#     smaller than statistical error" margin -- and calibrating at n = 5e5, a
#     decade past any published circumplex sample, gives 0.1/sqrt(2*(n-1)) =
#     1.0e-4. Smaller n only loosens the requirement, which is why the absence
#     of a minimum sample size is the harmless direction and the cormat path's
#     unbounded n is the binding one. No citekey on this repo's shelf carries
#     the sampling-SD result, and RR19 declined to manufacture a citation for a
#     textbook-standard one. Two n-free channels agree: print.circumplex_axes_
#     reliability() formats at 3 decimals (axes_fmt, R/axes_reliability_oop.R),
#     so print resolution is at least 1e-3 relative at the largest printable
#     SE; and a relative SE error delta moves nominal 95% Wald coverage by
#     about 0.23*delta, so 1e-4 shifts coverage by 0.002 points. One decade
#     looser (1e-3) reaches the printed resolution and becomes marginally
#     resolvable; one decade tighter buys nothing any channel can see.
#
#   C = 10 -- THE CALIBRATION CEILING: how far the enforced bound
#     p * kappa(cov2cor(Sigma-hat))^2 * eps may sit below the error it stands
#     for. The oracle's fixture sweep measured attainment ratios 1.27/2.4/3.28
#     across three decades of kappa, drifting up about 1.6x per decade; 10
#     covers that trend with at least 3x headroom at every kappa below the
#     floors this target sets.
#
# tau = delta_star / C is therefore the implementation constant: refuse when
# the bound exceeds tau, and a computed answer's error is capped at
# C*tau = delta_star. Refusal thresholds kappa = sqrt(tau/(p*eps)) are then
# 4.3e4 at p = 24, 7.5e4 at p = 8, and 1.06e5 at p = 4.
#
# WHAT THE BOUND IS AND IS NOT (RR19 section 3). p*kappa^2*eps is a
# conservative envelope, not an error model for this quantity. Measured against
# the exact-rational oracle over model-implied unit-diagonal matrices at
# p = 4, 8 and 9 -- the form every lavaan-fitted Sigma-hat has -- the actual SE
# relative error sits 5 to 8 decades BELOW the bound, attainment ratios at most
# 4e-6. The bound's only measured attainment is the committed fixture
# cairn/reviews/rb18-counterexample-b.rds, which no exported call can produce:
# it is p = 3 with df = 1 while axes_reliability() requires four scales (the
# minimum reachable df is 4), and it sits 25 units off the model manifold at
# its own stated configuration. What drives the error is coupling of near-null
# directions into the COMPONENT ROWS of the information matrix's inverse,
# indexed by df rather than by kappa: measured cval relative error is 3.4e-1
# with a sign flip at df = 1, 1.1e-8 at df = 4, and 1.1e-13 at df = 26. No
# measured regime shows the bound optimistic beyond the fixture's own 3.3x.
# The four-scale minimum is what keeps the reachable set out of that regime --
# lowering it means re-running the exact oracle at the new minimum first.
#
# ONE CONSTANT, NOT SEVERAL (RR19 section 4). tau does not depend on n. An
# n-dependent tau would make refusal a property of the yardstick rather than of
# the refused matrix -- the identical matrix computing at n = 200 and refusing
# at n = 20000, and gameable on the cormat path where n is typed by the user --
# while buying only n^(1/4) of threshold movement, since the floor moves as
# sqrt(tau). p already appears inside the bound, where it belongs.
#
# WHY THE LIMB EXISTS AT ALL (RR19 section 5). Removal was weighed on this
# mechanism's second escalation and rejected narrowly: past the floor the
# package has no shipped means of certifying the number to delta_star (IP3),
# and the only a-priori error estimate a replacement caution could carry is
# this same bound, which overstates the actual error by 5 to 8 decades in every
# geometry users occupy -- it would cry "up to 3% error" over numbers accurate
# to 1e-13. The reopening evidence is recorded in the D-entry superseding
# D-044. Changing delta_star or C is an escalation (RB, no-oracle), never a
# silent edit.
axes_degeneracy_delta_star <- 1e-4
axes_degeneracy_calibration_ceiling <- 10
axes_degeneracy_tau <-
  axes_degeneracy_delta_star / axes_degeneracy_calibration_ceiling

# Returns NULL (priceable), "singular" (non-finite entries: the literal the
# NA/NaN-diagonal route has carried since before M69, now reached here rather
# than in solve()), "indefinite", or "ill_conditioned". Callers refuse
# nonpositive and +Inf diagonals at their own doors first, so lambda_max > 0
# whenever the eigendecomposition runs.
#
# WITHIN the refusal region, which word (M90): "indefinite" is a statement
# about the user's MODEL -- the model-implied matrix has a genuinely negative
# direction -- so it is claimed only where the negativity exceeds what the
# fit's own noise can produce: lambda_min < -lambda_max * sqrt(p * eps).
# RATIONALE for that band (RR18 BC5's constant; M90 AC2 demands the rationale
# stated here): the priced matrix's entries are not exact -- they are
# optimizer output, carrying entrywise relative
# error no smaller than order sqrt(eps) (an iterative fit stopped on a
# relative objective tolerance `tol` leaves the implied moments with errors
# of order sqrt(tol) near a quadratic optimum, and sqrt(tol) >= sqrt(eps) for
# any achievable tolerance). A symmetric entrywise perturbation of relative
# size sqrt(eps) has spectral norm ~ sqrt(p)*sqrt(eps)*lambda_max under
# incoherent signs, and Weyl's inequality moves every eigenvalue by at most
# that norm -- so a computed eigenvalue within lambda_max*sqrt(p*eps) of zero
# is indistinguishable from a nonnegative one perturbed by the fit's own
# error, and is refused as "ill_conditioned" (a numerical caution), never
# reported as a defect of the user's model. The claim is one-directional:
# within the band, indefiniteness cannot be asserted; beyond it, "indefinite"
# is the best available reading of a decisively negative eigenvalue. The
# argument rests on the optimizer's error alone, not on cov2cor() rounding,
# so it covers every matrix this criterion is evaluated on: the two
# correlation-metric call sites AND the SE helper's raw arm, where since M91
# a trip labels only `naive_reason` (M91-D2, closing M90 review F11). Changing
# this constant is an escalation (RB, no-oracle), never a silent edit.
axes_sigma_degenerate <- function(sigma) {
  if (!all(is.finite(sigma))) return("singular")
  ev <- eigen(sigma, symmetric = TRUE, only.values = TRUE)$values
  p <- nrow(sigma)
  floor_ <- sqrt(p * .Machine$double.eps / axes_degeneracy_tau)
  if (ev[p] <= ev[1] * floor_) {
    if (ev[p] < -ev[1] * sqrt(p * .Machine$double.eps)) return("indefinite")
    return("ill_conditioned")
  }
  NULL
}


# The one expression both fitted-matrix consumers are fed from (M89): the
# fitted covariance matrix exactly as lavaan reports it, dimnames and all. A
# named seam rather than an inline `lavaan::fitted(fit)$cov` at each call site
# in axes_reliability(), so the assembly-level tests can inject a constructed
# degenerate matrix: no converged fit is known to reach the degenerate regime,
# and the criterion's assembly behavior still needs exercising through
# axes_reliability() itself.
axes_fitted_cov <- function(fit) lavaan::fitted(fit)$cov
