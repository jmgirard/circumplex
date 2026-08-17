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

# The EM iteration cap, raised far above lavaan's default of 500 (M65-D4). At
# the default, a healthy dataset converges on TOLERANCE and never sees the cap,
# but a thinly-covered item hits it and returns its last iterate -- so clause
# (iv) refused datasets FIML can estimate. Measured on the 24-item probe
# population (N = 300, one item's coverage varied): 40/300 stalls at 500 and
# converges in 0.23 s at 2000; 20/300 stalls at 500 and converges in 0.35 s at
# 50000; 25/300 needs ~50000 and 10.6 s. Healthy 10%-MCAR data takes the same
# 0.1-0.3 s at either cap, because the cap is not what stops it.
#
# The refusal is retained, and matters more with the cap raised, not less: a
# stalled iterate is not a slightly-worse estimate but an unusable one -- at
# 25/300 the cap-10000 iterate's covariance differs from the converged answer
# by 3.93 (measured), on unit-variance items. What changes is that reaching the
# cap now means the EM really is not converging, rather than that it was not
# given room to.
axes_fiml_em_iter_max <- 50000L


# The cap's SPELLING is version-dependent, and getting it wrong is a hard error
# rather than a silently ignored argument: lavaan renamed this option at 0.7-1,
# from a top-level `em.h1.iter.max` to a `max_iter` element of a nested
# `em.h1.args` list, and 0.7 aborts with "unknown argument" on the old name.
# (The nested spelling did not exist before 0.7, so the mapping is one-to-one in
# both directions -- lavOptions() carries exactly one of the two names.)
#
# Detected at RUN time off lavOptions(), never at build time: lavaan is a
# Suggests, and a user upgrades it without reinstalling this package, so a
# choice baked in at install time would be wrong on exactly the machines this
# guard exists for. If a future lavaan carries NEITHER name, no cap is passed
# and lavaan's own default applies -- the EM then stalls sooner on thin data and
# the refusals below fire, which is a conservative failure rather than a wrong
# number.
#
# Acceleration is pinned OFF wherever lavaan offers it (the 0.7 nested
# spelling; 0.6 has no such option and iterates plainly already). lavaan 0.7
# defaults the saturated-stage EM to SQUAREM acceleration, whose convergence
# on thinly-covered cells proved platform-sensitive: the 20/300-coverage cell
# the M65-D4 tests ride converged under acceleration on macOS and Linux but
# stalled at ANY cap on Windows, so FIML refused data it is documented to
# estimate, on one platform only. The 50000 cap and every measurement above
# were made under the plain EM, so the pin restores the calibrated regime
# rather than adding a new one; the cost on healthy data is nil (0.1-0.3 s
# either way, measured above), and at thin cells the plain iterate sits in the
# same EM-tolerance neighborhood as the accelerated one (max moment difference
# 1.9e-3 on the 20/300 cell, measured 2026-08-17).
axes_fiml_em_args <- function(cap = axes_fiml_em_iter_max) {
  opts <- names(lavaan::lavOptions())
  if ("em.h1.args" %in% opts) {
    list(em.h1.args = list(max_iter = cap, acceleration = "none"))
  } else if ("em.h1.iter.max" %in% opts) {
    list(em.h1.iter.max = cap)
  } else {
    list()
  }
}


# Did this warning report the unrestricted-moments EM hitting its cap? Two
# independent substrings, because either alone is fragile. The DIAGNOSIS
# sentence ("the sample moments using EM") is worded identically on both
# generations, but lavaan hard-wraps its messages at getOption("width")
# (lav_msg(): split on whitespace, then prefix the chunk after a break with a
# newline and three spaces), so WHERE the break lands moves with the width in
# force at emission time. A `fixed = TRUE` literal "moments using EM" therefore
# fails at exactly the two gaps separating its three words -- and one of those
# is the break lavaan actually takes at the default width 80, measured on
# 0.6.21 and 0.7.2 alike. Hence a whitespace class at BOTH gaps: making only
# the first tolerant still leaves the `using`/`EM` gap fatal.
# The REMEDY sentence names an `em.h1*` option, which is version-specific:
# lavaan renamed the option once already at 0.7-1, and a further rename of the
# stem would silence this predicate if it were the only clause. Deliberately
# NARROW: the structured-fit call site below muffles lavaan's boundary and
# optimizer warnings too, and matching one of those would turn a
# converged-but-boundary fit into an EM refusal.
axes_fiml_em_stalled <- function(w) {
  msg <- conditionMessage(w)
  grepl("moments[[:space:]]+using[[:space:]]+EM", msg) || grepl("em\\.h1", msg)
}


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
    do.call(lavaan::lavCor, c(
      list(
        dat,
        ordered = character(0),
        missing = "ml",
        output = "fit",
        meanstructure = TRUE
      ),
      axes_fiml_em_args()
    )),
    warning = function(w) {
      if (axes_fiml_em_stalled(w)) {
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


# --- The MAR mechanisms the evidence cells are generated under ----------------
#
# Package-internal rather than test-local, for the same reason axes_simulate()
# is: the seed-pinned harness (devel/m65-fiml-heavy-cells.R) and the suite's
# live re-run must generate the SAME missingness from the same seed, or the
# suite would be re-deriving a different quantity than the stored summary
# records and neither would notice. One definition, two callers.
#
# Both are MAR by construction, which is the property the cells turn on:
# missingness depends only on always-observed values, never on the value that
# goes missing. Verbatim from RR12's header, including the constants.
#
# M1 (moderate, cross-scale anchor): the first scale's items are always
# observed; every other item's cells go missing independently with
# P = plogis(qlogis(.12) + 1.5 * x_anchor), where x_anchor is the respondent's
# first item on scale 1.
axes_mar_m1 <- function(mat, n_items) {
  p <- stats::plogis(stats::qlogis(.12) + 1.5 * mat[, 1])
  for (j in seq.int(n_items + 1L, ncol(mat))) {
    mat[stats::runif(nrow(mat)) < p, j] <- NA
  }
  mat
}

# M2 (harsh, same-scale anchor): the first item of every scale is always
# observed; that scale's remaining items go missing with
# P = plogis(qlogis(.30) + 2.5 * x_first-item-of-that-scale). Selecting on an
# r ~ .53 same-scale correlate is what maximizes the available-case variance
# distortion -- this mechanism exists to make the wrong metric fail loudly,
# not to be realistic.
axes_mar_m2 <- function(mat, n_items) {
  for (s in seq_len(ncol(mat) / n_items) - 1L) {
    anchor <- mat[, n_items * s + 1L]
    p <- stats::plogis(stats::qlogis(.30) + 2.5 * anchor)
    for (j in seq.int(n_items * s + 2L, n_items * s + n_items)) {
      mat[stats::runif(nrow(mat)) < p, j] <- NA
    }
  }
  mat
}

# Per-cell MCAR, the BC10/BC13 mechanism. Trivial, but kept beside its two
# siblings so a reader comparing the three cells reads one file, and so the
# harness and the suite share this draw as well.
axes_mcar <- function(mat, rate) {
  mat[stats::runif(length(mat)) < rate] <- NA
  mat
}


# The jointly-observed count below which the pairwise overlap is reported as
# thin. It is a WARNING and never a refusal (M65-D2): RR12 section 7 binds no
# floor and says outright that any positive constant is arbitrary, so 30 is
# taken as the conventional small-sample floor for a correlation -- a
# convention, not a quantity derived here, and with no inferential meaning. The
# hard refusal is at zero overlap, where the moment is not estimable at all.
axes_fiml_min_overlap <- 30L


# The observed-data geometry, EM-free: which rows survive, how many respondents
# stand behind each item and each item PAIR, and the counts BC8 reports. `mat`
# is the numeric item matrix in item-map order, missing cells as NA.
#
# Split from axes_fiml_moments() because ORDER is load-bearing: every BC7
# refusal that can be read off the missingness pattern alone -- N_used <= p, a
# barely-observed or constant item, a never-jointly-observed pair -- must fire
# BEFORE the EM stage, not after. Handing a degenerate item to EM does not
# produce an informative failure; lavaan fabricates the unidentified moment and
# returns something that looks like an estimate (evidence V-F). So the caller
# screens on this, then estimates.
#
# Rows with NO observed item are dropped here (BC7): they carry no information
# for any moment, and leaving them in would inflate every denominator N_used
# feeds. `keep` is returned rather than the filtered matrix so the caller drops
# once, on its own copy, and every later quantity agrees about which rows exist.
axes_fiml_coverage <- function(mat) {
  obs <- !is.na(mat)
  keep <- rowSums(obs) > 0L
  obs <- obs[keep, , drop = FALSE]
  p <- ncol(mat)

  # Pairwise joint coverage: how many respondents answered BOTH items of a
  # pair. crossprod() on the observed-indicator matrix gives every pair at once;
  # the diagonal is per-ITEM coverage, which is a different quantity, so the
  # minimum is taken over the off-diagonal only.
  co <- crossprod(obs)
  list(
    keep = keep,
    n_used = sum(keep),
    n_dropped = sum(!keep),
    n_complete = sum(rowSums(obs) == p),
    item_n = diag(co),
    pair_n = co,
    min_coverage = if (p > 1L) min(co[upper.tri(co)]) else NA_real_
  )
}


# Saturated-FIML moments and the standardized item matrix. `mat` is the item
# matrix with its all-missing rows ALREADY dropped (axes_fiml_coverage()), so
# nrow(mat) is N_used and the two stages cannot disagree about the denominator.
axes_fiml_moments <- function(mat) {
  n_used <- nrow(mat)
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
  list(z = z, mean = mu, sd = sdv, R = stats::cov2cor(h1$cov))
}
