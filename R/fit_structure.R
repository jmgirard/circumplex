# Shared infrastructure for the circumplex structure tests of Acton & Revelle
# (2004). Every one of their exploratory criteria (Fisher, Gap, Variance, and
# Rotation tests) operates on the first two *unrotated principal-axis* factors
# of the scales' correlation matrix (A&R p. 13), so the extraction lives here
# once rather than in each test.

# Principal-axis factor extraction of the first two factors --------------------
# Iterated principal-axis factoring (PAF): communalities are initialized from
# the squared multiple correlations (SMCs), then refined by repeatedly placing
# the current communalities on the diagonal of the correlation matrix, taking
# the leading two eigenpairs as loadings, and updating the communalities from
# those loadings until they stabilize. No rotation is applied -- the A&R
# criteria are defined on the unrotated solution. Replaces the drafts' single
# `psych::fa(nfactors = 2, rotate = "none", fm = "pa")` call so `psych` is only
# a Suggests-level test oracle.
paf2 <- function(r, max_iter = 100L, tol = 1e-4) {
  p <- ncol(r)
  # An undefined correlation matrix (a zero-variance scale, or a pair of scales
  # never jointly observed, makes cor() return NA) has no factor solution:
  # eigen() would abort with a cryptic "infinite or missing values in 'x'".
  # Return NA loadings so every criterion returns NA_real_ (the degeneracy
  # policy), matching how structure_randall() already handles the same input.
  if (anyNA(r)) {
    return(matrix(
      NA_real_, nrow = p, ncol = 2,
      dimnames = list(rownames(r), c("PA1", "PA2"))
    ))
  }
  # SMC starting communalities: 1 - 1/diag(R^-1). A singular R (e.g. deviation-
  # scored scales, whose correlation matrix is rank-deficient) has no inverse;
  # fall back to a unit (identity) start, then let ridge repair supply a proper
  # matrix upstream.
  smc <- tryCatch(1 - 1 / diag(solve(r)), error = function(e) rep(1, p))
  smc[smc < 0] <- 0
  smc[smc > 1] <- 1
  comm <- smc

  loadings <- matrix(0, nrow = p, ncol = 2)
  for (i in seq_len(max_iter)) {
    reduced <- r
    diag(reduced) <- comm
    e <- eigen(reduced, symmetric = TRUE)
    vals <- e$values[1:2]
    # A non-positive-definite reduced matrix can yield negative leading
    # eigenvalues; clip to zero so sqrt() is defined (a zero-variance factor
    # simply contributes nothing to the loadings).
    vals[vals < 0] <- 0
    loadings <- e$vectors[, 1:2, drop = FALSE] %*% diag(sqrt(vals), 2, 2)
    new_comm <- rowSums(loadings^2)
    # Cap Heywood cases at the theoretical communality maximum of one.
    new_comm[new_comm > 1] <- 1
    if (max(abs(new_comm - comm)) < tol) {
      comm <- new_comm
      break
    }
    comm <- new_comm
  }

  # Factor orientation is arbitrary; sign each factor so its loadings sum to a
  # positive number (the psych::fa convention) for a reproducible solution.
  signs <- sign(colSums(loadings))
  signs[signs == 0] <- 1
  loadings <- loadings %*% diag(signs, 2, 2)
  colnames(loadings) <- c("PA1", "PA2")
  rownames(loadings) <- rownames(r)
  loadings
}

# Two-factor loadings for a set of circumplex scales ---------------------------
# Selects the scales from `data`, forms their correlation matrix, optionally
# repairs a non-positive-definite matrix with a ridge, and returns the first two
# unrotated principal-axis factors. Extraction is *always* principal axis: the
# A&R thresholds were calibrated under PA, so (unlike the drafts) ridge does not
# switch the estimator to maximum likelihood -- it is an orthogonal correlation-
# matrix repair only.
# Criterion statistics ---------------------------------------------------------
# The four effective Acton & Revelle (2004) criteria, each computed from a
# p x 2 loadings matrix (the first two unrotated principal-axis factors from
# `structure_loadings()`). Definitions follow the adjudicated readings in
# devel/ar2004-transcription.md -- A&R's equations, with the Fisher scale
# settled empirically by the T2 sanity gate (see structure_fisher below).
# Interpretive cutoffs are *not* applied here; they are nv-dependent and live
# in `structure_cutoffs`.
#
# Degenerate-loadings policy (one epsilon, DEGEN_TOL): a variable whose
# communality is negligible has no defined angle (Gap) and no defined
# normalized loading (VT), so those return NA_real_ if *any* variable is
# degenerate. Fisher and RT need no per-variable angle and stay defined
# unless the *whole solution* is degenerate (all communalities negligible for
# Fisher; a zero rotation profile for RT and VT), where a coefficient of
# variation would be 0/0 or a ratio of floating-point noise -- also NA_real_,
# never NaN.
DEGEN_TOL <- 1e-12

# Fisher Test of equal axes: the coefficient of variation of the variables'
# vector lengths sqrt(h2). A&R's Eq. 6 as printed uses the communalities h2
# themselves, but their prose describes vector lengths, and the two differ by
# about a factor of two in CV -- only one can carry the published .10/.15
# cutoffs. The T2 sanity gate (data-raw/structure-test-cutoffs.R) settled
# this empirically: reproducing A&R's design yields the published cutoffs for
# CV(sqrt(h2)) and roughly doubled values for CV(h2), so CIRC_STRUC evidently
# computed vector lengths (as does psych::circ.tests). Rotation-invariant, so
# the arbitrary PA orientation is immaterial.
structure_fisher <- function(loadings) {
  if (anyNA(loadings)) {
    return(NA_real_)
  }
  h2 <- rowSums(loadings^2)
  if (all(h2 < DEGEN_TOL)) {
    return(NA_real_)
  }
  h <- sqrt(h2)
  stats::sd(h) / mean(h)
}

# Gap Test of equal spacing (A&R Eq. 2): the variance of the angular gaps
# between adjacent variables, *including the wrap-around gap* between the last
# and first variable (2*pi + theta_1 - theta_nv), which is part of A&R's
# definition. With the wrap gap the gaps always sum to 2*pi; without it (the
# draft/psych bug) the statistic certifies a quarter-circle as perfectly
# spaced. Angles are recovered with atan2, which is exact at 180 degrees where
# sign(0)*acos() collapses to 0. Radians^2, on the A&R cutoff scale.
structure_gap <- function(loadings) {
  if (anyNA(loadings)) {
    return(NA_real_)
  }
  h2 <- rowSums(loadings^2)
  if (any(h2 < DEGEN_TOL)) {
    return(NA_real_)
  }
  theta <- sort(atan2(loadings[, 2], loadings[, 1]) %% (2 * pi))
  gaps <- c(diff(theta), 2 * pi + theta[1] - theta[length(theta)])
  stats::var(gaps)
}

# Rotate a p x 2 loadings matrix by theta radians.
rotate_loadings <- function(loadings, theta) {
  loadings %*% cbind(c(cos(theta), sin(theta)), c(-sin(theta), cos(theta)))
}

# Variance Test 2 of interstitiality (A&R Eq. 8): at each rotation theta, take
# the variance across variables of Y = (squared factor-1 loading) / (own
# communality); the statistic is the CV of that variance over rotations. Y is
# cos^2 of the variable's angle to the rotated axis, whose cross-variable
# variance has period 180 degrees in theta, so the default grid spans one full
# period (0-175 by 5 degrees); this makes the statistic exactly invariant to
# the arbitrary orientation of the unrotated solution (A&R leave their grid
# range unstated). The default grid is part of the statistic's definition --
# the `structure_cutoffs` calibration assumes it; non-default grids exist only
# for the calibration script's provenance diagnostics.
structure_vt <- function(loadings, grid_deg = seq(0, 175, by = 5)) {
  if (anyNA(loadings)) {
    return(NA_real_)
  }
  h2 <- rowSums(loadings^2)
  if (any(h2 < DEGEN_TOL)) {
    return(NA_real_)
  }
  x <- vapply(grid_deg * pi / 180, function(theta) {
    rl <- rotate_loadings(loadings, theta)
    stats::var(rl[, 1]^2 / h2)
  }, numeric(1))
  # Constant Y across variables (e.g. only two, or all collinear scales)
  # makes every per-rotation variance zero: 0/0, not a defined CV.
  if (mean(x) < DEGEN_TOL) {
    return(NA_real_)
  }
  stats::sd(x) / mean(x)
}

# Rotation Test of interstitiality (A&R Eq. 9): at each rotation theta, sum
# across variables the variance across the two factors of the squared
# loadings (a quartimax-like criterion); the statistic is the CV of that sum
# over rotations. The summand has period 90 degrees in theta, so the default
# grid spans one full period (0-85 by 5 degrees), again giving exact
# orientation invariance; the same definition-and-calibration caveat as
# structure_vt applies to the grid.
structure_rt <- function(loadings, grid_deg = seq(0, 85, by = 5)) {
  if (anyNA(loadings)) {
    return(NA_real_)
  }
  x <- vapply(grid_deg * pi / 180, function(theta) {
    rl2 <- rotate_loadings(loadings, theta)^2
    sum((rl2[, 1] - rl2[, 2])^2 / 2)
  }, numeric(1))
  # A zero rotation profile (all communalities negligible) is 0/0.
  if (mean(x) < DEGEN_TOL) {
    return(NA_real_)
  }
  stats::sd(x) / mean(x)
}

structure_loadings <- function(data, scales, ridge = 0) {
  stopifnot(is_var(scales))
  stopifnot(length(scales) >= 2)
  stopifnot(is_num(ridge, n = 1), ridge >= 0)

  mat <- as.matrix(data[scales])
  r <- stats::cor(mat, use = "pairwise.complete.obs")

  if (ridge > 0) {
    # Add the ridge to the diagonal of the *correlation matrix* (not the data,
    # as the buggy draft did) and rescale back to a unit diagonal. This lifts
    # the smallest eigenvalue by `ridge` before rescaling, restoring positive
    # definiteness while keeping a correlation matrix.
    diag(r) <- diag(r) + ridge
    r <- stats::cov2cor(r)
  }

  paf2(r)
}

# Interpretive cutoffs, keyed by number of scales -------------------------------
# Acton & Revelle's published cutoffs were calibrated at nv = 64/128 variables
# and do not transfer to 8 octant scales (their p. 18 documents a substantial
# nv effect on the Gap Test; e.g. the raw-scored Gap "almost certainly"
# cutoff moves from .01 at nv = 64/128 to .35 at nv = 8). These constants were
# re-derived under the A&R generating model on exactly the criterion
# statistics above, by data-raw/structure-test-cutoffs.R (seed 20260707,
# standardized-uniqueness reading, 2026-07-07; derivation record in
# data-raw/structure-test-cutoffs.rds, source transcription in
# devel/ar2004-transcription.md). The published nv = 64/128 record was
# reproduced as a sanity gate first (14/17 one-sided claims; three left-tail
# limits documented in the script).
#
# The outer key is the number of scales the cutoffs were calibrated for --
# cutoffs are valid ONLY at their calibrated nv, so consumers must look up
# structure_cutoffs[[as.character(nrow(loadings))]] and treat NULL as "no
# calibrated cutoffs; print none" (the Gap nv effect above is exactly the
# error this guards against). Only nv = 8 is calibrated so far; the
# derivation script's design is nv-generic if more are needed (e.g. 4- or
# 16-scale instruments).
#
# Inner keys: declared scoring ("raw" vs "deviation" = row-mean centered,
# what ipsatize() does) because A&R's cutoffs differ by scoring. Semantics
# follow A&R's own likelihood phrasing, pooled over their general-factor,
# axes/structure, and sample-size conditions: below `almost`, essentially no
# competing structure occurred (1st percentile); below `thrice`/`twice`, the
# named structure was at least 3x/2x as likely as its competitor. These are
# heuristic classification cutoffs read off simulated distributions -- never
# describe them as significance tests. Fisher certifies equal axes; gap, vt,
# and rt certify interstitiality vs simple structure.
structure_cutoffs <- list(
  "8" = list(
    fisher = list(
      raw = c(almost = 0.10, thrice = 0.13, twice = 0.15),
      deviation = c(almost = 0.07, thrice = 0.12, twice = 0.15)
    ),
    gap = list(
      raw = c(almost = 0.35, thrice = 0.51, twice = 0.55),
      deviation = c(almost = 0.15, thrice = 0.40, twice = 0.46)
    ),
    vt = list(
      raw = c(almost = 0.12, thrice = 0.33, twice = 0.37),
      deviation = c(almost = 0.19, thrice = 0.59, twice = 0.64)
    ),
    rt = list(
      raw = c(almost = 0.13, thrice = 0.30, twice = 0.35),
      deviation = c(almost = 0.32, thrice = 0.64, twice = 0.67)
    )
  )
)

# Classify a criterion statistic against A&R's cutoffs ------------------------
# Shared interpretation layer for all four tests (Fisher/Gap/VT/RT). Selects
# the cutoff triple for the given test, number of scales (`nv`), and *declared*
# scoring, then returns the strongest likelihood claim the statistic supports.
# A&R's cutoffs are ordered almost < thrice < twice, so a statistic below
# `almost` almost certainly matches the criterion, below `thrice` the criterion
# is at least three times as likely as its competitor, below `twice` at least
# twice as likely, and otherwise the criterion is not clearly supported
# ("weak"). The category is a bare code; the phrasing (equal axes vs
# interstitiality) is the caller's, since it differs by test.
#
# Two guards implement the "never apply the wrong cutoffs" rule from the method
# review (devel/fit-drafts-method-review.md): if no cutoffs are calibrated at
# this `nv` (only 8 is, so far) the category is NA -- A&R's substantial nv
# effect means the published nv = 64/128 cutoffs must not be reused at another
# scale count. And a degenerate (NA) statistic stays NA rather than being forced
# into a bin. The cutoffs, when they exist, are always returned so the caller
# can report them even when the class is undefined.
structure_interpret <- function(stat, test, nv, scoring) {
  test <- match.arg(test, c("fisher", "gap", "vt", "rt"))
  scoring <- match.arg(scoring, c("raw", "deviation"))

  cuts <- structure_cutoffs[[as.character(nv)]][[test]][[scoring]]
  if (is.null(cuts) || is.na(stat)) {
    return(list(cutoffs = cuts, category = NA_character_))
  }

  category <- if (stat < cuts[["almost"]]) {
    "almost"
  } else if (stat < cuts[["thrice"]]) {
    "thrice"
  } else if (stat < cuts[["twice"]]) {
    "twice"
  } else {
    "weak"
  }
  list(cutoffs = cuts, category = category)
}

#' Fisher Test of equal axes (Acton & Revelle, 2004)
#'
#' Internal wrapper tying the Fisher criterion statistic to its scoring-keyed,
#' number-of-scales-specific interpretation. Not yet exported -- the user-facing
#' fit-statistics API (a single typed entry point with print/summary/plot) is a
#' later task; this returns a plain list the API will consume.
#'
#' The Fisher Test of equal axes asks whether the circumplex variables have
#' comparable communalities (vector lengths) rather than one axis dominating.
#' The statistic is the coefficient of variation of the variables' vector
#' lengths sqrt(h2). Acton & Revelle's (2004) Eq. 6 as printed uses the
#' communalities h2 themselves, but their prose describes vector lengths; the
#' two differ by roughly a factor of two in CV, and only one scale can carry
#' their published cutoffs. The T2 re-derivation (data-raw/structure-test-
#' cutoffs.R) reproduced A&R's own simulation and found the published cutoffs
#' attach to the vector-length scale, so `structure_fisher()` and this wrapper
#' use CV(sqrt(h2)) (see devel/ar2004-transcription.md, "Empirical
#' adjudications"). The criterion is invariant to rotation of the factor pair,
#' so the arbitrary principal-axis orientation is immaterial.
#'
#' Cutoffs are keyed to the *declared* `scoring`: A&R report different cutoffs
#' for raw and deviation (row-mean-centered, i.e. [ipsatize()]d) data, and the
#' test has the most power without a large general factor, which deviation
#' scoring approximates. This function does not transform the data -- the caller
#' declares which scoring the passed scales are already on. Cutoffs are also
#' nv-specific (A&R p. 18); only nv = 8 is calibrated, and any other scale count
#' returns an undefined category (see `structure_cutoffs`).
#'
#' @param data A data frame containing the circumplex scales.
#' @param scales Variable names or column numbers of the circumplex scales.
#' @param scoring Declared scoring of `scales`, `"raw"` or `"deviation"`; picks
#'   the matching cutoffs (default `"raw"`).
#' @param ridge Non-negative ridge added to the correlation matrix diagonal to
#'   repair a non-positive-definite matrix (e.g. deviation-scored scales, whose
#'   correlation matrix is singular); default `0`. See `structure_loadings()`.
#' @return A list with the criterion `statistic`, the `test` name, declared
#'   `scoring`, number of scales `nv`, the `cutoffs` used (or `NULL` at an
#'   uncalibrated nv), the interpretation `category`, and the `loadings`.
#' @references Acton, G. S., & Revelle, W. (2004). Evaluation of ten
#'   psychometric criteria for circumplex structure. \emph{Methods of
#'   Psychological Research Online}, 9(1), 1-27.
#' @noRd
structure_fisher_test <- function(data, scales, scoring = "raw", ridge = 0) {
  scoring <- match.arg(scoring, c("raw", "deviation"))
  loadings <- structure_loadings(data, scales, ridge = ridge)
  statistic <- structure_fisher(loadings)
  nv <- nrow(loadings)
  interp <- structure_interpret(statistic, "fisher", nv, scoring)
  list(
    test = "fisher",
    statistic = statistic,
    scoring = scoring,
    nv = nv,
    cutoffs = interp$cutoffs,
    category = interp$category,
    loadings = loadings
  )
}

#' Gap Test of equal spacing (Acton & Revelle, 2004)
#'
#' Internal wrapper tying the Gap criterion statistic to its scoring-keyed,
#' number-of-scales-specific interpretation. Not yet exported -- the user-facing
#' fit-statistics API is a later task; this returns a plain list the API will
#' consume, mirroring `structure_fisher_test()`.
#'
#' The Gap Test of equal *spacing* asks whether the circumplex variables are
#' evenly distributed around the circle, as opposed to clustering into simple
#' structure. It detects interstitiality, **not** equal axes -- Acton & Revelle
#' (2004, p. 17) note it is insensitive to unequal axes, and the draft that
#' preceded this code carried a copy-pasted "equal axes" description that is
#' corrected here. The statistic is the variance of the angular gaps between
#' angularly adjacent variables, *including the wrap-around gap* across the
#' 0/360 degree branch cut (A&R Eq. 2); omitting that gap (the draft/psych bug,
#' fixed in `structure_gap()`) deflates the variance for simple structure,
#' where the wrap-around gap is often the largest, and biases the test toward
#' declaring circumplexity. Gaps are invariant to rotation of the factor pair,
#' so the arbitrary principal-axis orientation is immaterial.
#'
#' Cutoffs are keyed to the *declared* `scoring` (A&R report different cutoffs
#' for raw and deviation, i.e. [ipsatize()]d, data) and are nv-specific: A&R
#' found a substantial number-of-scales effect on the Gap Test (p. 18; the
#' raw "almost certainly" cutoff moves from .01 at nv = 64/128 to .35 at
#' nv = 8), so their published cutoffs must not be reused at another scale
#' count. Only nv = 8 is calibrated (see `structure_cutoffs`); any other scale
#' count returns an undefined category. This function does not transform the
#' data -- the caller declares which scoring the passed scales are already on.
#'
#' @param data A data frame containing the circumplex scales.
#' @param scales Variable names or column numbers of the circumplex scales.
#' @param scoring Declared scoring of `scales`, `"raw"` or `"deviation"`; picks
#'   the matching cutoffs (default `"raw"`).
#' @param ridge Non-negative ridge added to the correlation matrix diagonal to
#'   repair a non-positive-definite matrix; default `0`. See
#'   `structure_loadings()`.
#' @return A list with the criterion `statistic` (gap variance in radians^2),
#'   the `test` name, declared `scoring`, number of scales `nv`, the `cutoffs`
#'   used (or `NULL` at an uncalibrated nv), the interpretation `category`, and
#'   the `loadings`.
#' @references Acton, G. S., & Revelle, W. (2004). Evaluation of ten
#'   psychometric criteria for circumplex structure. \emph{Methods of
#'   Psychological Research Online}, 9(1), 1-27.
#' @noRd
structure_gap_test <- function(data, scales, scoring = "raw", ridge = 0) {
  scoring <- match.arg(scoring, c("raw", "deviation"))
  loadings <- structure_loadings(data, scales, ridge = ridge)
  statistic <- structure_gap(loadings)
  nv <- nrow(loadings)
  interp <- structure_interpret(statistic, "gap", nv, scoring)
  list(
    test = "gap",
    statistic = statistic,
    scoring = scoring,
    nv = nv,
    cutoffs = interp$cutoffs,
    category = interp$category,
    loadings = loadings
  )
}

#' Variance Test of interstitiality, VT2 (Acton & Revelle, 2004)
#'
#' Internal wrapper tying the VT2 criterion statistic to its scoring-keyed,
#' number-of-scales-specific interpretation. Not yet exported -- the user-facing
#' fit-statistics API is a later task; this returns a plain list the API will
#' consume, mirroring `structure_fisher_test()`.
#'
#' The Variance Test of interstitiality (VT2) asks whether the variables are
#' evenly spread in angle rather than clustered on a few axes. For each rotation
#' theta it takes the across-variable variance of the squared factor-1 loading
#' normalized by the variable's own communality (geometrically cos^2 of the
#' variable's angle to the rotated axis); the statistic is the coefficient of
#' variation of that variance over rotations (A&R Eq. 8). This is the
#' *effective* VT2: the draft computed the raw (unsquared) loading normalized by
#' a rotation-invariant scalar total, which is A&R's ineffective VT1 with a
#' no-op normalization -- see `structure_vt()`. The cross-variable variance has
#' period 180 degrees in theta, so the default grid spans a full period (0-175
#' by 5), making the statistic exactly invariant to the arbitrary
#' principal-axis orientation.
#'
#' Cutoffs are keyed to the *declared* `scoring` (A&R report different cutoffs
#' for raw and deviation, i.e. [ipsatize()]d, data) and are nv-specific; only
#' nv = 8 is calibrated (see `structure_cutoffs`), and any other scale count
#' returns an undefined category. A&R strongly recommend deviation scoring for
#' VT2 in every case, since a large general factor can make it mislabel simple
#' structure. This function does not transform the data -- the caller declares
#' which scoring the passed scales are already on.
#'
#' @param data A data frame containing the circumplex scales.
#' @param scales Variable names or column numbers of the circumplex scales.
#' @param scoring Declared scoring of `scales`, `"raw"` or `"deviation"`; picks
#'   the matching cutoffs (default `"raw"`).
#' @param ridge Non-negative ridge added to the correlation matrix diagonal to
#'   repair a non-positive-definite matrix; default `0`. See
#'   `structure_loadings()`.
#' @return A list with the criterion `statistic`, the `test` name, declared
#'   `scoring`, number of scales `nv`, the `cutoffs` used (or `NULL` at an
#'   uncalibrated nv), the interpretation `category`, and the `loadings`.
#' @references Acton, G. S., & Revelle, W. (2004). Evaluation of ten
#'   psychometric criteria for circumplex structure. \emph{Methods of
#'   Psychological Research Online}, 9(1), 1-27.
#' @noRd
structure_vt_test <- function(data, scales, scoring = "raw", ridge = 0) {
  scoring <- match.arg(scoring, c("raw", "deviation"))
  loadings <- structure_loadings(data, scales, ridge = ridge)
  statistic <- structure_vt(loadings)
  nv <- nrow(loadings)
  interp <- structure_interpret(statistic, "vt", nv, scoring)
  list(
    test = "vt",
    statistic = statistic,
    scoring = scoring,
    nv = nv,
    cutoffs = interp$cutoffs,
    category = interp$category,
    loadings = loadings
  )
}

#' Rotation Test of interstitiality (Acton & Revelle, 2004)
#'
#' Internal wrapper tying the Rotation Test statistic to its scoring-keyed,
#' number-of-scales-specific interpretation. Not yet exported -- the user-facing
#' fit-statistics API is a later task; this returns a plain list the API will
#' consume, mirroring `structure_fisher_test()`.
#'
#' The Rotation Test of interstitiality asks whether a quartimax-like simple-
#' structure criterion is indifferent to rotation, as it is for a true
#' circumplex. For each rotation theta it sums across variables the variance
#' across the two factors of the squared loadings; the statistic is the
#' coefficient of variation of that sum over rotations (A&R Eq. 9). The draft's
#' `criterion[0] <- ...` was a silent no-op that dropped the 0-degree rotation
#' and left a spurious zero in the last slot, badly inflating the CV; the
#' summand has period 90 degrees, so the default grid spans a full period (0-85
#' by 5), fixing the indexing and making the statistic orientation-invariant --
#' see `structure_rt()`.
#'
#' Cutoffs are keyed to the *declared* `scoring` (A&R report different cutoffs
#' for raw and deviation, i.e. [ipsatize()]d, data) and are nv-specific; only
#' nv = 8 is calibrated (see `structure_cutoffs`), and any other scale count
#' returns an undefined category. This function does not transform the data --
#' the caller declares which scoring the passed scales are already on.
#'
#' @param data A data frame containing the circumplex scales.
#' @param scales Variable names or column numbers of the circumplex scales.
#' @param scoring Declared scoring of `scales`, `"raw"` or `"deviation"`; picks
#'   the matching cutoffs (default `"raw"`).
#' @param ridge Non-negative ridge added to the correlation matrix diagonal to
#'   repair a non-positive-definite matrix; default `0`. See
#'   `structure_loadings()`.
#' @return A list with the criterion `statistic`, the `test` name, declared
#'   `scoring`, number of scales `nv`, the `cutoffs` used (or `NULL` at an
#'   uncalibrated nv), the interpretation `category`, and the `loadings`.
#' @references Acton, G. S., & Revelle, W. (2004). Evaluation of ten
#'   psychometric criteria for circumplex structure. \emph{Methods of
#'   Psychological Research Online}, 9(1), 1-27.
#' @noRd
structure_rt_test <- function(data, scales, scoring = "raw", ridge = 0) {
  scoring <- match.arg(scoring, c("raw", "deviation"))
  loadings <- structure_loadings(data, scales, ridge = ridge)
  statistic <- structure_rt(loadings)
  nv <- nrow(loadings)
  interp <- structure_interpret(statistic, "rt", nv, scoring)
  list(
    test = "rt",
    statistic = statistic,
    scoring = scoring,
    nv = nv,
    cutoffs = interp$cutoffs,
    category = interp$category,
    loadings = loadings
  )
}

# RANDALL correspondence index + randomization test ----------------------------
# Hubert & Arabie's (1987) correspondence index for a hypothesized circular
# order, as popularized by Tracey's (1997) RANDALL, with the actual
# randomization inference the draft lacked (it bootstrapped one simulated MVN
# dataset and produced no p-value; method-review S5). Unlike the four A&R
# criteria above, RANDALL needs no simulated cutoffs: its null distribution is
# available by construction, which is why A&R excluded it from their
# simulation (their footnote 3). It operates on the correlations directly --
# no factor extraction, no scoring-keyed cutoffs.

# Order predictions for nv variables in hypothesized circular order: each pair
# of lower-triangle slots (a, b) with circular distance d(a) < d(b) predicts
# r[a] > r[b]. Returned as parallel index vectors into the lower-triangle
# correlation vector (column-major, matching r[lower.tri(r)]).
randall_predictions <- function(nv) {
  pos <- seq_len(nv)
  d <- outer(pos, pos, function(i, j) pmin(abs(i - j), nv - abs(i - j)))
  dv <- d[lower.tri(d)]
  idx <- which(outer(dv, dv, "<"), arr.ind = TRUE)
  list(ia = idx[, 1], ib = idx[, 2])
}

# The correspondence index of a correlation matrix whose variables are in the
# hypothesized circular order: (agreements - violations) / predictions, i.e.
# 2 * P(agree) - 1. A tie counts as a violation (the draft's convention,
# retained because the acceptance criterion pins the index to the draft;
# measure-zero for continuous data, but it makes an all-tied matrix score -1).
# No predictions (nv < 4) or NA correlations return NA_real_, never NaN,
# matching the degeneracy policy above.
structure_randall <- function(r, pred = randall_predictions(ncol(r))) {
  vals <- r[lower.tri(r)]
  if (length(pred$ia) == 0 || anyNA(vals)) {
    return(NA_real_)
  }
  2 * mean(vals[pred$ia] > vals[pred$ib]) - 1
}

# All permutations of x, one per row (base R; used only for nv <= 9, i.e. at
# most 8! = 40320 rows).
all_perms <- function(x) {
  n <- length(x)
  if (n == 1L) {
    return(matrix(x, 1L, 1L))
  }
  do.call(rbind, lapply(seq_len(n), function(k) cbind(x[k], all_perms(x[-k]))))
}

#' RANDALL randomization test of hypothesized circular order
#'
#' Internal wrapper computing the Hubert & Arabie (1987) correspondence index
#' for the hypothesized circular order and its randomization-test p-value
#' (Tracey, 1997, RANDALL). Not yet exported -- the user-facing fit-statistics
#' API is a later task; this returns a plain list the API will consume.
#'
#' The order of `scales` *is* the hypothesis: the index counts, over every
#' pair of variable pairs with unequal circular distance, how often the
#' correlation of the closer pair exceeds that of the farther pair, scaled to
#' \[-1, 1\] (ties count as violations; measure-zero for continuous data). The
#' null hypothesis is that the assignment of variables to circular positions
#' is random; the p-value is the proportion of relabelings whose index reaches
#' the observed one.
#'
#' With `n_perm = NULL` (default) and nv <= 9 the null distribution is
#' enumerated *exactly*: the index is invariant under rotations of the
#' positions (circular distances are unchanged), so enumerating the (nv-1)!
#' relabelings with variable 1 held at position 1 covers all nv! relabelings
#' with uniform multiplicity. The identity relabeling is included, so the
#' exact p is always positive; dihedral relabelings preserve the index for any
#' matrix, so the exact p is never below 2/(nv-1)!. This path is deterministic
#' and consumes no RNG. For nv > 9 exact enumeration is infeasible and
#' `n_perm` must be supplied: `n_perm` random relabelings are drawn from the
#' global RNG stream (set a seed with [set.seed()] beforehand; per the
#' package's RNG contract there is no seed argument) and the p-value uses the
#' add-one convention (1 + #\{index* >= index\}) / (n_perm + 1).
#'
#' @param data A data frame containing the circumplex scales.
#' @param scales Variable names or column numbers of the circumplex scales,
#'   **in the hypothesized circular order**. At least 4 (fewer have no
#'   unequal-distance pairs, hence no order predictions).
#' @param n_perm `NULL` for the exact test (nv <= 9), or a single positive
#'   whole number of Monte Carlo relabelings.
#' @return A list with the correspondence-index `statistic`, the `test` name,
#'   number of scales `nv`, the `p_value`, the `method` (`"exact"` or
#'   `"monte carlo"`), and `n_perm` (relabelings evaluated). All-NA outputs
#'   (never NaN) if the correlations are incomplete.
#' @references Hubert, L., & Arabie, P. (1987). Evaluating order hypotheses
#'   within proximity matrices. \emph{Psychological Bulletin}, 102(1),
#'   172-178. Tracey, T. J. G. (1997). RANDALL: A Microsoft FORTRAN program
#'   for a randomization test of hypothesized order relations.
#'   \emph{Educational and Psychological Measurement}, 57(1), 164-168.
#' @noRd
structure_randall_test <- function(data, scales, n_perm = NULL) {
  stopifnot(is_var(scales))
  stopifnot(length(scales) >= 4)
  if (!is.null(n_perm)) {
    stopifnot(is_num(n_perm, n = 1), n_perm >= 1, n_perm == round(n_perm))
    n_perm <- as.integer(n_perm)
  }

  mat <- as.matrix(data[scales])
  r <- stats::cor(mat, use = "pairwise.complete.obs")
  nv <- ncol(r)
  pred <- randall_predictions(nv)
  observed <- structure_randall(r, pred)

  if (is.na(observed)) {
    return(list(
      test = "randall", statistic = NA_real_, nv = nv,
      p_value = NA_real_, method = NA_character_, n_perm = NA_integer_
    ))
  }

  # Index of the matrix relabeled by `perm` (position k holds variable
  # perm[k]): the lower-triangle value of slot (i, j) is r[perm[i], perm[j]].
  # Same arithmetic as structure_randall, so the identity relabeling
  # reproduces `observed` exactly and the >= comparisons below are exact.
  ii <- row(r)[lower.tri(r)]
  jj <- col(r)[lower.tri(r)]
  index_of <- function(perm) {
    v <- r[cbind(perm[ii], perm[jj])]
    2 * mean(v[pred$ia] > v[pred$ib]) - 1
  }

  if (is.null(n_perm)) {
    if (nv > 9) {
      stop("exact enumeration is infeasible for more than 9 scales; supply n_perm")
    }
    perms <- cbind(1L, all_perms(seq(2L, nv)))
    null_index <- vapply(
      seq_len(nrow(perms)), function(k) index_of(perms[k, ]), numeric(1)
    )
    p_value <- mean(null_index >= observed)
    method <- "exact"
    n_perm <- nrow(perms)
  } else {
    null_index <- vapply(
      seq_len(n_perm), function(k) index_of(sample.int(nv)), numeric(1)
    )
    p_value <- (1 + sum(null_index >= observed)) / (n_perm + 1)
    method <- "monte carlo"
  }

  list(
    test = "randall", statistic = observed, nv = nv,
    p_value = p_value, method = method, n_perm = n_perm
  )
}

# User-facing entry point ------------------------------------------------------

#' Evaluate circumplex structure (Acton & Revelle, 2004)
#'
#' Run the exploratory circumplex-structure criteria of Acton and Revelle
#' (2004) on a set of scales and return one object bundling all of the tests.
#' Four criteria are computed from the first two unrotated principal-axis
#' factors of the scales' correlation matrix -- the **Fisher Test** of equal
#' axes, the **Gap Test** of equal spacing, the **Variance Test** (VT2) and
#' **Rotation Test** of interstitiality -- and each statistic is classified
#' against simulation-derived, scoring- and scale-count-specific cutoffs. A
#' fifth test, **RANDALL** (Hubert & Arabie, 1987; Tracey, 1997), evaluates the
#' hypothesised circular *order* of the scales with a randomization test that
#' yields a genuine p-value.
#'
#' @details
#' The four factor-analytic criteria have the most power when there is no large
#' general factor, which *deviation scoring* (centering each respondent on their
#' own mean across the scales, exactly what [ipsatize()] does) approximates by
#' removing it (Acton & Revelle, 2004, p. 9). Deviation scoring is therefore the
#' default and is applied to all five tests; pass `scoring = "raw"` to leave the
#' scores untouched. The two scorings carry different cutoffs, matched
#' automatically.
#'
#' The interpretive cutoffs are **heuristic likelihood classifications read off
#' simulated distributions, not significance tests**, and they are specific to
#' the number of scales. Only eight scales (the canonical octant instrument) are
#' calibrated; with any other count the statistics are still reported but no
#' interpretation is attached (see [print()]/[summary()]). The cutoffs were
#' re-derived under Acton and Revelle's own generating model at eight scales;
#' see `vignette("evaluating-circumplex-structure")`. RANDALL needs no cutoffs:
#' with up to nine scales its null distribution is enumerated exactly, so its
#' p-value is available at any scale count of four or more.
#'
#' @param data A data frame (or matrix) containing the circumplex scales.
#' @param scales A character vector of column names (or a numeric vector of
#'   column indexes) selecting the circumplex scales, **in hypothesised circular
#'   order** (the order is RANDALL's order hypothesis). At least four scales are
#'   required.
#' @param scoring Either `"deviation"` (the default; row-mean-center the scales
#'   before analysis) or `"raw"` (analyze the scores as given). Selects the
#'   matching interpretive cutoffs.
#' @param ridge A non-negative ridge added to the diagonal of the correlation
#'   matrix (then rescaled to a unit diagonal) to repair a non-positive-definite
#'   matrix before factoring; default `0`, which matches the cutoff calibration.
#'   Raise it only if factoring fails, noting that a nonzero ridge moves the
#'   statistics off the calibrated scale.
#' @param n_perm `NULL` (the default) to compute RANDALL's p-value by exact
#'   enumeration, available for up to nine scales; otherwise a single positive
#'   whole number of Monte Carlo relabelings (required for ten or more scales).
#'   The Monte Carlo path draws from the global RNG stream, so set a seed with
#'   [set.seed()] beforehand for reproducibility.
#' @return An object of class `circumplex_structure` with `print()`,
#'   [summary()], and [plot()] methods. Its components are `results` (a data
#'   frame with one row per factor-analytic criterion: statistic, cutoffs, and
#'   interpretive category), `randall` (the RANDALL index, p-value, and method),
#'   `loadings` (the two unrotated principal-axis factors), and `details`.
#' @references
#' Acton, G. S., & Revelle, W. (2004). Evaluation of ten psychometric criteria
#' for circumplex structure. \emph{Methods of Psychological Research Online},
#' 9(1), 1-27.
#'
#' Hubert, L., & Arabie, P. (1987). Evaluating order hypotheses within proximity
#' matrices. \emph{Psychological Bulletin}, 102(1), 172-178.
#'
#' Tracey, T. J. G. (1997). RANDALL: A Microsoft FORTRAN program for a
#' randomization test of hypothesized order relations. \emph{Educational and
#' Psychological Measurement}, 57(1), 164-168.
#' @seealso [cpm_fit()] for a confirmatory circumplex model; [ipsatize()] for
#'   deviation scoring.
#' @family structure functions
#' @export
#' @examples
#' data("jz2017")
#' scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
#' res <- fit_structure(jz2017, scales = scales)
#' res
#' summary(res)
fit_structure <- function(data, scales, scoring = c("deviation", "raw"),
                          ridge = 0, n_perm = NULL) {
  call <- match.call()
  scoring <- match.arg(scoring)
  stopifnot(is.data.frame(data) || is.matrix(data))
  stopifnot(is_var(scales))
  stopifnot(length(scales) >= 4)
  stopifnot(is_num(ridge, n = 1), ridge >= 0)

  mat <- as.matrix(data[, scales, drop = FALSE])
  if (!is.numeric(mat)) {
    stop("`scales` must select numeric columns.", call. = FALSE)
  }
  # Deviation scoring = row-mean-centering across the selected scales (what
  # ipsatize() does): it removes a general factor so the first two PA factors
  # span the circumplex plane (A&R p. 9). This is the calibration scoring for
  # the deviation cutoffs -- data-raw/structure-test-cutoffs.R deviation-scores
  # with `x - rowMeans(x)` and factors at ridge 0.
  if (scoring == "deviation") {
    mat <- mat - rowMeans(mat, na.rm = TRUE)
  }
  scored <- as.data.frame(mat)
  sel <- colnames(scored)

  loadings <- structure_loadings(scored, sel, ridge = ridge)
  nv <- nrow(loadings)

  # The four factor-analytic criteria share one loadings matrix; classify each
  # against its scoring- and nv-keyed cutoffs (structure_interpret returns an
  # undefined category, and NULL cutoffs, at an uncalibrated nv).
  criteria <- list(
    Fisher   = list(fn = structure_fisher, key = "fisher", hyp = "equal axes"),
    Gap      = list(fn = structure_gap,    key = "gap",    hyp = "equal spacing"),
    Variance = list(fn = structure_vt,     key = "vt",     hyp = "interstitiality"),
    Rotation = list(fn = structure_rt,     key = "rt",     hyp = "interstitiality")
  )
  rows <- lapply(names(criteria), function(nm) {
    cr <- criteria[[nm]]
    stat <- cr$fn(loadings)
    interp <- structure_interpret(stat, cr$key, nv, scoring)
    cuts <- interp$cutoffs
    data.frame(
      Test = nm, Hypothesis = cr$hyp, Statistic = stat,
      Almost = if (is.null(cuts)) NA_real_ else cuts[["almost"]],
      Thrice = if (is.null(cuts)) NA_real_ else cuts[["thrice"]],
      Twice  = if (is.null(cuts)) NA_real_ else cuts[["twice"]],
      Category = interp$category,
      stringsAsFactors = FALSE
    )
  })
  results <- do.call(rbind, rows)

  randall <- structure_randall_test(scored, sel, n_perm = n_perm)

  new_structure(
    results = results,
    randall = randall,
    loadings = loadings,
    details = list(
      nv = nv,
      scoring = scoring,
      ridge = ridge,
      calibrated = !is.null(structure_cutoffs[[as.character(nv)]]),
      scales = sel
    ),
    call = call
  )
}
