# Circumplex axes reliability (Strack, Jacobs & Grosse Holtforth, 2013) --------
#
# A standalone estimator of the reliability of the circumplex axes, via a
# restricted tau-equivalent CFA that decomposes item variance and reads axis
# reliability off the isolated axes-variance component (Strack et al. 2013,
# SAGE Open 3(2), doi:10.1177/2158244013486115). The model is item-level and
# distinct from ssm_sem()'s scale-level SSM CFA (cairn milestone M54; design
# spec devel/m53-axes-reliability-spec.md, Fable review RR09).

# --- Fixed axis weights -------------------------------------------------------

# Fixed loadings of a circumplex scale on the two axes. The axes sit at
# X (communion) = 0 deg and Y (agency) = 90 deg -- the package convention
# (octants(): LM = 360; ssm_sem()'s cx/cy). A scale at angle theta loads
# w_x = cos(theta), w_y = sin(theta) (Strack et al. 2013, p. 3-4; RR09 F-4).
# Cosines route through snap_trig() so exact pole loadings stay exact and
# byte-portable across platforms' libm: theta = 360 -> (1, 0), theta = 90 ->
# (0, 1), and theta = 0 and 360 coincide (the M20/M26 pole lessons).
axis_weights <- function(angles_deg) {
  th <- as.numeric(angles_deg) * pi / 180
  cbind(w_x = snap_trig(cos(th)), w_y = snap_trig(sin(th)))
}

# Classify an angle set as an equally spaced circumplex, one status string:
# "missing", "duplicate", "unequal", or "ok". Modular by construction --
# positions reduce mod 360 first, so the package's LM = 360 and 0 are ONE
# position (RR09 section 4). Where that reduction actually bites is an angle
# supplied OUTSIDE [0, 360): c(10, 100, 190, 640) is equally spaced modulo 360,
# but a naive sorted-diff reads its wrap gap as negative and calls it a
# duplicate. A set carrying both 0 and 360 is caught either way (the wrap gap
# goes to 0), so that case does not pin this line -- the out-of-range one does,
# and is what the mutation test asserts.
# Every gap between successive positions must equal
# 360/k. The wrap-around gap from the last position back to the first is
# carried for symmetry with the modular reading, NOT because it catches a case
# the interior gaps miss: all gaps sum to 360 by construction, so k-1 interior
# gaps of 360/k force the wrap gap to 360/k too (verified by mutation -- with
# the wrap term removed, no test changes).
#
# `tol` admits floating-point representation error only -- the gaps of an
# exactly-constructed set carry ~1e-14 degrees of error at worst -- and never a
# near-equal (quasi-circumplex) set, which Strack et al. excluded (p. 5) and
# RR09 section 4 holds is scope-correct to refuse rather than merely cautious.
# A departure of 1e-4 degrees is already 4 orders of magnitude above the noise
# floor and is refused, so no real design slips through on tolerance.
angles_spacing_status <- function(angles_deg, tol = 1e-8) {
  # is.finite() rather than anyNA(): anyNA() does NOT reject +/-Inf (the M32/M35
  # lesson), and an infinite angle is worse than useless here -- `Inf %% 360` is
  # NaN and sort() SILENTLY DROPS it, so `k` below would be computed after the
  # drop and the surviving angles could satisfy 360/k and return "ok".
  if (!all(is.finite(as.numeric(angles_deg)))) return("nonfinite")
  a <- sort(as.numeric(angles_deg) %% 360)
  k <- length(a)
  gaps <- c(diff(a), 360 - (a[[k]] - a[[1]]))
  if (any(gaps <= tol)) return("duplicate")
  if (any(abs(gaps - 360 / k) > tol)) return("unequal")
  "ok"
}

# Per-axis effective test length item_n = sum of squared item weights
# (Strack et al. 2013, Table 3 col. 10; the Spearman-Brown composite length).
#
# For a balanced set of k equally spaced scales carrying n items each, both axes
# get item_n = n * k/2 at ANY rotation, because sum(cos^2) over k equally spaced
# angles is k/2 independently of where the set starts (k >= 3). That identity is
# what keeps the model's equal-axis-variance restriction -- the circumplex
# "no preferred rotation" axiom, p. 4 -- as substantively innocuous for a
# rotated or non-octant set as it is for the canonical octants (M60).
#
# Exactness, however, is an octant accident: octant sets give exact integers
# because the +/-.7071 weights' float error cancels, while 16 scales at 22.5 deg
# measure (32.000000000000000, 31.999999999999996). Compare non-octant item_n
# with a tolerance, never expect_identical(). Computed per axis so an unbalanced
# set degrades gracefully (Table 3 col. 10 is per axis, and fractional for
# SYMLOG at 8.67).
axis_item_n <- function(angles_deg, n_items) {
  w <- axis_weights(angles_deg)
  c(x = sum(n_items * w[, "w_x"]^2), y = sum(n_items * w[, "w_y"]^2))
}

# --- Reliability and SEm ------------------------------------------------------

# Spearman-Brown "list-length" reliability of a circumplex axis from its axes
# variance component xi1 (the mean inter-item correlation an axis induces) and
# effective test length item_n (Strack et al. 2013, p. 4):
# Rel = (item_n * xi1) / (1 + (item_n - 1) * xi1). item_n comes from
# axis_item_n(). Only xi1 (not the general/scale-specificity components) feeds
# reliability (p. 4).
axis_reliability_sb <- function(xi1, item_n) {
  (item_n * xi1) / (1 + (item_n - 1) * xi1)
}

# Standard error of measurement (Strack et al. 2013, p. 3): SEm = SD * sqrt(1 -
# Rel), feeding the +/-1.65*SEm single-profile location CI (p. 6). SD is the
# axis-score scale and is a researcher choice: the z-standardized default
# (sd = 1) gives SEm = sqrt(1 - rel); passing the raw axis SD (e.g. sqrt() of
# Table 3's raw-variance column) reproduces the paper's raw-scale SEm.
axis_sem <- function(rel, sd = 1) {
  sd * sqrt(1 - rel)
}

# --- The restricted tau-equivalent CFA (the lavaan constraint set) ------------

# Emit lavaan syntax for the flat fixed-links item-level model (Strack et al.
# 2013, Figure 2; spec devel/m53-axes-reliability-spec.md section 2).
#
# `items` is a list of item-name character vectors, one per circumplex scale;
# `angles_deg` is the matching per-scale angle (degrees, package convention:
# octants(), LM = 360, axes at communion 0 deg and agency 90 deg). Each item on
# a scale at angle theta loads with fixed weights cos(theta) on the X axis and
# sin(theta) on the Y axis (routed through snap_trig() so pole loadings stay
# exact and byte-portable), +1 on a single general latent, and +1 on its scale's
# specificity latent. The axis variances share one label (xi1) -- forced equal,
# the circumplex "no preferred rotation" axiom (p. 4) -- and every
# scale-specificity variance shares one label (zeta1). The general variance
# (xi2) is free; item errors stay free (tau-equivalent, p. 3). Every latent
# covariance is fixed at 0 by fitting with `orthogonal = TRUE` (lavaan frees them
# by default; RR09 BC4).
#
# Flat vs. hierarchical (RR09 Q1): Figure 2 is drawn hierarchically (items ->
# scale latents -> axes/general via fixed unit/cosine paths). This flat form is
# covariance-equivalent: every intermediate path is fixed (+1 or the cosine), so
# the product of fixed paths equals the flat fixed loading and each scale's
# disturbance becomes its specificity latent. The two are identical in fit.
axes_syntax <- function(items, angles_deg, start = NULL) {
  th <- as.numeric(angles_deg) * pi / 180
  wx <- snap_trig(cos(th))
  wy <- snap_trig(sin(th))
  ss <- sprintf("SS%d", seq_along(items))

  # One fixed loading term "w*item" per item; scales whose weight snaps to 0
  # (a pole scale on the orthogonal axis) contribute no term to that axis.
  load_terms <- function(w) {
    keep <- which(w != 0)
    unlist(lapply(keep, function(s) {
      paste0(fmt(w[[s]]), "*", items[[s]])
    }))
  }
  unit_terms <- function(nm) paste0("1*", nm)

  # Optional start values (the OLS-shadow seed): a `start(v)*` modifier on each
  # variance, floored positive so the optimizer starts inside the parameter
  # space (start values seed, never constrain -- a boundary estimate can still
  # go non-positive). No modifier when `start` is NULL (lavaan's own defaults).
  st <- function(key) {
    if (is.null(start)) "" else sprintf("start(%s)*", fmt(max(start[[key]], 0.01)))
  }

  lines <- c(
    "# circumplex axes-reliability model (generated by axes_syntax())",
    "# flat fixed-links form, covariance-equivalent to Strack (2013) Figure 2",
    "",
    paste("AX =~", paste(load_terms(wx), collapse = " + ")),
    paste("AY =~", paste(load_terms(wy), collapse = " + ")),
    paste("GEN =~", paste(unit_terms(unlist(items)), collapse = " + ")),
    vapply(
      seq_along(items),
      function(s) paste(ss[[s]], "=~", paste(unit_terms(items[[s]]), collapse = " + ")),
      character(1)
    ),
    "",
    "# equal axis variances (xi1), free general variance (xi2)",
    paste0("AX ~~ ", st("xi1"), "xi1*AX"),
    paste0("AY ~~ ", st("xi1"), "xi1*AY"),
    paste0("GEN ~~ ", st("xi2"), "xi2*GEN"),
    "",
    "# shared scale-specificity variance (zeta1); errors free (tau-equivalent)",
    vapply(ss, function(s) paste0(s, " ~~ ", st("zeta1"), "zeta1*", s),
           character(1))
  )
  paste(lines, collapse = "\n")
}

# SEM-independent OLS-shadow estimate of the three component variances (B-1):
# the off-diagonal item correlations are linear in the components --
#   r_ij = xi2 + xi1 * cos(theta_i - theta_j) + zeta1 * [scale_i == scale_j] --
# so an ordinary least-squares regression of the upper-triangle correlations on
# (1, cos-difference, same-scale) recovers (xi2, xi1, zeta1) with no SEM engine.
# Used as a cross-check on the CFA estimate (a third independent route beside
# lavaan and OpenMx) and as start values for the fit. Exact on the population
# matrix; a method-of-moments approximation in finite samples.
axes_ols_shadow <- function(R, item_angle_deg, item_scale) {
  ut <- upper.tri(R)
  th <- as.numeric(item_angle_deg) * pi / 180
  X <- cbind(
    1,
    outer(th, th, function(a, b) cos(a - b))[ut],
    as.numeric(outer(item_scale, item_scale, `==`)[ut])
  )
  b <- qr.solve(X, R[ut])
  c(xi2 = b[[1]], xi1 = b[[2]], zeta1 = b[[3]])
}

# Fit the axes-reliability model on item data through the single lavaan::cfa
# chokepoint (sem_fit_cfa, R/ssm_sem.R). `orthogonal = TRUE` is mandatory (it
# fixes every latent covariance at 0; RR09 BC4). The model assumes unit-variance
# items (the five components sum to 1, p. 4), so callers standardize the items
# before fitting -- the paper fits the item *correlation* matrix (spec section 2).
axes_fit <- function(dat, items, angles_deg, estimator = "ML",
                     se = "standard", missing = "listwise", start = NULL) {
  syn <- axes_syntax(items, angles_deg, start = start)
  sem_fit_cfa(
    syn, dat,
    estimator = estimator, se = se, missing = missing,
    orthogonal = TRUE
  )
}

# The cormat sibling of axes_fit(): the same syntax and the same mandatory
# `orthogonal = TRUE`, fit to a moment matrix instead of raw rows. It does NOT
# route through sem_fit_cfa(), and deliberately so -- that chokepoint exists to
# own the fiml/listwise `missing` translation and the multi-group group.label
# ordering, and neither concept applies to a fit with no rows (the BC5
# population oracle bypasses it for the same reason).
#
# `likelihood` is left at lavaan's default "normal", which rescales sample.cov
# by (N-1)/N. That is not an oversight to correct but the very thing that makes
# this path agree with the raw path exactly: lavaan applies the same (N-1)/N
# rescaling to the N-1 covariance it computes from raw z-scores, and that
# covariance IS cor(mat). Switching to likelihood = "wishart" here would put the
# two paths (N-1)/N apart -- see the AC2 round-trip test.
axes_fit_cormat <- function(R, items, angles_deg, n, estimator = "ML",
                            se = "standard", start = NULL) {
  lavaan::cfa(
    axes_syntax(items, angles_deg, start = start),
    sample.cov = R, sample.nobs = as.integer(n),
    estimator = estimator, se = se, orthogonal = TRUE
  )
}

# Whether a fitted lavaan model converged. A thin seam so the convergence guard
# in axes_reliability() (RR09 BC12) is testable via local_mocked_bindings().
axes_converged <- function(fit) {
  isTRUE(lavaan::lavInspect(fit, "converged"))
}

# --- Population model and simulation (oracle + bundled-data generator) ---------

# The exact population item-correlation matrix implied by the five orthogonal
# components (spec section 2). Item i on the scale at `angles_deg[s]` and item j
# on the scale at `angles_deg[t]` share xi2 (general) + xi1*cos(theta_s -
# theta_t) (axes) + zeta1*[s == t] (scale specificity); the item residual fills
# the unit diagonal. Every scale carries `n_items` items. The single
# authoritative construction shared by the population-matrix oracle (BC5), the
# finite-sample Monte-Carlo recovery (BC6), and axes_simulate().
axes_population_cor <- function(angles_deg, n_items, xi1, xi2, zeta1) {
  scale <- rep(seq_along(angles_deg), each = n_items)
  th <- rep(as.numeric(angles_deg), each = n_items) * pi / 180
  sig <- xi2 + xi1 * outer(th, th, function(a, b) cos(a - b)) +
    zeta1 * outer(scale, scale, `==`)
  diag(sig) <- 1
  list(sigma = sig, scale = scale)
}

# Simulate `n` respondents' item scores from the five-component population
# (axes_population_cor()) via the shared mvn_root() draw convention. Items are
# unit-variance by construction (the population is a correlation matrix), so the
# draws feed axes_fit() directly. Used by the BC6 Monte-Carlo recovery oracle
# and, seed-pinned, by the bundled example-dataset generator (data-raw/).
axes_simulate <- function(n, angles_deg, n_items, xi1, xi2, zeta1,
                          prefix = "item") {
  pop <- axes_population_cor(angles_deg, n_items, xi1, xi2, zeta1)
  p <- nrow(pop$sigma)
  x <- mvn_draws(n, rep(0, p), pop$sigma)
  colnames(x) <- sprintf("%s_%02d", prefix, seq_len(p))
  as.data.frame(x)
}

# --- Nunnally-Bernstein axis reliability (the comparison estimator) -----------

# Cronbach's alpha of a scale from its item scores `x` (n rows x m items):
# alpha = m/(m-1) * (1 - sum(item variances) / variance of the item sum). The
# per-scale reliability Rel_scale_i the Nunnally-Bernstein axis formula needs.
cronbach_alpha <- function(x) {
  m <- ncol(x)
  cv <- stats::cov(x)
  (m / (m - 1)) * (1 - sum(diag(cv)) / sum(cv))
}

# Nunnally-Bernstein reliability of a circumplex axis (Strack et al. 2013, p. 3;
# Nunnally & Bernstein 1994, p. 271, Eqs. 7-17), the comparison to the CFA/SB
# reliability:
#   Rel_axis(NB) = 1 - (Sum wi^2 - Sum wi^2 * Rel_scale_i) / Var_axis
# on z-standardized scale scores, where `w` are the per-SCALE cosine axis weights
# (scale-level: Sum wi^2 = 4.0 for octant type-a, NOT the item-level item_n),
# `rel_scale` each scale's reliability (cronbach_alpha()), and `var_axis` the
# observed variance of the weighted axis composite Sum(wi * scale_i). Numerator =
# Sum wi^2 (1 - rel_i) = the composite's error variance (errors uncorrelated,
# z-standardized). The paper's headline (Figure 3): N-B OVERESTIMATES axis
# reliability when scale-specificity is large, because scale-specificity inflates
# Var_axis without being charged as axis error -- the CFA reliability stays
# honest by isolating xi1.
axis_reliability_nb <- function(w, rel_scale, var_axis) {
  1 - sum(w^2 * (1 - rel_scale)) / var_axis
}

# --- Input resolution (instrument map or explicit map) ------------------------

# Resolve column selectors (character names or numeric indices) against `data`
# to character column names; an out-of-range numeric index becomes NA (caught as
# a missing item by the caller).
axes_colnames <- function(sel, data) {
  if (is.numeric(sel)) colnames(data)[sel] else as.character(sel)
}

# Normalize the two input forms to one internal map: a list of per-scale item
# column-name vectors, the matching per-scale angles (degrees), and scale labels.
# Instrument form (parallel to score()): `items` is ALL item columns in
# item-number order and the instrument's Scales$Items are 1-based indices into
# it. Explicit form: `items` is a list of per-scale item-column vectors and
# `angles` the per-scale angles.
axes_resolve_map <- function(data, items, angles, instrument) {
  if (!is.null(instrument)) {
    stopifnot(inherits(instrument, "circumplex_instrument"))
    stopifnot(is_var(items))
    if (!is.null(angles)) {
      stop("Supply either `instrument` or `angles`, not both.", call. = FALSE)
    }
    all_cols <- axes_colnames(items, data)
    key <- instrument$Scales
    item_list <- lapply(seq_len(nrow(key)), function(i) {
      nums <- as.integer(strsplit(key$Items[[i]], ",")[[1]])
      if (max(nums) > length(all_cols)) {
        stop(
          "The instrument's scale ", key$Abbrev[[i]], " indexes item ",
          max(nums), " but only ", length(all_cols), " items were supplied.",
          call. = FALSE
        )
      }
      all_cols[nums]
    })
    list(
      items = item_list,
      angles = as.numeric(key$Angle),
      labels = as.character(key$Abbrev)
    )
  } else {
    if (!is.list(items)) {
      stop(
        "Without an `instrument`, `items` must be a list of per-scale item ",
        "column vectors (and `angles` their angles).",
        call. = FALSE
      )
    }
    stopifnot(is.numeric(angles), length(angles) == length(items))
    labels <- names(items)
    if (is.null(labels)) labels <- sprintf("Scale%d", seq_along(items))
    list(
      items = lapply(items, axes_colnames, data = data),
      angles = as.numeric(angles),
      labels = labels
    )
  }
}

# --- The estimator ------------------------------------------------------------

#' Reliability of the circumplex axes (Strack, Jacobs & Grosse Holtforth, 2013)
#'
#' Estimate the reliability (and standard error of measurement) of the two
#' circumplex axes of an instrument with the item-level restricted
#' tau-equivalent CFA of Strack, Jacobs, and Grosse Holtforth (2013). The model
#' decomposes each item's variance into orthogonal components -- a general
#' factor, the two circumplex axes, scale specificity, and item specificity --
#' and reads the axes' reliability off the isolated axes-variance component with
#' the Spearman-Brown formula. It is a confirmatory, item-level complement to
#' [fit_structure()]'s exploratory scale-level criteria.
#'
#' @details
#' The model is fit to the item **correlation** matrix (the items are
#' z-standardized) as a flat fixed-links CFA: every item loads on the two axes
#' with fixed cosine weights, on a general factor with weight one, and on its
#' scale's specificity factor with weight one; the two axis variances are held
#' equal (the circumplex "no preferred rotation" axiom) and every
#' scale-specificity variance shares one value, while item errors stay free
#' (tau-equivalent). Only the axes-variance component feeds reliability.
#'
#' The Nunnally-Bernstein axis reliability (`nb_reliability`) is reported
#' alongside for comparison: it **overestimates** axis reliability when scale
#' specificity is large, because it charges scale-specificity variance to the
#' axis rather than isolating it (Strack et al. 2013, Figure 3).
#'
#' Because the model is fit to the item **correlation** matrix as if it were a
#' covariance matrix (the paper's own practice), the component point estimates
#' and the reliabilities are correct, but the component standard errors and the
#' global chi-square are **approximate** (Cudeck, 1989). Results are reported
#' **per axis** (X and Y): for a balanced instrument the two axes carry the
#' same axes-variance estimate and differ only through `item_n`.
#'
#' # Which instruments this accepts
#'
#' Any set of **equally spaced** scale angles, at any rotation: the canonical
#' octants, an interstitial set rotated 22.5 degrees off the axes, or a
#' non-octant count such as six or twelve scales. What matters is equal spacing,
#' not the count or the starting angle -- for any equally spaced set of `k`
#' scales, each axis draws the same effective test length (`k / 2` per item),
#' which is what keeps the equal-axis-variance restriction as innocuous as it
#' is for octants.
#'
#' Two limits. At least **four** scales are required: with three, every pair of
#' scales sits the same angular distance apart, and the general, axes, and
#' scale-specificity variances are then not separately identified. And spacing
#' must be equal, not merely close -- a quasi-circumplex is refused rather than
#' approximated, since Strack et al. (2013) excluded such instruments from the
#' model's validation. Every scale still needs at least two items.
#'
#' Missing data are handled by **listwise deletion only** (a message reports the
#' complete-case count); pairwise correlation input is never used. A boundary
#' fit (a non-positive estimated axes variance, or any negative estimated
#' variance) returns `NA` reliability and SEm with a warning and a boundary flag
#' rather than a clipped or negative value.
#'
#' # Supplying a correlation matrix instead of raw data
#'
#' Give `cormat` and `n` in place of `data` to estimate from an item correlation
#' matrix that someone else published, with no raw data in hand. The matrix must
#' be symmetric, positive definite, and carry a unit diagonal (the model assumes
#' unit-variance items); `items` selects and orders its rows by name, so the
#' matrix's own column order does not matter. Estimates are identical to those
#' the raw-data path would give for the same matrix.
#'
#' Two results are unavailable on this path, because both need the respondents'
#' own item scores rather than their correlations: the Nunnally-Bernstein
#' comparison is reported as `NA` (it needs each scale's alpha and the axis
#' composite's variance), and `sd = "raw"` is refused (there are no scale scores
#' to take an observed SD from). Supply the axis SDs numerically if you want SEm
#' on a raw scale.
#'
#' # Blockwise instruments
#'
#' Some circumplex instruments are administered in **blocks** (items grouped by
#' something other than their scale), which contributes a block-specificity
#' variance component of its own. This model has no such component, and the
#' package's instrument objects carry no block structure, so a blockwise
#' instrument analyzed here folds its block variance into the general and
#' scale-specificity components -- inflating them and, in turn, deflating the
#' share attributed to the axes. Strack et al. (2013, Table 3) report
#' block-specificity as high as 6.7%, so treat axes reliability from a blockwise
#' instrument as approximate.
#'
#' @param data A data frame (or matrix) containing the circumplex items. Supply
#'   exactly one of `data` or `cormat`.
#' @param cormat An item correlation matrix (the matrix-input path), symmetric
#'   with a unit diagonal and positive definite, with dimnames naming the items.
#'   Supply exactly one of `data` or `cormat`.
#' @param n For the `cormat` path, the sample size (number of observations) the
#'   correlation matrix was computed from. Required with `cormat`, and not
#'   accepted with `data` (which carries its own).
#' @param items Item selection. With `instrument`, a character vector of column
#'   names (or numeric indices) giving **all** items in item-number order, as in
#'   [score()]. Without `instrument`, a list with one element per scale, each a
#'   character vector (or numeric indices) of that scale's item columns.
#' @param angles A numeric vector of the scales' angles in degrees (one per
#'   scale), required for the explicit map and forbidden with `instrument`
#'   (which supplies its own). Must be equally spaced around the circle, at any
#'   rotation, with at least four scales; [octants()] gives the canonical eight.
#'   Angles outside `[0, 360)` are reduced onto their circumplex positions, so
#'   0 and 360 name the same position.
#' @param instrument Optional. A `circumplex_instrument` object supplying the
#'   scale angles and item membership (`Scales$Angle`, `Scales$Items`).
#' @param sd The scale for the standard error of measurement: `"std"` (the
#'   default) reports the z-standardized SEm `sqrt(1 - reliability)`; `"raw"`
#'   uses each axis composite's observed raw SD; or a numeric vector (length 1,
#'   recycled, or length 2 for the X and Y axes) of axis SDs.
#' @return An object of class `circumplex_axes_reliability` with `print()` and
#'   [summary()] methods: `results` (one row per axis: the axes variance, item_n,
#'   reliability, SEm, Nunnally-Bernstein reliability, and boundary flag),
#'   `components` (the estimated variance components with SEs), `fit` (global fit
#'   indices), and `details`.
#' @references
#' Strack, S., Jacobs, K. A., & Grosse Holtforth, M. (2013). The reliability of
#' circumplex axes. \emph{SAGE Open}, 3(2). \doi{10.1177/2158244013486115}
#'
#' Cudeck, R. (1989). Analysis of correlation matrices using covariance
#' structure models. \emph{Psychological Bulletin}, 105(2), 317-327.
#' @seealso [fit_structure()] for exploratory circumplex-structure criteria.
#' @export
#' @examplesIf requireNamespace("lavaan", quietly = TRUE)
#' # A simulated 32-item octant dataset (four items per octant scale).
#' data("simulated_items")
#'
#' # Map the item columns to their eight scales (four items each), in the
#' # octants() angle order, then estimate the axes reliability.
#' items <- split(names(simulated_items), rep(1:8, each = 4))
#' res <- axes_reliability(simulated_items, items = items, angles = octants())
#' res
#' summary(res)
#'
#' # The same estimates from the item correlation matrix alone, as when
#' # reanalyzing a matrix published without its raw data.
#' axes_reliability(
#'   cormat = cor(simulated_items), items = items, angles = octants(),
#'   n = nrow(simulated_items)
#' )
axes_reliability <- function(data = NULL, items, angles = NULL,
                             instrument = NULL, cormat = NULL, n = NULL,
                             sd = "std") {
  call <- match.call()
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("`axes_reliability()` requires the lavaan package.", call. = FALSE)
  }

  # Exactly one of data / cormat, and `n` only with cormat -- the house pattern
  # cpm_fit() already uses for its CircE-style matrix path (R/cpm_fit.R:1583).
  has_data <- !is.null(data)
  has_cormat <- !is.null(cormat)
  if (has_data == has_cormat) {
    stop("Supply exactly one of `data` or `cormat`.", call. = FALSE)
  }
  if (has_data && !is.null(n)) {
    stop(
      "`n` applies only to the `cormat` path; the raw-data path takes its ",
      "sample size from `data`.",
      call. = FALSE
    )
  }
  if (has_data) {
    stopifnot(is.data.frame(data) || is.matrix(data))
    if (is.matrix(data)) data <- as.data.frame(data)
  } else {
    cormat <- as.matrix(cormat)
    if (nrow(cormat) != ncol(cormat)) {
      stop("`cormat` must be a square matrix.", call. = FALSE)
    }
    # Both dimensions are indexed by name below (`cormat[all_cols, all_cols]`),
    # so both must carry the same names in the same order. Checking only
    # colnames() lets the commonest transcription shape through -- reading a
    # published matrix back with as.matrix(read.csv(...)) yields colnames and
    # NULL rownames -- and it then fails on the subset with a bare "subscript
    # out of bounds" instead of this refusal.
    if (is.null(colnames(cormat)) || is.null(rownames(cormat)) ||
        !identical(rownames(cormat), colnames(cormat))) {
      stop(
        "`cormat` must have dimnames naming its items, identical on both ",
        "dimensions and in the same order, so `items` can select them.",
        call. = FALSE
      )
    }
  }

  # axes_resolve_map() reads only colnames(), so the correlation matrix serves
  # as the column source on the cormat path exactly as the data frame does.
  map <- axes_resolve_map(if (has_data) data else cormat, items, angles,
                          instrument)
  item_cols <- map$items
  angles_deg <- map$angles
  n_scales <- length(item_cols)

  # --- Refuse contract (RR09 BC12; M60 generalized it past the octant set) ----
  if (anyNA(angles_deg)) {
    stop("`angles` contains a missing value.", call. = FALSE)
  }
  # anyNA() above does not reject +/-Inf (the M32/M35 lesson), and an infinite
  # angle would otherwise reach the fit: `Inf %% 360` is NaN, sort() drops it,
  # and the surviving angles can satisfy the spacing test -- so the fit dies in
  # qr.solve() naming nothing. Refuse it here, naming the offending scale.
  nonfinite <- which(!is.finite(as.numeric(angles_deg)))
  if (length(nonfinite) > 0) {
    stop(
      "`angles` must be finite; scale(s) ",
      paste(nonfinite, collapse = ", "), " carry ",
      paste(unique(as.character(as.numeric(angles_deg)[nonfinite])),
            collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  # Four scales is the identification floor, not a convention: at three equally
  # spaced scales every cross-scale pair carries the same cos(delta) = -0.5, so
  # the moment-structure design (cos delta, 1, same-scale) drops from rank 3 to
  # rank 2 and the three variance components are not separately estimable
  # (measured over k = 3:9 and 2-3 items/scale; RR09/D-026 holding 2).
  if (n_scales < 4L) {
    stop(
      "`axes_reliability()` needs at least 4 equally spaced scales; ",
      n_scales, if (n_scales == 1L) " was" else " were", " supplied.",
      if (n_scales == 3L) {
        paste0(
          " At 3 equally spaced scales every pair of scales sits the same ",
          "angular distance apart, so the general, axes, and scale-specificity ",
          "variances are not separately identified."
        )
      } else "",
      call. = FALSE
    )
  }
  shown <- paste(format(sort(as.numeric(angles_deg) %% 360)), collapse = ", ")
  # The final unnamed branch is switch()'s default: an unhandled status must
  # abort, never fall through to the fit. Unreachable today (the gates above
  # exclude "nonfinite"), but this helper is shared and switch() returns NULL
  # invisibly on no match, which would silently accept a malformed set.
  switch(angles_spacing_status(angles_deg),
    ok = NULL,
    duplicate = stop(
      "`angles` duplicates a circumplex position (0 and 360 degrees are one ",
      "position): ", shown, ".",
      call. = FALSE
    ),
    unequal = stop(
      "`angles` must be equally spaced around the circle: ", n_scales,
      " scales require a constant ", format(360 / n_scales),
      "-degree spacing, but were supplied as ", shown,
      ". A quasi-circumplex (near-equal spacing) is out of scope.",
      call. = FALSE
    ),
    stop("`angles` were not usable: ", shown, ".", call. = FALSE)
  )
  n_items_scale <- lengths(item_cols)
  if (any(n_items_scale < 2L)) {
    stop("Every scale must have at least 2 items.", call. = FALSE)
  }
  all_cols <- unlist(item_cols)
  src_cols <- if (has_data) colnames(data) else colnames(cormat)
  missing_cols <- setdiff(all_cols, src_cols)
  if (length(missing_cols) > 0 || anyNA(all_cols)) {
    stop(
      "Item column(s) not found in `", if (has_data) "data" else "cormat",
      "`: ",
      paste(stats::na.omit(union(missing_cols, all_cols[is.na(all_cols)])),
            collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (has_data) {
    mat <- as.matrix(data[, all_cols, drop = FALSE])
    if (!is.numeric(mat)) {
      stop("`items` must select numeric columns.", call. = FALSE)
    }
    if (any(is.infinite(mat) | is.nan(mat))) {
      stop("`data` contains non-finite (Inf/NaN) values.", call. = FALSE)
    }

    # --- Listwise deletion (RR09 BC13) ----------------------------------------
    n_total <- nrow(mat)
    mat <- mat[stats::complete.cases(mat), , drop = FALSE]
    n <- nrow(mat)
    p <- ncol(mat)
    message(
      "axes_reliability(): ", n, " complete case(s) used",
      if (n < n_total) {
        paste0(" (", n_total - n, " removed by listwise deletion)")
      },
      "."
    )
    if (n <= p) {
      stop(
        "Complete-case N (", n, ") must exceed the number of items (", p, ").",
        call. = FALSE
      )
    }

    item_var <- apply(mat, 2, stats::var)
    if (any(item_var <= 0)) {
      stop(
        "Zero-variance item(s): ",
        paste(all_cols[item_var <= 0], collapse = ", "), ".",
        call. = FALSE
      )
    }
    R <- stats::cor(mat)
  } else {
    # --- The correlation-matrix path --------------------------------------------
    if (!is.numeric(cormat)) {
      stop("`cormat` must be a numeric matrix.", call. = FALSE)
    }
    if (!all(is.finite(cormat))) {
      stop("`cormat` contains missing or non-finite values.", call. = FALSE)
    }
    if (!isSymmetric(unname(cormat), tol = 1e-8)) {
      stop("`cormat` must be symmetric.", call. = FALSE)
    }
    if (max(abs(diag(cormat) - 1)) > 1e-8) {
      stop(
        "`cormat` must have a unit diagonal (a correlation matrix); this model ",
        "assumes unit-variance items.",
        call. = FALSE
      )
    }
    # Subset AND reorder to the item map's order, so the fixed cosine loadings
    # line up with the items regardless of the matrix's own column order.
    R <- cormat[all_cols, all_cols, drop = FALSE]
    mat <- NULL
    p <- ncol(R)
    if (is.null(n)) {
      stop("`n` (the sample size) is required with `cormat`.", call. = FALSE)
    }
    # is_scalar_count() admits Inf (ceiling(Inf) == floor(Inf)), and Inf then
    # passes `n <= p` too -- the M32/M35 !is.finite() family. Guard it directly.
    if (!is_scalar_count(n) || !is.finite(n) || n <= p) {
      stop(
        "`n` must be a single whole number greater than the number of items (",
        p, ").",
        call. = FALSE
      )
    }
    n <- as.integer(n)
    n_total <- n
  }
  # A small positive tolerance so a near-singular matrix (e.g. duplicated or
  # collinear items, whose smallest eigenvalue is float noise ~1e-15) is refused
  # here rather than choking lavaan with a cryptic message downstream.
  if (min(eigen(R, symmetric = TRUE, only.values = TRUE)$values) <= 1e-8) {
    stop(
      "The item correlation matrix is not positive definite; the model ",
      "cannot be fit.",
      call. = FALSE
    )
  }

  # --- Fit the flat fixed-links CFA on the standardized items -----------------
  # SEM-independent OLS-shadow (B-1): a least-squares estimate of the three
  # component variances from the off-diagonal correlations, used as start values
  # for the fit and stored as a cross-check on the CFA estimate.
  item_angle <- rep(angles_deg, times = n_items_scale)
  item_scale <- rep(seq_len(n_scales), times = n_items_scale)
  ols <- axes_ols_shadow(R, item_angle, item_scale)

  # Convergence, boundary, and singularity are all guarded explicitly below, so
  # lavaan's own fit-time warnings (e.g. "some estimated lv variances are
  # negative" on a boundary fit) are redundant noise; suppress them in favor of
  # this function's own clean diagnostics.
  fit <- suppressWarnings(if (has_data) {
    zdf <- as.data.frame(scale(mat))
    colnames(zdf) <- all_cols
    axes_fit(zdf, item_cols, angles_deg, start = ols)
  } else {
    axes_fit_cormat(R, item_cols, angles_deg, n, start = ols)
  })
  if (!axes_converged(fit)) {
    stop(
      "The lavaan model did not converge; the axes reliability cannot be ",
      "estimated.",
      call. = FALSE
    )
  }

  # --- Extract components and per-axis reliability ----------------------------
  pe <- lavaan::parameterEstimates(fit)
  comp_var <- function(lat) pe$est[pe$op == "~~" & pe$lhs == lat & pe$rhs == lat]
  comp_se <- function(lat) pe$se[pe$op == "~~" & pe$lhs == lat & pe$rhs == lat]
  xi1 <- comp_var("AX")[[1]]
  xi2 <- comp_var("GEN")[[1]]
  zeta1 <- comp_var("SS1")[[1]]
  eps <- pe$est[pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs %in% all_cols]

  # Boundary: a non-positive axes variance, or any negative estimated variance,
  # is not a usable solution (RR09 BC11). NA the reliability/SEm -- never clip,
  # zero, or return a negative -- and flag it.
  boundary <- xi1 <= 0 || xi2 < 0 || zeta1 < 0 || any(eps < 0)
  if (boundary) {
    warning(
      "A boundary solution (non-positive axes variance or a negative ",
      "estimated variance) was reached; reliability and SEm are NA.",
      call. = FALSE
    )
  }

  item_n <- axis_item_n(angles_deg, n_items_scale)
  weights <- axis_weights(angles_deg)

  rel <- if (boundary) c(x = NA_real_, y = NA_real_) else {
    c(x = axis_reliability_sb(xi1, item_n[["x"]]),
      y = axis_reliability_sb(xi1, item_n[["y"]]))
  }

  # SEm scale: "std" (SD = 1), "raw" (observed axis-composite SD), or numeric.
  # "raw" needs the respondents' own scale scores, so it is unavailable from a
  # correlation matrix -- refused with the reason, never silently downgraded.
  scale_scores <- if (has_data) {
    vapply(
      item_cols, function(cols) rowMeans(mat[, cols, drop = FALSE]),
      numeric(n)
    )
  } else {
    NULL
  }
  if (identical(sd, "raw") && !has_data) {
    stop(
      "`sd = \"raw\"` needs the raw scale scores, which the `cormat` path does ",
      "not have; use \"std\" or supply the axis SDs numerically.",
      call. = FALSE
    )
  }
  axis_sd <- if (identical(sd, "std")) {
    c(x = 1, y = 1)
  } else if (identical(sd, "raw")) {
    c(
      x = stats::sd(as.numeric(scale_scores %*% weights[, "w_x"])),
      y = stats::sd(as.numeric(scale_scores %*% weights[, "w_y"]))
    )
  } else {
    stopifnot(is.numeric(sd), length(sd) %in% c(1L, 2L))
    if (length(sd) == 1L) c(x = sd, y = sd) else c(x = sd[[1]], y = sd[[2]])
  }
  sem <- if (boundary) c(x = NA_real_, y = NA_real_) else {
    c(x = axis_sem(rel[["x"]], axis_sd[["x"]]),
      y = axis_sem(rel[["y"]], axis_sd[["y"]]))
  }

  # Nunnally-Bernstein axis reliability (independent of the CFA fit): per-scale
  # alpha and the z-standardized weighted scale composite. Both inputs are
  # item-level quantities a correlation matrix cannot supply -- Cronbach's alpha
  # needs the item scores and the composite variance needs the respondents -- so
  # the cormat path reports NA with the reason (RR09 sec. 7.4: NA-with-reason,
  # never silently dropped), rather than an approximation the user cannot audit.
  nb <- if (has_data) {
    rel_scale <- vapply(
      item_cols, function(cols) cronbach_alpha(mat[, cols, drop = FALSE]),
      numeric(1)
    )
    zscore <- scale(scale_scores)
    c(
      x = axis_reliability_nb(
        weights[, "w_x"], rel_scale,
        stats::var(as.numeric(zscore %*% weights[, "w_x"]))
      ),
      y = axis_reliability_nb(
        weights[, "w_y"], rel_scale,
        stats::var(as.numeric(zscore %*% weights[, "w_y"]))
      )
    )
  } else {
    c(x = NA_real_, y = NA_real_)
  }

  results <- data.frame(
    Axis = c("X", "Y"),
    xi1 = c(xi1, xi1),
    item_n = c(item_n[["x"]], item_n[["y"]]),
    reliability = c(rel[["x"]], rel[["y"]]),
    sem = c(sem[["x"]], sem[["y"]]),
    nb_reliability = c(nb[["x"]], nb[["y"]]),
    boundary = c(boundary, boundary),
    stringsAsFactors = FALSE
  )
  components <- data.frame(
    Component = c("general", "axes", "scale_specificity", "item"),
    Symbol = c("xi2", "xi1", "zeta1", "epsilon"),
    Estimate = c(xi2, xi1, zeta1, mean(eps)),
    SE = c(comp_se("GEN")[[1]], comp_se("AX")[[1]], comp_se("SS1")[[1]], NA_real_),
    stringsAsFactors = FALSE
  )
  fm <- lavaan::fitMeasures(fit, c("chisq", "df", "pvalue", "rmsea", "cfi",
                                   "srmr"))
  new_axes_reliability(
    results = results,
    components = components,
    fit = as.list(fm),
    details = list(
      n = n, n_total = n_total, n_items = p, n_scales = n_scales,
      angles = angles_deg, labels = map$labels, sd = sd,
      input = if (has_data) "data" else "cormat",
      converged = TRUE, boundary = boundary,
      ols_shadow = ols
    ),
    call = call
  )
}
