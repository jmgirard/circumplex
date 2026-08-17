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
# set degrades gracefully -- Table 3 col. 10 is per axis, and an unbalanced or
# odd-k set gives a fractional value (five single-item positions give 2.5).
#
# Table 3's own fractional entry, SYMLOG's 8.67, is NOT such a set and is not
# reachable here: Strack fits SYMLOG as a sphere (three orthogonal axes), so its
# 26 items split 26/3 = 8.67 per axis. Under this two-axis contract a
# single-item set always gives k/2, a half-integer (M61; cairn RR11).
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

# Whether the scale-specificity component zeta1 is identified for this item map:
# exactly when at least one scale carries two items. A same-scale item PAIR is
# the only place zeta1 appears in a moment the model fits -- r_ij carries
# zeta1*[scale_i == scale_j], and i == j is the unit diagonal, not a fitted
# off-diagonal -- so with one item at every position zeta1 is perfectly
# confounded with the item residuals and the OLS shadow's same-scale design
# column is all zeros. Strack et al. (2013) drop it on exactly this condition:
# Table 3 (p. 7) prints "--" for scale-specificity on the single-item types e
# and f. A MIXED map still fits it: one multi-item scale supplies the pair, and
# the shared-label restriction carries the estimate to the single-item scales.
#
# Inferred from the item map rather than threaded as an argument (M61 gate,
# 2026-07-26), so the emitted syntax and the reported component set can never
# disagree about whether zeta1 was fitted.
axes_fits_zeta1 <- function(items) any(lengths(items) >= 2L)

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
axes_syntax <- function(items, angles_deg, item_block = NULL, start = NULL) {
  th <- as.numeric(angles_deg) * pi / 180
  wx <- snap_trig(cos(th))
  wy <- snap_trig(sin(th))
  # Scale-specificity is emitted only where it is identified (M61); with one
  # item at every position the SS latents and their shared zeta1 label are
  # dropped from the model entirely rather than fitted to the diagonal.
  fit_zeta1 <- axes_fits_zeta1(items)
  ss <- if (fit_zeta1) sprintf("SS%d", seq_along(items)) else character(0)

  # Block-specificity is emitted on the same terms (M63): only where the design
  # says it is identified, read off axes_design() so the emitted model and the
  # reported component set are one decision, never two that can drift apart.
  all_items <- unlist(items, use.names = FALSE)
  item_angle <- rep(as.numeric(angles_deg), times = lengths(items))
  item_scale <- rep(seq_along(items), times = lengths(items))
  fit_zeta2 <- axes_fits_zeta2(item_angle, item_scale, item_block)
  block_items <- if (fit_zeta2) {
    split(all_items, factor(item_block, levels = sort(unique(item_block))))
  } else {
    list()
  }
  bs <- if (fit_zeta2) sprintf("BS%d", seq_along(block_items)) else character(0)

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
  # go non-positive). No modifier when `start` is NULL (lavaan's own defaults),
  # and none when the seed simply lacks the key: the two-column OLS shadow of
  # the zeta1-dropped path returns no `zeta1` element, and `start[["zeta1"]]` on
  # a vector without that name is an error, not a NULL (M61 T2).
  st <- function(key) {
    if (is.null(start) || !key %in% names(start)) return("")
    sprintf("start(%s)*", fmt(max(start[[key]], 0.01)))
  }

  lines <- c(
    "# circumplex axes-reliability model (generated by axes_syntax())",
    "# flat fixed-links form, covariance-equivalent to Strack (2013) Figure 2",
    "",
    paste("AX =~", paste(load_terms(wx), collapse = " + ")),
    paste("AY =~", paste(load_terms(wy), collapse = " + ")),
    paste("GEN =~", paste(unit_terms(unlist(items)), collapse = " + ")),
    if (fit_zeta1) {
      vapply(
        seq_along(items),
        function(s) paste(ss[[s]], "=~", paste(unit_terms(items[[s]]), collapse = " + ")),
        character(1)
      )
    },
    if (fit_zeta2) {
      vapply(
        seq_along(block_items),
        function(b) {
          paste(bs[[b]], "=~",
                paste(unit_terms(block_items[[b]]), collapse = " + "))
        },
        character(1)
      )
    },
    "",
    "# equal axis variances (xi1), free general variance (xi2)",
    paste0("AX ~~ ", st("xi1"), "xi1*AX"),
    paste0("AY ~~ ", st("xi1"), "xi1*AY"),
    paste0("GEN ~~ ", st("xi2"), "xi2*GEN"),
    "",
    if (fit_zeta1) {
      c(
        "# shared scale-specificity variance (zeta1); errors free (tau-equivalent)",
        vapply(ss, function(s) paste0(s, " ~~ ", st("zeta1"), "zeta1*", s),
               character(1))
      )
    } else {
      # One item per scale position: zeta1 is unidentified and is dropped from
      # the model rather than fitted (Strack's types e and f). Errors stay free.
      "# no scale-specificity component (one item per position); errors free"
    },
    if (fit_zeta2) {
      c(
        "",
        "# shared block-specificity variance (zeta2), blockwise instruments only",
        vapply(bs, function(b) paste0(b, " ~~ ", st("zeta2"), "zeta2*", b),
               character(1))
      )
    } else if (!is.null(item_block)) {
      # Blocks were supplied but add no rank to the design -- the component is
      # not estimable from this map, so it is dropped rather than fitted to a
      # moment it shares with another component (M63; see axes_design()).
      # The comment deliberately does NOT name the parameter token, so that a
      # "no such component anywhere in this syntax" assertion stays meaningful
      # -- the same reason M61's dropped-scale-specificity comment does not say
      # zeta1 (its test asserts the token is absent from the whole string).
      c("", "# no block-specificity component (the supplied block map adds no",
        "# rank to the moment-structure design, so it is not estimable here)")
    }
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
#
# With one item at every scale position the same-scale indicator is identically
# zero off the diagonal, so that third column is a zero column: the design drops
# to rank 2 and qr.solve() fails outright. Drop it and regress on (1, cos-diff)
# alone, returning a two-component seed. The seed then matches the model's
# parameter set exactly, because axes_fits_zeta1() drops zeta1 on the same
# condition -- both read the item map, so they cannot disagree (M61 T3).
axes_ols_shadow <- function(R, item_angle_deg, item_scale, item_block = NULL) {
  X <- axes_design(item_angle_deg, item_scale, item_block)
  stats::setNames(qr.solve(X, R[upper.tri(R)]), colnames(X))
}

# The moment-structure design shared by the OLS shadow and the identification
# predicates: one row per off-diagonal item pair, one column per component the
# model can actually fit. The whole component set is decided here, in one place,
# so the syntax emitter, the shadow, and the reported components cannot disagree
# about which parameters exist (the M61 doctrine, extended to zeta2 at M63).
#
# Columns are added only where they carry information:
#
#   xi2, xi1   always -- the intercept and cos(theta_i - theta_j).
#   zeta1      when some same-scale pair exists off the diagonal; with one item
#              at every position the column is all zeros (M61).
#   zeta2      when adding same-block RAISES THE RANK of the design.
#
# The zeta2 test is a rank check rather than a structural rule ("some block
# spans two or more scales"), decided at the M63 implement gate, because the
# structural rule is not sufficient. Blocks pairing OPPOSITE scales span two
# scales each and are still unidentified: every same-block pair sits 180 degrees
# apart and every cross-block pair 90, so same-block is exactly -cos and adds no
# rank. The rank check catches that, the confounded-with-zeta1 case (blocks that
# are the scales), the all-one-block case (same-block is the intercept), and the
# all-singleton case (same-block is the zero column), with one test instead of
# four hand-written ones -- and catches whatever else a caller's map does that
# nobody enumerated.
axes_design <- function(item_angle_deg, item_scale, item_block = NULL) {
  p <- length(item_scale)
  ut <- upper.tri(matrix(0, p, p))
  th <- as.numeric(item_angle_deg) * pi / 180
  X <- cbind(1, outer(th, th, function(a, b) cos(a - b))[ut])
  colnames(X) <- c("xi2", "xi1")
  same <- as.numeric(outer(item_scale, item_scale, `==`)[ut])
  if (any(same != 0)) X <- cbind(X, zeta1 = same)
  if (!is.null(item_block)) {
    cand <- cbind(X, zeta2 = as.numeric(outer(item_block, item_block, `==`)[ut]))
    if (qr(cand)$rank > qr(X)$rank) X <- cand
  }
  X
}

# Whether the block-specificity component zeta2 is identified for this item and
# block map -- read off the design above, never from a separate rule, so the
# emitted syntax and the reported component set stay one decision (M61/M63).
axes_fits_zeta2 <- function(item_angle_deg, item_scale, item_block) {
  if (is.null(item_block)) return(FALSE)
  "zeta2" %in% colnames(axes_design(item_angle_deg, item_scale, item_block))
}

# Fit the axes-reliability model on item data through the single lavaan::cfa
# chokepoint (sem_fit_cfa, R/ssm_sem.R). `orthogonal = TRUE` is mandatory (it
# fixes every latent covariance at 0; RR09 BC4). The model assumes unit-variance
# items (the five components sum to 1, p. 4), so callers standardize the items
# before fitting -- the paper fits the item *correlation* matrix (spec section 2).
#
# `...` forwards further lavaan arguments through the chokepoint. It exists for
# one caller and one purpose (M65-D5): a `missing = "ml"` fit runs its OWN
# unrestricted-moments EM, for the saturated loglikelihood the fit indices are
# referenced against, and that EM needs the same raised iteration cap the
# saturated stage gets -- without a way in, the cap reached only one of the two
# EM sites and the chi-square could be computed against a stalled baseline.
axes_fit <- function(dat, items, angles_deg, item_block = NULL,
                     estimator = "ML",
                     se = "standard", missing = "listwise", start = NULL, ...) {
  syn <- axes_syntax(items, angles_deg, item_block = item_block, start = start)
  sem_fit_cfa(
    syn, dat,
    estimator = estimator, se = se, missing = missing,
    orthogonal = TRUE, ...
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
axes_fit_cormat <- function(R, items, angles_deg, n, item_block = NULL,
                            estimator = "ML",
                            se = "standard", start = NULL) {
  lavaan::cfa(
    axes_syntax(items, angles_deg, item_block = item_block, start = start),
    sample.cov = R, sample.nobs = as.integer(n),
    estimator = estimator, se = se, orthogonal = TRUE
  )
}

# Whether a fitted lavaan model converged. A thin seam so the convergence guard
# in axes_reliability() (RR09 BC12) is testable via local_mocked_bindings().
axes_converged <- function(fit) {
  isTRUE(lavaan::lavInspect(fit, "converged"))
}

# Which missing-data treatment lavaan actually used, in this package's spelling.
# lavaan reports "ml" (or "ml.x" when fixed.x is in play) for FIML; anything
# else -- including the cormat path, which has no rows and so no treatment --
# is the complete-case one. Mirrors sem_details()'s read-back in R/ssm_sem.R so
# the two entry points report the estimator identically.
axes_lav_missing <- function(fit) {
  lav <- tryCatch(
    lavaan::lavInspect(fit, "options")$missing,
    error = function(e) NULL
  )
  if (identical(lav, "ml") || identical(lav, "ml.x")) "fiml" else "listwise"
}

# Whether a fit landed on a boundary -- not a usable solution, so the caller
# NAs the reliability and SEm rather than reporting a clipped, negative, or
# imaginary value (RR09 BC11). Five disjuncts, and the first two bracket the
# axes variance on both sides:
#
#   xi1 <= 0  the axes carry no variance, so there is nothing to be reliable.
#   xi1 >= 1  the axes carry ALL of it. Spearman-Brown gives rel > 1 for
#             xi1 > 1 and exactly 1 at xi1 == 1 (rel > 1 iff xi1 > 1, since
#             item_n*xi1 > 1 + (item_n - 1)*xi1 reduces to xi1 > 1), and
#             axis_sem()'s sqrt(1 - rel) then returns NaN. Included at the
#             closed bound for symmetry with xi1 <= 0: rel == 1 requires zero
#             item-error variance, which is degenerate rather than perfect. On
#             the correlation metric this needs a grossly misspecified fit and
#             is not reachable through axes_reliability() (M62; see the test
#             file's never-NaN block), but the doctrine is never to emit a NaN,
#             so it is guarded rather than argued away.
#             What this bound does NOT claim: because it tests xi1 rather than
#             the derived rel, an xi1 within ~1e-15 of 1 is admitted while the
#             SB ratio rounds to exactly 1, giving SEm exactly 0 (M62 review,
#             finding scored 74 and recorded rather than actioned). That is
#             finite and non-negative -- a degenerate estimate, not a NaN -- so
#             it is outside what this guard is for. Moving the test onto rel
#             would close the float gap, at the cost of a per-axis predicate;
#             the sweep test pins the current behavior either way.
#
# The remaining three catch any negative estimated variance -- zeta1, zeta2, and
# the item errors. zeta1 is NULL on the
# zeta1-dropped path (M61), and NULL-ness is the same source of truth
# axes_fits_zeta1() gives the caller -- passing a separate flag alongside it
# would let the two disagree.
#
# Scalar by design: `||` errors on a length > 1 argument in R >= 4.3.
axes_is_boundary <- function(xi1, xi2, zeta1, eps, zeta2 = NULL) {
  xi1 <= 0 || xi1 >= 1 || xi2 < 0 || (!is.null(zeta1) && zeta1 < 0) ||
    (!is.null(zeta2) && zeta2 < 0) || any(eps < 0)
}

# --- Population model and simulation (oracle + bundled-data generator) ---------

# The exact population item-correlation matrix implied by the five orthogonal
# components (spec section 2). Item i on the scale at `angles_deg[s]` and item j
# on the scale at `angles_deg[t]` share xi2 (general) + xi1*cos(theta_s -
# theta_t) (axes) + zeta1*[s == t] (scale specificity); the item residual fills
# the unit diagonal. Every scale carries `n_items` items. The single
# authoritative construction shared by the population-matrix oracle (BC5), the
# finite-sample Monte-Carlo recovery (BC6), and axes_simulate().
axes_population_cor <- function(angles_deg, n_items, xi1, xi2, zeta1,
                                zeta2 = 0, item_block = NULL) {
  scale <- rep(seq_along(angles_deg), each = n_items)
  th <- rep(as.numeric(angles_deg), each = n_items) * pi / 180
  sig <- xi2 + xi1 * outer(th, th, function(a, b) cos(a - b)) +
    zeta1 * outer(scale, scale, `==`)
  # The fifth component (M63). Absent by default, so every pre-M63 caller
  # generates exactly the four-component population it did before.
  if (!is.null(item_block)) {
    sig <- sig + zeta2 * outer(item_block, item_block, `==`)
  }
  diag(sig) <- 1
  list(sigma = sig, scale = scale, block = item_block)
}

# The canonical blockwise layout: item j of every scale goes to block j, so a
# k-scale instrument with n items each has n blocks of k items, one per scale
# position. This is the crossed design that identifies zeta2 -- same-block and
# same-scale share no off-diagonal pair -- and it is the layout a blockwise
# instrument actually has, items being administered one block at a time with
# each block sampling the whole circle (Strack et al. 2013, type d).
axes_crossed_blocks <- function(n_scales, n_items) {
  rep(seq_len(n_items), times = n_scales)
}

# Simulate `n` respondents' item scores from the five-component population
# (axes_population_cor()) via the shared mvn_root() draw convention. Items are
# unit-variance by construction (the population is a correlation matrix), so the
# draws feed axes_fit() directly. Used by the BC6 Monte-Carlo recovery oracle
# and, seed-pinned, by the bundled example-dataset generator (data-raw/).
axes_simulate <- function(n, angles_deg, n_items, xi1, xi2, zeta1,
                          zeta2 = 0, item_block = NULL, prefix = "item") {
  pop <- axes_population_cor(angles_deg, n_items, xi1, xi2, zeta1,
                             zeta2 = zeta2, item_block = item_block)
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

# Normalize the `blocks` argument to a per-item integer block index aligned with
# `all_cols` -- the unlist(item_cols) order every design matrix here is built in
# -- or NULL when no blocks were supplied (the pre-M63 model).
#
# `blocks` is a list of per-block item vectors, mirroring the explicit `items`
# form exactly: same shape, same axes_colnames() name-or-index resolution, same
# optional names. That shape was chosen over a flat per-item label vector at the
# M63 implement gate because a flat vector's correctness rests on matching
# unlist(items) order silently, and a misaligned one yields a wrong answer
# rather than an error (the M25 positional-subsetting family).
#
# The blocks must PARTITION the items -- every item in exactly one block. A
# blockwise instrument administers every item in some block, so an item in no
# block is a map the model has no reading for, and an item in two is a
# contradiction; both are refused naming the offending item rather than
# resolved by a rule the caller never asked for (M63-D1).
axes_resolve_blocks <- function(blocks, src, all_cols) {
  if (is.null(blocks)) return(NULL)
  if (!is.list(blocks)) {
    stop(
      "`blocks` must be a list of per-block item column vectors (one element ",
      "per block), as `items` is a list of per-scale item column vectors.",
      call. = FALSE
    )
  }
  if (length(blocks) < 1L) {
    stop("`blocks` must name at least one block.", call. = FALSE)
  }
  cols <- lapply(blocks, axes_colnames, data = src)
  empty <- which(lengths(cols) < 1L)
  if (length(empty) > 0) {
    stop(
      "Every block must have at least 1 item; block(s) ",
      paste(empty, collapse = ", "), " have no items.",
      call. = FALSE
    )
  }
  flat <- unlist(cols, use.names = FALSE)
  unknown <- setdiff(flat, all_cols)
  if (length(unknown) > 0 || anyNA(flat)) {
    stop(
      "Block item(s) not found among the `items` columns: ",
      paste(stats::na.omit(union(unknown, flat[is.na(flat)])), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  dup <- unique(flat[duplicated(flat)])
  if (length(dup) > 0) {
    stop(
      "Item(s) in more than one block: ", paste(dup, collapse = ", "),
      ". `blocks` must partition the items.",
      call. = FALSE
    )
  }
  orphan <- setdiff(all_cols, flat)
  if (length(orphan) > 0) {
    stop(
      "Item(s) in no block: ", paste(orphan, collapse = ", "),
      ". `blocks` must partition the items.",
      call. = FALSE
    )
  }
  labels <- names(blocks)
  if (is.null(labels)) labels <- sprintf("Block%d", seq_along(blocks))
  index <- integer(length(all_cols))
  for (b in seq_along(cols)) index[match(cols[[b]], all_cols)] <- b
  list(index = index, labels = as.character(labels))
}

# --- The estimator ------------------------------------------------------------

#' Reliability of the circumplex axes (Strack, Jacobs & Grosse Holtforth, 2013)
#'
#' Estimate the reliability (and standard error of measurement) of the two
#' circumplex axes of an instrument with the item-level restricted
#' tau-equivalent CFA of Strack, Jacobs, and Grosse Holtforth (2013). The model
#' decomposes each item's variance into orthogonal components -- a general
#' factor, the two circumplex axes, scale specificity, block specificity for a
#' blockwise instrument, and item specificity -- and reads the axes'
#' reliability off the isolated axes-variance component with
#' the Spearman-Brown formula. It is a confirmatory, item-level complement to
#' [fit_structure()]'s exploratory scale-level criteria.
#'
#' @details
#' The model is fit to the item **correlation** matrix (the items are
#' z-standardized) as a flat fixed-links CFA: every item loads on the two axes
#' with fixed cosine weights, on a general factor with weight one, on its
#' scale's specificity factor with weight one, and -- when `blocks` are supplied
#' and identified -- on its block's specificity factor with weight one; the two
#' axis variances are held equal (the circumplex "no preferred rotation" axiom),
#' every scale-specificity variance shares one value and every
#' block-specificity variance shares one value, while item errors stay free
#' (tau-equivalent). Only the axes-variance component feeds reliability.
#'
#' The Nunnally-Bernstein axis reliability (`nb_reliability`) is reported
#' alongside for comparison: it **overestimates** axis reliability when scale
#' specificity is large, because it charges scale-specificity variance to the
#' axis rather than isolating it (Strack et al. 2013, Figure 3). It needs each
#' scale's coefficient alpha, which is undefined for a scale carrying a single
#' item, so it is reported as `NA` with a stated reason whenever any scale has
#' fewer than two items -- as Strack et al. themselves do, leaving it blank for
#' such instruments.
#'
#' Because the model is fit to the item **correlation** matrix as if it were a
#' covariance matrix (the paper's own practice), the component point estimates
#' and the reliabilities are correct, and both the component standard errors
#' and the global test statistic are **corrected** for that metric
#' (Cudeck, 1989; Satorra & Bentler, 1994). Results are reported
#' **per axis** (X and Y): for a balanced instrument the two axes carry the
#' same axes-variance estimate and differ only through `item_n`.
#'
#' What the correction does. Normal-theory maximum likelihood prices its
#' standard errors for a sample **covariance** input, while this estimator
#' consumes a sample **correlation** matrix, whose diagonal cannot vary at all
#' and whose off-diagonal cells are less variable than the corresponding
#' covariances. Left uncorrected, that mismatch **overstates** sampling
#' variability by about 40% for an instrument whose axes carry a lot of
#' variance (an axes variance of .35), and **understates** it slightly for
#' weak-axes, strong-general instruments -- so it could not be stated honestly
#' by any fixed caveat, because it changes sign across the range of instruments
#' this function accepts. The reported SEs are therefore adjusted to the
#' correlation metric and are calibrated uncertainty, not order-of-magnitude
#' guidance. They are typically **smaller** than the standard errors printed in
#' Strack et al. (2013), whose LISREL values carry the uncorrected
#' approximation. Point estimates, reliabilities, and SEm are unchanged by the
#' correction. What lavaan reported before it is kept in
#' `details$se_uncorrected`.
#'
#' The global test statistic carries the same mismatch in the other direction,
#' and is corrected too. Left alone it is too **small** -- fit is flattered,
#' because the sample correlations the estimator consumes vary less than the
#' covariances the reference chi-square is derived for. `chisq`, `pvalue`,
#' `rmsea` and `cfi` are therefore reported as Satorra-Bentler-type **scaled**
#' values, `chisq` divided by a factor computed at the fitted matrix (Satorra &
#' Bentler, 1994), with `cfi` additionally scaling its own baseline model. The
#' factor is not a constant: it is recomputed for every fit, which is the point
#' -- the size of the distortion depends on the instrument. `df` and `srmr` are
#' **unchanged**, being a count of restrictions and a residual summary rather
#' than test statistics with a reference distribution. What lavaan reported
#' before the scaling is kept in `details$fit_uncorrected`, and the two factors
#' in `details$scaling_factor`.
#'
#' One thing the scaling is **not**: it is not a robustness correction for
#' non-normal data. The factor is computed under normal theory throughout, and
#' it corrects one thing only -- that the estimator consumes a sample
#' correlation matrix where normal-theory maximum likelihood prices a sample
#' covariance matrix. It does not license the model against skewed or
#' heavy-tailed items, and it is unrelated to the Satorra-Bentler scaled
#' statistics reported by `ssm_sem()`'s robust estimators, which correct for
#' non-normality on the covariance metric.
#'
#' The scaled statistic matches its reference chi-square in **mean**; it is not
#' exact, and it does not make a badly misspecified model fit. If the factor
#' cannot be computed, all four are `NA` with the reason in
#' `details$fit_scaling_failed` -- never the uncorrected value in their place.
#' One refusal is shared with the component-SE correction, under one stated
#' criterion evaluated in the metric the reported numbers are computed in: a
#' fitted covariance matrix whose correlation form `cov2cor()` is degenerate
#' -- indefinite, singular, or so ill-conditioned that its smallest
#' eigenvalue, relative to its largest, falls at or below
#' `sqrt(p * .Machine$double.eps / 1e-6)`, where the `1e-6` is a stated
#' accuracy target: past that floor the corrected standard errors could carry
#' relative error above it -- is refused by both surfaces, and the refusal
#' says which degeneracy happened: `"indefinite"` when the smallest
#' eigenvalue is decisively negative (below
#' `-lambda_max * sqrt(p * .Machine$double.eps)` -- beyond the fit's own
#' numerical noise band, so it is a statement about the model),
#' `"ill_conditioned"` for
#' roundoff-level negativity, exact singularity, or mere ill-conditioning (a
#' numerical caution). Either way the corrected standard errors and the four
#' scaled statistics go `NA` together (each with its own warning naming that
#' reason) rather than one surface refusing while the other silently scales.
#' The standard-error surface additionally applies the same criterion to the
#' raw fitted matrix, which one of its internal arms inverts, so its refusals
#' nest the scaling surface's: whatever refuses the scaled statistics also
#' refuses the standard errors with the same reason -- when its two arms
#' would label one matrix differently, the correlation-metric arm's literal
#' is the one reported, which is what keeps the two surfaces in exact
#' agreement -- while a matrix degenerate only in the raw metric (wildly
#' unequal fitted variances over a well-conditioned correlation structure)
#' returns `NA` standard errors beside validly scaled fit statistics --
#' never the reverse. On a unit-diagonal fitted matrix the two metrics
#' coincide. Separately, a saturated model (`df = 0`) is refused as
#' `"saturated"` before any scaling arithmetic runs -- a refusal that
#' touches only the four scaled statistics; the corrected standard errors
#' still compute. (A `df = 0` fit is reachable today only at the internal
#' helpers' documented contract boundary: `axes_reliability()` itself
#' refuses the three-item maps that could saturate.) `df` and `srmr` still
#' report.
#'
#' If you cross-check against lavaan, match the variant. The scaled `chisq`,
#' `pvalue`, `rmsea` and `cfi` here are built with the definitions lavaan calls
#' `chisq.scaled`, `pvalue.scaled`, `rmsea.scaled` and `cfi.scaled` -- the
#' mean-adjusted Satorra-Bentler forms, `cfi` scaling its baseline term as well
#' as its model term. They are **not** the `*.robust` forms (`cfi.robust`,
#' `rmsea.robust`), which apply the Brosseau-Liard/Savalei adjustment and give
#' different numbers from the same inputs. And because the model is estimated
#' with plain maximum likelihood, the scaling being applied here rather than by
#' lavaan, `fitMeasures()` on an equivalent fit reports the **uncorrected**
#' values -- those in `details$fit_uncorrected` -- under the bare names `chisq`,
#' `pvalue`, `rmsea` and `cfi`. It reports no `*.scaled` or `*.robust` measure
#' at all on such a fit: lavaan supplies those only for a genuinely scaled
#' estimator such as `"MLM"` or `"MLR"`, and silently returns a shorter vector
#' when asked for one it does not have. So a cross-check against lavaan's bare
#' `cfi` will disagree with `$fit$cfi`, a request for `cfi.robust` will come
#' back empty rather than disagreeing, and neither outcome is a defect.
#' `details$baseline` and `details$scaling_factor` carry what you need to
#' rebuild the reported values yourself.
#'
#' # How well calibrated is the test, and at what sample size
#'
#' The scaling fixes the metric error, and the \eqn{\chi^2}{chi-squared} test
#' built on it is asymptotically exact: its rejection rate approaches the
#' nominal \eqn{\alpha}{alpha} as the number of distinct moments
#' p\eqn{^*}{*} = p(p+1)/2 falls relative to N. Measured by simulation at one
#' population (8 octant scales, 3 items each, axes variance .35), the rejection
#' rate at \eqn{\alpha}{alpha} = .05 runs .092, .079, .062, .054 at
#' p\eqn{^*}{*}/N = 0.50, 0.25, 0.12, 0.06 -- reaching the nominal band by
#' p\eqn{^*}{*}/N of about 0.06. That is a sweep at a single population, not a
#' general threshold.
#'
#' At **N = 600** the \eqn{\chi^2}{chi-squared} test **over-rejects**:
#' measured .06 to .11 at three
#' populations chosen to bracket the range of instruments this function accepts.
#' The uncorrected statistic under-rejects over the same range, at .02 to .03,
#' and -- unlike the scaled one -- moves *further* from nominal as N grows,
#' because its error is asymptotic while the scaled statistic's is a
#' finite-sample one that shrinks away.
#'
#' The over-rejection at a fixed N grows with instrument size (larger `df`) and
#' shrinks with N. So a p-value near whatever threshold you are using deserves
#' caution at moderate N and a large item count -- but note the direction: the
#' scaled test **over-flags** misfit rather than flattering it, which is the
#' safer error and the opposite of what the uncorrected statistic did.
#'
#' All of that evidence is **complete-data**. Under `missing = "fiml"` the
#' scaled statistic is calibrated in mean, but its rejection rate has not been
#' measured, so none of the rates above should be read as applying to that path.
#'
#' A related detail, in case you check: the fitted model does **not** reproduce
#' the correlation matrix's unit diagonal exactly, and that is expected rather
#' than a defect. With the loadings fixed, the stationarity condition available
#' for a free item error is the *weighted* diagonal, not the raw one, so
#' off-diagonal sampling misfit leaks into the implied diagonal at roughly the
#' sampling standard error of a correlation.
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
#' Scales may carry **one item each**, as Strack et al.'s types e and f do. With
#' a single item at every position no two items share a scale, so the
#' scale-specificity component is not identified and is dropped from the model
#' rather than estimated: the components table then has three rows instead of
#' four, and `details$zeta1_fitted` is `FALSE`. A *mixed* instrument still
#' estimates it -- one multi-item scale supplies the information, and the
#' shared-value restriction carries it to the rest.
#'
#' Two limits. At least **four** scales are required: with three, every pair of
#' scales sits the same angular distance apart, and the general, axes, and
#' scale-specificity variances are then not separately identified. And spacing
#' must be equal, not merely close -- a quasi-circumplex is refused rather than
#' approximated, since Strack et al. (2013) excluded such instruments from the
#' model's validation. Every scale needs at least one item.
#'
#' The model is two-dimensional. Instruments whose items span three dimensions
#' -- spherical designs such as SYMLOG (Strack et al.'s type f) -- are out of
#' scope, even though Strack et al. (2013) analyze one; their Table 3 SYMLOG
#' rows arise from a three-axis sphere model, not from any configuration this
#' function accepts.
#'
#' # Missing data
#'
#' `missing = "listwise"` is the default: only complete cases are used, and a
#' message reports how many there were. Pairwise-deletion correlations are
#' never used on either setting.
#'
#' `missing = "fiml"` instead estimates from every respondent who answered at
#' least one item, by full-information maximum likelihood. Two assumptions come
#' with it, and both are stronger than listwise deletion's: the data must be
#' **missing at random** (missingness may depend on values you observed, but
#' not on the unobserved values themselves) **and multivariate normal**. Under
#' MCAR — the special case where missingness is unrelated to anything —
#' listwise deletion is *consistent*, merely inefficient, so FIML buys
#' precision there and not correctness. Under MAR listwise deletion is
#' genuinely biased and FIML is not, which is when the switch is worth its
#' assumptions.
#'
#' The items are standardized by the saturated model's own FIML means and SDs,
#' never by the means and SDs of whichever cells happen to be observed, and
#' those standardized columns feed a single FIML fit. The reported standard
#' errors are observed-information standard errors on that standardized metric,
#' conditional on the standardization constants (they do not propagate the
#' uncertainty in the constants themselves). They carry the same
#' correlation-metric correction as every other path, applied multiplicatively
#' so that the observed information's own pricing of the missing data survives
#' it. What the correction does not reach is the uncertainty in the
#' standardization constants above. At mild rates that residual is too small to
#' pin down: at 2%, 5%, and 10% cellwise missingness it measures 0.1%, 0.8%,
#' and 1.8%, all well inside the Monte-Carlo error of the comparison itself
#' (about 3.6% over 200 replicates), so its size is bounded but its direction
#' at those rates is not established.
#'
#' It becomes measurable, and **anti-conservative**, as missingness grows.
#' Over 201 replicates at 15% cellwise MCAR the reported standard errors
#' average about 7% **below** the estimator's actual sampling variability, so
#' at that rate a confidence interval built from them is slightly too narrow.
#' Note the direction reverses: the mild-rate figures above, such as they are,
#' sit on the conservative side. Treat heavy missingness as the regime where
#' these standard errors are least trustworthy, and prefer a resampling
#' interval there if the uncertainty matters to your conclusion.
#'
#' The global fit statistics are scaled on this path too, by the same factor
#' the complete-data paths use rather than one rebuilt from the FIML fit's own
#' saturated stage. That is deliberate and follows the standard errors above:
#' lavaan's FIML chi-square is already referenced against the FIML saturated
#' loglikelihood, so it already prices the missing information, while the
#' scaling factor's normal-theory reference is exactly 1 -- which makes the
#' factor a metric-only ratio. A factor that priced missingness a second time
#' would double-count it.
#'
#' Two results are unavailable under `missing = "fiml"`, both because they need
#' items observed by every respondent: the Nunnally-Bernstein comparison is
#' `NA` with a stated reason, and `sd = "raw"` is refused — supply the axis SDs
#' numerically instead.
#'
#' A note on provenance: Strack et al. (2013) report no missing-data analyses,
#' so nothing about the FIML path rests on their results. It is certified
#' against this package's own synthetic oracle, where the true variance
#' components are known by construction.
#'
#' # Boundary solutions
#'
#' This contract governs every input path -- raw items on either `missing`
#' setting, and a supplied correlation matrix alike. A boundary fit returns `NA`
#' reliability and SEm with a warning and a boundary flag rather than a clipped,
#' negative, or missing value. A fit counts as a boundary when the estimated
#' axes variance falls outside `(0, 1)` -- at or below zero the axes carry no
#' variance to be reliable, and at or above one they carry all of it, which
#' drives the Spearman-Brown reliability to one or beyond, leaving the standard
#' error of measurement at zero or undefined -- or when any estimated variance
#' is negative.
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
#' Some circumplex instruments are administered in **blocks** -- items grouped
#' by something other than their scale -- which carries a block-specificity
#' variance of its own (Strack et al. 2013 report it as high as 6.7%). Supply
#' `blocks` to estimate it as a fifth component: the `components` table then
#' carries a `zeta2` row and `details$zeta2_fitted` is `TRUE`. The package's
#' instrument objects record no block structure, so the map comes from you.
#'
#' Block specificity is estimable only when the blocks are not a relabelling of
#' something the model already has. If every block coincides with a scale, or
#' all items share one block, or every item sits in its own block, the
#' component explains nothing the others do not; it is dropped from the model,
#' `details$zeta2_fitted` is `FALSE`, and the component table keeps its four
#' rows. That decision is read off the data's own moment structure rather than
#' from a rule of thumb about how the blocks look, so it also catches maps
#' whose redundancy is not obvious by eye.
#'
#' **What omitting `blocks` costs depends on the block geometry**, and it is not
#' a uniform penalty. The general factor never gives block variance back, so
#' `xi2` is inflated under most layouts and unchanged under a few; it is never
#' deflated. The axes variance -- the one reliability is read from -- moves only
#' when block membership carries information about the angular distance between
#' items, over and above what sharing a scale already says.
#'
#' The clean case is worth stating exactly, because it is both common and
#' checkable: **when each block draws exactly one item from every scale**, every
#' within-block pair is a different-scale pair and the blocks span every pair of
#' scale positions equally often. Block membership then says nothing about
#' angular distance, and `xi1`, the reliability, and the SEm are unaffected --
#' the component is worth estimating for its own sake, but ignoring it costs the
#' reliability nothing.
#'
#' Away from that case the bias runs in either direction and **"the blocks are
#' spread evenly around the circle" is not the test.** Blocks that pair
#' diametrically opposite scales are as dispersed as a block can be -- their
#' angles average to the centre of the circle -- and at eight scales they still
#' pull `xi1` about 9% below truth, because every within-block pair sits half a
#' turn apart and that is emphatically information about angular distance.
#' Blocks covering contiguous arcs pull it the other way, about 12% above. When
#' the blocks are neither one item per scale nor obviously arbitrary, estimate
#' the component rather than reasoning about the geometry.
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
#' @param blocks Optional. For a **blockwise** instrument, a list with one
#'   element per administration block, each a character vector (or numeric
#'   indices) of that block's item columns -- the same shape `items` takes for
#'   scales. The blocks must partition the items: every item in exactly one
#'   block. Supplying them adds the block-specificity component to the model;
#'   see "Blockwise instruments" below.
#' @param sd The scale for the standard error of measurement: `"std"` (the
#'   default) reports the z-standardized SEm `sqrt(1 - reliability)`; `"raw"`
#'   uses each axis composite's observed raw SD; or a numeric vector (length 1,
#'   recycled, or length 2 for the X and Y axes) of axis SDs. A supplied numeric
#'   SD must be finite and positive; anything else is refused rather than
#'   carried into the reported SEm.
#' @param missing How item-level missing data are handled on the `data` path:
#'   `"listwise"` (the default; complete cases only) or `"fiml"`
#'   (full-information maximum likelihood, via lavaan's `missing = "ml"`),
#'   which uses every respondent who answered at least one item. Not available
#'   with `cormat`, which carries no missing cells.
#' @return An object of class `circumplex_axes_reliability` with `print()` and
#'   [summary()] methods: `results` (one row per axis: the axes variance, item_n,
#'   reliability, SEm, Nunnally-Bernstein reliability, and boundary flag),
#'   `components` (the estimated variance components with SEs -- four rows by
#'   default, three when scale specificity was dropped, five when block
#'   specificity was fitted), `fit` (global fit indices), and `details`
#'   (including `zeta1_fitted` and `zeta2_fitted`, whether scale and block
#'   specificity were in the model, `blocks`, the block labels when a block map
#'   was supplied, `nb_reason`, why the Nunnally-Bernstein comparison is `NA`,
#'   `missing`, which missing-data treatment lavaan actually used,
#'   `n_complete`, the complete-case count, `min_coverage`, the fewest
#'   respondents behind any item pair, `se_uncorrected`, the component standard
#'   errors as normal-theory maximum likelihood reports them before the
#'   correlation-structure correction, `se_correction_failed`, `NULL` when
#'   that correction succeeded or a string naming why the reported SEs are
#'   `NA` -- notably the shared degeneracy criterion's two literals
#'   (smallest eigenvalue relative to largest at or below
#'   `sqrt(p * .Machine$double.eps / 1e-6)`, evaluated on `cov2cor()` of the
#'   fitted covariance matrix and, for this surface only, on the raw matrix
#'   as well): `"indefinite"` for a decisively negative smallest eigenvalue
#'   (below `-lambda_max * sqrt(p * .Machine$double.eps)`), a
#'   statement about the model, and `"ill_conditioned"` for roundoff-level
#'   negativity, singularity, or ill-conditioning, a numerical caution --
#'   which also sets `fit_scaling_failed` when the correlation form
#'   is what tripped it -- `fit_uncorrected`, the six fit statistics as
#'   lavaan reports them
#'   before the correlation-metric scaling, `scaling_factor`, the two
#'   Satorra-Bentler factors (`model` and `baseline`), and
#'   `fit_scaling_failed`, `NULL` when the scaling succeeded or a string naming
#'   why `chisq`, `pvalue`, `rmsea` and `cfi` are `NA`).
#'   `details` also carries `n_moments`, the number of distinct analyzed
#'   moments \eqn{p^* = p(p+1)/2}, and `baseline`, the independence model's
#'   **unscaled** `chisq` and `df`. Those two, with `fit$chisq`, `fit$df` and
#'   the `baseline` element of `scaling_factor` -- five inputs, since the
#'   baseline chi-square must be scaled by its own factor before it is used --
#'   reproduce the reported `cfi`. Note that `details$baseline` and the
#'   `baseline` element of `details$scaling_factor` are different quantities
#'   that share a name: the first is a chi-square and df pair, the second a
#'   scaling factor. `fit` carries `chisq`,
#'   `df`, `pvalue`, `rmsea`, `cfi` and `srmr`; the four chi-square-derived
#'   values are scaled and `df` and `srmr` are not.
#'   Three sample sizes sit beside each other in `details` and are not
#'   interchangeable. `n` is the one the fit was priced at -- the number of rows
#'   the estimator was actually handed, after listwise deletion or after
#'   dropping rows with no observed item, and the `n` you supplied on the
#'   correlation-matrix path. It is the N to divide `n_moments` by when locating
#'   a fit on the calibration table in
#'   \code{vignette("axes-reliability")}. `n_total` is the number of rows
#'   supplied before any of that, and `n_complete` the number answering every
#'   item. `n_complete` and `min_coverage` are
#'   present on every path so that a caller can read them unconditionally, and
#'   are `NA` where they carry no information: `min_coverage` outside
#'   `missing = "fiml"`, and both of them when a correlation matrix was supplied
#'   in place of raw data.
#' @references
#' Strack, S., Jacobs, K. A., & Grosse Holtforth, M. (2013). The reliability of
#' circumplex axes. \emph{SAGE Open}, 3(2). \doi{10.1177/2158244013486115}
#'
#' Cudeck, R. (1989). Analysis of correlation matrices using covariance
#' structure models. \emph{Psychological Bulletin}, 105(2), 317-327.
#'
#' Satorra, A., & Bentler, P. M. (1994). Corrections to test statistics and
#' standard errors in covariance structure analysis. In \emph{Latent variables
#' analysis: Applications for developmental research} (pp. 399-419).
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
                             blocks = NULL, sd = "std",
                             missing = c("listwise", "fiml")) {
  call <- match.call()
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("`axes_reliability()` requires the lavaan package.", call. = FALSE)
  }
  # Same spelling and same two values as ssm_sem() (R/ssm_sem.R), so the two
  # entry points to lavaan in this package name the estimator the same way; the
  # "fiml" -> "ml" translation is owned once, by sem_fit_cfa().
  missing <- match.arg(missing)

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
  # Not one of BC7's six refusal clauses, and it is not an oversight that it is
  # not: those clauses all describe a missingness pattern FIML cannot estimate
  # from, whereas this one is a path with no respondents at all. A published
  # correlation matrix carries no missing cells and no rows for the saturated
  # EM stage, so there is nothing for the argument to name.
  if (has_cormat && missing == "fiml") {
    stop(
      "`missing = \"fiml\"` needs the respondents' item scores, which the ",
      "`cormat` path does not have; supply `data`, or use the default ",
      "`missing = \"listwise\"`.",
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
        # Name only the components this map's model would actually fit: on an
        # all-single-item map zeta1 is dropped anyway (axes_fits_zeta1()), so
        # citing scale specificity as the casualty would misdirect the user
        # toward a component that was never in the model (M61 review, F3).
        paste0(
          " At 3 equally spaced scales every pair of scales sits the same ",
          "angular distance apart, so the ",
          if (axes_fits_zeta1(item_cols)) {
            "general, axes, and scale-specificity variances "
          } else {
            "general and axes variances "
          },
          "are not separately identified."
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
  # Every scale needs an item; two are no longer required (M61). One item per
  # position is Strack's types e and f, and the model handles it by dropping the
  # scale-specificity component, which is unidentified there -- see
  # axes_fits_zeta1(). A scale with NO items is still refused: it contributes
  # nothing to either axis and its angle would silently stop counting.
  n_items_scale <- lengths(item_cols)
  empty <- which(n_items_scale < 1L)
  if (length(empty) > 0) {
    stop(
      "Every scale must have at least 1 item; scale(s) ",
      paste(empty, collapse = ", "), " have none.",
      call. = FALSE
    )
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

  # Reported in `details` on every path so a caller can read the fields
  # unconditionally; set below wherever they mean something. The pairwise
  # minimum is a FIML diagnostic and stays NA elsewhere: on the listwise path
  # every retained row answered every item, so reporting N there would look
  # like a measurement rather than a tautology.
  n_complete <- NA_integer_
  min_coverage <- NA_real_

  if (has_data) {
    mat <- as.matrix(data[, all_cols, drop = FALSE])
    if (!is.numeric(mat)) {
      stop("`items` must select numeric columns.", call. = FALSE)
    }
    if (any(is.infinite(mat) | is.nan(mat))) {
      stop("`data` contains non-finite (Inf/NaN) values.", call. = FALSE)
    }

    n_total <- nrow(mat)
    p <- ncol(mat)

    if (missing == "listwise") {
      # --- Listwise deletion (RR09 BC13) --------------------------------------
      mat <- mat[stats::complete.cases(mat), , drop = FALSE]
      n <- nrow(mat)
      message(
        "axes_reliability(): ", n, " complete case(s) used",
        if (n < n_total) {
          paste0(" (", n_total - n, " removed by listwise deletion)")
        },
        "."
      )
      if (n <= p) {
        stop(
          "Complete-case N (", n, ") must exceed the number of items (", p,
          ").",
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
      zmat <- scale(mat)
      n_complete <- n
    } else {
      # --- FIML on the items (M65) --------------------------------------------
      # Two stages, one body of information. The saturated (EM) stage supplies
      # the standardizing moments and R-hat; the structured stage is ONE
      # lavaan::cfa(missing = "ml") on the columns those moments standardized.
      # Nothing here recomputes a moment from the standardized columns -- see
      # axes_fiml_moments() on why that would silently be an available-case
      # correlation wearing the FIML metric's clothes.
      cvg <- axes_fiml_coverage(mat)
      mat <- mat[cvg$keep, , drop = FALSE]
      n <- cvg$n_used
      n_complete <- cvg$n_complete
      min_coverage <- cvg$min_coverage
      message(
        "axes_reliability(): FIML on ", n,
        " respondent(s) with at least one observed item (", cvg$n_complete,
        " complete case(s))",
        if (cvg$n_dropped > 0L) {
          paste0("; ", cvg$n_dropped, " row(s) with no observed item dropped")
        },
        "; minimum pairwise coverage ", cvg$min_coverage, "."
      )

      # --- The refusal contract (BC7) ----------------------------------------
      # Clauses (i)-(iii) are readable off the missingness pattern alone and
      # fire BEFORE the EM stage. That order is the point: lavaan does not
      # refuse a moment it cannot identify, it fabricates one and returns a fit
      # that looks converged (evidence V-F), so a degenerate item or an
      # unobserved pair must be caught here or it is never caught at all.
      #
      # (i) The sample-size floor, on N_used rather than on the complete-case
      # count. This is the same floor the listwise path enforces, moved to the
      # quantity the FIML fit actually consumes -- and it is why a dataset the
      # listwise path refuses can be estimable here (BC14).
      if (n <= p) {
        stop(
          "N with at least one observed item (", n, ") must exceed the number ",
          "of items (", p, ").",
          call. = FALSE
        )
      }
      # (ii) Per-item: too few observed values to have a variance at all, then
      # a variance of zero among the values that are observed. Split in two
      # because var() on a single value returns NA rather than 0, so a
      # variance test alone would let a one-response item through as NA.
      thin_item <- which(cvg$item_n < 2L)
      if (length(thin_item) > 0) {
        stop(
          "Item(s) with fewer than 2 observed values, so no variance can be ",
          "estimated: ", paste(all_cols[thin_item], collapse = ", "), ".",
          call. = FALSE
        )
      }
      item_var <- apply(mat, 2, stats::var, na.rm = TRUE)
      if (any(item_var <= 0)) {
        stop(
          "Zero-variance item(s) among the observed values: ",
          paste(all_cols[item_var <= 0], collapse = ", "), ".",
          call. = FALSE
        )
      }
      # (iii) Per-pair: a pair no respondent answered both of. Named
      # explicitly, because this is the failure a user is least likely to
      # anticipate and the one lavaan hides most completely.
      zero_pair <- which(cvg$pair_n == 0 & upper.tri(cvg$pair_n),
                         arr.ind = TRUE)
      if (nrow(zero_pair) > 0) {
        shown_pairs <- seq_len(min(3L, nrow(zero_pair)))
        stop(
          "Item pair(s) never jointly observed, so their correlation is not ",
          "estimable: ",
          paste(
            all_cols[zero_pair[shown_pairs, 1L]], "and",
            all_cols[zero_pair[shown_pairs, 2L]],
            collapse = "; "
          ),
          if (nrow(zero_pair) > length(shown_pairs)) {
            paste0(" (and ", nrow(zero_pair) - length(shown_pairs),
                   " further pair(s))")
          },
          ".",
          call. = FALSE
        )
      }
      # Thin, but not empty: a warning rather than a refusal (M65-D2). The
      # number is named so the user can judge it, and it is a convention with
      # no inferential meaning -- see axes_fiml_min_overlap.
      #
      # Both clauses are load-bearing. The second says the thinnest pair is
      # thinner than the sample the CALLER SUPPLIED, which is precisely
      # "missingness thinned this pair": equality holds if and only if the
      # input frame had no missing cell at all. Without it the warning fired on
      # any complete sample under 30, reporting missing-data thinness on a
      # frame with no missing cell -- the sentence was then true of N and false
      # of itself. Small N alone is not this function's business to remark on,
      # and the listwise path does not remark on it either.
      #
      # The comparison is against `n_used + n_dropped` and NOT `n_used`, which
      # is counted after axes_fiml_coverage() drops all-missing rows: under
      # heavy unit nonresponse -- respondents who answered everything or
      # nothing -- every surviving row is complete, so `n_used` alone equals
      # min_coverage and silently suppressed a warning that was true.
      if (cvg$min_coverage < axes_fiml_min_overlap &&
            cvg$min_coverage < cvg$n_used + cvg$n_dropped) {
        warning(
          "Some item pair(s) were jointly observed by as few as ",
          cvg$min_coverage, " respondent(s); the estimated correlation ",
          "between them rests on very little data. (",
          axes_fiml_min_overlap,
          " is a conventional small-sample floor, not a threshold with ",
          "inferential meaning.)",
          call. = FALSE
        )
      }

      # (iv) Saturated-stage non-convergence is refused inside
      # axes_fiml_moments(), at the axes_fiml_h1() seam it owns.
      mom <- axes_fiml_moments(mat)
      R <- mom$R
      zmat <- mom$z
      # (v) Non-PD R-hat and (vi) structured-fit non-convergence are the
      # shared guards below -- the same eigenvalue floor and the same
      # axes_converged() seam the listwise path uses, now consuming R-hat and
      # the one-stage FIML fit.
    }
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
  # Blocks are resolved against the same column source and aligned to the same
  # `all_cols` order the design matrices use (M63); NULL when none were given.
  blk <- axes_resolve_blocks(blocks, if (has_data) data else cormat, all_cols)
  item_block <- blk$index
  ols <- axes_ols_shadow(R, item_angle, item_scale, item_block)

  # Convergence, boundary, and singularity are all guarded explicitly below, so
  # lavaan's own fit-time warnings (e.g. "some estimated lv variances are
  # negative" on a boundary fit) are redundant noise; suppress them in favor of
  # this function's own clean diagnostics.
  #
  # ONE warning is not noise, and is why this is withCallingHandlers() and not
  # suppressWarnings() (M65-D5): a `missing = "ml"` fit runs a SECOND
  # unrestricted-moments EM of its own, for the saturated loglikelihood that
  # chi-square, CFI and RMSEA are referenced against. It is not the saturated
  # stage above, axes_converged() cannot see it -- that predicate inspects the
  # STRUCTURED optimizer, which converges fine -- and lavaan reports its stall
  # by warning and then returning the stalled iterate anyway. Muffled, the
  # estimates and their SEs stayed correct while the global fit indices were
  # silently computed against the wrong baseline.
  em_stalled <- FALSE
  fiml_args <- if (has_data && missing == "fiml") axes_fiml_em_args() else list()
  fit <- withCallingHandlers(
    if (has_data) {
      zdf <- as.data.frame(zmat)
      colnames(zdf) <- all_cols
      do.call(axes_fit, c(
        list(zdf, item_cols, angles_deg, item_block = item_block, start = ols,
             missing = missing),
        fiml_args
      ))
    } else {
      axes_fit_cormat(R, item_cols, angles_deg, n, item_block = item_block,
                      start = ols)
    },
    warning = function(w) {
      if (axes_fiml_em_stalled(w)) {
        em_stalled <<- TRUE
      }
      invokeRestart("muffleWarning")
    }
  )
  # Order matters: a non-converged OPTIMIZER is BC7 clause (vi) and keeps its own
  # message, so it is tested first. The EM check below is the residual case --
  # the raised cap reaches both EM sites now, so this fires only if the
  # structured stage's EM needs more room than the saturated stage did on the
  # same data, which no measured dataset has.
  if (!axes_converged(fit)) {
    stop(
      "The lavaan model did not converge; the axes reliability cannot be ",
      "estimated.",
      call. = FALSE
    )
  }
  if (em_stalled) {
    stop(
      "The unrestricted (EM) stage of the FIML fit did not converge, so the ",
      "model fit statistics would be computed against a saturated model that ",
      "was never reached.",
      call. = FALSE
    )
  }

  # --- Extract components and per-axis reliability ----------------------------
  pe <- lavaan::parameterEstimates(fit)
  comp_var <- function(lat) pe$est[pe$op == "~~" & pe$lhs == lat & pe$rhs == lat]
  comp_se <- function(lat) pe$se[pe$op == "~~" & pe$lhs == lat & pe$rhs == lat]
  xi1 <- comp_var("AX")[[1]]
  xi2 <- comp_var("GEN")[[1]]
  # zeta1 exists only where the model fitted it (M61). Read it off the same
  # predicate the syntax emitter used, never off whether "SS1" happens to appear
  # in the parameter table -- one source of truth for the component set.
  fit_zeta1 <- axes_fits_zeta1(item_cols)
  zeta1 <- if (fit_zeta1) comp_var("SS1")[[1]] else NULL
  # Same discipline for zeta2 (M63): read the presence of the component off the
  # design predicate the syntax emitter used, never off whether "BS1" happens to
  # appear in the parameter table.
  #
  # Substituting the table lookup here is a NULL mutation -- no test reddens --
  # and that is correct rather than a coverage hole (the M60 lesson): BS1 is in
  # the table exactly when the emitter wrote it, and the emitter consults this
  # same predicate, so the two expressions are equal by construction today. The
  # predicate is used anyway because it stays correct if the emitter changes,
  # which is the drift no single-point mutation can exhibit. Recorded so a later
  # session does not re-chase the green.
  fit_zeta2 <- axes_fits_zeta2(item_angle, item_scale, item_block)
  zeta2 <- if (fit_zeta2) comp_var("BS1")[[1]] else NULL
  eps <- pe$est[pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs %in% all_cols]

  # Boundary: an axes variance outside (0, 1), or any negative estimated
  # variance, is not a usable solution (RR09 BC11; M62 added the upper bound).
  # NA the reliability/SEm -- never clip, zero, or return a negative or a NaN --
  # and flag it. The predicate is a named seam so the unreachable-in-practice
  # upper bound is still testable; see axes_is_boundary() for each disjunct.
  boundary <- axes_is_boundary(xi1, xi2, zeta1, eps, zeta2)
  if (boundary) {
    warning(
      "A boundary solution (an axes variance outside (0, 1), or a negative ",
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
  # Not computed under FIML: a row with any missing item has no scale score,
  # and rowMeans() would quietly hand back NA for it. Every consumer below is
  # unavailable on that path anyway (both are refused or NA'd just after), so
  # the composites are skipped rather than computed and discarded.
  scale_scores <- if (has_data && missing != "fiml") {
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
  # A hard error, not an NA (D-034 correction 2). The available-case SD of the
  # axis composite is computable here and looks perfectly reasonable, which is
  # the problem: it is the available-case quantity this path exists to avoid,
  # and it would land in the reported SEm with nothing marking it. Refusing
  # sends the user to an SD they chose knowingly.
  if (identical(sd, "raw") && missing == "fiml") {
    stop(
      "`sd = \"raw\"` needs each respondent's complete axis composite, which ",
      "the FIML path does not have; an available-case SD would reintroduce ",
      "exactly the bias this path avoids. Use \"std\" or supply the axis SDs ",
      "numerically.",
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
    # An axis SD scales SEm = sd * sqrt(1 - rel), so an unusable value here
    # lands straight in the results frame: -1 gave a negative SEm, Inf an
    # infinite one, and NA/NaN a missing one with nothing to say why (all
    # measured at the M62 plan gate). is.finite() rather than is.na(), which
    # admits +/-Inf -- the M32/M35 lesson, fourth recurrence. Zero is refused
    # with the negatives: it reports SEm = 0 on every axis, indistinguishable
    # from perfect measurement.
    if (!all(is.finite(sd)) || any(sd <= 0)) {
      stop(
        "`sd` must be finite and positive; received ",
        paste(format(sd), collapse = ", "), ".",
        call. = FALSE
      )
    }
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
  #
  # A second unavailability arrives with M61 (M61-D1): Cronbach's alpha is
  # undefined for a one-item scale -- cronbach_alpha() divides by m - 1, so it
  # returns NaN -- and the N-B formula has no rel_scale to consume. The test is
  # "ANY scale carries fewer than two items", NOT "zeta1 was dropped": a MIXED
  # map still fits zeta1 yet has an undefined alpha on its single-item scales,
  # and would otherwise propagate NaN into the results frame. Strack et al.
  # corroborate: Table 3 col 14 is blank for every single-item row, and p. 5
  # states the formula "was not applied for analyzing instruments with a single
  # item per spatial position".
  # The two unavailabilities are independent and can both hold at once -- a
  # correlation matrix whose scales each carry one item has no raw scores AND no
  # defined alpha. `nb_reason` therefore carries every reason that applies, not
  # the first one matched, so `details` stays a faithful record and print()
  # states both (M61 review, F4). `c()` drops the NULLs, so it is NULL when the
  # comparison is available.
  # A third unavailability arrives with M65: both N-B inputs are available-case
  # quantities under FIML. Cronbach's alpha over the observed cells and the
  # variance of a composite only complete respondents have are exactly the
  # metric RR12 ruled against, so the comparison is NA'd rather than computed
  # from whatever happened to be answered -- and, like the two before it, this
  # reason accumulates rather than replacing them.
  nb_reason <- c(
    if (!has_data) "cormat",
    if (missing == "fiml") "fiml",
    if (any(n_items_scale < 2L)) "single_item"
  )
  nb <- if (is.null(nb_reason)) {
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
  # The component set is variable-length (M61): the scale-specificity row is
  # present only where the model fitted zeta1. Rows are assembled from a list so
  # a dropped component leaves no row at all -- never an NA row, which would
  # read as "estimated, unavailable" rather than "not in this model".
  comp_rows <- list(
    c(Component = "general", Symbol = "xi2"),
    c(Component = "axes", Symbol = "xi1"),
    if (fit_zeta1) c(Component = "scale_specificity", Symbol = "zeta1"),
    if (fit_zeta2) c(Component = "block_specificity", Symbol = "zeta2"),
    c(Component = "item", Symbol = "epsilon")
  )
  comp_rows <- Filter(Negate(is.null), comp_rows)
  comp_est <- c(xi2, xi1, if (fit_zeta1) zeta1, if (fit_zeta2) zeta2,
                mean(eps))
  # What lavaan reported: normal-theory SEs priced for a sample COVARIANCE
  # input, while this estimator consumes a sample CORRELATION matrix. Retained
  # in `details` after the correction below replaces them, so the correction
  # stays auditable without a supported opt-out argument (M66 implement gate).
  se_uncorrected <- c(
    xi2 = comp_se("GEN")[[1]], xi1 = comp_se("AX")[[1]],
    if (fit_zeta1) c(zeta1 = comp_se("SS1")[[1]]),
    if (fit_zeta2) c(zeta2 = comp_se("BS1")[[1]])
  )

  # --- The corrected component standard errors (M66; RR13 BC1, D-035) --------
  #
  # `fitted(fit)$cov` is NOT in item-map order -- lavaan orders by first
  # appearance in the syntax -- so `all_cols` is passed alongside it and
  # axes_corrected_se() realigns off the dimnames. See that function; consuming
  # the matrix positionally returns a plausible number 3.6x off.
  #
  # `n` is the sample size the SEs are priced at on each path: complete cases
  # for listwise, the supplied `n` for cormat.
  corrected <- axes_corrected_se(
    axes_fitted_cov(fit), all_cols, item_angle, item_scale, item_block,
    n = n, fit_zeta1 = fit_zeta1, fit_zeta2 = fit_zeta2
  )
  se_reported <- if (missing == "fiml") {
    # The FIML path composes MULTIPLICATIVELY rather than by replacement
    # (RR13 BC4). Its observed-information SEs price the MISSING information
    # correctly -- they rise with the missingness rate, which is the job RR12
    # section 3 required of them -- and the complete-data formula above does
    # not price it at all. Multiplying by the metric ratio removes the
    # correlation-as-covariance error while KEEPING that pricing; replacing the
    # SE outright would silently discard it.
    #
    # `fiml_ratio` is taken from the helper rather than built here as
    # `corrected/naive`. Those two are priced at DIFFERENT matrices by design
    # (raw and cov2cor respectively), so their quotient is not the metric-only
    # conversion this line needs -- it inflates the reported SE by N/(N-1),
    # 0.17% at n = 600 and 1% at n = 100. D-037 supersedes RR13 BC4's
    # "evaluated at Sigma-hat" for exactly this reason.
    se_uncorrected * corrected$fiml_ratio[names(se_uncorrected)]
  } else {
    corrected$corrected[names(se_uncorrected)]
  }
  comp_ses <- c(
    se_reported[["xi2"]], se_reported[["xi1"]],
    if (fit_zeta1) se_reported[["zeta1"]],
    if (fit_zeta2) se_reported[["zeta2"]],
    NA_real_
  )
  components <- data.frame(
    Component = vapply(comp_rows, `[[`, character(1), "Component"),
    Symbol = vapply(comp_rows, `[[`, character(1), "Symbol"),
    Estimate = comp_est,
    SE = comp_ses,
    stringsAsFactors = FALSE
  )
  # SRMR is requested by its COVARIANCE-ONLY name, not the bare "srmr" alias.
  # The alias is path-dependent: on a `missing = "ml"` fit lavaan resolves it to
  # the mean-inclusive Bentler SRMR, while the listwise and cormat fits get the
  # covariance-only one -- so `$fit$srmr` silently measured two different things
  # depending on an argument, and did so on data with no missing cells at all.
  #
  # The mean-inclusive variant is not merely different here, it is wrong for this
  # model: lavaan frees every item intercept, so the mean structure is saturated,
  # the mean residuals are structurally zero, and the extra terms only dilute the
  # denominator from p(p+1)/2 to p(p+1)/2 + p. Measured: the reported value came
  # out deflated by exactly sqrt((p+1)/(p+3)) -- 0.96225 = sqrt(25/27) at p = 24,
  # 0.94591 = sqrt(17/19) at p = 16 -- always in the flattering direction, on a
  # documented return field readers compare against Hu & Bentler's .08.
  #
  # Named unconditionally rather than branched on `missing`, because on the
  # listwise and cormat fits the two names return bit-identical values (measured
  # on lavaan 0.6.21 AND 0.7-2, both paths), so this cannot disturb AC1's
  # bit-identity to the shipped numbers. The returned element keeps the name
  # "srmr": the contract is the quantity, not lavaan's spelling of it.
  #
  # The last three are NOT reported. They are the inputs the scaled statistic is
  # rebuilt from (M68): CFI needs the independence model's own chi-square and
  # df, and RMSEA needs the sample size lavaan itself priced the index at. They
  # go through the same membership guard as the reported six, because the scaled
  # values are as version-dependent on them as `$fit$srmr` is on its own name.
  want <- c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr_bentler_nomean",
            "baseline.chisq", "baseline.df", "ntotal")
  fm <- lavaan::fitMeasures(fit, want)
  # lavaan DROPS a measure name it does not recognize, returning a shorter
  # vector rather than refusing (measured on BOTH generations: two names, one
  # bogus, returns one element -- silently on 0.6.21, and on 0.7.2 with a
  # `unknown fit measure: '<name>'` warning. The drop is what matters and is
  # common to both; only the silence is version-specific).
  # `srmr_bentler_nomean` is an internal variant rather than a documented alias
  # and lavaan is a Suggests with no version floor, so a future rename would
  # otherwise delete `$fit$srmr` -- a documented @return field -- and leave the
  # object looking well formed. Refuse instead of shipping a hole.
  #
  # Keyed on MEMBERSHIP, then reordered here. `identical(names(fm), want)` also
  # failed on order and on length, for which `setdiff()` reports nothing, so any
  # mismatch that was not a dropped name refused with the degenerate message
  # "(missing: )" -- a guard that fails safe while telling the user nothing.
  if (!all(want %in% names(fm))) {
    stop(
      "The installed lavaan did not return the expected fit measures (missing: ",
      paste(setdiff(want, names(fm)), collapse = ", "),
      "). This is a lavaan version incompatibility; please report it.",
      call. = FALSE
    )
  }
  fm <- fm[want]
  names(fm)[names(fm) == "srmr_bentler_nomean"] <- "srmr"

  # The global test statistic carries the correlation-as-covariance mismatch in
  # the OTHER direction from the component SEs above -- lavaan refers T to a
  # chi-square derived for a sample COVARIANCE matrix, so fit is flattered. The
  # Satorra-Bentler scaling factor removes it; see R/axes_scaled_fit.R.
  #
  # `sigma` arrives in lavaan's variable order and is realigned there off its
  # own dimnames, exactly as for the SEs.
  #
  # ALL THREE INPUT PATHS SCALE, and the FIML path uses this same complete-data
  # factor rather than one built from its saturated stage (M68-D1). The reason
  # is the composition argument the SEs make just above: lavaan's FIML T is
  # already referenced against the FIML saturated loglikelihood, so it already
  # prices the missing information, while c's normal-theory reference is exactly
  # 1 -- which makes c a metric-only ratio. A factor that priced missingness
  # again would price it twice. A path-dependent `$fit$chisq` is also the exact
  # trap the M65 SRMR fix cured a few lines up.
  #
  # Order matters and is already right: the `em_stalled` refusal fires far above
  # this point, so no scaled statistic is ever computed against a saturated
  # model that was never reached.
  scaling <- axes_scaling_factor(
    axes_fitted_cov(fit), all_cols, item_angle, item_scale, item_block,
    fit_zeta1 = fit_zeta1, fit_zeta2 = fit_zeta2,
    df = fm[["df"]], baseline_df = fm[["baseline.df"]]
  )
  scaled <- axes_scale_fit_measures(fm, scaling)

  new_axes_reliability(
    results = results,
    components = components,
    fit = scaled$fit,
    details = list(
      n = n, n_total = n_total, n_items = p, n_scales = n_scales,
      # The count of distinct analyzed moments, p* = p(p+1)/2 (M70). Reported
      # rather than left to the caller because `p*/N` -- with `n` above as the
      # N, the one lavaan was actually handed -- is what the calibration table
      # in vignette("axes-reliability") is indexed by, and recomputing it off
      # `n_items` invites reading `n_total` or `n_complete` as the denominator.
      n_moments = p * (p + 1) / 2,
      angles = angles_deg, labels = map$labels, sd = sd,
      input = if (has_data) "data" else "cormat",
      # Read off the FITTED object rather than echoed from the argument, the
      # same discipline sem_details() uses (R/ssm_sem.R). An echo would keep
      # saying "fiml" if the argument ever stopped reaching lavaan, which is
      # the one failure this field exists to make visible.
      missing = axes_lav_missing(fit),
      n_complete = n_complete, min_coverage = min_coverage,
      converged = TRUE, boundary = boundary,
      # Whether the scale-specificity component was in the fitted model at all
      # (M61): FALSE means one item per scale position, so zeta1 was
      # unidentified and dropped rather than estimated.
      zeta1_fitted = fit_zeta1,
      # Whether block specificity was in the fitted model (M63). FALSE with
      # `blocks` supplied means the map did not identify zeta2 -- the blocks
      # added no rank to the moment-structure design -- so it was dropped
      # rather than fitted to a moment it shares with another component.
      zeta2_fitted = fit_zeta2,
      blocks = if (is.null(blk)) NULL else blk$labels,
      # Why the Nunnally-Bernstein comparison is NA, or NULL when it is
      # available: "cormat" (no raw scores) or "single_item" (alpha undefined).
      nb_reason = nb_reason,
      # What lavaan reported before the correlation-structure correction (M66).
      # Kept so a user can see the size of the correction on their own data,
      # and so a pre-M66 analysis can be reproduced, without the package
      # offering a supported way to ASK for the uncorrected number.
      se_uncorrected = se_uncorrected,
      # NULL when the correction succeeded; otherwise why every corrected SE
      # is NA ("singular" from the nonpositive-diagonal door or non-finite
      # entries, "infinite_diagonal", "ill_conditioned"/"indefinite" from the
      # stated degeneracy criterion's M90 partition -- M89/M90,
      # axes_sigma_degenerate(); when the helper's two arms would label one
      # matrix differently, the correlation-metric arm's literal is reported
      # -- and "unidentified", "indefinite" forwarded from axes_se_pricing()
      # as backstops behind it). M71 audited the list against the source;
      # the PRICING "indefinite" backstop has never been observed to fire
      # (R/axes_corrected_se.R:185-198), so this enumerates what the helper
      # CONTAINS, not what a user has been shown -- the criterion's
      # "indefinite", by contrast, fires on any decisively indefinite
      # fitted matrix (tested).
      se_correction_failed = corrected$reason,
      # NULL unless the SE helper's internal raw arm -- the uncorrected
      # normal-theory pricing, kept only as the tie to lavaan's own SE
      # (D-037) -- was refused while every reported number computed (M91;
      # RR18 rec 7). Carries the same refusal vocabulary as
      # `se_correction_failed`. Deliberately silent: no warning and no
      # printed note accompany it, because the refused quantity is never
      # user-reported and the reported SEs beside it are present and correct
      # (M91-D1).
      naive_reason = corrected$naive_reason,
      # What lavaan reported before the correlation-metric scaling (M68), on the
      # same footing as `se_uncorrected` above: visible for comparison and for
      # reproducing a pre-M68 analysis, without the package offering a supported
      # way to ASK for the uncorrected statistic.
      fit_uncorrected = scaled$uncorrected,
      # The independence model's chi-square and df (M70), in the grouped shape
      # `scaling_factor` below uses. Rebuilding `cfi` takes five inputs -- these
      # two, `fit$chisq`, `fit$df`, and `scaling_factor[["baseline"]]`, which
      # scales the baseline chi-square before the excess is taken -- and these
      # were the two a caller could not otherwise obtain without inverting the
      # uncorrected value. Both are lavaan's own, UNSCALED:
      # `axes_scale_fit_measures()` applies the `baseline` factor at the point
      # of use, which is why the rebuild needs it separately.
      baseline = c(chisq = fm[["baseline.chisq"]], df = fm[["baseline.df"]]),
      # The two Satorra-Bentler factors: `model` divides the fitted model's
      # chi-square, `baseline` the independence model's (which only CFI reads).
      # Both are NA together when the scaling failed.
      scaling_factor = c(model = scaling$scale, baseline = scaling$baseline),
      # NULL when the scaling succeeded; otherwise why the four chi-square-
      # derived statistics are NA ("singular", "ill_conditioned"/"indefinite"
      # from the shared degeneracy criterion's M90 partition (M89/M90),
      # "saturated", "unidentified", "df_mismatch", "baseline_df_mismatch",
      # "infinite_diagonal"), enumerated from the source the same way as the
      # SE list above, and carrying the same caveat: it enumerates what the
      # helper CONTAINS, not what a user has been shown. All eight are
      # reachable at the helper's contract boundary; what a call through
      # axes_reliability() can actually reach is a strictly smaller set,
      # because this assembly refuses upstream several of the shapes that
      # reach them.
      #
      # Three of them are worth naming, for what the M89/M90 re-cuts left:
      #
      #   "unidentified"  fires when Delta'V Delta is singular. One measured
      #                   route remains: a degenerate Delta (a one-scale map
      #                   makes zeta1 identical to the all-ones xi2), which no
      #                   conditioning test of Sigma-hat can see. The re-cut
      #                   closed the other measured route (an ordinary map at a
      #                   correlation-degenerate Sigma-hat): the criterion now
      #                   prices cov2cor(Sigma-hat) -- the metric everything
      #                   below computes in -- so that shape is refused at the
      #                   door (RR18).
      #   "saturated"     fires on df = 0 (M90, RR18 BC4), ahead of every
      #                   matrix computation. Before the guard, the measured
      #                   p = 3 saturated construction (the only p where q can
      #                   reach p(p+1)/2, and only with zeta1 fitted) reached
      #                   the cval division and answered "indefinite" via
      #                   cval = Inf -- a comment here previously claimed it
      #                   answered "unidentified"; that claim was measured
      #                   false at the M90 replan audit.
      #   "indefinite"    is, since M90, the criterion's own word for a
      #                   decisively negative smallest eigenvalue -- past the
      #                   convergence-noise band, a statement about the
      #                   model. The final nonpositive/non-finite-factor
      #                   backstop no longer says "indefinite": it says
      #                   "ill_conditioned", because a negative computed cval
      #                   there is cancellation, not evidence (its one
      #                   measured instance, kappa = 6.65e6 at p = 3, has a
      #                   positive exact value and is refused at the door by
      #                   the tau floor -- RR18; a 30,000-draw search reached
      #                   the backstop zero times, M90 AC5).
      #
      # None of these routes outlives the assembly: axes_reliability()
      # refuses fewer than four scales, and axes_design() drops a component
      # collinear with another, so no call through it has been seen to reach
      # them at the exported surface.
      fit_scaling_failed = scaling$reason,
      ols_shadow = ols
    ),
    call = call
  )
}
