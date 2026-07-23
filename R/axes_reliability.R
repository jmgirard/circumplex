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

# Per-axis effective test length item_n = sum of squared item weights
# (Strack et al. 2013, Table 3 col. 10; the Spearman-Brown composite length).
# Balanced octant instruments give exact integers after snapping -- 64-item ->
# 32, 32 -> 16, 16 -> 8 -- equal across axes, because the +/-.7071 weights'
# float error cancels over a full octant set. Computed per axis so unbalanced
# and deferred non-octant types degrade gracefully (Table 3 col. 10 is per axis
# and fractional for SYMLOG).
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
axes_syntax <- function(items, angles_deg) {
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
    "AX ~~ xi1*AX",
    "AY ~~ xi1*AY",
    "GEN ~~ xi2*GEN",
    "",
    "# shared scale-specificity variance (zeta1); errors free (tau-equivalent)",
    vapply(ss, function(s) paste0(s, " ~~ zeta1*", s), character(1))
  )
  paste(lines, collapse = "\n")
}

# Fit the axes-reliability model on item data through the single lavaan::cfa
# chokepoint (sem_fit_cfa, R/ssm_sem.R). `orthogonal = TRUE` is mandatory (it
# fixes every latent covariance at 0; RR09 BC4). The model assumes unit-variance
# items (the five components sum to 1, p. 4), so callers standardize the items
# before fitting -- the paper fits the item *correlation* matrix (spec section 2).
axes_fit <- function(dat, items, angles_deg, estimator = "ML",
                     se = "standard", missing = "listwise") {
  syn <- axes_syntax(items, angles_deg)
  sem_fit_cfa(
    syn, dat,
    estimator = estimator, se = se, missing = missing,
    orthogonal = TRUE
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
#' circumplex axes of an octant instrument with the item-level restricted
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
#' Missing data are handled by **listwise deletion only** (a message reports the
#' complete-case count); pairwise correlation input is never used. A boundary
#' fit (a non-positive estimated axes variance, or any negative estimated
#' variance) returns `NA` reliability and SEm with a warning and a boundary flag
#' rather than a clipped or negative value.
#'
#' @param data A data frame (or matrix) containing the circumplex items.
#' @param items Item selection. With `instrument`, a character vector of column
#'   names (or numeric indices) giving **all** items in item-number order, as in
#'   [score()]. Without `instrument`, a list with one element per scale, each a
#'   character vector (or numeric indices) of that scale's item columns.
#' @param angles A numeric vector of the scales' angles in degrees (one per
#'   scale), required for the explicit map and forbidden with `instrument`
#'   (which supplies its own). Use [octants()].
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
#' @references Strack, S., Jacobs, K. A., & Grosse Holtforth, M. (2013). The
#'   reliability of circumplex axes. \emph{SAGE Open}, 3(2).
#'   \doi{10.1177/2158244013486115}
#' @seealso [fit_structure()] for exploratory circumplex-structure criteria.
#' @export
axes_reliability <- function(data, items, angles = NULL, instrument = NULL,
                             sd = "std") {
  call <- match.call()
  stopifnot(is.data.frame(data) || is.matrix(data))
  if (is.matrix(data)) data <- as.data.frame(data)
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("`axes_reliability()` requires the lavaan package.", call. = FALSE)
  }

  map <- axes_resolve_map(data, items, angles, instrument)
  item_cols <- map$items
  angles_deg <- map$angles
  n_scales <- length(item_cols)

  # --- Refuse contract (RR09 BC12) --------------------------------------------
  if (n_scales != 8L) {
    stop(
      "`axes_reliability()` supports octant (8-scale) instruments; ",
      n_scales, " scales were supplied.",
      call. = FALSE
    )
  }
  if (anyNA(angles_deg)) {
    stop("`angles` contains a missing value.", call. = FALSE)
  }
  # The angle multiset must equal octants() modulo 360 (equal octant spacing);
  # this rejects unequal spacing and duplicate angles alike.
  if (!identical(
    sort(as.numeric(angles_deg) %% 360),
    sort(as.numeric(octants()) %% 360)
  )) {
    stop(
      "`angles` must be the eight octant angles (see octants()); an unequal ",
      "spacing or a duplicated angle is not a type-a octant circumplex.",
      call. = FALSE
    )
  }
  n_items_scale <- lengths(item_cols)
  if (any(n_items_scale < 2L)) {
    stop("Every scale must have at least 2 items.", call. = FALSE)
  }
  all_cols <- unlist(item_cols)
  missing_cols <- setdiff(all_cols, colnames(data))
  if (length(missing_cols) > 0 || anyNA(all_cols)) {
    stop(
      "Item column(s) not found in `data`: ",
      paste(stats::na.omit(union(missing_cols, all_cols[is.na(all_cols)])),
            collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  mat <- as.matrix(data[, all_cols, drop = FALSE])
  if (!is.numeric(mat)) {
    stop("`items` must select numeric columns.", call. = FALSE)
  }
  if (any(is.infinite(mat) | is.nan(mat))) {
    stop("`data` contains non-finite (Inf/NaN) values.", call. = FALSE)
  }

  # --- Listwise deletion (RR09 BC13) ------------------------------------------
  n_total <- nrow(mat)
  mat <- mat[stats::complete.cases(mat), , drop = FALSE]
  n <- nrow(mat)
  p <- ncol(mat)
  message(
    "axes_reliability(): ", n, " complete case(s) used",
    if (n < n_total) paste0(" (", n_total - n, " removed by listwise deletion)"),
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
  zdf <- as.data.frame(scale(mat))
  colnames(zdf) <- all_cols
  # Convergence, boundary, and singularity are all guarded explicitly below, so
  # lavaan's own fit-time warnings (e.g. "some estimated lv variances are
  # negative" on a boundary fit) are redundant noise; suppress them in favor of
  # this function's own clean diagnostics.
  fit <- suppressWarnings(axes_fit(zdf, item_cols, angles_deg))
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
  scale_scores <- vapply(
    item_cols, function(cols) rowMeans(mat[, cols, drop = FALSE]),
    numeric(n)
  )
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
  # alpha and the z-standardized weighted scale composite.
  rel_scale <- vapply(
    item_cols, function(cols) cronbach_alpha(mat[, cols, drop = FALSE]),
    numeric(1)
  )
  zscore <- scale(scale_scores)
  nb <- c(
    x = axis_reliability_nb(
      weights[, "w_x"], rel_scale,
      stats::var(as.numeric(zscore %*% weights[, "w_x"]))
    ),
    y = axis_reliability_nb(
      weights[, "w_y"], rel_scale,
      stats::var(as.numeric(zscore %*% weights[, "w_y"]))
    )
  )

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
      converged = TRUE, boundary = boundary
    ),
    call = call
  )
}
