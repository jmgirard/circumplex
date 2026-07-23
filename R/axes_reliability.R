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
