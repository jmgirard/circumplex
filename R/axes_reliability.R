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
