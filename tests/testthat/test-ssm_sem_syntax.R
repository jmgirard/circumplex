# T2 (M5): lavaan-syntax generator from a circumplex instrument.
# Traces to devel/m5-sem-design.md sections noted per block.

# Helpers ----------------------------------------------------------------------

# The conventional (Gurtman) closed-form SSM weights: e = mean, x = (2/p)*cos,
# y = (2/p)*sin. This is what ssm_parameters_cpp() applies (src/parameters.cpp).
closed_form_weights <- function(angles_deg) {
  th <- angles_deg * pi / 180
  p <- length(th)
  m <- rbind(rep(1 / p, p), (2 / p) * cos(th), (2 / p) * sin(th))
  rownames(m) <- c("e", "x", "y")
  m
}

# An angle set that VIOLATES the harmonic-balance condition (spec §2.1/§3.5),
# so the OLS projection and the closed form genuinely disagree there. Not
# equally spaced and not a balanced-but-unequal set.
unbalanced_angles <- c(0, 30, 90, 200, 290)

# The OLS weights: (spec §2.1) ------------------------------------------------

test_that("OLS weights equal the closed form at equally spaced angles (§2.1)", {
  th <- as.numeric(octants())
  W <- sem_ols_weights(th * pi / 180, names = paste0("s", seq_along(th)))
  expect_equal(unname(W), unname(closed_form_weights(th)), tolerance = 1e-12)
  expect_equal(rownames(W), c("e", "x", "y"))
  expect_equal(colnames(W), paste0("s", seq_along(th)))
})

test_that("OLS weights are a left inverse of B at any full-rank angle set (§2.1)", {
  for (th_deg in list(as.numeric(octants()), unbalanced_angles, c(0, 40, 130, 250))) {
    th <- th_deg * pi / 180
    B <- cbind(1, cos(th), sin(th))
    W <- sem_ols_weights(th, names = paste0("s", seq_along(th)))
    expect_equal(unname(W %*% B), diag(3), tolerance = 1e-12)
  }
})

test_that("OLS weights differ from the closed form off harmonic balance (§2.1/§3.5)", {
  th <- unbalanced_angles * pi / 180
  W <- sem_ols_weights(th, names = paste0("s", seq_along(th)))
  cf <- closed_form_weights(unbalanced_angles)
  # The two functionals genuinely diverge here (review: max diff ~0.18).
  expect_gt(max(abs(unname(W) - unname(cf))), 0.05)
})

test_that("OLS weights recover an exactly-cosine profile off balance; closed form does not (§5.5)", {
  th <- unbalanced_angles * pi / 180
  # profile_i = E + A*cos(theta_i - D)
  E <- 2; A <- 1.5; D <- 40 * pi / 180
  profile <- E + A * cos(th - D)
  truth <- c(E, A * cos(D), A * sin(D))
  W <- sem_ols_weights(th, names = paste0("s", seq_along(th)))
  expect_equal(as.numeric(W %*% profile), truth, tolerance = 1e-12)
  cf <- closed_form_weights(unbalanced_angles)
  expect_gt(max(abs(as.numeric(cf %*% profile) - truth)), 0.05)
})

test_that("degenerate angle geometry (rank < 3) is refused (§2.1)", {
  # Fewer than 3 distinct angles cannot span the cosine basis.
  expect_error(
    sem_ols_weights(c(0, 180) * pi / 180, names = c("a", "b")),
    "rank|distinct|three|full"
  )
})

# Syntax generation contract (spec §3.5) --------------------------------------

test_that("ssm_sem_syntax() returns a string carrying the weights attribute (§3.5)", {
  syn <- ssm_sem_syntax(scales = paste0("s", 1:8), angles = as.numeric(octants()))
  expect_type(syn, "character")
  W <- attr(syn, "weights")
  expect_equal(dim(W), c(3, 8))
  expect_equal(unname(W), unname(closed_form_weights(as.numeric(octants()))),
    tolerance = 1e-12
  )
  expect_equal(attr(syn, "model"), "scaled")
})

test_that("ssm_sem_syntax() reads angles and names from an instrument (§3.5)", {
  skip_on_cran()
  syn <- ssm_sem_syntax(instrument = iipsc)
  expect_equal(as.numeric(attr(syn, "angles")), iipsc$Scales$Angle)
  # Scale names appear as observed variables in the syntax.
  expect_true(all(vapply(iipsc$Scales$Abbrev, function(n) grepl(n, syn), logical(1))))
})

test_that("emitted syntax never contains ampl:= or disp:= (§2.2/§3.5)", {
  for (m in c("scaled", "strict")) {
    syn <- ssm_sem_syntax(
      scales = paste0("s", 1:8), angles = as.numeric(octants()), model = m
    )
    expect_false(grepl("ampl\\s*:=", syn))
    expect_false(grepl("disp\\s*:=", syn))
  }
})

test_that("inspection := lines auto-emit under strict+measure, not scaled (§3.5)", {
  # The covariance-metric inspection lines are linear (and thus clean) only
  # under strict and only for a measure's covariance profile; scaled makes
  # them nonlinear clutter, so they are omitted there (§3.5, open decision).
  args <- list(scales = paste0("s", 1:8), angles = as.numeric(octants()),
    measures = "M1")
  scaled <- do.call(ssm_sem_syntax, c(args, model = "scaled"))
  strict <- do.call(ssm_sem_syntax, c(args, model = "strict"))
  expect_false(grepl("cov_e\\w*\\s*:=", scaled))
  expect_true(grepl("cov_e\\w*\\s*:=", strict))
  # No measure => nothing to inspect, even under strict.
  strict_nom <- ssm_sem_syntax(scales = paste0("s", 1:8),
    angles = as.numeric(octants()), model = "strict")
  expect_false(grepl("cov_e\\w*\\s*:=", strict_nom))
  # include_defined = FALSE suppresses the auto emission.
  strict_off <- do.call(ssm_sem_syntax,
    c(args, model = "strict", include_defined = FALSE))
  expect_false(grepl("cov_e\\w*\\s*:=", strict_off))
  # Forcing the lines under scaled is refused rather than emitting nonlinear junk.
  expect_error(
    do.call(ssm_sem_syntax, c(args, model = "scaled", include_defined = TRUE)),
    "strict"
  )
})

test_that("measures enter as observed variables covarying with the factors (§3.3)", {
  syn <- ssm_sem_syntax(
    scales = paste0("s", 1:8), angles = as.numeric(octants()), measures = "PANAS"
  )
  expect_true(grepl("PANAS", syn))
})

test_that("ssm_sem_syntax() validates inputs and gates model size (§3.4)", {
  # Neither instrument nor scales/angles supplied.
  expect_error(ssm_sem_syntax(), "instrument|scales|angles")
  # Length mismatch.
  expect_error(
    ssm_sem_syntax(scales = c("a", "b", "c"), angles = c(0, 90)),
    "length|match"
  )
  # Scaled-model identification gate boundary (with the g-plane covariances
  # fixed per the T3 amendment, the scaled tier frees 3p, so the counting
  # gate moves to p >= 5): p = 4 is refused, p = 5 (just-identified, df = 0)
  # is emitted. Pins the boundary on BOTH sides so a future miscount of
  # sem_free_params() cannot ship silently.
  expect_error(
    ssm_sem_syntax(scales = paste0("s", 1:4), angles = c(0, 90, 180, 270),
      model = "scaled"),
    "identif|too few|at least|p ="
  )
  syn5 <- ssm_sem_syntax(
    scales = paste0("s", 1:5), angles = c(0, 72, 144, 216, 288),
    model = "scaled"
  )
  expect_type(syn5, "character")
})

# lavaan gate / graceful degradation (spec §7.1/§7.4) -------------------------

test_that("require_lavaan() errors clearly when lavaan is absent (§7.4)", {
  testthat::local_mocked_bindings(has_lavaan = function() FALSE)
  expect_error(require_lavaan(), "lavaan")
  expect_error(require_lavaan(), "install\\.packages")
})

test_that("require_lavaan() is silent when lavaan is present (§7.4)", {
  testthat::local_mocked_bindings(has_lavaan = function() TRUE)
  expect_silent(require_lavaan())
})

# Fit-under-lavaan on a reference instrument (spec §3.5 acceptance) ------------

test_that("emitted syntax fits under DEFAULT lavaan, both tiers, with/without a measure", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  scales <- names(jz2017)[2:9] # PA..NO octant scales
  angles <- as.numeric(octants())

  for (m in c("scaled", "strict")) {
    syn <- ssm_sem_syntax(scales = scales, angles = angles, model = m)
    # Default cfa() settings on purpose: the syntax must be self-identifying,
    # not silently mis-fit by lavaan's auto.fix.first marker rule.
    # suppressWarnings: a theoretically-constrained fixed-angle model on real
    # data can raise Heywood post-checks (negative residual variances) -- a
    # property of the data-model fit, not of the emitted syntax.
    fit <- suppressWarnings(lavaan::cfa(syn, data = jz2017))
    expect_true(lavaan::lavInspect(fit, "converged"))
    expect_gt(as.numeric(lavaan::fitMeasures(fit, "df")), 0)

    # With an external measure (a numeric non-circumplex scale as a stand-in).
    syn_m <- ssm_sem_syntax(scales = scales, angles = angles, model = m,
      measures = "PARPD")
    fit_m <- suppressWarnings(lavaan::cfa(syn_m, data = jz2017))
    expect_true(lavaan::lavInspect(fit_m, "converged"))
  }
})

test_that("fitted model is the INTENDED one: df matches the free-param count (§3.4)", {
  skip_on_cran()
  # Directly guards against lavaan's auto.fix.first silently fixing loadings
  # (which yields a different, wrong model with a larger df).
  skip_if_not_installed("lavaan")
  scales <- names(jz2017)[2:9]
  angles <- as.numeric(octants())
  p <- length(scales)
  moments <- p * (p + 1) / 2

  for (m in c("scaled", "strict")) {
    syn <- ssm_sem_syntax(scales = scales, angles = angles, model = m)
    fit <- suppressWarnings(lavaan::cfa(syn, data = jz2017))
    df <- as.numeric(lavaan::fitMeasures(fit, "df"))
    expect_equal(df, moments - sem_free_params(m, p, 0L))
  }
})

test_that("scaled fit keeps every scale's angle fixed at its theoretical value", {
  skip_on_cran()
  # The plane loadings (lx_i, ly_i) must lie on the ray at theta_i: the direction
  # is fixed even though the saturation is free. A wrong (auto.fix.first) model
  # would fail this.
  skip_if_not_installed("lavaan")
  scales <- names(jz2017)[2:9]
  angles <- as.numeric(octants())
  syn <- ssm_sem_syntax(scales = scales, angles = angles, model = "scaled")
  fit <- suppressWarnings(lavaan::cfa(syn, data = jz2017))
  est <- lavaan::lavInspect(fit, "est")$lambda # rows = scales, cols = g, cx, cy
  for (i in seq_along(scales)) {
    lx <- est[i, "cx"]
    ly <- est[i, "cy"]
    # atan2(ly, lx) equals theta_i modulo pi (sign = the plane reflection).
    got <- atan2(ly, lx) %% pi
    want <- (angles[[i]] * pi / 180) %% pi
    expect_equal(min(abs(got - want), abs(got - want - pi), abs(got - want + pi)),
      0, tolerance = 1e-6)
  }
})

# Multi-group emission (spec §3.5 / §6.2, T4) ---------------------------------

# Pre-change single-group output, captured verbatim from the generator BEFORE
# the multi-group layer was added. n_groups = 1 must remain byte-identical to
# these strings forever (the hard requirement): a future refactor of the
# multi-group branch that perturbs the single-group path is caught here.
exp_scaled_nomeas <- "# circumplex SSM measurement model (generated by ssm_sem_syntax())\n# scales: s1, s2, s3, s4, s5, s6, s7, s8\n# angles (degrees): 90, 135, 180, 225, 270, 315, 360, 45\n# model tier: scaled\n\n# general factor: free per-scale saturations\ng =~ NA*s1 + a1*s1 + a2*s2 + a3*s3 + a4*s4 + a5*s5 + a6*s6 + a7*s7 + a8*s8\n# circumplex plane: loadings free but with each scale's angle fixed\ncx =~ NA*s1 + lx1*s1 + lx2*s2 + lx3*s3 + lx4*s4 + lx5*s5 + lx6*s6 + lx7*s7 + lx8*s8\ncy =~ NA*s1 + ly1*s1 + ly2*s2 + ly3*s3 + ly4*s4 + ly5*s5 + ly6*s6 + ly7*s7 + ly8*s8\n# fixed-angle direction constraints: sin(a)*lx - cos(a)*ly == 0\n0 == 1*lx1 - 0*ly1\n0 == 0.70710678118654757*lx2 - -0.70710678118654746*ly2\n0 == 0*lx3 - -1*ly3\n0 == -0.70710678118654746*lx4 - -0.70710678118654768*ly4\n0 == -1*lx5 - 0*ly5\n0 == -0.70710678118654768*lx6 - 0.70710678118654735*ly6\n0 == 0*lx7 - 1*ly7\n0 == 0.70710678118654746*lx8 - 0.70710678118654757*ly8\n# isotropic orthonormal plane metric (plane scale absorbed by loadings)\ng ~~ 1*g\ncx ~~ 1*cx\ncy ~~ 1*cy\ncx ~~ 0*cy\n# general-plane covariances fixed to zero: with free per-scale\n# saturations, freeing these is locally unidentified exactly at\n# phi_g = 0 (the trade a_i +/- d*c_i*cos/sin(angle_i) <-> phi_g is\n# first-order flat there), so they cannot be estimated. To model a\n# general factor leaning into the plane, use the strict tier, whose\n# fixed loadings leave the full factor covariance matrix free.\ng ~~ 0*cx\ng ~~ 0*cy\n\n# NOTE: amplitude (a) and displacement (d) are deliberately NOT defined\n# here. They are nonlinear (sqrt / atan2) and their intervals must be\n# built in-package through circular quantiles, never via lavaan := or\n# delta-method CIs (which ignore the angular branch cut)."

exp_scaled_meas <- "# circumplex SSM measurement model (generated by ssm_sem_syntax())\n# scales: s1, s2, s3, s4, s5, s6, s7, s8\n# angles (degrees): 90, 135, 180, 225, 270, 315, 360, 45\n# model tier: scaled\n\n# general factor: free per-scale saturations\ng =~ NA*s1 + a1*s1 + a2*s2 + a3*s3 + a4*s4 + a5*s5 + a6*s6 + a7*s7 + a8*s8\n# circumplex plane: loadings free but with each scale's angle fixed\ncx =~ NA*s1 + lx1*s1 + lx2*s2 + lx3*s3 + lx4*s4 + lx5*s5 + lx6*s6 + lx7*s7 + lx8*s8\ncy =~ NA*s1 + ly1*s1 + ly2*s2 + ly3*s3 + ly4*s4 + ly5*s5 + ly6*s6 + ly7*s7 + ly8*s8\n# fixed-angle direction constraints: sin(a)*lx - cos(a)*ly == 0\n0 == 1*lx1 - 0*ly1\n0 == 0.70710678118654757*lx2 - -0.70710678118654746*ly2\n0 == 0*lx3 - -1*ly3\n0 == -0.70710678118654746*lx4 - -0.70710678118654768*ly4\n0 == -1*lx5 - 0*ly5\n0 == -0.70710678118654768*lx6 - 0.70710678118654735*ly6\n0 == 0*lx7 - 1*ly7\n0 == 0.70710678118654746*lx8 - 0.70710678118654757*ly8\n# isotropic orthonormal plane metric (plane scale absorbed by loadings)\ng ~~ 1*g\ncx ~~ 1*cx\ncy ~~ 1*cy\ncx ~~ 0*cy\n# general-plane covariances fixed to zero: with free per-scale\n# saturations, freeing these is locally unidentified exactly at\n# phi_g = 0 (the trade a_i +/- d*c_i*cos/sin(angle_i) <-> phi_g is\n# first-order flat there), so they cannot be estimated. To model a\n# general factor leaning into the plane, use the strict tier, whose\n# fixed loadings leave the full factor covariance matrix free.\ng ~~ 0*cx\ng ~~ 0*cy\n\n# external measure(s): related to circumplex factors\nM1 ~~ mg1*g\nM1 ~~ mcx1*cx\nM1 ~~ mcy1*cy\n\n# NOTE: amplitude (a) and displacement (d) are deliberately NOT defined\n# here. They are nonlinear (sqrt / atan2) and their intervals must be\n# built in-package through circular quantiles, never via lavaan := or\n# delta-method CIs (which ignore the angular branch cut)."

exp_strict_nomeas <- "# circumplex SSM measurement model (generated by ssm_sem_syntax())\n# scales: s1, s2, s3, s4, s5, s6, s7, s8\n# angles (degrees): 90, 135, 180, 225, 270, 315, 360, 45\n# model tier: strict\n\n# fixed unit-cosine loadings; free 3x3 factor covariance\ng =~ 1*s1 + 1*s2 + 1*s3 + 1*s4 + 1*s5 + 1*s6 + 1*s7 + 1*s8\ncx =~ 0*s1 + -0.70710678118654746*s2 + -1*s3 + -0.70710678118654768*s4 + 0*s5 + 0.70710678118654735*s6 + 1*s7 + 0.70710678118654757*s8\ncy =~ 1*s1 + 0.70710678118654757*s2 + 0*s3 + -0.70710678118654746*s4 + -1*s5 + -0.70710678118654768*s6 + 0*s7 + 0.70710678118654746*s8\ng ~~ NA*g\ncx ~~ NA*cx\ncy ~~ NA*cy\ng ~~ cx\ng ~~ cy\ncx ~~ cy\n\n# NOTE: amplitude (a) and displacement (d) are deliberately NOT defined\n# here. They are nonlinear (sqrt / atan2) and their intervals must be\n# built in-package through circular quantiles, never via lavaan := or\n# delta-method CIs (which ignore the angular branch cut)."

exp_strict_meas <- "# circumplex SSM measurement model (generated by ssm_sem_syntax())\n# scales: s1, s2, s3, s4, s5, s6, s7, s8\n# angles (degrees): 90, 135, 180, 225, 270, 315, 360, 45\n# model tier: strict\n\n# fixed unit-cosine loadings; free 3x3 factor covariance\ng =~ 1*s1 + 1*s2 + 1*s3 + 1*s4 + 1*s5 + 1*s6 + 1*s7 + 1*s8\ncx =~ 0*s1 + -0.70710678118654746*s2 + -1*s3 + -0.70710678118654768*s4 + 0*s5 + 0.70710678118654735*s6 + 1*s7 + 0.70710678118654757*s8\ncy =~ 1*s1 + 0.70710678118654757*s2 + 0*s3 + -0.70710678118654746*s4 + -1*s5 + -0.70710678118654768*s6 + 0*s7 + 0.70710678118654746*s8\ng ~~ NA*g\ncx ~~ NA*cx\ncy ~~ NA*cy\ng ~~ cx\ng ~~ cy\ncx ~~ cy\n\n# external measure(s): related to circumplex factors\nM1 ~~ mg1*g\nM1 ~~ mcx1*cx\nM1 ~~ mcy1*cy\n\n# Inspection only (covariance metric): the OLS projection of each\n# measure's model-implied covariance profile. Under the strict tier the\n# fixed loadings make this equal the factor covariances. These are NOT\n# the reported latent SSM parameters (which transform the\n# correlation-metric profile), and their delta SEs are lavaan\n# approximations; the reported estimates and intervals come from the\n# circumplex package.\ncov_e1 := mg1\ncov_x1 := mcx1\ncov_y1 := mcy1\n\n# NOTE: amplitude (a) and displacement (d) are deliberately NOT defined\n# here. They are nonlinear (sqrt / atan2) and their intervals must be\n# built in-package through circular quantiles, never via lavaan := or\n# delta-method CIs (which ignore the angular branch cut)."

test_that("n_groups = 1 emission is byte-identical to the pre-change output (§3.5)", {
  octs <- as.numeric(octants())
  scales <- paste0("s", 1:8)
  strip <- function(x) {
    attributes(x) <- NULL
    x
  }
  expect_identical(
    strip(ssm_sem_syntax(scales = scales, angles = octs, model = "scaled")),
    exp_scaled_nomeas
  )
  expect_identical(
    strip(ssm_sem_syntax(scales = scales, angles = octs, model = "scaled",
      measures = "M1")),
    exp_scaled_meas
  )
  expect_identical(
    strip(ssm_sem_syntax(scales = scales, angles = octs, model = "strict")),
    exp_strict_nomeas
  )
  expect_identical(
    strip(ssm_sem_syntax(scales = scales, angles = octs, model = "strict",
      measures = "M1")),
    exp_strict_meas
  )
  # The single-group default carries n_groups but no invariance attribute.
  syn <- ssm_sem_syntax(scales = scales, angles = octs)
  expect_identical(attr(syn, "n_groups"), 1L)
  expect_null(attr(syn, "invariance"))
})

test_that("invariance supplied with n_groups = 1 errors clearly (§6.2)", {
  scales <- paste0("s", 1:8)
  octs <- as.numeric(octants())
  expect_error(
    ssm_sem_syntax(scales = scales, angles = octs, invariance = "metric"),
    "invariance"
  )
  # Even supplying the default value explicitly is refused (it must be left off).
  expect_error(
    ssm_sem_syntax(scales = scales, angles = octs, invariance = "configural"),
    "multi-group"
  )
})

test_that("n_groups validation refuses non-positive-whole-number inputs (§3.5)", {
  scales <- paste0("s", 1:8)
  octs <- as.numeric(octants())
  expect_error(ssm_sem_syntax(scales = scales, angles = octs, n_groups = 0))
  expect_error(ssm_sem_syntax(scales = scales, angles = octs, n_groups = 2.5))
  expect_error(ssm_sem_syntax(scales = scales, angles = octs, n_groups = c(2, 3)))
})

test_that("multi-group emission carries n_groups and invariance attributes (§3.5)", {
  scales <- paste0("s", 1:8)
  octs <- as.numeric(octants())
  for (rr in c("configural", "metric", "scalar", "strict_residuals")) {
    syn <- ssm_sem_syntax(scales = scales, angles = octs, n_groups = 2,
      invariance = rr)
    expect_identical(attr(syn, "n_groups"), 2L)
    expect_identical(attr(syn, "invariance"), rr)
    # Mean structure is always emitted for G >= 2.
    expect_true(grepl("mean structure", syn))
    # g-plane covariances stay fixed to 0 in every group at every scaled rung.
    expect_true(grepl("g ~~ c(0,0)*cx", syn, fixed = TRUE))
    expect_true(grepl("g ~~ c(0,0)*cy", syn, fixed = TRUE))
  }
})

test_that("strict tier accepts a vacuous metric rung with an explanatory note (§6.2)", {
  scales <- paste0("s", 1:8)
  octs <- as.numeric(octants())
  syn <- ssm_sem_syntax(scales = scales, angles = octs, model = "strict",
    n_groups = 2, invariance = "metric")
  expect_true(grepl("metric rung is vacuous", syn))
})

# Multi-group fitting under lavaan (spec §3.5 / §6.2 acceptance) ---------------

# Build a two-group, p = 8 octant dataset from two sem_pop() populations with
# different scale saturations and factor-measure covariances (so configural
# genuinely differs across groups) plus a group-B mean shift (so scalar/mean
# structure is exercised). One column m1 lets the m = 0 fits (which omit
# measures) and the m = 1 fits share a single dataset.
sem_two_group_data <- function(n = 400, seed = 20260707) {
  octs <- as.numeric(octants())
  p <- 8L
  mk <- function(a, cc, sm) {
    sem_pop(
      a = rep(a, p), cc = rep(cc, p), theta = rep(0.5, p),
      angles_deg = octs, sigma_m = matrix(sm, 3, 1), v_m = 1,
      scales = paste0("s", seq_len(p)), measures = "m1"
    )
  }
  pop1 <- mk(0.60, 0.50, c(0.30, 0.40, 0.20))
  pop2 <- mk(0.75, 0.55, c(0.20, 0.30, 0.35))
  rmvn <- function(n, mu, sigma) {
    l <- chol(sigma)
    sweep(matrix(stats::rnorm(n * ncol(sigma)), n) %*% l, 2, mu, "+")
  }
  set.seed(seed)
  q <- nrow(pop1$sigma)
  d1 <- as.data.frame(rmvn(n, rep(0, q), pop1$sigma))
  d2 <- as.data.frame(rmvn(n, rep(0.4, q), pop2$sigma))
  colnames(d1) <- colnames(d2) <- rownames(pop1$sigma)
  d1$grp <- "A"
  d2$grp <- "B"
  rbind(d1, d2)
}

test_that("multi-group syntax fits and converges under default cfa, both tiers x every rung", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  dat <- sem_two_group_data()
  octs <- as.numeric(octants())
  scales <- paste0("s", 1:8)
  rungs <- c("configural", "metric", "scalar", "strict_residuals")
  for (mdl in c("scaled", "strict")) {
    for (meas in list(NULL, "m1")) {
      for (rr in rungs) {
        syn <- ssm_sem_syntax(scales = scales, angles = octs, measures = meas,
          model = mdl, n_groups = 2, invariance = rr)
        fit <- suppressWarnings(lavaan::cfa(syn, data = dat, group = "grp"))
        expect_true(lavaan::lavInspect(fit, "converged"),
          label = sprintf("%s / %s / m=%d converged", mdl, rr, length(meas)))
      }
    }
  }
})

test_that("shared parameters use repeated-vector labels: no lavaan single-label warning", {
  skip_on_cran()
  # Cross-group equality is stated explicitly as c(a1,a1)*s1 (not a plain
  # a1*s1), so lavaan must not warn that "using a single label per parameter
  # in a multiple group setting implies imposing equality constraints".
  skip_if_not_installed("lavaan")
  dat <- sem_two_group_data()
  octs <- as.numeric(octants())
  scales <- paste0("s", 1:8)
  for (rr in c("metric", "scalar", "strict_residuals")) {
    syn <- ssm_sem_syntax(scales = scales, angles = octs, model = "scaled",
      n_groups = 2, invariance = rr)
    warns <- character(0)
    withCallingHandlers(
      lavaan::cfa(syn, data = dat, group = "grp"),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_false(any(grepl("single label", warns)),
      label = sprintf("no single-label warning at the %s rung", rr))
  }
})

test_that("multi-group df matches the free-parameter count (lavaan authoritative, §3.4/§6.2)", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  dat <- sem_two_group_data()
  octs <- as.numeric(octants())
  scales <- paste0("s", 1:8)
  p <- 8L
  rungs <- c("configural", "metric", "scalar", "strict_residuals")
  for (mdl in c("scaled", "strict")) {
    for (mm in c(0L, 1L)) {
      meas <- if (mm == 1L) "m1" else NULL
      moments <- 2 * ((p + mm) * (p + mm + 1) / 2 + (p + mm))
      for (rr in rungs) {
        syn <- ssm_sem_syntax(scales = scales, angles = octs, measures = meas,
          model = mdl, n_groups = 2, invariance = rr)
        fit <- suppressWarnings(lavaan::cfa(syn, data = dat, group = "grp"))
        got <- as.numeric(lavaan::fitMeasures(fit, "df"))
        want <- moments - sem_free_params(mdl, p, mm, 2L, rr)
        expect_equal(got, want,
          label = sprintf("%s / %s / m=%d df", mdl, rr, mm))
      }
    }
  }
})

test_that("equality structure matches the invariance contract (§6.2, parameterTable)", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  dat <- sem_two_group_data()
  octs <- as.numeric(octants())
  scales <- paste0("s", 1:8)
  fitmg <- function(rr) {
    syn <- ssm_sem_syntax(scales = scales, angles = octs, model = "scaled",
      n_groups = 2, invariance = rr)
    suppressWarnings(lavaan::cfa(syn, data = dat, group = "grp"))
  }

  # Configural: loadings free per group -> distinct estimates across groups.
  cfg <- fitmg("configural")
  ec <- lavaan::lavInspect(cfg, "est")
  cfg_diff <- max(abs(ec[[1]]$lambda[, "g"] - ec[[2]]$lambda[, "g"]))
  expect_gt(cfg_diff, 0.02)
  # g-plane covariances fixed to 0 in every group at the configural rung.
  for (gk in seq_along(ec)) {
    expect_equal(unname(ec[[gk]]$psi["g", "cx"]), 0)
    expect_equal(unname(ec[[gk]]$psi["g", "cy"]), 0)
  }

  # Metric: shared loadings -> a_i / lx_i / ly_i equal across groups (one
  # cross-group parameter each), and the shared vp_g<k> label makes the plane
  # isotropic per group (var(cx_gk) = var(cy_gk)).
  met <- fitmg("metric")
  em <- lavaan::lavInspect(met, "est")
  for (fac in c("g", "cx", "cy")) {
    expect_equal(unname(em[[1]]$lambda[, fac]), unname(em[[2]]$lambda[, fac]))
  }
  expect_equal(unname(em[[2]]$psi["cx", "cx"]), unname(em[[2]]$psi["cy", "cy"]))
  # Reference group's factor metric fixed at 1.
  expect_equal(unname(em[[1]]$psi["g", "g"]), 1)
  expect_equal(unname(em[[1]]$psi["cx", "cx"]), 1)
  # g-plane covariances still fixed 0 in every group at the metric rung.
  for (gk in seq_along(em)) {
    expect_equal(unname(em[[gk]]$psi["g", "cx"]), 0)
    expect_equal(unname(em[[gk]]$psi["g", "cy"]), 0)
  }
  # The shared metric label appears in both groups' parameter rows.
  ptm <- lavaan::parameterTable(met)
  a1 <- ptm[ptm$op == "=~" & ptm$lhs == "g" & ptm$rhs == "s1" &
    nzchar(ptm$label), ]
  expect_true(all(a1$label == "a1"))

  # Scalar: scale intercepts shared (nu_i), latent means freed in group 2 only.
  scl <- fitmg("scalar")
  es <- lavaan::lavInspect(scl, "est")
  expect_equal(unname(es[[1]]$alpha["g", 1]), 0)
  expect_gt(abs(es[[2]]$alpha["g", 1]), 1e-6)
  pts <- lavaan::parameterTable(scl)
  nu1 <- pts[pts$op == "~1" & pts$lhs == "s1" & nzchar(pts$label), ]
  expect_true(all(nu1$label == "nu1"))
})

test_that("fitted configural solution keeps every angle fixed per group (§5.5)", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  dat <- sem_two_group_data()
  octs <- as.numeric(octants())
  scales <- paste0("s", 1:8)
  syn <- ssm_sem_syntax(scales = scales, angles = octs, model = "scaled",
    n_groups = 2, invariance = "configural")
  fit <- suppressWarnings(lavaan::cfa(syn, data = dat, group = "grp"))
  est <- lavaan::lavInspect(fit, "est")
  for (gk in seq_along(est)) {
    lam <- est[[gk]]$lambda
    for (i in seq_along(scales)) {
      got <- atan2(lam[i, "cy"], lam[i, "cx"]) %% pi
      want <- (octs[[i]] * pi / 180) %% pi
      expect_equal(
        min(abs(got - want), abs(got - want - pi), abs(got - want + pi)),
        0,
        tolerance = 1e-6,
        label = sprintf("group %d scale %d direction", gk, i)
      )
    }
  }
})
