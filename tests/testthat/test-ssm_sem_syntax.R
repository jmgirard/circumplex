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
