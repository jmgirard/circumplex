# T3 (M5): latent-variable SSM estimation + circular-aware CIs.
# Traces to devel/m5-sem-design.md sections noted per block.
#
# Oracle rule (Brief A sec. 6.1): every expected value below is either computed
# in-test from a constructed population (closed-form truth, spec sec. 8.1) or
# checked against the package's own already-validated estimators
# (ssm_parameters() at equally spaced angles); no external numbers.

# Helpers -----------------------------------------------------------------------
# sem_pop() and sem_pop_fit() live in helper-ssm-sem.R (shared with the
# devel/m5-coverage-oracle.R harness so the truth algebra has one copy).

# CCW circular-arc membership in degrees (thin degree-space wrapper over the
# package's canonical ssm_ci_d_cover(), so the test convention cannot drift
# from the shipped one)
angle_covered <- function(lci, uci, value) {
  isTRUE(ssm_ci_d_cover(
    value * pi / 180, lci * pi / 180, uci * pi / 180
  )$cover)
}

oct <- as.numeric(octants())
oct_scales <- paste0("s", 1:8)

# The SSM transform (spec sec. 2/8.2) -------------------------------------------

test_that("sem transform equals ssm_parameters() at equally spaced angles (sec. 8.2)", {
  profile <- c(0.55, 0.58, 0.62, 0.76, 1.21, 1.21, 1.48, 0.90)
  W <- sem_ols_weights(oct * pi / 180, names = oct_scales)
  got <- sem_ssm_transform(profile, W, oct * pi / 180)
  ref <- suppressWarnings(ssm_parameters(profile, oct))
  expect_equal(got[["e"]], ref$Elev, tolerance = 1e-12)
  expect_equal(got[["x"]], ref$Xval, tolerance = 1e-12)
  expect_equal(got[["y"]], ref$Yval, tolerance = 1e-12)
  expect_equal(got[["a"]], ref$Ampl, tolerance = 1e-12)
  expect_equal(got[["d"]] * 180 / pi, as.numeric(ref$Disp), tolerance = 1e-10)
  expect_equal(got[["fit"]], ref$Fit, tolerance = 1e-12)
})

test_that("sem transform recovers an exact cosine profile at balance-violating angles; closed form does not (sec. 5.5)", {
  th_deg <- c(0, 30, 90, 200, 290) # violates harmonic balance (T2 fixture)
  th <- th_deg * pi / 180
  E <- 0.4
  A <- 0.3
  D <- 310 * pi / 180
  profile <- E + A * cos(th - D)
  W <- sem_ols_weights(th, names = paste0("s", 1:5))
  got <- sem_ssm_transform(profile, W, th)
  expect_equal(got[["e"]], E, tolerance = 1e-12)
  expect_equal(got[["a"]], A, tolerance = 1e-12)
  expect_equal(got[["d"]], D, tolerance = 1e-12)
  expect_equal(got[["fit"]], 1, tolerance = 1e-12)
  # The closed form is not a projection here and misses the constructed truth
  ref <- suppressWarnings(ssm_parameters(profile, th_deg))
  expect_gt(abs(ref$Ampl - A), 0.01)
})

test_that("sem transform inherits the degenerate-NA semantics (sec. 5.5)", {
  W <- sem_ols_weights(oct * pi / 180, names = oct_scales)
  # Flat profile: displacement and fit undefined
  flat <- sem_ssm_transform(rep(0.3, 8), W, oct * pi / 180)
  expect_true(is.na(flat[["d"]]))
  expect_true(is.na(flat[["fit"]]))
  # Pure second harmonic: real variance, zero first-harmonic amplitude
  second <- sem_ssm_transform(cos(2 * oct * pi / 180), W, oct * pi / 180)
  expect_true(is.na(second[["d"]]))
  expect_equal(second[["fit"]], 0)
  expect_lt(second[["a"]], 1e-12)
})

test_that("vectorized sem transform matches the scalar reference row-by-row (M9)", {
  # The scalar sem_ssm_transform() is the validated reference (tested above vs
  # ssm_parameters()); the matrix pass used in sem_estimate() must reproduce it
  # row for row, including the sec. 5.5 degenerate-NA semantics.
  W <- sem_ols_weights(oct * pi / 180, names = oct_scales)
  th <- oct * pi / 180
  set.seed(1)
  P <- rbind(
    c(0.55, 0.58, 0.62, 0.76, 1.21, 1.21, 1.48, 0.90), # normal
    rep(0.3, 8), # flat -> displacement and fit NA
    cos(2 * oct * pi / 180), # zero first-harmonic amplitude -> d NA, fit 0
    matrix(stats::rnorm(8 * 20, 0.5, 0.2), ncol = 8) # random interior rows
  )
  got <- sem_ssm_transform_mat(P, W, th)
  ref <- t(apply(P, 1, sem_ssm_transform, weights = W, angles_rad = th))
  expect_equal(colnames(got), c("e", "x", "y", "a", "d", "fit"))
  expect_equal(unname(got), unname(ref), tolerance = 1e-12)
  expect_true(is.na(got[2, "d"]) && is.na(got[2, "fit"])) # flat row
  expect_true(is.na(got[3, "d"]) && got[3, "fit"] == 0) # zero-amplitude row
})

# Analytic-truth recovery (spec sec. 4.1/8.1) ------------------------------------

test_that("latent SSM parameters recover the closed-form truth from population moments (sec. 8.1)", {
  skip_if_not_installed("lavaan")
  # Interior cell: a* comfortably positive, d* mid-quadrant, heterogeneous
  # saturations and residuals so nothing collapses to the observed profile.
  a <- seq(0.5, 0.8, length.out = 8)
  cc <- seq(0.7, 0.5, length.out = 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  delta <- 60 * pi / 180
  sigma_m <- cbind(c(0.2, 0.4 * cos(delta), 0.4 * sin(delta)))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)
  fit <- sem_pop_fit(pop)

  res <- ssm_sem_parameters(
    fit,
    scales = pop$scales, angles = oct, measures = pop$measures,
    boots = 50
  )
  expect_s3_class(res, "circumplex_ssm_sem")
  expect_s3_class(res, "circumplex_ssm")

  # Truth: the package's own validated estimator applied to rho0 (equally
  # spaced angles, so the shared functional is exact).
  truth <- ssm_parameters(as.numeric(pop$rho0), oct)
  r <- res$results
  expect_equal(r$e_est, truth$Elev, tolerance = 1e-4)
  expect_equal(r$x_est, truth$Xval, tolerance = 1e-4)
  expect_equal(r$y_est, truth$Yval, tolerance = 1e-4)
  expect_equal(r$a_est, truth$Ampl, tolerance = 1e-4)
  expect_equal(as.numeric(r$d_est), as.numeric(truth$Disp), tolerance = 1e-2)
  expect_equal(r$fit_est, truth$Fit, tolerance = 1e-4)

  # The latent profile is stored as the scores
  expect_equal(
    as.numeric(res$scores[1, pop$scales]), as.numeric(pop$rho0),
    tolerance = 1e-4
  )
})

test_that("a general factor leaning into the plane is recovered by the strict tier and lowers latent fit (sec. 4.2)", {
  skip_if_not_installed("lavaan")
  # g-plane lean is expressible only under the strict tier (fixed unit-cosine
  # loadings, fully free factor covariance): under the scaled tier, free
  # g-plane covariances are locally unidentified at zero (the T3 finding
  # recorded in spec sec. 12.3), so they are fixed there.
  a <- rep(1, 8)
  cc <- rep(1, 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  sigma_m <- cbind(c(0.3, 0.35, 0.15))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1, phi_gx = 0.4, phi_gy = 0.2)
  fit <- sem_pop_fit(pop, model = "strict")
  res <- ssm_sem_parameters(
    fit,
    scales = pop$scales, angles = oct, measures = pop$measures, boots = 50
  )
  truth <- ssm_parameters(as.numeric(pop$rho0), oct)
  expect_equal(res$results$fit_est, truth$Fit, tolerance = 1e-4)
  expect_lt(res$results$fit_est, 1 - 1e-4)
  expect_identical(res$model$tier, "strict")
})

# Theta -> 0 population-level equivalence ladder (spec sec. 8.2) -----------------

test_that("as residual variance -> 0 the latent profile converges to the observed profile (sec. 8.2)", {
  skip_if_not_installed("lavaan")
  a <- rep(0.5, 8)
  cc <- rep(0.65, 8)
  delta <- 20 * pi / 180
  sigma_m <- cbind(c(0.25, 0.45 * cos(delta), 0.45 * sin(delta)))
  for (theta_val in c(0.5, 0.1, 0.02)) {
    pop <- sem_pop(a, cc, theta_val * seq(0.75, 1.25, length.out = 8), oct, sigma_m, v_m = 1)
    fit <- sem_pop_fit(pop)
    res <- suppressWarnings(ssm_sem_parameters(
      fit,
      scales = pop$scales, angles = oct, measures = pop$measures, boots = 50
    ))
    # Observed-profile functional on the same population moments
    obs <- pop$sigma[pop$scales, pop$measures] /
      sqrt(diag(pop$sigma)[pop$scales] * pop$sigma[pop$measures, pop$measures])
    diff <- max(abs(as.numeric(res$scores[1, pop$scales]) - as.numeric(obs)))
    # Tolerance schedule shrinking with Theta: disattenuation scales with the
    # residual share, so the gap must vanish along the ladder.
    expect_lt(diff, theta_val)
  }
})

# Boundary suite (spec sec. 5.5) -------------------------------------------------

test_that("latent displacement at the 0/360 pole is reported on the package convention with a contiguous straddling CI (sec. 5.5)", {
  skip_if_not_installed("lavaan")
  a <- rep(0.55, 8)
  cc <- rep(0.6, 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  # Measure aligned exactly with the +x axis: d* = 0 (equivalently 360)
  sigma_m <- cbind(c(0.15, 0.45, 0))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)
  fit <- sem_pop_fit(pop, n = 500) # modest n so the CI has real width
  set.seed(20260707)
  res <- ssm_sem_parameters(
    fit,
    scales = pop$scales, angles = oct, measures = pop$measures, boots = 500
  )
  r <- res$results
  d <- as.numeric(r$d_est)
  expect_gte(d, 0)
  expect_lt(d, 360 + 1e-9)
  expect_lt(min(d, 360 - d), 1) # at the pole
  # The circular CI straddles the pole contiguously and contains the estimate
  lci <- as.numeric(r$d_lci)
  uci <- as.numeric(r$d_uci)
  arc <- (uci - lci) %% 360
  expect_lt(arc, 90)
  expect_true(angle_covered(lci, uci, d %% 360))
  expect_true(angle_covered(lci, uci, 0)) # the pole itself is inside
})

test_that("latent amplitude near zero degrades gracefully: guardrail path, no crash (sec. 5.5)", {
  skip_if_not_installed("lavaan")
  a <- rep(0.55, 8)
  cc <- rep(0.6, 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  # Measure orthogonal to the plane: pure elevation, a* = 0, d* undefined
  sigma_m <- cbind(c(0.4, 0, 0))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)
  fit <- sem_pop_fit(pop, n = 400)
  set.seed(20260707)
  res <- suppressWarnings(ssm_sem_parameters(
    fit,
    scales = pop$scales, angles = oct, measures = pop$measures, boots = 500
  ))
  r <- res$results
  expect_lt(r$a_est, 0.05)
  expect_gt(r$e_est, 0.2)
  # The amplitude replicates are folded (all positive), so at true a* = 0 the
  # percentile lower bound sits at a small positive value and the shipped
  # certification rule (a_lci > 0 at print precision) can still certify --
  # the same operating characteristic the observed path has, measured and
  # recorded in the B-spec (sec. 12.5) and ported unchanged (M5 spec sec. 5.2,
  # review F12). The honest boundary signal is the direction itself: the
  # displacement draws are near-uniform, so the circular CI covers most of
  # the circle rather than pretending precision.
  expect_gt(r$a_uci, r$a_est) # interval machinery intact
  arc <- (as.numeric(r$d_uci) - as.numeric(r$d_lci)) %% 360
  expect_gt(arc, 180)
})

test_that("a flat latent profile (measure independent of everything) degrades like the observed path (sec. 5.5)", {
  skip_if_not_installed("lavaan")
  a <- rep(0.55, 8)
  cc <- rep(0.6, 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  sigma_m <- cbind(c(0, 0, 0))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)
  fit <- sem_pop_fit(pop, n = 400)
  set.seed(20260707)
  res <- suppressWarnings(ssm_sem_parameters(
    fit,
    scales = pop$scales, angles = oct, measures = pop$measures, boots = 200
  ))
  r <- res$results
  expect_lt(abs(r$e_est), 0.05)
  expect_lt(r$a_est, 0.05)
  # At the fitted optimum the profile is optimizer-noise scale, not exactly
  # flat (the scale-relative tolerance correctly declines to NA it -- the
  # observed path behaves identically for an independent measure at finite
  # n), so the honest degradation signal is the direction: near-uniform
  # displacement draws must yield a circular CI covering most of the circle.
  arc <- (as.numeric(r$d_uci) - as.numeric(r$d_lci)) %% 360
  expect_gt(arc, 180)
  expect_true(all(is.finite(c(r$e_lci, r$e_uci, r$a_uci))))
})

test_that("two-measure latent contrast near +/-180 stays on the estimate's branch (sec. 5.5/6.4)", {
  skip_if_not_installed("lavaan")
  a <- rep(0.55, 8)
  cc <- rep(0.6, 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  d1 <- 5 * pi / 180
  d2 <- 186 * pi / 180
  sigma_m <- cbind(
    c(0.15, 0.4 * cos(d1), 0.4 * sin(d1)),
    c(0.15, 0.4 * cos(d2), 0.4 * sin(d2))
  )
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = c(1, 1))
  fit <- sem_pop_fit(pop, n = 500)
  set.seed(20260707)
  res <- ssm_sem_parameters(
    fit,
    scales = pop$scales, angles = oct, measures = pop$measures,
    boots = 500, contrast = TRUE
  )
  r <- res$results
  expect_equal(nrow(r), 3) # m1, m2, contrast
  dc <- as.numeric(r$d_est[3])
  # Second minus first in (-180, 180]: 186 - 5 = 181 -> -179
  expect_gt(dc, -180)
  expect_lte(dc, 180)
  expect_equal(abs(dc), 179, tolerance = 1.5)
  # Branch alignment: the estimate lies numerically inside its own interval
  # (endpoints may legitimately exceed +/-180 on the estimate's branch)
  expect_gte(dc, as.numeric(r$d_lci[3]))
  expect_lte(dc, as.numeric(r$d_uci[3]))
  # Method audit: the inherited contrast plot renders the latent contrast
  expect_s3_class(ssm_plot_contrast(res), "ggplot")
})

# Guards (spec sec. 4.5) ----------------------------------------------------------

test_that("a disattenuated point correlation at/above 1 is refused with the scale named (sec. 4.5)", {
  skip_if_not_installed("lavaan")
  a <- rep(0.55, 8)
  cc <- rep(0.6, 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  # M is the common part t_1 of scale 1 (sigma_m = Phi lambda_1). Shrinking
  # Var(M) below Var(t_1) drives the model-implied disattenuated rho*_1
  # comfortably above 1 (~1.05), so the point guard fires ROBUSTLY on every
  # platform. The earlier construction set rho*_1 == 1 exactly, which the fit
  # recovered as 1 +/- ~1e-7; whether that cleared the >= 1 - 1e-12 guard was
  # platform-dependent (the CI runners landed just under, so the draw-engine
  # escalation fired instead of the point guard) -- the M5 CI portability fix.
  th <- oct * pi / 180
  lambda1 <- c(a[1], cc[1] * cos(th[1]), cc[1] * sin(th[1]))
  pop <- sem_pop(a, cc, theta, oct, cbind(lambda1), v_m = sum(lambda1^2))
  sig <- pop$sigma
  sig[pop$measures, pop$measures] <- 0.9 * sig[pop$measures, pop$measures]
  syn <- ssm_sem_syntax(
    scales = pop$scales, angles = oct, measures = pop$measures, model = "scaled"
  )
  fit <- suppressWarnings(
    lavaan::cfa(syn, sample.cov = sig, sample.nobs = 10000)
  )
  expect_error(
    suppressWarnings(ssm_sem_parameters(
      fit,
      scales = pop$scales, angles = oct, measures = pop$measures, boots = 50
    )),
    "s1"
  )
})

test_that("inadmissible MVN draws are filtered engine-side with a cause-naming warning (sec. 4.5)", {
  skip_if_not_installed("lavaan")
  a <- rep(0.55, 8)
  cc <- rep(0.6, 8)
  theta <- seq(1.0, 1.4, length.out = 8) # heavy attenuation -> disattenuated values near 1
  delta <- 40 * pi / 180
  # rho*_max close to (but below) 1 so a small share of draws cross it
  # (calibrated: ~4% at this seed, inside the (0, 5%] warning band)
  sigma_m <- cbind(0.93 * c(0.55, 0.6 * cos(delta), 0.6 * sin(delta)) /
    sqrt(sum(c(0.55, 0.6 * cos(delta), 0.6 * sin(delta))^2)))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)
  fit <- sem_pop_fit(pop, n = 250)
  set.seed(20260707)
  expect_warning(
    ssm_sem_parameters(
      fit,
      scales = pop$scales, angles = oct, measures = pop$measures, boots = 400
    ),
    "inadmissible"
  )
})

test_that("an inadmissible-draw share above the threshold escalates to an error (sec. 4.5)", {
  skip_if_not_installed("lavaan")
  a <- rep(0.55, 8)
  cc <- rep(0.6, 8)
  theta <- seq(1.0, 1.4, length.out = 8)
  delta <- 40 * pi / 180
  dir3 <- c(0.55, 0.6 * cos(delta), 0.6 * sin(delta))
  # Deep in the near-boundary regime (~36% of draws inadmissible at this seed)
  sigma_m <- cbind(0.985 * dir3 / sqrt(sum(dir3^2)))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)
  fit <- sem_pop_fit(pop, n = 150)
  set.seed(20260707)
  expect_error(
    suppressWarnings(ssm_sem_parameters(
      fit,
      scales = pop$scales, angles = oct, measures = pop$measures, boots = 400
    )),
    "boot|model"
  )
})

# The MVN engine and the direction constraints (spec sec. 5.1) --------------------

test_that("MVN draws respect the linear fixed-angle direction constraints (sec. 5.1)", {
  skip_if_not_installed("lavaan")
  a <- rep(0.55, 8)
  cc <- rep(0.6, 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  sigma_m <- cbind(c(0.2, 0.3, 0.2))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)
  fit <- sem_pop_fit(pop, n = 300)
  psi <- lavaan::coef(fit)
  set.seed(1)
  draws <- mvn_draws(200, psi, as.matrix(lavaan::vcov(fit)))
  colnames(draws) <- names(psi)
  th <- oct * pi / 180
  for (i in seq_len(8)) {
    resid <- sin(th[i]) * draws[, paste0("lx", i)] -
      cos(th[i]) * draws[, paste0("ly", i)]
    # Solver precision: the constraint holds to ~1e-6 in lavaan's vcov; the
    # draws' loading SD is ~5e-2, so this is a relative error of ~1e-4.
    expect_lt(max(abs(resid)), 1e-4)
  }
})

test_that("mvn results are reproducible from a seed (RNG contract, sec. 5.4)", {
  skip_if_not_installed("lavaan")
  a <- rep(0.55, 8)
  cc <- rep(0.6, 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  sigma_m <- cbind(c(0.2, 0.3, 0.2))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)
  fit <- sem_pop_fit(pop, n = 300)
  set.seed(42)
  r1 <- ssm_sem_parameters(
    fit,
    scales = pop$scales, angles = oct, measures = pop$measures, boots = 200
  )
  set.seed(42)
  r2 <- ssm_sem_parameters(
    fit,
    scales = pop$scales, angles = oct, measures = pop$measures, boots = 200
  )
  expect_identical(r1$results, r2$results)
})

# Cross-implementation spot check (spec sec. 8.4) ---------------------------------

test_that("MVN propagation of a linear strict-tier := quantity matches lavaan's delta SE (sec. 8.4)", {
  skip_if_not_installed("lavaan")
  # Under the strict tier cov_e1 := mg1 is linear, so lavaan's delta SE is
  # trustworthy there and must agree with the MVN-propagation SD of the SAME
  # covariance-metric quantity within Monte Carlo error. This checks that the
  # vcov plumbing (free-parameter indexing through sem_structure()) is wired
  # correctly -- not a validation of the reported intervals, which
  # deliberately diverge for amplitude and displacement.
  a <- rep(1, 8)
  cc <- rep(1, 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  sigma_m <- cbind(c(0.3, 0.35, 0.15))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)
  syn <- ssm_sem_syntax(
    scales = pop$scales, angles = oct, measures = pop$measures,
    model = "strict", include_defined = TRUE
  )
  fit <- lavaan::cfa(syn, sample.cov = pop$sigma, sample.nobs = 500)
  pe <- lavaan::parameterEstimates(fit)
  se_delta <- pe$se[pe$lhs == "cov_e1" & pe$op == ":="]
  struct <- sem_structure(fit, pop$scales, pop$measures)
  set.seed(3)
  draws <- mvn_draws(
    4000, as.numeric(lavaan::coef(fit)), as.matrix(lavaan::vcov(fit))
  )
  # cov_e1 is the mg1 parameter itself; extract it through the same component
  # machinery the estimand map uses
  sm_draws <- sem_component(struct$sm, draws)
  expect_equal(stats::sd(sm_draws[, 1]), se_delta, tolerance = 0.05)
})

# The bootstrap engine (spec sec. 5.1) --------------------------------------------

test_that("ci_method = 'boot' refits per resample and agrees with mvn to Monte Carlo error", {
  skip_if_not_installed("lavaan")
  skip_on_cran()
  data("jz2017", envir = environment())
  set.seed(20260707)
  res_b <- suppressWarnings(ssm_sem(
    jz2017[1:400, ],
    scales = names(jz2017)[2:9], measures = "PARPD",
    ci_method = "boot", boots = 100
  ))
  set.seed(20260707)
  res_m <- ssm_sem(
    jz2017[1:400, ],
    scales = names(jz2017)[2:9], measures = "PARPD",
    ci_method = "mvn", boots = 2000
  )
  expect_equal(res_b$results$e_est, res_m$results$e_est, tolerance = 1e-8)
  # Same estimand, same point estimate; intervals comparable in scale
  width_b <- res_b$results$a_uci - res_b$results$a_lci
  width_m <- res_m$results$a_uci - res_m$results$a_lci
  expect_lt(abs(width_b - width_m) / width_m, 0.6)
  expect_identical(res_b$details$method, "boot")
})

# ssm_sem() end-to-end (spec sec. 7.2/7.3) ----------------------------------------

test_that("ssm_sem() runs end-to-end on real data and matches the observed SSM direction (sec. 1.1)", {
  skip_if_not_installed("lavaan")
  data("jz2017", envir = environment())
  scales <- names(jz2017)[2:9]
  set.seed(20260707)
  res <- ssm_sem(jz2017, scales = scales, measures = "PARPD", boots = 500)
  expect_s3_class(res, "circumplex_ssm_sem")
  expect_identical(res$details$score_type, "Latent")
  expect_identical(res$details$method, "mvn")
  expect_true(inherits(res$sem, "lavaan"))
  expect_identical(res$model$tier, "scaled")
  expect_equal(dim(res$model$weights), c(3L, 8L))
  # The mvn engine propagates the sandwich vcov by default (coverage study:
  # plain ML vcov undercovers displacement under realistic misspecification),
  # and the default estimator is MLR so the printed fit block is robust too
  # (vcov verified bit-identical between ML+robust-se and MLR)
  expect_match(lavaan::lavInspect(res$sem, "options")$se, "robust")
  # (lavaan stores MLR internally as ML + a scaled test, so pin the behavior:
  # the scaled test statistic exists and print() uses the robust block)
  expect_true("chisq.scaled" %in% names(lavaan::fitMeasures(res$sem)))
  expect_output(print(res), "robust")

  # Same construct: the latent displacement should be near the observed one
  set.seed(20260707)
  obs <- ssm_analyze(jz2017, scales = scales, measures = "PARPD", boots = 100)
  dd <- abs(as.numeric(angle_dist(
    as_radian(as_degree(as.numeric(res$results$d_est))),
    as_radian(as_degree(as.numeric(obs$results$d_est)))
  ))) * 180 / pi
  expect_lt(dd, 30)
  # Disattenuation: latent amplitude at least as large as observed here
  expect_gt(res$results$a_est, 0.9 * obs$results$a_est)
})

test_that("ssm_sem() validates its arguments (sec. 7.2)", {
  skip_if_not_installed("lavaan")
  data("jz2017", envir = environment())
  scales <- names(jz2017)[2:9]
  # No measures without grouping: the single-group mean path has no product
  # (sec. 1.3)
  expect_error(ssm_sem(jz2017, scales = scales), "measures")
  # The invariance ladder is a multi-group workflow (T4)
  expect_error(
    ssm_sem(jz2017, scales = scales, measures = "PARPD",
      invariance = "metric"),
    "grouping"
  )
  # Contrast arity: exactly two measures
  expect_error(
    ssm_sem(jz2017, scales = scales, measures = "PARPD", contrast = TRUE),
    "two"
  )
  # Angle/scale length mismatch
  expect_error(
    ssm_sem(jz2017, scales = scales[1:4], measures = "PARPD"),
    "length|angles"
  )
})

test_that("contrast arity is validated at every branch (sec. 7.2)", {
  # Characterization test for the shared contrast-arity validator: locks the
  # message on each failure branch across both entry points. The grouped
  # branches error before any lavaan fit, so raw jz2017 suffices; the
  # ssm_sem_parameters() grouped branch routes through the same validated
  # helper exercised here via ssm_sem().
  skip_if_not_installed("lavaan")
  data("jz2017", envir = environment())
  scales <- names(jz2017)[2:9]

  # Grouped contrast requires exactly two groups
  d3 <- jz2017
  d3$G3 <- factor(rep_len(c("a", "b", "c"), nrow(d3)))
  expect_error(
    ssm_sem(d3, scales = scales, grouping = "G3", measures = "PARPD",
      contrast = TRUE),
    "exactly two groups"
  )

  # Grouped measure-path contrast requires exactly one measure
  expect_error(
    ssm_sem(jz2017, scales = scales, grouping = "Gender",
      measures = c("PARPD", "SCZPD"), contrast = TRUE),
    "exactly one measure"
  )

  # User-supplied single-group fit: the ungrouped branch still needs two
  # measures (second minus first)
  th_deg <- c(0, 30, 90, 200, 290)
  a <- rep(0.55, 5)
  cc <- rep(0.6, 5)
  theta <- seq(0.3, 0.6, length.out = 5)
  sigma_m <- cbind(c(0.2, 0.3, 0.2))
  pop <- sem_pop(a, cc, theta, th_deg, sigma_m, v_m = 1)
  fit <- sem_pop_fit(pop, model = "strict")
  expect_error(
    ssm_sem_parameters(fit, scales = pop$scales, angles = th_deg,
      measures = pop$measures[1], contrast = TRUE),
    "two measures"
  )
})

test_that("ssm_sem() gates on lavaan with a clear install hint (sec. 7.4)", {
  testthat::local_mocked_bindings(has_lavaan = function() FALSE)
  expect_error(
    ssm_sem(data.frame(x = 1), scales = "x", measures = "x"),
    "install\\.packages"
  )
})

test_that("ssm_sem_parameters() refuses angles that do not match the fitted model", {
  skip_if_not_installed("lavaan")
  th_deg <- c(0, 30, 90, 200, 290)
  a <- rep(0.55, 5)
  cc <- rep(0.6, 5)
  theta <- seq(0.3, 0.6, length.out = 5)
  sigma_m <- cbind(c(0.2, 0.3, 0.2))
  pop <- sem_pop(a, cc, theta, th_deg, sigma_m, v_m = 1)
  fit <- sem_pop_fit(pop, model = "strict") # scaled is under-identified at p=5
  # Forgetting `angles =` (the octants default) must error, not silently
  # project onto the wrong cosine basis
  expect_error(
    ssm_sem_parameters(fit,
      scales = pop$scales, angles = c(90, 180, 270, 360, 45),
      measures = pop$measures, boots = 20
    ),
    "angles"
  )
  # The generating angles pass
  res <- ssm_sem_parameters(fit,
    scales = pop$scales, angles = th_deg,
    measures = pop$measures, boots = 20
  )
  expect_s3_class(res, "circumplex_ssm_sem")
})

test_that("ssm_sem_parameters() handles user-supplied multi-group fits per group (T4 escape hatch)", {
  skip_if_not_installed("lavaan")
  a <- rep(0.55, 8)
  cc <- rep(0.6, 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  sigma_m <- cbind(c(0.2, 0.3, 0.2))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)
  set.seed(5)
  dat <- as.data.frame(
    matrix(rnorm(800 * 9), 800, 9) %*% chol(pop$sigma)
  )
  colnames(dat) <- colnames(pop$sigma)
  dat$grp <- rep(c("A", "B"), each = 400)
  syn <- ssm_sem_syntax(
    scales = pop$scales, angles = oct, measures = pop$measures,
    n_groups = 2, invariance = "metric"
  )
  fit <- suppressWarnings(lavaan::cfa(syn, data = dat, group = "grp"))
  set.seed(6)
  res <- suppressWarnings(ssm_sem_parameters(fit,
    scales = pop$scales, angles = oct,
    measures = pop$measures, boots = 50
  ))
  # One profile row per lavaan group, labeled by the group labels
  expect_equal(nrow(res$results), 2)
  expect_setequal(res$results$Group, c("A", "B"))
  # Both groups were simulated from the same population: profiles agree
  expect_lt(
    max(abs(as.numeric(res$scores[1, pop$scales]) -
      as.numeric(res$scores[2, pop$scales]))), 0.15
  )
})

test_that("ssm_sem_parameters() refuses the unidentified free-g-plane scaled configuration", {
  skip_if_not_installed("lavaan")
  a <- rep(0.55, 8)
  cc <- rep(0.6, 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  sigma_m <- cbind(c(0.2, 0.3, 0.2))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)
  syn <- ssm_sem_syntax(
    scales = pop$scales, angles = oct, measures = pop$measures
  )
  # The natural user edit the escape hatch invites: freeing the g-plane
  # covariances in the scaled syntax -- the exact locally-unidentified
  # parameterization the generator refuses to emit
  syn_free <- gsub("g ~~ 0*cx", "g ~~ cx", syn, fixed = TRUE)
  syn_free <- gsub("g ~~ 0*cy", "g ~~ cy", syn_free, fixed = TRUE)
  fit <- suppressWarnings(lavaan::cfa(
    syn_free,
    sample.cov = pop$sigma, sample.nobs = 300, optim.method = "BFGS"
  ))
  expect_error(
    suppressWarnings(ssm_sem_parameters(fit,
      scales = pop$scales, angles = oct,
      measures = pop$measures, boots = 20
    )),
    "unidentified|strict"
  )
})

test_that("ssm_sem_parameters() warns when a raw-data se='standard' fit meets the mvn engine", {
  skip_if_not_installed("lavaan")
  a <- rep(0.55, 8)
  cc <- rep(0.6, 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  sigma_m <- cbind(c(0.2, 0.3, 0.2))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)
  set.seed(6)
  dat <- as.data.frame(matrix(rnorm(400 * 9), 400, 9) %*% chol(pop$sigma))
  colnames(dat) <- colnames(pop$sigma)
  syn <- ssm_sem_syntax(
    scales = pop$scales, angles = oct, measures = pop$measures
  )
  fit <- lavaan::cfa(syn, data = dat) # lavaan default se = "standard"
  expect_warning(
    ssm_sem_parameters(fit,
      scales = pop$scales, angles = oct,
      measures = pop$measures, boots = 50
    ),
    "robust"
  )
  # Summary-moment fits cannot do better and are not warned
  fit_cov <- sem_pop_fit(pop, n = 300)
  expect_no_warning(
    ssm_sem_parameters(fit_cov,
      scales = pop$scales, angles = oct,
      measures = pop$measures, boots = 50
    )
  )
})

test_that("ssm_sem_parameters() refuses a structurally incompatible lavaan fit (sec. 7.2)", {
  skip_if_not_installed("lavaan")
  # A generic one-factor model: none of the circumplex structure is present
  dat <- as.data.frame(matrix(rnorm(200 * 4), 200, 4,
    dimnames = list(NULL, paste0("y", 1:4))
  ))
  fit <- lavaan::cfa("f =~ y1 + y2 + y3 + y4", data = dat)
  expect_error(
    ssm_sem_parameters(fit,
      scales = paste0("y", 1:4),
      angles = c(90, 180, 270, 360), measures = "y1"
    ),
    "compatible|parameter|structure"
  )
})

# Subclass methods and the method audit (spec sec. 7.3) ----------------------------

test_that("summary.circumplex_ssm_sem() states the actual inferential method (sec. 7.3)", {
  skip_if_not_installed("lavaan")
  a <- rep(0.55, 8)
  cc <- rep(0.6, 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  sigma_m <- cbind(c(0.2, 0.3, 0.2))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)
  fit <- sem_pop_fit(pop, n = 300)
  set.seed(1)
  res <- ssm_sem_parameters(
    fit,
    scales = pop$scales, angles = oct, measures = pop$measures, boots = 100
  )
  out <- paste(utils::capture.output(summary(res)), collapse = "\n")
  expect_match(out, "Latent")
  expect_match(out, "MVN Draws")
  expect_no_match(out, "Bootstrap Resamples")
  expect_no_match(out, "Listwise Deletion:")
})

test_that("sem_detail_labels() single-sources the summary detail-line labels", {
  # Locks both branches of the label seam that summary() delegates to.
  mvn_fiml <- sem_detail_labels(list(method = "mvn", missing = "fiml"))
  expect_match(mvn_fiml$replicate, "MVN Draws")
  expect_identical(mvn_fiml$missing, "FIML")
  boot_lw <- sem_detail_labels(list(method = "boot", missing = "listwise"))
  expect_match(boot_lw$replicate, "Bootstrap Refits")
  expect_identical(boot_lw$missing, "Listwise deletion")
})

test_that("print.circumplex_ssm_sem() prepends the measurement-model block (sec. 7.3)", {
  skip_if_not_installed("lavaan")
  a <- rep(0.55, 8)
  cc <- rep(0.6, 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  sigma_m <- cbind(c(0.2, 0.3, 0.2))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)
  fit <- sem_pop_fit(pop, n = 300)
  set.seed(1)
  res <- ssm_sem_parameters(
    fit,
    scales = pop$scales, angles = oct, measures = pop$measures, boots = 100
  )
  out <- paste(utils::capture.output(print(res)), collapse = "\n")
  expect_match(out, "scaled")
  expect_match(out, "CFI|RMSEA")
  expect_match(out, "Profile \\[")
})

test_that("ssm_ci_accuracy() refuses latent (SEM) objects with a pointer to the harness (sec. 7.3)", {
  skip_if_not_installed("lavaan")
  a <- rep(0.55, 8)
  cc <- rep(0.6, 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  sigma_m <- cbind(c(0.2, 0.3, 0.2))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)
  fit <- sem_pop_fit(pop, n = 300)
  set.seed(1)
  res <- ssm_sem_parameters(
    fit,
    scales = pop$scales, angles = oct, measures = pop$measures, boots = 100
  )
  expect_error(ssm_ci_accuracy(res, reps = 5), "latent|SEM")
})

test_that("inherited consumers render latent results sensibly: ssm_table() and plots (sec. 7.3 method audit)", {
  # The sec. 7.3 method audit, in full (every exported function that
  # dispatches on or checks the circumplex_ssm class string, per Grep):
  #   print.circumplex_ssm    -> overridden (measurement-model block) [tested]
  #   summary.circumplex_ssm  -> overridden (method/missing lines)    [tested]
  #   ssm_ci_accuracy()       -> refusal guard added                  [tested]
  #   ssm_table()             -> inherited OK: results + score_type
  #                              ("Latent-based ..." caption)         [here]
  #   ssm_plot_circle()       -> inherited OK: results/details only   [here]
  #   ssm_plot_curve()        -> inherited OK: results/details only   [here]
  #   ssm_plot_contrast()     -> inherited OK: contrast results row   [tested
  #                              in the +/-180 contrast test]
  #   ssm_suff_stats()        -> internal; unreachable for the subclass
  #                              (only caller is behind the guard)
  skip_if_not_installed("lavaan")
  a <- rep(0.55, 8)
  cc <- rep(0.6, 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  sigma_m <- cbind(c(0.2, 0.3, 0.2))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)
  fit <- sem_pop_fit(pop, n = 300)
  set.seed(1)
  res <- ssm_sem_parameters(
    fit,
    scales = pop$scales, angles = oct, measures = pop$measures, boots = 100
  )
  tab <- ssm_table(res, render = FALSE)
  expect_s3_class(tab, "data.frame")
  expect_match(dcaption(res), "Latent")
  p1 <- ssm_plot_circle(res)
  expect_s3_class(p1, "ggplot")
  p2 <- ssm_plot_curve(res)
  expect_s3_class(p2, "ggplot")
})

# M5 milestone-close review fixes (2026-07-08) --------------------------------

test_that("sem_fmt_p() never renders a p-value as exactly zero", {
  # Decisive p-values display as a bound, not the improper "p = 0"
  expect_identical(sem_fmt_p(9.4e-7, 4), "< 0.0001")
  expect_identical(sem_fmt_p(9.4e-7, 4, prose = TRUE), "< 0.0001")
  expect_identical(sem_fmt_p(0.0432, 3, prose = TRUE), "= 0.043")
  expect_identical(sem_fmt_p(0.0432, 3), "0.043")
  expect_identical(sem_fmt_p(NA_real_, 3), NA_character_)
  # Boundary: exactly at the threshold is representable, not bounded
  expect_identical(sem_fmt_p(1e-3, 3), "0.001")
})

test_that("engine preconditions on user-supplied fits fail with actionable errors", {
  skip_if_not_installed("lavaan")
  a <- rep(0.55, 8)
  cc <- rep(0.6, 8)
  theta <- seq(0.3, 0.6, length.out = 8)
  sigma_m <- cbind(c(0.2, 0.3, 0.2))
  pop <- sem_pop(a, cc, theta, oct, sigma_m, v_m = 1)

  # A summary-moment (sample.cov) fit cannot be resampled
  fit_cov <- sem_pop_fit(pop, n = 500)
  expect_error(
    ssm_sem_parameters(
      fit_cov,
      scales = pop$scales, angles = oct, measures = pop$measures,
      ci_method = "boot", boots = 10
    ),
    "summary moments"
  )

  # A fit with se = "none" carries no covariance for the mvn engine
  fit_none <- sem_pop_fit(pop, n = 500, se = "none")
  expect_error(
    ssm_sem_parameters(
      fit_none,
      scales = pop$scales, angles = oct, measures = pop$measures,
      ci_method = "mvn", boots = 10
    ),
    "se = \"none\""
  )
})

test_that("a bootstrap-covariance fit meeting the mvn engine gets an advisory", {
  skip_if_not_installed("lavaan")
  # The advisory is keyed solely on the fit's recorded `se` option, so test it
  # deterministically: fit with se = "standard" (fast, stable, real stored
  # vcov) and relabel the stored option to "bootstrap". This exercises the
  # exact advisory branch without invoking lavaan's small-sample bootstrap,
  # which is environment-fragile for models built from generated syntax -- a
  # degenerate resample surfaces (inside lavaan's own vcov recomputation) as an
  # internal "model is NULL" error, unrelated to this advisory.
  a <- rep(0.7, 8)
  cc <- rep(0.75, 8)
  theta <- rep(0.4, 8)
  pop <- sem_pop(a, cc, theta, oct, cbind(c(0.3, 0.35, 0.25)), v_m = 1)
  set.seed(42)
  n <- 800
  dat <- as.data.frame(
    matrix(stats::rnorm(n * nrow(pop$sigma)), n) %*% chol(pop$sigma)
  )
  names(dat) <- colnames(pop$sigma)
  syn <- ssm_sem_syntax(
    scales = pop$scales, angles = oct, measures = pop$measures
  )
  fit <- suppressWarnings(lavaan::cfa(syn, data = dat, se = "standard"))
  fit@Options$se <- "bootstrap"
  w <- testthat::capture_warnings(
    ssm_sem_parameters(
      fit,
      scales = pop$scales, angles = oct, measures = pop$measures, boots = 50
    )
  )
  expect_true(any(grepl("bootstrap-estimated covariance", w)))
})
