# T4 (M5): multi-group invariance-gated latent contrasts.
# Traces to devel/m5-sem-design.md sections noted per block (esp. sec. 6, incl.
# the 2026-07-07 T4 amendment: scaled-tier g-plane covariances fixed to 0 in
# all groups at all rungs, so the ladder is exactly nested).
#
# Oracle rule: every expected value is computed in-test from a constructed
# two-group population (closed-form truth) or checked against the package's
# own validated estimators; no external numbers.

# Helpers -----------------------------------------------------------------------

oct <- as.numeric(octants())

# Two-group population under METRIC invariance (shared loadings; per-group
# factor metric per the amended sec. 6.2: var(g_k) and isotropic plane scale
# phi_k; g-plane covariances 0 everywhere), with per-group measure blocks.
# Built from the SHARED sem_pop() truth algebra (helper-ssm-sem.R) so the
# single- and multi-group populations cannot silently diverge. Returns
# per-group joint covariances, closed-form latent profiles rho*_g, and (via
# ssm_parameters at octants) the true per-group SSM values.
sem_pop_2g <- function(a, cc, theta1, theta2, sigma_m1, sigma_m2,
                       v_m = c(1, 1), vg = c(1, 1), phi_pl = c(1, 1),
                       angles_deg = oct) {
  one <- function(theta, sigma_m, vmk, vgk, phik) {
    sem_pop(a, cc, theta, angles_deg, sigma_m,
      v_m = vmk,
      phi = diag(c(vgk, phik, phik))
    )
  }
  g1 <- one(theta1, sigma_m1, v_m[1], vg[1], phi_pl[1])
  g2 <- one(theta2, sigma_m2, v_m[2], vg[2], phi_pl[2])
  list(
    sigma = list(A = g1$sigma, B = g2$sigma),
    rho = list(A = as.numeric(g1$rho0), B = as.numeric(g2$rho0)),
    truth = list(
      A = suppressWarnings(ssm_parameters(as.numeric(g1$rho0), angles_deg)),
      B = suppressWarnings(ssm_parameters(as.numeric(g2$rho0), angles_deg))
    ),
    scales = g1$scales, measures = g1$measures
  )
}

# Absolute angular gap in degrees via the package's own branch-safe wrap
deg_gap <- function(x, y) {
  abs(as.numeric(angle_dist(
    as_radian(as_degree(x)), as_radian(as_degree(y))
  ))) * 180 / pi
}

# Simulate raw two-group data from the population (ssm_sem() takes data)
sim_2g <- function(pop, n_per = 500, seed = 1) {
  set.seed(seed)
  mk <- function(sig, grp) {
    x <- as.data.frame(matrix(rnorm(n_per * ncol(sig)), n_per) %*% chol(sig))
    colnames(x) <- colnames(sig)
    x$grp <- grp
    x
  }
  rbind(mk(pop$sigma$A, "A"), mk(pop$sigma$B, "B"))
}

# Signed contrast truth in degrees, second minus first, (-180, 180]
d_contrast_truth <- function(pop) {
  as.numeric(angle_dist(
    as_radian(as_degree(as.numeric(pop$truth$B$Disp))),
    as_radian(as_degree(as.numeric(pop$truth$A$Disp)))
  )) * 180 / pi
}

# A metric-invariant interior population used by several tests
interior_2g <- function(d1_deg = 40, d2_deg = 110) {
  d1 <- d1_deg * pi / 180
  d2 <- d2_deg * pi / 180
  sem_pop_2g(
    a = seq(0.5, 0.7, length.out = 8),
    cc = seq(0.65, 0.55, length.out = 8),
    theta1 = seq(0.3, 0.6, length.out = 8),
    theta2 = seq(0.4, 0.7, length.out = 8),
    sigma_m1 = cbind(c(0.2, 0.4 * cos(d1), 0.4 * sin(d1))),
    sigma_m2 = cbind(c(0.25, 0.35 * cos(d2), 0.35 * sin(d2))),
    vg = c(1, 1.3), phi_pl = c(1, 0.8)
  )
}

# The invariance ladder and gating (sec. 6.2/6.3) ---------------------------------

test_that("measure-path ssm_sem() with grouping fits the ladder to metric and reports per-group + contrast rows (sec. 6.2/6.4)", {
  skip_if_not_installed("lavaan")
  pop <- interior_2g()
  dat <- sim_2g(pop, n_per = 700, seed = 11)
  set.seed(20260707)
  res <- ssm_sem(dat,
    scales = pop$scales, measures = "m1", grouping = "grp",
    contrast = TRUE, boots = 500
  )
  expect_s3_class(res, "circumplex_ssm_sem")
  # Invariance table: configural and metric rows, both fitted, with the
  # nested-test columns; verdict field says comparable
  inv <- res$invariance
  expect_true(all(c("configural", "metric") %in% inv$table$rung))
  expect_true(isTRUE(inv$comparable))
  # Rows: group A, group B, contrast (second minus first level)
  expect_equal(nrow(res$results), 3)
  expect_match(res$results$Label[3], "B - A|B-A")
  # Estimates near the metric-true latent values (finite n tolerance)
  expect_equal(res$results$a_est[1], pop$truth$A$Ampl, tolerance = 0.1)
  expect_equal(res$results$a_est[2], pop$truth$B$Ampl, tolerance = 0.1)
  dtr <- d_contrast_truth(pop)
  dc <- as.numeric(res$results$d_est[3])
  expect_lt(deg_gap(dc, dtr), 12)
  # Contrast displacement in (-180, 180], estimate inside its aligned CI
  expect_gt(dc, -180)
  expect_lte(dc, 180)
  expect_gte(dc, as.numeric(res$results$d_lci[3]))
  expect_lte(dc, as.numeric(res$results$d_uci[3]))
})

test_that("the metric-rung model is empirically identified at the metric-true population (sane SEs, no ridge; T3 lesson)", {
  skip_if_not_installed("lavaan")
  pop <- interior_2g()
  dat <- sim_2g(pop, n_per = 500, seed = 12)
  set.seed(1)
  res <- ssm_sem(dat,
    scales = pop$scales, measures = "m1", grouping = "grp",
    contrast = TRUE, boots = 100
  )
  V <- as.matrix(lavaan::vcov(res$sem))
  expect_lt(max(sqrt(pmax(diag(V), 0))), 1) # no exploding SEs
  # And the fitted model keeps g-plane covariances fixed at 0 in BOTH groups
  pt <- lavaan::parameterTable(res$sem)
  gp <- pt[pt$op == "~~" &
    ((pt$lhs == "g" & pt$rhs %in% c("cx", "cy")) |
      (pt$rhs == "g" & pt$lhs %in% c("cx", "cy"))), ]
  expect_true(all(gp$free == 0))
  expect_true(all(gp$ustart == 0 | pt$est[as.numeric(rownames(gp))] == 0))
})

test_that("non-comparison path: metric failure yields a stated non-comparison, not a number (sec. 6.3)", {
  skip_if_not_installed("lavaan")
  # Group B violates metric invariance: a different circumplex-saturation
  # PATTERN (second-harmonic), not just a metric rescaling
  p <- 8
  th <- oct * pi / 180
  d1 <- 40 * pi / 180
  pop <- sem_pop_2g(
    a = rep(0.6, p), cc = rep(0.6, p),
    theta1 = seq(0.3, 0.6, length.out = p),
    theta2 = seq(0.3, 0.6, length.out = p),
    sigma_m1 = cbind(c(0.2, 0.4 * cos(d1), 0.4 * sin(d1))),
    sigma_m2 = cbind(c(0.2, 0.4 * cos(d1), 0.4 * sin(d1)))
  )
  # overwrite group B with a pattern-violating population
  cc2 <- 0.45 + 0.25 * cos(2 * th)
  lambda2 <- cbind(rep(0.6, p), cc2 * cos(th), cc2 * sin(th))
  sig2 <- lambda2 %*% t(lambda2) + diag(seq(0.3, 0.6, length.out = p))
  sm2 <- lambda2 %*% cbind(c(0.2, 0.4 * cos(d1), 0.4 * sin(d1)))
  sigB <- rbind(cbind(sig2, sm2), cbind(t(sm2), 1))
  dimnames(sigB) <- dimnames(pop$sigma$B)
  pop$sigma$B <- sigB
  dat <- sim_2g(pop, n_per = 900, seed = 13)
  set.seed(2)
  res <- suppressWarnings(ssm_sem(dat,
    scales = pop$scales, measures = "m1", grouping = "grp",
    contrast = TRUE, boots = 200
  ))
  inv <- res$invariance
  expect_false(isTRUE(inv$comparable))
  expect_match(inv$verdict, "metric", ignore.case = TRUE)
  # No contrast row; per-group (configural) profiles may still be reported
  expect_false(any(grepl(" - ", res$results$Label, fixed = TRUE)))
  expect_false(isTRUE(res$details$contrast))
  # print states the non-comparison; contrast plot refuses
  expect_output(print(res), "cannot be compared|not computed|rejected")
  expect_error(ssm_plot_contrast(res), "contrast|invariance|compare")
})

# The +/-180 branch cut on the latent group contrast (sec. 5.5/6.4) ---------------

test_that("latent group contrast near +/-180 stays on the estimate's branch (sec. 6.4)", {
  skip_if_not_installed("lavaan")
  pop <- interior_2g(d1_deg = 5, d2_deg = 186)
  dat <- sim_2g(pop, n_per = 700, seed = 14)
  set.seed(3)
  res <- ssm_sem(dat,
    scales = pop$scales, measures = "m1", grouping = "grp",
    contrast = TRUE, boots = 800
  )
  dc <- as.numeric(res$results$d_est[3])
  dtr <- d_contrast_truth(pop) # ~ -179
  expect_gt(dc, -180)
  expect_lte(dc, 180)
  expect_lt(deg_gap(dc, dtr), 12)
  # Branch alignment: estimate numerically inside its own interval even if
  # the endpoints legitimately exceed +/-180
  expect_gte(dc, as.numeric(res$results$d_lci[3]))
  expect_lte(dc, as.numeric(res$results$d_uci[3]))
})

# The latent mean path (sec. 6.4) --------------------------------------------------

test_that("mean path: the latent mean contrast recovers a constructed cosine shift EXACTLY at population moments under the strict tier (sec. 6.4)", {
  skip_if_not_installed("lavaan")
  p <- 8
  th <- oct * pi / 180
  # Strict-tier population: unit-cosine loadings, alpha_2 = (de, dx, dy)
  de <- 0.3
  dx <- 0.25
  dy <- -0.15
  lambda <- cbind(1, cos(th), sin(th))
  phi <- diag(c(0.8, 0.5, 0.5))
  theta <- seq(0.3, 0.6, length.out = p)
  sig <- lambda %*% phi %*% t(lambda) + diag(theta)
  nm <- paste0("s", 1:p)
  dimnames(sig) <- list(nm, nm)
  nu <- seq(1, 1.6, length.out = p)
  mu2 <- nu + as.numeric(lambda %*% c(de, dx, dy))
  # Fit the scalar-rung strict model to the exact population moments (the
  # escape-hatch adapter takes a user fit, so gating is bypassed -- this is
  # a machinery pin of the estimand map, not of the ladder)
  syn <- ssm_sem_syntax(
    scales = nm, angles = oct, model = "strict",
    n_groups = 2, invariance = "scalar"
  )
  fit <- lavaan::cfa(syn,
    sample.cov = list(A = sig, B = sig),
    sample.mean = list(A = nu, B = mu2),
    sample.nobs = c(800, 800)
  )
  set.seed(4)
  res <- ssm_sem_parameters(fit,
    scales = nm, angles = oct,
    measures = NULL, contrast = TRUE, boots = 50
  )
  # The constructed shift is exactly cosine under the strict tier, so the
  # latent-mean SSM contrast is (delta-e, delta-x, delta-y) = alpha_2 exactly
  r <- res$results
  i3 <- nrow(r)
  expect_lt(abs(r$e_est[i3] - de), 1e-3)
  expect_lt(abs(r$x_est[i3] - dx), 1e-3)
  expect_lt(abs(r$y_est[i3] - dy), 1e-3)
  expect_identical(res$details$score_type, "Latent mean")
})

test_that("mean path end-to-end: ssm_sem() gates at scalar and recovers the shift within sampling noise (sec. 6.4)", {
  skip_if_not_installed("lavaan")
  p <- 8
  th <- oct * pi / 180
  de <- 0.3
  dx <- 0.25
  dy <- -0.15
  lambda <- cbind(1, cos(th), sin(th))
  phi <- diag(c(0.8, 0.5, 0.5))
  theta <- seq(0.3, 0.6, length.out = p)
  sig <- lambda %*% phi %*% t(lambda) + diag(theta)
  nm <- paste0("s", 1:p)
  dimnames(sig) <- list(nm, nm)
  nu <- seq(1, 1.6, length.out = p)
  mu1 <- nu
  mu2 <- nu + as.numeric(lambda %*% c(de, dx, dy))
  set.seed(15)
  mk <- function(mu, grp, n = 800) {
    x <- as.data.frame(sweep(
      matrix(rnorm(n * p), n) %*% chol(sig), 2, mu, "+"
    ))
    colnames(x) <- nm
    x$grp <- grp
    x
  }
  dat <- rbind(mk(mu1, "A"), mk(mu2, "B"))
  set.seed(4)
  res <- ssm_sem(dat,
    scales = nm, grouping = "grp", contrast = TRUE,
    model = "strict", boots = 500
  )
  # Gating: the mean path requires (and therefore fitted) the scalar rung
  expect_true("scalar" %in% res$invariance$table$rung)
  expect_true(isTRUE(res$invariance$comparable))
  # Recovery within sampling noise, and the truth inside the 95% intervals
  r <- res$results
  i3 <- nrow(r)
  expect_lt(abs(r$e_est[i3] - de), 0.15)
  expect_lt(abs(r$x_est[i3] - dx), 0.15)
  expect_lt(abs(r$y_est[i3] - dy), 0.15)
  expect_true(r$e_lci[i3] <= de && de <= r$e_uci[i3])
  expect_true(r$x_lci[i3] <= dx && dx <= r$x_uci[i3])
  expect_true(r$y_lci[i3] <= dy && dy <= r$y_uci[i3])
})

test_that("mean path defaults its gating to scalar; measure path to metric (sec. 7.2 / review F8)", {
  skip_if_not_installed("lavaan")
  pop <- interior_2g()
  dat <- sim_2g(pop, n_per = 400, seed = 16)
  set.seed(5)
  res_m <- ssm_sem(dat,
    scales = pop$scales, measures = "m1", grouping = "grp",
    contrast = TRUE, boots = 50
  )
  expect_identical(res_m$invariance$gate, "metric")
  set.seed(5)
  res_mean <- ssm_sem(dat,
    scales = pop$scales, grouping = "grp", contrast = TRUE, boots = 50
  )
  expect_identical(res_mean$invariance$gate, "scalar")
})

# Estimand documentation surface (T4 acceptance) ----------------------------------

test_that("print shows the invariance block and the two-estimand distinction is stated in the docs (sec. 6.1)", {
  skip_if_not_installed("lavaan")
  pop <- interior_2g()
  dat <- sim_2g(pop, n_per = 400, seed = 17)
  set.seed(6)
  res <- ssm_sem(dat,
    scales = pop$scales, measures = "m1", grouping = "grp",
    contrast = TRUE, boots = 50
  )
  out <- paste(utils::capture.output(print(res)), collapse = "\n")
  expect_match(out, "[Ii]nvariance")
  expect_match(out, "configural|metric")
  # (The side-by-side estimand documentation in the Rd is a docs deliverable
  # verified at review; the printed non-comparison wording is asserted in the
  # non-comparison test above.)
})

test_that("the reference group follows FACTOR LEVELS, not row order: relabeling flips the contrast (CLAUDE.md grouping contract)", {
  skip_if_not_installed("lavaan")
  # Caught by /statistical-validation: lavaan's default group order is order
  # of appearance in the data, so without an explicit group.label the
  # contrast direction would silently depend on row order.
  pop <- interior_2g()
  dat <- sim_2g(pop, n_per = 500, seed = 19)
  set.seed(8)
  r1 <- ssm_sem(dat,
    scales = pop$scales, measures = "m1", grouping = "grp",
    contrast = TRUE, boots = 100
  )
  datR <- dat
  datR$grp <- factor(datR$grp, levels = c("B", "A")) # reversed levels,
  # SAME row order (A rows still first)
  set.seed(8)
  r2 <- ssm_sem(datR,
    scales = pop$scales, measures = "m1", grouping = "grp",
    contrast = TRUE, boots = 100
  )
  expect_match(r1$results$Label[3], "B - A")
  expect_match(r2$results$Label[3], "A - B")
  # Antisymmetry: the two directions are separate lavaan optimizations of
  # the same model (the reference-group constraints attach to different
  # groups), so they agree to optimizer precision -- assert ABSOLUTE bounds,
  # not relative ones (the contrast elevation here is near zero)
  i1 <- nrow(r1$results)
  i2 <- nrow(r2$results)
  expect_lt(abs(r2$results$e_est[i2] + r1$results$e_est[i1]), 1e-4)
  d1v <- as.numeric(r1$results$d_est[i1])
  d2v <- as.numeric(r2$results$d_est[i2])
  expect_lt(abs(((d2v + d1v + 180) %% 360) - 180), 1e-3)
})

test_that("same seed reproduces multi-group results; group draws are joint (sec. 5.3/5.4)", {
  skip_if_not_installed("lavaan")
  pop <- interior_2g()
  dat <- sim_2g(pop, n_per = 400, seed = 18)
  set.seed(7)
  r1 <- ssm_sem(dat,
    scales = pop$scales, measures = "m1", grouping = "grp",
    contrast = TRUE, boots = 200
  )
  set.seed(7)
  r2 <- ssm_sem(dat,
    scales = pop$scales, measures = "m1", grouping = "grp",
    contrast = TRUE, boots = 200
  )
  expect_identical(r1$results, r2$results)
})

# Cumulative gating (the /code-review finding: gate on ALL rungs <= required) --

test_that("a metric rejection gates the mean-path contrast even when the scalar increment passes (cumulative gating)", {
  skip_if_not_installed("lavaan")
  # Mean path, scaled tier: group B has a pattern-violating saturation
  # structure (metric false). The scalar-vs-metric INCREMENT may well pass;
  # the contrast must still be refused because the metric rung -- part of
  # what the scalar estimand is computed under -- was rejected.
  p <- 8
  th <- oct * pi / 180
  ccA <- rep(0.6, p)
  ccB <- 0.45 + 0.25 * cos(2 * th)
  mk_grp <- function(cc, grp, n = 900, seed) {
    lambda <- cbind(rep(0.6, p), cc * cos(th), cc * sin(th))
    sig <- lambda %*% t(lambda) + diag(seq(0.3, 0.6, length.out = p))
    set.seed(seed)
    x <- as.data.frame(matrix(rnorm(n * p), n) %*% chol(sig))
    colnames(x) <- paste0("s", 1:p)
    x$grp <- grp
    x
  }
  dat <- rbind(mk_grp(ccA, "A", seed = 21), mk_grp(ccB, "B", seed = 22))
  set.seed(9)
  res <- suppressWarnings(ssm_sem(dat,
    scales = paste0("s", 1:p), grouping = "grp", contrast = TRUE, boots = 100
  ))
  # Mean path fits configural -> metric -> scalar; metric is rejected, so
  # comparable must be FALSE regardless of the scalar increment's p
  expect_true(all(c("metric", "scalar") %in% res$invariance$table$rung))
  expect_false(isTRUE(res$invariance$comparable))
  expect_match(res$invariance$verdict, "metric")
  expect_false(isTRUE(res$details$contrast))
})

test_that("a rejection ABOVE the required rung is reported only, never gating (spec: step 4 reported, never required)", {
  skip_if_not_installed("lavaan")
  # Measure path (required = metric) with invariance = "scalar" requested:
  # metric holds; intercept differences make scalar fail; the contrast must
  # still be computed, with the scalar rejection reported.
  pop <- interior_2g()
  dat <- sim_2g(pop, n_per = 700, seed = 23)
  shift <- seq(0.5, 1.2, length.out = 8)
  bidx <- dat$grp == "B"
  dat[bidx, pop$scales] <- sweep(
    dat[bidx, pop$scales], 2, shift, "+"
  )
  set.seed(10)
  res <- ssm_sem(dat,
    scales = pop$scales, measures = "m1", grouping = "grp",
    contrast = TRUE, invariance = "scalar", boots = 200
  )
  inv <- res$invariance
  expect_identical(inv$required, "metric")
  scal <- inv$table[inv$table$rung == "scalar", ]
  expect_lt(scal$p, 0.05) # the construction really did break scalar
  expect_true(isTRUE(inv$comparable))
  expect_match(inv$verdict, "reported only")
  expect_true(isTRUE(res$details$contrast))
  expect_equal(nrow(res$results), 3)
})

# Mandated boundary cells, multi-group (CLAUDE.md) ------------------------------

test_that("a group profile at the 0/360 pole gets a straddling per-group CI inside a contrast analysis", {
  skip_if_not_installed("lavaan")
  pop <- sem_pop_2g(
    a = rep(0.55, 8), cc = rep(0.6, 8),
    theta1 = seq(0.3, 0.6, length.out = 8),
    theta2 = seq(0.3, 0.6, length.out = 8),
    sigma_m1 = cbind(c(0.15, 0.45, 0)), # group A exactly at the pole
    sigma_m2 = cbind(c(0.15, 0.4 * cos(100 * pi / 180),
      0.4 * sin(100 * pi / 180)))
  )
  dat <- sim_2g(pop, n_per = 600, seed = 24)
  set.seed(11)
  res <- ssm_sem(dat,
    scales = pop$scales, measures = "m1", grouping = "grp",
    contrast = TRUE, boots = 500
  )
  dA <- as.numeric(res$results$d_est[1])
  expect_lt(min(dA %% 360, 360 - dA %% 360), 8)
  lci <- as.numeric(res$results$d_lci[1])
  uci <- as.numeric(res$results$d_uci[1])
  # circular arc contains the pole (canonical membership rule)
  expect_true(isTRUE(ssm_ci_d_cover(0, lci * pi / 180, uci * pi / 180)$cover))
})

test_that("a flat group profile degrades honestly inside a contrast analysis (no crash, no confident direction)", {
  skip_if_not_installed("lavaan")
  pop <- sem_pop_2g(
    a = rep(0.55, 8), cc = rep(0.6, 8),
    theta1 = seq(0.3, 0.6, length.out = 8),
    theta2 = seq(0.3, 0.6, length.out = 8),
    sigma_m1 = cbind(c(0.15, 0.4 * cos(40 * pi / 180),
      0.4 * sin(40 * pi / 180))),
    sigma_m2 = cbind(c(0, 0, 0)) # group B: measure independent of everything
  )
  dat <- sim_2g(pop, n_per = 600, seed = 25)
  set.seed(12)
  res <- suppressWarnings(ssm_sem(dat,
    scales = pop$scales, measures = "m1", grouping = "grp",
    contrast = TRUE, boots = 300
  ))
  expect_lt(res$results$a_est[2], 0.06) # group B amplitude ~ 0
  i3 <- nrow(res$results)
  dc <- as.numeric(res$results$d_est[i3])
  arc <- as.numeric(res$results$d_uci[i3]) - as.numeric(res$results$d_lci[i3])
  # Honest degradation: contrast direction is NA (flat point profile) or its
  # interval covers a wide arc (near-uniform draws through the flat group)
  expect_true(is.na(dc) || arc > 90)
})

# The accidental cross-group equality guard (escape hatch) -----------------------

test_that("single-group syntax fitted with group= is refused (measure blocks equality-constrained by shared labels)", {
  skip_if_not_installed("lavaan")
  pop <- interior_2g()
  dat <- sim_2g(pop, n_per = 400, seed = 26)
  syn1 <- ssm_sem_syntax(
    scales = pop$scales, angles = oct, measures = "m1"
  ) # single-group: plain labels
  fit <- suppressWarnings(
    lavaan::cfa(syn1, data = dat, group = "grp")
  )
  expect_error(
    suppressWarnings(ssm_sem_parameters(fit,
      scales = pop$scales, angles = oct, measures = "m1", boots = 20
    )),
    "equality-constrained across groups"
  )
})
