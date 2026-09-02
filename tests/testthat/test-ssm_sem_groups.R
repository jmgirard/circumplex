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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  # print states the non-comparison; contrast plot refuses BY RESTATING THE
  # VERDICT (the pre-review bare stopifnot matched the old loose "contrast"
  # pattern here while telling the user nothing -- keep this pin strict)
  expect_output(print(res), "cannot be compared|not computed|rejected")
  expect_error(ssm_plot_contrast(res), "not computed")
  expect_error(ssm_plot_contrast(res), "cannot be compared")
})

# The +/-180 branch cut on the latent group contrast (sec. 5.5/6.4) ---------------

test_that("latent group contrast near +/-180 stays on the estimate's branch (sec. 6.4)", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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

# The Delta-CFI secondary criterion (M57) ----------------------------------------
#
# Cheung & Rensvold (2002) proposed a general Delta-GFI criterion for the
# invariance ladder; the CFI member is implemented here as a labeled,
# REPORTED-ONLY secondary criterion. Value and direction come from
# cairn/references/cheung2002.md (the paper's own p. 251 sentence states the
# direction backwards -- it contradicts the Table 5 simulation the critical
# values come from). Scope is two groups and plain-ML CFI; the package's
# default estimator is MLR, so the binary flag is OFF by default BY DESIGN.

# Group A is metric-invariant with group B up to per-group factor dispersion
# and its own measure block; `eps` then perturbs group B's circumplex
# saturations by (1 + eps*cos(2*theta)) -- a SECOND-HARMONIC pattern violation
# the scaled tier's per-group isotropic plane scale cannot absorb. eps and
# n_per tune the two criteria independently: the population CFI drop is set by
# eps alone while Delta-chi-square grows with n, which is what makes a
# Delta-CFI/Delta-chi-square DISAGREEMENT constructible (see the reported-only
# test below). eps = 0 is exactly metric-invariant.
dcfi_pop_2g <- function(eps) {
  a <- seq(0.5, 0.7, length.out = 8)
  cc_a <- seq(0.65, 0.55, length.out = 8)
  d1 <- 40 * pi / 180
  d2 <- 110 * pi / 180
  gA <- sem_pop(
    a, cc_a, seq(0.3, 0.6, length.out = 8), oct,
    cbind(c(0.2, 0.4 * cos(d1), 0.4 * sin(d1))),
    v_m = 1
  )
  gB <- sem_pop(
    a, cc_a * (1 + eps * cos(2 * oct * pi / 180)),
    seq(0.4, 0.7, length.out = 8), oct,
    cbind(c(0.25, 0.35 * cos(d2), 0.35 * sin(d2))),
    v_m = 1
  )
  list(
    sigma = list(A = gA$sigma, B = gB$sigma),
    scales = gA$scales, measures = gA$measures
  )
}

# Simulate raw data for an arbitrary number of groups (sim_2g() is two-group)
sim_groups <- function(sigmas, n_per, seed) {
  set.seed(seed)
  do.call(rbind, lapply(names(sigmas), function(g) {
    sig <- sigmas[[g]]
    x <- as.data.frame(matrix(rnorm(n_per * ncol(sig)), n_per) %*% chol(sig))
    colnames(x) <- colnames(sig)
    x$grp <- g
    x
  }))
}

# The printed ladder's header line, for asserting which columns are shown
dcfi_header <- function(res) {
  out <- utils::capture.output(print(res))
  grep("rung", out, value = TRUE)[[1]]
}

# Printed output with all whitespace collapsed. The attribution block is
# wrapped programmatically (its reason list is variable-length), so an
# assertion keyed to where a line happens to break would fence the wrapper
# rather than the wording.
dcfi_printed <- function(res) {
  gsub("\\s+", " ", paste(utils::capture.output(print(res)), collapse = " "))
}

test_that("dcfi is the CFI difference between adjacent fitted rungs, NA where there is no predecessor (cheung2002)", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  pop <- dcfi_pop_2g(0)
  dat <- sim_groups(pop$sigma, n_per = 400, seed = 11)
  set.seed(31)
  res <- ssm_sem(dat,
    scales = pop$scales, measures = "m1", grouping = "grp",
    estimator = "ML", boots = 20
  )
  tab <- res$invariance$table
  expect_true("dcfi" %in% names(tab))
  # configural has no predecessor
  expect_true(is.na(tab$dcfi[tab$rung == "configural"]))
  # Oracle 1 (invariant): the column IS the difference of the CFI the same
  # table displays -- the two internal routes must agree exactly
  expect_equal(tab$dcfi[-1], diff(tab$cfi), tolerance = 0)
  # Oracle 2 (live, independent): refit both rungs outside the ladder and
  # difference lavaan's own plain CFI. Independent of sem_fit_ladder's
  # bookkeeping, so it catches an off-by-one pairing the invariant cannot.
  dat$grp <- factor(dat$grp)
  cfi_refit <- vapply(c("configural", "metric"), function(rung) {
    fit <- lavaan::cfa(
      ssm_sem_syntax(
        scales = pop$scales, angles = oct, measures = "m1",
        n_groups = 2, invariance = rung
      ),
      data = dat, group = "grp", group.label = levels(dat$grp),
      estimator = "ML", se = "robust.huber.white", missing = "listwise"
    )
    unname(lavaan::fitMeasures(fit)[["cfi"]])
  }, numeric(1))
  expect_equal(
    tab$dcfi[tab$rung == "metric"],
    cfi_refit[["metric"]] - cfi_refit[["configural"]],
    tolerance = 1e-8
  )
})

test_that("the strict tier's VACUOUS metric rung carries no dcfi, and the next rung differences against configural", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  p <- 8
  th <- oct * pi / 180
  lambda <- cbind(1, cos(th), sin(th))
  phi <- diag(c(0.8, 0.5, 0.5))
  sig <- lambda %*% phi %*% t(lambda) + diag(seq(0.3, 0.6, length.out = p))
  nm <- paste0("s", 1:p)
  dimnames(sig) <- list(nm, nm)
  nu <- seq(1, 1.6, length.out = p)
  mu2 <- nu + as.numeric(lambda %*% c(0.3, 0.25, -0.15))
  set.seed(32)
  mk <- function(mu, grp, n = 600) {
    x <- as.data.frame(sweep(matrix(rnorm(n * p), n) %*% chol(sig), 2, mu, "+"))
    colnames(x) <- nm
    x$grp <- grp
    x
  }
  dat <- rbind(mk(nu, "A"), mk(mu2, "B"))
  set.seed(33)
  res <- ssm_sem(dat,
    scales = nm, grouping = "grp", contrast = TRUE, model = "strict",
    estimator = "ML", boots = 20
  )
  tab <- res$invariance$table
  vac <- tab$rung == "metric"
  expect_true(nzchar(tab$note[vac])) # really the vacuous row
  expect_true(is.na(tab$dcfi[vac]))
  expect_true(is.na(tab$cr[vac]))
  # scalar differences against the last FITTED rung (configural), exactly as
  # its Delta-chi-square does -- the vacuous rung imposes nothing to difference
  expect_equal(
    tab$dcfi[tab$rung == "scalar"],
    tab$cfi[tab$rung == "scalar"] - tab$cfi[tab$rung == "configural"],
    tolerance = 0
  )
})

test_that("in scope (two groups, plain-ML CFI) the Cheung-Rensvold flag retains above the -.01 cutoff and rejects below it", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  # Retained: metric-invariant population, CFI does not drop
  pop_ok <- dcfi_pop_2g(0)
  dat_ok <- sim_groups(pop_ok$sigma, n_per = 400, seed = 11)
  set.seed(34)
  ok <- ssm_sem(dat_ok,
    scales = pop_ok$scales, measures = "m1", grouping = "grp",
    estimator = "ML", boots = 20
  )
  tab_ok <- ok$invariance$table
  expect_true(isTRUE(ok$invariance$dcfi_scope$in_scope))
  expect_gt(tab_ok$dcfi[tab_ok$rung == "metric"], -0.01)
  expect_identical(tab_ok$cr[tab_ok$rung == "metric"], "retain")
  expect_true(is.na(tab_ok$cr[tab_ok$rung == "configural"]))
  # Rejected: a strong second-harmonic pattern violation drops CFI by > .01
  pop_bad <- dcfi_pop_2g(0.35)
  dat_bad <- sim_groups(pop_bad$sigma, n_per = 700, seed = 61)
  set.seed(35)
  bad <- suppressWarnings(ssm_sem(dat_bad,
    scales = pop_bad$scales, measures = "m1", grouping = "grp",
    estimator = "ML", boots = 20
  ))
  tab_bad <- bad$invariance$table
  expect_lt(tab_bad$dcfi[tab_bad$rung == "metric"], -0.01)
  expect_identical(tab_bad$cr[tab_bad$rung == "metric"], "reject")
  # Printed: the value, the flag, and the attribution WITH its scope label
  out <- dcfi_printed(bad)
  expect_match(out, "Cheung & Rensvold \\(2002\\)")
  expect_match(out, "alpha = .01", fixed = TRUE)
  expect_match(out, "two-group", fixed = TRUE)
  expect_match(dcfi_header(bad), "dcfi")
  expect_match(dcfi_header(bad), "cr\\s*$")
})

test_that("the -.01 cutoff is a >= boundary and never fires outside its validated scope (cheung2002 operational rule)", {
  # Deterministic unit pin of the criterion itself: the transcription's rule is
  # "Delta-CFI < -.01 -> reject; Delta-CFI >= -.01 -> retained", so a value
  # exactly AT the cutoff retains. No fit lands exactly on -.01, hence the
  # direct helper test (cairn/references/cheung2002.md).
  expect_identical(
    sem_dcfi_flag(c(NA, 0.004, 0, -0.005, -0.01, -0.0100001, -0.03), TRUE),
    c(NA, "retain", "retain", "retain", "retain", "reject", "reject")
  )
  # Out of scope: values are still differenced, but no verdict is attached
  expect_true(all(is.na(sem_dcfi_flag(c(0.004, -0.03), FALSE))))
  expect_true(all(is.na(sem_dcfi_flag(c(0.004, -0.03), NA))))
})

test_that("outside the validated scope (robust CFI, or more than two groups) dcfi prints with a not-validated note and NO binary flag", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  # (a) robust CFI: the package's DEFAULT estimator (MLR), two groups
  pop <- dcfi_pop_2g(0.02)
  dat2 <- sim_groups(pop$sigma, n_per = 400, seed = 51)
  set.seed(36)
  mlr <- ssm_sem(dat2,
    scales = pop$scales, measures = "m1", grouping = "grp", boots = 20
  )
  inv <- mlr$invariance
  expect_false(isTRUE(inv$dcfi_scope$in_scope))
  expect_false(isTRUE(inv$dcfi_scope$cfi_plain))
  expect_false(is.na(inv$table$dcfi[inv$table$rung == "metric"])) # value kept
  expect_true(all(is.na(inv$table$cr))) # verdict withheld
  out <- dcfi_printed(mlr)
  expect_match(out, "Cheung & Rensvold \\(2002\\)")
  expect_match(out, "not validated", ignore.case = TRUE)
  expect_match(out, "robust CFI", fixed = TRUE)
  expect_match(out, "descriptive only", fixed = TRUE)
  expect_no_match(dcfi_header(mlr), "cr\\s*$") # no flag column at all
  # (b) three groups under plain ML
  pop3 <- list(
    A = dcfi_pop_2g(0)$sigma$A,
    B = dcfi_pop_2g(0.02)$sigma$B,
    C = dcfi_pop_2g(-0.02)$sigma$B
  )
  dat3 <- sim_groups(pop3, n_per = 400, seed = 41)
  set.seed(37)
  g3 <- suppressWarnings(ssm_sem(dat3,
    scales = pop$scales, measures = "m1", grouping = "grp",
    estimator = "ML", boots = 20
  ))
  inv3 <- g3$invariance
  expect_identical(inv3$dcfi_scope$n_groups, 3L)
  expect_true(isTRUE(inv3$dcfi_scope$cfi_plain)) # plain CFI, but 3 groups
  expect_false(isTRUE(inv3$dcfi_scope$in_scope))
  expect_false(is.na(inv3$table$dcfi[inv3$table$rung == "metric"]))
  expect_true(all(is.na(inv3$table$cr)))
  out3 <- dcfi_printed(g3)
  expect_match(out3, "not validated", ignore.case = TRUE)
  expect_match(out3, "3 groups", fixed = TRUE)
  expect_no_match(dcfi_header(g3), "cr\\s*$")
})

test_that("Delta-CFI is REPORTED ONLY: it never moves the gate, the verdict, or the estimation fit, even when it disagrees with Delta-chi-square", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  # A deliberate DISAGREEMENT: a small pattern violation at large n. The
  # Cheung-Rensvold flag retains (CFI drops only ~.002) while the nested
  # Delta-chi-square rejects (p ~ .0004). Only the latter may gate.
  pop <- dcfi_pop_2g(0.12)
  dat <- sim_groups(pop$sigma, n_per = 2000, seed = 31)
  set.seed(38)
  res <- suppressWarnings(ssm_sem(dat,
    scales = pop$scales, measures = "m1", grouping = "grp",
    contrast = TRUE, estimator = "ML", boots = 20
  ))
  inv <- res$invariance
  tab <- inv$table
  met <- tab$rung == "metric"
  # The two criteria really do disagree on this fixture
  expect_gt(tab$dcfi[met], -0.01)
  expect_identical(tab$cr[met], "retain")
  expect_lt(tab$p[met], inv$alpha)
  # The gate follows Delta-chi-square ALONE. Recomputed here from the table's
  # own p column and alpha -- a Delta-CFI-blind determination.
  gated <- tab[!nzchar(tab$note) & tab$rung != "configural", ]
  expect_identical(
    inv$comparable,
    !any(is.na(gated$p)) && !any(gated$p < inv$alpha, na.rm = TRUE)
  )
  expect_false(isTRUE(inv$comparable))
  # The verdict text is the Delta-chi-square verdict, with no CFI in it
  expect_match(inv$verdict, "metric invariance rejected")
  expect_no_match(inv$verdict, "CFI", ignore.case = TRUE)
  expect_no_match(inv$verdict, "retain")
  # The estimation fit is still the configural one (the non-comparison path),
  # and no contrast row was produced
  expect_false(isTRUE(res$details$contrast))
  expect_equal(nrow(res$results), 2)
  expect_identical(
    lavaan::fitMeasures(res$sem)[["df"]],
    tab$df[tab$rung == "configural"]
  )
})

# A low-saturation two-group population: a small baseline chi-square makes CFI
# move fast per unit of misfit while Delta-chi-square stays modest at small n.
# This is what makes the COMPLEMENT of the disagreement fixture reachable --
# Delta-CFI rejecting while the nested test retains (see the AND-leak test).
dcfi_pop_lowsat <- function(eps) {
  a <- rep(0.35, 8)
  cc_a <- rep(0.40, 8)
  d1 <- 40 * pi / 180
  d2 <- 110 * pi / 180
  gA <- sem_pop(
    a, cc_a, rep(0.6, 8), oct,
    cbind(c(0.15, 0.3 * cos(d1), 0.3 * sin(d1))), v_m = 1
  )
  gB <- sem_pop(
    a, cc_a * (1 + eps * cos(2 * oct * pi / 180)), rep(0.6, 8), oct,
    cbind(c(0.18, 0.28 * cos(d2), 0.28 * sin(d2))), v_m = 1
  )
  list(sigma = list(A = gA$sigma, B = gB$sigma), scales = gA$scales)
}

test_that("a non-ML estimator that still yields a plain CFI is OUT of scope (Cheung & Rensvold simulated ML only)", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  # GLS, WLS, ULS and continuous DWLS all produce plain-named fit measures, so
  # "no cfi.robust/cfi.scaled" does NOT imply "normal-theory ML CFI". Cheung &
  # Rensvold's Limitations section (p. 251) restricts the criterion to ML, so a
  # non-ML fit must get the value and NO verdict -- flagging it would assert a
  # cutoff the source never simulated. Caught at the M57 review (F1).
  pop <- dcfi_pop_2g(0)
  dat <- sim_groups(pop$sigma, n_per = 300, seed = 11)
  set.seed(41)
  gls <- suppressWarnings(ssm_sem(dat,
    scales = pop$scales, measures = "m1", grouping = "grp",
    estimator = "GLS", se = "standard", boots = 20
  ))
  sc <- gls$invariance$dcfi_scope
  expect_identical(sc$estimator, "GLS")
  expect_true(isTRUE(sc$cfi_plain)) # the trap: the CFI *is* plain-named
  expect_false(isTRUE(sc$ml)) # but the estimator is not ML
  expect_false(isTRUE(sc$in_scope))
  expect_false(is.na(gls$invariance$table$dcfi[2])) # value still reported
  expect_true(all(is.na(gls$invariance$table$cr))) # verdict withheld
  expect_no_match(dcfi_header(gls), "cr\\s*$")
  out <- dcfi_printed(gls)
  expect_match(out, "not validated", ignore.case = TRUE)
  expect_match(out, "non-ML estimator: GLS", fixed = TRUE)
  # And the ML family stays IN scope: MLR/MLM are ML estimation, excluded only
  # by their robust CFI, so the plain-CFI test must remain the operative one
  set.seed(42)
  ml <- ssm_sem(dat,
    scales = pop$scales, measures = "m1", grouping = "grp",
    estimator = "ML", boots = 20
  )
  expect_identical(ml$invariance$dcfi_scope$estimator, "ML")
  expect_true(isTRUE(ml$invariance$dcfi_scope$in_scope))
})

test_that("printed dcfi resolves against the cutoff: values either side of -.01 never render alike (M57 review F2)", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  # round(dcfi, 3) would print both -0.0096 (retain) and -0.0104 (reject) as
  # "-0.01", showing one number under two opposite labels directly beneath a
  # rule stated in terms of that number.
  pop <- dcfi_pop_2g(0)
  dat <- sim_groups(pop$sigma, n_per = 400, seed = 11)
  set.seed(43)
  res <- ssm_sem(dat,
    scales = pop$scales, measures = "m1", grouping = "grp",
    estimator = "ML", boots = 20
  )
  spliced <- res
  spliced$invariance$table$dcfi[2] <- -0.0096
  spliced$invariance$table$cr <- sem_dcfi_flag(
    spliced$invariance$table$dcfi, TRUE
  )
  # Assert on the metric RUNG's own printed row, not the whole output -- the
  # attribution note legitimately contains the literal "-0.01" (it states the
  # rule), so a whole-output negative match would fence nothing.
  metric_row <- function(x) {
    grep("^ *metric ", utils::capture.output(print(x)), value = TRUE)[[1]]
  }
  row_hi <- metric_row(spliced)
  spliced$invariance$table$dcfi[2] <- -0.0104
  spliced$invariance$table$cr <- sem_dcfi_flag(
    spliced$invariance$table$dcfi, TRUE
  )
  row_lo <- metric_row(spliced)
  # The two labels really are opposite at these values
  expect_match(row_hi, "retain")
  expect_match(row_lo, "reject")
  # ... and the printed VALUE distinguishes them
  expect_match(row_hi, "-0.0096", fixed = TRUE)
  expect_match(row_lo, "-0.0104", fixed = TRUE)
  # Neither collapses onto the bare cutoff the label is decided against
  expect_no_match(row_hi, "-0.01 ", fixed = TRUE)
  expect_no_match(row_lo, "-0.01 ", fixed = TRUE)
})

test_that("the gate follows Delta-chi-square when Delta-CFI REJECTS and the nested test retains (AND-leak guard)", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  # Complement of the disagreement fixture above. There, ΔCFI retained while
  # Δχ² rejected, and the truth was comparable = FALSE -- so a leak of the form
  # "comparable requires BOTH criteria to retain" would also have produced
  # FALSE and passed. Here ΔCFI REJECTS while Δχ² retains and the truth is
  # comparable = TRUE, which such a leak cannot produce. The two fixtures
  # together fence both directions (M57 review F3).
  pop <- dcfi_pop_lowsat(0.30)
  dat <- sim_groups(pop$sigma, n_per = 150, seed = 64)
  set.seed(44)
  res <- ssm_sem(dat,
    scales = pop$scales, measures = "m1", grouping = "grp",
    contrast = TRUE, estimator = "ML", boots = 200
  )
  inv <- res$invariance
  met <- inv$table$rung == "metric"
  expect_true(isTRUE(inv$dcfi_scope$in_scope)) # the flag is live here
  expect_lt(inv$table$dcfi[met], -0.01) # Cheung-Rensvold rejects
  expect_identical(inv$table$cr[met], "reject")
  expect_gte(inv$table$p[met], inv$alpha) # the nested test retains
  # The gate follows the nested test alone: comparable, and the contrast IS
  # computed despite the secondary criterion rejecting the same rung
  expect_true(isTRUE(inv$comparable))
  expect_true(isTRUE(res$details$contrast))
  expect_equal(nrow(res$results), 3)
  expect_match(res$results$Label[3], "B - A")
  expect_match(inv$verdict, "metric invariance retained")
  expect_no_match(inv$verdict, "CFI", ignore.case = TRUE)
})
