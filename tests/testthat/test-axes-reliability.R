# M54 axes_reliability(): fixed axis weights and per-axis item_n (T1).

test_that("BC3: per-axis item_n is exact for balanced octant instruments", {
  ang <- octants()
  expect_identical(axis_item_n(ang, 8L), c(x = 32, y = 32)) # 64-item
  expect_identical(axis_item_n(ang, 4L), c(x = 16, y = 16)) # 32-item
  expect_identical(axis_item_n(ang, 2L), c(x = 8, y = 8))   # 16-item
  # equal across the two axes for every balanced octant instrument
  for (k in 1:8) {
    inn <- axis_item_n(ang, k)
    expect_identical(inn[["x"]], inn[["y"]])
  }
})

test_that("BC10: pole weights snap exactly and theta 0 == 360", {
  expect_identical(as.numeric(axis_weights(360)), c(1, 0)) # LM at the pole
  expect_identical(as.numeric(axis_weights(90)), c(0, 1))  # PA on the y-axis
  expect_identical(axis_weights(0), axis_weights(360))     # 0 and 360 coincide
  # no ~1e-16 residue leaks: off-pole weights are exactly +/- cos(45 deg)
  w <- axis_weights(octants())
  resid <- w[!(w %in% c(0, 1, -1))]
  expect_true(all(abs(abs(resid) - cospi(0.25)) < 1e-12))
})

# Layer-A published-value oracle. Fixtures are Strack et al. (2013) Table 3
# (p. 7), banked in cairn/references/strack2013.md (two-channel verified:
# pdftotext text layer + page-image render). IIP S6 Self is a documented source
# erratum (components sum to 101.0%); BC1 handling ruled by RR10.

test_that("BC1: Spearman-Brown reproduces Table 3 reliability (Layer A)", {
  # Four anchor rows (col 6 %axes / 100, col 10 item_n, col 11 Rel), +/-.005.
  anchors <- data.frame(
    xi1    = c(.260, .134, .117, .028), # IAL S1, IPI-A S9, OCAI S15, COC S16
    item_n = c(32,   16,   8,    8),
    rel    = c(.92,  .71,  .51,  .19)
  )
  expect_true(all(abs(
    axis_reliability_sb(anchors$xi1, anchors$item_n) - anchors$rel
  ) <= .005))

  # The twelve banked non-blocked type-a rows: components (%gen, %axes, %scale,
  # %item; %block = 0 for all), item_n, Rel. strack2013.md Table 3.
  typea <- data.frame(
    row   = c("IAL1S", "IAL1O", "IAL2S", "IASR3S", "IASR3O", "IIP4S",
              "IIP5t1", "IIP5t2", "IIP6S", "IMI6O", "SASC8S", "IPIA9S"),
    gen   = c(2.1, 2.0, 2.9, 1.1, 1.4, 13.9, 16.6, 20.5, 17.7, 1.7, 4.8, 19.2),
    axes  = c(26.0, 26.1, 23.0, 22.9, 21.5, 11.8, 13.2, 11.8, 13.0, 27.9,
              17.8, 13.4),
    scale = c(6.5, 5.7, 5.3, 9.1, 8.7, 1.5, 1.5, 2.0, 2.4, 5.9, 6.2, 2.8),
    item  = c(65.4, 66.2, 68.8, 66.9, 68.4, 72.8, 68.7, 65.7, 67.9, 64.5,
              71.2, 64.6),
    item_n = c(32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 16),
    rel   = c(.92, .92, .90, .90, .90, .81, .83, .81, .81, .92, .87, .71)
  )
  typea$sum <- typea$gen + typea$axes + typea$scale + typea$item

  # Component-sum guard (RR10 Q4): each banked row sums to its banked total
  # +/-0.1; exactly one row (IIP S6 Self) is inconsistent, at 101.0.
  self_consistent <- abs(typea$sum - 100.0) <= 0.1
  expect_identical(typea$row[!self_consistent], "IIP6S")
  expect_true(abs(typea$sum[typea$row == "IIP6S"] - 101.0) <= 0.1)

  # Sweep the eleven self-consistent rows at +/-.01.
  sc <- typea[self_consistent, ]
  expect_true(all(abs(
    axis_reliability_sb(sc$axes / 100, sc$item_n) - sc$rel
  ) <= .01))

  # IIP S6 Self erratum (RR10 option a): the printed pair is inconsistent, and
  # the sum-restoring single-digit correction (%axes 13.0 -> 12.0) reproduces.
  expect_true(abs(axis_reliability_sb(.130, 32) - .81) > .01)   # printed pair
  expect_true(abs(axis_reliability_sb(.120, 32) - .81) <= .005) # corrected pair
})

test_that("BC2: SEm formula reproduces Table 3 col 13 (Layer A)", {
  # sqrt(col 12 raw variance) * sqrt(1 - col 11 rel) reproduces col 13 SEm
  # within +/-.02 for the IAL, OCAI, COC anchor rows. strack2013.md Table 3.
  bc2 <- data.frame(
    var = c(0.98, 15.95, 6.70), # IAL S1, OCAI S15, COC S16 (Self)
    rel = c(.92,  .51,   .19),
    sem = c(0.28, 2.78,  2.33)
  )
  expect_true(all(abs(
    axis_sem(bc2$rel, sd = sqrt(bc2$var)) - bc2$sem
  ) <= .02))
})

# The lavaan constraint set (T3). Build the exact population correlation matrix
# of an octant type-a instrument from known (xi1, xi2, zeta1) components (the
# package's axes_population_cor()), fit the flat fixed-links model through the
# sem_fit_cfa() chokepoint, and assert its structure on the fitted lavaan object
# (RR09 BC4). (Exact recovery of the components -- and the (N-1)/N rescaling --
# is the population-matrix oracle in T4/BC5; here the population matrix only
# guarantees a well-conditioned fit.)

test_that("BC4: fitted lavaan object has the intended constraint set", {
  skip_if_not_installed("lavaan")
  set.seed(486115)
  oct <- octants()
  k <- 4L # 32-item instrument -> item_n 16
  pop <- axes_population_cor(oct, k, xi1 = .26, xi2 = .05, zeta1 = .07)
  p <- nrow(pop$sigma)
  inames <- sprintf("i%02d", seq_len(p))
  items <- split(inames, pop$scale)

  dat <- as.data.frame(mvn_draws(3000L, rep(0, p), pop$sigma))
  dat <- as.data.frame(scale(dat)) # z-standardize: fit the correlation matrix
  colnames(dat) <- inames

  fit <- axes_fit(dat, items, oct)
  expect_true(lavaan::lavInspect(fit, "converged"))

  pt <- lavaan::parTable(fit)
  latents <- c("AX", "AY", "GEN", sprintf("SS%d", seq_along(oct)))

  # All loadings fixed (zero free loadings).
  expect_true(all(pt$free[pt$op == "=~"] == 0))

  # AX/AY variances equality-constrained (one shared, non-empty label).
  axl <- pt$label[pt$op == "~~" & pt$lhs == "AX" & pt$rhs == "AX"]
  ayl <- pt$label[pt$op == "~~" & pt$lhs == "AY" & pt$rhs == "AY"]
  expect_identical(axl, ayl)
  expect_true(nzchar(axl))

  # Every scale-specificity variance shares one label.
  ss <- pt$op == "~~" & pt$lhs == pt$rhs & grepl("^SS[0-9]+$", pt$lhs)
  expect_length(unique(pt$label[ss]), 1L)
  expect_true(nzchar(unique(pt$label[ss])))

  # Every latent covariance fixed at 0.
  lcov <- pt$op == "~~" & pt$lhs != pt$rhs &
    pt$lhs %in% latents & pt$rhs %in% latents
  expect_true(any(lcov))
  expect_true(all(pt$free[lcov] == 0))
  expect_true(all(pt$ustart[lcov] == 0))

  # Item errors free.
  ierr <- pt$op == "~~" & pt$lhs == pt$rhs & pt$lhs %in% inames
  expect_length(which(ierr), p)
  expect_true(all(pt$free[ierr] > 0))

  # df = p(p+1)/2 - p - 3 for the non-blocked model (3 latent variances +
  # p free errors; all loadings and latent covariances fixed).
  expect_equal(
    unname(lavaan::fitMeasures(fit, "df")),
    p * (p + 1) / 2 - p - 3
  )
})

test_that("BC5: exact population matrix recovers every component (Layer B)", {
  skip_if_not_installed("lavaan")
  oct <- octants()
  k <- 4L
  xi1 <- .15
  xi2 <- .08
  zeta1 <- .12
  eps <- 1 - xi1 - xi2 - zeta1
  pop <- axes_population_cor(oct, k, xi1, xi2, zeta1)
  sigma <- pop$sigma
  p <- nrow(sigma)
  inames <- sprintf("i%02d", seq_len(p))
  dimnames(sigma) <- list(inames, inames)
  items <- split(inames, pop$scale)

  # Fit the EXACT population matrix. lavaan's default ML rescales by the biased
  # (N-1)/N likelihood divisor (RR09's verified trap: .1497 for a true .15 at
  # N = 500); likelihood = "wishart" uses the N-1 divisor, recovering every
  # component to numerical precision at chisq = 0. This oracle path fits a
  # covariance matrix directly, so it does not route through sem_fit_cfa() (the
  # fiml/listwise raw-data chokepoint); the constraint set is axes_syntax()'s.
  fit <- lavaan::cfa(
    axes_syntax(items, oct),
    sample.cov = sigma, sample.nobs = 500L,
    orthogonal = TRUE, likelihood = "wishart"
  )
  expect_true(lavaan::lavInspect(fit, "converged"))

  pe <- lavaan::parameterEstimates(fit)
  vhat <- function(lat) {
    pe$est[pe$op == "~~" & pe$lhs == lat & pe$rhs == lat][[1]]
  }
  expect_lt(abs(vhat("AX") - xi1), 1e-4)
  expect_lt(abs(vhat("AY") - xi1), 1e-4)
  expect_lt(abs(vhat("GEN") - xi2), 1e-4)
  expect_lt(abs(vhat("SS1") - zeta1), 1e-4)

  # All item residual variances equal within 1e-6 (and recover the true eps).
  ehat <- pe$est[pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs %in% inames]
  expect_lt(max(ehat) - min(ehat), 1e-6)
  expect_lt(abs(mean(ehat) - eps), 1e-4)

  # Exact fit at the population matrix.
  expect_lt(unname(lavaan::fitMeasures(fit, "chisq")), 1e-6)
})

# Finite-sample Monte-Carlo recovery (T5). Simulate replicate item datasets from
# a known five-component population (axes_simulate(), the generator shared with
# the bundled example dataset), fit each, and check that the mean recovered xi1
# lands within 2 Monte-Carlo SEs of truth at two distinct xi1 levels (RR09 BC6).
# The near-unbiasedness is genuine, not seed-tuned: at N = 2000, reps = 150 the
# realized |bias|/MCSE is ~1 (well inside the 2-SE band) at both cells.

axes_mc_recover_xi1 <- function(oct, k, xi1, xi2, zeta1, n, reps, seed) {
  set.seed(seed)
  pop <- axes_population_cor(oct, k, xi1, xi2, zeta1)
  inames <- sprintf("item_%02d", seq_along(pop$scale))
  items <- split(inames, pop$scale)
  est <- numeric(reps)
  for (r in seq_len(reps)) {
    dat <- as.data.frame(scale(axes_simulate(n, oct, k, xi1, xi2, zeta1)))
    colnames(dat) <- inames
    pe <- lavaan::parameterEstimates(axes_fit(dat, items, oct))
    est[r] <- pe$est[pe$op == "~~" & pe$lhs == "AX" & pe$rhs == "AX"][[1]]
  }
  list(mean = mean(est), mcse = stats::sd(est) / sqrt(reps))
}

test_that("BC6: Monte-Carlo mean xi1 recovers truth within 2 MC-SEs (Layer B)", {
  skip_if_not_installed("lavaan")
  oct <- octants()
  cells <- list(
    list(xi1 = .10, seed = 11L),
    list(xi1 = .20, seed = 22L)
  )
  for (cell in cells) {
    mc <- axes_mc_recover_xi1(
      oct, k = 4L, xi1 = cell$xi1, xi2 = .05, zeta1 = .08,
      n = 2000L, reps = 150L, seed = cell$seed
    )
    expect_lt(abs(mc$mean - cell$xi1), 2 * mc$mcse)
  }
})

# Cross-engine oracle (T6). The identical flat fixed-links model, fit in OpenMx
# and in lavaan on the identical sample covariance, must agree on every free
# component variance (RR09 BC7). The axes structure has the closed form
#   Sigma = xi1*C + xi2*J + zeta1*B + diag(eps),
# where C_ij = cos(theta_i - theta_j) = AX AX' + AY AY' with the equal axis
# variance factored out, J is all-ones (the +1 general loadings), and B marks
# same-scale item pairs (the shared scale-specificity) -- exactly the lavaan
# constraint set. OpenMx is Suggests already (no new Imports; D-006/D-014).

axes_mx_components <- function(S, n, angles_deg, n_items) {
  p <- nrow(S)
  nm <- rownames(S)
  scale <- rep(seq_along(angles_deg), each = n_items)
  th <- rep(as.numeric(angles_deg), each = n_items) * pi / 180
  model <- OpenMx::mxModel(
    "axes",
    OpenMx::mxMatrix("Full", 1, 1, free = TRUE, values = .15, lbound = 0,
                     name = "xi1"),
    OpenMx::mxMatrix("Full", 1, 1, free = TRUE, values = .05, lbound = 0,
                     name = "xi2"),
    OpenMx::mxMatrix("Full", 1, 1, free = TRUE, values = .10, lbound = 0,
                     name = "zeta1"),
    OpenMx::mxMatrix("Full", p, 1, free = TRUE, values = .5, lbound = 0,
                     name = "eps"),
    OpenMx::mxMatrix("Full", p, p, free = FALSE,
                     values = outer(th, th, function(a, b) cos(a - b)),
                     name = "C"),
    OpenMx::mxMatrix("Full", p, p, free = FALSE, values = 1, name = "J"),
    OpenMx::mxMatrix("Full", p, p, free = FALSE,
                     values = outer(scale, scale, `==`) * 1, name = "B"),
    OpenMx::mxAlgebra(
      xi1[1, 1] * C + xi2[1, 1] * J + zeta1[1, 1] * B + vec2diag(eps),
      name = "Sigma", dimnames = list(nm, nm)
    ),
    OpenMx::mxData(observed = S, type = "cov", numObs = n),
    OpenMx::mxExpectationNormal(covariance = "Sigma"),
    OpenMx::mxFitFunctionML()
  )
  fit <- suppressWarnings(suppressMessages(
    OpenMx::mxRun(model, silent = TRUE, suppressWarnings = TRUE)
  ))
  c(
    xi1 = OpenMx::mxEval(xi1, fit)[1, 1],
    xi2 = OpenMx::mxEval(xi2, fit)[1, 1],
    zeta1 = OpenMx::mxEval(zeta1, fit)[1, 1]
  )
}

axes_lav_components <- function(S, n, items, angles_deg) {
  fit <- lavaan::cfa(
    axes_syntax(items, angles_deg),
    sample.cov = S, sample.nobs = n, orthogonal = TRUE, likelihood = "wishart"
  )
  pe <- lavaan::parameterEstimates(fit)
  vv <- function(lat) pe$est[pe$op == "~~" & pe$lhs == lat & pe$rhs == lat][[1]]
  c(xi1 = vv("AX"), xi2 = vv("GEN"), zeta1 = vv("SS1"))
}

test_that("BC7: lavaan and OpenMx agree on the component variances (Layer B)", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("OpenMx")
  oct <- octants()
  k <- 4L
  for (seed in c(7L, 8L)) {
    set.seed(seed)
    dat <- as.data.frame(scale(axes_simulate(2000L, oct, k, .15, .05, .10)))
    p <- ncol(dat)
    inames <- sprintf("i%02d", seq_len(p))
    colnames(dat) <- inames
    items <- split(inames, rep(seq_along(oct), each = k))
    S <- stats::cov(dat)

    lav <- axes_lav_components(S, 2000L, items, oct)
    mx <- axes_mx_components(S, 2000L, oct, k)
    expect_lt(max(abs(lav - mx)), 1e-3) # observed ~6e-5
  }
})

# Nunnally-Bernstein comparison (T7). BC8: a code-independent worked-example
# oracle for the N-B formula -- NOT Table 3 col 14, which is not recomputable
# from printed values (RR09 Q6). BC9: a synthetic high-scale-specificity cell
# reproducing the paper's Figure 3 headline (N-B overestimates axis reliability).

test_that("BC8: N-B formula matches a hand-worked example (independent route)", {
  # A worked X-axis octant weight pattern (0, +/-sqrt(.5), +/-1); scale-level
  # Sum wi^2 = 4.0 (Strack 2013 p. 3), decoupled from octants() ordering so the
  # pairing with rel_scale is explicit.
  w <- c(1, sqrt(.5), 0, -sqrt(.5), -1, -sqrt(.5), 0, sqrt(.5))
  rel_scale <- c(.82, .78, .80, .76, .84, .79, .81, .77)
  var_axis <- 3.9
  expect_equal(sum(w^2), 4.0)

  # Hand-worked oracle: Sum wi^2 (1 - rel_i) with wi^2 = {1, .5, 0, .5, ...}:
  #   1(.18) + .5(.22) + .5(.24) + 1(.16) + .5(.21) + .5(.23) = .79
  #   NB = 1 - .79 / 3.9 = 0.7974358974358974
  expect_equal(
    axis_reliability_nb(w, rel_scale, var_axis),
    0.7974358974358974,
    tolerance = 1e-6
  )

  # Independent route: an explicit scalar accumulation over scales.
  err <- 0
  for (s in seq_along(w)) err <- err + w[s]^2 * (1 - rel_scale[s])
  expect_equal(axis_reliability_nb(w, rel_scale, var_axis), 1 - err / var_axis,
    tolerance = 1e-12)
})

# Compute the X-axis N-B reliability from raw item data (the full N-B path):
# per-scale Cronbach alpha, the z-standardized weighted scale composite, its
# variance, then the N-B formula.
axes_nb_from_data <- function(dat, items, angles_deg) {
  w <- axis_weights(angles_deg)[, "w_x"]
  rel_scale <- vapply(
    items, function(nm) cronbach_alpha(dat[, nm, drop = FALSE]), numeric(1)
  )
  sscore <- scale(vapply(
    items, function(nm) rowMeans(dat[, nm, drop = FALSE]), numeric(nrow(dat))
  ))
  var_axis <- stats::var(as.numeric(sscore %*% w))
  axis_reliability_nb(w, rel_scale, var_axis)
}

test_that("BC9: N-B overestimates vs CFA at high scale-specificity (Figure 3)", {
  skip_if_not_installed("lavaan")
  oct <- octants()
  k <- 4L
  # High scale-specificity cell: zeta1 = .45 (>= .40 of item variance),
  # xi1 = .12 (<= .15 axes). The paper's MEIL/CV-LI regime (p. 8).
  set.seed(913)
  dat <- axes_simulate(5000L, oct, k, xi1 = .12, xi2 = .03, zeta1 = .45)
  p <- ncol(dat)
  inames <- sprintf("i%02d", seq_len(p))
  colnames(dat) <- inames
  items <- split(inames, rep(seq_along(oct), each = k))

  # CFA/SB reliability (X axis).
  zdat <- as.data.frame(scale(dat))
  colnames(zdat) <- inames
  pe <- lavaan::parameterEstimates(axes_fit(zdat, items, oct))
  xi1hat <- pe$est[pe$op == "~~" & pe$lhs == "AX" & pe$rhs == "AX"][[1]]
  cfa_rel <- axis_reliability_sb(xi1hat, axis_item_n(oct, k)[["x"]])

  # N-B reliability (X axis) from the raw scale scores.
  nb_rel <- axes_nb_from_data(dat, items, oct)

  expect_gt(nb_rel - cfa_rel, .05) # pre-registered margin; observed ~.21
})
