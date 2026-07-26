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
  # Levels pinned explicitly so the scale->angle pairing is stated rather than
  # inferred. (split() on a NUMERIC group vector already orders levels
  # numerically, so this is equivalent, not a fix -- only a CHARACTER group
  # vector would sort "10" before "2".)
  items <- split(inames, factor(pop$scale, levels = seq_along(oct)))
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
    items <- split(inames, factor(rep(seq_along(oct), each = k),
                                  levels = seq_along(oct))) # levels stated, see above
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

# Refuse / boundary / listwise contract (T8). BC11 boundary, BC12 refusals,
# BC13 listwise policy on the exported axes_reliability().

# A valid octant item dataset + its explicit item map, for perturbing.
axes_valid_fixture <- function(n = 1500L, k = 4L, xi1 = .20, seed = 42L) {
  oct <- octants()
  set.seed(seed)
  dat <- axes_simulate(n, oct, k, xi1, .05, .08)
  inames <- sprintf("i%02d", seq_len(ncol(dat)))
  colnames(dat) <- inames
  list(
    data = dat, oct = oct,
    items = split(inames, rep(seq_along(oct), each = k)),
    names = inames
  )
}

test_that("BC11: small positive xi1 gives a small reliability, not a boundary", {
  skip_if_not_installed("lavaan")
  oct <- octants()
  set.seed(2)
  dat <- axes_simulate(4000L, oct, 2L, xi1 = .028, xi2 = .04, zeta1 = .05)
  inames <- sprintf("j%02d", seq_len(ncol(dat)))
  colnames(dat) <- inames
  items <- split(inames, rep(seq_along(oct), each = 2L))
  res <- suppressMessages(axes_reliability(dat, items = items, angles = oct))
  expect_false(res$details$boundary)
  # COC-style small-but-real reliability -- never NA, negative, or zero.
  expect_gt(res$results$reliability[[1]], 0)
  expect_lt(res$results$reliability[[1]], .40)
})

test_that("BC11: a boundary fit (xi1 <= 0) returns NA + warning + flag", {
  skip_if_not_installed("lavaan")
  oct <- octants()
  # xi1 = 0 population, small N: seed 5 yields a negative xi1-hat (a boundary).
  set.seed(5)
  dat <- axes_simulate(400L, oct, 4L, xi1 = 0, xi2 = .05, zeta1 = .40)
  inames <- sprintf("i%02d", seq_len(ncol(dat)))
  colnames(dat) <- inames
  items <- split(inames, rep(seq_along(oct), each = 4L))
  expect_warning(
    res <- suppressMessages(axes_reliability(dat, items = items, angles = oct)),
    "boundary"
  )
  expect_true(res$details$boundary)
  expect_true(all(is.na(res$results$reliability)))
  expect_true(all(is.na(res$results$sem)))
  expect_lt(res$results$xi1[[1]], 0) # recorded, never clipped to 0
})

test_that("BC12: each malformed input errors informatively", {
  skip_if_not_installed("lavaan")
  fx <- axes_valid_fixture()
  ok <- function() suppressMessages(
    axes_reliability(fx$data, items = fx$items, angles = fx$oct)
  )
  expect_no_error(ok()) # the fixture itself is valid

  # A 7-scale subset of the octants is no longer refused for its COUNT (M60
  # accepts any k >= 4) but for its spacing -- dropping one octant leaves a
  # 90-degree gap among 45-degree ones.
  expect_error(
    suppressMessages(axes_reliability(
      fx$data, items = fx$items[1:7], angles = fx$oct[1:7]
    )),
    "equally spaced"
  )
  # unequal spacing
  bad_ang <- fx$oct
  bad_ang[[1]] <- bad_ang[[1]] + 5
  expect_error(
    suppressMessages(axes_reliability(fx$data, items = fx$items, angles = bad_ang)),
    "equally spaced"
  )
  # duplicate angle
  dup_ang <- fx$oct
  dup_ang[[2]] <- dup_ang[[1]]
  expect_error(
    suppressMessages(axes_reliability(fx$data, items = fx$items, angles = dup_ang)),
    "duplicat"
  )
  # NA angle
  na_ang <- fx$oct
  na_ang[[3]] <- NA_real_
  expect_error(
    suppressMessages(axes_reliability(fx$data, items = fx$items, angles = na_ang)),
    "missing"
  )
  # a scale with < 2 items
  one_item <- fx$items
  one_item[[1]] <- one_item[[1]][1]
  expect_error(
    suppressMessages(axes_reliability(fx$data, items = one_item, angles = fx$oct)),
    "at least 2 items"
  )
  # item absent from data
  absent <- fx$items
  absent[[1]][[1]] <- "not_a_column"
  expect_error(
    suppressMessages(axes_reliability(fx$data, items = absent, angles = fx$oct)),
    "not found"
  )
  # non-finite value
  inf_data <- fx$data
  inf_data[[1, fx$names[[1]]]] <- Inf
  expect_error(
    suppressMessages(axes_reliability(inf_data, items = fx$items, angles = fx$oct)),
    "non-finite"
  )
  # zero-variance item
  zv_data <- fx$data
  zv_data[[fx$names[[1]]]] <- 1
  expect_error(
    suppressMessages(axes_reliability(zv_data, items = fx$items, angles = fx$oct)),
    "Zero-variance"
  )
  # complete-case N <= p
  small <- axes_valid_fixture(n = 20L)
  expect_error(
    suppressMessages(axes_reliability(small$data, items = small$items, angles = small$oct)),
    "Complete-case N"
  )
  # non-positive-definite correlation matrix (a duplicated item column)
  pd_data <- fx$data
  pd_data[[fx$names[[2]]]] <- pd_data[[fx$names[[1]]]]
  expect_error(
    suppressMessages(axes_reliability(pd_data, items = fx$items, angles = fx$oct)),
    "positive definite"
  )
  # lavaan non-convergence (via the mockable seam)
  testthat::local_mocked_bindings(axes_converged = function(fit) FALSE)
  expect_error(
    suppressMessages(axes_reliability(fx$data, items = fx$items, angles = fx$oct)),
    "did not converge"
  )
})

# OLS-shadow estimator (T9, B-1). A SEM-independent least-squares recovery of
# the component variances from the off-diagonal correlations -- a third
# independent route beside lavaan and OpenMx, and the fit's start values.

test_that("OLS-shadow recovers the components exactly on the population matrix", {
  oct <- octants()
  k <- 4L
  truth <- c(xi2 = .08, xi1 = .15, zeta1 = .12)
  pop <- axes_population_cor(oct, k, truth[["xi1"]], truth[["xi2"]], truth[["zeta1"]])
  item_angle <- rep(oct, each = k)
  ols <- axes_ols_shadow(pop$sigma, item_angle, pop$scale)
  expect_equal(ols[["xi2"]], truth[["xi2"]], tolerance = 1e-8)
  expect_equal(ols[["xi1"]], truth[["xi1"]], tolerance = 1e-8)
  expect_equal(ols[["zeta1"]], truth[["zeta1"]], tolerance = 1e-8)
})

test_that("OLS-shadow cross-checks the CFA estimate on finite data", {
  skip_if_not_installed("lavaan")
  oct <- octants()
  k <- 4L
  set.seed(3)
  dat <- axes_simulate(2000L, oct, k, xi1 = .15, xi2 = .08, zeta1 = .12)
  inames <- sprintf("i%02d", seq_len(ncol(dat)))
  colnames(dat) <- inames
  items <- split(inames, rep(seq_along(oct), each = k))
  res <- suppressMessages(axes_reliability(dat, items = items, angles = oct))
  # The stored SEM-independent OLS-shadow agrees with the CFA axes variance.
  expect_lt(abs(res$details$ols_shadow[["xi1"]] - res$results$xi1[[1]]), 1e-2)
})

test_that("start values do not change the converged CFA estimates", {
  skip_if_not_installed("lavaan")
  oct <- octants()
  k <- 4L
  set.seed(4)
  dat <- as.data.frame(scale(axes_simulate(2000L, oct, k, .15, .08, .12)))
  inames <- sprintf("i%02d", seq_len(ncol(dat)))
  colnames(dat) <- inames
  items <- split(inames, rep(seq_along(oct), each = k))
  xi1_no <- lavaan::parameterEstimates(
    suppressWarnings(axes_fit(dat, items, oct))
  )
  xi1_no <- xi1_no$est[xi1_no$lhs == "AX" & xi1_no$op == "~~"][1]
  xi1_st <- lavaan::parameterEstimates(
    suppressWarnings(axes_fit(dat, items, oct, start = c(xi1 = .15, xi2 = .08, zeta1 = .12)))
  )
  xi1_st <- xi1_st$est[xi1_st$lhs == "AX" & xi1_st$op == "~~"][1]
  expect_equal(xi1_no, xi1_st, tolerance = 1e-6)
})

test_that("BC13: listwise deletion reports N and refuses when N <= p", {
  skip_if_not_installed("lavaan")
  fx <- axes_valid_fixture(n = 1500L)
  # Missingness is removed listwise, and the complete-case N is reported.
  miss <- fx$data
  miss[1:100, fx$names[[1]]] <- NA
  expect_message(
    axes_reliability(miss, items = fx$items, angles = fx$oct),
    "1400 complete case"
  )
  # Enough rows overall, but too few complete cases -> refuse.
  fewcc <- fx$data
  fewcc[26:1500, fx$names[[1]]] <- NA # only 25 complete cases, p = 32
  expect_error(
    suppressMessages(axes_reliability(fewcc, items = fx$items, angles = fx$oct)),
    "Complete-case N"
  )
})

# S3 object, methods, and the bundled example dataset (T10, AC14).

test_that("AC14: axes_reliability() runs on the bundled simulated_items data", {
  skip_if_not_installed("lavaan")
  data("simulated_items", package = "circumplex", envir = environment())
  expect_equal(dim(simulated_items), c(500L, 32L))

  # The exact call from the (non-\dontrun) help example.
  items <- split(names(simulated_items), rep(1:8, each = 4))
  res <- suppressMessages(
    axes_reliability(simulated_items, items = items, angles = octants())
  )
  expect_s3_class(res, "circumplex_axes_reliability")
  expect_false(res$details$boundary)
  # A sensible, non-degenerate reliability from the .18 axes-variance design.
  expect_gt(res$results$reliability[[1]], .6)
  expect_lt(res$results$reliability[[1]], .9)

  # print()/summary() dispatch and return the object invisibly.
  expect_output(print(res), "Circumplex Axes Reliability")
  expect_output(summary(res), "Variance components")
  expect_identical(suppressMessages(print(res)), res)
})

test_that("axes_reliability() works via the instrument path", {
  skip_if_not_installed("lavaan")
  # A simulated 32-item dataset matching iipsc's item->scale->angle mapping.
  data("iipsc", package = "circumplex", envir = environment())
  key <- iipsc$Scales
  item_scale <- integer(32)
  item_angle <- numeric(32)
  for (s in seq_len(nrow(key))) {
    nums <- as.integer(strsplit(key$Items[[s]], ",")[[1]])
    item_scale[nums] <- s
    item_angle[nums] <- key$Angle[[s]]
  }
  th <- item_angle * pi / 180
  sigma <- .06 + .18 * outer(th, th, function(a, b) cos(a - b)) +
    .10 * outer(item_scale, item_scale, `==`)
  diag(sigma) <- 1
  set.seed(99)
  dat <- as.data.frame(mvn_draws(800L, rep(0, 32), sigma))
  colnames(dat) <- sprintf("item%02d", seq_len(32))

  res <- suppressMessages(
    axes_reliability(dat, items = seq_len(32), instrument = iipsc)
  )
  expect_s3_class(res, "circumplex_axes_reliability")
  expect_false(res$details$boundary)
  # The instrument path must reproduce the explicit-map result on the same data
  # (split() orders the groups 1..8, matching key$Angle's row order).
  emap <- split(colnames(dat), item_scale)
  res2 <- suppressMessages(
    axes_reliability(dat, items = emap, angles = as.numeric(key$Angle))
  )
  expect_equal(res$results$xi1[[1]], res2$results$xi1[[1]], tolerance = 1e-6)
})

# --- M59: the cormat input path -----------------------------------------------

# AC2 round-trip oracle. This is an EXACT oracle, not an approximate one, and the
# reason is worth stating because the tolerance below would otherwise look
# suspiciously tight. The raw path z-standardizes the complete cases (scale(),
# N-1 divisor) and hands lavaan the frame; lavaan's default ML likelihood then
# rescales the N-1 covariance of those z-scores -- which is exactly cor(mat) --
# by (N-1)/N. Passing sample.cov = cor(mat) with sample.nobs = N under the SAME
# default likelihood applies the same (N-1)/N rescaling, so both paths hand the
# optimizer bit-identical moments. RR09 BC5's trap is thereby handled by matching
# it, not by widening tolerance: likelihood = "wishart" (which the BC5 population
# oracle above uses, for the different purpose of recovering an exact truth)
# would BREAK this equivalence by precisely (N-1)/N.

axes_rt_compare <- function(raw, cm, tol = 1e-6) {
  expect_equal(cm$results$xi1, raw$results$xi1, tolerance = tol)
  expect_equal(cm$results$item_n, raw$results$item_n, tolerance = tol)
  expect_equal(cm$results$reliability, raw$results$reliability, tolerance = tol)
  expect_equal(cm$results$sem, raw$results$sem, tolerance = tol)
  expect_equal(cm$components$Estimate, raw$components$Estimate, tolerance = tol)
  expect_equal(cm$components$SE, raw$components$SE, tolerance = tol)
  expect_equal(cm$fit$df, raw$fit$df)
  expect_equal(cm$fit$chisq, raw$fit$chisq, tolerance = tol)
}

test_that("AC2: the cormat path reproduces the raw path exactly", {
  skip_if_not_installed("lavaan")

  # Dataset 1: the bundled 32-item example.
  data("simulated_items", package = "circumplex", envir = environment())
  items1 <- split(names(simulated_items), rep(1:8, each = 4))
  raw1 <- suppressMessages(
    axes_reliability(simulated_items, items = items1, angles = octants())
  )
  cm1 <- suppressMessages(axes_reliability(
    cormat = stats::cor(simulated_items), items = items1,
    angles = octants(), n = nrow(simulated_items)
  ))
  axes_rt_compare(raw1, cm1)

  # Dataset 2: a 16-item draw at a different component design, so the
  # equivalence is not an artifact of one instrument size or one xi1 level.
  oct <- octants()
  set.seed(2059)
  dat2 <- axes_simulate(700L, oct, 2L, xi1 = .22, xi2 = .05, zeta1 = .14)
  items2 <- split(names(dat2), rep(1:8, each = 2))
  raw2 <- suppressMessages(
    axes_reliability(dat2, items = items2, angles = oct)
  )
  cm2 <- suppressMessages(axes_reliability(
    cormat = stats::cor(dat2), items = items2, angles = oct, n = nrow(dat2)
  ))
  axes_rt_compare(raw2, cm2)
})

# AC3(a): the deterministic population-matrix oracle, driven through the PUBLIC
# cormat path (BC5 above fits lavaan directly, so it does not exercise the
# argument handling, subsetting, or reordering this milestone adds).
#
# The public path deliberately keeps lavaan's default likelihood = "normal" so
# it agrees with the raw path exactly (see the AC2 note), and that convention
# rescales sample.cov by (N-1)/N. Rather than hide that behind a loose
# tolerance, the second block below PINS it: the recovered component is truth *
# (n-1)/n to numerical precision, at three sample sizes. Recovery within 1e-4
# then follows from choosing an n where that exact offset is below the bar.

test_that("AC3(a): the population matrix recovers every component (cormat path)", {
  skip_if_not_installed("lavaan")
  oct <- octants()
  k <- 4L
  xi1 <- .15
  xi2 <- .08
  zeta1 <- .12
  pop <- axes_population_cor(oct, k, xi1, xi2, zeta1)
  sigma <- pop$sigma
  inames <- sprintf("i%02d", seq_len(nrow(sigma)))
  dimnames(sigma) <- list(inames, inames)
  items <- split(inames, pop$scale)

  # The offset is exactly (n-1)/n -- measured identical at n = 500, 5e3, 5e4.
  for (n in c(500L, 5000L, 50000L)) {
    res <- suppressMessages(
      axes_reliability(cormat = sigma, items = items, angles = oct, n = n)
    )
    est <- res$components$Estimate
    # 1e-6, not the 1e-8 first written here: that passed at ~3e-11 relative on
    # macOS and failed on CI at 1.3e-8 (optimizer precision is platform- and
    # BLAS-dependent -- the M20 family). 1e-6 keeps the discrimination that
    # matters, since the alternative hypothesis this pins down (no (n-1)/n
    # rescaling at all) is off by relative 1/n = 2e-3 / 2e-4 / 2e-5 at the three
    # cells -- still 20x the tolerance at the tightest one, and 77x above the
    # observed cross-platform noise.
    expect_equal(est[[2]], xi1 * (n - 1) / n, tolerance = 1e-6)   # axes
    expect_equal(est[[1]], xi2 * (n - 1) / n, tolerance = 1e-6)   # general
    expect_equal(est[[3]], zeta1 * (n - 1) / n, tolerance = 1e-6) # scale
    expect_lt(res$fit$chisq, 1e-6)
  }

  # AC3(a) proper: at an n where that exact offset (xi1/n = 3e-6) is below the
  # 1e-4 bar, the public path recovers the truth outright.
  res <- suppressMessages(
    axes_reliability(cormat = sigma, items = items, angles = oct, n = 50000L)
  )
  est <- res$components$Estimate
  expect_lt(abs(est[[2]] - xi1), 1e-4)
  expect_lt(abs(est[[1]] - xi2), 1e-4)
  expect_lt(abs(est[[3]] - zeta1), 1e-4)
  expect_lt(res$fit$chisq, 1e-6)

  # The matrix may arrive in any column order, and a permuted cormat must give
  # the identical answer. Note carefully WHICH quantity fences the reordering
  # step: lavaan matches `sample.cov` by dimnames itself, so
  # components$Estimate is invariant to a permuted input whether or not the
  # package reorders (measured: max |diff| exactly 0 with the reorder removed).
  # The quantity the reorder actually determines is the OLS shadow, which is
  # built POSITIONALLY from R against item_angle/item_scale -- without the
  # reorder its xi1 collapses from .15 to ~2.6e-4. So assert the shadow here,
  # mirroring the raw path's OLS-shadow check, or the reorder ships unfenced.
  set.seed(59)
  perm <- sample(nrow(sigma))
  res_p <- suppressMessages(axes_reliability(
    cormat = sigma[perm, perm], items = items, angles = oct, n = 50000L
  ))
  # 1e-8 rather than 1e-10 for the same portability reason as above; the
  # reorder is fenced by the ols_shadow assertions below, not by this one.
  expect_equal(res_p$components$Estimate, est, tolerance = 1e-8)
  # ols_shadow comes from a closed-form qr.solve() on a fixed design matrix,
  # not an optimizer, so it is stable across platforms at this tolerance.
  expect_equal(res$details$ols_shadow[["xi1"]], xi1, tolerance = 1e-8)
  expect_equal(res$details$ols_shadow[["xi2"]], xi2, tolerance = 1e-8)
  expect_equal(res$details$ols_shadow[["zeta1"]], zeta1, tolerance = 1e-8)
  expect_equal(res_p$details$ols_shadow, res$details$ols_shadow, tolerance = 1e-8)
})

# AC3(b): cross-engine. OpenMx and the public cormat path fit the identical
# model to the identical correlation matrix, agreeing to ~1.9e-5 on both seeds.
#
# What that residual is NOT: the two engines' likelihood normalization. That
# was the obvious explanation and it is measurably false -- correcting the
# lavaan side for (N-1)/N makes agreement WORSE, and pairing OpenMx against a
# wishart-likelihood lavaan fit (the convention the raw-data BC7 companion
# above uses) is looser still at 5.5e-5 to 6.5e-5. Empirically OpenMx's
# type="cov"/numObs convention sits closer to lavaan's default "normal" than to
# "wishart", so the public path is the tighter pairing, and what remains is
# ordinary disagreement between two different optimizers on one matrix.
#
# The assertions encode both facts rather than absorbing them into a loose
# bar: a 2e-4 bar (10x the observed disagreement, and 5x tighter than AC3(b)'s
# 1e-3, which a tighter test still satisfies), plus a direct check that the
# disagreement is SMALLER than the (N-1)/N offset would be -- which is what
# falsifies the normalization explanation instead of merely not testing it.
# OpenMx is already Suggests; no new Imports (D-006/D-014).

test_that("AC3(b): lavaan and OpenMx agree on the cormat path (Layer B)", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("OpenMx")
  oct <- octants()
  k <- 4L
  n <- 2000L
  for (seed in c(7L, 8L)) {
    set.seed(seed)
    dat <- as.data.frame(scale(axes_simulate(n, oct, k, .15, .05, .10)))
    inames <- sprintf("i%02d", seq_len(ncol(dat)))
    colnames(dat) <- inames
    items <- split(inames, rep(seq_along(oct), each = k))
    R <- stats::cor(dat)

    res <- suppressMessages(
      axes_reliability(cormat = R, items = items, angles = oct, n = n)
    )
    lav <- c(
      xi1 = res$components$Estimate[[2]],
      xi2 = res$components$Estimate[[1]],
      zeta1 = res$components$Estimate[[3]]
    )
    mx <- axes_mx_components(R, n, oct, k)
    disagreement <- max(abs(lav - mx))
    expect_lt(disagreement, 5e-4) # observed 1.95e-5 (seed 7), 1.92e-5 (seed 8)
    # Falsify the normalization explanation COMPARATIVELY, not against an
    # absolute threshold. If the residual were the (N-1)/N convention gap, then
    # pairing OpenMx against a wishart-likelihood lavaan fit -- which matches
    # OpenMx's own convention -- would agree BETTER. It agrees worse (measured
    # 6.5e-5 seed 7, 5.5e-5 seed 8, against 1.9e-5 for the shipped pairing).
    # A ratio is the portable form of the claim: both sides absorb platform
    # optimizer noise together, where a bound sitting 4x from the offset would
    # be one BLAS difference away from a false failure.
    wishart <- axes_lav_components(R, n, items, oct)
    expect_lt(disagreement, max(abs(wishart - mx)))
  }
})

# AC4 + AC5: what the cormat path reports NA for, refuses, and errors on.

test_that("AC4: N-B is NA with a stated reason and sd = 'raw' is refused", {
  skip_if_not_installed("lavaan")
  data("simulated_items", package = "circumplex", envir = environment())
  items <- split(names(simulated_items), rep(1:8, each = 4))
  R <- stats::cor(simulated_items)

  res <- suppressMessages(
    axes_reliability(cormat = R, items = items, angles = octants(), n = 500L)
  )
  # NA-with-reason (RR09 sec. 7.4): the column stays, carrying NA, and both
  # print() and summary() say why -- never silently dropped.
  expect_true("nb_reliability" %in% names(res$results))
  expect_true(all(is.na(res$results$nb_reliability)))
  expect_output(print(res), "Nunnally-Bernstein comparison needs the raw item")
  expect_output(summary(res), "Nunnally-Bernstein comparison needs the raw item")
  expect_output(print(res), "Input:\\s+correlation matrix")
  expect_output(print(res), "Sample N:")

  # "std" and numeric sd work; "raw" is refused with the reason.
  expect_false(is.na(res$results$sem[[1]]))
  res_num <- suppressMessages(axes_reliability(
    cormat = R, items = items, angles = octants(), n = 500L, sd = c(2, 3)
  ))
  expect_equal(
    res_num$results$sem,
    c(2, 3) * sqrt(1 - res$results$reliability),
    tolerance = 1e-10
  )
  expect_error(
    axes_reliability(cormat = R, items = items, angles = octants(), n = 500L,
                     sd = "raw"),
    "needs the raw scale scores"
  )
})

test_that("AC5: each malformed cormat/n input errors informatively", {
  skip_if_not_installed("lavaan")
  data("simulated_items", package = "circumplex", envir = environment())
  items <- split(names(simulated_items), rep(1:8, each = 4))
  oct <- octants()
  R <- stats::cor(simulated_items)
  p <- ncol(R)
  ok <- function(...) axes_reliability(items = items, angles = oct, ...)

  # Exactly one of data / cormat.
  expect_error(
    axes_reliability(simulated_items, items = items, angles = oct, cormat = R,
                     n = 500L),
    "exactly one of `data` or `cormat`"
  )
  expect_error(axes_reliability(items = items, angles = oct),
               "exactly one of `data` or `cormat`")
  # `n` belongs to the cormat path only.
  expect_error(
    axes_reliability(simulated_items, items = items, angles = oct, n = 500L),
    "applies only to the `cormat` path"
  )

  # Shape and content of the matrix.
  expect_error(ok(cormat = R[1:4, ], n = 500L), "must be a square matrix")
  expect_error(ok(cormat = unname(R), n = 500L), "must have dimnames")
  # Half-named is the shape as.matrix(read.csv(...)) produces -- colnames kept,
  # rownames dropped -- i.e. the default outcome of transcribing a published
  # matrix, which is the workflow this path exists for. It must reach the
  # informative refusal, not a bare "subscript out of bounds" from the subset.
  half <- R
  rownames(half) <- NULL
  expect_error(ok(cormat = half, n = 500L), "must have dimnames")
  colonly <- R
  colnames(colonly) <- NULL
  expect_error(ok(cormat = colonly, n = 500L), "must have dimnames")
  # Names present on both dimensions but in different orders would silently
  # mis-subset (rows read in one order, columns in another).
  scrambled <- R
  rownames(scrambled) <- rev(colnames(R))
  expect_error(ok(cormat = scrambled, n = 500L), "must have dimnames")
  asym <- R
  asym[1, 2] <- asym[1, 2] + .1
  expect_error(ok(cormat = asym, n = 500L), "must be symmetric")
  nonunit <- R
  diag(nonunit)[1] <- 1.5
  expect_error(ok(cormat = nonunit, n = 500L), "must have a unit diagonal")
  nafill <- R
  nafill[1, 2] <- nafill[2, 1] <- NA_real_
  expect_error(ok(cormat = nafill, n = 500L), "missing or non-finite")
  # Singular: two items made perfectly collinear (still symmetric, unit diag).
  sing <- R
  sing[1, 2] <- sing[2, 1] <- 1
  expect_error(ok(cormat = sing, n = 500L), "not positive definite")
  # An item the matrix does not carry.
  bad_items <- items
  bad_items[[1]][[1]] <- "not_an_item"
  expect_error(
    axes_reliability(cormat = R, items = bad_items, angles = oct, n = 500L),
    "not found in `cormat`"
  )

  # The sample size.
  expect_error(ok(cormat = R), "`n` \\(the sample size\\) is required")
  expect_error(ok(cormat = R, n = "500"), "single whole number")
  expect_error(ok(cormat = R, n = c(100L, 200L)), "single whole number")
  expect_error(ok(cormat = R, n = 500.5), "single whole number")
  expect_error(ok(cormat = R, n = NA_integer_), "single whole number")
  # Inf slips is_scalar_count() (ceiling(Inf) == floor(Inf)) AND the n <= p
  # comparison, so it needs the explicit is.finite() guard.
  expect_error(ok(cormat = R, n = Inf), "single whole number")
  expect_error(ok(cormat = R, n = p), "greater than the number of items")
  expect_error(ok(cormat = R, n = p - 1L), "greater than the number of items")
})

# M60 axes_reliability(): any equally spaced angle set, any rotation ----------

# A fixture at an arbitrary equally spaced angle set. Unlike
# axes_valid_fixture(), the scale count is a parameter, so the split() group
# levels are pinned explicitly, stating the scale->angle pairing rather than
# leaving it to coercion. This is equivalence, not a repair: split() coerces a
# NUMERIC group vector with factor(), whose levels sort numerically, so 1:12
# already pairs correctly. Only a CHARACTER group vector would sort "10" before
# "2" -- the hazard the M33/M34 ordering lessons describe.
axes_spaced_fixture <- function(angles, n = 1500L, k = 4L, xi1 = .20,
                                seed = 42L) {
  set.seed(seed)
  dat <- axes_simulate(n, angles, k, xi1, .05, .08)
  inames <- sprintf("i%02d", seq_len(ncol(dat)))
  colnames(dat) <- inames
  grp <- factor(rep(seq_along(angles), each = k), levels = seq_along(angles))
  list(data = dat, angles = angles, items = split(inames, grp), names = inames)
}

test_that("M60: a rotated equally spaced set estimates (Strack type b)", {
  skip_if_not_installed("lavaan")
  # Type b: eight scales at 45 deg spacing, rotated 22.5 deg off the axes
  # (strack2013 p. 2 -- weights +/-.38268 and +/-.92388).
  ang <- seq(22.5, 337.5, by = 45)
  fx <- axes_spaced_fixture(ang)
  res <- suppressMessages(
    axes_reliability(fx$data, items = fx$items, angles = fx$angles)
  )
  expect_s3_class(res, "circumplex_axes_reliability")
  expect_true(all(is.finite(res$results$reliability)))
  expect_true(all(res$results$reliability > 0 & res$results$reliability < 1))
  # The equal-axis-variance restriction makes both axes agree at any rotation.
  expect_equal(res$results$reliability[[1]], res$results$reliability[[2]],
               tolerance = 1e-6)
  # The type-b magnitudes actually reached the model.
  # as.vector(): unique() on a matrix works ROWWISE, so it would return rows
  # rather than the distinct weight magnitudes.
  w <- as.vector(abs(axis_weights(ang)))
  expect_equal(sort(unique(round(w, 5))), c(0.38268, 0.92388))
})

test_that("M60: equally spaced sets with k != 8 estimate", {
  skip_if_not_installed("lavaan")
  for (k in c(6L, 12L)) {
    ang <- (seq_len(k) - 1L) * (360 / k)
    fx <- axes_spaced_fixture(ang, n = 2000L)
    res <- suppressMessages(
      axes_reliability(fx$data, items = fx$items, angles = fx$angles)
    )
    expect_true(all(is.finite(res$results$reliability)), label = paste("k =", k))
    expect_equal(res$results$reliability[[1]], res$results$reliability[[2]],
                 tolerance = 1e-6)
    expect_identical(res$details$n_scales, k)
  }
})

test_that("M60: the refusal contract survives the relaxation", {
  skip_if_not_installed("lavaan")
  fx <- axes_spaced_fixture(octants())
  bad <- function(angles = fx$angles, items = fx$items) {
    suppressMessages(axes_reliability(fx$data, items = items, angles = angles))
  }

  # Unequal spacing stays refused -- a quasi-circumplex is out of scope, and the
  # tolerance admits float representation error only (RR09 section 4).
  ang <- fx$angles
  ang[[1]] <- ang[[1]] + 5
  expect_error(bad(ang), "equally spaced")
  # A departure far too small to be a real design, but far larger than float
  # noise, is still refused.
  ang2 <- fx$angles
  ang2[[1]] <- ang2[[1]] + 1e-4
  expect_error(bad(ang2), "equally spaced")

  # Duplicates, NAs.
  dup <- fx$angles
  dup[[2]] <- dup[[1]]
  expect_error(bad(dup), "duplicat")
  na_ang <- fx$angles
  na_ang[[3]] <- NA_real_
  expect_error(bad(na_ang), "missing")

  # Fewer than 4 scales: at k = 3 every cross-scale pair carries the same
  # cos(delta) = -0.5, so the moment design (cos delta, 1, same-scale) drops to
  # rank 2 and the three components are not separately identified.
  expect_error(bad(c(0, 120, 240), fx$items[1:3]), "identif")
  expect_error(bad(c(0, 180), fx$items[1:2]), "at least 4")

  # Fewer than 2 items on a scale (M61 territory, still refused here).
  one <- fx$items
  one[[1]] <- one[[1]][1]
  expect_error(bad(items = one), "at least 2 items")

  # A non-finite angle must be REFUSED BY NAME, not carried into the fit.
  # anyNA() does not reject Inf, and `Inf %% 360` is NaN which sort() drops, so
  # without an is.finite() gate the spacing test reads the SURVIVING angles as
  # equally spaced and the run dies later in qr.solve() naming nothing.
  inf_ang <- fx$angles
  inf_ang[[2]] <- Inf
  expect_error(bad(inf_ang), "must be finite")
  expect_error(bad(inf_ang), "scale\\(s\\) 2")
  neg_inf <- fx$angles
  neg_inf[[5]] <- -Inf
  expect_error(bad(neg_inf), "must be finite")
})

test_that("M60: the spacing test is modular at the pole", {
  skip_if_not_installed("lavaan")
  fx <- axes_spaced_fixture(octants())
  # octants() carries LM = 360; the same set written with 0 must behave
  # identically, not read as unequally spaced.
  zero_form <- as.numeric(fx$angles)
  zero_form[zero_form == 360] <- 0
  a <- suppressMessages(
    axes_reliability(fx$data, items = fx$items, angles = fx$angles)
  )
  b <- suppressMessages(
    axes_reliability(fx$data, items = fx$items, angles = zero_form)
  )
  expect_equal(a$results$reliability, b$results$reliability)
  # But 0 and 360 in the SAME set are one position twice -- a duplicate.
  both <- as.numeric(fx$angles)
  both[both == 45] <- 0 # now carries both 0 and 360
  expect_error(
    suppressMessages(
      axes_reliability(fx$data, items = fx$items, angles = both)
    ),
    "duplicat"
  )
})

test_that("M60: angles_spacing_status() classifies at the pole and near-misses", {
  # The package's own octant set, LM = 360.
  expect_identical(angles_spacing_status(octants()), "ok")
  # The same eight positions written with 0 instead of 360 -- one position, so
  # the modular reduction must call both "ok" (the classic pole bug is to read
  # 0 and 360 as distinct and report unequal spacing).
  expect_identical(angles_spacing_status(c(0, 45, 90, 135, 180, 225, 270, 315)),
                   "ok")
  # 0 AND 360 in one set is that position twice.
  expect_identical(angles_spacing_status(c(0, 45, 90, 135, 180, 225, 270, 360)),
                   "duplicate")
  expect_identical(angles_spacing_status(c(45, 45, 135, 225)), "duplicate")

  # Rotations and other counts.
  expect_identical(angles_spacing_status(seq(22.5, 337.5, by = 45)), "ok")
  for (k in 4:24) {
    expect_identical(angles_spacing_status((seq_len(k) - 1L) * (360 / k)), "ok")
    # ... and at an arbitrary rotation of the same set.
    expect_identical(
      angles_spacing_status((seq_len(k) - 1L) * (360 / k) + 17.3), "ok"
    )
  }

  # Near-misses are refused: the tolerance is float noise, not a design margin.
  expect_identical(angles_spacing_status(c(0, 90, 180, 270.0001)), "unequal")
  expect_identical(angles_spacing_status(c(0, 90, 180, 270 + 1e-4)), "unequal")
  # A set bunched into one arc: its interior gaps are constant but are not
  # 360/k, which is what refuses it. (The wrap-around gap cannot be the thing
  # that catches this, or anything else -- all gaps sum to 360, so constant
  # interior gaps of 360/k force the wrap gap to match. Verified by mutation.)
  expect_identical(angles_spacing_status(c(0, 10, 20, 30)), "unequal")

  # Non-finite angles are classified, never silently dropped by sort(). Inf is
  # the dangerous one: anyNA() does not reject it, `Inf %% 360` is NaN, and
  # sort() drops NaN -- so without this branch the surviving angles could
  # satisfy 360/k and the set would read as "ok".
  expect_identical(angles_spacing_status(c(0, 90, NA, 270)), "nonfinite")
  expect_identical(angles_spacing_status(c(0, 90, NaN, 270)), "nonfinite")
  expect_identical(angles_spacing_status(c(0, 120, 240, Inf)), "nonfinite")
  expect_identical(angles_spacing_status(c(octants(), Inf)), "nonfinite")
  expect_identical(angles_spacing_status(c(0, 90, 180, -Inf)), "nonfinite")

  # Angles supplied outside [0, 360) reduce onto their circumplex positions.
  # This is the ONLY case that pins the `%% 360` reduction: with it removed,
  # the octant sets and the 0-and-360 duplicate below still classify correctly
  # (the wrap gap compensates), but this set is misread as a duplicate.
  expect_identical(angles_spacing_status(c(10, 100, 190, 640)), "ok")
  expect_identical(angles_spacing_status(c(-90, 0, 90, 180)), "ok")
  expect_identical(angles_spacing_status(c(0, 90, 180, 270, 450)), "duplicate")

  # The tolerance is loose enough for an exactly-constructed odd set, whose
  # gaps carry real float error (360/7 is not representable).
  expect_identical(angles_spacing_status((0:6) * (360 / 7)), "ok")
  expect_identical(angles_spacing_status((0:8) * (360 / 9) + 123.456), "ok")
})

test_that("M60: per-axis item_n is n * k/2 at any rotation", {
  # The tolerance is set from the DISCRIMINATION required, not from what this
  # machine prints (M59): the smallest error that could matter is one item, so
  # item_n = 1.0, and 1e-8 fences that at 1e8x while sitting ~6 orders above
  # the ~1e-14 float noise these sums actually carry.
  tol <- 1e-8
  for (k in 4:16) {
    for (rot in c(0, 22.5, 17.3, 180)) {
      ang <- (seq_len(k) - 1L) * (360 / k) + rot
      for (n in c(1L, 2L, 5L)) {
        inn <- axis_item_n(ang, n)
        expect_equal(inn[["x"]], n * k / 2, tolerance = tol,
                     label = sprintf("k=%d rot=%s n=%d x", k, rot, n))
        expect_equal(inn[["y"]], n * k / 2, tolerance = tol,
                     label = sprintf("k=%d rot=%s n=%d y", k, rot, n))
      }
    }
  }

  # The octant set stays EXACT -- BC3 above asserts expect_identical() on it,
  # and that must not be weakened just because rotated sets need a tolerance.
  expect_identical(axis_item_n(octants(), 4L), c(x = 16, y = 16))

  # An unbalanced set legitimately gives different item_n per axis, and a
  # fractional value (the SYMLOG shape, Table 3 col. 10 = 8.67). Nothing here
  # rounds or forces the two axes to agree.
  unb <- axis_item_n(c(0, 90, 180, 270), c(3L, 1L, 3L, 1L))
  expect_equal(unb[["x"]], 6, tolerance = tol)
  expect_equal(unb[["y"]], 2, tolerance = tol)
})

test_that("M60: Spearman-Brown reproduces the non-octant Table 3 rows (Layer A)", {
  # Strack et al. (2013) Table 3 col 1 is the circumplex TYPE, so the paper
  # publishes anchors beyond type a. Banked in cairn/references/strack2013.md
  # (two channels on p. 7: pdftotext text layer + page-image render).

  # Type b -- CV-LI, eight scales at 45 deg rotated 22.5 deg off the axes
  # (p. 2). This is exactly the configuration M60 unlocks.
  typeb <- data.frame(
    row    = c("CVLI12S", "CVLI12O", "CVLI12M", "CVLI13S"),
    gen    = c(22.6, 42.9, 35.4, 19.6),
    axes   = c(3.5, 2.7, 1.9, 7.6),
    scale  = c(19.6, 15.0, 19.6, 19.7),
    item   = c(54.3, 39.4, 43.1, 53.1),
    item_n = c(16, 16, 16, 16),
    rel    = c(.37, .31, .24, .57)
  )
  # Every type-b row is internally consistent -- unlike the IIP S6 erratum, no
  # exception carve-out is needed here.
  expect_true(all(abs(
    typeb$gen + typeb$axes + typeb$scale + typeb$item - 100.0
  ) <= 0.1))
  expect_true(all(abs(
    axis_reliability_sb(typeb$axes / 100, typeb$item_n) - typeb$rel
  ) <= .01))

  # Type c -- MEIL S14 Self. Its COMPONENTS are a second source defect (they sum
  # to 74.4, not 100.0; both extraction channels agree, and RR10 saw it in the
  # text layer), so this row is asserted as a reliability anchor only, never as
  # a component-sum guard.
  expect_true(abs(4.3 + 5.5 + 27.9 + 36.7 - 74.4) <= 0.1) # the defect, pinned
  expect_true(abs(axis_reliability_sb(.055, 30) - .63) <= .01)

  # The sweep discriminates: it is not satisfied by any item_n. Reading the
  # type-b rows at the octant item_n of 32 would miss every printed value.
  expect_false(all(abs(
    axis_reliability_sb(typeb$axes / 100, 32) - typeb$rel
  ) <= .01))
})

# M60 Layer-B: the BC5/BC6/BC7 oracles re-run at the configurations M60 unlocks
# -- a rotated octant set (Strack type b, no weight lands on 0) and non-octant
# counts (k = 6, k = 12, where scales DO sit on the poles). The estimator's
# geometry-specific risk lives in the weights and the moment design, which the
# exact population cell exercises completely.

axes_pop_recovers <- function(angles, k, xi1, xi2, zeta1) {
  pop <- axes_population_cor(angles, k, xi1, xi2, zeta1)
  sigma <- pop$sigma
  p <- nrow(sigma)
  inames <- sprintf("i%02d", seq_len(p))
  dimnames(sigma) <- list(inames, inames)
  items <- split(inames, factor(pop$scale, levels = seq_along(angles)))
  fit <- lavaan::cfa(
    axes_syntax(items, angles),
    sample.cov = sigma, sample.nobs = 500L,
    orthogonal = TRUE, likelihood = "wishart"
  )
  pe <- lavaan::parameterEstimates(fit)
  vv <- function(lat) pe$est[pe$op == "~~" & pe$lhs == lat & pe$rhs == lat][[1]]
  list(
    converged = lavaan::lavInspect(fit, "converged"),
    est = c(xi1 = vv("AX"), xi1b = vv("AY"), xi2 = vv("GEN"),
            zeta1 = vv("SS1")),
    chisq = unname(lavaan::fitMeasures(fit, "chisq"))
  )
}

test_that("M60: exact population recovery holds at rotated and non-octant sets", {
  skip_if_not_installed("lavaan")
  xi1 <- .15; xi2 <- .08; zeta1 <- .12
  cells <- list(
    `type-b rotated octants` = seq(22.5, 337.5, by = 45),
    `k = 6` = (seq_len(6L) - 1L) * 60,
    `k = 12` = (seq_len(12L) - 1L) * 30,
    `k = 5 at an odd rotation` = (seq_len(5L) - 1L) * 72 + 13.7
  )
  for (nm in names(cells)) {
    got <- axes_pop_recovers(cells[[nm]], k = 3L, xi1, xi2, zeta1)
    expect_true(got$converged, label = nm)
    # Exact at the population matrix -- no Monte-Carlo slack.
    expect_lt(abs(got$est[["xi1"]] - xi1), 1e-4)
    expect_lt(abs(got$est[["xi1b"]] - xi1), 1e-4)
    expect_lt(abs(got$est[["xi2"]] - xi2), 1e-4)
    expect_lt(abs(got$est[["zeta1"]] - zeta1), 1e-4)
    expect_lt(got$chisq, 1e-6)
  }
})

test_that("M60: Monte-Carlo recovery holds at a rotated octant set (Layer B)", {
  skip_if_not_installed("lavaan")
  mc <- axes_mc_recover_xi1(
    seq(22.5, 337.5, by = 45), k = 4L, xi1 = .15, xi2 = .05, zeta1 = .08,
    n = 1500L, reps = 100L, seed = 60L
  )
  expect_lt(abs(mc$mean - .15), 2 * mc$mcse)
})

test_that("M60: lavaan and OpenMx agree at a non-octant set (Layer B)", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("OpenMx")
  for (ang in list(seq(22.5, 337.5, by = 45), (seq_len(6L) - 1L) * 60)) {
    k <- 4L
    set.seed(60L)
    dat <- as.data.frame(scale(axes_simulate(2000L, ang, k, .15, .05, .10)))
    p <- ncol(dat)
    inames <- sprintf("i%02d", seq_len(p))
    colnames(dat) <- inames
    items <- split(inames, factor(rep(seq_along(ang), each = k),
                                  levels = seq_along(ang)))
    S <- stats::cov(dat)
    lav <- axes_lav_components(S, 2000L, items, ang)
    mx <- axes_mx_components(S, 2000L, ang, k)
    expect_lt(max(abs(lav - mx)), 1e-3)
  }
})

# --- M61: single-item scale positions (the zeta1-dropped path) ----------------
# T1 pins the two pre-M61 facts the rest of the milestone turns on, BEFORE
# anything changes: the arithmetic that forces M61-D1, and the single line that
# refuses a single-item instrument today. T6 relaxes that line; this test is
# what makes the relaxation visible in the diff rather than silent.

test_that("M61 T1: cronbach_alpha() is NaN at one item -- the reason N-B cannot report", {
  set.seed(61L)
  one <- matrix(stats::rnorm(50), ncol = 1)
  # m/(m-1) is 1/0 = Inf and (1 - sum(diag(cv))/sum(cv)) is exactly 0, so the
  # product is NaN -- not Inf, and not a number. This is the arithmetic behind
  # M61-D1: alpha is undefined for a one-item scale, so the Nunnally-Bernstein
  # axis formula has no rel_scale to consume and must report NA with a stated
  # reason rather than propagate NaN into a results frame.
  expect_true(is.nan(cronbach_alpha(one)))
  # Two items is where it becomes defined -- the boundary M61-D1 draws.
  expect_true(is.finite(cronbach_alpha(matrix(stats::rnorm(100), ncol = 2))))
})

test_that("M61 T1: the >= 2-items refusal is the only gate refusing a single-item set", {
  skip_if_not_installed("lavaan")
  # The COC shape (Strack type e; Table 3 p. 7: 16 items, no scales, item_n 8):
  # sixteen equally spaced positions carrying one item each.
  ang <- (seq_len(16L) - 1L) * 22.5
  fx <- axes_spaced_fixture(ang, n = 800L, k = 1L)
  expect_error(
    suppressMessages(
      axes_reliability(fx$data, items = fx$items, angles = fx$angles)
    ),
    "at least 2 items"
  )
  # ... and every OTHER gate in the refuse contract passes on this input, so the
  # item-count line really is the only thing in the way. Without this, a later
  # reader could think spacing or scale count were also implicated.
  expect_identical(angles_spacing_status(ang), "ok")
  expect_identical(length(fx$items), 16L)
  expect_true(all(lengths(fx$items) == 1L))
})
