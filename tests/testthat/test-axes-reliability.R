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

test_that("the fit-measure guard names the actual problem on every mismatch", {
  skip_if_not_installed("lavaan")
  # The guard exists because `fitMeasures()` DROPS a name it does not
  # recognize and returns a shorter vector, so a future lavaan retiring
  # `srmr_bentler_nomean` would delete `$fit$srmr` -- a documented @return
  # field -- and leave the object looking well formed. Measured on both
  # generations at M67, since the guard's own comment used to assert this
  # wrongly: requesting one real and one bogus measure returns ONE element,
  # silently on 0.6.21 and with a `unknown fit measure: 'srmr_bogus_name'`
  # warning on 0.7.2. The drop is real on both; only the silence was
  # version-specific.
  #
  # What is under test here is the guard's DIAGNOSIS. It used to test
  # `identical(names(fm), want)`, which also fails on order and on length,
  # while its message reported `setdiff(want, names(fm))` -- so any mismatch
  # that was not a dropped name printed the degenerate "(missing: )" and told
  # the user nothing.
  oct <- octants()
  set.seed(11)
  dat <- axes_simulate(300L, oct, 2L, xi1 = .15, xi2 = .05, zeta1 = .10)
  inames <- sprintf("g%02d", seq_len(ncol(dat)))
  colnames(dat) <- inames
  items <- split(inames, rep(seq_along(oct), each = 2L))
  call_it <- function() {
    suppressMessages(axes_reliability(dat, items = items, angles = oct))
  }
  real_fm <- lavaan::fitMeasures

  # (1) An order-only difference is not a missing measure, and must not be
  # reported as one. lavaan preserves the requested order on both generations
  # today, so this is forward-looking: the guard must key on membership and
  # then impose the order itself.
  local_mocked_bindings(
    fitMeasures = function(object, fit.measures, ...) {
      rev(real_fm(object, fit.measures, ...))
    },
    .package = "lavaan"
  )
  res <- call_it()
  expect_identical(
    names(res$fit),
    c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr")
  )

  # (2) A genuine drop still refuses, and names the measure that went missing.
  local_mocked_bindings(
    fitMeasures = function(object, fit.measures, ...) {
      fm <- real_fm(object, fit.measures, ...)
      fm[names(fm) != "cfi"]
    },
    .package = "lavaan"
  )
  err <- expect_error(call_it(), "did not return the expected fit measures")
  expect_match(conditionMessage(err), "missing: cfi", fixed = TRUE)
  expect_false(grepl("missing: )", conditionMessage(err), fixed = TRUE))
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

# --- The never-NaN contract (M62) --------------------------------------------
# Two ways axes_reliability() could report a NaN, negative, or infinite SEm.
# The first is a xi1 >= 1 fit, where Spearman-Brown returns a reliability at or
# above 1 and axis_sem()'s sqrt(1 - rel) goes imaginary. That state is not
# reachable through the exported function -- on a unit-diagonal correlation
# matrix the model reproduces, eps_i = 1 - xi1 - xi2 - zeta1, so xi1 > 1 with
# the other components non-negative drives the matrix indefinite and the
# positive-definite gate refuses it first (measured at the M62 plan gate: an
# engineered cormat implying xi1 = 1.15, xi2 = .20 has min eigenvalue exactly
# -0.35, its own implied eps). Evidence, not proof -- a finite-sample fit is
# approximate -- so the guard is owed, and the predicate is tested directly
# because no end-to-end fixture can reach it. The two halves compose: these
# tests prove xi1 >= 1 sets the boundary flag, and the BC11 test above proves
# the flag yields NA + warning rather than a computed value.

test_that("M62: axes_is_boundary() catches xi1 >= 1 as well as xi1 <= 0", {
  ok <- function(xi1 = .5, xi2 = .1, zeta1 = .1, eps = c(.3, .3)) {
    axes_is_boundary(xi1, xi2, zeta1, eps)
  }
  expect_false(ok()) # the interior case is not a boundary

  # The new half. >= 1 is deliberate and symmetric with the shipped <= 0: at
  # exactly 1 the SB reliability is exactly 1 and SEm exactly 0, which requires
  # zero item-error variance -- degenerate, not usable.
  expect_true(ok(xi1 = 1))
  expect_true(ok(xi1 = 1.001))
  expect_false(ok(xi1 = .999)) # fences the threshold from below

  # The shipped half, unchanged. Enumerated rather than assumed: M62 adds a
  # disjunct to this expression, and generalizing a gate is where incidental
  # refusals get dropped (the M60 lesson).
  expect_true(ok(xi1 = 0))
  expect_true(ok(xi1 = -.01))
  expect_true(ok(xi2 = -.01))
  expect_true(ok(zeta1 = -.01))
  expect_true(ok(eps = c(.3, -.01)))

  # The zeta1-dropped path (M61): zeta1 is NULL, and NULL must not itself read
  # as a boundary, nor error the way `logical(0)` would inside `||`.
  expect_false(ok(zeta1 = NULL))
  expect_true(ok(zeta1 = NULL, eps = c(.3, -.01)))
})

test_that("M62: no xi1 the boundary guard admits can yield a NaN or negative SEm", {
  # The property AC1 actually claims, swept rather than spot-checked: across
  # every item_n this package can produce -- 2 is the k = 4 single-item floor,
  # 2.5 an odd-k half-integer, 26/3 the SYMLOG-shaped fractional value, 16 and
  # 32 the octant cases -- every admitted xi1 gives a finite, non-negative SEm.
  #
  # `sem >= 0`, deliberately not `> 0`, and the grid runs right up to the bound
  # to keep that honest. The guard tests xi1, not the derived reliability, so
  # within about 1e-15 of 1 the SB ratio ROUNDS to exactly 1 and SEm is exactly
  # 0 for an xi1 the predicate admits (measured: item_n 26/3 and 32 at
  # xi1 = 1 - 1e-15). A bare zero is finite, non-negative and non-NaN, which is
  # the whole of what this milestone promises -- but a grid stopping short of
  # that regime while asserting `> 0` would be a test proving its property on a
  # hand-picked interior and claiming it universally.
  grid <- expand.grid(
    xi1 = c(1e-8, .001, .05, .5, .95, .999, 1 - 1e-9, 1 - 1e-14, 1 - 1e-15),
    item_n = c(2, 2.5, 26 / 3, 16, 32)
  )
  rel <- axis_reliability_sb(grid$xi1, grid$item_n)
  sem <- axis_sem(rel)
  # Mapped, not vectorized: the predicate uses `||`, which errors on a
  # length > 1 argument in R >= 4.3 -- it is a scalar decision by design.
  admitted <- vapply(grid$xi1, axes_is_boundary, logical(1),
                     xi2 = .1, zeta1 = .1, eps = .3)
  expect_false(any(admitted))
  expect_true(all(is.finite(rel)))
  expect_true(all(rel > 0 & rel <= 1))
  expect_true(all(is.finite(sem)))
  expect_true(all(!is.nan(sem) & sem >= 0))
})

test_that("M62: no accepted input lets a bare `NaNs produced` warning escape", {
  skip_if_not_installed("lavaan")
  # AC1's second clause, asserted on UNMOCKED calls. The sibling test below
  # forces the boundary through a mock, which routes straight to a literal
  # NA_real_ and so never runs the arithmetic that could raise this warning --
  # it cannot probe this claim, and must not be read as doing so.
  warn_texts <- function(expr) {
    w <- character(0)
    withCallingHandlers(
      suppressMessages(expr),
      warning = function(c) {
        w <<- c(w, conditionMessage(c))
        invokeRestart("muffleWarning")
      }
    )
    w
  }

  # An ordinary fit: no warnings of any kind, so certainly no NaN one.
  fx <- axes_valid_fixture()
  expect_false(any(grepl(
    "NaN", warn_texts(axes_reliability(fx$data, items = fx$items,
                                       angles = fx$oct))
  )))

  # A real boundary fit, where the NA path is actually taken (the BC11 seed):
  # the boundary warning is raised and it is the ONLY one -- R's bare
  # "NaNs produced" from sqrt() of a negative never reaches the user, which is
  # what the guard exists to prevent.
  oct <- octants()
  set.seed(5)
  bd <- axes_simulate(400L, oct, 4L, xi1 = 0, xi2 = .05, zeta1 = .40)
  inames <- sprintf("i%02d", seq_len(ncol(bd)))
  colnames(bd) <- inames
  w <- warn_texts(axes_reliability(
    bd, items = split(inames, rep(seq_along(oct), each = 4L)), angles = oct
  ))
  expect_true(any(grepl("boundary", w)))
  expect_false(any(grepl("NaN", w)))
})

test_that("M62: the boundary branch reports NA rather than NaN, end to end", {
  skip_if_not_installed("lavaan")
  fx <- axes_valid_fixture()
  # Forces the boundary through the same seam the convergence guard uses. This
  # proves the boundary -> NA wiring and that NA is not NaN; it does NOT probe
  # the guard's condition (the mock replaces it) -- the predicate test above is
  # what reddens when the xi1 >= 1 disjunct is deleted.
  testthat::local_mocked_bindings(axes_is_boundary = function(...) TRUE)
  expect_warning(
    res <- suppressMessages(
      axes_reliability(fx$data, items = fx$items, angles = fx$oct)
    ),
    "boundary"
  )
  expect_true(all(is.na(res$results$sem)))
  expect_false(any(is.nan(res$results$sem))) # NA, never NaN
  expect_true(res$details$boundary)
})

test_that("M62: a numeric `sd` must be finite and positive", {
  skip_if_not_installed("lavaan")
  fx <- axes_valid_fixture()
  run <- function(s) suppressMessages(
    axes_reliability(fx$data, items = fx$items, angles = fx$oct, sd = s)
  )

  # The second never-NaN path, and the only one reachable from the exported
  # API: `sd` scales SEm = sd * sqrt(1 - rel), so each of these reached the
  # results frame unchallenged before M62 -- measured at the plan gate as
  # sem = -0.4764406, Inf, NA, and NaN respectively.
  for (bad in list(-1, 0, Inf, -Inf, NA_real_, NaN)) {
    expect_error(run(bad), "must be finite and positive")
  }
  # Length-2 (per-axis SDs): one bad element is enough, and it is caught
  # whichever axis carries it.
  expect_error(run(c(2, -1)), "must be finite and positive")
  expect_error(run(c(NaN, 2)), "must be finite and positive")
  # is.finite() rather than is.na(): is.na() admits +/-Inf, which is why the
  # Inf cases above are the ones that pin this line (the M32/M35 lesson).

  # Everything already legal stays legal, and returns what it returned before.
  ref <- run("std")
  expect_true(all(is.finite(ref$results$sem)))
  expect_equal(
    run(3)$results$sem, 3 * ref$results$sem, tolerance = 1e-10
  )
  expect_equal(
    run(c(2, 3))$results$sem, c(2, 3) * ref$results$sem, tolerance = 1e-10
  )
  expect_true(all(is.finite(run("raw")$results$sem)))
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
  # a scale with no items at all (M61 relaxed this gate from "< 2 items" to
  # "< 1 item": one item per position is Strack's types e and f, and is now
  # estimated with the scale-specificity component dropped)
  no_item <- fx$items
  no_item[[1]] <- character(0)
  expect_error(
    suppressMessages(axes_reliability(fx$data, items = no_item, angles = fx$oct)),
    "at least 1 item"
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

  # A scale with NO items stays refused, and names itself. (Fewer than 2 items
  # was refused here until M61 relaxed it to the zeta1 drop rule; a mixed map
  # with one single-item scale is now estimated, not an error.)
  none <- fx$items
  none[[1]] <- character(0)
  expect_error(bad(items = none), "at least 1 item")
  expect_error(bad(items = none), "scale\\(s\\) 1")

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

  # An unbalanced set legitimately gives different item_n per axis. Nothing here
  # rounds or forces the two axes to agree. (Table 3's fractional entry, SYMLOG's
  # 8.67, is NOT this shape -- it is a three-axis sphere model's 26/3, out of
  # scope for a two-axis contract; see M61 / cairn RR11. The reachable fractional
  # cases are an odd-k or unbalanced set, tested at M61 T7.)
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

test_that("M61 T1/T6: the single-item set passes every gate, and now estimates", {
  skip_if_not_installed("lavaan")
  # The COC shape (Strack type e; Table 3 p. 7: 16 items, no scales, item_n 8):
  # sixteen equally spaced positions carrying one item each. Until M61 the
  # >= 2-items line refused exactly this; T1 pinned that every OTHER gate in the
  # refuse contract passed on it, so the item-count line really was the only
  # thing in the way -- no spacing or scale-count problem was hiding behind it.
  ang <- (seq_len(16L) - 1L) * 22.5
  fx <- axes_spaced_fixture(ang, n = 800L, k = 1L)
  expect_identical(angles_spacing_status(ang), "ok")
  expect_identical(length(fx$items), 16L)
  expect_true(all(lengths(fx$items) == 1L))
  expect_false(axes_fits_zeta1(fx$items))

  res <- suppressMessages(
    axes_reliability(fx$data, items = fx$items, angles = fx$angles)
  )
  expect_s3_class(res, "circumplex_axes_reliability")
  expect_true(all(is.finite(res$results$reliability)))
  # item_n = k/2 = 8, the COC value Strack prints.
  expect_equal(res$results$item_n, c(8, 8), tolerance = 1e-8)
})

test_that("M61 T2: axes_fits_zeta1() reads the drop rule off the item map", {
  # The rule is "at least one scale carries a PAIR", not "every scale does" --
  # a mixed map still fits zeta1 because one multi-item scale supplies the
  # off-diagonal moment and the shared label carries it to the rest.
  expect_true(axes_fits_zeta1(list(c("a", "b"), c("c", "d"))))
  expect_true(axes_fits_zeta1(list(c("a", "b"), "c", "d", "e")))  # mixed
  expect_false(axes_fits_zeta1(list("a", "b", "c", "d")))         # all single
  # A zero-item scale is not a pair either; it must not be read as one.
  expect_false(axes_fits_zeta1(list(character(0), "b", "c", "d")))
})

test_that("M61 T2: axes_syntax() drops the SS latents exactly on the single-item map", {
  ang4 <- c(0, 90, 180, 270)
  single <- list("i1", "i2", "i3", "i4")
  mixed <- list(c("i1", "i2"), "i3", "i4", "i5")

  syn_s <- axes_syntax(single, ang4)
  # No scale-specificity anywhere: no SS latent definitions, no zeta1 label.
  expect_false(grepl("SS1", syn_s, fixed = TRUE))
  expect_false(grepl("zeta1", syn_s, fixed = TRUE))
  expect_true(grepl("no scale-specificity component", syn_s, fixed = TRUE))
  # The rest of the model is untouched: both axes, the general factor, and the
  # shared xi1 label all survive the drop.
  expect_true(grepl("AX ~~ xi1*AX", syn_s, fixed = TRUE))
  expect_true(grepl("AY ~~ xi1*AY", syn_s, fixed = TRUE))
  expect_true(grepl("GEN ~~ xi2*GEN", syn_s, fixed = TRUE))

  # The mixed map keeps zeta1 -- and keeps an SS latent for the SINGLE-item
  # scales too, since the shared-label restriction is what identifies them.
  syn_m <- axes_syntax(mixed, ang4)
  expect_true(grepl("SS1 =~ 1*i1 + 1*i2", syn_m, fixed = TRUE))
  expect_true(grepl("SS2 =~ 1*i3", syn_m, fixed = TRUE))
  expect_true(grepl("SS4 ~~ zeta1*SS4", syn_m, fixed = TRUE))
})

test_that("M61 T2: a seed without zeta1 emits no modifier rather than erroring", {
  ang4 <- c(0, 90, 180, 270)
  # The two-column OLS shadow (T3) returns a seed with no `zeta1` element.
  # `start[["zeta1"]]` on that vector is a subscript error, not a NULL, so the
  # lookup must test for the name -- this is the fence on that.
  seed2 <- c(xi2 = .05, xi1 = .20)
  syn <- expect_no_error(axes_syntax(list("i1", "i2", "i3", "i4"), ang4,
                                     start = seed2))
  # fmt() prints a double at full precision, so match the value's leading
  # digits rather than pinning its digit count -- the assertion is about WHICH
  # parameters get a modifier, not about how fmt() formats.
  expect_match(syn, "AX ~~ start\\(0\\.2[0-9]*\\)\\*xi1\\*AX")
  expect_match(syn, "GEN ~~ start\\(0\\.05[0-9]*\\)\\*xi2\\*GEN")
  expect_false(grepl("zeta1", syn, fixed = TRUE))

  # A full three-element seed still seeds all three on the zeta1-fitted path.
  syn3 <- axes_syntax(list(c("i1", "i2"), "i3", "i4", "i5"), ang4,
                      start = c(xi2 = .05, xi1 = .20, zeta1 = .08))
  expect_match(syn3, "SS1 ~~ start\\(0\\.08[0-9]*\\)\\*zeta1\\*SS1")
})

test_that("M61 T3: the OLS shadow drops to two columns when no same-scale pair exists", {
  # Single item at every position: the same-scale indicator is identically zero
  # off the diagonal, so the three-column design has a zero column. Before M61
  # this was a hard qr.solve() failure, not a graceful degradation.
  ang <- (seq_len(8L) - 1L) * 45
  xi1 <- .18
  xi2 <- .07
  # zeta1 is irrelevant at n_items = 1 -- its block lands entirely on the
  # diagonal, which axes_population_cor() overwrites with 1 -- so the generating
  # population genuinely has no scale-specificity term, whatever is passed here.
  pop <- axes_population_cor(ang, 1L, xi1, xi2, zeta1 = 0)
  item_scale <- pop$scale
  item_angle <- rep(ang, each = 1L)

  seed <- expect_no_error(axes_ols_shadow(pop$sigma, item_angle, item_scale))
  expect_identical(names(seed), c("xi2", "xi1"))
  # Exact on the population matrix: r_ij = xi2 + xi1*cos(theta_i - theta_j) is
  # linear in the two remaining columns, so least squares is not an
  # approximation here (the same claim the three-column shadow makes).
  expect_lt(abs(seed[["xi1"]] - xi1), 1e-10)
  expect_lt(abs(seed[["xi2"]] - xi2), 1e-10)

  # zeta1 really is unrecoverable, not merely omitted: any zeta1 gives the same
  # population matrix at one item per position, so nothing could recover it.
  pop_b <- axes_population_cor(ang, 1L, xi1, xi2, zeta1 = .40)
  expect_identical(pop_b$sigma, pop$sigma)
})

test_that("M61 T3: a mixed map keeps the three-column shadow", {
  ang <- (seq_len(4L) - 1L) * 90
  # Scale 1 carries a pair; scales 2-4 carry one item each. One pair is enough
  # to identify zeta1, so the third column survives.
  item_scale <- c(1L, 1L, 2L, 3L, 4L)
  item_angle <- ang[item_scale]
  xi1 <- .15
  xi2 <- .06
  zeta1 <- .10
  th <- item_angle * pi / 180
  sig <- xi2 + xi1 * outer(th, th, function(a, b) cos(a - b)) +
    zeta1 * outer(item_scale, item_scale, `==`)
  diag(sig) <- 1

  seed <- axes_ols_shadow(sig, item_angle, item_scale)
  expect_identical(names(seed), c("xi2", "xi1", "zeta1"))
  expect_lt(abs(seed[["xi1"]] - xi1), 1e-10)
  expect_lt(abs(seed[["xi2"]] - xi2), 1e-10)
  expect_lt(abs(seed[["zeta1"]] - zeta1), 1e-10)
})

test_that("M61 T4/T6: the zeta1-dropped path returns a three-row component set (AC1)", {
  skip_if_not_installed("lavaan")
  ang <- (seq_len(12L) - 1L) * 30
  fx <- axes_spaced_fixture(ang, n = 1200L, k = 1L)
  res <- suppressMessages(
    axes_reliability(fx$data, items = fx$items, angles = fx$angles)
  )

  # Three rows, and the scale-specificity row is ABSENT rather than NA -- an NA
  # row would read as "estimated but unavailable" instead of "not in this
  # model", which is the distinction M61 exists to make.
  expect_identical(nrow(res$components), 3L)
  expect_identical(res$components$Component, c("general", "axes", "item"))
  expect_identical(res$components$Symbol, c("xi2", "xi1", "epsilon"))
  expect_false("scale_specificity" %in% res$components$Component)
  # The two fitted components still carry standard errors; only the item row's
  # SE is NA (it is a mean of free residuals, as on the zeta1-fitted path).
  expect_true(all(is.finite(res$components$SE[1:2])))
  expect_true(is.na(res$components$SE[[3]]))

  # details records the drop, so a caller can tell the two models apart without
  # inspecting the component table.
  expect_false(res$details$zeta1_fitted)
  expect_true(all(is.finite(res$results$reliability)))
  expect_true(all(res$results$reliability > 0 & res$results$reliability < 1))
})

# The population implied by an UNBALANCED item map: scale s contributes
# `counts[s]` items at `angles[s]`. Same construction as axes_population_cor(),
# which assumes a constant item count per scale and so cannot express a mixed
# map. One coherent Sigma -- never two independent draws glued together, which
# would carry zero true cross-block correlation and correspond to no population
# at all (caught at the M61 review gate, finding F1).
axes_unbalanced_population <- function(angles, counts, xi1, xi2, zeta1) {
  item_scale <- rep(seq_along(angles), times = counts)
  th <- angles[item_scale] * pi / 180
  sig <- xi2 + xi1 * outer(th, th, function(a, b) cos(a - b)) +
    zeta1 * outer(item_scale, item_scale, `==`)
  diag(sig) <- 1
  list(sigma = sig, scale = item_scale)
}

test_that("M61 T4/T6: a mixed map still fits zeta1 and keeps four rows (AC2)", {
  skip_if_not_installed("lavaan")
  # Eight equally spaced positions; scale 1 carries a pair, the rest one item
  # each. One pair is all the drop rule requires.
  ang <- (seq_len(8L) - 1L) * 45
  cnt <- c(2L, rep(1L, 7L))
  pop <- axes_unbalanced_population(ang, cnt, .20, .05, .08)
  set.seed(614L)
  x <- mvn_draws(1500L, rep(0, nrow(pop$sigma)), pop$sigma)
  inames <- sprintf("i%02d", seq_len(ncol(x)))
  mixed_dat <- as.data.frame(x)
  colnames(mixed_dat) <- inames
  items <- split(inames, factor(pop$scale, levels = seq_along(ang)))

  expect_true(axes_fits_zeta1(items))
  expect_identical(unname(lengths(items)), cnt)
  res <- suppressMessages(
    axes_reliability(mixed_dat, items = items, angles = ang)
  )
  expect_identical(nrow(res$components), 4L)
  expect_true("scale_specificity" %in% res$components$Component)
  expect_true(res$details$zeta1_fitted)
  # The seed carried zeta1 too, since the OLS shadow kept its third column.
  expect_identical(names(res$details$ols_shadow), c("xi2", "xi1", "zeta1"))
  # The fixture is a real population, so the estimates must land in the right
  # neighbourhood -- structural assertions alone would pass over a fit that had
  # gone to a boundary solution. These bounds are ABSOLUTE and deliberately
  # loose: this is one finite sample, and zeta1 rests on the single item pair
  # scale 1 contributes, so its sampling variance is large (measured .058 for a
  # truth of .08 at n = 1500). Exact recovery is the population oracle's job,
  # asserted at 1e-4 in the mixed Layer-B test below; what this fences is a
  # boundary or grossly wrong fit reaching the results frame.
  est <- stats::setNames(res$components$Estimate, res$components$Symbol)
  expect_lt(abs(est[["xi1"]] - .20), .05)
  expect_lt(abs(est[["xi2"]] - .05), .05)
  expect_lt(abs(est[["zeta1"]] - .08), .05)
  expect_false(res$results$boundary[[1]])
})

# Exact-population oracle for the MIXED map -- the configuration M61 newly
# accepts and which no Layer-B cell otherwise covers (M61 review finding F2).
# The second cell puts the multi-item scale somewhere other than first, which is
# what confirms comp_var("SS1") reads the SHARED zeta1 label rather than
# happening to read a latent that owns a pair.
test_that("M61: exact population recovery at mixed item counts (Layer B)", {
  skip_if_not_installed("lavaan")
  ang <- (seq_len(8L) - 1L) * 45
  xi1 <- .20; xi2 <- .05; zeta1 <- .08
  cells <- list(
    `pair on scale 1` = c(2L, rep(1L, 7L)),
    `pair on scale 3` = c(1L, 1L, 2L, rep(1L, 5L)),
    `two pairs` = c(2L, 1L, 1L, 2L, 1L, 1L, 1L, 1L)
  )
  for (nm in names(cells)) {
    pop <- axes_unbalanced_population(ang, cells[[nm]], xi1, xi2, zeta1)
    sigma <- pop$sigma
    inames <- sprintf("u%02d", seq_len(nrow(sigma)))
    dimnames(sigma) <- list(inames, inames)
    items <- split(inames, factor(pop$scale, levels = seq_along(ang)))
    fit <- lavaan::cfa(
      axes_syntax(items, ang), sample.cov = sigma, sample.nobs = 500L,
      orthogonal = TRUE, likelihood = "wishart"
    )
    expect_true(lavaan::lavInspect(fit, "converged"), label = nm)
    pe <- lavaan::parameterEstimates(fit)
    vv <- function(lat) pe$est[pe$op == "~~" & pe$lhs == lat & pe$rhs == lat][[1]]
    expect_lt(abs(vv("AX") - xi1), 1e-4)
    expect_lt(abs(vv("AY") - xi1), 1e-4)
    expect_lt(abs(vv("GEN") - xi2), 1e-4)
    expect_lt(abs(vv("SS1") - zeta1), 1e-4)
    expect_lt(unname(lavaan::fitMeasures(fit, "chisq")), 1e-6)
  }
})

test_that("M61 T5: N-B is NA-with-reason on the single-item path, never NaN (AC3)", {
  skip_if_not_installed("lavaan")
  ang <- (seq_len(8L) - 1L) * 45
  fx <- axes_spaced_fixture(ang, n = 1000L, k = 1L)
  res <- suppressMessages(
    axes_reliability(fx$data, items = fx$items, angles = fx$angles)
  )

  # NA, and specifically NOT NaN: is.na() is TRUE for both, so the NaN check has
  # to be made separately or the criterion's "never NaN" clause goes untested.
  expect_true(all(is.na(res$results$nb_reliability)))
  expect_false(any(is.nan(res$results$nb_reliability)))
  expect_identical(res$details$nb_reason, "single_item")

  # The reason reaches the user, on the same house pattern the cormat path uses.
  out <- paste(utils::capture.output(print(res)), collapse = "\n")
  expect_match(out, "Nunnally-Bernstein comparison needs each scale's alpha")
  expect_match(out, "undefined for a scale carrying only one item")
  # ... and the display shows a dash rather than a number for it.
  expect_match(out, "NB_Reliability")
})

test_that("M61 T5: a MIXED map also reports N-B as NA -- the M61-D1 hole", {
  skip_if_not_installed("lavaan")
  # This is the branch AC3's literal wording would have missed: zeta1 IS fitted
  # here, so a "zeta1-dropped path" rule would let alpha's NaN through.
  ang <- (seq_len(8L) - 1L) * 45
  set.seed(615L)
  dat2 <- axes_simulate(1200L, ang, 2L, .20, .05, .08)
  colnames(dat2) <- sprintf("j%02d", seq_len(ncol(dat2)))
  # Scale 1 keeps both its items; every other scale keeps only its first.
  keep <- c(1L, 2L, seq(3L, ncol(dat2), by = 2L))
  mixed <- dat2[, keep, drop = FALSE]
  items <- c(list(colnames(dat2)[1:2]), as.list(colnames(dat2)[seq(3L, ncol(dat2), by = 2L)]))

  expect_true(axes_fits_zeta1(items))         # zeta1 IS fitted
  res <- suppressMessages(axes_reliability(mixed, items = items, angles = ang))
  expect_true(res$details$zeta1_fitted)
  expect_identical(res$details$nb_reason, "single_item")
  expect_true(all(is.na(res$results$nb_reliability)))
  expect_false(any(is.nan(res$results$nb_reliability)))
})

test_that("M61 T5: N-B stays available and unannotated when every scale has a pair", {
  skip_if_not_installed("lavaan")
  fx <- axes_spaced_fixture(octants(), n = 800L, k = 4L)
  res <- suppressMessages(
    axes_reliability(fx$data, items = fx$items, angles = fx$angles)
  )
  expect_null(res$details$nb_reason)
  expect_true(all(is.finite(res$results$nb_reliability)))
  out <- paste(utils::capture.output(print(res)), collapse = "\n")
  expect_false(grepl("undefined for a scale", out, fixed = TRUE))
  expect_false(grepl("correlation-matrix path", out, fixed = TRUE))
})

# M61 Layer A (T7). The six single-item Table 3 rows, banked in
# cairn/references/strack2013.md (two channels on p. 7: the born-digital
# pdftotext text layer and a 200-dpi page-image render). Like the M60 sweep,
# this is a FORMULA-LAYER oracle: it calls axis_reliability_sb() directly with
# the paper's printed (%axes, item_n) pairs. It is not, and must not become, a
# path through axes_reliability().

test_that("M61 T7: Spearman-Brown reproduces the six single-item Table 3 rows (Layer A)", {
  # Type e -- COC, sixteen single-item positions (Table 1: 16 items, no
  # scales). This IS a configuration the package accepts: 16 positions give
  # item_n = 16/2 = 8, exactly the printed value.
  typee <- data.frame(
    row    = c("COC16S", "COC16O", "COC16M"),
    gen    = c(34.1, 46.7, 43.1),
    axes   = c(2.8, 3.2, 1.9),
    item   = c(63.1, 50.1, 55.0),
    item_n = c(8, 8, 8),
    rel    = c(.19, .21, .13)
  )
  # Type f -- SYMLOG. NOT a package-supported configuration: Strack fits SYMLOG
  # as a SPHERE (three orthogonal axes; "spheres (e.g., Bales & Cohen, 1979)"
  # p. 2, "realizes a sphere" p. 5, "the SYMLOG for a sphere model" p. 9), and
  # its item_n 8.67 = 26/3 is unreachable in any two-axis equally spaced set,
  # where single-item sets give k/2 -- a half-integer. These rows are the
  # paper's only published fractional-item_n triples, so they anchor the scalar
  # identity axis_reliability_sb() -- and only that. Never promote them to an
  # end-to-end axes_reliability() fixture.
  typef <- data.frame(
    row    = c("SYM17S", "SYM17O", "SYM17M"),
    gen    = c(14.4, 11.8, 15.2),
    axes   = c(27.2, 30.3, 28.1),
    item   = c(58.4, 57.9, 56.7),
    item_n = c(8.67, 8.67, 8.67),
    rel    = c(.76, .79, .77)
  )
  six <- rbind(typee, typef)

  # Scale-specificity is "--" on all six (the paper drops zeta1 too), so the
  # component sum is %gen + %axes + %item alone. All six are internally
  # consistent -- unlike the type-c row, these carry a real sum guard.
  expect_true(all(abs(six$gen + six$axes + six$item - 100.0) <= .05))

  # The sweep itself.
  expect_true(all(abs(
    axis_reliability_sb(six$axes / 100, six$item_n) - six$rel
  ) <= .01))

  # The sweep discriminates -- but only against a DISTANT item_n. At SYMLOG's
  # xi1 the printed 8.67 and its nearest reachable neighbour 8.5 differ by only
  # ~.0035 in reliability, far inside the +/-.01 window, so this check would be
  # worthless at a near miss and nobody may read it as "verified 8.67".
  expect_true(all(abs(
    axis_reliability_sb(typef$axes / 100, 32) - typef$rel
  ) > .01))
})

# A per-axis item_n coded INDEPENDENTLY of axis_item_n(): plain cos/sin, no
# snap_trig(), no axis_weights(). Comparing the results frame against the
# function that produced it would assert nothing (BC4).
axes_analytic_item_n <- function(angles_deg, counts) {
  th <- angles_deg * pi / 180
  c(x = sum(counts * cos(th)^2), y = sum(counts * sin(th)^2))
}

test_that("M61 T7: fractional item_n survives end to end on the zeta1-dropped path", {
  skip_if_not_installed("lavaan")
  # Five single-item positions -- an ODD count, which is the only single-item
  # shape giving a fractional item_n (k/2 = 2.5). This is the reachable
  # fractional case; SYMLOG's 8.67 is a sphere-model value and is asserted at
  # the formula layer only.
  ang <- (seq_len(5L) - 1L) * 72
  fx <- axes_spaced_fixture(ang, n = 1200L, k = 1L)
  res <- suppressMessages(
    axes_reliability(fx$data, items = fx$items, angles = fx$angles)
  )

  want <- axes_analytic_item_n(ang, rep(1L, 5L))
  expect_equal(res$results$item_n, unname(want[c("x", "y")]), tolerance = 1e-8)
  expect_equal(res$results$item_n, c(2.5, 2.5), tolerance = 1e-8)
  # Stored as a double, not silently coerced to integer -- a rounding-tolerant
  # comparison at a half-integer would not catch that on its own.
  expect_true(is.double(res$results$item_n))
  # No expect_identical() here: the half-integer is NOT float-exact at every
  # rotation (k = 5 at 13.7 degrees measures 2.4999999999999996), so pinning
  # identity would be a platform trap, not a stronger assertion.

  expect_false(res$details$zeta1_fitted)
  expect_true(all(is.finite(res$results$reliability)))
})

test_that("M61 T7: fractional AND unequal item_n survives on the zeta1-fitted path", {
  skip_if_not_installed("lavaan")
  # Four positions rotated 22.5 degrees off the axes, carrying 2/3/2/2 items.
  # This is the only shape whose per-axis item_n are fractional *and differ*,
  # so it alone catches an x/y conflation -- a single-item set always gives
  # equal axes and is structurally blind to that defect.
  ang <- c(22.5, 112.5, 202.5, 292.5)
  cnt <- c(2L, 3L, 2L, 2L)
  item_scale <- rep(seq_along(ang), times = cnt)
  th <- ang[item_scale] * pi / 180
  xi1 <- .20; xi2 <- .05; zeta1 <- .08
  sig <- xi2 + xi1 * outer(th, th, function(a, b) cos(a - b)) +
    zeta1 * outer(item_scale, item_scale, `==`)
  diag(sig) <- 1

  set.seed(617L)
  x <- mvn_draws(1500L, rep(0, nrow(sig)), sig)
  inames <- sprintf("m%02d", seq_len(ncol(x)))
  dat <- as.data.frame(x)
  colnames(dat) <- inames
  items <- split(inames, factor(item_scale, levels = seq_along(ang)))

  expect_true(axes_fits_zeta1(items))
  res <- suppressMessages(axes_reliability(dat, items = items, angles = ang))

  want <- axes_analytic_item_n(ang, cnt)
  expect_equal(res$results$item_n, unname(want[c("x", "y")]), tolerance = 1e-8)
  expect_equal(res$results$item_n, c(4.1464466, 4.8535534), tolerance = 1e-6)
  expect_true(is.double(res$results$item_n))
  # The two axes genuinely differ -- nothing recycled one into both rows.
  expect_false(isTRUE(all.equal(res$results$item_n[[1]], res$results$item_n[[2]])))
  expect_true(res$details$zeta1_fitted)
  # ... and the reliabilities differ with them, since they share one xi1 and
  # differ only through item_n.
  expect_false(isTRUE(all.equal(
    res$results$reliability[[1]], res$results$reliability[[2]]
  )))
})

# M61 Layer B (T8). The BC5/BC6/BC7 oracles re-run at single-item
# configurations, where the model has no zeta1 at all. The generating population
# genuinely carries no scale-specificity: at one item per position the zeta1
# block of axes_population_cor() lands entirely on the diagonal, which is
# overwritten with 1, so passing zeta1 = 0 is exact rather than an
# approximation (pinned in the T3 test above).

axes_pop_recovers_single <- function(angles, xi1, xi2) {
  pop <- axes_population_cor(angles, 1L, xi1, xi2, zeta1 = 0)
  sigma <- pop$sigma
  p <- nrow(sigma)
  inames <- sprintf("s%02d", seq_len(p))
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
    est = c(xi1 = vv("AX"), xi1b = vv("AY"), xi2 = vv("GEN")),
    # No SS latent exists to read -- assert that, rather than reading a value.
    has_ss = any(pe$op == "~~" & grepl("^SS", pe$lhs)),
    chisq = unname(lavaan::fitMeasures(fit, "chisq"))
  )
}

test_that("M61 T8: exact population recovery at single-item configurations (AC6)", {
  skip_if_not_installed("lavaan")
  xi1 <- .16
  xi2 <- .09
  cells <- list(
    `k = 5 (fractional item_n 2.5)` = (seq_len(5L) - 1L) * 72,
    `k = 8 octants` = octants(),
    `k = 16 (the COC shape, item_n 8)` = (seq_len(16L) - 1L) * 22.5,
    `k = 6 at an odd rotation` = (seq_len(6L) - 1L) * 60 + 11.3
  )
  for (nm in names(cells)) {
    got <- axes_pop_recovers_single(cells[[nm]], xi1, xi2)
    expect_true(got$converged, label = nm)
    # Exact at the population matrix -- no Monte-Carlo slack.
    expect_lt(abs(got$est[["xi1"]] - xi1), 1e-4)
    expect_lt(abs(got$est[["xi1b"]] - xi1), 1e-4)
    expect_lt(abs(got$est[["xi2"]] - xi2), 1e-4)
    # Neither the generating Sigma nor the fitted model carries a zeta1 term.
    expect_false(got$has_ss, label = nm)
    expect_lt(got$chisq, 1e-6)
  }
})

test_that("M61 T8: Monte-Carlo recovery holds at a single-item set (Layer B)", {
  skip_if_not_installed("lavaan")
  # axes_mc_recover_xi1() reads AX only, so it carries over to the zeta1-dropped
  # path unchanged; k = 1 item per position is the whole difference.
  mc <- axes_mc_recover_xi1(
    (seq_len(12L) - 1L) * 30, k = 1L, xi1 = .18, xi2 = .06, zeta1 = 0,
    n = 2000L, reps = 100L, seed = 618L
  )
  expect_lt(abs(mc$mean - .18), 2 * mc$mcse)
})

# The OpenMx cross-check without the zeta1*B term. At one item per position B is
# the identity, so zeta1 would be perfectly confounded with the item residuals:
# dropping it is what makes the model identified, not a simplification.
axes_mx_components_single <- function(S, n, angles_deg) {
  p <- nrow(S)
  nm <- rownames(S)
  th <- as.numeric(angles_deg) * pi / 180
  model <- OpenMx::mxModel(
    "axes_single",
    OpenMx::mxMatrix("Full", 1, 1, free = TRUE, values = .15, lbound = 0,
                     name = "xi1"),
    OpenMx::mxMatrix("Full", 1, 1, free = TRUE, values = .05, lbound = 0,
                     name = "xi2"),
    OpenMx::mxMatrix("Full", p, 1, free = TRUE, values = .5, lbound = 0,
                     name = "eps"),
    OpenMx::mxMatrix("Full", p, p, free = FALSE,
                     values = outer(th, th, function(a, b) cos(a - b)),
                     name = "C"),
    OpenMx::mxMatrix("Full", p, p, free = FALSE, values = 1, name = "J"),
    OpenMx::mxAlgebra(
      xi1[1, 1] * C + xi2[1, 1] * J + vec2diag(eps),
      name = "Sigma", dimnames = list(nm, nm)
    ),
    OpenMx::mxData(observed = S, type = "cov", numObs = n),
    OpenMx::mxExpectationNormal(covariance = "Sigma"),
    OpenMx::mxFitFunctionML()
  )
  fit <- suppressWarnings(suppressMessages(
    OpenMx::mxRun(model, silent = TRUE, suppressWarnings = TRUE)
  ))
  c(xi1 = OpenMx::mxEval(xi1, fit)[1, 1], xi2 = OpenMx::mxEval(xi2, fit)[1, 1])
}

test_that("M61 T8: lavaan and OpenMx agree at a single-item set (Layer B)", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("OpenMx")
  for (ang in list((seq_len(12L) - 1L) * 30, (seq_len(5L) - 1L) * 72)) {
    set.seed(619L)
    dat <- as.data.frame(scale(axes_simulate(2000L, ang, 1L, .16, .06, 0)))
    inames <- sprintf("s%02d", seq_len(ncol(dat)))
    colnames(dat) <- inames
    items <- split(inames, factor(seq_along(ang), levels = seq_along(ang)))
    S <- stats::cov(dat)

    fit <- lavaan::cfa(
      axes_syntax(items, ang), sample.cov = S, sample.nobs = 2000L,
      orthogonal = TRUE, likelihood = "wishart"
    )
    pe <- lavaan::parameterEstimates(fit)
    vv <- function(lat) pe$est[pe$op == "~~" & pe$lhs == lat & pe$rhs == lat][[1]]
    lav <- c(xi1 = vv("AX"), xi2 = vv("GEN"))
    mx <- axes_mx_components_single(S, 2000L, ang)
    expect_lt(max(abs(lav - mx)), 1e-3)
  }
})

test_that("M61 review F3: the k < 4 message names only components the map would fit", {
  skip_if_not_installed("lavaan")
  ang3 <- c(0, 120, 240)
  set.seed(63L)
  d <- as.data.frame(matrix(stats::rnorm(200 * 6), ncol = 6))
  colnames(d) <- sprintf("v%d", 1:6)

  # Multi-item map: zeta1 WOULD be fitted, so scale specificity is named.
  multi <- list(c("v1", "v2"), c("v3", "v4"), c("v5", "v6"))
  expect_error(
    suppressMessages(axes_reliability(d, items = multi, angles = ang3)),
    "general, axes, and scale-specificity variances"
  )
  # Single-item map: zeta1 would be DROPPED, so naming it would misdirect the
  # user toward a component that was never in the model.
  single <- list("v1", "v2", "v3")
  expect_error(
    suppressMessages(axes_reliability(d, items = single, angles = ang3)),
    "general and axes variances are not separately identified"
  )
  err <- tryCatch(
    suppressMessages(axes_reliability(d, items = single, angles = ang3)),
    error = function(e) conditionMessage(e)
  )
  expect_false(grepl("scale-specificity", err, fixed = TRUE))
  # Both still refuse, and for the same underlying reason.
  expect_match(err, "at least 4 equally spaced scales")
})

test_that("M61 review F4: nb_reason carries every reason that applies", {
  skip_if_not_installed("lavaan")
  ang <- (seq_len(8L) - 1L) * 45
  pop <- axes_population_cor(ang, 1L, .20, .05, zeta1 = 0)
  R <- pop$sigma
  inames <- sprintf("c%02d", seq_len(nrow(R)))
  dimnames(R) <- list(inames, inames)
  items <- split(inames, factor(pop$scale, levels = seq_along(ang)))

  # A correlation matrix whose scales each carry one item: BOTH unavailabilities
  # hold at once. Reporting only "cormat" would hide the alpha-undefined fact
  # and make it unrecoverable from `details`.
  res <- suppressMessages(
    axes_reliability(cormat = R, items = items, angles = ang, n = 500L)
  )
  expect_setequal(res$details$nb_reason, c("cormat", "single_item"))
  expect_true(all(is.na(res$results$nb_reliability)))
  expect_false(any(is.nan(res$results$nb_reliability)))
  out <- paste(utils::capture.output(print(res)), collapse = "\n")
  expect_match(out, "correlation-matrix path")
  expect_match(out, "undefined for a scale carrying only one item")

  # Each reason still stands alone where only one applies.
  fx <- axes_spaced_fixture(ang, n = 900L, k = 1L)
  r1 <- suppressMessages(
    axes_reliability(fx$data, items = fx$items, angles = fx$angles)
  )
  expect_identical(r1$details$nb_reason, "single_item")
  fx2 <- axes_spaced_fixture(ang, n = 900L, k = 3L)
  r2 <- suppressMessages(
    axes_reliability(cormat = stats::cor(as.matrix(fx2$data)),
                     items = fx2$items, angles = fx2$angles, n = 900L)
  )
  expect_identical(r2$details$nb_reason, "cormat")
})

# --- M63: blockwise instruments and the zeta2 component -----------------------
#
# Blocks group items by something OTHER than their scale (Strack's type d), so
# the canonical blockwise design takes one item from each scale into each block:
# `same-block` and `same-scale` are then genuinely different indicators, which
# is exactly the condition that identifies zeta2.

# Four scales, k items each, with `nb` blocks laid across the scales: item j of
# every scale goes to block j. Requires k == nb, the balanced case.
axes_block_fixture <- function(angles = c(0, 90, 180, 270), k = 2L) {
  inames <- sprintf("i%02d", seq_len(length(angles) * k))
  scale_of <- rep(seq_along(angles), each = k)
  block_of <- rep(seq_len(k), times = length(angles))
  list(
    names = inames,
    items = split(inames, factor(scale_of, levels = seq_along(angles))),
    blocks = split(inames, factor(block_of, levels = seq_len(k))),
    angles = angles,
    scale_of = scale_of,
    block_of = block_of
  )
}

test_that("M63 T1: axes_resolve_blocks() maps blocks onto the item order", {
  fx <- axes_block_fixture()
  src <- as.data.frame(matrix(0, nrow = 2, ncol = length(fx$names),
                              dimnames = list(NULL, fx$names)))
  all_cols <- unlist(fx$items, use.names = FALSE)

  # NULL blocks stay NULL -- the no-zeta2 path, unchanged from M61.
  expect_null(axes_resolve_blocks(NULL, src, all_cols))

  got <- axes_resolve_blocks(fx$blocks, src, all_cols)
  # The returned index is aligned with `all_cols`, not with the block list's
  # own order: item j of every scale carries block j.
  expect_identical(got$index, fx$block_of)
  expect_identical(got$labels, c("1", "2"))

  # Numeric indices resolve exactly as names do (axes_colnames()).
  by_num <- axes_resolve_blocks(list(c(1L, 3L, 5L, 7L), c(2L, 4L, 6L, 8L)),
                                src, all_cols)
  expect_identical(by_num$index, got$index)
  expect_identical(by_num$labels, c("Block1", "Block2"))
})

test_that("M63 T1: `blocks` must partition the items, and says which item broke it", {
  fx <- axes_block_fixture()
  src <- as.data.frame(matrix(0, nrow = 2, ncol = length(fx$names),
                              dimnames = list(NULL, fx$names)))
  all_cols <- unlist(fx$items, use.names = FALSE)
  r <- function(b) axes_resolve_blocks(b, src, all_cols)

  # Not a list at all -- the settled API shape is a list of item vectors, so a
  # flat label vector is refused rather than silently reinterpreted.
  expect_error(r(c("A", "A", "B", "B", "A", "A", "B", "B")),
               "must be a list of per-block item")
  expect_error(r(list()), "at least one block")
  # An empty block contributes no items and would silently stop counting.
  expect_error(r(list(character(0), fx$names)), "no items")
  # An item named in a block but absent from the data.
  expect_error(r(list(c("i01", "nope"), fx$names[-1])), "not found.*nope")
  # An item in two blocks at once: the partition is broken by duplication.
  expect_error(r(list(fx$names[1:5], fx$names[5:8])), "more than one block.*i05")
  # An item in no block: the partition is broken by omission.
  expect_error(r(list(fx$names[1:4], fx$names[5:7])), "no block.*i08")
})

test_that("M63 T2: axes_design() names the columns the model will fit", {
  fx <- axes_block_fixture()
  ang <- rep(fx$angles, each = 2L)

  # No blocks: the pre-M63 three-column design, unchanged.
  X0 <- axes_design(ang, fx$scale_of)
  expect_identical(colnames(X0), c("xi2", "xi1", "zeta1"))
  # Blocks laid across the scales: zeta2 joins as a fourth column.
  X1 <- axes_design(ang, fx$scale_of, fx$block_of)
  expect_identical(colnames(X1), c("xi2", "xi1", "zeta1", "zeta2"))
  # The design is the upper triangle of the item-by-item matrix.
  p <- length(ang)
  expect_identical(nrow(X1), as.integer(p * (p - 1L) / 2L))
})

test_that("M63 T2: axes_fits_zeta2() keeps zeta2 only where it is identified", {
  fx <- axes_block_fixture()
  ang <- rep(fx$angles, each = 2L)
  fits <- function(blk) axes_fits_zeta2(ang, fx$scale_of, blk)

  # Identified: blocks cut across the scales, so same-block is a genuinely
  # different indicator from same-scale.
  expect_true(fits(fx$block_of))
  # Blocks that ARE the scales: same-block == same-scale, perfectly confounded
  # with zeta1. This is the case the M63 gate named.
  expect_false(fits(fx$scale_of))
  # One block holding everything: same-block is all ones off the diagonal, so
  # it is the intercept column and carries no information of its own.
  expect_false(fits(rep(1L, length(ang))))
  # Every item its own block: same-block is all zeros off the diagonal -- the
  # zero-column case that killed qr.solve() before M61 handled its zeta1 twin.
  expect_false(fits(seq_along(ang)))
  # No blocks supplied at all.
  expect_false(axes_fits_zeta2(ang, fx$scale_of, NULL))
})

test_that("M63 T2: a block map spanning two scales can still be unidentified", {
  # THE case that decided the M63 gate for a rank check over a structural rule.
  # Four scales, one item each, at 0/90/180/270; blocks pair OPPOSITE scales.
  # Every same-block pair is 180 deg apart (cos = -1) and every cross-block pair
  # is 90 deg apart (cos = 0), so same-block == -cos exactly: the block column
  # is a scalar multiple of the axes column and adds no rank.
  ang <- c(0, 90, 180, 270)
  scale_of <- 1:4
  paired <- c(1L, 2L, 1L, 2L)   # {0,180} and {90,270}

  # A structural rule -- "some block spans >= 2 scales" -- would say identified.
  expect_true(any(tapply(scale_of, paired, function(s) length(unique(s))) >= 2))
  # The rank check says otherwise, and the rank check is right.
  expect_false(axes_fits_zeta2(ang, scale_of, paired))
  expect_identical(colnames(axes_design(ang, scale_of, paired)), c("xi2", "xi1"))

  # Rotating the same pairing off the axes does not rescue it: the collinearity
  # is in the pairing, not the phase.
  expect_false(axes_fits_zeta2(ang + 22.5, scale_of, paired))

  # But blocking ADJACENT scales instead is identified at the same angles --
  # so the refusal above is about this pairing, not about k = 4 or single items.
  adjacent <- c(1L, 1L, 2L, 2L)  # {0,90} and {180,270}
  expect_true(axes_fits_zeta2(ang, scale_of, adjacent))
})

test_that("M63 T2: the OLS shadow recovers zeta2 exactly on the population", {
  fx <- axes_block_fixture(k = 2L)
  ang <- rep(fx$angles, each = 2L)
  xi1 <- .20; xi2 <- .05; zeta1 <- .08; zeta2 <- .06

  # Build the population correlation matrix by hand from the five-component
  # decomposition, so the shadow is checked against arithmetic it never saw.
  p <- length(ang)
  th <- ang * pi / 180
  sig <- xi2 + xi1 * outer(th, th, function(a, b) cos(a - b)) +
    zeta1 * outer(fx$scale_of, fx$scale_of, `==`) +
    zeta2 * outer(fx$block_of, fx$block_of, `==`)
  diag(sig) <- 1

  got <- axes_ols_shadow(sig, ang, fx$scale_of, fx$block_of)
  expect_identical(names(got), c("xi2", "xi1", "zeta1", "zeta2"))
  expect_lt(abs(got[["xi1"]] - xi1), 1e-10)
  expect_lt(abs(got[["xi2"]] - xi2), 1e-10)
  expect_lt(abs(got[["zeta1"]] - zeta1), 1e-10)
  expect_lt(abs(got[["zeta2"]] - zeta2), 1e-10)

  # Omitting the block map from the SAME matrix biases the other components --
  # the block variance has to go somewhere. This is the OLS-side preview of the
  # AC4 claim the CFA makes end to end.
  naive <- axes_ols_shadow(sig, ang, fx$scale_of)
  expect_false("zeta2" %in% names(naive))
  expect_gt(abs(naive[["xi1"]] - xi1) + abs(naive[["xi2"]] - xi2) +
              abs(naive[["zeta1"]] - zeta1), 1e-3)
})

test_that("M63 T3: axes_syntax() emits BS latents sharing one zeta2 label", {
  fx <- axes_block_fixture()
  syn <- axes_syntax(fx$items, fx$angles, item_block = fx$block_of)

  # One block latent per block, loading +1 on that block's items -- and the
  # items are the ACROSS-scale ones, not a scale's worth.
  expect_true(grepl("BS1 =~ 1*i01 + 1*i03 + 1*i05 + 1*i07", syn, fixed = TRUE))
  expect_true(grepl("BS2 =~ 1*i02 + 1*i04 + 1*i06 + 1*i08", syn, fixed = TRUE))
  # Every block variance shares the one zeta2 label (the model's restriction).
  expect_true(grepl("BS1 ~~ zeta2*BS1", syn, fixed = TRUE))
  expect_true(grepl("BS2 ~~ zeta2*BS2", syn, fixed = TRUE))
  # The rest of the model is untouched by the addition.
  expect_true(grepl("AX ~~ xi1*AX", syn, fixed = TRUE))
  expect_true(grepl("GEN ~~ xi2*GEN", syn, fixed = TRUE))
  expect_true(grepl("SS1 ~~ zeta1*SS1", syn, fixed = TRUE))
})

test_that("M63 T3: an unidentified block map emits no BS latents at all", {
  fx <- axes_block_fixture()
  # Blocks that ARE the scales: axes_fits_zeta2() is FALSE, so the component is
  # dropped from the model rather than fitted to a confounded moment -- exactly
  # how M61 drops zeta1, and read off the same design.
  syn <- axes_syntax(fx$items, fx$angles, item_block = fx$scale_of)
  expect_false(grepl("BS1", syn, fixed = TRUE))
  expect_false(grepl("zeta2", syn, fixed = TRUE))
  expect_true(grepl("no block-specificity component", syn, fixed = TRUE))
  # zeta1 is still there -- dropping zeta2 must not take its neighbour with it.
  expect_true(grepl("SS1 ~~ zeta1*SS1", syn, fixed = TRUE))

  # No blocks supplied: byte-identical to the pre-M63 emission.
  expect_identical(axes_syntax(fx$items, fx$angles),
                   axes_syntax(fx$items, fx$angles, item_block = NULL))
  expect_false(grepl("zeta2", axes_syntax(fx$items, fx$angles), fixed = TRUE))
})

test_that("M63 T3: zeta2 takes a start modifier only when the seed carries it", {
  fx <- axes_block_fixture()
  # A dyadic seed (1/16) so fmt()'s full-precision printing is exact: .06 would
  # print as 0.059999999999999998 and a regex pinned to "0.06" would never
  # match. The M61 comment warns about the digit COUNT; the value's own decimal
  # expansion is the other half of the same trap.
  seed <- c(xi2 = .05, xi1 = .20, zeta1 = .08, zeta2 = .0625)
  syn <- axes_syntax(fx$items, fx$angles, item_block = fx$block_of, start = seed)
  expect_match(syn, "BS1 ~~ start\\(0\\.0625[0-9]*\\)\\*zeta2\\*BS1")
  expect_match(syn, "BS2 ~~ start\\(0\\.0625[0-9]*\\)\\*zeta2\\*BS2")

  # A seed WITHOUT zeta2 (the shadow returns none when zeta2 is unidentified)
  # must emit no modifier rather than erroring on the missing name -- the same
  # trap M61 hit with zeta1.
  seed3 <- c(xi2 = .05, xi1 = .20, zeta1 = .08)
  syn3 <- expect_no_error(
    axes_syntax(fx$items, fx$angles, item_block = fx$block_of, start = seed3)
  )
  expect_true(grepl("BS1 ~~ zeta2*BS1", syn3, fixed = TRUE))
})

test_that("M63 T4: the shadow drops BOTH specificity columns when neither is identified", {
  # One item per scale (no zeta1) AND blocks that are the scales (no zeta2):
  # the design falls back to the two-column form M61 introduced. Neither drop
  # may take the other's column with it.
  ang <- c(0, 90, 180, 270)
  scale_of <- 1:4
  sig <- 0.05 + 0.20 * outer(ang * pi / 180, ang * pi / 180,
                             function(a, b) cos(a - b))
  diag(sig) <- 1

  both <- axes_ols_shadow(sig, ang, scale_of, scale_of)
  expect_identical(names(both), c("xi2", "xi1"))
  expect_lt(abs(both[["xi1"]] - .20), 1e-10)

  # One item per scale but blocks that DO cut across: zeta2 survives alone,
  # with no zeta1 beside it (Strack's type e administered in blocks).
  crossed <- c(1L, 1L, 2L, 2L)
  sig2 <- sig + 0.06 * outer(crossed, crossed, `==`)
  diag(sig2) <- 1
  only2 <- axes_ols_shadow(sig2, ang, scale_of, crossed)
  expect_identical(names(only2), c("xi2", "xi1", "zeta2"))
  expect_lt(abs(only2[["zeta2"]] - .06), 1e-10)
})

test_that("M63 T5: axes_population_cor() carries zeta2 into the population", {
  ang <- c(0, 90, 180, 270)
  k <- 2L
  blk <- axes_crossed_blocks(length(ang), k)
  expect_identical(blk, c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L))

  pop <- axes_population_cor(ang, k, xi1 = .20, xi2 = .05, zeta1 = .08,
                             zeta2 = .06, item_block = blk)
  # A same-block, different-scale pair carries xi2 + xi1*cos(dtheta) + zeta2 --
  # and NOT zeta1, which is what makes the two components separable.
  # Items 1 and 3: scales 1 and 2 (0 and 90 deg), both block 1.
  expect_lt(abs(pop$sigma[1, 3] - (.05 + .20 * cos(pi / 2) + .06)), 1e-12)
  # Items 1 and 2: same scale (0 deg), different blocks -> zeta1, not zeta2.
  expect_lt(abs(pop$sigma[1, 2] - (.05 + .20 * 1 + .08)), 1e-12)
  # Items 1 and 4: different scale, different block -> neither specificity.
  expect_lt(abs(pop$sigma[1, 4] - (.05 + .20 * cos(pi / 2))), 1e-12)
  expect_true(all(diag(pop$sigma) == 1))

  # Omitting the block map reproduces the pre-M63 population EXACTLY, so no
  # existing oracle silently moves under this change.
  expect_identical(
    axes_population_cor(ang, k, .20, .05, .08)$sigma,
    axes_population_cor(ang, k, .20, .05, .08, zeta2 = .06)$sigma
  )

  # The population must stay a valid correlation matrix at these settings.
  expect_gt(min(eigen(pop$sigma, symmetric = TRUE, only.values = TRUE)$values), 0)
})

test_that("M63 T5: the shadow recovers all four components off a simulated draw", {
  ang <- c(0, 45, 90, 135, 180, 225, 270, 315)
  k <- 2L
  blk <- axes_crossed_blocks(length(ang), k)
  truth <- c(xi1 = .20, xi2 = .05, zeta1 = .08, zeta2 = .06)

  set.seed(4242L)
  dat <- axes_simulate(6000L, ang, k, truth[["xi1"]], truth[["xi2"]],
                       truth[["zeta1"]], zeta2 = truth[["zeta2"]],
                       item_block = blk)
  expect_identical(ncol(dat), length(ang) * k)

  got <- axes_ols_shadow(stats::cor(as.matrix(dat)),
                         rep(ang, each = k), rep(seq_along(ang), each = k), blk)
  # Finite-sample, so an absolute bound wide enough for sampling noise at
  # n = 6000 but far narrower than the .06 signal being detected (M59/M61: set
  # the bar from the discrimination required, and state it absolutely).
  for (nm in names(truth)) expect_lt(abs(got[[nm]] - truth[[nm]]), .02)
})

test_that("M63 T6: axes_is_boundary() catches a negative zeta2", {
  # The new disjunct, tested on the UNMOCKED predicate: a negative block
  # variance is not a usable solution, exactly as a negative zeta1 is not.
  # (M62 lesson (i): mock the seam and the arithmetic under test never runs.)
  expect_true(axes_is_boundary(.2, .05, .08, c(.5, .5), zeta2 = -.01))
  expect_false(axes_is_boundary(.2, .05, .08, c(.5, .5), zeta2 = .01))
  # zeta2 = NULL is the no-blocks path and must not read as a boundary.
  expect_false(axes_is_boundary(.2, .05, .08, c(.5, .5), zeta2 = NULL))
  # A negative zeta2 must not be masked by, or mask, the other disjuncts.
  expect_true(axes_is_boundary(.2, .05, NULL, c(.5, .5), zeta2 = -.01))
  expect_true(axes_is_boundary(0, .05, .08, c(.5, .5), zeta2 = .01))
  # Default NULL keeps every pre-M63 call site's behaviour byte-identical.
  expect_false(axes_is_boundary(.2, .05, .08, c(.5, .5)))
})

test_that("M63 T6: axes_reliability() fits and reports zeta2 end to end", {
  skip_if_not_installed("lavaan")
  ang <- c(0, 45, 90, 135, 180, 225, 270, 315)
  k <- 2L
  blk_idx <- axes_crossed_blocks(length(ang), k)
  set.seed(909L)
  dat <- axes_simulate(3000L, ang, k, xi1 = .20, xi2 = .05, zeta1 = .08,
                       zeta2 = .06, item_block = blk_idx)
  inames <- colnames(dat)
  items <- split(inames, rep(seq_along(ang), each = k))
  blocks <- split(inames, blk_idx)

  res <- suppressMessages(
    axes_reliability(dat, items = items, angles = ang, blocks = blocks)
  )
  expect_s3_class(res, "circumplex_axes_reliability")
  expect_true(res$details$zeta2_fitted)
  expect_identical(res$details$blocks, c("1", "2"))
  # Five component rows now: general, axes, scale, block, item.
  expect_identical(res$components$Symbol,
                   c("xi2", "xi1", "zeta1", "zeta2", "epsilon"))
  z2 <- res$components$Estimate[res$components$Symbol == "zeta2"]
  expect_lt(abs(z2 - .06), .02)
  expect_true(is.finite(res$components$SE[res$components$Symbol == "zeta2"]))
  # And the estimate the whole milestone exists for: xi1 recovered, not
  # deflated by block variance leaking into the other components.
  expect_lt(abs(res$components$Estimate[res$components$Symbol == "xi1"] - .20),
            .02)
  expect_false(res$results$boundary[[1]])
  expect_true(all(is.finite(res$results$reliability)))
  expect_true(all(is.finite(res$results$sem)))

  # Blocks that are the scales: accepted, but reported as not fitted, with the
  # four-row component set and no NA/NaN anywhere.
  res_u <- suppressMessages(
    axes_reliability(dat, items = items, angles = ang, blocks = items)
  )
  expect_false(res_u$details$zeta2_fitted)
  expect_identical(res_u$components$Symbol, c("xi2", "xi1", "zeta1", "epsilon"))
  expect_true(all(is.finite(res_u$results$reliability)))

  # No blocks at all reproduces the pre-M63 result EXACTLY on the same data.
  res_n <- suppressMessages(
    axes_reliability(dat, items = items, angles = ang)
  )
  expect_false(res_n$details$zeta2_fitted)
  expect_null(res_n$details$blocks)
  expect_equal(res_u$results$reliability, res_n$results$reliability,
               tolerance = 1e-10)
})

# --- M63 T7: the Layer-B oracle for zeta2 -------------------------------------
#
# Three claims, each on the EXACT population matrix so no sampling noise stands
# between the model and its truth: the fit recovers zeta2 (AC3), the bias from
# omitting it is conditional on block geometry (AC4, per M63-D2), and three
# independent engines agree (AC5).

# Fit the exact population with likelihood = "wishart" (the N-1 divisor), the
# BC5 convention: lavaan's default ML rescales by (N-1)/N and would miss truth
# by ~.0003 at N = 500 for reasons that have nothing to do with zeta2.
axes_pop_fit_components <- function(sigma, items, angles_deg, item_block) {
  fit <- lavaan::cfa(
    axes_syntax(items, angles_deg, item_block = item_block),
    sample.cov = sigma, sample.nobs = 500L,
    orthogonal = TRUE, likelihood = "wishart"
  )
  pe <- lavaan::parameterEstimates(fit)
  v <- function(l) pe$est[pe$op == "~~" & pe$lhs == l & pe$rhs == l][[1]]
  out <- c(xi1 = v("AX"), xi2 = v("GEN"), zeta1 = v("SS1"))
  if (!is.null(item_block) &&
      axes_fits_zeta2(rep(angles_deg, times = lengths(items)),
                      rep(seq_along(items), times = lengths(items)),
                      item_block)) {
    out[["zeta2"]] <- v("BS1")
  }
  out
}

axes_zeta2_pop <- function(angles, k, truth, item_block) {
  pop <- axes_population_cor(angles, k, truth[["xi1"]], truth[["xi2"]],
                             truth[["zeta1"]], zeta2 = truth[["zeta2"]],
                             item_block = item_block)
  inames <- sprintf("i%02d", seq_along(pop$scale))
  dimnames(pop$sigma) <- list(inames, inames)
  list(sigma = pop$sigma, scale = pop$scale, names = inames,
       items = split(inames, factor(pop$scale, levels = seq_along(angles))))
}

test_that("M63 T7 (AC3): the fit recovers zeta2 on the exact population", {
  skip_if_not_installed("lavaan")
  ang <- octants()
  k <- 4L
  truth <- c(xi1 = .20, xi2 = .05, zeta1 = .08, zeta2 = .06)
  blk <- axes_crossed_blocks(length(ang), k)
  px <- axes_zeta2_pop(ang, k, truth, blk)

  got <- axes_pop_fit_components(px$sigma, px$items, ang, blk)
  expect_identical(names(got), c("xi1", "xi2", "zeta1", "zeta2"))
  # Absolute bounds, stated absolutely (M61's relative-tolerance trap). 1e-4 is
  # four orders below the .06 signal, so it fences the estimate without pinning
  # optimizer noise (M59: set the bar from the discrimination required).
  for (nm in names(truth)) expect_lt(abs(got[[nm]] - truth[[nm]]), 1e-4)

  # The population must be a genuine correlation matrix at these settings, or
  # "recovery" would be recovery of something unreachable.
  expect_gt(min(eigen(px$sigma, symmetric = TRUE, only.values = TRUE)$values), 0)
  expect_true(all(diag(px$sigma) == 1))
})

test_that("M63 T7 (AC4): the omitted-zeta2 bias in xi1 is conditional on geometry", {
  skip_if_not_installed("lavaan")
  ang <- octants()
  k <- 4L
  truth <- c(xi1 = .20, xi2 = .05, zeta1 = .08, zeta2 = .06)
  item_scale <- rep(seq_along(ang), each = k)

  # Angle-BALANCED: each block draws one item from every scale.
  balanced <- axes_crossed_blocks(length(ang), k)
  # Angle-CLUSTERED: each block spans a contiguous half of the circle.
  clustered <- ifelse(item_scale <= 4, 1L, 2L)
  # Both must be identified, or the comparison is about identifiability rather
  # than about geometry.
  expect_true(axes_fits_zeta2(rep(ang, each = k), item_scale, balanced))
  expect_true(axes_fits_zeta2(rep(ang, each = k), item_scale, clustered))

  for (case in list(list(blk = balanced, bal = TRUE),
                    list(blk = clustered, bal = FALSE))) {
    px <- axes_zeta2_pop(ang, k, truth, case$blk)
    # Fit the SAME population with zeta2 omitted from the model.
    naive <- axes_pop_fit_components(px$sigma, px$items, ang, item_block = NULL)
    expect_false("zeta2" %in% names(naive))
    bias_xi1 <- naive[["xi1"]] - truth[["xi1"]]

    if (case$bal) {
      # Provably zero: within-block pairs are all cross-scale and span every
      # scale pair uniformly, so same-block is orthogonal to cos(theta_i -
      # theta_j) and omitting it cannot move the cosine coefficient (M63-D2).
      #
      # The bound is set from the DISCRIMINATION required, not from what this
      # machine printed (M59): the alternative hypothesis is the clustered
      # branch below at +.024, so 1e-4 still separates the two by 240x while
      # sitting ~3400x above the observed 2.9e-8. The exact-arithmetic route
      # (the OLS shadow) gives -7.5e-16; the gap is the ML optimizer's own
      # convergence tolerance, which is platform-variable, so a bound near
      # machine epsilon would fence the optimizer rather than the claim.
      expect_lt(abs(bias_xi1), 1e-4)
    } else {
      # Angle-clustered blocks correlate with the cosine column, and there the
      # component genuinely protects xi1: >= 10% of truth.
      expect_gt(abs(bias_xi1), .10 * truth[["xi1"]])
    }
    # The one unconditional claim: the general factor absorbs block variance
    # under BOTH geometries.
    expect_gt(naive[["xi2"]] - truth[["xi2"]], .005)
  }
})

test_that("M63 review: even angular spread does NOT make omitting zeta2 safe", {
  skip_if_not_installed("lavaan")
  # The review counterexample to the FIRST wording of this milestone's own
  # conditional. That wording said xi1 was unaffected when "each block draws
  # about evenly from around the circle", which is false: blocks pairing
  # diametrically opposite scales are maximally dispersed -- every block's
  # angles average to the centre of the circle, mean resultant length 0 -- and
  # still bias xi1, because every within-block pair sits half a turn apart and
  # that IS information about angular distance.
  #
  # This test fences the CONDITION, not two instances of it: the earlier AC4
  # test exercises only crossed and contiguous layouts, so restating the rule
  # wrongly reddened nothing. Here an even-spread layout is asserted to be
  # UNSAFE, which is exactly what the false rule denied.
  ang <- octants()
  k <- 2L
  truth <- c(xi1 = .20, xi2 = .05, zeta1 = .08, zeta2 = .06)
  item_scale <- rep(seq_along(ang), each = k)
  item_angle <- rep(ang, each = k)
  antipodal <- ((item_scale - 1L) %% 4L) + 1L   # pairs each scale with its opposite

  # Every block is maximally dispersed: its angles average to the circle centre.
  for (b in unique(antipodal)) {
    th <- item_angle[antipodal == b] * pi / 180
    mrl <- Mod(mean(complex(real = cos(th), imaginary = sin(th))))
    expect_lt(mrl, 1e-12)
  }
  # ...and it is identified, so this is a real fit and not a dropped component.
  expect_true(axes_fits_zeta2(item_angle, item_scale, antipodal))

  px <- axes_zeta2_pop(ang, k, truth, antipodal)
  naive <- axes_pop_fit_components(px$sigma, px$items, ang, item_block = NULL)
  bias <- naive[["xi1"]] - truth[["xi1"]]
  # Biased DOWNWARD by ~9% of truth -- opposite in sign to the contiguous case,
  # which is why "it can go either way" is the honest statement.
  expect_lt(bias, -.05 * truth[["xi1"]])
  # And the component recovers truth when it IS fitted, so the bias is the
  # omission's doing rather than a misspecified population.
  full <- axes_pop_fit_components(px$sigma, px$items, ang, antipodal)
  expect_lt(abs(full[["xi1"]] - truth[["xi1"]]), 1e-4)

  # The one layout the docs DO promise is safe: one item per scale per block.
  safe <- axes_crossed_blocks(length(ang), k)
  px_s <- axes_zeta2_pop(ang, k, truth, safe)
  naive_s <- axes_pop_fit_components(px_s$sigma, px_s$items, ang,
                                     item_block = NULL)
  expect_lt(abs(naive_s[["xi1"]] - truth[["xi1"]]), 1e-4)
})

test_that("M63 review: opposite-scale blocks are identified except at k = 4", {
  # The docs originally offered opposite-scale blocks as an example of a map
  # the rank check REFUSES. That holds only at four scales, where same-block
  # equals -cos exactly; at six, eight and twelve the pairing is identified.
  # Eight is this package's canonical layout, so the example was wrong exactly
  # where most users live. Pinned so the claim cannot be reinstated.
  for (kk in c(6L, 8L, 12L)) {
    a <- seq(0, 360 - 360 / kk, length.out = kk)
    s <- rep(seq_len(kk), each = 2L)
    expect_true(axes_fits_zeta2(rep(a, each = 2L), s,
                                ((s - 1L) %% (kk / 2L)) + 1L))
  }
  # k = 4 remains the genuine collinear case (the T2 test above covers why).
  expect_false(axes_fits_zeta2(rep(c(0, 90, 180, 270), each = 2L),
                               rep(1:4, each = 2L),
                               ((rep(1:4, each = 2L) - 1L) %% 2L) + 1L))
})

test_that("M63 review: xi2 inflation is not unconditional", {
  # The shipped prose said the general factor absorbs block variance "in every
  # configuration". It does not: this layout leaves xi2 exactly untouched while
  # xi1 carries -0.25 * zeta2. Corrected to "inflated under most layouts,
  # never deflated" -- and pinned here so the stronger claim cannot return.
  ang <- c(0, 90, 180, 270)
  k <- 2L
  item_scale <- rep(seq_along(ang), each = k)
  item_angle <- rep(ang, each = k)
  blk <- c(1L, 2L, 4L, 4L, 2L, 1L, 3L, 3L)
  X <- axes_design(item_angle, item_scale)
  ut <- upper.tri(matrix(0, length(item_scale), length(item_scale)))
  aux <- qr.solve(X, as.numeric(outer(blk, blk, `==`)[ut]))
  # Intercept coefficient exactly zero -> zero xi2 bias at any zeta2.
  expect_lt(abs(aux[[1]]), 1e-10)
  # while the cosine coefficient is emphatically not zero.
  expect_gt(abs(aux[[2]]), .2)
})

test_that("M63 T7 (AC4): closed-form omitted-variable bias predicts the fitted bias", {
  skip_if_not_installed("lavaan")
  # An independent route to the same number, so the conditional above rests on
  # a derivation and not only on a fitted value: for y = X*beta + gamma*z, the
  # bias in beta from dropping z is gamma * (X'X)^-1 X'z. The cosine element of
  # that auxiliary solve IS the xi1 bias per unit zeta2.
  ang <- octants()
  k <- 4L
  truth <- c(xi1 = .20, xi2 = .05, zeta1 = .08, zeta2 = .06)
  item_scale <- rep(seq_along(ang), each = k)
  item_angle <- rep(ang, each = k)
  X <- axes_design(item_angle, item_scale)
  ut <- upper.tri(matrix(0, length(item_scale), length(item_scale)))

  for (blk in list(axes_crossed_blocks(length(ang), k),
                   ifelse(item_scale <= 4, 1L, 2L))) {
    z <- as.numeric(outer(blk, blk, `==`)[ut])
    predicted <- truth[["zeta2"]] * qr.solve(X, z)[[2]]
    px <- axes_zeta2_pop(ang, k, truth, blk)
    observed <- axes_pop_fit_components(px$sigma, px$items, ang,
                                        item_block = NULL)[["xi1"]] -
      truth[["xi1"]]
    # Two routes, one number. The bound discriminates against "the algebra is
    # wrong", which would put predicted and observed a whole bias apart (~.024),
    # so 1e-3 separates them by ~24x while leaving ~96x over the observed
    # 1.04e-5 residual -- the ML optimizer's convergence tolerance again, not a
    # disagreement between the two routes (M59).
    expect_lt(abs(predicted - observed), 1e-3)
  }
})

# OpenMx route for the five-component model: the same Sigma built from matrix
# algebra rather than lavaan syntax, so agreement is between two independent
# implementations of the model and not two calls into one. Mirrors BC7's helper
# with the block matrix added.
axes_mx_components_zeta2 <- function(S, n, angles_deg, n_items, item_block) {
  p <- nrow(S)
  nm <- rownames(S)
  scale <- rep(seq_along(angles_deg), each = n_items)
  th <- rep(as.numeric(angles_deg), each = n_items) * pi / 180
  model <- OpenMx::mxModel(
    "axes2",
    OpenMx::mxMatrix("Full", 1, 1, free = TRUE, values = .15, lbound = 0,
                     name = "xi1"),
    OpenMx::mxMatrix("Full", 1, 1, free = TRUE, values = .05, lbound = 0,
                     name = "xi2"),
    OpenMx::mxMatrix("Full", 1, 1, free = TRUE, values = .10, lbound = 0,
                     name = "zeta1"),
    OpenMx::mxMatrix("Full", 1, 1, free = TRUE, values = .05, lbound = 0,
                     name = "zeta2"),
    OpenMx::mxMatrix("Full", p, 1, free = TRUE, values = .5, lbound = 0,
                     name = "eps"),
    OpenMx::mxMatrix("Full", p, p, free = FALSE,
                     values = outer(th, th, function(a, b) cos(a - b)),
                     name = "C"),
    OpenMx::mxMatrix("Full", p, p, free = FALSE, values = 1, name = "J"),
    OpenMx::mxMatrix("Full", p, p, free = FALSE,
                     values = outer(scale, scale, `==`) * 1, name = "B"),
    OpenMx::mxMatrix("Full", p, p, free = FALSE,
                     values = outer(item_block, item_block, `==`) * 1,
                     name = "K"),
    OpenMx::mxAlgebra(
      xi1[1, 1] * C + xi2[1, 1] * J + zeta1[1, 1] * B + zeta2[1, 1] * K +
        vec2diag(eps),
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
    zeta1 = OpenMx::mxEval(zeta1, fit)[1, 1],
    zeta2 = OpenMx::mxEval(zeta2, fit)[1, 1]
  )
}

test_that("M63 T7 (AC5): lavaan, OpenMx and the OLS shadow agree on zeta2", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("OpenMx")
  ang <- octants()
  k <- 2L
  blk <- axes_crossed_blocks(length(ang), k)
  item_scale <- rep(seq_along(ang), each = k)
  item_angle <- rep(ang, each = k)

  for (seed in c(11L, 12L)) {
    set.seed(seed)
    dat <- as.data.frame(scale(
      axes_simulate(2500L, ang, k, .20, .05, .08, zeta2 = .06,
                    item_block = blk)
    ))
    inames <- sprintf("i%02d", seq_len(ncol(dat)))
    colnames(dat) <- inames
    items <- split(inames, factor(item_scale, levels = seq_along(ang)))
    S <- stats::cov(dat)

    lav <- axes_pop_fit_components(S, items, ang, blk)[c("xi1", "xi2",
                                                         "zeta1", "zeta2")]
    mx <- axes_mx_components_zeta2(S, 2500L, ang, k, blk)
    ols <- axes_ols_shadow(stats::cor(as.matrix(dat)), item_angle, item_scale,
                           blk)[c("xi1", "xi2", "zeta1", "zeta2")]

    # Two SEM engines on the same sample should agree to optimizer precision;
    # 1e-3 leaves room for their different parameterizations (BC7 observes
    # ~6e-5 on the four-component model).
    expect_lt(max(abs(lav - mx)), 1e-3)
    # The OLS shadow is a method-of-moments estimator, not ML, so it agrees to
    # sampling order rather than to optimizer precision -- .02 fences it well
    # inside the .06 signal it must resolve.
    expect_lt(max(abs(lav - ols)), .02)
  }
})

test_that("M63 T8 (AC5): the blocked Table 3 rows reproduce Rel and SEm (Layer A)", {
  # The six rows in Table 3 (p. 7) printing a col-8 block-specificity value:
  # the three blocked type-a rows excluded from the BC1 sweep, and the three
  # type-d (OCAI) rows. Banked in cairn/references/strack2013.md (M63 T8), two
  # channels. These anchor the FORMULA layer only -- the paper prints no
  # correlation matrix, and reliability never touches zeta2 -- so they fence
  # Spearman-Brown and the SEm identity, never the zeta2 estimator.
  tbl <- data.frame(
    inst   = c("CSIV", "TRC-g", "TRC-t", "OCAI", "OCAI", "OCAI"),
    persp  = c("Self", "Self", "Self", "Self", "Other", "Meta"),
    gen    = c(13.5, 11.6, 13.6, 31.6, 42.6, 48.2),
    axes   = c(14.8,  8.0,  5.5, 11.7,  7.8,  7.3),
    scale  = c( 4.2,  4.9,  6.5,  3.8,  0.6,  3.4),
    block  = c( 2.8,  3.7,  6.7,  2.6,  4.9,  5.2),
    item   = c(67.6, 71.8, 67.7, 50.2, 44.1, 36.5),
    item_n = c(  32,   20,   20,    8,    8,    8),
    rel    = c( .84,  .63,  .54,  .51,  .40,  .38),
    rawvar = c(0.60, 1.89, 1.23, 15.95, 9.98, 9.64),
    sem    = c(0.31, 0.83, 0.75,  2.78, 2.44, 2.43),
    stringsAsFactors = FALSE
  )

  # Spearman-Brown on printed col 6 / col 10 reproduces printed col 11, every
  # row, at the paper's own print precision.
  sb <- axis_reliability_sb(tbl$axes / 100, tbl$item_n)
  expect_true(all(abs(sb - tbl$rel) < .01))
  # SEm = sqrt(raw variance) * sqrt(1 - Rel) reproduces printed col 13. The
  # .02 slack is the BC2 convention: the inputs are printed pre-rounded.
  expect_true(all(abs(axis_sem(tbl$rel, sqrt(tbl$rawvar)) - tbl$sem) < .02))

  # The five-component sum. Four rows are self-consistent; two are the source's
  # own pre-existing defects, PINNED with their printed sums rather than
  # averaged away or silently excluded (RR10's ruling for the IIP S6 erratum).
  sums <- tbl$gen + tbl$axes + tbl$scale + tbl$block + tbl$item
  ok <- !(tbl$inst == "CSIV" | (tbl$inst == "OCAI" & tbl$persp == "Meta"))
  expect_true(all(abs(sums[ok] - 100) < .15))     # 100.0, 100.0, 99.9, 100.0
  expect_equal(sums[tbl$inst == "CSIV"], 102.9, tolerance = 1e-8)
  expect_equal(sums[tbl$inst == "OCAI" & tbl$persp == "Meta"], 100.6,
               tolerance = 1e-8)

  # Every row carries a nonzero block-specificity: that is what makes these the
  # zeta2 population and not a slice of the non-blocked sweep.
  expect_true(all(tbl$block > 0))
})

test_that("M63 T9 (AC7): every documented surface names the block component", {
  # man/ in the dev tree, Rd_db() once installed -- the dual-source pattern
  # test-rd-latex-safe.R already uses, because a man/-only guard silently
  # SKIPS under R CMD check (installed packages carry help/, not man/) and a
  # Rd_db()-only guard errors under load_all(). The M7 lesson is that a guard
  # reachable on only one of those paths runs in neither gate that ships.
  rd_file <- test_path("..", "..", "man", "axes_reliability.Rd")
  rd <- if (file.exists(rd_file)) {
    paste(readLines(rd_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  } else {
    db <- tools::Rd_db("circumplex")
    paste(as.character(db[["axes_reliability.Rd"]]), collapse = "")
  }
  # Fail loudly rather than pass vacuously if neither source yielded anything.
  expect_gt(nchar(rd), 1000L)

  # Each assertion pins a structure that exists ONLY if the documentation does.
  # A bare expect_match(rd, "blocks") is FALSE COVERAGE and was measured to be:
  # the prose section says "administered in blocks", so deleting @param blocks
  # left the guard green (the M39/M40 trap). \item{blocks}{ is emitted only by
  # the \arguments entry, so it cannot be satisfied by prose.
  expect_match(rd, "\\item{blocks}{", fixed = TRUE)
  # Likewise for the flag: pin the \value phrasing, not the bare token, which
  # also appears in the Blockwise prose two sections away.
  expect_match(rd, "zeta1_fitted} and \\code{zeta2_fitted}", fixed = TRUE)
  # The @return enumerates the component-row counts, and five is now reachable.
  expect_match(rd, "five when block", fixed = TRUE)

  # COMPLETENESS, not just presence. The first version of this guard asserted
  # only that the new text existed, and review caught two enumerations of the
  # component set that still listed four members -- the M56/M62 "widening a
  # definition strands its other descriptions" lesson, fourth recurrence. The
  # asymmetry is the trap: a sweep for the OLD claim's keywords finds stale
  # negative claims to delete, and is blind to positive lists that need
  # EXTENDING. So pin the description's own enumeration: it runs from the
  # general factor to item specificity, and block specificity must sit inside
  # it. Deleting the member from that sentence reddens this.
  desc <- sub(".*decomposes each item's variance into orthogonal components",
              "", rd)
  desc <- sub("and reads the axes.*", "", desc)
  expect_match(desc, "block specificity", fixed = TRUE)
  # The corrected conditional replaced the unconditional caveat: the claim that
  # block variance deflates the axes share unconditionally is FALSE for an
  # angle-balanced layout (M63-D2) and must not have survived anywhere.
  expect_false(grepl("treat axes reliability from a blockwise", rd,
                     fixed = TRUE))
  # The corrected condition (review F1). The docs must state the safe case as
  # one-item-per-scale, NOT as "spread evenly around the circle", which review
  # disproved with a maximally-dispersed counterexample that still biases xi1.
  expect_match(rd, "one item from every scale", fixed = TRUE)
  # And the disproved framing must not come back in either of its two forms:
  # the "angularly clustered" safety rule, or the k=4-only worked example that
  # claimed opposite-scale blocks are refused.
  expect_false(grepl("angularly clustered", rd, fixed = TRUE))
  expect_false(grepl("diametrically opposite scales, say", rd, fixed = TRUE))

  # print()/summary() render the components table generically, so the new row
  # must reach the console without either method enumerating components itself.
  skip_if_not_installed("lavaan")
  ang <- octants()
  blk_idx <- axes_crossed_blocks(length(ang), 2L)
  set.seed(77L)
  dat <- axes_simulate(1200L, ang, 2L, .20, .05, .08, zeta2 = .06,
                       item_block = blk_idx)
  inames <- colnames(dat)
  res <- suppressMessages(axes_reliability(
    dat, items = split(inames, rep(seq_along(ang), each = 2L)),
    angles = ang, blocks = split(inames, blk_idx)
  ))
  out <- paste(utils::capture.output(summary(res)), collapse = "\n")
  expect_match(out, "block_specificity", fixed = TRUE)
})


# --- M70: a self-describing object -------------------------------------------
#
# `p*/N` indexes the calibration table the vignette documents, so both halves
# have to be readable off the object without the user recomputing either.

# THE fitted lavaan object axes_reliability() built, caught on its way through
# the axes_converged() seam. Refitting an equivalent model instead would test
# the refit's own row selection -- which is the very thing under test here,
# since `n` is meant to be the N lavaan was actually handed.
axes_capture_fit <- function(thunk) {
  captured <- NULL
  res <- testthat::with_mocked_bindings(
    thunk(),
    axes_converged = function(fit) {
      captured <<- fit
      isTRUE(lavaan::lavInspect(fit, "converged"))
    }
  )
  list(
    res = res,
    ntotal = unname(lavaan::fitMeasures(captured, "ntotal")),
    baseline = unname(lavaan::fitMeasures(captured, c("baseline.chisq",
                                                      "baseline.df")))
  )
}

test_that("AC1: details reports p* and the N the fit was priced at", {
  skip_if_not_installed("lavaan")
  fx <- axes_valid_fixture(n = 1200L, k = 3L, seed = 70L)
  p <- length(fx$names)
  expect_identical(p, 24L)               # AC1's known-p fixture
  pstar <- p * (p + 1) / 2               # 300

  # Thirty rows missing entirely plus scattered item-level gaps, so that `n`,
  # `n_total` and `n_complete` are three DIFFERENT numbers on both raw paths.
  # Measured on the package's own example data all three are 500, and every
  # assertion below would pass while reading the wrong field.
  set.seed(70)
  holed <- fx$data
  holed[1:30, ] <- NA_real_
  for (j in 1:6) holed[sample(31:nrow(holed), 40L), j] <- NA_real_

  lw <- axes_capture_fit(function() {
    suppressMessages(axes_reliability(holed, items = fx$items, angles = fx$oct))
  })
  expect_identical(lw$res$details$n_moments, pstar)
  expect_equal(as.numeric(lw$res$details$n), lw$ntotal)
  expect_lt(lw$res$details$n, lw$res$details$n_total)

  fm <- axes_capture_fit(function() {
    suppressMessages(axes_reliability(holed, items = fx$items, angles = fx$oct,
                                      missing = "fiml"))
  })
  expect_identical(fm$res$details$n_moments, pstar)
  expect_equal(as.numeric(fm$res$details$n), fm$ntotal)
  expect_lt(fm$res$details$n, fm$res$details$n_total)
  expect_lt(fm$res$details$n_complete, fm$res$details$n)
  # The three N's are pairwise distinct on this fixture, which is what makes
  # the equality above discriminating rather than coincidental.
  expect_false(fm$res$details$n == lw$res$details$n)

  cm <- axes_capture_fit(function() {
    suppressMessages(axes_reliability(cormat = stats::cor(fx$data),
                                      items = fx$items, angles = fx$oct,
                                      n = 640L))
  })
  expect_identical(cm$res$details$n_moments, pstar)
  expect_equal(as.numeric(cm$res$details$n), cm$ntotal)
  expect_identical(cm$res$details$n, 640L)
})

test_that("AC1: details exposes the baseline chisq and df as one pair", {
  skip_if_not_installed("lavaan")
  fx <- axes_valid_fixture(n = 900L, k = 3L, seed = 71L)
  got <- axes_capture_fit(function() {
    suppressMessages(axes_reliability(fx$data, items = fx$items,
                                      angles = fx$oct))
  })
  bl <- got$res$details$baseline
  expect_named(bl, c("chisq", "df"))
  expect_equal(unname(bl), got$baseline)
  # The independence model frees p variances out of p*, leaving p(p-1)/2.
  expect_identical(unname(bl[["df"]]), 24 * 23 / 2)
})


test_that("AC2: the Rd names both new fields and what they are for", {
  rd_file <- test_path("..", "..", "man", "axes_reliability.Rd")
  rd <- if (file.exists(rd_file)) {
    paste(readLines(rd_file, warn = FALSE), collapse = " ")
  } else {
    paste(as.character(tools::Rd_db("circumplex")[["axes_reliability.Rd"]]),
          collapse = "")
  }
  expect_gt(nchar(rd), 1000L)
  rd <- gsub("\\s+", " ", rd)

  # Each field name is pinned WITH a verb-carrying phrase of what it is. A bare
  # name match would survive deleting the clause that explains the field, which
  # is the only part a reader needs.
  expect_match(rd, "\\code{n_moments}, the number of distinct analyzed moments",
               fixed = TRUE)
  expect_match(rd,
               "\\code{baseline}, the independence model's \\strong{unscaled}",
               fixed = TRUE)
  # The five-input rebuild, pinned because stating only two of them is the
  # defect the M70 review found in this very sentence.
  expect_match(rd, "five inputs, since the baseline chi-square must be scaled",
               fixed = TRUE)
  # `n` was already shipped; what M70 adds is the sentence saying WHICH of the
  # three sample sizes it is, without which naming it in the vignette is a
  # pointer to an ambiguity.
  expect_match(rd, "Three sample sizes sit beside each other", fixed = TRUE)
  expect_match(rd, "\\code{n} is the one the fit was priced at", fixed = TRUE)
})


test_that("AC2: the vignette's calibration table and its object pointer travel together", {
  # Under R CMD check the tests run from <pkg>.Rcheck/tests/testthat, where
  # ../../vignettes does not exist -- so a bare skip_if_not(file.exists(...))
  # here is satisfied to skip ALWAYS on the surface that matters most, and this
  # guard would never have run on CRAN or CI. The installed copy under
  # inst/doc is the fallback, exactly as the Rd guard above falls back to
  # tools::Rd_db().
  vig <- test_path("..", "..", "vignettes", "axes-reliability.Rmd")
  if (!file.exists(vig)) {
    vig <- system.file("doc", "axes-reliability.Rmd", package = "circumplex")
  }
  skip_if(!nzchar(vig) || !file.exists(vig),
          "the vignette source is not readable from either location")
  txt <- gsub("\\s+", " ", paste(readLines(vig, warn = FALSE), collapse = " "))
  expect_gt(nchar(txt), 1000L)

  has_table <- grepl("| p\\*/N | 0.50 | 0.25 | 0.12 | 0.06 |", txt, fixed = TRUE)
  has_pointer <- grepl("`details$n_moments` is p\\*, and `details$n`", txt,
                       fixed = TRUE)
  # Paired on purpose. A calibration table with no way to locate your own fit
  # on it is the state this milestone existed to end; a pointer to a table that
  # has moved is worse. Removing either one alone reddens here.
  expect_identical(has_table, has_pointer)
  expect_true(has_table)
})


# ---- M89 AC6: the degeneracy criterion tripping inside axes_reliability() ----

test_that("M89 AC6: a degenerate fitted matrix NAs the corrected SEs and the four scaled statistics together", {
  skip_if_not_installed("lavaan")
  oct <- octants()
  pop <- axes_population_cor(oct, 3L, .35, .10, .08)
  sigma <- pop$sigma
  inames <- sprintf("i%02d", seq_len(nrow(sigma)))
  dimnames(sigma) <- list(inames, inames)
  items <- split(inames, pop$scale)

  # No converged fit is known to reach the degenerate regime, so a degenerate
  # fitted matrix is CONSTRUCTED and injected at the one seam both consumers
  # read (axes_fitted_cov): the population matrix with one diagonal entry
  # inflated by 1e10, which fails the stated criterion (M89) while leaving
  # everything upstream of the two consumers -- the fit, the point estimates,
  # lavaan's own fit measures -- untouched.
  bad <- sigma
  bad[4L, 4L] <- bad[4L, 4L] * 1e10
  local_mocked_bindings(axes_fitted_cov = function(fit) bad)

  w <- testthat::capture_warnings(
    res <- suppressMessages(
      axes_reliability(cormat = sigma, items = items, angles = oct, n = 600L)
    )
  )

  # Each surface's own warning names the SHARED reason -- one from the
  # corrected-SE surface, one from the scaled-fit surface, same literal.
  expect_length(grep("ill_conditioned", w, fixed = TRUE), 2L)
  expect_true(any(grepl("standard errors could not be computed", w)))
  expect_true(any(grepl("scaled fit statistics could not be computed", w)))

  # Both failure fields carry the shared literal.
  expect_identical(res$details$se_correction_failed, "ill_conditioned")
  expect_identical(res$details$fit_scaling_failed, "ill_conditioned")

  # The corrected component SEs are all NA together...
  expect_true(all(is.na(res$components$SE)))

  # ...and so are the four statistics D-036 scales, while df and srmr are
  # unaffected: neither is a test statistic, and both must keep reporting
  # lavaan's own values beside the four NAs.
  expect_identical(res$fit$chisq, NA_real_)
  expect_identical(res$fit$pvalue, NA_real_)
  expect_identical(res$fit$rmsea, NA_real_)
  expect_identical(res$fit$cfi, NA_real_)
  expect_identical(res$fit$df, res$details$fit_uncorrected$df)
  expect_identical(res$fit$srmr, res$details$fit_uncorrected$srmr)
  expect_true(is.finite(res$fit$df))
  expect_true(is.finite(res$fit$srmr))

  # The point estimates, reliability and SEm ride on the fit itself, not on
  # the injected matrix, and stay reported.
  expect_true(all(is.finite(res$results$reliability)))
  expect_true(all(is.finite(res$components$Estimate)))
})
