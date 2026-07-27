# M66 axes_reliability(): the corrected asymptotic covariance (RR13 BC1-BC6).
#
# The model is fit to an item CORRELATION matrix as if it were a covariance
# matrix, so lavaan's normal-theory SEs price vech(S) variability while the
# estimator consumes vech(R). RR13 derived both sides in closed form -- Sigma is
# linear in the components, so the delta method is exact here -- and these tests
# pin the derivation against the values that review published.

# The probe population RR13's anchors are stated at: 8 octant scales, 3 items
# each, xi1 = .35, xi2 = .10, zeta1 = .08, n = 600.
probe_pop <- function() {
  oct <- octants()
  pop <- axes_population_cor(oct, 3L, xi1 = .35, xi2 = .10, zeta1 = .08)
  nm <- sprintf("item_%02d", seq_len(nrow(pop$sigma)))
  dimnames(pop$sigma) <- list(nm, nm)
  list(
    sigma = pop$sigma, names = nm, scale = pop$scale, angles = oct,
    items = unname(split(nm, pop$scale)),
    item_angle = rep(as.numeric(oct), each = 3L)
  )
}


test_that("BC2: the corrected covariance reproduces RR13's deterministic anchors", {
  skip_if_not_installed("lavaan")
  pp <- probe_pop()
  fit <- axes_fit_cormat(pp$sigma, pp$items, pp$angles, n = 600)
  sigma_hat <- lavaan::fitted(fit)$cov

  got <- axes_corrected_se(
    sigma_hat, pp$names, pp$item_angle, pp$scale,
    n = 600, fit_zeta1 = TRUE, fit_zeta2 = FALSE
  )

  # The two anchor values, absolute tolerances per BC2. Stated absolutely
  # (expect_lt on the absolute gap), never via expect_equal's RELATIVE
  # tolerance -- the M59/M61 lesson.
  expect_lt(abs(got$naive[["xi1"]] - 0.01677), 2e-4)
  expect_lt(abs(got$corrected[["xi1"]] - 0.01164), 2e-4)

  # corrected/uncorrected per component, BC2's (1/1.441, 1/1.067, 1/0.997).
  ratio <- got$corrected / got$naive
  expect_lt(abs(ratio[["xi1"]] - 1 / 1.441), 0.01)
  expect_lt(abs(ratio[["xi2"]] - 1 / 1.067), 0.01)
  expect_lt(abs(ratio[["zeta1"]] - 1 / 0.997), 0.01)
})


test_that("BC2: the naive branch reproduces lavaan's own information matrix", {
  skip_if_not_installed("lavaan")
  pp <- probe_pop()
  fit <- axes_fit_cormat(pp$sigma, pp$items, pp$angles, n = 600)
  pe <- lavaan::parameterEstimates(fit)
  lav_se <- function(lat) pe$se[pe$op == "~~" & pe$lhs == lat & pe$rhs == lat][[1]]

  got <- axes_corrected_se(
    lavaan::fitted(fit)$cov, pp$names, pp$item_angle, pp$scale,
    n = 600, fit_zeta1 = TRUE, fit_zeta2 = FALSE
  )

  # This is the load-bearing check on the DERIVATIVE STRUCTURE: the naive
  # quantity 2*tr(W Sigma W Sigma) is an independent route to the ML
  # information-matrix value lavaan reports, so agreement to ~1e-7 says the
  # {C, J, B, E_ii} set and the (Delta'V Delta)^-1 row are right. A wrong
  # derivative matrix cannot pass this by luck. The corrected branch differs
  # from the naive one ONLY in W_c, so pinning naive pins most of the code.
  expect_lt(abs(got$naive[["xi1"]] - lav_se("AX")), 1e-7)
  expect_lt(abs(got$naive[["xi2"]] - lav_se("GEN")), 1e-7)
  expect_lt(abs(got$naive[["zeta1"]] - lav_se("SS1")), 1e-7)
})


test_that("BC1: Sigma-hat is realigned to the item map, not consumed as given", {
  skip_if_not_installed("lavaan")
  pp <- probe_pop()
  fit <- axes_fit_cormat(pp$sigma, pp$items, pp$angles, n = 600)
  sigma_hat <- lavaan::fitted(fit)$cov

  # lavaan orders the model's variables by first appearance in the syntax, and
  # axes_syntax() drops zero-weight loading terms from the AX line, so the
  # fitted matrix does NOT come back in item-map order. Feeding it positionally
  # pairs every item with another item's angle and scale: measured at the M66
  # plan gate, that returns SE(xi1) = 0.0046 where 0.01677 is right -- a 3.6x
  # error, no error condition, a number that looks perfectly plausible.
  expect_false(identical(rownames(sigma_hat), pp$names))

  aligned <- axes_corrected_se(
    sigma_hat[pp$names, pp$names], pp$names, pp$item_angle, pp$scale,
    n = 600, fit_zeta1 = TRUE, fit_zeta2 = FALSE
  )
  as_given <- axes_corrected_se(
    sigma_hat, pp$names, pp$item_angle, pp$scale,
    n = 600, fit_zeta1 = TRUE, fit_zeta2 = FALSE
  )
  # Equal only because the helper realigns internally. Delete the realignment
  # and this reddens: `as_given` becomes the 0.0046 answer.
  expect_equal(as_given, aligned)
  expect_lt(abs(as_given$naive[["xi1"]] - 0.01677), 2e-4)

  # A matrix with no dimnames cannot be realigned, so it is refused rather
  # than silently consumed in whatever order it arrived.
  bare <- unname(sigma_hat)
  expect_error(
    axes_corrected_se(bare, pp$names, pp$item_angle, pp$scale,
                      n = 600, fit_zeta1 = TRUE, fit_zeta2 = FALSE),
    "dimnames"
  )
})


test_that("BC1: components$SE is the corrected value, details keeps the naive one", {
  skip_if_not_installed("lavaan")
  pp <- probe_pop()
  res <- axes_reliability(cormat = pp$sigma, items = pp$items,
                          angles = pp$angles, n = 600)

  fit <- axes_fit_cormat(pp$sigma, pp$items, pp$angles, n = 600)
  want <- axes_corrected_se(
    lavaan::fitted(fit)$cov, pp$names, pp$item_angle, pp$scale,
    n = 600, fit_zeta1 = TRUE, fit_zeta2 = FALSE
  )
  sym <- res$components$Symbol
  got <- res$components$SE

  # The reported SE is the CORRECTED one, component by component. Pinned
  # against the helper's own output rather than a literal, because the literal
  # is already pinned in the BC2 anchor test above -- here the claim is that
  # the estimator reports what the helper computed.
  #
  # `tol` is set from the DISCRIMINATION required, not from what one run prints
  # (the M59/M64 lesson). The alternative hypothesis is "the estimator reported
  # the NAIVE value instead", and the two are furthest apart at xi1 (44%) and
  # CLOSEST at zeta1, where they differ by 0.31% -- so 1e-6 keeps ~3000x
  # headroom to the signal while sitting ~1e4 above the ~1e-10 relative jitter
  # between two independent lavaan optimizations of the same problem, which is
  # what this test's refit incurs.
  tol <- 1e-6
  for (s in c("xi1", "xi2", "zeta1")) {
    expect_equal(got[sym == s], unname(want$corrected[[s]]), tolerance = tol)
  }
  # ... and it is NOT lavaan's uncorrected number. xi1's two values differ by
  # 44% at this population, so this discriminates decisively; asserting it
  # avoids a green run where the correction was computed and then dropped.
  expect_gt(abs(got[sym == "xi1"] - want$naive[["xi1"]]), 1e-3)

  # details retains what lavaan reported, so the correction stays auditable.
  expect_named(res$details$se_uncorrected, c("xi2", "xi1", "zeta1"),
               ignore.order = TRUE)
  pe <- lavaan::parameterEstimates(fit)
  lav <- function(lat) pe$se[pe$op == "~~" & pe$lhs == lat & pe$rhs == lat][[1]]
  expect_equal(res$details$se_uncorrected[["xi1"]], lav("AX"), tolerance = tol)
  expect_equal(res$details$se_uncorrected[["xi2"]], lav("GEN"), tolerance = tol)
  expect_equal(res$details$se_uncorrected[["zeta1"]], lav("SS1"), tolerance = tol)

  # The item row is a mean of free residuals and keeps its NA SE (M54).
  expect_true(is.na(got[sym == "epsilon"]))
})


test_that("BC1: the block-specificity component is corrected too (the K matrix)", {
  skip_if_not_installed("lavaan")
  # RR13's reproduction appendix omits K entirely -- it derives {C, J, B, E_ii}
  # only -- so zeta2 is the one piece of BC1 with no published worked value
  # behind it, and it needs its own anchor rather than inheriting the octant
  # one (M66 plan gate). The crossed layout is the identifying one: item j of
  # every scale goes to block j, so same-block and same-scale share no
  # off-diagonal pair.
  oct <- octants()
  blk <- axes_crossed_blocks(8L, 3L)
  pop <- axes_population_cor(oct, 3L, xi1 = .35, xi2 = .10, zeta1 = .08,
                             zeta2 = .05, item_block = blk)
  nm <- sprintf("item_%02d", seq_len(nrow(pop$sigma)))
  dimnames(pop$sigma) <- list(nm, nm)
  items <- unname(split(nm, pop$scale))
  ia <- rep(as.numeric(oct), each = 3L)

  fit <- axes_fit_cormat(pop$sigma, items, oct, n = 600, item_block = blk)
  pe <- lavaan::parameterEstimates(fit)
  lav <- function(l) pe$se[pe$op == "~~" & pe$lhs == l & pe$rhs == l][[1]]
  got <- axes_corrected_se(lavaan::fitted(fit)$cov, nm, ia, pop$scale,
                           item_block = blk, n = 600,
                           fit_zeta1 = TRUE, fit_zeta2 = TRUE)

  expect_named(got$corrected, c("xi1", "xi2", "zeta1", "zeta2"))

  # The INDEPENDENT fence on K: with the block component in the model, every
  # naive SE must still reproduce lavaan's own information-matrix value. lavaan
  # derives it by its own route, so this is not the implementation checked
  # against itself -- a wrong K misprices the whole information matrix and
  # every one of these four moves.
  expect_lt(abs(got$naive[["zeta2"]] - lav("BS1")), 1e-7)
  expect_lt(abs(got$naive[["xi1"]] - lav("AX")), 1e-7)
  expect_lt(abs(got$naive[["xi2"]] - lav("GEN")), 1e-7)
  expect_lt(abs(got$naive[["zeta1"]] - lav("SS1")), 1e-7)

  # The correction's own zeta2 value. This literal comes from THIS
  # implementation, so on its own it would only compare the code to itself
  # (the M65 (j) trap). It earns its place in combination: K is fenced above
  # against lavaan, and the W_c transform is fenced against RR13's published
  # 0.01164 in the BC2 anchor test, so what this pins is that the two compose
  # for the block component as they do for the other three. Recorded as a
  # regression pin, not as an oracle.
  expect_lt(abs(got$corrected[["zeta2"]] - 0.0042646), 2e-6)
  expect_lt(abs(got$naive[["zeta2"]] / got$corrected[["zeta2"]] - 0.9978), 1e-3)

  # zeta2 reaches the reported table through the estimator, not just the helper.
  res <- axes_reliability(cormat = pop$sigma, items = items, angles = oct,
                          n = 600, blocks = split(nm, blk))
  expect_true(res$details$zeta2_fitted)
  se_z2 <- res$components$SE[res$components$Symbol == "zeta2"]
  expect_equal(se_z2, unname(got$corrected[["zeta2"]]), tolerance = 1e-6)
})


# The population metric ratio at the probe population, derived in RR13's
# reproduction appendix (sqrt(naive/actual) = 1.441229). Used ONLY as the proxy
# in the fixture arm below, never by the shipped code, which evaluates the ratio
# at each fit's own Sigma-hat.
M66_POP_RATIO <- 1.4412


test_that("BC4: the corrected FIML SE calibrates over the committed fixture", {
  fx <- readRDS(test_path("fixtures", "m65-heavy-cells.rds"))
  expect_gte(fx$provenance$reps, 200L)

  # The fixture stores fiml.se (uncorrected) and fiml.xi1 per replicate, and no
  # Sigma-hat -- the per-replicate ratio is not reconstructible from it. So this
  # arm divides by the POPULATION constant, which is exactly how RR13 produced
  # the 1.001/1.008/1.018 it cites. The live arm below is what establishes that
  # the constant is a sound stand-in for what the code actually does.
  for (rate in names(fx$mcar)) {
    cell <- fx$mcar[[rate]]
    calib <- mean(cell[, "fiml.se"] / M66_POP_RATIO, na.rm = TRUE) /
      stats::sd(cell[, "fiml.xi1"], na.rm = TRUE)
    expect_gt(calib, 0.90)
    expect_lt(calib, 1.10)
  }

  # RR13 B-4: the listwise-under-deletion columns are a free N-invariance
  # asset, carrying the same check across an order of magnitude of effective N
  # (complete cases run ~370 at 2% MCAR down to ~48 at 10%) at no simulation
  # cost. na.rm is load-bearing rather than defensive: at 10% MCAR one of the
  # 200 replicates leaves 32 complete cases for 24 items and does not fit.
  for (rate in names(fx$mcar)) {
    cell <- fx$mcar[[rate]]
    ok <- !is.na(cell[, "lw_se"]) & !is.na(cell[, "lw_xi1"])
    expect_gte(sum(ok), 195L)
    calib <- mean(cell[ok, "lw_se"] / M66_POP_RATIO) /
      stats::sd(cell[ok, "lw_xi1"])
    expect_gt(calib, 0.90)
    expect_lt(calib, 1.10)
  }
})


test_that("BC4: the shipped composition evaluates the ratio at Sigma-hat", {
  skip_if_not_installed("lavaan")
  fx <- readRDS(test_path("fixtures", "m65-heavy-cells.rds"))
  oct <- octants()
  seeds <- fx$provenance$seeds$mcar

  live <- function(rate, r) {
    set.seed(seeds[[r]])
    mat <- axes_mcar(as.matrix(axes_simulate(600L, oct, 3L, .35, .10, .08)), rate)
    res <- suppressMessages(suppressWarnings(
      axes_reliability(as.data.frame(mat),
                       items = split(colnames(mat), rep(1:8, each = 3)),
                       angles = oct, missing = "fiml")
    ))
    naive <- res$details$se_uncorrected[["xi1"]]
    c(naive = naive,
      per_sigma = res$components$SE[res$components$Symbol == "xi1"],
      ratio = naive / res$components$SE[res$components$Symbol == "xi1"])
  }

  got <- vapply(c(0.02, 0.05, 0.10),
                function(rate) live(rate, 1L), numeric(3))

  # BC4 requires the ratio be evaluated AT Sigma-hat, not taken as a constant.
  # A per-fit ratio varies across fits; a hardcoded constant would not. The
  # three cells differ in missingness rate, so their Sigma-hats differ and so
  # must their ratios -- this reddens if the code is ever changed to divide by
  # a literal.
  expect_false(isTRUE(all.equal(got["ratio", 1], got["ratio", 3])))

  # And the per-Sigma-hat ratio does NOT equal the population constant. It runs
  # systematically ABOVE it -- measured 1.4499/1.4501/1.4507 against 1.4412
  # over 20 replicates per rate at M66 T4, consistent in sign and size across
  # all three rates, so it is a finite-sample offset and not scatter. The
  # fixture arm above therefore uses a proxy that is ~2% CONSERVATIVE in the
  # corrected SE: the shipped composition reports a slightly SMALLER SE and so
  # calibrates slightly nearer 1 than the numbers that test asserts. The offset
  # is well inside the ~3.6% MC SE of an empirical SD over 200 replicates, so
  # it cannot change AC4's verdict at this replicate count -- which is what
  # makes the constant a sound stand-in, NOT that the two agree exactly.
  expect_gt(mean(got["ratio", ]), M66_POP_RATIO)
  expect_lt(mean(got["ratio", ]) / M66_POP_RATIO - 1, 0.05)
})


test_that("BC1: a non-invertible Sigma-hat gives NA SEs with a reason, never a number", {
  pp <- probe_pop()
  # A singular matrix: duplicate one item's row/column exactly.
  sing <- pp$sigma
  sing[2L, ] <- sing[1L, ]
  sing[, 2L] <- sing[, 1L]

  expect_warning(
    got <- axes_corrected_se(sing, pp$names, pp$item_angle, pp$scale,
                             n = 600, fit_zeta1 = TRUE, fit_zeta2 = FALSE),
    "could not be computed"
  )
  expect_true(all(is.na(got$corrected)))
  expect_true(all(is.na(got$naive)))
  # NA, never NaN, and never a fallback to the uncorrected value.
  expect_false(any(is.nan(got$corrected)))
  expect_identical(got$reason, "singular")
})
