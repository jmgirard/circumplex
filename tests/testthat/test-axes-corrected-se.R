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
  #
  # Read off `fiml_ratio`, NOT `corrected / naive`. RR13's published constants
  # are UNIT-DIAGONAL quantities (its reproduction appendix derives both sides
  # at the population matrix P with diag(P) = 1), so `fiml_ratio` -- both sides
  # at cov2cor(Sigma-hat) -- is the quantity they describe. After M69
  # `corrected` and `naive` are priced at different matrices, so their quotient
  # carries the N/(N-1) artifact and is not what RR13 anchored (M69 review
  # round 1, F16, found independently by two lenses).
  #
  # The tolerance tightens from 0.01 to 0.001 in the same move. At 0.01 this
  # assertion could not tell the two quantities apart at all: the mixed
  # quotient sits 0.00185 from RR13's constants and `fiml_ratio` sits 0.00029,
  # both inside a 0.01 window. 0.001 fences the mixed alternative while keeping
  # ~3x headroom on the value actually asserted.
  ratio <- got$fiml_ratio
  expect_lt(abs(ratio[["xi1"]] - 1 / 1.441), 0.001)
  expect_lt(abs(ratio[["xi2"]] - 1 / 1.067), 0.001)
  expect_lt(abs(ratio[["zeta1"]] - 1 / 0.997), 0.001)
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
  # Re-pinned at M69: both literals moved when the corrected branch was repriced
  # at cov2cor(Sigma-hat). 0.0042646 -> 0.0042719 (gap 7.3e-6 against the 2e-6
  # window), and the ratio 0.9978 -> 0.9961 (gap 1.7e-3 against 1e-3).
  expect_lt(abs(got$corrected[["zeta2"]] - 0.0042719), 2e-6)
  # The ratio pin now reads `fiml_ratio`, NOT naive/corrected. Those two are
  # priced at different matrices by design after M69, so their quotient is not a
  # meaningful quantity to pin -- it carries an N/(N-1) artifact (D-037).
  # `fiml_ratio` is the same-matrix conversion the FIML path actually applies.
  expect_lt(abs(got$fiml_ratio[["zeta2"]] - 1.0022604), 1e-4)

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


m66_cells <- function() {
  readRDS(test_path("fixtures", "m66-corrected-se-cells.rds"))
}

# calibration = mean corrected SE / empirical SD of the estimator. The whole
# point of the correction: a calibrated SE is one whose average equals the
# estimator's actual sampling variability.
m66_calib <- function(cell) {
  ok <- !is.na(cell[, "se"]) & !is.na(cell[, "xi1"])
  list(calib = mean(cell[ok, "se"]) / stats::sd(cell[ok, "xi1"]),
       r = sum(ok),
       mc_se = 1 / sqrt(2 * (sum(ok) - 1)))
}


test_that("BC3: the correction calibrates on complete data over 201 replicates", {
  fx <- m66_cells()
  expect_gte(fx$provenance$reps, 200L)
  got <- m66_calib(fx$complete)
  expect_gte(got$r, 200L)
  expect_gt(got$calib, 0.90)
  expect_lt(got$calib, 1.10)

  # The corrected SE must also equal the CLOSED-FORM value, not merely land in
  # the band: the band is a Monte-Carlo comparison and would tolerate a
  # systematically wrong SE paired with a coincidentally matching SD. The
  # analytic value at this population is 0.011639 (T1's anchor).
  ok <- !is.na(fx$complete[, "se"])
  expect_lt(abs(mean(fx$complete[ok, "se"]) - 0.011639), 5e-4)

  # ... and xi1 itself is unbiased, so the SD in the denominator is the
  # estimator's own variability and not a drifted one.
  expect_lt(abs(mean(fx$complete[ok, "xi1"]) - 0.35), 3 * 0.0121 / sqrt(got$r))
})


test_that("BC5: the correction holds past mild missingness, at 15% MCAR and M1 MAR", {
  fx <- m66_cells()

  for (nm in c("mcar15", "m1")) {
    got <- m66_calib(fx[[nm]])
    # BC5's replicate floor is stated as a precision requirement -- "enough
    # replicates that the MC SE of the SD is <= 5%" -- so it is asserted as
    # one rather than as a count, which is what the requirement actually says.
    expect_lte(got$mc_se, 0.05, label = paste0("MC SE of the SD, ", nm))
    expect_gt(got$calib, 0.85, label = paste0("calibration, ", nm))
    expect_lt(got$calib, 1.15, label = paste0("calibration, ", nm))
  }

  # Direction, recorded because it is a real limitation rather than noise: at
  # 15% cellwise MCAR the correction runs ANTI-CONSERVATIVE -- measured 0.9255,
  # i.e. the reported SE understates the estimator's true variability by ~7.5%,
  # which is 2.1 MC SEs below 1 and cannot be read as sampling noise. It is
  # inside BC5's band, which RR13 deliberately set wider ([0.85, 1.15]) for
  # exactly this regime, and it is what the roxygen's beyond-mild-missingness
  # sentence reports. Pinned so a future change that worsens it is caught
  # rather than absorbed by the band's width.
  expect_lt(m66_calib(fx$mcar15)$calib, 1.0)
  expect_gt(m66_calib(fx$mcar15)$calib, 0.88)
  # The MAR cell, by contrast, calibrates essentially exactly (1.0152).
  expect_lt(abs(m66_calib(fx$m1)$calib - 1), 0.06)
})


test_that("M66: stored cells reproduce live, so the fixture is not stale", {
  skip_if_not_installed("lavaan")
  fx <- m66_cells()
  oct <- octants()
  items_of <- function(mat) split(colnames(mat), rep(1:8, each = 3))
  draw <- function(n, seed) {
    set.seed(seed)
    as.matrix(axes_simulate(n, oct, 3L, .35, .10, .08))
  }
  live <- function(mat, ...) {
    res <- suppressMessages(suppressWarnings(
      axes_reliability(as.data.frame(mat), items = items_of(mat),
                       angles = oct, ...)
    ))
    c(xi1 = res$results$xi1[[1]],
      se = res$components$SE[res$components$Symbol == "xi1"])
  }
  # 1e-4 for the same reason M65's harness uses it: lavaan 0.7 accelerates the
  # h1 EM and stops at a slightly different point inside the same convergence
  # tolerance, measured at ~1e-6 relative. Two orders of headroom to that, and
  # far tighter than any drift in the estimator would produce.
  tol <- 1e-4

  # BOTH stored columns are re-derived, never the point estimate alone: BC3 and
  # BC5 assert entirely on the SEs, so leaving `se` unread would leave every
  # calibration claim resting on the file with nothing live behind it.
  s <- fx$provenance$seeds$complete[[1]]
  lv <- live(draw(600L, s))
  expect_equal(unname(lv[["xi1"]]), unname(fx$complete[1, "xi1"]), tolerance = tol)
  expect_equal(unname(lv[["se"]]), unname(fx$complete[1, "se"]), tolerance = tol)

  s <- fx$provenance$seeds$mcar15[[1]]
  lv <- live(axes_mcar(draw(600L, s), 0.15), missing = "fiml")
  expect_equal(unname(lv[["xi1"]]), unname(fx$mcar15[1, "xi1"]), tolerance = tol)
  expect_equal(unname(lv[["se"]]), unname(fx$mcar15[1, "se"]), tolerance = tol)

  # What this does NOT re-run, stated so coverage is never overestimated: the
  # other 200 replicates of each cell, the entire M1 MAR cell (one fit runs
  # 18-68 s), and therefore every MEAN and SD the two criteria above are
  # computed from. Those come from the stored file; what is re-derived here is
  # that the code still produces the per-replicate values in it.
  expect_identical(nrow(fx$m1), nrow(fx$complete))
})


test_that("BC6: a pipeline bootstrap independently reproduces the corrected SE", {
  # The independent oracle. The correction is analytic; this resamples
  # respondents and re-runs the WHOLE pipeline per resample -- crucially
  # re-computing the correlation matrix, which is where the in-sample
  # standardization lives. lavaan's own se = "bootstrap" would NOT
  # re-standardize (it resamples the z-columns and so reproduces the
  # covariance-metric variability the correction removes), which is why BC6
  # forbids it and why devel/m66-bootstrap-oracle.R writes this by hand.
  #
  # Stored rather than run here because the comparison is only as sharp as the
  # bootstrap's OWN Monte-Carlo noise: the SD of a bootstrap SD over B
  # resamples is ~1/sqrt(2B), about 5% at B = 200. Measured at M66 on seed
  # 1001, B = 200 gave 0.013625 against a converged 0.012967 -- noise alone
  # moved a real 9.5% gap to 15.06% and tipped it over BC6's 15% bar. The
  # fixture uses B = 1000 (noise ~2.2%; the running SD is stable from B = 400
  # on). The bar is BC6's and is not relaxed; what changed is the precision of
  # the instrument reading it.
  fx <- readRDS(test_path("fixtures", "m66-bootstrap-oracle.rds"))
  expect_gte(fx$provenance$b, 200L)
  expect_gte(nrow(fx$draws), 2L)

  for (i in seq_len(nrow(fx$draws))) {
    d <- fx$draws[i, ]
    where <- paste0("draw ", rownames(fx$draws)[i])
    expect_gte(d[["kept"]], fx$provenance$b * 0.95)
    expect_lt(abs(d[["boot"]] - d[["analytic"]]) / d[["analytic"]], 0.15,
              label = paste0("bootstrap vs corrected SE, ", where))
    # Direction, against the thing actually being ruled out: the bootstrap must
    # sit far closer to the CORRECTED SE than to the uncorrected one, which is
    # ~44% larger. Without this a bootstrap landing between them could satisfy
    # the 15% bar while agreeing with neither.
    expect_lt(abs(d[["boot"]] - d[["analytic"]]),
              abs(d[["boot"]] - d[["naive"]]),
              label = paste0("bootstrap nearer corrected than naive, ", where))
  }
})


test_that("BC6: the stored bootstrap reproduces live, so the oracle is not stale", {
  skip_if_not_installed("lavaan")
  skip_on_cran()
  fx <- readRDS(test_path("fixtures", "m66-bootstrap-oracle.rds"))
  seed <- fx$provenance$seeds[[1]]
  oct <- octants()

  set.seed(seed)
  dat <- as.matrix(axes_simulate(fx$provenance$n, oct, 3L, .35, .10, .08))
  items <- split(colnames(dat), rep(1:8, each = 3))
  res <- suppressMessages(
    axes_reliability(as.data.frame(dat), items = items, angles = oct)
  )

  # The ANALYTIC side is cheap and exact, so it is re-derived in full: if the
  # correction ever drifts, this reddens even though the expensive bootstrap
  # half stays stored. That is the half a stale fixture would otherwise hide.
  expect_equal(res$components$SE[res$components$Symbol == "xi1"],
               unname(fx$draws[1L, "analytic"]), tolerance = 1e-4)
  expect_equal(unname(res$details$se_uncorrected[["xi1"]]),
               unname(fx$draws[1L, "naive"]), tolerance = 1e-4)

  # A short live bootstrap: 60 resamples, enough to prove the resampling path
  # still runs and lands in the right neighbourhood, not enough to sharpen
  # BC6's bar -- so it is asserted loosely and says so. Its own MC noise at
  # B = 60 is ~9%, which is why 0.35 and not 0.15.
  est <- vapply(seq_len(60L), function(b) {
    set.seed(seed * 1000L + b)
    idx <- sample.int(nrow(dat), nrow(dat), replace = TRUE)
    f <- tryCatch(
      suppressWarnings(axes_fit_cormat(stats::cor(dat[idx, , drop = FALSE]),
                                       items, oct, n = nrow(dat))),
      error = function(e) NULL
    )
    if (is.null(f)) return(NA_real_)
    pe <- lavaan::parameterEstimates(f)
    pe$est[pe$op == "~~" & pe$lhs == "AX" & pe$rhs == "AX"][[1]]
  }, numeric(1))
  live_sd <- stats::sd(est, na.rm = TRUE)
  expect_lt(abs(live_sd - fx$draws[1L, "boot"]) / fx$draws[1L, "boot"], 0.35)
})


test_that("AC7: the printed caveat drops the SE warning and keeps the fit one", {
  skip_if_not_installed("lavaan")
  pp <- probe_pop()
  res <- axes_reliability(cormat = pp$sigma, items = pp$items,
                          angles = pp$angles, n = 600)
  out <- paste(capture.output(print(res)), collapse = " ")
  out <- gsub("\\s+", " ", out)

  # What must still be there: the citation.
  expect_match(out, "Cudeck, 1989", fixed = TRUE)
  # ... and the two positive claims, one per correction. M68 falsified the
  # global-fit sentence this test used to pin ("Global fit is flattered by
  # roughly 4%") -- that figure was one population's, printed as a constant --
  # so it moves to the absence block below and the scaling claim takes its
  # place. The two are pinned SEPARATELY because the two corrections can fail
  # independently and the object emits them independently -- and on different
  # surfaces as of the M68 review: the SE claim in print(), the scaling claim
  # beside the fit line summary() prints (F16).
  expect_match(out, "standard errors are adjusted to the correlation metric",
               fixed = TRUE)
  sm <- gsub("\\s+", " ", paste(capture.output(summary(res)), collapse = " "))
  expect_match(sm, "chisq, pvalue, rmsea and cfi are scaled", fixed = TRUE)
  expect_match(sm, "Satorra & Bentler, 1994", fixed = TRUE)
  expect_match(sm, "df and srmr are unchanged", fixed = TRUE)

  # What must be GONE. This is the directional half the M56/M63 lesson says the
  # stale-claim sweep keeps missing: hunting the old wording finds negative
  # assertions to delete, so each falsified phrase is pinned as an ABSENCE
  # here. Any of them coming back means the caveat is warning about an error
  # the package no longer has.
  #
  # The four below pin the OLD text verbatim, so they catch a revert. They do
  # NOT catch a REWORDING: the old caveat said "component SEs overstate" while
  # the new one says "standard errors", so the same false claim written in the
  # current text's vocabulary would slip past all four (found by the M66
  # fresh-context guard review, which the author's own mutation testing missed
  # because a mutation only ever tests the revert). The two verb-stem asserts
  # after them close that: no wording of "the SEs overstate/understate your
  # uncertainty" survives either, and neither stem appears anywhere in any
  # printed note today.
  expect_no_match(out, "component SEs overstate", fixed = TRUE)
  expect_no_match(out, "they are slightly understated", fixed = TRUE)
  expect_no_match(out, "the standard errors and global fit are approximate",
                  fixed = TRUE)
  expect_no_match(out, "How approximate depends on the instrument", fixed = TRUE)
  expect_no_match(out, "overstat", fixed = TRUE)
  expect_no_match(out, "understat", fixed = TRUE)
  # Forward-looking rather than a revert guard: "order-of-magnitude guidance"
  # was never in the PRINTED caveat (it lived only in the roxygen), so this
  # cannot fail on any reversion of this string. Kept deliberately, and labelled
  # so, to catch that Rd vocabulary migrating into print.
  expect_no_match(out, "order-of-magnitude guidance", fixed = TRUE)
  # M68's own falsified claims, pinned as absences beside the positives above.
  # "roughly 4%" is the load-bearing one: it is a population-specific figure
  # that read as a property of the method, and a revert would restore it while
  # every other assertion here stayed green.
  expect_no_match(out, "flattered by roughly 4%", fixed = TRUE)
  expect_no_match(out, "global fit statistics are approximate", fixed = TRUE)
})


test_that("AC7: the Rd states the correction and no longer claims otherwise", {
  # man/ in the dev tree, Rd_db() once installed -- the dual-source pattern
  # test-rd-latex-safe.R and test-axes-reliability.R already use. A man/-only
  # guard cannot even OPEN the file under R CMD check (installed packages carry
  # help/, not man/), which is how this test errored the whole check on its
  # first run; a Rd_db()-only guard errors under load_all(). The M7 lesson is
  # that a guard reachable on only one of those paths runs in neither gate that
  # ships.
  rd_file <- test_path("..", "..", "man", "axes_reliability.Rd")
  rd <- if (file.exists(rd_file)) {
    paste(readLines(rd_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  } else {
    db <- tools::Rd_db("circumplex")
    paste(as.character(db[["axes_reliability.Rd"]]), collapse = "")
  }
  # Fail loudly rather than pass vacuously if neither source yielded anything.
  expect_gt(nchar(rd), 1000L)
  rd <- gsub("\\s+", " ", rd)

  # The FIML sentence the correction falsified (R/axes_reliability.R, the
  # "# Missing data" section). It said the FIML SEs "remain approximate for the
  # same correlation-as-covariance reason as the default path"; both halves of
  # that are now false, since the default path is corrected and so is this one.
  expect_false(grepl("remain approximate for the same", rd, fixed = TRUE))
  # Every absence assert above needs a POSITIVE partner, or the rule can be
  # reverted-by-deletion with the guard green. Without the next assert, deleting
  # the sentence that says the FIML path IS corrected leaves the section silent
  # on the question, satisfies the absence assert by deletion, and keeps the
  # following sentence (which the residual assert pins) intact -- an Rd that
  # claims nothing, with the suite green. Found by the M66 fresh-context guard
  # review; it was the one true unpaired negative in either guard.
  expect_match(rd, "carry the same correlation-metric correction as every other path",
               fixed = TRUE)
  # The residual the correction genuinely does NOT reach. Pinned with its
  # PREDICATE, not just the noun phrase: "uncertainty in the standardization
  # constants" alone stays green if the sentence is inverted to say the
  # correction now removes it.
  expect_match(rd,
               "What the correction does not reach is the uncertainty in the standardization constants",
               fixed = TRUE)

  # The @details block: corrected SEs, and the chi-square explicitly NOT
  # corrected, with RR13 B-1's figures so a reader can check the direction.
  expect_match(rd, "are calibrated uncertainty, not order-of-magnitude",
               fixed = TRUE)
  # M68 falsified both of these, and they are now pinned as ABSENCES with
  # paired positives. The figure matters more than the sentence: 261.1 was
  # measured at one reference population and printed as though it described the
  # method, which is the specific defect D-036 overturned. A revert would bring
  # the number back verbatim.
  expect_false(grepl("261.1 against 273 degrees of freedom", rd, fixed = TRUE))
  expect_false(grepl("is \\strong{not} corrected", rd, fixed = TRUE))
  # The paired positives, so the absences cannot be satisfied by deleting the
  # paragraph and leaving the page silent on the question.
  expect_match(rd, "reported as Satorra-Bentler-type \\strong{scaled} values",
               fixed = TRUE)
  expect_match(rd, "\\code{df} and \\code{srmr} are \\strong{unchanged}",
               fixed = TRUE)
  expect_match(rd, "\\code{details$fit_uncorrected}", fixed = TRUE)
  # The honesty clause: the scaled statistic matches its reference in mean and
  # is not exact. Deleting it would leave the page overclaiming, which no
  # absence assertion above would notice.
  expect_match(rd, "matches its reference chi-square in \\strong{mean}",
               fixed = TRUE)
  # `fixed = TRUE` takes the string literally, so it must NOT carry regex
  # escapes -- "details\\$se_uncorrected" would look for a literal backslash
  # and fail against a page that says exactly the right thing (the M57 lesson,
  # caught here by the assertion failing rather than by review).
  expect_match(rd, "\\code{details$se_uncorrected}", fixed = TRUE)
  # The old unconditional claim must not survive anywhere in the page.
  expect_false(grepl("Treat the component SEs as", rd, fixed = TRUE))
})


test_that("M66 review F3: print() WIRING for the correction-failure note", {
  skip_if_not_installed("lavaan")
  # Title says WIRING deliberately (the M62 lesson): this constructs the
  # failure state on the object rather than provoking it, so it proves the
  # print branch is connected and says nothing about the CONDITION selecting
  # it. That distinction is load-bearing here, because the state is currently
  # UNREACHABLE end-to-end -- axes_reliability()'s positive-definiteness gate
  # refuses a singular correlation matrix before any fit happens ("The item
  # correlation matrix is not positive definite; the model cannot be fit"),
  # which is why no integration test exists and why this one must not pretend
  # to be one.
  pp <- probe_pop()
  res <- axes_reliability(cormat = pp$sigma, items = pp$items,
                          angles = pp$angles, n = 600)
  res$details$se_correction_failed <- "singular"
  res$components$SE <- NA_real_
  out <- gsub("\\s+", " ", paste(capture.output(print(res)), collapse = " "))

  # The failure is stated, with its reason.
  expect_match(out, "component standard errors could not be computed (singular)",
               fixed = TRUE)
  expect_match(out, "point estimates, reliability, and SEm are unaffected",
               fixed = TRUE)
  # The calibrated-SE claim is SUPPRESSED -- printing it beside an all-NA SE
  # column would assert a property of numbers that are not there.
  expect_no_match(out, "are corrected for this and are calibrated", fixed = TRUE)
  expect_no_match(out, "typically smaller than the values printed by",
                  fixed = TRUE)
  # ... but nothing true is lost with it. This object has a FAILED SE
  # correction and a SUCCESSFUL scaling -- the two are independent as of M68 --
  # so the surviving half must still print, and must not have been dragged down
  # with the suppressed one.
  expect_match(out, "Cudeck, 1989", fixed = TRUE)
  expect_no_match(out, "flattered by roughly 4%", fixed = TRUE)
  # The scaled-fit sentence lives beside the fit line summary() prints, not in
  # print()'s block (M68 review, F16), so it is summary() that must carry it.
  sm <- gsub("\\s+", " ", paste(capture.output(summary(res)), collapse = " "))
  expect_match(sm, "chisq, pvalue, rmsea and cfi are scaled", fixed = TRUE)
  expect_no_match(out, "chisq, pvalue, rmsea and cfi are scaled", fixed = TRUE)
  # M68 review F1: the opening's own summary clause is an assertion too. With
  # the SE correction failed it must NOT claim both sides were corrected.
  expect_no_match(out, "both sides of that mismatch", fixed = TRUE)
  expect_match(out, "the global-fit side of that mismatch is corrected",
               fixed = TRUE)

  # The mirror image: a failed SCALING with working SEs. Injected the same way
  # and for the same reason -- axes_reliability()'s positive-definiteness gate
  # makes the state unreachable end-to-end, so this proves the branch is wired,
  # not that anything selects it.
  res2 <- axes_reliability(cormat = pp$sigma, items = pp$items,
                           angles = pp$angles, n = 600)
  res2$details$fit_scaling_failed <- "unidentified"
  res2$fit[c("chisq", "pvalue", "rmsea", "cfi")] <- NA_real_
  out2 <- gsub("\\s+", " ", paste(capture.output(summary(res2)), collapse = " "))
  expect_match(out2, "could not be scaled to the correlation metric (unidentified)",
               fixed = TRUE)
  expect_match(out2, "details$fit_uncorrected", fixed = TRUE)
  # The scaling claim is suppressed -- it would assert a property of four NAs.
  expect_no_match(out2, "chisq, pvalue, rmsea and cfi are scaled", fixed = TRUE)
  # ... while the SE claim, which is still true here, survives.
  expect_match(out2, "standard errors are adjusted to the correlation metric",
               fixed = TRUE)
  # F1 in the mirror direction: only the SE side may be claimed here.
  expect_match(out2, "the standard-error side of that mismatch is corrected",
               fixed = TRUE)
  expect_no_match(out2, "both sides of that mismatch", fixed = TRUE)

  # And the normal path is unchanged by either branch.
  ok <- gsub("\\s+", " ", paste(capture.output(summary(
    axes_reliability(cormat = pp$sigma, items = pp$items,
                     angles = pp$angles, n = 600))), collapse = " "))
  expect_match(ok, "standard errors are adjusted to the correlation metric",
               fixed = TRUE)
  expect_match(ok, "chisq, pvalue, rmsea and cfi are scaled", fixed = TRUE)
  # Both corrections live: the opening says so, and only here.
  expect_match(ok, "both sides of that mismatch are corrected", fixed = TRUE)
  expect_no_match(ok, "could not be computed", fixed = TRUE)
  expect_no_match(ok, "could not be scaled", fixed = TRUE)
})


test_that("AC7: the vignette's caveats match the corrected contract", {
  # The teaching vignette is the FOURTH surface carrying the SE claim, and the
  # one M66's own doc sweep missed: AC7 enumerated the printed caveat, two
  # roxygen passages and NEWS, so nothing pointed at vignettes/. Caught by the
  # review gate's prior-review lens, which recognised it as the M56/M62/M63
  # stale-prose family recurring on this very file. CLAUDE.md holds vignettes
  # to statistically precise prose, and until this was fixed the vignette told
  # readers the SEs were order-of-magnitude guidance while print(), the Rd and
  # NEWS all said they were calibrated.
  #
  # Guarded at SOURCE rather than in the rendered article: the .Rmd is what an
  # author edits, it is present in the dev tree and in the built package's
  # vignette sources, and a rendered-HTML guard would need the site built.
  vig <- test_path("..", "..", "vignettes", "axes-reliability.Rmd")
  skip_if_not(file.exists(vig), "vignette source not available")
  txt <- gsub("\\s+", " ", paste(readLines(vig, warn = FALSE), collapse = " "))
  expect_gt(nchar(txt), 1000L)

  # The falsified claims, pinned as absences.
  expect_no_match(txt, "order-of-magnitude", fixed = TRUE)
  expect_no_match(txt, "The standard errors and global fit are approximate",
                  fixed = TRUE)
  expect_no_match(txt, "approximate for the same correlation-as-covariance",
                  fixed = TRUE)
  # Paired positives, so the section cannot satisfy the absences by DELETION --
  # the trap the guard review caught in the Rd guard for exactly this reason.
  expect_match(txt, "The component standard errors are **corrected** for it",
               fixed = TRUE)
  expect_match(txt, "carry the same correlation-metric correction as every other",
               fixed = TRUE)
  # M68 falsified the "not corrected" clause; absence plus paired positives.
  expect_no_match(txt, "fit indices are not corrected", fixed = TRUE)
  expect_no_match(txt, "flattered by roughly 4%", fixed = TRUE)
  expect_match(txt, "**global fit statistics are corrected too**", fixed = TRUE)
  expect_match(txt, "`details$fit_uncorrected`", fixed = TRUE)
  # The section must keep telling the reader what the correction does NOT buy.
  # Without this, the vignette could be rewritten into an overclaim and every
  # assertion above would stay green.
  expect_match(txt, "it is a calibration, not an exactness guarantee",
               fixed = TRUE)
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
  # NA, never NaN, and never a fallback to the uncorrected value. The literal
  # moved at M89: an exactly singular matrix is now refused by the stated
  # degeneracy criterion (its smallest eigenvalue is 0), before the solve()
  # that used to fail on it emergently as "singular".
  expect_false(any(is.nan(got$corrected)))
  expect_identical(got$reason, "ill_conditioned")
})


# ---- M69 / AC2: the vech-space oracle at cov2cor(Sigma-hat) ------------------
#
# The shipped corrected branch folds the covariance-to-correlation Jacobian into
# a p x p sandwich. This rebuilds the same asymptotic variance the STUPID way --
# literal p* x p* duplication matrix, V, Gamma_S and the standardization
# Jacobian J -- so the two routes share no arithmetic.
#
# Priced at cov2cor(Sigma-hat) per RR15 Q1: the fold's compression Sigma_ij =
# rho_ij holds only at a unit diagonal, and lavaan's sample.cov.rescale leaves
# the fitted diagonal at (N-1)/N. Appended at the END of this file deliberately
# -- BC3 anchors the lavaan fence at lines 67-69 and 191-194, so nothing may be
# inserted above them (M69 deviation D4).

m69_dup <- function(p) {
  pstar <- p * (p + 1) / 2
  D <- matrix(0, p * p, pstar)
  k <- 0L
  for (j in seq_len(p)) for (i in j:p) {
    k <- k + 1L
    D[(j - 1) * p + i, k] <- 1
    D[(i - 1) * p + j, k] <- 1
  }
  D
}

m69_vech <- function(M) M[lower.tri(M, diag = TRUE)]

# Component SEs from literal vech-space matrices. `sigma` must ALREADY be a
# correlation matrix in the item map's own order. Returns the normal-theory
# (`naive`) and correlation-metric (`corrected`) SEs at that matrix.
m69_vech_se <- function(sigma, mats, n_comp, n) {
  p <- nrow(sigma)
  pstar <- p * (p + 1) / 2
  D <- m69_dup(p)
  Dp <- solve(t(D) %*% D) %*% t(D)
  si <- solve(sigma)

  # V, the normal-theory ML weight in vech coordinates, and Gamma_S, the acov of
  # vech(S). They are inverses; the oracle asserts that rather than assuming it.
  V <- 0.5 * t(D) %*% kronecker(si, si) %*% D
  Gs <- 2 * Dp %*% kronecker(sigma, sigma) %*% t(Dp)
  testthat::expect_lt(max(abs(V %*% Gs - diag(pstar))), 1e-9)

  # Gamma_R = J Gamma_S J', J the row-by-row Jacobian of the
  # covariance-to-correlation map at a unit diagonal:
  #   dr_ij = ds_ij - 0.5 * rho_ij * (ds_ii + ds_jj)  (i != j),  dr_ii = 0.
  idx <- which(lower.tri(matrix(0, p, p), diag = TRUE), arr.ind = TRUE)
  J <- matrix(0, pstar, pstar)
  for (a in seq_len(pstar)) {
    i <- idx[a, 1]
    j <- idx[a, 2]
    if (i == j) next
    J[a, a] <- 1
    ai <- which(idx[, 1] == i & idx[, 2] == i)
    aj <- which(idx[, 1] == j & idx[, 2] == j)
    J[a, ai] <- J[a, ai] - 0.5 * sigma[i, j]
    J[a, aj] <- J[a, aj] - 0.5 * sigma[i, j]
  }
  Gr <- J %*% Gs %*% t(J)
  # Independent check on Gamma_R: under normality the asymptotic variance of
  # sqrt(n) r_ij is the Pearson-Filon (1 - rho^2)^2, which J never sees.
  for (a in seq_len(pstar)) {
    i <- idx[a, 1]
    j <- idx[a, 2]
    want <- if (i == j) 0 else (1 - sigma[i, j]^2)^2
    testthat::expect_lt(abs(Gr[a, a] - want), 1e-9)
  }

  Delta <- vapply(mats, m69_vech, numeric(pstar))
  bread <- solve(t(Delta) %*% V %*% Delta)
  meat <- t(Delta) %*% V %*% Gr %*% V %*% Delta
  # Oracle self-check: priced with Gamma_S instead of Gamma_R the sandwich
  # collapses to the bread exactly, because V and Gamma_S are inverses. A slip
  # in D, V or Delta breaks this before it reaches any comparison.
  meat_s <- t(Delta) %*% V %*% Gs %*% V %*% Delta
  testthat::expect_lt(max(abs(bread %*% meat_s %*% bread - bread)), 1e-9)

  r <- seq_len(n_comp)
  list(
    naive = sqrt(diag(bread)[r] / n),
    corrected = sqrt(diag(bread %*% meat %*% bread)[r] / n)
  )
}


test_that("AC2: the corrected branch matches the vech oracle at cov2cor, octant map", {
  skip_if_not_installed("lavaan")
  pp <- probe_pop()
  fit <- axes_fit_cormat(pp$sigma, pp$items, pp$angles, n = 600)
  sigma_hat <- lavaan::fitted(fit)$cov[pp$names, pp$names]

  d <- axes_se_derivs(pp$item_angle, pp$scale, NULL, TRUE, FALSE)
  want <- m69_vech_se(stats::cov2cor(sigma_hat), d$mats, d$n_comp, 600)

  got <- axes_corrected_se(
    lavaan::fitted(fit)$cov, pp$names, pp$item_angle, pp$scale,
    n = 600, fit_zeta1 = TRUE, fit_zeta2 = FALSE
  )

  # 1e-6 relative, derived in AC2 from the discrimination required: the
  # superseded raw-Sigma-hat pricing differs by 1.05e-3 on its closest
  # component, so this fences that alternative by 1000x.
  expect_lt(max(abs(unname(got$corrected) / want$corrected - 1)), 1e-6)
})


test_that("AC2: the corrected branch matches the vech oracle at cov2cor, blockwise map", {
  skip_if_not_installed("lavaan")
  oct <- octants()
  # The crossed layout, as the BC1 K-matrix test above uses: a contiguous one
  # biases xi1 (the M63 lesson).
  blk <- axes_crossed_blocks(8L, 3L)
  pop <- axes_population_cor(oct, 3L, xi1 = .35, xi2 = .10, zeta1 = .08,
                             zeta2 = .05, item_block = blk)
  nm <- sprintf("item_%02d", seq_len(nrow(pop$sigma)))
  dimnames(pop$sigma) <- list(nm, nm)
  items <- unname(split(nm, pop$scale))
  item_angle <- rep(as.numeric(oct), each = 3L)

  fit <- axes_fit_cormat(pop$sigma, items, oct, n = 600, item_block = blk)
  sigma_hat <- lavaan::fitted(fit)$cov[nm, nm]

  d <- axes_se_derivs(item_angle, pop$scale, blk, TRUE, TRUE)
  want <- m69_vech_se(stats::cov2cor(sigma_hat), d$mats, d$n_comp, 600)

  got <- axes_corrected_se(
    lavaan::fitted(fit)$cov, nm, item_angle, pop$scale, item_block = blk,
    n = 600, fit_zeta1 = TRUE, fit_zeta2 = TRUE
  )

  expect_lt(max(abs(unname(got$corrected) / want$corrected - 1)), 1e-6)
})


# ---- M69 / AC7 (BC2): invariance of the correlation-metric quantities --------
#
# `corrected` and `fiml_ratio` are priced at cov2cor(Sigma-hat), which is an
# exact retraction onto unit-diagonal matrices, so both are invariant to
# Sigma-hat -> D Sigma-hat D for ANY positive diagonal D. `naive` is priced at
# the raw matrix and is homogeneous of degree 1, so it scales.
#
# Diagonal invariance rather than mere scalar invariance is the point (RR15 Q5):
# a scalar-only pin stays GREEN under a "divide by the mean diagonal" or "divide
# by (N-1)/N" pseudo-fix, and the fitted diagonal is not constant under
# misspecification (0.943-1.072 measured on a FIML fit, RR15 B3). Diagonal
# invariance is the property only cov2cor delivers.
#
# Deviation D1: the rescaled matrix carries its dimnames re-attached.
# `D %*% S %*% D` drops them, and axes_corrected_se() refuses a dimnames-free
# matrix by design (pinned at :104), so the literal BC2 recipe would error.

test_that("AC7: corrected and fiml_ratio are invariant to diagonal rescaling", {
  skip_if_not_installed("lavaan")
  pp <- probe_pop()
  fit <- axes_fit_cormat(pp$sigma, pp$items, pp$angles, n = 600)
  sigma_hat <- lavaan::fitted(fit)$cov

  se_at <- function(m) axes_corrected_se(
    m, pp$names, pp$item_angle, pp$scale,
    n = 600, fit_zeta1 = TRUE, fit_zeta2 = FALSE
  )
  base <- se_at(sigma_hat)

  set.seed(69L)
  dv <- exp(stats::runif(nrow(sigma_hat), -0.3, 0.3))
  rescale <- function(m, d) {
    out <- diag(d) %*% m %*% diag(d)
    dimnames(out) <- dimnames(m)          # D1: the bare product drops these
    out
  }
  diag_rs <- se_at(rescale(sigma_hat, dv))
  scalar_rs <- se_at(rescale(sigma_hat, rep(sqrt(2), nrow(sigma_hat))))

  # 1e-6, derived in BC2: the superseded raw/mixed pricing violates these
  # identities by O(1) factors (1.538-2.114 at scalar 2), six orders above;
  # measured floating-point drift of the cov2cor path is 4.4e-16. Never
  # bit-identity.
  expect_lt(max(abs(diag_rs$corrected / base$corrected - 1)), 1e-6)
  expect_lt(max(abs(diag_rs$fiml_ratio / base$fiml_ratio - 1)), 1e-6)
  expect_lt(max(abs(scalar_rs$corrected / base$corrected - 1)), 1e-6)
  expect_lt(max(abs(scalar_rs$fiml_ratio / base$fiml_ratio - 1)), 1e-6)

  # The companion that stops the above from being the trivial consequence of
  # normalizing EVERYTHING: naive is still priced at the raw matrix, so it
  # scales by exactly the scalar. If a later edit normalized naive too, these
  # invariances would still pass while the lavaan fence at :67-69 broke -- this
  # says which matrix naive is on, from inside the same test.
  # D = sqrt(2)*I, so the rescaled matrix is 2*Sigma-hat and naive scales by
  # exactly 2 -- homogeneous of degree 1, matching RR15's measured 2.000000.
  expect_lt(max(abs(scalar_rs$naive / base$naive - 2)), 1e-6)
  expect_gt(max(abs(diag_rs$naive / base$naive - 1)), 1e-3)
})


test_that("AC7: the same invariance holds on the zeta2 (blockwise) probe", {
  skip_if_not_installed("lavaan")
  # Deviation D1 reads "the probe fits" as the octant probe PLUS the zeta2
  # probe. The test above covers only the first, leaving the block-specificity
  # component -- whose literals this milestone re-pinned -- unchecked for
  # invariance (M69 review round 1, F13).
  oct <- octants()
  blk <- axes_crossed_blocks(8L, 3L)
  pop <- axes_population_cor(oct, 3L, xi1 = .35, xi2 = .10, zeta1 = .08,
                             zeta2 = .05, item_block = blk)
  nm <- sprintf("item_%02d", seq_len(nrow(pop$sigma)))
  dimnames(pop$sigma) <- list(nm, nm)
  ia <- rep(as.numeric(oct), each = 3L)
  fit <- axes_fit_cormat(pop$sigma, unname(split(nm, pop$scale)), oct,
                         n = 600, item_block = blk)
  sigma_hat <- lavaan::fitted(fit)$cov

  se_at <- function(m) axes_corrected_se(
    m, nm, ia, pop$scale, item_block = blk,
    n = 600, fit_zeta1 = TRUE, fit_zeta2 = TRUE
  )
  base <- se_at(sigma_hat)

  set.seed(70L)
  dv <- exp(stats::runif(nrow(sigma_hat), -0.3, 0.3))
  rescale <- function(m, d) {
    out <- diag(d) %*% m %*% diag(d)
    dimnames(out) <- dimnames(m)
    out
  }
  diag_rs <- se_at(rescale(sigma_hat, dv))
  scalar_rs <- se_at(rescale(sigma_hat, rep(sqrt(2), nrow(sigma_hat))))

  expect_true("zeta2" %in% names(base$corrected))
  expect_lt(max(abs(diag_rs$corrected / base$corrected - 1)), 1e-6)
  expect_lt(max(abs(diag_rs$fiml_ratio / base$fiml_ratio - 1)), 1e-6)
  expect_lt(max(abs(scalar_rs$corrected / base$corrected - 1)), 1e-6)
  expect_lt(max(abs(scalar_rs$fiml_ratio / base$fiml_ratio - 1)), 1e-6)
  expect_lt(max(abs(scalar_rs$naive / base$naive - 2)), 1e-6)
  # The anti-triviality companion, carried over from the octant test rather
  # than left to it: without this, an edit that normalized `naive` too would
  # leave every invariance above green on this probe while breaking the lavaan
  # fence (M69 review round 2, A7).
  expect_gt(max(abs(diag_rs$naive / base$naive - 1)), 1e-3)
})


test_that("AC7: the reported FIML SE is se_uncorrected times fiml_ratio", {
  skip_if_not_installed("lavaan")
  # Deviation D1's wiring assertion. After D2 this is the PRIMARY evidence that
  # the repricing reaches the FIML surface at all: the committed band fixture
  # stores the uncorrected SE and no Sigma-hat, so it cannot respond to the
  # change.
  #
  # The fit's own Sigma-hat is not exposed in `details`, and rebuilding the fit
  # test-side would construct both sides of the comparison from the same code
  # and catch nothing common-mode (the M65 (j) trap). So the real helper is
  # CAPTURED rather than replaced: the mock calls through and records what the
  # estimator actually received. That keeps this a wiring assertion and nothing
  # more, which is exactly what it claims to be (the M62 lesson: a seam mock
  # proves the branch wiring, never the condition selecting it).
  captured <- NULL
  real <- axes_corrected_se
  testthat::local_mocked_bindings(
    axes_corrected_se = function(...) {
      out <- real(...)
      captured <<- out
      out
    },
    .package = "circumplex"
  )

  fx <- readRDS(test_path("fixtures", "m65-heavy-cells.rds"))
  oct <- octants()
  set.seed(fx$provenance$seeds$mcar[[1L]])
  mat <- axes_mcar(as.matrix(axes_simulate(600L, oct, 3L, .35, .10, .08)), 0.05)
  res <- suppressMessages(suppressWarnings(
    axes_reliability(as.data.frame(mat),
                     items = split(colnames(mat), rep(1:8, each = 3)),
                     angles = oct, missing = "fiml")
  ))

  expect_false(is.null(captured))
  expect_true("fiml_ratio" %in% names(captured))

  sym <- res$components$Symbol
  unc <- res$details$se_uncorrected
  for (s in names(unc)) {
    expect_equal(
      res$components$SE[sym == s],
      unname(unc[[s]] * captured$fiml_ratio[[s]]),
      tolerance = 1e-10,
      label = paste0("reported FIML SE == se_uncorrected * fiml_ratio, ", s)
    )
  }

  # ... and the ratio applied is NOT the mixed-matrix quotient. The two differ
  # by N/(N-1) = 1.00167 at n = 600, so this discriminates decisively and is
  # what would redden if line 1691 ever went back to corrected/naive (D-037).
  mixed <- captured$corrected / captured$naive
  expect_gt(max(abs(mixed / captured$fiml_ratio - 1)), 1e-4)
})


# ---- M69 / AC10 (BC5): the failure contract, extended to fiml_ratio ----------

test_that("AC10: a nonpositive diagonal is refused before cov2cor() runs", {
  pp <- probe_pop()
  bad <- pp$sigma
  bad[3L, 3L] <- 0                        # a zero variance; cov2cor gives NaN

  expect_warning(
    got <- axes_corrected_se(bad, pp$names, pp$item_angle, pp$scale,
                             n = 600, fit_zeta1 = TRUE, fit_zeta2 = FALSE),
    "could not be computed"
  )
  # "singular", not the pre-M89 "nonpositive_diagonal": M89 unified the two
  # fitted-matrix surfaces' reason vocabulary on the literal the sibling
  # guard in axes_scaling_factor() already printed for this input.
  expect_identical(got$reason, "singular")

  # All THREE vectors NA together -- the contract fiml_ratio joined at M69.
  expect_true(all(is.na(got$naive)))
  expect_true(all(is.na(got$corrected)))
  expect_true(all(is.na(got$fiml_ratio)))
  # NA, never NaN. Without the guard, cov2cor() of a zero diagonal produces NaN
  # rows and the failure would surface as "indefinite" or as raw NaN rather
  # than as this honest refusal (RR15 B2, the M62 doctrine).
  expect_false(any(is.nan(got$fiml_ratio)))
  expect_false(any(is.nan(got$corrected)))

  # A non-finite diagonal must NOT take this door, and must not error. The
  # predicate is NA-safe precisely so this input keeps its literal, "singular"
  # -- reached before M69 through solve() -> tryCatch, and since M89 through
  # the degeneracy criterion's finiteness arm, ahead of any pricing. Written
  # as a regression test because M69 shipped the erroring version to review
  # (round 1, F1).
  na_diag <- pp$sigma
  na_diag[1L, 1L] <- NA_real_
  expect_warning(
    gna <- axes_corrected_se(na_diag, pp$names, pp$item_angle, pp$scale,
                             n = 600, fit_zeta1 = TRUE, fit_zeta2 = FALSE),
    "could not be computed"
  )
  expect_identical(gna$reason, "singular")
  expect_true(all(is.na(gna$naive)))
  expect_true(all(is.na(gna$corrected)))
  expect_true(all(is.na(gna$fiml_ratio)))

  nan_diag <- pp$sigma
  nan_diag[2L, 2L] <- NaN
  expect_warning(
    gnan <- axes_corrected_se(nan_diag, pp$names, pp$item_angle, pp$scale,
                              n = 600, fit_zeta1 = TRUE, fit_zeta2 = FALSE),
    "could not be computed"
  )
  expect_identical(gnan$reason, "singular")

  # A negative diagonal takes the same door.
  bad2 <- pp$sigma
  bad2[5L, 5L] <- -0.2
  expect_warning(
    got2 <- axes_corrected_se(bad2, pp$names, pp$item_angle, pp$scale,
                              n = 600, fit_zeta1 = TRUE, fit_zeta2 = FALSE),
    "could not be computed"
  )
  expect_identical(got2$reason, "singular")
})


test_that("AC10: every non-success return NAs all three vectors together", {
  # Runtime half -- always runs, including under R CMD check. This is the
  # load-bearing assertion; the source enumeration below is a completeness aid.
  pp <- probe_pop()
  sing <- pp$sigma
  sing[2L, ] <- sing[1L, ]
  sing[, 2L] <- sing[, 1L]
  got <- suppressWarnings(
    axes_corrected_se(sing, pp$names, pp$item_angle, pp$scale,
                      n = 600, fit_zeta1 = TRUE, fit_zeta2 = FALSE)
  )
  # The pre-M69 singular path must NA fiml_ratio too, not just its two elders.
  expect_true(all(is.na(got$fiml_ratio)))
  expect_named(got, c("naive", "corrected", "fiml_ratio", "reason"))
})


test_that("AC10: the na_out() calls are the only non-success returns (BC5 enumeration)", {
  # BC5's enumeration procedure, run mechanically rather than by eye, so
  # "three na_out calls" cannot quietly become four unnoticed.
  #
  # SKIPPED under R CMD check: an installed package carries no R/ sources, and
  # reading them there errors rather than skipping -- the M7 trap, hit twice in
  # this milestone (the sibling guard in test-axes-scaled-fit.R first, then
  # this one, because fixing the first did not sweep for the second). Said
  # plainly instead of left silent: this half runs in development only. The
  # runtime half of the contract is asserted in the test above and always runs.
  src_path <- test_path("..", "..", "R", "axes_corrected_se.R")
  skip_if_not(file.exists(src_path), "package R/ sources absent (installed)")
  src <- readLines(src_path)

  reasons <- regmatches(src, regexpr('return\\("[a-z_]+"\\)', src))
  reasons <- sub('return\\("', "", sub('"\\)', "", reasons))
  # axes_se_pricing()'s three, plus the two axes_sigma_degenerate() returns
  # (M89): its finiteness arm reuses "singular", and the stated criterion
  # itself is "ill_conditioned".
  expect_setequal(
    reasons,
    c("singular", "unidentified", "indefinite", "ill_conditioned")
  )
  # Plus the guards' own reasons, which route through na_out() directly: the
  # nonpositive-diagonal door (relabeled "nonpositive_diagonal" -> "singular"
  # at M89) and the +Inf door this surface adopted at M89.
  expect_true(any(grepl('na_out("singular")', src, fixed = TRUE)))
  expect_true(any(grepl('na_out("infinite_diagonal")', src, fixed = TRUE)))
  # D5's error exit, which is NOT part of the NA-together contract.
  expect_true(any(grepl("must carry dimnames", src, fixed = TRUE)))
})


# ---- M89 T10: "unidentified" fired as a RETURNED reason, not just enumerated ----

test_that("the corrected SEs refuse as 'unidentified' when the model's derivatives are degenerate", {
  # Until M89 no test asserted this literal as a returned reason -- only that
  # the string occurs in the source (the BC5 enumeration test above). The
  # degeneracy criterion M89 added refuses a degenerate SIGMA-hat before any
  # pricing runs, so a probe that reaches this door must be degenerate in
  # DELTA instead: the information matrix Delta'V Delta is singular while
  # Sigma-hat itself is perfectly well conditioned.
  #
  # The construction is the model's own: `xi2` is an all-ones matrix and
  # `zeta1` is the same-scale indicator, so a map whose items all sit on ONE
  # scale makes the two derivative matrices IDENTICAL when zeta1 is fitted.
  # Two identical columns of Delta cannot be told apart by any amount of data,
  # which is exactly what "unidentified" names.
  pp <- probe_pop()
  p <- nrow(pp$sigma)
  one_scale <- rep("A", p)

  d <- axes_se_derivs(pp$item_angle, one_scale, NULL, TRUE, FALSE)
  expect_identical(d$mats[[2L]], d$mats[[3L]])

  # Sigma-hat is NOT what fails here: the stated criterion accepts it, so the
  # refusal below cannot be the criterion firing under another name.
  expect_null(axes_sigma_degenerate(pp$sigma))

  expect_warning(
    got <- axes_corrected_se(pp$sigma, pp$names, pp$item_angle, one_scale,
                             n = 600, fit_zeta1 = TRUE, fit_zeta2 = FALSE),
    "could not be computed"
  )
  # WHICH failure, never bare failure.
  expect_identical(got$reason, "unidentified")
  expect_true(all(is.na(got$naive)))
  expect_true(all(is.na(got$corrected)))
  expect_true(all(is.na(got$fiml_ratio)))
  expect_false(any(is.nan(got$corrected)))

  # The passing control, and it passes for the claim's reason: the ONLY change
  # is dropping zeta1, which removes the duplicate derivative matrix. Same
  # sigma, same map, same n -- so the refusal above is the degenerate Delta and
  # nothing else about this input.
  ctl <- axes_corrected_se(pp$sigma, pp$names, pp$item_angle, one_scale,
                           n = 600, fit_zeta1 = FALSE, fit_zeta2 = FALSE)
  expect_null(ctl$reason)
  expect_true(all(is.finite(ctl$corrected)))
})
