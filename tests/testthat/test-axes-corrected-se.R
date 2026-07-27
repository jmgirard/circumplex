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
  skip_if_not_installed("lavaan")
  skip_on_cran()
  # The independent oracle. The delta-method correction is analytic; this
  # resamples respondents and re-runs the WHOLE pipeline per resample --
  # crucially including re-computing the correlation matrix, which is where the
  # in-sample standardization lives. lavaan's own se = "bootstrap" would NOT
  # re-standardize: it resamples the z-columns and so reproduces the
  # covariance-metric variability the correction exists to remove, which is why
  # RR13 BC6 forbids it and why this is written by hand.
  oct <- octants()
  items_of <- function(mat) split(colnames(mat), rep(1:8, each = 3))
  n <- 600L
  n_boot <- 200L

  boot_se <- function(seed) {
    set.seed(seed)
    dat <- as.matrix(axes_simulate(n, oct, 3L, .35, .10, .08))
    items <- items_of(dat)
    fit0 <- axes_reliability(as.data.frame(dat), items = items, angles = oct)
    analytic <- fit0$components$SE[fit0$components$Symbol == "xi1"]

    est <- vapply(seq_len(n_boot), function(b) {
      idx <- sample.int(n, n, replace = TRUE)
      r <- stats::cor(dat[idx, , drop = FALSE])
      f <- tryCatch(
        suppressWarnings(axes_fit_cormat(r, items, oct, n = n)),
        error = function(e) NULL
      )
      if (is.null(f)) return(NA_real_)
      pe <- lavaan::parameterEstimates(f)
      pe$est[pe$op == "~~" & pe$lhs == "AX" & pe$rhs == "AX"][[1]]
    }, numeric(1))

    c(analytic = analytic, boot = stats::sd(est, na.rm = TRUE),
      kept = sum(!is.na(est)))
  }

  for (seed in c(1001L, 1002L)) {
    got <- boot_se(seed)
    expect_gte(got[["kept"]], 190)
    rel <- abs(got[["boot"]] - got[["analytic"]]) / got[["analytic"]]
    expect_lt(rel, 0.15,
              label = paste0("bootstrap vs corrected SE, seed ", seed))
    # Direction check against the thing being ruled out: the bootstrap must sit
    # far closer to the CORRECTED SE than to the uncorrected one, which is 44%
    # larger. Without this, a bootstrap that happened to land between them
    # could satisfy the 15% bar while agreeing with neither.
    naive <- got[["analytic"]] * 1.4412
    expect_lt(abs(got[["boot"]] - got[["analytic"]]),
              abs(got[["boot"]] - naive))
  }
})


test_that("AC7: the printed caveat drops the SE warning and keeps the fit one", {
  skip_if_not_installed("lavaan")
  pp <- probe_pop()
  res <- axes_reliability(cormat = pp$sigma, items = pp$items,
                          angles = pp$angles, n = 600)
  out <- paste(capture.output(print(res)), collapse = " ")
  out <- gsub("\\s+", " ", out)

  # What must still be there: the citation and the global-fit sentence, which
  # the correction does not touch.
  expect_match(out, "Cudeck, 1989", fixed = TRUE)
  expect_match(out, "Global fit is flattered by roughly 4%", fixed = TRUE)
  # ... and the new positive claim.
  expect_match(out, "standard errors are corrected for this and are calibrated",
               fixed = TRUE)

  # What must be GONE. This is the directional half the M56/M63 lesson says the
  # stale-claim sweep keeps missing: hunting the old wording finds negative
  # assertions to delete, so each falsified phrase is pinned as an ABSENCE
  # here. Any of them coming back means the caveat is warning about an error
  # the package no longer has.
  expect_no_match(out, "component SEs overstate", fixed = TRUE)
  expect_no_match(out, "order-of-magnitude guidance", fixed = TRUE)
  expect_no_match(out, "they are slightly understated", fixed = TRUE)
  expect_no_match(out, "the standard errors and global fit are approximate",
                  fixed = TRUE)
})


test_that("AC7: the Rd states the correction and no longer claims otherwise", {
  rd <- paste(readLines(test_path("..", "..", "man", "axes_reliability.Rd")),
              collapse = " ")
  rd <- gsub("\\s+", " ", rd)
  skip_if(!nzchar(rd))

  # The FIML sentence the correction falsified (R/axes_reliability.R, the
  # "# Missing data" section). It said the FIML SEs "remain approximate for the
  # same correlation-as-covariance reason as the default path"; both halves of
  # that are now false, since the default path is corrected and so is this one.
  expect_false(grepl("remain approximate for the same", rd, fixed = TRUE))
  # The residual the correction genuinely does NOT reach is stated instead.
  expect_match(rd, "uncertainty in the standardization constants", fixed = TRUE)

  # The @details block: corrected SEs, and the chi-square explicitly NOT
  # corrected, with RR13 B-1's figures so a reader can check the direction.
  expect_match(rd, "are calibrated uncertainty, not order-of-magnitude",
               fixed = TRUE)
  expect_match(rd, "261.1 against 273 degrees of freedom", fixed = TRUE)
  # `fixed = TRUE` takes the string literally, so it must NOT carry regex
  # escapes -- "details\\$se_uncorrected" would look for a literal backslash
  # and fail against a page that says exactly the right thing (the M57 lesson,
  # caught here by the assertion failing rather than by review).
  expect_match(rd, "\\code{details$se_uncorrected}", fixed = TRUE)
  # The old unconditional claim must not survive anywhere in the page.
  expect_false(grepl("Treat the component SEs as", rd, fixed = TRUE))
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
