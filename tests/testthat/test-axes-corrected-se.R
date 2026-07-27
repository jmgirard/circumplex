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
