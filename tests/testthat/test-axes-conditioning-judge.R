# M147: the per-fit certificate is the sole conditioning judge (D-061, D-062;
# RR22 rec 9, RR24).
#
# Until M147 axes_pricing_core() inverted the information matrix with R's
# default solve() tolerance, which refused "unidentified" wherever LAPACK's
# reciprocal condition estimate fell below eps -- a threshold RR22 measured
# straddling real platforms at counterexample B. The certified path now
# inverts with tol = 0 and "unidentified" fires on exactly three EXACT grounds
# (a pair of bit-identical component matrices, a component matrix identical to
# the identity, a non-finite or exact-singular inversion); a condition estimate
# below sqrt(eps) routes the fit to the certificate through
# axes_degeneracy_refusal(), whether or not the floor on sigma fired, and the
# certificate decides. The raw lavaan-tie arm keeps the default tolerance.
#
# Every test names WHICH failure, never bare failure; a platform-decided
# outcome (a zero pivot under tol = 0) is asserted on both routes.

oct <- as.numeric(octants())

# The information matrix as the core forms it, for the direct solve() whose
# error class names a route. Copied from the core's own loop deliberately: the
# core returns the inverse and the condition estimate, not the matrix.
judge_info <- function(sigma, d) {
  si <- solve(sigma)
  sim <- lapply(d$mats, function(m) si %*% m)
  q <- length(sim)
  info <- matrix(0, q, q)
  for (s in seq_len(q)) for (t in s:q) {
    info[s, t] <- info[t, s] <- 0.5 * sum(sim[[s]] * t(sim[[t]]))
  }
  info
}

# The RR13 probe population (test-axes-corrected-se.R's `probe_pop()`, which
# is file-local there): 8 octant scales, 3 items each.
judge_pop <- function() {
  pop <- axes_population_cor(octants(), 3L, xi1 = .35, xi2 = .10, zeta1 = .08)
  nm <- sprintf("item_%02d", seq_len(nrow(pop$sigma)))
  dimnames(pop$sigma) <- list(nm, nm)
  list(sigma = pop$sigma, scale = as.character(pop$scale),
       item_angle = rep(oct, each = 3L))
}

judge_df <- function(sigma, d) nrow(sigma) * (nrow(sigma) + 1) / 2 - length(d$mats)
judge_bdf <- function(sigma) nrow(sigma) * (nrow(sigma) - 1) / 2

# Both surfaces on one matrix and derivative configuration; returns the two
# reasons and the two warning vectors.
judge_both <- function(sigma, ang, scale, block, fit_zeta1, fit_zeta2) {
  d <- axes_se_derivs(ang, scale, block, fit_zeta1, fit_zeta2)
  w_se <- testthat::capture_warnings(
    se <- axes_corrected_se(sigma, rownames(sigma), ang, scale, block, n = 600,
                            fit_zeta1 = fit_zeta1, fit_zeta2 = fit_zeta2)
  )
  w_sf <- testthat::capture_warnings(
    sf <- axes_scaling_factor(sigma, rownames(sigma), ang, scale, block,
                              fit_zeta1 = fit_zeta1, fit_zeta2 = fit_zeta2,
                              df = judge_df(sigma, d), baseline_df = judge_bdf(sigma))
  )
  list(se = se, sf = sf, w_se = w_se, w_sf = w_sf, d = d)
}


# ---- AC1: the source guard ------------------------------------------------------

test_that("AC1: the core inverts under tol = 0 and returns 'unidentified' from three sites", {
  f <- axes_pricing_core
  expect_true("tol" %in% names(formals(f)))
  expect_identical(formals(f)$tol, 0)
  attr(f, "srcref") <- NULL
  txt <- deparse(f, width.cutoff = 500L)
  # One inversion of the information matrix, reading the formal.
  expect_length(grep("solve(info, tol = tol)", txt, fixed = TRUE), 1L)
  expect_length(grep("solve(info)", txt, fixed = TRUE), 0L)
  # Three return sites for the literal, and no other.
  expect_identical(sum(vapply(txt, function(l) lengths(regmatches(
    l, gregexpr('"unidentified"', l, fixed = TRUE))), 0L)), 3L)
  # The selector threshold, by name, where the decision reads it.
  g <- axes_degeneracy_refusal
  attr(g, "srcref") <- NULL
  expect_length(grep("sqrt(.Machine$double.eps)", deparse(g, width.cutoff = 500L),
                     fixed = TRUE), 1L)
})


# ---- AC1: the duplicate-pair ground, at both surfaces and three variants -------

test_that("AC1: a pair of bit-identical component matrices refuses 'unidentified' at both surfaces", {
  pp <- judge_pop()
  p <- nrow(pp$sigma)
  expect_null(axes_sigma_degenerate(pp$sigma))

  # The M89 T10 construction: one scale, zeta1 identical to the all-ones xi2.
  one <- rep("A", p)
  d1 <- axes_se_derivs(pp$item_angle, one, NULL, TRUE, FALSE)
  expect_identical(d1$mats$xi2, d1$mats$zeta1)
  expect_identical(axes_pricing_core(pp$sigma, d1), "unidentified")
  r1 <- judge_both(pp$sigma, pp$item_angle, one, NULL, TRUE, FALSE)
  expect_identical(r1$se$reason, "unidentified")
  expect_identical(r1$sf$reason, "unidentified")
  expect_length(r1$w_se, 1L)
  expect_length(r1$w_sf, 1L)
  expect_null(r1$se$naive_reason)

  # Variant: TWO duplicate pairs -- one scale AND one block, so xi2, zeta1 and
  # zeta2 are all the all-ones matrix.
  d2 <- axes_se_derivs(pp$item_angle, one, rep(1L, p), TRUE, TRUE)
  expect_identical(d2$mats$xi2, d2$mats$zeta1)
  expect_identical(d2$mats$zeta1, d2$mats$zeta2)
  r2 <- judge_both(pp$sigma, pp$item_angle, one, rep(1L, p), TRUE, TRUE)
  expect_identical(r2$se$reason, "unidentified")
  expect_identical(r2$sf$reason, "unidentified")

  # Variant: a NON-ADJACENT pair -- eight scales, one block, so zeta2 (fourth)
  # duplicates xi2 (second) while zeta1 (third) between them is distinct.
  d3 <- axes_se_derivs(pp$item_angle, pp$scale, rep(1L, p), TRUE, TRUE)
  expect_identical(d3$mats$xi2, d3$mats$zeta2)
  expect_false(identical(d3$mats$xi2, d3$mats$zeta1))
  r3 <- judge_both(pp$sigma, pp$item_angle, pp$scale, rep(1L, p), TRUE, TRUE)
  expect_identical(r3$se$reason, "unidentified")
  expect_identical(r3$sf$reason, "unidentified")

  # Variant: the pair reaching the RAW arm. That arm prices the raw matrix at
  # the default tolerance through the same core, so the structural check is
  # what it meets there too; asserted at the arm's own call shape, since a
  # duplicate design refuses the cov2cor arm first and never reaches it
  # end-to-end.
  expect_identical(axes_se_pricing(pp$sigma, d1, 600, tol = .Machine$double.eps),
                   "unidentified")

  # The passing control, for the claim's reason: dropping zeta1 removes the
  # duplicate and nothing else.
  ctl <- judge_both(pp$sigma, pp$item_angle, one, NULL, FALSE, FALSE)
  expect_null(ctl$se$reason)
  expect_null(ctl$sf$reason)
})


# ---- AC1: the identity ground ------------------------------------------------------

test_that("AC1: a component matrix identical to the identity refuses 'unidentified' at both surfaces", {
  # One item per scale with zeta1 fitted: the same-scale indicator IS the
  # identity, the sum of the p item-error derivatives. No pair is identical,
  # so the pairwise clause cannot see it; the floor admits the matrix; and on
  # this class LAPACK's condition estimate is order-eps noise (RR24), so an
  # exact design-level clause is the only platform-stable ground.
  S <- m106_family_a(0.3, 1L)
  sc <- as.character(1:8)
  expect_null(axes_sigma_degenerate(S))
  expect_false(axes_fits_zeta1(split(seq_len(8), sc)))   # the API never fits it
  d <- axes_se_derivs(oct, sc, NULL, TRUE, FALSE)
  expect_identical(d$mats$zeta1, diag(8))
  expect_false(any(duplicated(lapply(d$mats[seq_len(d$n_comp)], identity))))
  expect_identical(axes_pricing_core(S, d), "unidentified")

  r <- judge_both(S, oct, sc, NULL, TRUE, FALSE)
  expect_identical(r$se$reason, "unidentified")
  expect_identical(r$sf$reason, "unidentified")
  expect_length(r$w_se, 1L)
  expect_length(r$w_sf, 1L)
  expect_true(all(is.na(r$se$corrected)))
  expect_identical(r$sf$scale, NA_real_)

  # Control: the same design as the API fits it (no zeta1) computes.
  ctl <- judge_both(S, oct, sc, NULL, FALSE, FALSE)
  expect_null(ctl$se$reason)
  expect_null(ctl$sf$reason)

  # The OTHER slot that can equal the identity: one item per block with zeta2
  # fitted. A check reading only the zeta1 slot would pass the case above and
  # fail here.
  bl <- 1:8
  expect_false(axes_fits_zeta2(oct, sc, bl))
  dz <- axes_se_derivs(oct, sc, bl, FALSE, TRUE)
  expect_identical(dz$mats$zeta2, diag(8))
  expect_identical(axes_pricing_core(S, dz), "unidentified")
  rz <- judge_both(S, oct, sc, bl, FALSE, TRUE)
  expect_identical(rz$se$reason, "unidentified")
  expect_identical(rz$sf$reason, "unidentified")
})


# ---- AC1: the routed ground ----------------------------------------------------------

# One routed design: floor-admitted, no exact dependence among the component
# matrices, information matrix singular up to cosine rounding. `dep` is the
# exact identity the design realises in real arithmetic, as a function of the
# component matrices; it holds to rounding. Whether the stored doubles realise
# it EXACTLY is the platform's cos() to decide, not this test's: a libm that
# returns an exact value makes the design exactly singular with the code still
# correct, and the refusing route below covers that case (M147 review).
judge_routed <- function(lbl, S, ang, sc, bl, fit_zeta1, fit_zeta2, dep) {
  # The direct solve()'s message is read in English: LAPACK's exact-singular
  # message is translated under a non-English LANGUAGE (measured: German).
  testthat::local_reproducible_output(lang = "en")
  d <- axes_se_derivs(ang, sc, bl, fit_zeta1, fit_zeta2)
  gap <- max(abs(d$mats$xi1 - dep(d$mats)))
  expect_lt(gap, 1e-15, label = lbl)
  expect_false(any(duplicated(d$mats[seq_len(d$n_comp)])), label = lbl)
  expect_false(any(vapply(d$mats[seq_len(d$n_comp)], identical, TRUE,
                          diag(nrow(S)))), label = lbl)
  expect_null(axes_sigma_degenerate(S), label = lbl)

  core <- axes_pricing_core(S, d)
  info <- judge_info(S, d)
  direct <- tryCatch(solve(info, tol = 0), error = function(e) e)
  r <- judge_both(S, ang, sc, bl, fit_zeta1, fit_zeta2)
  ref <- axes_degeneracy_refusal(S, d)

  if (is.character(core)) {
    # THE REFUSING ROUTE: LAPACK hit an exact zero pivot on this platform.
    expect_identical(core, "unidentified", label = lbl)
    expect_s3_class(direct, "error")
    expect_match(conditionMessage(direct), "exactly singular")
    expect_identical(r$se$reason, "unidentified", label = lbl)
    expect_identical(r$sf$reason, "unidentified", label = lbl)
    expect_identical(ref$reason, "unidentified", label = lbl)
  } else {
    # THE PRICED ROUTE: the inversion succeeded, the estimate sits below the
    # selector threshold, so the certificate was consulted and it decides.
    expect_false(inherits(direct, "error"), label = lbl)
    expect_lt(core$rcond_info, sqrt(.Machine$double.eps), label = lbl)
    expect_false(is.null(ref$cert), label = lbl)
    expect_identical(ref$reason, "uncertified", label = lbl)
    expect_identical(r$se$reason, "uncertified", label = lbl)
    expect_identical(r$sf$reason, "uncertified", label = lbl)
    expect_length(grep("estimated relative error ", r$w_se, fixed = TRUE), 1L)
    expect_length(grep("estimated relative error ", r$w_sf, fixed = TRUE), 1L)
  }
  expect_length(r$w_se, 1L)
  expect_length(r$w_sf, 1L)
  expect_true(all(is.na(r$se$corrected)), label = lbl)
  expect_identical(r$sf$scale, NA_real_, label = lbl)
}

test_that("AC1: a floor-admitted design below the selector threshold is judged by the certificate, on either route", {
  # Two antipodal blocks over the four cardinal scales, two items per scale,
  # zeta2 fitted: C = 2B - Z in exact arithmetic, off by cosine rounding in
  # the stored doubles (cos(pi/2) is 6e-17, not 0). Measured 2026-09-21 on
  # macOS/arm64: rcond(info) 4.2e-17, tol = 0 inverts, certificate se 5.0.
  ang <- rep(c(90, 180, 270, 360), each = 2L)
  sc <- as.character(rep(1:4, each = 2L))
  bl <- c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L)
  pop <- axes_population_cor(c(90, 180, 270, 360), 2L, xi1 = .3, xi2 = .2,
                             zeta1 = .1, zeta2 = .15, item_block = bl)
  S <- pop$sigma
  nm <- paste0("i", 1:8)
  dimnames(S) <- list(nm, nm)
  expect_false(axes_fits_zeta2(ang, sc, bl))              # the API never fits it
  judge_routed("antipodal blocks", S, ang, sc, bl, TRUE, TRUE,
               function(m) 2 * m$zeta1 - m$zeta2)

  # A non-block form: three equally spaced scales, two items each, zeta1
  # fitted: C = 1.5B - 0.5J, off by cosine rounding (cos(2pi/3) is not -0.5
  # exactly). axes_reliability() refuses fewer than four scales, so this too
  # is a helper-boundary design. Measured 2026-09-21 on macOS/arm64:
  # rcond(info) 1.0e-16, tol = 0 inverts, certificate se 5.0.
  ang3 <- rep(c(90, 210, 330), each = 2L)
  sc3 <- as.character(rep(1:3, each = 2L))
  pop3 <- axes_population_cor(c(90, 210, 330), 2L, xi1 = .3, xi2 = .2, zeta1 = .15)
  S3 <- pop3$sigma
  nm3 <- paste0("i", 1:6)
  dimnames(S3) <- list(nm3, nm3)
  judge_routed("three equally spaced scales", S3, ang3, sc3, NULL, TRUE, FALSE,
               function(m) 1.5 * m$zeta1 - 0.5 * m$xi2)
})


# ---- AC1: the selector and the seam ---------------------------------------------------

test_that("AC1: the refusal decision prices the core once, hands it to both surfaces, and consults the certificate only where selected", {
  # A clean floor-admitted matrix: the core is priced inside the decision,
  # the certificate is NOT consulted, and the surfaces consume the priced core
  # rather than inverting again.
  S <- m106_family_a(0.3, 1L)
  sc <- as.character(1:8)
  d <- axes_se_derivs(oct, sc, NULL, FALSE, FALSE)
  ref <- axes_degeneracy_refusal(S, d)
  expect_null(ref$reason)
  expect_null(ref$cert)
  expect_true(is.list(ref$core))
  expect_gt(ref$core$rcond_info, sqrt(.Machine$double.eps))

  # Handed through the seam: the certified-path inversion runs exactly once
  # per fit for both surfaces; the raw arm's default-tolerance inversion is
  # its own, at eps.
  calls <- list()
  real_core <- axes_pricing_core
  local_mocked_bindings(axes_pricing_core = function(sigma, d, tol = 0) {
    calls[[length(calls) + 1L]] <<- tol
    real_core(sigma, d, tol = tol)
  })
  shared <- axes_shared_refusal(S, rownames(S), oct, sc, NULL,
                                fit_zeta1 = FALSE, fit_zeta2 = FALSE)
  se <- axes_corrected_se(S, rownames(S), oct, sc, n = 600, fit_zeta1 = FALSE,
                          fit_zeta2 = FALSE, refusal = shared)
  sf <- axes_scaling_factor(S, rownames(S), oct, sc, fit_zeta1 = FALSE,
                            fit_zeta2 = FALSE, df = judge_df(S, d),
                            baseline_df = judge_bdf(S), refusal = shared)
  expect_null(se$reason)
  expect_null(sf$reason)
  tols <- unlist(calls)
  expect_identical(sum(tols == 0), 1L)
  expect_identical(sum(tols == .Machine$double.eps), 1L)
  expect_length(tols, 2L)
})

test_that("AC1: a fit the floor sends to the certificate still computes when it passes, with the once-priced core", {
  # The certificate cases a5 and b9b: floor "ill_conditioned", certificate
  # inside the target, computing at both surfaces (as M111 established), now
  # through a core priced once in the decision.
  S <- m106_family_a(2.4e-5, 1L)
  sc <- as.character(1:8)
  expect_identical(axes_sigma_degenerate(S), "ill_conditioned")
  d <- axes_se_derivs(oct, sc, NULL, FALSE, FALSE)
  ref <- axes_degeneracy_refusal(S, d)
  expect_null(ref$reason)
  expect_false(is.null(ref$cert))
  expect_true(is.list(ref$core))
  r <- judge_both(S, oct, sc, NULL, FALSE, FALSE)
  expect_null(r$se$reason)
  expect_null(r$sf$reason)
  expect_length(r$w_se, 0L)
  expect_length(r$w_sf, 0L)
})


# ---- AC1: the raw arm keeps the default tolerance --------------------------------

test_that("AC1: the raw arm refuses at the default tolerance into naive_reason while every reported number computes", {
  # A raw matrix whose information matrix sits below eps while its cov2cor
  # image computes cleanly: a diagonal rescaling cannot do it (cov2cor undoes
  # it and the raw-metric criterion catches it first as "ill_conditioned"), so
  # the arm is exercised at its own call shape -- the raw arm calls the
  # pricing at tol = eps -- on a matrix the default tolerance refuses and
  # tol = 0 inverts (the M106 family at kappa 7.6e8, region of the M147
  # sweep). Which literal: "unidentified", from the inversion, not the
  # criterion's.
  S <- m106_family_a(3.2e-9, 1L)
  sc <- as.character(1:8)
  d <- axes_se_derivs(oct, sc, NULL, FALSE, FALSE)
  info <- judge_info(S, d)
  skip_if(rcond(info) >= .Machine$double.eps,
          "this platform's condition estimate admits the matrix at eps")
  expect_identical(axes_se_pricing(S, d, 600, tol = .Machine$double.eps),
                   "unidentified")
  zero <- axes_se_pricing(S, d, 600, tol = 0)
  expect_true(is.list(zero) || identical(zero, "unidentified"))
})


# ---- M147 review, fix-now findings (T9) ------------------------------------------

test_that("T9: the refusal object pins the derivative set it was priced for, and both surfaces check it", {
  # The refusal object carries the pricing core (M147), and the core's
  # `acov` is folded against the surface's own `d$mats` with Map(), which
  # recycles on a length mismatch. Pinning the matrix alone (M117) lets a
  # caller pair `fit_zeta2 = TRUE` at the seam with FALSE at a surface and
  # get plausible wrong numbers, so the derivative set is pinned beside it.
  S <- m106_family_a(0.3, 1L)
  sc <- as.character(1:8)
  bl <- rep(1:2, each = 4L)
  shared <- axes_shared_refusal(S, rownames(S), oct, sc, bl,
                                fit_zeta1 = FALSE, fit_zeta2 = TRUE)
  expect_null(shared$reason)
  expect_identical(shared$derivs,
                   list(components = c("xi1", "xi2", "zeta2"), n_mats = 11L))

  # The matched pair is consumed at both surfaces.
  d2 <- axes_se_derivs(oct, sc, bl, FALSE, TRUE)
  se <- axes_corrected_se(S, rownames(S), oct, sc, bl, n = 600,
                          fit_zeta1 = FALSE, fit_zeta2 = TRUE, refusal = shared)
  expect_null(se$reason)
  sf <- axes_scaling_factor(S, rownames(S), oct, sc, bl,
                            fit_zeta1 = FALSE, fit_zeta2 = TRUE,
                            df = judge_df(S, d2), baseline_df = judge_bdf(S),
                            refusal = shared)
  expect_null(sf$reason)

  # A surface asked for a different derivative set on the SAME matrix aborts,
  # and names the derivative set (not the matrix) as the mismatch.
  d0 <- axes_se_derivs(oct, sc, bl, FALSE, FALSE)
  expect_error(
    axes_corrected_se(S, rownames(S), oct, sc, bl, n = 600,
                      fit_zeta1 = FALSE, fit_zeta2 = FALSE, refusal = shared),
    "different derivative set"
  )
  expect_error(
    axes_scaling_factor(S, rownames(S), oct, sc, bl,
                        fit_zeta1 = FALSE, fit_zeta2 = FALSE,
                        df = judge_df(S, d0), baseline_df = judge_bdf(S),
                        refusal = shared),
    "different derivative set"
  )
  # The matrix pin (M117) is unchanged and still checked first.
  S2 <- S
  S2[1, 2] <- S2[2, 1] <- S2[1, 2] + 1e-3
  expect_error(
    axes_corrected_se(S2, rownames(S2), oct, sc, bl, n = 600,
                      fit_zeta1 = FALSE, fit_zeta2 = TRUE, refusal = shared),
    "different matrix"
  )
})

test_that("T9: a named item map builds the same derivative set as an unnamed one, so the exact grounds still fire", {
  # outer() copies names() onto dimnames, and identical() reads dimnames, so
  # a named `item_scale` used to defeat both exact grounds: the same-scale
  # indicator no longer matched the unnamed all-ones xi2 and never matched
  # diag(p). axes_reliability() builds the map unnamed, so the miss was
  # latent; the derivative builder now strips names.
  pp <- judge_pop()
  p <- nrow(pp$sigma)
  nm <- rownames(pp$sigma)
  ang <- pp$item_angle
  names(ang) <- nm
  one <- rep("A", p)
  names(one) <- nm
  bl <- rep(1L, p)
  names(bl) <- nm
  dn <- axes_se_derivs(ang, one, bl, TRUE, TRUE)
  du <- axes_se_derivs(pp$item_angle, rep("A", p), rep(1L, p), TRUE, TRUE)
  expect_identical(dn, du)
  expect_true(all(vapply(dn$mats, function(m) is.null(dimnames(m)), TRUE)))

  # The duplicate-pair ground at the M89 one-scale construction, named map.
  d1 <- axes_se_derivs(ang, one, NULL, TRUE, FALSE)
  expect_identical(axes_pricing_core(pp$sigma, d1), "unidentified")
  r1 <- judge_both(pp$sigma, ang, one, NULL, TRUE, FALSE)
  expect_identical(r1$se$reason, "unidentified")
  expect_identical(r1$sf$reason, "unidentified")

  # The identity ground at one item per scale, named map.
  S <- m106_family_a(0.3, 1L)
  sc <- as.character(1:8)
  names(sc) <- rownames(S)
  di <- axes_se_derivs(oct, sc, NULL, TRUE, FALSE)
  expect_identical(di$mats$zeta1, diag(8))
  expect_identical(axes_pricing_core(S, di), "unidentified")
})

test_that("T9: a 'singular' from the core is kept as the literal where the floor admitted the matrix, and falls through to the certificate only where the floor said 'ill_conditioned'", {
  # "singular" from the core means solve(sigma) itself refused at its default
  # tolerance. Master returned that literal from the pricing on a
  # floor-admitted matrix; M111 gave the SAME literal "uncertified" only
  # where the floor had fired. The fall-through keeps M111's literal on the
  # "ill_conditioned" branch and master's on the admitted one. No real
  # matrix reaches the admitted branch (rcond(sigma) is decades above eps
  # wherever the floor admits), so the core is mocked.
  local_mocked_bindings(axes_pricing_core = function(sigma, d, tol = 0) "singular")
  sc <- as.character(1:8)
  d <- axes_se_derivs(oct, sc, NULL, FALSE, FALSE)

  adm <- m106_family_a(0.3, 1L)
  expect_null(axes_sigma_degenerate(adm))
  ra <- axes_degeneracy_refusal(adm, d)
  expect_identical(ra$reason, "singular")
  expect_null(ra$cert)
  expect_null(ra$core)

  ill <- m106_family_a(2.4e-5, 1L)
  expect_identical(axes_sigma_degenerate(ill), "ill_conditioned")
  ri <- axes_degeneracy_refusal(ill, d)
  expect_false(identical(ri$reason, "singular"))
  expect_false(is.null(ri$cert))
  expect_null(ri$core)
})
