# Tests for the CPM (circular process model / Browne 1992) engine core.
#
# ORACLE RULE (m4-browne-design.md sec. 6.1): no expected numerical value comes
# from memory or from devel/g2xx1.txt. Every expected value below is derived
# in-test by closed form, construction, or an independent base-R computation.
#
# Seeding: the RNG-silence requirement applies to the ENGINE, not to the tests.
# Tests seed their own RNG with set.seed() and restore .Random.seed on exit so
# that seeding here cannot mask an engine that touches the stream.

# ---- helpers used only by the tests -----------------------------------------

# Build a random positive-definite correlation matrix (independent of engine).
rand_cor <- function(p) {
  A <- matrix(stats::rnorm(p * (p + 2)), nrow = p)
  S <- tcrossprod(A)
  d <- 1 / sqrt(diag(S))
  R <- d * S * rep(d, each = p)
  (R + t(R)) / 2
}

# Build a matrix of angular separations delta_ij = theta_i - theta_j (radians).
delta_mat <- function(theta) outer(theta, theta, `-`)

# Independent (re-derived) rho and P for cross-checking constructions.
ref_rho <- function(delta, beta) {
  k <- seq_along(beta) - 1L
  vapply(delta, function(d) sum(beta * cos(k * d)), numeric(1))
}
ref_P <- function(theta, zeta, beta) {
  p <- length(theta)
  D <- delta_mat(theta)
  C <- matrix(ref_rho(as.vector(D), beta), nrow = p)
  Dz <- diag(zeta)
  P <- Dz %*% C %*% Dz + (diag(p) - Dz^2)
  diag(P) <- 1
  P
}

# Independent ML discrepancy.
ref_F <- function(R, P) {
  as.numeric(determinant(P, logarithm = TRUE)$modulus) -
    as.numeric(determinant(R, logarithm = TRUE)$modulus) +
    sum(diag(solve(P, R))) - nrow(R)
}

# ---- 0. functions exist (test-first sanity) ---------------------------------

test_that("engine building blocks exist", {
  expect_true(is.function(cpm_rho))
  expect_true(is.function(cpm_rho_deriv))
  expect_true(is.function(cpm_implied_cor))
  expect_true(is.function(cpm_discrepancy))
  expect_true(is.function(cpm_gradient))
  expect_true(is.function(cpm_engine))
})

# ---- 1. rho / rho' and implied matrix (construction oracle) -----------------

test_that("cpm_rho and cpm_rho_deriv match the closed form", {
  beta <- c(0.4, 0.3, 0.2, 0.1)
  k <- seq_along(beta) - 1L
  d <- c(0, 0.5, 1.2, pi, 2 * pi, -0.7)
  expect_equal(cpm_rho(d, beta), vapply(d, function(x) sum(beta * cos(k * x)), numeric(1)))
  expect_equal(cpm_rho(0, beta), 1) # rho(0) = sum beta = 1 (correlation function)
  expect_equal(
    cpm_rho_deriv(d, beta),
    vapply(d, function(x) -sum(k * beta * sin(k * x)), numeric(1))
  )
})

test_that("cpm_implied_cor equals P = Dz C Dz + (I - Dz^2) with unit diagonal", {
  set.seed(1)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  theta <- c(0, 1, 2, 3, 4)
  zeta <- c(0.9, 0.8, 0.7, 0.6, 0.5)
  beta <- c(0.5, 0.3, 0.2)
  P <- cpm_implied_cor(theta, zeta, beta)
  expect_equal(diag(P), rep(1, 5))
  expect_equal(P, ref_P(theta, zeta, beta))
  expect_true(isSymmetric(P))
})

# ---- 2. ML discrepancy: definition and scale invariance ---------------------

test_that("cpm_discrepancy matches ln|P| - ln|R| + tr(R P^-1) - p and is >= 0", {
  set.seed(2)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  R <- rand_cor(6)
  theta <- c(0, 1, 2, 3, 4, 5)
  zeta <- rep(0.7, 6)
  beta <- c(0.5, 0.3, 0.2)
  P <- cpm_implied_cor(theta, zeta, beta)
  expect_equal(cpm_discrepancy(R, P), ref_F(R, P))
  # F(R, R) == 0 and F >= 0.
  expect_equal(cpm_discrepancy(R, R), 0, tolerance = 1e-10)
  expect_gte(cpm_discrepancy(R, P), -1e-10)
})

test_that("F is invariant to a common positive diagonal rescaling (scale invariance)", {
  # sec. 3.2: embedding both R and P with the same diagonal D leaves F unchanged;
  # ln-det and trace terms cancel algebraically. Exact to machine precision.
  set.seed(3)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  R <- rand_cor(6)
  P <- cpm_implied_cor(c(0, 1, 2, 3, 4, 5), rep(0.7, 6), c(0.5, 0.3, 0.2))
  D <- diag(c(0.3, 1.7, 2.0, 0.5, 4.0, 1.1))
  Rs <- D %*% R %*% D
  Ps <- D %*% P %*% D
  expect_equal(cpm_discrepancy(Rs, Ps), cpm_discrepancy(R, P), tolerance = 1e-12)
})

# ---- 3. parameterization forward/inverse round trip -------------------------

test_that("pack/unpack are inverse maps (logit zeta, softmax beta, free angles)", {
  set.seed(4)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  spec <- cpm_spec(p = 6, m = 2, variant = "A", reference = 1)
  spec$theta_ref_val <- 0        # reference angle fixed at 0
  theta <- c(0, 0.7, 1.4, 2.1, 2.8, 3.5)
  zeta <- c(0.9, 0.8, 0.7, 0.6, 0.85, 0.75)
  # feasible beta from a softmax of random v (v0 = 0); m = 2 => length m + 1 = 3
  v <- c(0, stats::rnorm(2))
  beta <- exp(v) / sum(exp(v))
  gstar <- cpm_pack(theta, zeta, beta, spec)
  nat <- cpm_unpack(gstar, spec)
  # angles compared modulo 2pi (reference held fixed)
  expect_equal(cpm_rho(delta_mat(nat$theta), beta), cpm_rho(delta_mat(theta), beta))
  expect_equal(nat$zeta, zeta, tolerance = 1e-10)
  expect_equal(nat$beta, beta, tolerance = 1e-10)
  expect_equal(sum(nat$beta), 1, tolerance = 1e-12) # sum constraint holds identically
})

# ---- 4. analytic gradient vs central finite differences (sec. 6.4) --------------

test_that("analytic gradient matches central finite differences at random points", {
  set.seed(20260706)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  h <- 1e-6
  n_pts <- 30
  spec <- cpm_spec(p = 6, m = 2, variant = "A", reference = 1)
  spec$theta_ref_val <- 0        # reference angle fixed at 0 for these points
  q <- spec$q
  for (t in seq_len(n_pts)) {
    R <- rand_cor(6)
    theta <- c(0, sort(stats::runif(5, 0, 2 * pi)))
    zeta <- stats::runif(6, 0.05, 0.995)
    v <- c(0, stats::rnorm(2))     # m = 2 => length m + 1 = 3 betas
    beta <- exp(v) / sum(exp(v))
    gstar <- cpm_pack(theta, zeta, beta, spec)
    ga <- cpm_gradient(gstar, R, spec)
    gfd <- numeric(q)
    for (i in seq_len(q)) {
      gp <- gstar; gm <- gstar
      gp[i] <- gp[i] + h; gm[i] <- gm[i] - h
      fp <- cpm_objective(gp, R, spec)
      fm <- cpm_objective(gm, R, spec)
      gfd[i] <- (fp - fm) / (2 * h)
    }
    # Mixed absolute/relative criterion per component (sec. 6.4).
    expect_true(
      all(abs(ga - gfd) <= 1e-7 * pmax(1, abs(gfd))),
      info = paste0("gradient point ", t, ": max err ",
                    max(abs(ga - gfd) / pmax(1, abs(gfd))))
    )
  }
})

# ---- 5. exact-recovery round trips (sec. 6.4) -----------------------------------

recovers <- function(theta0, zeta0, beta0, m, variant = "A", reference = 1) {
  P0 <- cpm_implied_cor(theta0, zeta0, beta0)
  fit <- cpm_engine(P0, angles = theta0 * 180 / pi, m = m,
                    variant = variant, reference = reference)
  list(fit = fit, P0 = P0)
}

test_that("exact recovery: generic feasible gamma0", {
  theta0 <- c(0, 0.8, 1.7, 2.6, 3.5, 4.4, 5.3, 6.0)
  zeta0 <- c(0.85, 0.8, 0.75, 0.7, 0.82, 0.78, 0.73, 0.68)
  beta0 <- c(0.5, 0.3, 0.15, 0.05)
  r <- recovers(theta0, zeta0, beta0, m = 3)
  expect_lte(r$fit$F, 1e-10)
  expect_lt(max(abs(circumplex:::angle_dist(as_radian(r$fit$theta_rad),
                                            as_radian(theta0)))), 1e-5)
  expect_equal(r$fit$zeta, zeta0, tolerance = 1e-5)
  expect_equal(r$fit$beta, beta0, tolerance = 1e-5)
})

test_that("exact recovery: an angle exactly at the 0/360 pole", {
  # theta[2] is the reference-relative pole; test one item at exactly 0.
  theta0 <- c(0, 0, 0.9, 1.8, 2.7, 3.6, 4.5, 5.4)
  zeta0 <- rep(0.75, 8)
  beta0 <- c(0.5, 0.3, 0.2)
  r <- recovers(theta0, zeta0, beta0, m = 3)
  expect_lte(r$fit$F, 1e-9)
  # reported angle for the pole item is ~0 or ~360 (DESIGN G2)
  a2 <- r$fit$theta[2]
  expect_true(min(abs(a2 - 0), abs(a2 - 360)) < 1e-3)
})

test_that("exact recovery: near-equal angles", {
  theta0 <- c(0, 0.02, 0.9, 1.8, 2.7, 3.6, 4.5, 5.4)
  zeta0 <- rep(0.7, 8)
  beta0 <- c(0.5, 0.3, 0.2)
  r <- recovers(theta0, zeta0, beta0, m = 3)
  expect_lte(r$fit$F, 1e-8)
})

test_that("exact recovery: small beta tail", {
  theta0 <- c(0, 0.8, 1.6, 2.4, 3.2, 4.0, 4.8, 5.6)
  zeta0 <- rep(0.8, 8)
  beta0 <- c(0.6, 0.35, 0.049, 0.001)
  r <- recovers(theta0, zeta0, beta0, m = 3)
  expect_lte(r$fit$F, 1e-8)
})

# ---- 6. circulant DFT check (variant D, sec. 6.4) -------------------------------

test_that("variant D circulant: fitted beta matches truncated nonneg DFT", {
  p <- 8
  m <- 3
  theta <- (0:(p - 1)) * 2 * pi / p       # equally spaced
  zeta <- 0.8                             # single (variant D)
  beta <- c(0.5, 0.3, 0.15, 0.05)         # in-family, m = 3
  P <- cpm_implied_cor(theta, rep(zeta, p), beta)
  fit <- cpm_engine(P, angles = theta * 180 / pi, m = m,
                    variant = "D", reference = 1)
  # The circulant's common part first row is zeta^2 * rho(2*pi*j/p); the
  # (truncated, nonnegative) DFT recovers beta by construction.
  expect_lte(fit$F, 1e-9)
  expect_equal(fit$beta, beta, tolerance = 1e-5)
  expect_equal(fit$zeta, rep(zeta, p), tolerance = 1e-5)
})

# ---- 7. df table pins (sec. 1.4) ------------------------------------------------

test_that("df table: p=8, m=3 gives 10/17/17/24 for A/B/C/D", {
  expect_equal(cpm_spec(8, 3, "A", 1)$df, 10)
  expect_equal(cpm_spec(8, 3, "B", 1)$df, 17)
  expect_equal(cpm_spec(8, 3, "C", 1)$df, 17)
  expect_equal(cpm_spec(8, 3, "D", 1)$df, 24)
})

test_that("feasibility: df = 0 warns, singular R refused, m-cap enforced", {
  # df = 0 for variant A: p = 5, m = 1 => q = 2*5-1+1 = 10 = p(p-1)/2 = 10.
  theta5 <- c(0, 72, 144, 216, 288)
  P5 <- cpm_implied_cor(theta5 * pi / 180, rep(0.8, 5), c(0.6, 0.4))
  expect_warning(cpm_engine(P5, angles = theta5, m = 1, variant = "A"),
                 regexp = "df")
  # singular R refused with a clear error
  Rsing <- matrix(1, 4, 4)
  expect_error(cpm_engine(Rsing, angles = c(0, 90, 180, 270), m = 1, variant = "A"),
               regexp = "positive definite|singular|PD")
  # m above the default cap floor((p-1)/2) errors for variant A
  expect_error(cpm_spec(8, 4, "A", 1), regexp = "m")
})

# ---- 8. RNG silence (sec. 6.5, A-review F4) --------------------------------------

test_that("cpm_engine leaves .Random.seed untouched and is deterministic", {
  theta0 <- c(0, 0.8, 1.6, 2.4, 3.2, 4.0, 4.8, 5.6)
  P0 <- cpm_implied_cor(theta0, rep(0.8, 8), c(0.5, 0.3, 0.2))

  # No seed set: .Random.seed should not be created by the engine.
  if (exists(".Random.seed", envir = globalenv())) {
    rm(".Random.seed", envir = globalenv())
  }
  fit1 <- cpm_engine(P0, angles = theta0 * 180 / pi, m = 3, variant = "A")
  expect_false(exists(".Random.seed", envir = globalenv()))

  # With a seed set: the engine must not advance the stream.
  set.seed(999)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  before <- .Random.seed
  fit2 <- cpm_engine(P0, angles = theta0 * 180 / pi, m = 3, variant = "A")
  expect_identical(.Random.seed, before)

  # Byte-identical estimates across calls.
  expect_identical(fit1$theta, fit2$theta)
  expect_identical(fit1$zeta, fit2$zeta)
  expect_identical(fit1$beta, fit2$beta)
})

# ---- 9. convention trap: degrees at API, radians inside (sec. 6.5) --------------

test_that("degree API call reproduces an internal radian construction exactly", {
  theta_deg <- c(0, 45, 90, 135, 180, 225, 270, 315)
  theta_rad <- theta_deg * pi / 180
  P0 <- cpm_implied_cor(theta_rad, rep(0.8, 8), c(0.5, 0.3, 0.2))
  fit <- cpm_engine(P0, angles = theta_deg, m = 3, variant = "A")
  # Reported angles are in [0, 360); reference-relative directions match input.
  # Compare via (cos, sin) of the reference-relative angle (robust to the
  # exact-half-turn +/-pi atom at the antipode).
  expect_true(all(fit$theta >= 0 & fit$theta < 360))
  rr_fit <- fit$theta_rad - fit$theta_rad[1]
  rr_in <- theta_rad - theta_rad[1]
  expect_lt(max(abs(cos(rr_fit) - cos(rr_in))), 1e-4)
  expect_lt(max(abs(sin(rr_fit) - sin(rr_in))), 1e-4)
})

# ---- 10. boundary / canonicalization / polish / diagnostics -----------------

test_that("mirror starts converge to equal F and identical canonical output", {
  # An in-family truth: fitting P0 and its reflection both recover the same
  # canonicalized (theory-closest) solution.
  theta0 <- c(0, 0.8, 1.7, 2.6, 3.5, 4.4, 5.3, 6.0)
  zeta0 <- rep(0.78, 8)
  beta0 <- c(0.5, 0.3, 0.2)
  P0 <- cpm_implied_cor(theta0, zeta0, beta0)
  # Reflected generating angles produce the mirror P; canonicalization toward
  # the theory angles must return angles matching the (unreflected) theory.
  fit <- cpm_engine(P0, angles = theta0 * 180 / pi, m = 3, variant = "A")
  # canonical solution close to theory
  d <- circumplex:::angle_dist(as_radian(fit$theta_rad), as_radian(theta0))
  expect_lt(max(abs(d)), 1e-3)
})

test_that("canonicalization is invariant to reflecting the generating angles", {
  theta0 <- c(0, 0.8, 1.7, 2.6, 3.5, 4.4, 5.3, 6.0)
  zeta0 <- c(0.85, 0.8, 0.75, 0.7, 0.82, 0.78, 0.73, 0.68)
  beta0 <- c(0.5, 0.3, 0.2)
  P0 <- cpm_implied_cor(theta0, zeta0, beta0)
  # Reflect the theory angles about the reference; the same data P0 with
  # reflected theory should canonicalize to the reflected theory.
  theta_ref <- theta0[1]
  theta_refl <- 2 * theta_ref - theta0
  P_refl <- cpm_implied_cor(theta_refl, zeta0, beta0)  # this is the mirror data
  fit_a <- cpm_engine(P0, angles = theta0 * 180 / pi, m = 3, variant = "A")
  fit_b <- cpm_engine(P_refl, angles = theta_refl * 180 / pi, m = 3, variant = "A")
  # F and reference-relative estimates (unsigned pattern) match.
  expect_equal(fit_a$F, fit_b$F, tolerance = 1e-6)
  expect_equal(sort(fit_a$zeta), sort(fit_b$zeta), tolerance = 1e-4)
})

test_that("canonicalization is invariant to permuting scale order", {
  theta0 <- c(0, 0.8, 1.7, 2.6, 3.5, 4.4, 5.3, 6.0)
  zeta0 <- c(0.85, 0.8, 0.75, 0.7, 0.82, 0.78, 0.73, 0.68)
  beta0 <- c(0.5, 0.3, 0.2)
  P0 <- cpm_implied_cor(theta0, zeta0, beta0)
  perm <- c(1, 3, 5, 7, 2, 4, 6, 8)  # keeps reference (1) first
  Pp <- P0[perm, perm]
  fit_a <- cpm_engine(P0, angles = theta0 * 180 / pi, m = 3, variant = "A")
  fit_b <- cpm_engine(Pp, angles = theta0[perm] * 180 / pi, m = 3, variant = "A")
  expect_equal(fit_a$F, fit_b$F, tolerance = 1e-6)
  expect_equal(fit_a$beta, fit_b$beta, tolerance = 1e-4)
})

test_that("beta_m -> 0 polish drops the boundary harmonic; df increases", {
  # In-family truth with a negligible top harmonic: polish should drop it.
  theta0 <- c(0, 0.8, 1.6, 2.4, 3.2, 4.0, 4.8, 5.6)
  zeta0 <- rep(0.8, 8)
  beta0 <- c(0.55, 0.30, 0.15, 0.0)  # beta_3 = 0 exactly (softmax -> tiny)
  # Renormalize to be safe.
  beta0 <- beta0 / sum(beta0)
  P0 <- cpm_implied_cor(theta0, zeta0, beta0)
  fit <- cpm_engine(P0, angles = theta0 * 180 / pi, m = 3, variant = "A")
  # Removing a boundary harmonic drops a free parameter, so df INCREASES
  # relative to the nominal m = 3 model (design sec. 3.5: fewer free params, the
  # conservative-leaning chi-square-mixture reference).
  nominal_df <- cpm_spec(8, 3, "A", 1)$df
  expect_gt(fit$df, nominal_df)
  expect_true(length(fit$removed_harmonics) >= 1)
  expect_lte(fit$F, 1e-8)
  # the TOP harmonic (k = 3) was removed, so m-as-fitted decreases (sec. 3.5)
  expect_equal(fit$m, 2)
})

test_that("zeta -> 1 fires a Heywood flag", {
  theta0 <- c(0, 0.8, 1.6, 2.4, 3.2, 4.0, 4.8, 5.6)
  zeta0 <- c(0.999, rep(0.7, 7))  # one near 1
  beta0 <- c(0.5, 0.3, 0.2)
  P0 <- cpm_implied_cor(theta0, zeta0, beta0)
  fit <- suppressWarnings(
    cpm_engine(P0, angles = theta0 * 180 / pi, m = 3, variant = "A")
  )
  expect_true(fit$heywood)
})

test_that("exact recovery for variants B (fixed angles) and C (single zeta)", {
  theta0 <- c(0, 45, 90, 135, 180, 225, 270, 315) * pi / 180
  # Variant B: angles fixed, free zeta.
  zetaB <- c(0.85, 0.8, 0.75, 0.7, 0.82, 0.78, 0.73, 0.68)
  beta0 <- c(0.5, 0.3, 0.2)
  PB <- cpm_implied_cor(theta0, zetaB, beta0)
  fitB <- cpm_engine(PB, angles = theta0 * 180 / pi, m = 3, variant = "B")
  expect_lte(fitB$F, 1e-9)
  expect_equal(fitB$zeta, zetaB, tolerance = 1e-5)
  # Variant C: free angles, single shared zeta.
  zetaC <- 0.77
  PC <- cpm_implied_cor(theta0, rep(zetaC, 8), beta0)
  fitC <- cpm_engine(PC, angles = theta0 * 180 / pi, m = 3, variant = "C")
  expect_lte(fitC$F, 1e-9)
  expect_equal(unique(round(fitC$zeta, 6)), round(zetaC, 6), tolerance = 1e-5)
})

test_that("canonicalization is invariant to a common rotation of the angles", {
  theta0 <- c(0, 0.8, 1.7, 2.6, 3.5, 4.4, 5.3, 6.0)
  zeta0 <- c(0.85, 0.8, 0.75, 0.7, 0.82, 0.78, 0.73, 0.68)
  beta0 <- c(0.5, 0.3, 0.2)
  P0 <- cpm_implied_cor(theta0, zeta0, beta0)
  rot <- 1.3
  fit_a <- cpm_engine(P0, angles = theta0 * 180 / pi, m = 3, variant = "A")
  fit_b <- cpm_engine(P0, angles = (theta0 + rot) * 180 / pi, m = 3, variant = "A")
  # F and reference-relative angle pattern are rotation-invariant.
  expect_equal(fit_a$F, fit_b$F, tolerance = 1e-6)
  rr_a <- fit_a$theta_rad - fit_a$theta_rad[1]
  rr_b <- fit_b$theta_rad - fit_b$theta_rad[1]
  expect_lt(max(abs(cos(rr_a) - cos(rr_b))), 1e-3)
  expect_lt(max(abs(sin(rr_a) - sin(rr_b))), 1e-3)
})

test_that("a clean in-family fit is not flagged multimodal", {
  theta0 <- c(0, 0.8, 1.7, 2.6, 3.5, 4.4, 5.3, 6.0)
  zeta0 <- rep(0.78, 8)
  beta0 <- c(0.5, 0.3, 0.2)
  P0 <- cpm_implied_cor(theta0, zeta0, beta0)
  fit <- cpm_engine(P0, angles = theta0 * 180 / pi, m = 3, variant = "A")
  expect_false(fit$multimodal)
  expect_true(fit$accepted)
})

test_that("clustered angles fire the ill-conditioning warning", {
  # All items in a very tight arc -> ill-conditioned Hessian (angles weakly
  # separated, so the discrepancy is nearly flat in their differences).
  theta0 <- c(0, cumsum(rep(0.02, 7)))
  zeta0 <- rep(0.75, 8)
  beta0 <- c(0.5, 0.3, 0.2)
  P0 <- cpm_implied_cor(theta0, zeta0, beta0)
  expect_warning(
    cpm_engine(P0, angles = theta0 * 180 / pi, m = 3, variant = "A"),
    regexp = "condition|ill-conditioned|clustered"
  )
})

# ---- review additions (Fable pass over the B1 implementation) ----------------

test_that("analytic gradient matches FD on a polished (reduced keep_k) spec", {
  # The polish path optimizes a spec whose beta support is a strict subset of
  # 0:m (here the INTERIOR harmonic k = 1 removed). The gradient over that
  # reduced softmax was only indirectly covered by the full-spec FD test.
  set.seed(20260706)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  h <- 1e-6
  # the same spec surgery cpm_polish_beta performs: drop k = 1
  spec <- cpm_spec_reduce(cpm_spec(p = 8, m = 3, variant = "A", reference = 1),
                          c(0L, 2L, 3L))
  spec$theta_ref_val <- 0
  for (t in 1:10) {
    R <- rand_cor(8)
    theta <- c(0, sort(stats::runif(7, 0, 2 * pi)))
    zeta <- stats::runif(8, 0.05, 0.995)
    v <- stats::rnorm(2)
    b_keep <- exp(c(0, v)) / sum(exp(c(0, v)))
    beta <- numeric(4)
    beta[spec$keep_k + 1L] <- b_keep          # beta_1 = 0 (removed harmonic)
    gstar <- cpm_pack(theta, zeta, beta, spec)
    ga <- cpm_gradient(gstar, R, spec)
    gfd <- numeric(spec$q)
    for (i in seq_len(spec$q)) {
      gp <- gstar; gm <- gstar
      gp[i] <- gp[i] + h; gm[i] <- gm[i] - h
      gfd[i] <- (cpm_objective(gp, R, spec) - cpm_objective(gm, R, spec)) / (2 * h)
    }
    expect_true(
      all(abs(ga - gfd) <= 1e-7 * pmax(1, abs(gfd))),
      info = paste0("reduced-spec gradient point ", t)
    )
  }
})

test_that("polish removes an INTERIOR harmonic (beta_1 = 0, beta_2/3 > 0)", {
  theta0 <- seq(0, 2 * pi * 7 / 8, length.out = 8)
  zeta0 <- rep(0.8, 8)
  beta0 <- c(0.5, 0, 0.3, 0.2)               # interior k = 1 on the boundary
  P0 <- cpm_implied_cor(theta0, zeta0, beta0)
  fit <- cpm_engine(P0, angles = theta0 * 180 / pi, m = 3, variant = "A")
  expect_identical(fit$removed_harmonics, 1L)
  # removing one free parameter: q drops by 1, df rises by 1; m itself is
  # unchanged (the TOP harmonic survives)
  expect_equal(fit$df, cpm_spec(8, 3, "A", 1)$df + 1)
  expect_equal(fit$m, 3)
  expect_lte(fit$F, 1e-8)
  expect_identical(fit$beta[2], 0)
  expect_equal(fit$beta[c(1, 3, 4)], beta0[c(1, 3, 4)], tolerance = 1e-4)
})

test_that("all harmonics on the boundary: polish collapses to beta_0 = 1", {
  # rho(delta) === 1 (a pure general factor): every k >= 1 is polished out and
  # the reduced model has a SINGLE kept harmonic. Regression for the
  # diag(scalar) crash in the reduced-softmax gradient; angles are genuinely
  # unidentified here, so the multimodality flag SHOULD fire (that is the
  # documented non-identification signature, design sec. 2.5/sec. 3.5).
  theta0 <- seq(0, 2 * pi * 7 / 8, length.out = 8)
  P0 <- cpm_implied_cor(theta0, rep(0.8, 8), c(1, 0, 0, 0))
  fit <- suppressWarnings(
    cpm_engine(P0, angles = theta0 * 180 / pi, m = 3, variant = "A")
  )
  expect_identical(fit$removed_harmonics, 1:3)
  expect_equal(fit$beta, c(1, 0, 0, 0), tolerance = 1e-6)
  expect_lte(fit$F, 1e-8)
  expect_true(fit$multimodal)
  expect_equal(fit$m, 0)   # every harmonic removed: fitted order is 0
})

test_that("canonicalization tie-break: CCW rule picks the deciding branch", {
  # theta_theory all at 0 makes both reflections exactly equidistant from
  # theory (|shortest arc| is reflection-symmetric), forcing the tie path.
  spec <- cpm_spec(p = 3, m = 1, variant = "A", reference = 1)
  spec$theta_ref_val <- 0
  theory <- c(0, 0, 0)
  zeta <- c(0.7, 0.7, 0.7)
  beta <- c(0.6, 0.4)

  # scale 2 at 330 deg relative: CW, so the CCW rule must pick the REFLECTED
  # branch (scale 2 at 30 deg).
  g_cw <- cpm_pack(c(0, 330, 60) * pi / 180, zeta, beta, spec)
  res <- cpm_canonicalize(g_cw, spec, theory)
  expect_false(res$warn)
  expect_equal(res$par, cpm_reflect_par(g_cw, spec))

  # scale 2 exactly at 180 (undecided) falls through; scale 3 at 40 deg
  # decides CCW for the unreflected branch.
  g_fall <- cpm_pack(c(0, 180, 40) * pi / 180, zeta, beta, spec)
  res <- cpm_canonicalize(g_fall, spec, theory)
  expect_false(res$warn)
  expect_equal(res$par, g_fall)
})

test_that("canonicalization tie-break: fully undecided warns and reports as-is", {
  spec <- cpm_spec(p = 3, m = 1, variant = "A", reference = 1)
  spec$theta_ref_val <- 0
  # every non-reference scale exactly opposite the reference: no scale ever
  # decides, in either branch
  g_undec <- cpm_pack(c(0, 180, 180) * pi / 180, c(0.7, 0.7, 0.7), c(0.6, 0.4),
                      spec)
  res <- cpm_canonicalize(g_undec, spec, c(0, 0, 0))
  expect_true(res$warn)
  expect_equal(res$par, g_undec)
})

test_that("exact-octant in-family fit is NOT flagged multimodal", {
  # Octant angles put one scale exactly opposite the reference (relative angle
  # +pi in both mirrors via the angle_dist atom), which a non-circular mirror
  # comparison misreads as a distinct equal-F optimum. Regression for the
  # circular mirror/same-point detection.
  theta0 <- seq(0, 2 * pi * 7 / 8, length.out = 8)   # exact octants
  zeta0 <- rep(0.8, 8)
  beta0 <- c(0.4, 0.3, 0.2, 0.1)
  P0 <- cpm_implied_cor(theta0, zeta0, beta0)
  fit <- cpm_engine(P0, angles = theta0 * 180 / pi, m = 3, variant = "A")
  expect_false(fit$multimodal)
  expect_true(fit$accepted)
  expect_lte(fit$F, 1e-10)
})

test_that("analytic gradient matches FD for variants C and D (shared zeta)", {
  # The shared-u chain (dF/du = z(1-z) * sum_i dF/dzeta_i) and the fixed-angle
  # unpack path were not exercised by the variant-A FD test.
  set.seed(20260707)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  h <- 1e-6
  for (variant in c("C", "D")) {
    spec <- cpm_spec(p = 6, m = 2, variant = variant, reference = 1)
    spec$theta_ref_val <- 0
    theta_fix <- c(0, sort(stats::runif(5, 0, 2 * pi)))
    spec$theta_fixed <- theta_fix
    for (t in 1:10) {
      R <- rand_cor(6)
      theta <- if (variant == "C") {
        c(0, sort(stats::runif(5, 0, 2 * pi)))
      } else {
        theta_fix
      }
      zeta <- rep(stats::runif(1, 0.1, 0.95), 6)
      v <- c(0, stats::rnorm(2))
      beta <- exp(v) / sum(exp(v))
      gstar <- cpm_pack(theta, zeta, beta, spec)
      ga <- cpm_gradient(gstar, R, spec)
      gfd <- numeric(spec$q)
      for (i in seq_len(spec$q)) {
        gp <- gstar; gm <- gstar
        gp[i] <- gp[i] + h; gm[i] <- gm[i] - h
        gfd[i] <- (cpm_objective(gp, R, spec) -
                     cpm_objective(gm, R, spec)) / (2 * h)
      }
      expect_true(
        all(abs(ga - gfd) <= 1e-7 * pmax(1, abs(gfd))),
        info = paste0("variant ", variant, " gradient point ", t)
      )
    }
  }
})

# ---- M4 review #1: vacuous "reproduced" via the g0+mirror pair ---------------

test_that("g0+mirror alone cannot certify convergence acceptance (A/C)", {
  # Reflection is an exact F-isometry (rho is even), so the mirror start is a
  # deterministic image of g0 and always ties its F. "Reproduced by >= 2
  # starts" must mean >= 2 INDEPENDENT starts: the g0/mirror pair counts once.
  # Seed 19 gives a random R where g0 and its mirror share the multi-start
  # min F but every independent jitter lands in a strictly worse basin --
  # a start-dependent optimum that must NOT be reported as accepted.
  set.seed(19)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  R <- rand_cor(8)
  octant_deg <- c(90, 135, 180, 225, 270, 315, 360, 45)

  # Pin the vacuous pattern itself (guards against the seed silently drifting
  # to a case where a jitter also reaches min F): rebuild the engine's
  # multi-start set and check g0+mirror are at min F, all jitters worse.
  spec <- cpm_spec(8, 3, "A", 1)
  # Same wrapping as the engine (LM = 360 must become 0 here as it does there,
  # or the rebuilt start set is not the one the engine optimizes).
  theta_theory <- (octant_deg * pi / 180) %% (2 * pi)
  spec$theta_ref_val <- theta_theory[1]
  spec$theta_fixed <- theta_theory
  sv <- cpm_start_values((R + t(R)) / 2, theta_theory, 3)
  starts <- list(
    cpm_pack(theta_theory, sv$zeta, sv$beta, spec),
    cpm_reflect_par(cpm_pack(theta_theory, sv$zeta, sv$beta, spec), spec)
  )
  for (off in cpm_jitter_offsets_deg(spec$free_angles)) {
    theta_j <- theta_theory
    theta_j[spec$free_pos] <- theta_theory[spec$free_pos] + off * pi / 180
    starts[[length(starts) + 1]] <- cpm_pack(theta_j, sv$zeta, sv$beta, spec)
  }
  Fs <- vapply(starts, function(g) cpm_optimize_one(g, (R + t(R)) / 2, spec)$F,
               numeric(1))
  at_min <- abs(Fs - min(Fs)) <= 1e-8
  expect_true(all(at_min[1:2]))
  expect_false(any(at_min[-(1:2)]))

  # The engine must warn and report accepted = FALSE. Collect all warnings
  # (this R also fires the unrelated Hessian ill-conditioning warning).
  ws <- character(0)
  fit <- withCallingHandlers(
    cpm_engine(R, angles = octant_deg, m = 3, variant = "A"),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("convergence acceptance", ws)))
  expect_false(fit$accepted)
})

test_that("free-angle acceptance still holds when a jitter confirms g0", {
  # Clean in-family data: jitter starts converge to the same optimum as g0,
  # which IS a valid independent reproduction -- must stay accepted.
  theta0 <- c(0, 0.8, 1.7, 2.6, 3.5, 4.4, 5.3, 6.0)
  P0 <- cpm_implied_cor(theta0, rep(0.78, 8), c(0.5, 0.3, 0.2))
  for (v in c("A", "C")) {
    fit <- cpm_engine(P0, angles = theta0 * 180 / pi, m = 3, variant = v)
    expect_true(fit$accepted, info = paste("variant", v))
  }
})

# ---- 12. beta = 0 start boundary (Linux-CI regression, 2026-07) --------------
# The LS start coefficient for a harmonic absent from the population is
# analytically zero; floating point lands it at exactly 0.0 or +/-1e-16
# DEPENDING ON THE BLAS (exact 0.0 under the runners' reference BLAS, which
# crashed cpm_pack's softmax inverse). These tests are platform-independent:
# the helper is pinned with a literal exact zero, and the engine-level pin
# asserts the invariant (strictly interior starts) that every platform must
# satisfy.

test_that("cpm_beta_start_interior() floors exact zeros like their negative twins", {
  fb <- c(0.4, 0.3, 0.2, 0.1)
  # the CI crash case: a literal exact-zero trailing coefficient
  out0 <- cpm_beta_start_interior(c(0.5, 0.3, 0.2, 0), fb)
  expect_true(all(out0 > 0))
  expect_equal(out0, c(0.5, 0.3, 0.2, 0.01) / 1.01)
  # analytically identical epsilon-negative twin takes the same path
  outn <- cpm_beta_start_interior(c(0.5, 0.3, 0.2, -1e-16), fb)
  expect_identical(out0, outn)
  # strictly positive input is untouched (only normalized)
  pos <- c(0.6, 0.35, 0.049, 0.001)
  expect_identical(cpm_beta_start_interior(pos, fb), pos / sum(pos))
  # undefined LS solve and the all-zero corner keep the documented fallback
  expect_identical(
    cpm_beta_start_interior(c(NA_real_, 0.3, 0.2, 0.1), fb), fb / sum(fb)
  )
  expect_identical(cpm_beta_start_interior(rep(0, 4), fb), fb / sum(fb))
})

test_that("vanishing-harmonic populations yield strictly interior starts that pack", {
  # Both CI-failing populations: true beta has m = 2 harmonics, fitted m = 3,
  # so the m = 3 start coefficient is analytically zero (BLAS-knife-edge).
  cases <- list(
    pole = list(theta = c(0, 0, 0.9, 1.8, 2.7, 3.6, 4.5, 5.4), zeta = rep(0.75, 8)),
    mirror = list(theta = c(0, 0.8, 1.7, 2.6, 3.5, 4.4, 5.3, 6.0), zeta = rep(0.78, 8))
  )
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    P0 <- cpm_implied_cor(cs$theta, cs$zeta, c(0.5, 0.3, 0.2))
    sv <- cpm_start_values(P0, cs$theta, m = 3)
    expect_true(all(sv$beta > 0), label = paste0(nm, ": all(sv$beta > 0)"))
    spec <- cpm_spec(8, 3, "A", 1)
    expect_silent(cpm_pack(cs$theta, sv$zeta, sv$beta, spec))
  }
})
