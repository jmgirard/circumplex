# Tests for the cpm_fit() user-facing API and the circumplex_cpm class (M4/B2).
#
# ORACLE RULE (m4-browne-design.md sec. 6.1): no expected numerical value comes
# from memory or from devel/g2xx1.txt. Every expected value below is derived
# in-test by closed form, construction, or an independent base-R computation.

# ---- helpers ----------------------------------------------------------------

# Independent implied correlation matrix (re-derived, not the engine's).
api_ref_P <- function(theta, zeta, beta) {
  p <- length(theta)
  D <- outer(theta, theta, `-`)
  k <- seq_along(beta) - 1L
  C <- matrix(vapply(as.vector(D), function(d) sum(beta * cos(k * d)), numeric(1)),
              nrow = p)
  Dz <- diag(zeta)
  P <- Dz %*% C %*% Dz + (diag(p) - Dz^2)
  diag(P) <- 1
  P
}

# A clean, well-identified in-family octant matrix (F = 0 => perfect fit).
clean_octant_P <- function() {
  theta <- c(0, 45, 90, 135, 180, 225, 270, 315) * pi / 180
  zeta <- c(0.75, 0.80, 0.72, 0.78, 0.74, 0.82, 0.76, 0.79)
  beta <- c(0.45, 0.35, 0.15, 0.05)
  api_ref_P(theta, zeta, beta)
}

# A deterministic matrix that genuinely misfits the m = 3 quasi-circumplex: an
# in-family octant model plus a fixed k = 4 (Nyquist) alternating harmonic,
# which lies beyond the m = 3 cap and cannot be absorbed by moving angles. Well
# conditioned (no Heywood) so the analytic CIs and fit indices are all finite
# and exactly reproducible.
misfit_octant_P <- function() {
  a <- c(0, 45, 90, 135, 180, 225, 270, 315) * pi / 180
  P0 <- api_ref_P(a, rep(0.78, 8), c(0.45, 0.35, 0.15, 0.05))
  v <- rep(c(1, -1), 4)
  E <- v %o% v
  diag(E) <- 0
  R <- P0 + 0.05 * E
  round((R + t(R)) / 2, 6)                     # symmetric, exactly reproducible
}

oct_labels <- function() c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
oct_angles <- function() c(0, 45, 90, 135, 180, 225, 270, 315)

# Simulated raw data from a clean in-family octant model (Cholesky route,
# independent of any package simulation code). Consumes the RNG: callers seed
# and clean up .Random.seed per the test-cpm_fit.R convention.
sim_octant_data <- function(N, seed) {
  set.seed(seed)
  theta <- oct_angles() * pi / 180
  P <- api_ref_P(theta, rep(0.75, 8), c(0.45, 0.35, 0.15, 0.05))
  X <- matrix(stats::rnorm(N * 8), N) %*% chol(P)
  colnames(X) <- oct_labels()
  as.data.frame(X)
}

# ---- object shape (design sec. 5.4) -----------------------------------------

test_that("cpm_fit returns a circumplex_cpm matching the design sec. 5.4 sketch", {
  fit <- cpm_fit(cormat = clean_octant_P(), scales = oct_labels(),
                 angles = oct_angles(), n = 500)
  expect_s3_class(fit, "circumplex_cpm")
  expect_named(fit,
    c("results", "betas", "fit", "corfun", "matrices", "details", "call"))

  expect_named(fit$results,
    c("Scale", "Angle_theory", "Angle", "Angle_lci", "Angle_uci",
      "Zeta", "Zeta_lci", "Zeta_uci", "Communality"))
  expect_named(fit$betas, c("k", "Beta", "Beta_lci", "Beta_uci"))
  expect_true(all(c("chisq", "df", "pvalue", "rmsea", "rmsea_ci", "srmr",
    "cfi", "tli", "aic", "bic", "F", "n", "N") %in% names(fit$fit)))
  expect_true(is.function(fit$corfun))
  expect_named(fit$matrices, c("R", "Phat", "residuals"))

  # Communality is zeta^2; the reference angle CI is degenerate (fixed).
  expect_equal(fit$results$Communality, fit$results$Zeta^2)
  expect_equal(fit$results$Angle_lci[1], fit$results$Angle[1])
  expect_equal(fit$results$Angle_uci[1], fit$results$Angle[1])
})

test_that("the Brief-B contract fields are present (gamma-hat, N, m, matrices)", {
  fit <- cpm_fit(cormat = clean_octant_P(), scales = oct_labels(),
                 angles = oct_angles(), n = 500)
  # gamma-hat = theta / zeta / beta, plus N and m, plus programmatic handles.
  expect_true(all(c("spec", "par", "theta_rad", "N", "m") %in% names(fit$details)))
  expect_equal(length(fit$details$theta_rad), 8)
  expect_equal(nrow(fit$results), 8)          # zeta / angle per scale
  expect_true(all(fit$betas$k == 0:3))        # beta_0 .. beta_m
  expect_equal(fit$fit$N, 500)
})

# ---- fit indices (design sec. 5.3) ------------------------------------------

test_that("the test statistic uses n = N - 1 (Wishart df), not N", {
  # Non-perfect fit so N vs N-1 give distinguishable T.
  R <- misfit_octant_P()
  N <- 200L
  fit <- cpm_fit(cormat = R, scales = oct_labels(), angles = oct_angles(), n = N)
  Fhat <- fit$fit$F
  expect_gt(Fhat, 1e-6)                        # genuinely misfitting
  expect_equal(fit$fit$chisq, (N - 1) * Fhat)
  expect_false(isTRUE(all.equal(fit$fit$chisq, N * Fhat)))
  expect_equal(fit$fit$n, N - 1)
  expect_equal(fit$fit$N, N)
})

test_that("SRMR uses the off-diagonal-only denominator p(p-1)/2", {
  R <- misfit_octant_P()
  fit <- cpm_fit(cormat = R, scales = oct_labels(), angles = oct_angles(), n = 300)
  resid <- fit$matrices$residuals
  off <- resid[upper.tri(resid)]
  p <- nrow(R)
  expect_equal(fit$fit$srmr, sqrt(sum(off^2) / (p * (p - 1) / 2)))
  # NOT the diagonal-inclusive p(p+1)/2 convention (design sec. 6.3, F6).
  alt <- sqrt(sum(off^2) / (p * (p + 1) / 2))
  expect_false(isTRUE(all.equal(fit$fit$srmr, alt)))
})

test_that("CFI/TLI degrade to 1 (not NaN/Inf) when the baseline has no misfit", {
  # Near-independence data: -ln|R| ~ 0 so the independence baseline misfit
  # T0 - df0 <= 0. With a well-fitting model the standard CFI ratio is 0/0 and
  # the TLI ratio divides by ~0; convention returns perfect incremental fit (1)
  # rather than NaN/Inf (milestone-close review finding).
  p <- 8L
  R <- diag(p)
  R[upper.tri(R)] <- R[lower.tri(R)] <- 0.01     # PD, near identity
  fi <- circumplex:::cpm_fit_indices(
    Fhat = 1e-6, df = 10L, p = p, N = 500L, R = R, Phat = R, q = 17L
  )
  expect_true(is.finite(fi$cfi))                 # was NaN (0/0) before the guard
  expect_true(is.finite(fi$tli))
  expect_equal(fi$cfi, 1)
  expect_equal(fi$tli, 1)
})

test_that("AIC/BIC follow T + 2q and T + q ln(N)", {
  R <- misfit_octant_P()
  N <- 250L
  fit <- cpm_fit(cormat = R, scales = oct_labels(), angles = oct_angles(), n = N)
  p <- nrow(R)
  q <- p * (p - 1) / 2 - fit$fit$df
  expect_equal(fit$fit$aic, fit$fit$chisq + 2 * q)
  expect_equal(fit$fit$bic, fit$fit$chisq + q * log(N))
})

test_that("RMSEA CI: excellent fit collapses to [0, 0] (both edge guards)", {
  # Perfect in-family fit => T ~ 0 => the lambda_U equation has no root; the
  # guard must return [0, 0] rather than erroring (design sec. 5.3, A-review F5).
  fit <- cpm_fit(cormat = clean_octant_P(), scales = oct_labels(),
                 angles = oct_angles(), n = 500)
  expect_equal(fit$fit$rmsea, 0)
  expect_equal(fit$fit$rmsea_ci, c(0, 0))
})

test_that("RMSEA CI: estimate lies inside and endpoints match the noncentral tails", {
  R <- misfit_octant_P()
  # N chosen so T is large enough that neither edge guard is active (both bounds
  # are interior roots), letting us confirm the intended tail probabilities.
  N <- 300L
  fit <- cpm_fit(cormat = R, scales = oct_labels(), angles = oct_angles(), n = N)
  ci <- fit$fit$rmsea_ci
  expect_gt(ci[1], 0)                            # lower guard inactive here
  expect_gte(fit$fit$rmsea, ci[1])
  expect_lte(fit$fit$rmsea, ci[2])
  # Reconstruct the ncp from the RMSEA bounds and confirm the intended tail
  # probabilities (90% CI: lower ncp at .95, upper ncp at .05).
  n <- N - 1L
  df <- fit$fit$df
  lambda_l <- ci[1]^2 * n * df
  lambda_u <- ci[2]^2 * n * df
  expect_equal(pchisq(fit$fit$chisq, df, ncp = lambda_l), 0.95, tolerance = 1e-4)
  expect_equal(pchisq(fit$fit$chisq, df, ncp = lambda_u), 0.05, tolerance = 1e-4)
})

# ---- analytic CIs (design sec. 5.2) -----------------------------------------

test_that("analytic CIs are finite and centered on the estimates", {
  R <- misfit_octant_P()
  fit <- cpm_fit(cormat = R, scales = oct_labels(), angles = oct_angles(),
                 n = 400, interval = 0.95)
  # non-reference angles have a genuine (non-degenerate) interval
  expect_true(all(is.finite(fit$results$Angle_lci)))
  expect_true(all(fit$results$Angle_lci <= fit$results$Angle))
  expect_true(all(fit$results$Angle <= fit$results$Angle_uci))
  # symmetric on the natural scale (Wald): estimate is the midpoint
  mid_zeta <- (fit$results$Zeta_lci + fit$results$Zeta_uci) / 2
  expect_equal(mid_zeta, fit$results$Zeta, tolerance = 1e-8)
  mid_beta <- (fit$betas$Beta_lci + fit$betas$Beta_uci) / 2
  expect_equal(mid_beta, fit$betas$Beta, tolerance = 1e-8)
})

test_that("a singular information matrix (Heywood) yields NA CIs, not an error", {
  data("jz2017")
  fit <- suppressWarnings(
    # NO scale Heywoods at zeta = 1; analytic CIs are the object under test.
    cpm_fit(jz2017, scales = oct_labels(), ci_method = "analytic")
  )
  expect_true(fit$details$heywood)
  expect_true(all(is.na(fit$results$Zeta_lci)))
})

test_that("Angle_theory echoes the supplied angles (LM = 360, not 0)", {
  # CLAUDE.md convention: LM = 360, not 0. octants() supplies 360 for LM; the
  # engine wraps 360 -> 0 internally, so the reported theoretical angle must be
  # sourced from the user input, not the wrapped internal value.
  fit <- cpm_fit(cormat = clean_octant_P(), scales = oct_labels(),
                 angles = as.numeric(octants()), n = 500)
  expect_equal(fit$results$Angle_theory, as.numeric(octants()))
  expect_equal(fit$results$Angle_theory[oct_labels() == "LM"], 360)
})

test_that("analytic SEs match an independent brute-force delta-method", {
  # Independent check of the logit/softmax/angle delta method: numerically
  # differentiate the natural parameters w.r.t. the unconstrained vector and
  # propagate avar = (2/n) H^-1, then compare to cpm_analytic_se().
  R <- misfit_octant_P()
  N <- 300L
  fit <- cpm_fit(cormat = R, scales = oct_labels(), angles = oct_angles(), n = N)
  spec <- fit$details$spec
  par <- fit$details$par
  H <- circumplex:::cpm_hessian_fd(par, R, spec)
  avar <- (2 / (N - 1)) * solve(H)

  nat_vec <- function(g) {
    nu <- circumplex:::cpm_unpack(g, spec)
    c(nu$theta, nu$zeta, nu$beta)
  }
  h <- 1e-6
  q <- length(par)
  Jn <- vapply(seq_len(q), function(i) {
    gp <- par; gm <- par
    gp[i] <- gp[i] + h; gm[i] <- gm[i] - h
    (nat_vec(gp) - nat_vec(gm)) / (2 * h)
  }, numeric(2 * 8 + spec$m + 1L))
  se_all <- sqrt(pmax(diag(Jn %*% avar %*% t(Jn)), 0))
  p <- 8
  se_theta_deg <- se_all[1:p] * 180 / pi
  se_zeta <- se_all[(p + 1):(2 * p)]
  se_beta <- se_all[(2 * p + 1):(2 * p + spec$m + 1L)]

  se <- circumplex:::cpm_analytic_se(
    list(spec = spec, par = par,
         zeta = circumplex:::cpm_unpack(par, spec)$zeta,
         beta = circumplex:::cpm_unpack(par, spec)$beta),
    R, N
  )
  expect_equal(se$angle, se_theta_deg, tolerance = 1e-6)
  expect_equal(se$zeta, se_zeta, tolerance = 1e-8)
  expect_equal(se$beta, se_beta, tolerance = 1e-8)
})

# ---- correlation function and matrices --------------------------------------

test_that("corfun returns rho-hat in degrees; rho(0) = 1", {
  fit <- cpm_fit(cormat = clean_octant_P(), scales = oct_labels(),
                 angles = oct_angles(), n = 500)
  expect_equal(fit$corfun(0), 1, tolerance = 1e-8)
  delta_deg <- c(0, 45, 90, 180)
  expect_equal(
    fit$corfun(delta_deg),
    circumplex:::cpm_rho(delta_deg * pi / 180, fit$betas$Beta)
  )
})

test_that("matrices: Phat is a correlation matrix and residuals = R - Phat", {
  R <- misfit_octant_P()
  fit <- cpm_fit(cormat = R, scales = oct_labels(), angles = oct_angles(), n = 300)
  # diag() carries the matrix dimnames now that R/Phat/residuals are named
  # (design gap G3); unname before comparing to the bare unit diagonal.
  expect_equal(unname(diag(fit$matrices$Phat)), rep(1, 8))
  expect_true(isSymmetric(unname(fit$matrices$Phat), tol = 1e-8))
  expect_equal(fit$matrices$residuals, fit$matrices$R - fit$matrices$Phat)
  # G3: R/Phat/residuals carry the scale names in fitted order.
  expect_identical(dimnames(fit$matrices$R), list(oct_labels(), oct_labels()))
  expect_identical(dimnames(fit$matrices$Phat), list(oct_labels(), oct_labels()))
  expect_identical(dimnames(fit$matrices$residuals),
                   list(oct_labels(), oct_labels()))
})

# ---- convention traps (design sec. 6.5) -------------------------------------

test_that("degrees at the API: circumplex_degree and numeric angles agree", {
  R <- misfit_octant_P()
  f_num <- cpm_fit(cormat = R, scales = oct_labels(), angles = oct_angles(),
                   n = 300)
  f_deg <- cpm_fit(cormat = R, scales = oct_labels(),
                   angles = as_degree(oct_angles()), n = 300)
  expect_equal(f_num$results, f_deg$results)
  expect_equal(f_num$fit, f_deg$fit)
})

test_that("raw-data and cormat paths agree on identical inputs", {
  data("jz2017")
  sdata <- stats::na.omit(jz2017[oct_labels()])
  N <- nrow(sdata)
  R <- stats::cor(sdata)
  # Analytic on both paths so the comparison is deterministic (the raw-data
  # default is bootstrap once B3 lands; point-estimate agreement across CI
  # methods is pinned in the bootstrap section below).
  f_raw <- suppressWarnings(
    cpm_fit(jz2017, scales = oct_labels(), ci_method = "analytic")
  )
  f_cm <- suppressWarnings(cpm_fit(cormat = R, scales = oct_labels(), n = N))
  expect_equal(f_raw$results$Angle, f_cm$results$Angle)
  expect_equal(f_raw$results$Zeta, f_cm$results$Zeta)
  expect_equal(f_raw$fit$chisq, f_cm$fit$chisq)
  expect_equal(f_raw$fit$N, N)
})

# ---- input validation via is_*() helpers (design sec. 4) --------------------

test_that("invalid input is rejected with clear errors", {
  R <- clean_octant_P()
  s <- oct_labels()
  a <- oct_angles()

  # exactly one of data / cormat
  expect_error(cpm_fit(scales = s, angles = a), "exactly one")
  expect_error(
    cpm_fit(data = as.data.frame(R), cormat = R, scales = s, angles = a, n = 100),
    "exactly one"
  )
  # cormat sanity
  Rns <- R; Rns[1, 2] <- Rns[1, 2] + 0.1       # break symmetry
  expect_error(cpm_fit(cormat = Rns, scales = s, angles = a, n = 100), "symmetric")
  Rnd <- R; diag(Rnd) <- 2                      # non-unit diagonal
  expect_error(cpm_fit(cormat = Rnd, scales = s, angles = a, n = 100),
               "unit diagonal")
  # sample size
  expect_error(cpm_fit(cormat = R, scales = s, angles = a), "required")
  expect_error(cpm_fit(cormat = R, scales = s, angles = a, n = 5), "n > p|n >")
  # angle length mismatch
  expect_error(cpm_fit(cormat = R, scales = s, angles = a[1:7], n = 100))
  # ci_method restrictions: no raw data to resample on the cormat path
  expect_error(
    cpm_fit(cormat = R, scales = s, angles = a, n = 100, ci_method = "bootstrap"),
    "cormat"
  )
  # listwise = FALSE unsupported
  expect_error(cpm_fit(jz2017, scales = s, listwise = FALSE), "listwise")
})

test_that("saturated model (df = 0) warns and returns NA fit indices", {
  # Variant A, p = 5, m = 1 => q = 2*5 - 1 + 1 = 10 = p(p-1)/2 => df = 0.
  theta5 <- c(0, 72, 144, 216, 288) * pi / 180
  P5 <- api_ref_P(theta5, rep(0.8, 5), c(0.6, 0.4))
  expect_warning(
    cpm_fit(cormat = P5, scales = paste0("S", 1:5),
            angles = c(0, 72, 144, 216, 288), m = 1, n = 200),
    "df"
  )
  fit <- suppressWarnings(
    cpm_fit(cormat = P5, scales = paste0("S", 1:5),
            angles = c(0, 72, 144, 216, 288), m = 1, n = 200)
  )
  expect_equal(fit$fit$df, 0)
  expect_true(is.na(fit$fit$rmsea))
  expect_true(is.na(fit$fit$pvalue))
})

# ---- bootstrap CIs (design sec. 5.2, M4/B3) ----------------------------------

test_that("bootstrap is the raw-data default and is seed-reproducible", {
  d <- sim_octant_data(300, 42)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)

  set.seed(1)
  f1 <- suppressWarnings(
    cpm_fit(d, scales = oct_labels(), angles = oct_angles(), boots = 100)
  )
  expect_identical(f1$details$ci_method, "bootstrap")

  set.seed(1)
  f2 <- suppressWarnings(
    cpm_fit(d, scales = oct_labels(), angles = oct_angles(), boots = 100)
  )
  expect_equal(f1$results, f2$results)
  expect_equal(f1$betas, f2$betas)

  # A different seed changes the CIs but never the point estimates (the
  # engine runs before, and independently of, the resampling).
  set.seed(2)
  f3 <- suppressWarnings(
    cpm_fit(d, scales = oct_labels(), angles = oct_angles(), boots = 100)
  )
  expect_equal(f3$results$Angle, f1$results$Angle)
  expect_equal(f3$results$Zeta, f1$results$Zeta)
  expect_equal(f3$betas$Beta, f1$betas$Beta)
  expect_false(identical(f3$results$Angle_lci, f1$results$Angle_lci))
})

test_that("bootstrap point estimates and fit indices match the analytic path", {
  d <- sim_octant_data(300, 42)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)

  set.seed(1)
  f_bs <- suppressWarnings(
    cpm_fit(d, scales = oct_labels(), angles = oct_angles(), boots = 50)
  )
  f_an <- cpm_fit(d, scales = oct_labels(), angles = oct_angles(), ci_method = "analytic")
  expect_equal(f_bs$results$Angle, f_an$results$Angle)
  expect_equal(f_bs$results$Zeta, f_an$results$Zeta)
  expect_equal(f_bs$betas$Beta, f_an$betas$Beta)
  expect_equal(f_bs$fit, f_an$fit)
})

test_that("bootstrap angle CIs straddling 0/360 are wrapped and contain the estimate", {
  # PA's true angle is 0; with seed 42 at N = 300 its fitted angle lands near
  # the pole (~353 degrees), so the replicate distribution straddles 0/360.
  # reference = 3 keeps PA's angle free. The circular quantile machinery
  # (quantile.circumplex_radian) reports the interval wrapped to [0, 360),
  # so a straddling CI has lci > uci, the displacement-CI convention.
  d <- sim_octant_data(300, 42)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)

  set.seed(3)
  fit <- suppressWarnings(
    cpm_fit(d, scales = oct_labels(), angles = oct_angles(), reference = 3,
            boots = 200)
  )
  est <- fit$results$Angle[1]
  lci <- fit$results$Angle_lci[1]
  uci <- fit$results$Angle_uci[1]

  # The construction must actually straddle, or the test has rotted.
  expect_gt(est, 270)
  expect_gt(lci, uci)
  expect_true(lci >= 0 && lci < 360 && uci >= 0 && uci < 360)
  # Circular containment: est lies in [lci, 360) U [0, uci].
  expect_true(est >= lci || est <= uci)
  # The wrapped interval is short (a genuine CI, not a near-full circle).
  expect_lt((uci - lci) %% 360, 90)

  # The reference angle is fixed: its interval is degenerate at the estimate.
  expect_equal(fit$results$Angle_lci[3], fit$results$Angle[3])
  expect_equal(fit$results$Angle_uci[3], fit$results$Angle[3])

  # Zeta/beta percentile intervals respect their natural (closed) ranges;
  # near-boundary replicates may round to the boundary itself.
  expect_true(all(fit$results$Zeta_lci >= 0 & fit$results$Zeta_uci <= 1))
  expect_true(all(fit$results$Zeta_lci <= fit$results$Zeta_uci))
  expect_true(all(fit$betas$Beta_lci >= 0 & fit$betas$Beta_uci <= 1))
  expect_true(all(fit$betas$Beta_lci <= fit$betas$Beta_uci))
})

test_that("discarded bootstrap replicates are counted, warned, and surfaced", {
  # N = 12 rows on p = 8 scales: most resamples are rank-deficient (non-PD),
  # and some warm refits fail the scaled-gradient acceptance criterion.
  d <- sim_octant_data(12, 5)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)

  set.seed(9)
  w <- capture_warnings(fit <- cpm_fit(d, scales = oct_labels(), angles = oct_angles(), boots = 100))
  expect_true(any(grepl("excluded", w)))

  det <- fit$details
  expect_identical(
    det$boots_used + det$boots_degenerate + det$boots_nonconvergent,
    100L
  )
  expect_gt(det$boots_degenerate, 0)
  expect_lt(det$boots_used, 100)
  expect_gt(det$boots_used, 0)

  # Surviving replicates still yield finite intervals (conditional on
  # estimability, the ssm_analyze convention).
  expect_true(all(is.finite(fit$results$Zeta_lci)))

  # The accounting reaches the user: summary() prints the exclusion note.
  out <- paste(utils::capture.output(summary(fit)), collapse = "\n")
  expect_match(out, "excluded")
})

test_that("the per-replicate mirror guard reflects a mirrored solution back", {
  # A-review F10: reflect any replicate angularly closer to the mirror of
  # gamma-hat than to gamma-hat. Reflection is an involution, so guarding the
  # mirrored gamma-hat must restore it exactly; gamma-hat itself is untouched.
  fit <- cpm_fit(cormat = clean_octant_P(), scales = oct_labels(),
                 angles = oct_angles(), n = 500)
  spec <- fit$details$spec
  par_hat <- fit$details$par
  ref_rel <- circumplex:::cpm_ref_relative(par_hat, spec)

  same <- circumplex:::cpm_mirror_guard(par_hat, spec, ref_rel)
  expect_false(same$reflected)
  expect_identical(same$par, par_hat)

  mirrored <- circumplex:::cpm_reflect_par(par_hat, spec)
  guarded <- circumplex:::cpm_mirror_guard(mirrored, spec, ref_rel)
  expect_true(guarded$reflected)
  expect_equal(guarded$par, par_hat)
})

test_that("RNG contract: analytic path is RNG-silent, bootstrap consumes the stream", {
  d <- sim_octant_data(300, 42)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)

  # cormat path (analytic default): .Random.seed untouched.
  set.seed(11)
  before <- .Random.seed
  invisible(cpm_fit(cormat = clean_octant_P(), scales = oct_labels(),
                    angles = oct_angles(), n = 500))
  expect_identical(.Random.seed, before)

  # raw-data path with analytic CIs: also RNG-silent.
  invisible(cpm_fit(d, scales = oct_labels(), angles = oct_angles(), ci_method = "analytic"))
  expect_identical(.Random.seed, before)

  # bootstrap path: consumes the stream (documented entry point).
  invisible(suppressWarnings(
    cpm_fit(d, scales = oct_labels(), angles = oct_angles(), boots = 25)
  ))
  expect_false(identical(.Random.seed, before))
})

# ---- print / summary snapshots ----------------------------------------------

test_that("print and summary render as expected", {
  fit <- cpm_fit(cormat = misfit_octant_P(), scales = oct_labels(),
                 angles = oct_angles(), n = 300)
  expect_snapshot(print(fit))
  expect_snapshot(summary(fit))
})

test_that("summary()'s analytic-CI caution follows the coverage-oracle calibration", {
  # Calibrated by devel/m4-coverage-oracle.R (M4/B6; DESIGN.md): analytic CIs
  # mis-covered for every studied truth below N = 2000; between 2000 and
  # ~50000 they mis-covered only in near-boundary regimes, which the fit's
  # own diagnostics (Heywood, removed harmonics, small beta, conditioning)
  # signal. The caution is therefore unconditional below 2000 and
  # marker-conditional up to 50000.
  # (1) N >= 2000, well-identified fit: no caution. (cpm_clean_truth()'s
  # smallest beta, .15, keeps clear margin above the .10 marker.)
  tr <- cpm_clean_truth()
  P0 <- cpm_implied_cor(as.numeric(as_radian(as_degree(tr$angles))),
                        tr$zeta, tr$beta)
  clean <- cpm_fit(cormat = P0, scales = paste0("V", 1:8), angles = tr$angles,
                   n = 5000, m = 3)
  expect_identical(cpm_boundary_markers(clean), character(0))
  expect_false(grepl("mis-cover",
                     paste(capture.output(summary(clean)), collapse = "\n"),
                     fixed = TRUE))
  # (2) N >= 2000 but a Heywood (boundary) solution: caution fires and names
  # the marker it fired on (never a dangling "see the diagnostics above").
  voc <- cpm_oracle_voc()
  hey <- suppressWarnings(cpm_fit(cormat = voc$R, scales = voc$names,
                                  angles = voc$th_start, n = 5000, m = 2))
  expect_true(hey$details$heywood)
  expect_true(cpm_boundary_proximity(hey))
  out <- paste(capture.output(summary(hey)), collapse = "\n")
  expect_match(out, "near a parameter boundary")
  expect_match(out, "Heywood communality")
  # (3) N < 2000: unconditional caution, regardless of markers.
  small <- cpm_fit(cormat = P0, scales = paste0("V", 1:8), angles = tr$angles,
                   n = 300, m = 3)
  expect_match(paste(capture.output(summary(small)), collapse = "\n"),
               "mis-cover")
  # (4) Marker edge cases (B6 review fixes): an exactly singular Hessian
  # (condition Inf) IS a marker, and an NA beta degrades to "no marker"
  # rather than an NA crash inside summary().
  sing <- clean
  sing$details$hessian_condition <- Inf
  expect_true("ill-conditioned Hessian" %in% cpm_boundary_markers(sing))
  nab <- clean
  nab$betas$Beta[2] <- NA_real_
  expect_identical(cpm_boundary_markers(nab), character(0))
  expect_no_error(summary(nab))
})

test_that("print and summary render a bootstrap fit as expected", {
  d <- sim_octant_data(300, 42)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  set.seed(1)
  fit <- suppressWarnings(
    cpm_fit(d, scales = oct_labels(), angles = oct_angles(), boots = 100)
  )
  expect_snapshot(print(fit))
  expect_snapshot(summary(fit))
})

# ---- cpm_simulate(): return contract, population recovery, RNG (M4/B4) -------
#
# Brief-A gap G1 (return contract), G2 (mean-based path only; the augmented
# correlation path reduces to matrices$Phat and is B's own job), G3 (dimnames)
# resolved here. Oracle rule holds: every expected value is derived in-test by
# construction, not from memory.

test_that("cpm_simulate: G1 return contract (matrix, dims, names, numeric)", {
  fit <- cpm_fit(cormat = clean_octant_P(), scales = oct_labels(),
                 angles = oct_angles(), n = 500)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  set.seed(1)
  X <- cpm_simulate(fit, n = 50)
  expect_true(is.matrix(X))
  expect_type(X, "double")
  expect_identical(dim(X), c(50L, 8L))
  expect_identical(colnames(X), oct_labels())     # fitted scale order
  expect_null(rownames(X))
  expect_false(anyNA(X))
})

test_that("cpm_simulate: population covariance is exactly Phat (factor form)", {
  # The generative covariance must equal matrices$Phat to machine precision:
  # rebuild Lambda Lambda^T + (I - D_zeta^2) independently from the reported
  # estimates and check it against Phat -- the contract that makes cor(X) -> Phat.
  fit <- cpm_fit(cormat = clean_octant_P(), scales = oct_labels(),
                 angles = oct_angles(), n = 500)
  theta <- fit$details$theta_rad
  zeta <- fit$results$Zeta
  beta <- fit$betas$Beta
  m <- fit$details$m
  sb <- sqrt(beta)
  Lam <- sb[1] * rep(1, 8)
  for (k in seq_len(m)) {
    Lam <- cbind(Lam, sb[k + 1] * cos(k * theta), sb[k + 1] * sin(k * theta))
  }
  Lam <- zeta * Lam
  Sigma <- Lam %*% t(Lam) + diag(1 - zeta^2)
  expect_equal(unname(Sigma), unname(fit$matrices$Phat), tolerance = 1e-10)
})

test_that("cpm_simulate: large-n sample cor -> Phat, margins ~ standardized", {
  fit <- cpm_fit(cormat = clean_octant_P(), scales = oct_labels(),
                 angles = oct_angles(), n = 500)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  set.seed(20260706)
  X <- cpm_simulate(fit, n = 40000)
  expect_equal(unname(cor(X)), unname(fit$matrices$Phat), tolerance = 0.03)
  expect_lt(max(abs(colMeans(X))), 0.05)          # zero-mean population
  expect_lt(max(abs(apply(X, 2, sd) - 1)), 0.05)  # unit-variance population
})

test_that("cpm_simulate: RNG contract -- reproducible and seed-sensitive", {
  fit <- cpm_fit(cormat = clean_octant_P(), scales = oct_labels(),
                 angles = oct_angles(), n = 500)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  set.seed(7)
  a <- cpm_simulate(fit, n = 100)
  set.seed(7)
  b <- cpm_simulate(fit, n = 100)
  expect_identical(a, b)                           # same seed -> identical
  set.seed(8)
  cc <- cpm_simulate(fit, n = 100)
  expect_false(isTRUE(all.equal(a, cc)))           # different seed -> different
  # A fit with analytic CIs is RNG-silent; only cpm_simulate consumes the stream.
  set.seed(99)
  before <- .Random.seed
  invisible(cpm_simulate(fit, n = 10))
  expect_false(identical(.Random.seed, before))
})

test_that("cpm_simulate: boundary -- angle at the 0/360 pole recovers Phat", {
  # Generating angle exactly at the pole (LM at 360). The factor form is pure
  # trig, so the pole is not special; cor(X) must still track Phat.
  theta <- c(0, 45, 90, 135, 180, 225, 270, 360) * pi / 180
  P <- api_ref_P(theta, rep(0.78, 8), c(0.45, 0.35, 0.15, 0.05))
  fit <- cpm_fit(cormat = P, scales = oct_labels(),
                 angles = c(0, 45, 90, 135, 180, 225, 270, 360), n = 500)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  set.seed(3)
  X <- cpm_simulate(fit, n = 30000)
  expect_equal(unname(cor(X)), unname(fit$matrices$Phat), tolerance = 0.03)
})

test_that("cpm_simulate: a polished-out harmonic still reproduces Phat", {
  # An in-family model with beta_3 == 0 makes cpm_fit polish k = 3 out; the
  # sqrt(beta_3) = 0 columns of Lambda then contribute nothing and the
  # generative covariance must remain exactly Phat.
  theta <- oct_angles() * pi / 180
  P <- api_ref_P(theta, rep(0.78, 8), c(0.5, 0.35, 0.15, 0))
  fit <- suppressWarnings(
    cpm_fit(cormat = P, scales = oct_labels(), angles = oct_angles(), n = 500)
  )
  expect_true(length(fit$details$removed_harmonics) >= 1)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  set.seed(5)
  X <- cpm_simulate(fit, n = 40000)
  expect_equal(unname(cor(X)), unname(fit$matrices$Phat), tolerance = 0.03)
})

test_that("cpm_simulate: prototype of the Z1 (ssm_ci_accuracy) mean-based loop", {
  # The mean-based plug-in population (spec sec. 3.2): standardized draws Z from
  # cpm_simulate, rescaled X_g = Z D_s + 1 mu^T, then the user's SSM procedure
  # is rerun on the simulated data. Exercises the exact consumption Z1 needs and
  # confirms the SSM profile is recovered at large n.
  fit <- cpm_fit(cormat = clean_octant_P(), scales = oct_labels(),
                 angles = oct_angles(), n = 500)
  on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  set.seed(101)
  sds <- c(1.2, 0.9, 1.1, 1.0, 1.3, 0.8, 1.05, 0.95)
  mu <- c(0.4, 0.1, -0.2, 0.0, 0.3, -0.1, 0.2, 0.05)
  Z <- cpm_simulate(fit, n = 5000)
  Xg <- sweep(Z * rep(sds, each = nrow(Z)), 2, mu, "+")
  df <- as.data.frame(Xg)
  res <- ssm_analyze(df, scales = oct_labels(), angles = oct_angles())
  expect_s3_class(res, "circumplex_ssm")
  # Truth: closed-form SSM on the population mean profile mu at these angles
  # (ssm_parameters takes degrees and returns Elev/Ampl/... columns).
  truth <- ssm_parameters(mu, oct_angles())
  est <- res$results
  # Absolute margins: these SSM parameters are near zero, where a relative
  # tolerance is meaninglessly tight; a loose absolute bound is the sanity gate.
  expect_lt(abs(est$e_est - truth$Elev), 0.04)
  expect_lt(abs(est$a_est - truth$Ampl), 0.04)
})

test_that("cpm_simulate: input validation via inherits()/is_count()", {
  fit <- cpm_fit(cormat = clean_octant_P(), scales = oct_labels(),
                 angles = oct_angles(), n = 500)
  expect_error(cpm_simulate(list(), n = 10), "circumplex_cpm")
  expect_error(cpm_simulate(fit, n = 0))
  expect_error(cpm_simulate(fit, n = -5))
  expect_error(cpm_simulate(fit, n = 2.5))
  expect_error(cpm_simulate(fit, n = c(10, 20)))
})
