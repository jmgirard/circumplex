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
    cpm_fit(jz2017, scales = oct_labels())     # NO scale Heywoods at zeta = 1
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
  expect_equal(diag(fit$matrices$Phat), rep(1, 8))
  expect_true(isSymmetric(unname(fit$matrices$Phat), tol = 1e-8))
  expect_equal(fit$matrices$residuals, fit$matrices$R - fit$matrices$Phat)
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
  f_raw <- suppressWarnings(cpm_fit(jz2017, scales = oct_labels()))
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
  # ci_method restrictions
  expect_error(
    cpm_fit(cormat = R, scales = s, angles = a, n = 100, ci_method = "bootstrap"),
    "cormat"
  )
  expect_error(
    suppressWarnings(cpm_fit(jz2017, scales = s, ci_method = "bootstrap")),
    "not yet implemented"
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

# ---- print / summary snapshots ----------------------------------------------

test_that("print and summary render as expected", {
  fit <- cpm_fit(cormat = misfit_octant_P(), scales = oct_labels(),
                 angles = oct_angles(), n = 300)
  expect_snapshot(print(fit))
  expect_snapshot(summary(fit))
})
