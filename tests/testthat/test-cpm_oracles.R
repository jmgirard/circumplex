# CPM validation battery (M4/B6): published, cross-implementation, and
# simulation oracles per devel/m4-browne-design.md sec. 6.
#
# ---- Published-oracle provenance (design sec. 6.1) ---------------------------
# Every expected value below was transcribed 2026-07-06 from Grassi, Luccio &
# Di Blas (2010), "CircE: An R implementation of Browne's circular stochastic
# process model", Behavior Research Methods 42(1), 55-73
# (doi:10.3758/BRM.42.1.55) -- oracle O2 of the design sec. 6.2 set. Two
# extraction channels were diffed (rendered-page visual read and the PDF text
# layer via pdftotext); the second fully independent human re-read per the
# sec. 6.1 protocol was completed 2026-07-19 (Jeff), against the primary
# source. It confirmed the transcribed values and corrected three records
# ABOUT them -- the m = 1 fit-measure page anchor, the Table 2/3 column
# label (p-hat, not zeta), and the sixth verbal-ability scale name
# (ForeignLanguage, not ForeignLiterature). Sources by fixture:
#   * correlation matrix, N:  Table 1 (p. 58), cross-checked against Listing 1
#   * model estimates:        Table 2 (p. 60)
#   * constrained-model F:    Table 3 (p. 60)
#   * unconstrained m = 1 fit measures (T, df, p, F0, RMSEA, null chi-sq,
#     TLI, CFI, SRMR):        Appendix A (pp. 70-71), NOT Table 3
#   * full-precision m = 1 output (angles, SEs, CIs, v, z, betas, fit):
#                             Appendix A (pp. 70-72)
# Appendix A prints its blocks in its own variable order, not Table 1's; the
# fixtures store everything in Table-1 order (see helper-cpm-oracles.R).
# The paper states this example reanalyzes the data Browne (1992) used
# (their Table 2) and that CircE's m = 1..3 results "coincide precisely with
# the ones obtained by CIRCUM" (p. 59), so these fixtures transitively cover
# oracle O1 (Browne's own program) as well.
#
# ---- The CIRCUM/CircE model difference (triaged per design sec. 6.3) ---------
# CIRCUM and CircE fit Browne's *covariance* structure
#   Sigma = D_zeta (P_c + D_v) D_zeta
# with p free scaling parameters, so the fitted diagonal is NOT constrained to
# the observed unit diagonal (Appendix A prints "Ratios of Reproduced Variances
# to Input Variances" of .963-1.042). Our engine fits the correlation structure
# with diag P(gamma) == 1 identically. Same df (their p extra parameters meet
# the p diagonal moments), same estimand, asymptotically equivalent -- but
# finite-sample different: at N = 175 the published F-hat (0.089815) is below
# our within-family optimum (0.09596), angles differ by up to ~1.3 deg, and
# T/RMSEA/CFI shift accordingly. Our family is nested in theirs (set s = 1),
# so F_ours >= F_published always; both directions are asserted below with
# documented allowances, and the free-scaling attribution is proven two ways:
# (a) our discrepancy evaluated at their reconstructed Sigma-hat reproduces
# their published F-hat to ~4e-7, and (b) an OpenMx fit of the free-scaling
# model reproduces their published estimates to publication precision.
# Consequence recorded in the design doc (sec. 11, 2026-07-06): the sec. 3.2
# claim that sigma-hat = 1 at the optimum when fitting R is false at finite N.

# OpenMx transcription of P = D_z C D_z + (I - D_z^2) (design sec. 1.3/6.4).
# OpenMx's ML fit function for type = "cov" data applies the (N-1)/N ML
# rescale to the observed matrix internally; for the diag-constrained model
# that rescale SHIFTS the optimum (the family is not closed under scalar
# multiplication), so the observed matrix is pre-multiplied by N/(N-1) to make
# OpenMx minimize our F(R, P) exactly. (Found empirically: without it, both
# CSOLNP and SLSQP move away from our optimum to a point that is better for
# R*(N-1)/N and worse for R.) The free-scaling variant adds D_s Sigma D_s with
# free s -- Browne's/CIRCUM's parameterization, closed under rescaling.
cpm_mx_model <- function(R, N, m, th0_rad, free_angles = TRUE,
                         free_scaling = FALSE, zeta0 = 0.8, w0 = 0) {
  p <- nrow(R)
  nm <- rownames(R)
  R_obs <- if (free_scaling) R else R * N / (N - 1)
  cterms <- paste0("beta[", seq_len(m) + 1, ", 1] * cos(",
                   ifelse(seq_len(m) == 1, "", paste0(seq_len(m), " * ")),
                   "Delta)")
  c_expr <- paste(c("beta[1, 1]", cterms), collapse = " + ")
  sig_expr <- if (free_scaling) {
    "vec2diag(s) %*% (vec2diag(zeta) %*% C %*% vec2diag(zeta) + Ip -
       vec2diag(zeta * zeta)) %*% vec2diag(s)"
  } else {
    "vec2diag(zeta) %*% C %*% vec2diag(zeta) + Ip - vec2diag(zeta * zeta)"
  }
  args <- list(
    "cpm",
    OpenMx::mxMatrix("Full", p, 1, free = c(FALSE, rep(free_angles, p - 1)),
                     values = th0_rad, name = "theta"),
    OpenMx::mxMatrix("Full", p, 1, free = TRUE, values = zeta0,
                     lbound = 0.01, ubound = 0.9999, name = "zeta"),
    OpenMx::mxMatrix("Full", m + 1, 1, free = c(FALSE, rep(TRUE, m)),
                     values = w0, name = "w"),
    OpenMx::mxMatrix("Full", p, 1, free = FALSE, values = 1, name = "unit"),
    OpenMx::mxMatrix("Iden", p, name = "Ip"),
    OpenMx::mxAlgebra(exp(w) / sum(exp(w)), name = "beta"),
    OpenMx::mxAlgebra(theta %*% t(unit) - unit %*% t(theta), name = "Delta"),
    OpenMx::mxAlgebraFromString(c_expr, name = "C"),
    OpenMx::mxAlgebraFromString(sig_expr, name = "Sigma",
                                dimnames = list(nm, nm)),
    OpenMx::mxData(observed = R_obs, type = "cov", numObs = N),
    OpenMx::mxExpectationNormal(covariance = "Sigma"),
    OpenMx::mxFitFunctionML()
  )
  if (free_scaling) {
    # mxModel components are named and order-independent
    args <- c(args, list(OpenMx::mxMatrix("Full", p, 1, free = TRUE,
                                          values = 1, lbound = 0.1,
                                          name = "s")))
  }
  do.call(OpenMx::mxModel, args)
}

cpm_mx_run <- function(model) {
  fit <- suppressWarnings(suppressMessages(
    OpenMx::mxRun(model, silent = TRUE, suppressWarnings = TRUE)
  ))
  list(theta_deg = (OpenMx::mxEval(theta, fit)[, 1] %% (2 * pi)) * 180 / pi,
       zeta = OpenMx::mxEval(zeta, fit)[, 1],
       beta = OpenMx::mxEval(beta, fit)[, 1],
       Sigma = unname(OpenMx::mxEval(Sigma, fit)))
}

# ---- published oracle: discrepancy identity ----------------------------------

test_that("published oracle: our discrepancy reproduces CircE's F-hat at their solution", {
  voc <- cpm_oracle_voc()
  app <- cpm_oracle_voc_appendix()
  # Rebuild their fitted Sigma-hat = D_z (P_c + D_v) D_z from Appendix A.
  th <- app$theta * pi / 180
  Pc <- outer(th, th, function(a, b) cpm_rho(a - b, app$beta))
  diag(Pc) <- 1
  Sig <- diag(app$z) %*% (Pc + diag(app$v)) %*% diag(app$z)
  # Their published F-hat is OUR discrepancy function evaluated at their
  # Sigma-hat: the two programs share the ML discrepancy exactly.
  expect_equal(cpm_discrepancy(voc$R, Sig), app$Fhat, tolerance = 1e-4)
  # The fitted diagonal reproduces their published variance ratios -- the
  # free-scaling model difference, nailed to the published record.
  expect_equal(diag(Sig), app$var_ratios, tolerance = 5e-4,
               ignore_attr = TRUE)
  # Their communality index rho(x_i, c_i) (Browne, 1992, Eq. 4) is our zeta
  # (the index, NOT zeta^2; design sec. 6.5): rho(x, c) = 1/sqrt(1 + v)
  # reproduces their comm. ind. column.
  expect_equal(round(1 / sqrt(1 + app$v), 2), app$comm)
  # Their CIs on that index are NONsymmetric; what is symmetric is the
  # underlying interval on ln v_ii (Browne, 1982, pp. 95-96; stated on
  # p. 57), which the transform below inverts. A different CI shape from our
  # symmetric-natural zeta CIs, recorded per the sec. 6.3 checklist item 7.
  v_ci <- app$v[1] * exp(c(1, -1) * 1.96 * app$v_se[1] / app$v[1])
  expect_equal(round(1 / sqrt(1 + v_ci), 2), app$comm_ci[1, ])
})

# ---- published oracle: unconstrained m = 1 (Appendix A / Tables 2-3) ---------

test_that("published oracle: unconstrained m = 1 matches CircE/CIRCUM", {
  voc <- cpm_oracle_voc()
  app <- cpm_oracle_voc_appendix()
  fit <- cpm_oracle_voc_fit()

  # Superiority within our family (design sec. 6.3): the published optimum is
  # in the LARGER free-scaling family, so our diag-constrained F-hat must lie
  # between their F-hat and F at their scale-free parameters.
  zeta_pub <- 1 / sqrt(1 + app$v)
  P_pub <- cpm_implied_cor(app$theta * pi / 180, zeta_pub, app$beta)
  expect_gte(cpm_discrepancy(voc$R, P_pub) + 1e-8, fit$fit$F)
  expect_gte(fit$fit$F, app$Fhat - 5e-7)          # nesting direction
  expect_lt(fit$fit$F - app$Fhat, 0.01)           # model-difference allowance

  # Scale-free parameters, model-difference allowances (observed gaps at
  # N = 175: zeta <= .0031, beta .0015, angles <= 1.30 deg, T 1.07):
  expect_equal(fit$results$Zeta, zeta_pub, tolerance = 0.005,
               ignore_attr = TRUE)
  expect_equal(fit$betas$Beta, app$beta, tolerance = 0.005,
               ignore_attr = TRUE)
  expect_lt(abs(fit$corfun(180) - app$mcsc), 0.005)
  expect_lt(cpm_mirror_diff_deg(fit$results$Angle, app$theta), 1.5)
  expect_lt(abs(fit$fit$chisq - app$Tstat), 1.5)
  expect_equal(fit$fit$df, app$df)

  # Null model is convention-identical (independence): T0 matches exactly.
  expect_equal((voc$N - 1) * (-determinant(voc$R)$modulus[1]),
               app$null_chisq, tolerance = 0.01, ignore_attr = TRUE)

  # Analytic angle-CI half-widths vs their 1.96 * SE: same shape (symmetric),
  # free-scaling information difference <= 2 deg (observed max 1.92).
  half <- (fit$results$Angle_uci - fit$results$Angle_lci) / 2
  expect_lt(max(abs(half - 1.96 * app$theta_se)), 2)

  # SRMR convention (design sec. 6.3 item 6): their .04 is the
  # diagonal-inclusive p(p+1)/2 denominator; ours is off-diagonal-only, and
  # the diagonal residuals are 0, so ours * sqrt((p-1)/(p+1)) recovers theirs.
  expect_lt(abs(fit$fit$srmr * sqrt(6 / 8) - app$srmr), 0.005)
})

test_that("published oracle: our fit-index formulas reproduce CIRCUM's at their T", {
  voc <- cpm_oracle_voc()
  app <- cpm_oracle_voc_appendix()
  # Feed the published T through our index computations (Fhat = T / (N - 1)):
  # every derived index must reproduce the published output at its printed
  # precision. This isolates formula correctness from the model difference.
  idx <- cpm_fit_indices(app$Tstat / (voc$N - 1), app$df, 7, voc$N,
                         voc$R, diag(7), 14)
  expect_equal(round(idx$pvalue, 3), app$pvalue)
  expect_equal(round(idx$rmsea, 3), app$rmsea)
  expect_equal(round(idx$rmsea_ci, 3), app$rmsea_ci, ignore_attr = TRUE)
  expect_equal(round(idx$cfi, 3), app$cfi)
  expect_equal(round(idx$tli, 3), app$tli)
  # F0 and its CI are the RMSEA quantities on the discrepancy scale:
  # F0 = rmsea^2 * df, F0 CI = rmsea CI^2 * df. (The published F0 point of
  # .049 appears to be truncated, not rounded -- the value is .04958 -- so
  # the point gets an absolute band while the CI matches at 3 decimals.)
  expect_lt(abs(idx$rmsea^2 * app$df - app$F0), 0.001)
  expect_equal(round(idx$rmsea_ci^2 * app$df, 3), app$F0_ci,
               ignore_attr = TRUE)
})

# ---- published oracle: constrained variants and boundary rows ----------------

test_that("published oracle: equal-communality rows (Table 2 model 2b)", {
  voc <- cpm_oracle_voc()
  f1 <- cpm_fit(cormat = voc$R, scales = voc$names,
                angles = c(0, 48, 111, 121, 190, 208, 272), n = voc$N, m = 1,
                model = "equal-communality")
  # Published: beta (.628, .372), F .299 (Table 3), rho180 .26, and the
  # common communality index -- which Table 2 labels rho-hat_1, NOT zeta --
  # at .87. That column is our Zeta (design sec. 6.5), hence the comparison.
  expect_equal(f1$betas$Beta, c(.628, .372), tolerance = 0.005,
               ignore_attr = TRUE)
  expect_equal(f1$results$Zeta[1], .87, tolerance = 0.005)
  expect_lt(abs(f1$corfun(180) - .26), 0.005)
  expect_lt(cpm_mirror_diff_deg(f1$results$Angle,
                                c(0, 48, 111, 121, 190, 208, 272)), 3)
  expect_gte(f1$fit$F, .299 - 5e-4)               # nesting direction
  expect_lt(f1$fit$F - .299, 0.01)                # observed gap .0067

  f2 <- suppressWarnings(cpm_fit(cormat = voc$R, scales = voc$names,
                                 angles = c(0, 50, 111, 122, 190, 209, 272),
                                 n = voc$N, m = 2,
                                 model = "equal-communality"))
  # Published: beta (.619, .370, .011), communality index rho-hat_1 .88
  # (Table 2's column, not zeta), F .292.
  expect_equal(f2$betas$Beta, c(.619, .370, .011), tolerance = 0.005,
               ignore_attr = TRUE)
  expect_equal(f2$results$Zeta[1], .88, tolerance = 0.005)
  expect_lt(cpm_mirror_diff_deg(f2$results$Angle,
                                c(0, 50, 111, 122, 190, 209, 272)), 3)
  expect_gte(f2$fit$F, .292 - 5e-4)
  expect_lt(f2$fit$F - .292, 0.01)                # observed gap .006
})

test_that("published oracle: m = 2 reproduces CIRCUM's two Heywood cases", {
  voc <- cpm_oracle_voc()
  fit <- suppressWarnings(cpm_fit(cormat = voc$R, scales = voc$names,
                                  angles = voc$th_start, n = voc$N, m = 2))
  # Table 2 model 1a m = 2: beta (.608, .355, .038), communality index
  # rho-hat (.96, .83, 1, .77, .82, .94, 1) -- the same Table 2 column as
  # above, our Zeta -- with Technology and Social at the bound
  # (the paper: "the FS correlation function with m = 2 gave two Heywood
  # cases", p. 59, crediting Browne, 1992, p. 494 for the same finding).
  expect_equal(fit$betas$Beta, c(.608, .355, .038), tolerance = 0.005,
               ignore_attr = TRUE)
  expect_equal(fit$results$Zeta, c(.96, .83, 1, .77, .82, .94, 1),
               tolerance = 0.0075, ignore_attr = TRUE)
  expect_true(fit$details$heywood)
  expect_identical(which(fit$results$Zeta > 0.995), c(3L, 7L))
  # Table 3: F-hat .067.
  expect_gte(fit$fit$F, .067 - 5e-4)
  expect_lt(fit$fit$F - .067, 0.005)
  expect_lt(cpm_mirror_diff_deg(fit$results$Angle,
                                c(0, 52, 106, 117, 176, 192, 263)), 2.5)
})

test_that("published oracle: m = 3 drives beta_3 to the boundary as CIRCUM found", {
  voc <- cpm_oracle_voc()
  fit <- suppressWarnings(cpm_fit(cormat = voc$R, scales = voc$names,
                                  angles = voc$th_start, n = voc$N, m = 3))
  # "FS with m = 3 resulted in a correlation function weight estimate,
  # beta_3, attaining the lower bound of zero" (p. 59); Table 2 pins the
  # surviving weights at the m = 2 values. Our boundary polish removes the
  # vanishing harmonic and reports the reduced model (design sec. 3.5); note
  # the published df convention keeps the boundary parameter (sec. 6.3
  # checklist item 3), so only F and the surviving betas are compared.
  expect_true(3 %in% fit$details$removed_harmonics)
  expect_identical(fit$details$m, 2L)
  expect_identical(fit$betas$Beta[4], 0)
  expect_equal(fit$betas$Beta[1:3], c(.608, .355, .038), tolerance = 0.005,
               ignore_attr = TRUE)
  expect_gte(fit$fit$F, .067 - 5e-4)
  expect_lt(fit$fit$F - .067, 0.005)
})

test_that("published oracle: verbal-ability matrix and the m cap", {
  # Listing 7-8 (pp. 67-68): CircE's m = 5 attempt on p = 6 variables is
  # underidentified (negative df) and ends in a singular Hessian; our API
  # refuses over-cap m up front instead (design sec. 1.4).
  verbal <- cpm_oracle_verbal()
  expect_error(
    cpm_fit(cormat = verbal$R, scales = verbal$names, n = verbal$N, m = 5,
            angles = 360 * (0:5) / 6),
    "m"
  )
  # The in-cap fit on this published (non-circumplex, simplex-like) matrix
  # must still satisfy the engine's own acceptance criterion.
  fit <- suppressWarnings(cpm_fit(cormat = verbal$R, scales = verbal$names,
                                  n = verbal$N, m = 2,
                                  angles = 360 * (0:5) / 6))
  expect_true(fit$details$accepted)
})

# ---- cross-implementation oracles (Suggests only; design sec. 6.4) -----------

test_that("OpenMx oracle: diag-constrained m = 1 agrees to well under 1e-4", {
  skip_if_not_installed("OpenMx")
  skip_on_cran()
  voc <- cpm_oracle_voc()
  ours <- cpm_oracle_voc_fit()
  mx <- cpm_mx_run(cpm_mx_model(voc$R, voc$N, m = 1,
                                th0_rad = voc$th_start * pi / 180))
  # Independent optimizer (CSOLNP), independent model code, same discrepancy:
  # observed agreement 1.4e-5 deg / 7e-8 (zeta, beta) / dF 2.5e-14.
  expect_lt(max(cpm_angdiff_deg(mx$theta_deg, ours$results$Angle)), 1e-3)
  expect_equal(mx$zeta, ours$results$Zeta, tolerance = 1e-5,
               ignore_attr = TRUE)
  expect_equal(mx$beta, ours$betas$Beta, tolerance = 1e-5,
               ignore_attr = TRUE)
  expect_lt(abs(cpm_discrepancy(voc$R, mx$Sigma) - ours$fit$F), 1e-8)
})

test_that("OpenMx oracle: diag-constrained m = 3 agrees on a clean in-family R", {
  skip_if_not_installed("OpenMx")
  skip_on_cran()
  truth <- cpm_clean_truth()
  exact <- cpm_fit(cormat = cpm_implied_cor(
                     as.numeric(as_radian(as_degree(truth$angles))),
                     truth$zeta, truth$beta),
                   scales = paste0("V", 1:8), angles = truth$angles,
                   n = 20000, m = 3)
  set.seed(20260706)
  R <- stats::cor(cpm_simulate(exact, 20000))
  ours <- cpm_fit(cormat = R, scales = paste0("V", 1:8),
                  angles = truth$angles, n = 20000, m = 3)
  expect_false(ours$details$heywood)
  mx <- cpm_mx_run(cpm_mx_model(R, 20000, m = 3,
                                th0_rad = truth$angles * pi / 180,
                                zeta0 = 0.75))
  # 0.005 deg is ~9e-5 rad, inside the design's 1e-4 cross-implementation
  # target; CSOLNP's default stopping point lands ~1e-3 deg from our optimum
  expect_lt(max(cpm_angdiff_deg(mx$theta_deg, ours$results$Angle)), 5e-3)
  expect_equal(mx$zeta, ours$results$Zeta, tolerance = 1e-5,
               ignore_attr = TRUE)
  expect_equal(mx$beta, ours$betas$Beta, tolerance = 1e-5,
               ignore_attr = TRUE)
  expect_lt(abs(cpm_discrepancy(R, mx$Sigma) - ours$fit$F), 1e-8)
})

test_that("OpenMx oracle: the free-scaling model reproduces published CircE", {
  skip_if_not_installed("OpenMx")
  skip_on_cran()
  voc <- cpm_oracle_voc()
  app <- cpm_oracle_voc_appendix()
  mx <- cpm_mx_run(cpm_mx_model(voc$R, voc$N, m = 1,
                                th0_rad = voc$th_start * pi / 180,
                                free_scaling = TRUE))
  # Browne's covariance parameterization, independently optimized, lands on
  # the published CIRCUM/CircE output: zeta/beta to their 4 decimals, angles
  # to ~0.01 deg. This closes the model-difference attribution: our engine ==
  # OpenMx(diag-constrained); OpenMx(free-scaling) == published CIRCUM/CircE.
  expect_equal(mx$zeta, round(1 / sqrt(1 + app$v), 4), tolerance = 5e-4,
               ignore_attr = TRUE)
  expect_equal(mx$beta, app$beta, tolerance = 5e-4, ignore_attr = TRUE)
  expect_lt(cpm_mirror_diff_deg(mx$theta_deg, app$theta), 0.05)
  expect_equal(cpm_discrepancy(voc$R, mx$Sigma), app$Fhat, tolerance = 2e-3)

  # Equal-spacing row (Table 2 model 3c): fixed grid angles, free scaling.
  mxb <- cpm_mx_run(cpm_mx_model(voc$R, voc$N, m = 1,
                                 th0_rad = 2 * pi * (0:6) / 7,
                                 free_angles = FALSE, free_scaling = TRUE))
  expect_equal(mxb$beta, c(.704, .296), tolerance = 2e-3, ignore_attr = TRUE)
  expect_equal(cpm_discrepancy(voc$R, mxb$Sigma), .574, tolerance = 1e-3)
  # Per-variable zeta are NOT pinned: the symmetric fixed grid has near-tied
  # optima with cyclically related zeta patterns (equal F at published
  # precision); the published pattern is one basin, CSOLNP's another.

  # Our diag-constrained variant B on the same grid: nesting direction plus
  # a wider documented allowance (with angles fixed, the free scalings have
  # more leverage; observed gaps F .030, beta .020).
  fB <- cpm_fit(cormat = voc$R, scales = voc$names, angles = 360 * (0:6) / 7,
                n = voc$N, m = 1, model = "constrained-angles")
  expect_gte(fB$fit$F, .574 - 5e-4)
  expect_lt(fB$fit$F - .574, 0.05)
  expect_lt(max(abs(fB$betas$Beta - c(.704, .296))), 0.03)
  # And OpenMx agrees with our engine on our variant-B objective exactly.
  mxc <- cpm_mx_run(cpm_mx_model(voc$R, voc$N, m = 1,
                                 th0_rad = 2 * pi * (0:6) / 7,
                                 free_angles = FALSE))
  expect_equal(mxc$zeta, fB$results$Zeta, tolerance = 1e-4,
               ignore_attr = TRUE)
  expect_lt(abs(cpm_discrepancy(voc$R, mxc$Sigma) - fB$fit$F), 1e-7)
})

test_that("lavaan oracle: constrained 3-factor m = 1 lands on our optimum", {
  skip_if_not_installed("lavaan")
  skip_on_cran()
  voc <- cpm_oracle_voc()
  p <- 7
  ours <- cpm_oracle_voc_fit()
  # Factor form (design sec. 1.3): Lam = [zeta sqrt(b0), zeta sqrt(b1) cos th,
  # zeta sqrt(b1) sin th]; constraints: common b0/b1 ratio across items,
  # model-implied unit diagonal, rotation pinned by l31 == 0. Warm-started at
  # our solution (constrained SQP from cold starts is fragile); the assertion
  # is that an independent implementation cannot improve on, and stays at,
  # our optimum.
  l1 <- paste0("l1", seq_len(p))
  l2 <- paste0("l2", seq_len(p))
  l3 <- paste0("l3", seq_len(p))
  ps <- paste0("ps", seq_len(p))
  th_r <- ours$details$theta_rad
  ze <- ours$results$Zeta
  be <- ours$betas$Beta
  sv <- list(ze * sqrt(be[1]), ze * sqrt(be[2]) * cos(th_r),
             ze * sqrt(be[2]) * sin(th_r))
  lhs <- function(f, lab, start) {
    paste0(f, " =~ ", paste(sprintf("start(%.8f)*%s + %s*%s", start,
                                    voc$names, lab, voc$names),
                            collapse = " + "))
  }
  model <- c(
    lhs("G", l1, sv[[1]]), lhs("Fc", l2, sv[[2]]), lhs("Fs", l3, sv[[3]]),
    paste0(voc$names, " ~~ ", ps, "*", voc$names),
    "l31 == 0",
    paste0("l1", 2:p, "^2 * (l21^2 + l31^2) == l11^2 * (l2", 2:p,
           "^2 + l3", 2:p, "^2)"),
    paste0(ps, " == 1 - l1", seq_len(p), "^2 - l2", seq_len(p),
           "^2 - l3", seq_len(p), "^2")
  )
  fit <- lavaan::cfa(paste(model, collapse = "\n"), sample.cov = voc$R,
                     sample.nobs = voc$N, sample.cov.rescale = FALSE,
                     std.lv = TRUE, orthogonal = TRUE, se = "none")
  expect_true(lavaan::lavInspect(fit, "converged"))
  est <- lavaan::lavInspect(fit, "est")
  L <- est$lambda
  P_lav <- unname(L %*% t(L) + diag(diag(est$theta)))
  # Observed: dF 4e-7, zeta 3e-6, beta 1e-8, angles 1e-3 deg.
  expect_lt(abs(cpm_discrepancy(voc$R, P_lav) - ours$fit$F), 1e-5)
  expect_equal(sqrt(rowSums(L^2)), ours$results$Zeta, tolerance = 1e-4,
               ignore_attr = TRUE)
  expect_equal((L[1, 2]^2 + L[1, 3]^2) / sum(L[1, ]^2), ours$betas$Beta[2],
               tolerance = 1e-4)
  th_lav <- (atan2(L[, 3], L[, 2]) * 180 / pi) %% 360
  expect_lt(max(cpm_angdiff_deg(th_lav, ours$results$Angle)), 0.05)
})

# ---- simulation oracles (design sec. 6.4) ------------------------------------

test_that("sampling consistency: simulate large N, refit, recover gamma_0", {
  skip_on_cran()
  truth <- cpm_clean_truth()
  P0 <- cpm_implied_cor(as.numeric(as_radian(as_degree(truth$angles))),
                        truth$zeta, truth$beta)
  exact <- cpm_fit(cormat = P0, scales = paste0("V", 1:8),
                   angles = truth$angles, n = 50000, m = 3)
  set.seed(20260707)
  refit <- cpm_fit(cormat = stats::cor(cpm_simulate(exact, 50000)),
                   scales = paste0("V", 1:8), angles = truth$angles,
                   n = 50000, m = 3)
  # Loose tolerances: this catches wrong-model bugs (wrong-sign harmonics,
  # zeta/zeta^2 confusion, tens-of-degrees distortions), not sampling noise.
  # Even at this N and with the clean configuration, the weakly identified
  # near-rotation mode (Hessian condition ~1.5e3) leaves smooth correlated
  # angle errors of ~2.5 deg and zeta errors of ~.04 across seeds.
  expect_lt(max(cpm_angdiff_deg(refit$results$Angle, truth$angles %% 360)), 4)
  expect_lt(max(abs(refit$results$Zeta - truth$zeta)), 0.05)
  expect_lt(max(abs(refit$betas$Beta - truth$beta)), 0.03)
})

test_that("T-calibration: under in-family truth T = n * F-hat is chi-square(df)", {
  skip_on_cran()
  # A-review F1 / design sec. 6.4: at N = 2000 the test statistic must be
  # consistent with its chi-square reference at the fitted df. Uses the
  # well-identified truth so df is stable (no boundary polish) across
  # replicates; the coverage-oracle script (devel/m4-coverage-oracle.R)
  # repeats this at scale and at the hard octant-like truths.
  truth <- cpm_clean_truth()
  P0 <- cpm_implied_cor(as.numeric(as_radian(as_degree(truth$angles))),
                        truth$zeta, truth$beta)
  exact <- cpm_fit(cormat = P0, scales = paste0("V", 1:8),
                   angles = truth$angles, n = 2000, m = 3)
  set.seed(20260708)
  Tstat <- replicate(200, {
    R <- stats::cor(cpm_simulate(exact, 2000))
    # conditioning warnings are routine sampling behavior here; acceptance
    # and df stability still gate which replicates enter the KS check
    eng <- suppressWarnings(
      cpm_engine(R, angles = truth$angles, m = 3, variant = "A")
    )
    if (eng$accepted && length(eng$removed_harmonics) == 0) {
      1999 * eng$F
    } else {
      NA_real_
    }
  })
  Tstat <- Tstat[!is.na(Tstat)]
  expect_gt(length(Tstat), 180)
  ks <- stats::ks.test(Tstat, stats::pchisq, df = 10)
  expect_gt(ks$p.value, 0.01)
  # location sanity: mean of chi-square(10) is 10
  expect_equal(mean(Tstat), 10, tolerance = 0.1)
})

# ---- convention traps (design sec. 6.5) --------------------------------------

test_that("zeta is the communality index; Communality is its square", {
  voc <- cpm_oracle_voc()
  fit <- cpm_oracle_voc_fit()
  expect_identical(fit$results$Communality, fit$results$Zeta^2)
  # CircE's "communality index" column (rho(x_i, c_i)) is zeta, not zeta^2:
  # the published .93-.98 values match our Zeta (tested above) and are
  # incompatible with our Communality column.
  app <- cpm_oracle_voc_appendix()
  expect_gt(max(abs(fit$results$Communality - app$comm)), 0.05)
})

# ---- free-scaling family (M18): OUR engine vs the published/CircE oracles ----
# The payoff of scaling = "free": compare at SAME-MODEL tolerances (spec sec. 6),
# retiring the B6 model-difference allowances -- our covariance fit is the same
# estimand CIRCUM/CircE report. Two independent oracle types back every value:
# published program output (frozen; Grassi App. A) and an independent
# cross-implementation (live; the OpenMx free-scaling fit).

test_that("free-scaling frozen oracle: our engine reproduces Grassi App. A", {
  voc <- cpm_oracle_voc()
  app <- cpm_oracle_voc_appendix()
  fit <- cpm_fit(cormat = voc$R, scales = voc$names, angles = voc$th_start,
                 n = voc$N, m = 1, scaling = "free")
  # Angles to published precision (mirror-aware, reference-relative; sec. 6.5).
  expect_lt(cpm_mirror_diff_deg(fit$results$Angle, app$theta), 0.01)
  # Communality index zeta = 1/sqrt(1 + v) to their 4 printed decimals.
  expect_equal(fit$results$Zeta, round(1 / sqrt(1 + app$v), 4),
               tolerance = 5e-4, ignore_attr = TRUE)
  # Correlation-function weights to 4 decimals.
  expect_equal(fit$betas$Beta, app$beta, tolerance = 5e-4, ignore_attr = TRUE)
  # Variance ratios sigma^2 = reproduced/input variance (Appendix A .963-1.042).
  expect_equal(fit$results$VarRatio, app$var_ratios, tolerance = 5e-4,
               ignore_attr = TRUE)
  # Discrepancy and the full fit-index set, now at same-model tolerances.
  expect_equal(fit$fit$F, app$Fhat, tolerance = 1e-4)
  expect_equal(fit$fit$chisq, app$Tstat, tolerance = 5e-3)
  expect_equal(fit$fit$df, app$df)
  expect_lt(abs(fit$fit$pvalue - app$pvalue), 1e-3)   # published to 3 dp
  # RMSEA .0842 -> published .084 (spec sec. 4); published to 3 dp, so compare
  # within half a printed unit (absolute), like the RMSEA-CI below.
  expect_lt(abs(fit$fit$rmsea - app$rmsea), 1e-3)
  expect_lt(max(abs(fit$fit$rmsea_ci - app$rmsea_ci)), 1e-3)
  expect_equal(fit$fit$cfi, app$cfi, tolerance = 1e-3)
  expect_equal(fit$fit$tli, app$tli, tolerance = 1e-3)
  # SRMR: our off-diagonal convention converted to CircE's diagonal-inclusive
  # value (spec sec. 4). The free family's diagonal residuals are 1 - sigma^2.
  # Published SRMR is printed to 2 dp, so compare within half a printed unit.
  p <- 7
  srmr_circe <- sqrt((p * (p - 1) / 2 * fit$fit$srmr^2 +
                        sum((1 - fit$results$VarRatio)^2)) / (p * (p + 1) / 2))
  expect_lt(abs(srmr_circe - app$srmr), 5e-3)
})

test_that("free-scaling live oracle: our engine agrees with OpenMx free-scaling", {
  skip_if_not_installed("OpenMx")
  skip_on_cran()
  voc <- cpm_oracle_voc()
  fit <- cpm_fit(cormat = voc$R, scales = voc$names, angles = voc$th_start,
                 n = voc$N, m = 1, scaling = "free")
  mx <- cpm_mx_run(cpm_mx_model(voc$R, voc$N, m = 1,
                                th0_rad = voc$th_start * pi / 180,
                                free_scaling = TRUE))
  # Independent optimizer (CSOLNP), independent model code, same covariance
  # discrepancy: agreement to publication precision.
  expect_lt(cpm_mirror_diff_deg(fit$results$Angle, mx$theta_deg), 0.02)
  expect_equal(fit$results$Zeta, mx$zeta, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(fit$betas$Beta, mx$beta, tolerance = 1e-3, ignore_attr = TRUE)
  # sigma^2 vs diag(Sigma_mx): OpenMx fits R un-premultiplied on the free path
  # (the family is closed under rescaling), so its ML (N-1)/N rescale of the
  # observed matrix is absorbed ENTIRELY into sigma-hat -- diag(Sigma_mx) =
  # VarRatio * (N-1)/N -- while theta/zeta/beta/F stay invariant (asserted
  # above). A clean cross-implementation confirmation of the rescale-
  # equivariance property. cpm_discrepancy at Sigma_mx matches our F to CSOLNP's
  # optimizer tail (its p free scales + p zeta stop a hair short of our optimum).
  expect_equal(fit$results$VarRatio * (voc$N - 1) / voc$N, diag(mx$Sigma),
               tolerance = 1e-3, ignore_attr = TRUE)
  expect_lt(abs(cpm_discrepancy(voc$R, mx$Sigma) - fit$fit$F), 5e-4)
})

test_that("free-scaling: fixed-grid (variant B) reproduces Table 2 model 3c", {
  # Table 2 model 3c: equally spaced angles, free scaling. Our variant-B free
  # fit lands on the published beta and F at same-model tolerances (the B6
  # model-difference allowance that variant B needed against the diag family is
  # retired for its own free-scaling comparison).
  voc <- cpm_oracle_voc()
  fB <- cpm_fit(cormat = voc$R, scales = voc$names, angles = 360 * (0:6) / 7,
                n = voc$N, m = 1, model = "constrained-angles", scaling = "free")
  expect_equal(fB$betas$Beta, c(.704, .296), tolerance = 2e-3, ignore_attr = TRUE)
  expect_equal(fB$fit$F, .574, tolerance = 1e-3)
})

# ---- free-scaling analytic-CI coverage oracle (M19) -------------------------
#
# The heavy oracle (devel/m4-coverage-oracle.R stage 3, CPM_COV_FREE_ONLY=1;
# 500 reps, recorded in DESIGN.md and devel/m19-free-coverage-results.rds)
# validates the free family's analytic (Wald) CI coverage. These two in-suite
# tests are its fast reproductions: (1) a small seeded coverage smoke that
# catches a broken SE (which would tank coverage), and (2) an INDEPENDENT
# oracle type for the SE that feeds those CIs -- a live parametric-bootstrap SE
# cross-check -- so the coverage claim meets the >=2-oracle-types bar
# (simulation-coverage + live). D-010.

# signed shortest rotation a -> b in degrees, in (-180, 180]
cpm_ang_signed <- function(a, b) -((a - b + 180) %% 360 - 180)

test_that("free-scaling coverage smoke: interior N=2000 analytic CIs cover in-band", {
  skip_on_cran()
  # Interior correlation truth (the DESIGN.md free record's interior cell:
  # sigma_pop = 1, so this is a pure circumplex correlation). Recorded full-run
  # coverage at N = 2000 is angle .928 / zeta .954 / beta .948; a small seeded
  # reproduction must land in-band. A broken free SE would collapse coverage.
  angles <- octants()
  arad <- as.numeric(as_radian(as_degree(angles)))
  zeta <- rep(0.75, 8L); beta <- c(.35, .30, .20, .15)
  P0 <- cpm_implied_cor(arad, zeta, beta)
  U <- chol(P0)
  z <- stats::qnorm(0.975)
  N <- 2000L; reps <- 80L
  cov <- matrix(NA, reps, 3L, dimnames = list(NULL, c("angle", "zeta", "beta")))
  for (i in seq_len(reps)) {
    set.seed(20260713L + i)
    X <- matrix(stats::rnorm(N * 8L), N) %*% U
    R <- stats::cor(X)
    eng <- suppressWarnings(cpm_engine(R, angles = angles, m = 3,
                                       variant = "A", scaling = "free"))
    if (!isTRUE(eng$accepted)) next
    se <- tryCatch(suppressWarnings(cpm_analytic_se(eng, R, N)),
                   error = function(e) NULL)
    if (is.null(se) || anyNA(se$angle) || anyNA(se$zeta)) next
    fp <- eng$spec$free_pos
    cov[i, "angle"] <- mean(abs(cpm_ang_signed(eng$theta[fp], angles[fp] %% 360))
                            <= z * se$angle[fp])
    cov[i, "zeta"] <- mean(abs(eng$zeta - zeta) <= z * se$zeta)
    cov[i, "beta"] <- mean(abs(eng$beta - beta) <= z * se$beta)
  }
  rates <- colMeans(cov, na.rm = TRUE)
  # In-band with generous Monte-Carlo slack (recorded ~.93-.95 at 500 reps): a
  # working SE keeps every type comfortably above .85; a broken one falls far
  # below. Upper fence guards against a degenerate always-cover SE.
  expect_gt(min(rates), 0.85)
  expect_lt(max(rates), 0.995)
  # The free bordered Hessian is well-conditioned at N = 2000 (DESIGN.md:
  # ~0% SE-failure), so nearly every replicate is usable.
  expect_gt(sum(!is.na(cov[, "zeta"])), 0.9 * reps)
})

test_that("free-scaling SE cross-check: analytic Wald SE agrees with parametric bootstrap (live oracle)", {
  skip_on_cran()
  # Second, INDEPENDENT oracle type for the SEs behind the free-family CIs: draw
  # one interior dataset, fit free, take the FD-Hessian analytic SE, then a
  # parametric bootstrap (refit free on data drawn from the fitted model) as a
  # fully independent SE estimate. At a clean interior N they must agree to
  # sampling error -- validating the SE machinery the coverage oracle relies on.
  angles <- octants()
  arad <- as.numeric(as_radian(as_degree(angles)))
  zeta <- rep(0.75, 8L); beta <- c(.35, .30, .20, .15)
  P0 <- cpm_implied_cor(arad, zeta, beta)
  N <- 2000L
  set.seed(424242L)
  X <- matrix(stats::rnorm(N * 8L), N) %*% chol(P0)
  R <- stats::cor(X)
  eng <- suppressWarnings(cpm_engine(R, angles = angles, m = 3, variant = "A",
                                     scaling = "free"))
  se <- cpm_analytic_se(eng, R, N)
  expect_false(anyNA(se$zeta))
  # Parametric bootstrap SE from the fitted model Sigma-hat (= eng$P).
  Uhat <- chol(eng$P)
  B <- 200L
  fp <- eng$spec$free_pos
  th <- matrix(NA, B, length(fp)); zt <- matrix(NA, B, 8L)
  bt <- matrix(NA, B, length(eng$beta))
  for (b in seq_len(B)) {
    set.seed(70000L + b)
    Xb <- matrix(stats::rnorm(N * 8L), N) %*% Uhat
    eb <- suppressWarnings(tryCatch(
      cpm_engine(stats::cor(Xb), angles = angles, m = 3, variant = "A",
                 scaling = "free"),
      error = function(e) NULL))
    if (is.null(eb) || !isTRUE(eb$accepted)) next
    th[b, ] <- cpm_ang_signed(eb$theta[fp], eng$theta[fp])  # residual vs fit
    zt[b, ] <- eb$zeta
    bt[b, ] <- eb$beta
  }
  se_boot_angle <- apply(th, 2, stats::sd, na.rm = TRUE)
  se_boot_zeta <- apply(zt, 2, stats::sd, na.rm = TRUE)
  se_boot_beta <- apply(bt, 2, stats::sd, na.rm = TRUE)
  keep <- se$beta > 0                                  # kept harmonics only
  # Median ratio analytic/bootstrap per type; agree to sampling error at B=200.
  # A broken analytic SE would be off by a factor, not ~1.
  r_ang <- stats::median(se$angle[fp] / se_boot_angle)
  r_zeta <- stats::median(se$zeta / se_boot_zeta)
  r_beta <- stats::median(se$beta[keep] / se_boot_beta[keep])
  expect_gt(min(r_ang, r_zeta, r_beta), 0.7)
  expect_lt(max(r_ang, r_zeta, r_beta), 1.4)
})
