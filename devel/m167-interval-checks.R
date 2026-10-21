# M167 T2: checks that the interval prototypes in devel/m167-interval-helpers.R
# compute what their sources define, before the oracle relies on them.
#
#   1. F1's zeta arm reproduces CircE's published communality-index intervals
#      (Grassi et al. 2010, Appendix A; the reconstruction at
#      tests/testthat/test-cpm_oracles.R:136), for all seven rows.
#   2. The B1 harness copy (m167_boot_reps + m167_percentile) gives intervals
#      identical to cpm_fit()'s shipped bootstrap at the same seed.
#   3. F2's Wishart draw matches a row-level parametric bootstrap (the
#      test-cpm_oracles.R cross-check's construction) in replicate SD, and
#      both agree with the analytic SE at an interior N = 2000 fit within the
#      test file's [0.7, 1.4] ratio bar.
#   4. The bias-corrected form gives the closed-form answer on a normal
#      sample: with replicates N(mu, s^2) and estimate mu + c s, z0 = c and
#      the endpoints are mu + s (2 c -/+ 1.96).
#   4b. The basic bootstrap gives its closed form on a normal sample.
#   4c. P1 (profile likelihood) matches the Wald interval at an interior
#      N = 20000 fit, and a gradient-free re-profile puts the statistic at
#      3.84 at its endpoints.
#   4d. P1's finite endpoints at boundary N = 250 (mostly Heywood fits) are
#      not false: re-profiled from the estimate, the statistic there is not
#      clearly below 3.84.
#   5. The angle side rule scores hand-built arcs correctly.
#
# Usage: Rscript devel/m167-interval-checks.R   (about 2 minutes)
# Prints PASS/FAIL per check and exits non-zero on any FAIL.

devtools::load_all(".", quiet = TRUE)
source(file.path("devel", "m167-interval-helpers.R"))
source(file.path("tests", "testthat", "helper-cpm-oracles.R"))

fails <- 0L
report <- function(name, ok, detail = "") {
  cat(sprintf("%s  %s %s\n", if (ok) "PASS" else "FAIL", name, detail))
  if (!ok) fails <<- fails + 1L
}

# ---- 1. F1 zeta arm vs CircE ---------------------------------------------------
app <- cpm_oracle_voc_appendix()
zeta <- 1 / sqrt(1 + app$v)
se_zeta <- app$v_se * 0.5 * (1 + app$v)^(-3 / 2)     # delta method, v -> zeta
ci1 <- m167_lnv_ci(zeta, se_zeta)
got <- round(ci1, 2)
report("F1 zeta arm reproduces CircE comm. ind. CIs (7 rows)",
       isTRUE(all.equal(unname(got), unname(app$comm_ci))),
       paste(apply(got, 1, paste, collapse = "-"), collapse = " "))

# ---- shared truth --------------------------------------------------------------
angles <- octants()
scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
arad <- as.numeric(as_radian(as_degree(angles)))
P_int <- cpm_implied_cor(arad, rep(0.75, 8), c(.35, .30, .20, .15))
P_bnd <- cpm_implied_cor(arad, rep(0.75, 8), c(.45, .35, .15, .05))

# ---- 2. B1 harness copy equals cpm_fit()'s bootstrap ---------------------------
same <- logical(0)
for (s in 1:3) {
  set.seed(9100 + s)
  X <- matrix(stats::rnorm(400 * 8), 400) %*% chol(P_bnd)
  colnames(X) <- scales
  set.seed(500 + s)
  fit <- suppressWarnings(cpm_fit(data = as.data.frame(X), scales = scales,
                                  angles = angles, m = 3, boots = 150))
  set.seed(500 + s)
  eng <- suppressWarnings(cpm_engine(stats::cor(X), angles = angles, m = 3,
                                     variant = "A"))
  ci <- m167_percentile(suppressWarnings(m167_boot_reps(eng, X, 150)))
  same <- c(same,
            identical(unname(ci$angle), unname(cbind(fit$results$Angle_lci,
                                                     fit$results$Angle_uci))),
            identical(unname(ci$zeta), unname(cbind(fit$results$Zeta_lci,
                                                    fit$results$Zeta_uci))),
            identical(unname(ci$beta), unname(cbind(fit$betas$Beta_lci,
                                                    fit$betas$Beta_uci))))
}
report("B1 harness intervals identical to cpm_fit() (3 datasets x 3 families)",
       all(same), sprintf("%d of %d identical", sum(same), length(same)))

# ---- 3. F2 Wishart draw vs row draw vs analytic SE -----------------------------
N <- 2000L
set.seed(424242L)
X <- matrix(stats::rnorm(N * 8L), N) %*% chol(P_int)
R <- stats::cor(X)
eng <- suppressWarnings(cpm_engine(R, angles = angles, m = 3, variant = "A"))
se <- cpm_analytic_se(eng, R, N)
B <- 300L
set.seed(71000L)
f2 <- suppressWarnings(m167_param_reps(eng, N, B))
Uhat <- chol(eng$P)
row_reps <- m167_refit_reps(eng, B, function(b) {
  stats::cor(matrix(stats::rnorm(N * 8L), N) %*% Uhat)
})
fp <- eng$spec$free_pos
circ_sd <- function(th) {
  apply(th, 2, function(x) {
    d <- ((x - stats::median(x) + pi) %% (2 * pi)) - pi
    stats::sd(d) * 180 / pi
  })
}
sd_w <- list(angle = circ_sd(f2$theta)[fp], zeta = apply(f2$zeta, 2, stats::sd),
             beta = apply(f2$beta, 2, stats::sd))
sd_r <- list(angle = circ_sd(row_reps$theta)[fp],
             zeta = apply(row_reps$zeta, 2, stats::sd),
             beta = apply(row_reps$beta, 2, stats::sd))
r_wr <- vapply(names(sd_w), function(f) stats::median(sd_w[[f]] / sd_r[[f]]),
               numeric(1))
report("F2 Wishart vs row-draw replicate SD, median ratio per family in [0.8, 1.25]",
       all(r_wr > 0.8 & r_wr < 1.25),
       paste(sprintf("%s %.3f", names(r_wr), r_wr), collapse = ", "))
r_wa <- c(angle = stats::median(se$angle[fp] / sd_w$angle),
          zeta = stats::median(se$zeta / sd_w$zeta),
          beta = stats::median(se$beta / sd_w$beta))
report("analytic SE vs F2 replicate SD, median ratio per family in [0.7, 1.4]",
       all(r_wa > 0.7 & r_wa < 1.4),
       paste(sprintf("%s %.3f", names(r_wa), r_wa), collapse = ", "))

# ---- 4. bias-corrected form, closed-form normal case ---------------------------
mu <- 2; s <- 0.5; cc <- 0.4
reps <- stats::qnorm(stats::ppoints(1e5), mu, s)
bc <- m167_bc_one(reps, mu + cc * s)
want <- mu + s * (2 * cc + c(-1, 1) * stats::qnorm(0.975))
report("bias-corrected interval equals mu + s (2c -/+ 1.96) on a normal sample",
       max(abs(bc - want)) < 2e-3,
       sprintf("got %.4f %.4f, want %.4f %.4f", bc[1], bc[2], want[1], want[2]))
bc0 <- m167_bc_one(reps, mu)
pc <- stats::quantile(reps, c(.025, .975), names = FALSE)
report("bias-corrected interval equals the percentile interval when z0 = 0",
       max(abs(bc0 - pc)) < 1e-4)

# ---- 4b. basic bootstrap, closed-form normal case ------------------------------
# with replicates N(mu, s^2) and estimate mu + c s the basic interval is
# [2 (mu + c s) - (mu + 1.96 s), 2 (mu + c s) - (mu - 1.96 s)]
reps_obj <- list(n_ok = 1e5, zeta = matrix(reps, ncol = 1),
                 beta = matrix(reps, ncol = 1))
bb <- m167_basic(reps_obj, list(zeta = mu + cc * s, beta = mu + cc * s))
want_b <- mu + s * (2 * cc + c(-1, 1) * stats::qnorm(0.975))
report("basic interval equals 2 est - percentile endpoints on a normal sample",
       max(abs(bb$zeta[1, ] - want_b)) < 2e-3,
       sprintf("got %.4f %.4f", bb$zeta[1, 1], bb$zeta[1, 2]))

# ---- 4c. P1 agrees with Wald at large N ----------------------------------------
# Profile-likelihood and Wald intervals are asymptotically equivalent, so at
# an interior N = 20000 fit their endpoints agree closely; the Wald interval
# here is an independent construction (Hessian SE) of the same quantity.
N <- 20000L
set.seed(5L)
X <- matrix(stats::rnorm(N * 8L), N) %*% chol(P_int)
R <- stats::cor(X)
eng <- suppressWarnings(cpm_engine(R, angles = angles, m = 3, variant = "A"))
se <- cpm_analytic_se(eng, R, N)
pr <- m167_profile_ci(eng, R, N)
wd <- m167_wald(eng, se)
d_p1 <- max(abs(c(pr$zeta - wd$zeta, pr$beta - wd$beta)))
report("P1 endpoints within .002 of Wald at interior N = 20000 (zeta and beta)",
       d_p1 < 0.002, sprintf("max difference %.5f", d_p1))
# the statistic at each P1 endpoint equals the cutoff (re-profiled directly)
ldR <- as.numeric(determinant(R, logarithm = TRUE)$modulus)
F_hat <- cpm_objective(eng$par, R, eng$spec, ldR = ldR)
idx <- eng$spec$i_zeta[1]
lr_at <- function(t) {
  f <- stats::nlminb(eng$par[-idx], function(x) {
    cpm_objective(append(x, stats::qlogis(t), idx - 1L), R, eng$spec, ldR)
  }, control = list(rel.tol = 1e-12))
  (N - 1) * (f$objective - F_hat)
}
lr_ends <- vapply(pr$zeta[1, ], lr_at, numeric(1))
report("LR at P1's zeta_1 endpoints equals 3.84 within .05 (gradient-free refit)",
       max(abs(lr_ends - stats::qchisq(.95, 1))) < 0.05,
       sprintf("%.3f %.3f", lr_ends[1], lr_ends[2]))

# ---- 4d. P1 endpoints in the Heywood regime ------------------------------------
# At boundary N = 250 most fits are Heywood fits, where a warm-started profile
# once stalled and gave false endpoints (M167 review). Re-profile each finite
# P1 endpoint from the estimate with a gradient-free minimizer: the statistic
# there must not fall clearly below the cutoff. `p1_fun` lets a plant swap in
# another P1 implementation.
p1_endpoint_check <- function(p1_fun, n_fits = 12L) {
  crit <- stats::qchisq(.95, 1)
  low <- 0L
  tested <- 0L
  heywood_tested <- 0L
  for (i in seq_len(n_fits)) {
    set.seed(88000L + i)
    X <- matrix(stats::rnorm(250 * 8L), 250) %*% chol(P_bnd)
    R <- stats::cor(X)
    eng <- suppressWarnings(cpm_engine(R, angles = angles, m = 3,
                                       variant = "A"))
    if (!isTRUE(eng$accepted)) next
    pr <- p1_fun(eng, R, 250L)
    setup <- m167_profile_setup(eng, R, 250L)
    for (comp in setup$comps) {
      ends <- pr[[comp$family]][comp$row, ]
      for (t in ends) {
        if (is.na(t) || t <= 1e-6 || t >= 1 - 1e-6) next   # natural bounds
        st <- m167_profile_stat(setup, comp, stats::qlogis(t),
                                use_gradient = FALSE)$stat
        tested <- tested + 1L
        if (comp$family == "zeta" && eng$zeta[comp$row] > 0.995) {
          heywood_tested <- heywood_tested + 1L
        }
        if (st < crit - 0.05) low <- low + 1L
      }
    }
  }
  c(tested = tested, heywood_tested = heywood_tested, low = low)
}
hc <- p1_endpoint_check(m167_profile_ci)
report("P1 endpoints at boundary N = 250: none with the statistic below 3.79",
       hc[["low"]] == 0L && hc[["heywood_tested"]] > 0L,
       sprintf("%d endpoints tested (%d on Heywood communalities), %d low",
               hc[["tested"]], hc[["heywood_tested"]], hc[["low"]]))

# ---- 5. angle side rule --------------------------------------------------------
arcs <- rbind(c(350, 10),    # straddles 0/360, truth 0 inside
              c(20, 40),     # ccw of truth 0
              c(300, 340),   # cw of truth 0
              c(-30, 400),   # Wald wider than a full turn
              c(170, 190))   # truth 0 opposite: midpoint 180 -> treated as cw
got <- m167_side_angle(arcs, 0)
report("angle side rule on hand-built arcs", identical(got, c(0, 1, -1, 0, -1)),
       paste(got, collapse = " "))
lin <- m167_side_linear(rbind(c(.1, .2), c(.3, .4), c(.0, .05), c(NA, 1)), .25)
report("linear side rule", identical(lin, c(-1, 1, -1, NA)),
       paste(lin, collapse = " "))

cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails > 0) 1L else 0L)
