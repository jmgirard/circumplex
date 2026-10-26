# M167 T2: checks that the interval prototypes in devel/m167-interval-helpers.R
# compute what their sources define, before the oracle relies on them.
#
#   1. F1's zeta arm reproduces CircE's published communality-index intervals
#      (Grassi et al. 2010, Appendix A; the reconstruction at
#      tests/testthat/test-cpm_oracles.R:138-143), for all seven rows.
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
#      neither too narrow nor too wide: an independent search (its own anchor
#      and its own dispersed starts and probes, RR27 Q4) puts the statistic
#      between 3.79 and 3.89 at every one, and P1's anchor no more than .05
#      T units above the check's (a lower P1 anchor is the better reference). The two-start P1 that M167's third review found too
#      narrow is the plant and must fail.
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
# At boundary N = 250 most fits are Heywood fits, and lower basins exist that
# a profile started from few points misses (M167 reviews 2 and 3, RR27). The
# check runs its own search, independent of P1's starts and seed: an anchor
# from 36 dispersed free refits around the engine optimum (sd .7), a wall
# probe per scale and +/-30 degree angle probes; then, at every finite P1
# endpoint, a re-profile from 12 starts at sd .5 and 12 at sd .8 around the
# estimate's free parameters, the same probes, the check's own anchor
# solution and P1's own endpoint solution. The statistic is taken against the
# lower of the check's anchor and P1's, F evaluated at the parameters P1
# reports, so a P1 that out-searches the check is not read as too narrow.
# Too narrow (below 3.79), too wide (above 3.89) and a P1 anchor more than
# .05 T units above the check's all fail. `p1_fun` lets the plant swap in.
p1_endpoint_check <- function(p1_fun, n_fits = 12L) {
  crit <- stats::qchisq(.95, 1)
  N <- 250L
  out <- c(fits = 0L, tested = 0L, heywood_tested = 0L, low = 0L, high = 0L,
           anchor_off = 0L, unverified = 0L)
  for (i in seq_len(n_fits)) {
    set.seed(88000L + i)
    X <- matrix(stats::rnorm(N * 8L), N) %*% chol(P_bnd)
    R <- stats::cor(X)
    eng <- suppressWarnings(cpm_engine(R, angles = angles, m = 3,
                                       variant = "A"))
    if (!isTRUE(eng$accepted)) next
    out[["fits"]] <- out[["fits"]] + 1L
    pr <- suppressWarnings(p1_fun(eng, R, N))
    if (!is.null(pr$diag)) {
      out[["unverified"]] <- out[["unverified"]] +
        sum(pr$diag$components$unverified)
    }
    spec <- eng$spec
    q <- length(eng$par)
    probe <- function(g) {
      c(lapply(spec$i_zeta, function(k) {
        h <- g
        h[k] <- stats::qlogis(0.99)
        h
      }), unlist(lapply(spec$i_angle, function(k) {
        lapply(c(30, -30), function(d) {
          h <- g
          h[k] <- h[k] + d * pi / 180
          h
        })
      }), recursive = FALSE))
    }
    set.seed(99000L + i)
    anchor_starts <- c(lapply(1:36, function(k) {
      eng$par + stats::rnorm(q, 0, 0.7)
    }), probe(eng$par))
    best <- list(par = eng$par, F = eng$F)
    for (st in anchor_starts) {
      f <- tryCatch(cpm_optimize_one(st, R, spec), error = function(e) NULL)
      if (!is.null(f) && is.finite(f$F) && f$F < best$F) best <- f
    }
    setup <- m167_profile_setup(eng, R, N)
    # P1's anchor, evaluated at the parameters P1 reports (the engine
    # optimum for the plant): a check anchor below it means P1 missed the
    # global minimum; a P1 anchor below the check's is the better reference
    F_p1 <- if (is.null(pr$diag)) eng$F else setup$obj(pr$diag$anchor_g)
    if (setup$n * (F_p1 - best$F) > 0.05) {
      out[["anchor_off"]] <- out[["anchor_off"]] + 1L
    }
    setup$F_hat <- min(best$F, F_p1)
    for (j in seq_along(setup$comps)) {
      comp <- setup$comps[[j]]
      ends <- pr[[comp$family]][comp$row, ]
      p1_end <- if (is.null(pr$diag)) NULL else pr$diag$end_g[[j]]
      for (side in 1:2) {
        t <- ends[side]
        if (is.na(t) || t <= 1e-6 || t >= 1 - 1e-6) next   # natural bounds
        s <- stats::qlogis(t)
        g_est <- comp$full(comp$x0, s)
        # P1's own endpoint solution is a start too, so a reading above the
        # cutoff means P1's own point is above it, not that the check's
        # search fell short
        sts <- c(lapply(1:12, function(k) comp$x0 + stats::rnorm(length(comp$x0), 0, 0.5)),
                 lapply(1:12, function(k) comp$x0 + stats::rnorm(length(comp$x0), 0, 0.8)),
                 lapply(probe(g_est), comp$split),
                 list(comp$split(best$par)),
                 if (!is.null(p1_end[[side]])) list(comp$split(p1_end[[side]])))
        st <- m167_profile_stat(setup, comp, s, starts = sts)$raw
        out[["tested"]] <- out[["tested"]] + 1L
        if (comp$family == "zeta" && eng$zeta[comp$row] > 0.995) {
          out[["heywood_tested"]] <- out[["heywood_tested"]] + 1L
        }
        if (st < crit - 0.05) out[["low"]] <- out[["low"]] + 1L
        if (st > crit + 0.05) out[["high"]] <- out[["high"]] + 1L
      }
    }
  }
  out
}
check_detail <- function(hc) {
  sprintf(paste("%d fits, %d endpoints tested (%d on Heywood communalities),",
                "%d low, %d high, %d anchors off, %d components unverified"),
          hc[["fits"]], hc[["tested"]], hc[["heywood_tested"]], hc[["low"]],
          hc[["high"]], hc[["anchor_off"]], hc[["unverified"]])
}
hc <- p1_endpoint_check(function(eng, R, N) m167_profile_ci(eng, R, N, seed = 1L))
report("P1 endpoints at boundary N = 250: all within [3.79, 3.89], anchors agree",
       hc[["low"]] == 0L && hc[["high"]] == 0L && hc[["anchor_off"]] == 0L &&
         hc[["heywood_tested"]] > 0L,
       check_detail(hc))
hp <- p1_endpoint_check(m167_profile_ci_twostart)
report("plant: the two-start P1 fails the same check",
       hp[["low"]] > 0L || hp[["high"]] > 0L || hp[["anchor_off"]] > 0L,
       check_detail(hp))

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
