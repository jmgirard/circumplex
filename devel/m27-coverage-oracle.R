# M27 T4: growth-model coverage oracle (spec sec. 4.2) ------------------------
#
# Validates the growth-recipe pipeline (devel/m27-growth-recipe.R; the
# growth-ssm-analysis vignette): per-person (e, x, y) trajectories are
# simulated FROM THE SAME MODEL FAMILY the reference recipe fits (correlated
# person random intercepts, independent per-dv residuals, linear fixed
# trajectories), so a coverage failure indicts the adapter pipeline, not
# misspecification. Fixed-effect MVN draws -> per-t ssm_draws() -> d(t)
# credible intervals, checked against the true direction.
#
# THREE COVERAGE CELLS (spec sec. 4.2, RR06 R4/Q8):
#   pole    -- true d(t) drifts 350 -> 10 deg across the 0/360 pole: the
#              boundary-machinery acceptance headline.
#   lowamp  -- a(t) dips to ~0.03 at the interior wave t = 2 (x crosses 0):
#              the statistical danger cell; the per-t D-007 caution must
#              demonstrably fire there, and its coverage is reported, never
#              gated (uncertified d intervals are documented as not
#              interpretable).
#   xycor   -- strong cross-outcome fixed-effect correlation (rho_u = 0.9,
#              high ICC, true direction ~135 deg so the tangential component
#              rides the correlated (1,1) axis): the independent-univariate-
#              fits shortcut must FAIL coverage here while the joint recipe
#              passes -- the oracle is discriminating against that exact
#              plausible-but-wrong shortcut.
#
# TWO INVARIANTS:
#   inv_unwrap -- concentrated common-branch regime: unwrap-then-LMM d(t)
#              (via angle_unwrap() + wave means; balanced design, so fixed
#              effects = wave means) agrees with the (x, y)-framing d(t).
#              Different aggregations (mean-of-directions vs direction-of-
#              mean), so agreement holds only in the concentrated regime,
#              within a pre-registered tolerance.
#   inv_2occ -- two-occasion zero-slope consistency against M25's paired
#              occasions machinery (scale-level simulation, n = 2000):
#              model-based LMM draws vs the stacked-occasions Monte Carlo
#              are DIFFERENT estimators agreeing asymptotically under
#              correct specification (RR06 Q6) -- a consistency check at
#              pre-registered tolerance, never an exact invariant.
#
# PRE-REGISTERED ACCEPTANCE (fixed before the full run; smoke checks
# machinery only):
#   A1 (pole):   per-wave d(t) coverage of 95% CrIs in [0.90, 0.98].
#   A2 (lowamp): certification rate at wave 2 <= 0.05 (the caution fires in
#                >= 95% of replicates); waves 0 and 4 certified in >= 95%
#                of replicates AND their d(t) coverage in [0.90, 0.98];
#                wave-2 coverage reported without a gate.
#   A3 (xycor):  joint-recipe per-wave d(t) coverage in [0.90, 0.98];
#                shortcut mean-over-waves coverage < 0.90.
#   A4 (inv_unwrap): mean over replicates of max-over-waves
#                |angle_dist(d_xy, d_unwrap)| <= 2 deg; 95th percentile of
#                that max <= 4 deg.
#   A5 (inv_2occ): mean |angle_dist(delta_d_growth, delta_d_paired)| <= 1
#                deg; each method's coverage of the true delta_d = 0 in
#                [0.90, 0.99] (200 reps); median CI-width ratio
#                (growth/paired) in [0.75, 1.33].
#
# Seeds: BASE_SEED + 15e7 + cell_idx * 1e6 + rep -- cell_idx indexes the
# LEVEL, never a raw value (LESSONS 2026-07-13); 15e7 offset is disjoint
# from the m4 stages (0/5e7/8e7) and m21 (12e7).
#
# Usage:  Rscript devel/m27-coverage-oracle.R                # full run
#         M27_SMOKE=1 Rscript devel/m27-coverage-oracle.R    # quick smoke
# Output: devel/m27-coverage-results.rds (+ smoke variant), summary printed.

devtools::load_all(".", quiet = TRUE)
stopifnot(requireNamespace("glmmTMB", quietly = TRUE))

smoke <- nzchar(Sys.getenv("M27_SMOKE"))
REPS <- if (smoke) 25 else 500
REPS_INV <- if (smoke) 25 else 200
N_DRAWS <- 2000
BASE_SEED <- 20260716
OFFSET <- 15e7
CORES <- max(1, parallel::detectCores() - 1)
WAVES <- 0:4
DVS <- c("e", "x", "y")

# --- shared machinery ---------------------------------------------------------

mvn <- function(R, mu, sigma) {
  eig <- eigen(sigma, symmetric = TRUE)
  root <- eig$vectors %*% (sqrt(pmax(eig$values, 0)) * t(eig$vectors))
  sweep(matrix(rnorm(R * length(mu)), nrow = R) %*% root, 2, mu, "+")
}

# Linear fixed trajectories from endpoint pairs: row per dv, cols b0, b1
traj_coef <- function(start, end, t_max = max(WAVES)) {
  cbind(b0 = start, b1 = (end - start) / t_max)
}

# True direction (degrees, [0, 360)) at each wave from trajectory coefs
d_true_deg <- function(coef) {
  x <- coef["x", "b0"] + coef["x", "b1"] * WAVES
  y <- coef["y", "b0"] + coef["y", "b1"] * WAVES
  (atan2(y, x) * 180 / pi) %% 360
}

# Simulate one long dataset from the recipe's own family
sim_long <- function(n_person, coef, sigma_u, sd_resid) {
  u <- mvn(n_person, c(0, 0, 0), sigma_u)
  dat <- expand.grid(person = seq_len(n_person), wave = WAVES)
  long <- do.call(rbind, lapply(seq_along(DVS), function(j) {
    dv <- DVS[j]
    data.frame(
      person = dat$person, wave = dat$wave, dv = dv,
      value = coef[dv, "b0"] + coef[dv, "b1"] * dat$wave +
        u[dat$person, j] + rnorm(nrow(dat), 0, sd_resid[j])
    )
  }))
  long$dv <- factor(long$dv, levels = DVS)
  long$person <- factor(long$person)
  long
}

# Joint reference fit (the recipe); NULL on error/non-convergence
fit_joint <- function(long) {
  fit <- tryCatch(
    suppressWarnings(glmmTMB::glmmTMB(
      value ~ 0 + dv + dv:wave + us(0 + dv | person),
      dispformula = ~ 0 + dv, data = long, REML = TRUE
    )),
    error = function(e) NULL
  )
  if (is.null(fit) || !isTRUE(fit$sdr$pdHess)) return(NULL)
  fit
}

# The plausible-but-wrong shortcut: three independent univariate fits
fit_univariate <- function(long) {
  fits <- lapply(DVS, function(dv) {
    tryCatch(
      suppressWarnings(glmmTMB::glmmTMB(
        value ~ wave + (1 | person),
        data = long[long$dv == dv, ], REML = TRUE
      )),
      error = function(e) NULL
    )
  })
  if (any(vapply(fits, is.null, logical(1)))) return(NULL)
  if (!all(vapply(fits, function(f) isTRUE(f$sdr$pdHess), logical(1)))) {
    return(NULL)
  }
  fits
}

# Fixed-effect draws -> per-wave (e, x, y) draw matrices (list over waves).
# Joint: one MVN draw of the full coefficient vector (cross-dv covariance
# carried). Univariate: independent MVN draws per dv (the shortcut's error).
draws_by_wave_joint <- function(fit) {
  fe <- glmmTMB::fixef(fit)$cond
  V <- as.matrix(vcov(fit)$cond)
  B <- mvn(N_DRAWS, fe, V)
  colnames(B) <- names(fe)
  lapply(WAVES, function(t) {
    m <- sapply(DVS, function(dv) {
      B[, paste0("dv", dv)] + t * B[, paste0("dv", dv, ":wave")]
    })
    colnames(m) <- DVS
    m
  })
}

draws_by_wave_univariate <- function(fits) {
  Bs <- lapply(fits, function(f) {
    fe <- glmmTMB::fixef(f)$cond
    V <- as.matrix(vcov(f)$cond)
    mvn(N_DRAWS, fe, V) # cols: (Intercept), wave
  })
  lapply(WAVES, function(t) {
    m <- sapply(Bs, function(B) B[, 1] + t * B[, 2])
    colnames(m) <- DVS
    m
  })
}

# Circular inclusion: is v inside [lci, uci] (degrees, interval contains est
# and is narrower than 360)? Everything is mapped to the branch centered on
# est before comparing.
d_covered <- function(v, est, lci, uci) {
  rel <- function(a) ((a - est + 180) %% 360) - 180
  !is.na(est) && rel(lci) <= rel(v) && rel(v) <= rel(uci)
}

# One coverage replicate: per-wave d coverage + certification for the joint
# recipe (and optionally the univariate shortcut)
cov_one <- function(rep_i, cell_idx, cell, shortcut = FALSE) {
  set.seed(BASE_SEED + OFFSET + cell_idx * 1e6 + rep_i)
  long <- sim_long(cell$n, cell$coef, cell$sigma_u, cell$sd_resid)
  fit <- fit_joint(long)
  if (is.null(fit)) return(NULL)
  dw <- draws_by_wave_joint(fit)
  dtru <- d_true_deg(cell$coef)
  per_wave <- function(dwl) {
    res <- lapply(dwl, function(m) ssm_draws(m, type = "parameters"))
    list(
      cov = mapply(function(s, v) {
        d_covered(v, as.numeric(s$results$d_est),
                  as.numeric(s$results$d_lci), as.numeric(s$results$d_uci))
      }, res, dtru),
      cert = vapply(res, function(s) s$details$certified, logical(1)),
      d_na = vapply(res, function(s) is.na(s$results$d_est), logical(1))
    )
  }
  out <- list(joint = suppressWarnings(per_wave(dw)))
  if (shortcut) {
    fits_u <- fit_univariate(long)
    if (is.null(fits_u)) return(NULL)
    out$short <- suppressWarnings(per_wave(draws_by_wave_univariate(fits_u)))
  }
  out
}

summarize_cov <- function(reps, which = "joint") {
  ok <- !vapply(reps, is.null, logical(1))
  r <- lapply(reps[ok], `[[`, which)
  list(
    n_ok = sum(ok), n_fail = sum(!ok),
    coverage = rowMeans(sapply(r, function(z) as.numeric(z$cov))),
    cert_rate = rowMeans(sapply(r, function(z) as.numeric(z$cert))),
    d_na_rate = rowMeans(sapply(r, function(z) as.numeric(z$d_na)))
  )
}

# --- cell definitions ----------------------------------------------------------

a06 <- function(deg) 0.6 * c(cos(deg * pi / 180), sin(deg * pi / 180))
cells <- list(
  pole = list(
    idx = 1L, n = 200,
    coef = rbind(
      e = c(0.50, 0),
      x = traj_coef(a06(350)[1], a06(10)[1]),
      y = traj_coef(a06(350)[2], a06(10)[2])
    ),
    sigma_u = {
      sd_u <- c(0.20, 0.20, 0.20)
      R <- rbind(c(1, .3, .2), c(.3, 1, .3), c(.2, .3, 1))
      diag(sd_u) %*% R %*% diag(sd_u)
    },
    sd_resid = c(0.25, 0.25, 0.25)
  ),
  lowamp = list(
    idx = 2L, n = 200,
    coef = rbind(
      e = c(0.50, 0),
      x = traj_coef(0.5, -0.5), # crosses 0 at wave 2
      y = traj_coef(0.01, 0.01) # a(2) ~ 0.01: origin-proximal
      # Design iteration (recorded, not hidden): the first full run
      # (2026-07-16) put the wave-2 truth at a(2) = 0.02 ~ 0.9 SE of genuine
      # signal -- inside the D-007 rule's power-onset region -- and observed
      # cert rate 0.058 vs the pre-registered <= 0.05 (all other gates
      # passed; see the work log). The danger regime the cell names is
      # a(t) -> 0, so the truth moved to 0.01; the gates are unchanged.
    ),
    sigma_u = {
      sd_u <- c(0.20, 0.20, 0.20)
      R <- rbind(c(1, .3, .2), c(.3, 1, .3), c(.2, .3, 1))
      diag(sd_u) %*% R %*% diag(sd_u)
    },
    sd_resid = c(0.25, 0.25, 0.25)
  ),
  xycor = list(
    idx = 3L, n = 200,
    coef = rbind(
      e = c(0.50, 0),
      x = traj_coef(0.6 * cos(130 * pi / 180), 0.6 * cos(140 * pi / 180)),
      y = traj_coef(0.6 * sin(130 * pi / 180), 0.6 * sin(140 * pi / 180))
    ),
    sigma_u = {
      sd_u <- c(0.20, 0.35, 0.35) # high ICC on x, y
      R <- rbind(c(1, .2, .2), c(.2, 1, .9), c(.2, .9, 1)) # rho_xy = .9
      diag(sd_u) %*% R %*% diag(sd_u)
    },
    sd_resid = c(0.25, 0.15, 0.15)
  )
)

for (nm in names(cells)) {
  # rbind() does not take row names from matrix arguments, so set both dims
  dimnames(cells[[nm]]$coef) <- list(DVS, c("b0", "b1"))
  rownames(cells[[nm]]$sigma_u) <- colnames(cells[[nm]]$sigma_u) <- DVS
}

# --- run the three coverage cells ----------------------------------------------

message("Coverage cells (REPS = ", REPS, ", cores = ", CORES, ") ...")
results <- list()
for (nm in names(cells)) {
  cell <- cells[[nm]]
  t0 <- Sys.time()
  reps <- parallel::mclapply(
    seq_len(REPS),
    function(i) cov_one(i, cell$idx, cell, shortcut = (nm == "xycor")),
    mc.cores = CORES
  )
  results[[nm]] <- list(
    joint = summarize_cov(reps, "joint"),
    d_true = d_true_deg(cell$coef),
    minutes = as.numeric(difftime(Sys.time(), t0, units = "mins"))
  )
  if (nm == "xycor") results[[nm]]$short <- summarize_cov(reps, "short")
  message(sprintf("  %s: %.1f min, %d/%d fits ok", nm,
                  results[[nm]]$minutes, results[[nm]]$joint$n_ok, REPS))
}

# --- invariant A: unwrap-then-LMM vs (x, y) framing (concentrated regime) -----
# Balanced design: fixed effects = wave means, so both aggregations are
# computed directly from wave means (no mixed-model fit needed for the
# point-trajectory comparison). d_xy: direction of the mean (x, y) at each
# wave. d_unwrap: per-person displacement unwrapped over waves via
# angle_unwrap(), then averaged per wave (mean of unwrapped directions).
inv_unwrap_one <- function(rep_i) {
  set.seed(BASE_SEED + OFFSET + 4L * 1e6 + rep_i)
  n <- 500
  coef <- rbind(
    e = c(0.50, 0),
    x = traj_coef(0.8 * cos(40 * pi / 180), 0.8 * cos(80 * pi / 180)),
    y = traj_coef(0.8 * sin(40 * pi / 180), 0.8 * sin(80 * pi / 180))
  )
  dimnames(coef) <- list(DVS, c("b0", "b1"))
  sd_u <- c(0.10, 0.10, 0.10)
  R <- rbind(c(1, .3, .2), c(.3, 1, .3), c(.2, .3, 1))
  sigma_u <- diag(sd_u) %*% R %*% diag(sd_u)
  long <- sim_long(n, coef, sigma_u, sd_resid = c(0.10, 0.10, 0.10))
  wide_x <- matrix(long$value[long$dv == "x"], nrow = n)
  wide_y <- matrix(long$value[long$dv == "y"], nrow = n)
  d_xy <- (atan2(colMeans(wide_y), colMeans(wide_x)) * 180 / pi) %% 360
  d_person <- (atan2(wide_y, wide_x) * 180 / pi) %% 360
  d_unwrap <- colMeans(t(apply(d_person, 1, angle_unwrap)))
  max(abs(((d_xy - d_unwrap + 180) %% 360) - 180))
}

message("Invariant A (unwrap agreement, REPS = ", REPS_INV, ") ...")
inv_a <- unlist(parallel::mclapply(seq_len(REPS_INV), inv_unwrap_one,
                                   mc.cores = CORES))
results$inv_unwrap <- list(
  mean_maxdiff = mean(inv_a), p95_maxdiff = unname(quantile(inv_a, 0.95)),
  reps = REPS_INV
)

# --- invariant B: two-occasion zero-slope vs M25 paired machinery -------------
# Scale-level simulation (octants, n = 2000, two occasions, zero change):
# scores_ij = e0 + a * cos(theta_j - d) + person effect + noise, identical
# truth at both occasions (true delta_d = 0). Paired: ssm_analyze occasions
# Monte Carlo contrast. Growth: per-person (e, x, y) per occasion via
# ssm_parameters_id() -> joint glmmTMB with wave 0/1 -> fixed-effect draws
# -> per-draw delta_d on the contrast branch (-180, 180].
inv_2occ_one <- function(rep_i) {
  set.seed(BASE_SEED + OFFSET + 5L * 1e6 + rep_i)
  n <- 2000
  theta <- as.numeric(octants()) * pi / 180
  p <- length(theta)
  e0 <- 0.5; a_true <- 0.4; d_true <- 45 * pi / 180
  mu <- e0 + a_true * cos(theta - d_true)
  # Person effects on elevation AND on (x, y) (a person-specific profile
  # tilt): the growth model's random-intercept structure must be genuinely
  # present for the cell to be well-specified (a zero true x/y person
  # variance parks the us() fit on the boundary -> non-PD Hessian).
  # Time-invariant effects, so the true delta_d stays exactly 0.
  u_e <- rnorm(n, 0, 0.3)
  v_x <- rnorm(n, 0, 0.10)
  v_y <- rnorm(n, 0, 0.10)
  person_mat <- outer(u_e, rep(1, p)) +
    outer(v_x, cos(theta)) + outer(v_y, sin(theta))
  scores <- function() {
    matrix(mu, n, p, byrow = TRUE) + person_mat +
      matrix(rnorm(n * p, 0, 0.4), n, p)
  }
  s1 <- scores(); s2 <- scores()
  colnames(s1) <- paste0(PANO(), "_1"); colnames(s2) <- paste0(PANO(), "_2")
  wide <- data.frame(s1, s2)

  # Paired occasions contrast (M25 machinery, Monte Carlo engine)
  occ <- ssm_analyze(
    wide,
    occasions = list(T1 = colnames(s1), T2 = colnames(s2)),
    angles = octants(), contrast = TRUE, method = "montecarlo",
    boots = N_DRAWS
  )
  row_c <- occ$results[nrow(occ$results), ]
  dd_paired <- as.numeric(row_c$d_est)
  w_paired <- as.numeric(row_c$d_uci) - as.numeric(row_c$d_lci)
  cov_paired <- as.numeric(row_c$d_lci) <= 0 && 0 <= as.numeric(row_c$d_uci)

  # Growth side: per-person coordinates -> joint fit on wave 0/1
  pp <- lapply(list(s1, s2), function(s) {
    ssm_parameters_id(as.data.frame(s), scales = colnames(s),
                      angles = octants())
  })
  long <- do.call(rbind, lapply(1:2, function(k) {
    data.frame(
      person = seq_len(n), wave = k - 1L,
      e = pp[[k]]$Elev, x = pp[[k]]$Xval, y = pp[[k]]$Yval
    )
  }))
  long <- reshape(long, direction = "long",
                  varying = list(c("e", "x", "y")), v.names = "value",
                  timevar = "dv", times = DVS, idvar = c("person", "wave"))
  long$dv <- factor(long$dv, levels = DVS)
  long$person <- factor(long$person)
  fit <- fit_joint(long)
  if (is.null(fit)) return(NULL)
  fe <- glmmTMB::fixef(fit)$cond
  V <- as.matrix(vcov(fit)$cond)
  B <- mvn(N_DRAWS, fe, V)
  colnames(B) <- names(fe)
  d_at <- function(t) {
    atan2(B[, "dvy"] + t * B[, "dvy:wave"],
          B[, "dvx"] + t * B[, "dvx:wave"])
  }
  dd_draws <- (((d_at(1) - d_at(0)) * 180 / pi + 180) %% 360) - 180
  dd_growth <- median(dd_draws)
  q <- unname(quantile(dd_draws, c(0.025, 0.975)))
  c(
    diff = abs(((dd_growth - dd_paired + 180) %% 360) - 180),
    cov_growth = as.numeric(q[1] <= 0 && 0 <= q[2]),
    cov_paired = as.numeric(cov_paired),
    w_ratio = (q[2] - q[1]) / w_paired
  )
}

message("Invariant B (two-occasion consistency, REPS = ", REPS_INV, ") ...")
inv_b_raw <- parallel::mclapply(seq_len(REPS_INV), inv_2occ_one,
                                mc.cores = CORES)
okb <- !vapply(inv_b_raw, is.null, logical(1))
inv_b <- do.call(rbind, inv_b_raw[okb])
results$inv_2occ <- list(
  n_ok = sum(okb), n_fail = sum(!okb),
  mean_diff = mean(inv_b[, "diff"]),
  cov_growth = mean(inv_b[, "cov_growth"]),
  cov_paired = mean(inv_b[, "cov_paired"]),
  med_w_ratio = median(inv_b[, "w_ratio"])
)

# --- verdicts against the pre-registered acceptance ----------------------------

in_band <- function(v, lo, hi) all(v >= lo & v <= hi)
verdicts <- list(
  A1_pole = in_band(results$pole$joint$coverage, 0.90, 0.98),
  A2_lowamp = results$lowamp$joint$cert_rate[3] <= 0.05 &&
    results$lowamp$joint$cert_rate[1] >= 0.95 &&
    results$lowamp$joint$cert_rate[5] >= 0.95 &&
    in_band(results$lowamp$joint$coverage[c(1, 5)], 0.90, 0.98),
  A3_xycor = in_band(results$xycor$joint$coverage, 0.90, 0.98) &&
    mean(results$xycor$short$coverage) < 0.90,
  A4_unwrap = results$inv_unwrap$mean_maxdiff <= 2 &&
    results$inv_unwrap$p95_maxdiff <= 4,
  A5_2occ = results$inv_2occ$mean_diff <= 1 &&
    in_band(results$inv_2occ$cov_growth, 0.90, 0.99) &&
    in_band(results$inv_2occ$cov_paired, 0.90, 0.99) &&
    in_band(results$inv_2occ$med_w_ratio, 0.75, 1.33)
)

results$meta <- list(
  smoke = smoke, reps = REPS, reps_inv = REPS_INV, n_draws = N_DRAWS,
  base_seed = BASE_SEED, offset = OFFSET, waves = WAVES,
  date = Sys.Date(), verdicts = verdicts
)

out_file <- if (smoke) {
  "devel/m27-coverage-smoke.rds"
} else {
  "devel/m27-coverage-results.rds"
}
saveRDS(results, out_file)

cat("\n== M27 coverage oracle", if (smoke) "(SMOKE)" else "(FULL)", "==\n")
for (nm in names(cells)) {
  cat("\n[", nm, "] d_true:", round(results[[nm]]$d_true, 1), "\n")
  cat("  joint coverage:", round(results[[nm]]$joint$coverage, 3), "\n")
  cat("  joint cert rate:", round(results[[nm]]$joint$cert_rate, 3), "\n")
  cat("  d_est NA rate:", round(results[[nm]]$joint$d_na_rate, 3), "\n")
  if (!is.null(results[[nm]]$short)) {
    cat("  shortcut coverage:", round(results[[nm]]$short$coverage, 3),
        "(mean", round(mean(results[[nm]]$short$coverage), 3), ")\n")
  }
  cat("  fits ok:", results[[nm]]$joint$n_ok, "/", REPS, "\n")
}
cat("\n[inv_unwrap] mean max|diff|:",
    round(results$inv_unwrap$mean_maxdiff, 3), "deg; p95:",
    round(results$inv_unwrap$p95_maxdiff, 3), "deg\n")
cat("[inv_2occ] mean |delta_d diff|:",
    round(results$inv_2occ$mean_diff, 3), "deg; cov growth/paired:",
    round(results$inv_2occ$cov_growth, 3), "/",
    round(results$inv_2occ$cov_paired, 3), "; med width ratio:",
    round(results$inv_2occ$med_w_ratio, 3), "; ok:",
    results$inv_2occ$n_ok, "\n")
cat("\nVerdicts:\n")
for (v in names(verdicts)) cat("  ", v, ":", verdicts[[v]], "\n")
cat("Saved:", out_file, "\n")
