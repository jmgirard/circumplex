# M21/T1: T_diag-vs-T_free calibration comparison (D-009 item 3; spec sec. 9
# "measure now, decide later" -- this is the "decide" half's evidence).
#
# Question: which family's test statistic T = (N - 1) * F-hat is better
# calibrated to its nominal chi-square_df reference at a true circumplex
# correlation model -- the shipped unit-scaling ("diag") family or the M18
# free-scaling family? Better calibration would make that family the
# preferable default for the MODEL TEST (fit chisq/p-value); this is the
# deferred inference-default decision D-009 routed to a future D-entry.
#
# Design: PAIRED, unlike the committed stage-1/stage-3 oracle runs (disjoint
# seeds, one family each). Each replicate draws one X ~ N(0, P0), computes
# R = cor(X) once, and fits BOTH engines to the same R. df is identical by
# construction (spec sec. 4: free adds p sigma parameters AND p diagonal
# moments; both families use df = n_moments - q), so T_unit and T_free target
# the same chi-square_df and the per-replicate difference T_free - T_unit is
# directly interpretable (T_free <= T_unit always: the free family's extra
# nuisance freedom absorbs part of the same misfit).
#
# Provenance: truths, configs, and REPS mirror devel/m4-coverage-oracle.R
# (stages 1/3); the committed summary KS p-values live in
# devel/m4-coverage-oracle-results.rds (diag, N = 250/500/1000, df = 10) and
# devel/m19-free-coverage-results.rds (free, 12 cells) and are echoed in the
# output for cross-reference. Engine-only fits (no SEs, no bootstrap), so the
# full 12-cell x 500-rep x 2-family run is session-runnable (minutes).
#
# Seeds: BASE_SEED + 12e7 + cell/rep offsets -- disjoint from stage 1 (no
# offset), stage 2 (5e7), and stage 3 (8e7). Cell terms index the LEVEL,
# never the raw value (M19 lesson: 1e3*N aliased two cells).
#
# Usage:  Rscript devel/m21-t-calibration.R                 # full run
#         CPM_T_SMOKE=1 Rscript devel/m21-t-calibration.R   # quick smoke
#         CPM_T_VARIANT=C CPM_T_SMOKE=1 Rscript ...  # non-A variant spot check
#           (RR05 R6 belt-and-suspenders; the equal-communality truths are
#           inside variant C, so C is the natural non-A check)

devtools::load_all(".", quiet = TRUE)

smoke <- nzchar(Sys.getenv("CPM_T_SMOKE"))
VARIANT <- Sys.getenv("CPM_T_VARIANT", "A")
REPS <- if (smoke) 25 else 500
NS_ALL <- if (smoke) c(250, 2000) else c(250, 1000, 2000, 5000, 20000, 50000)
BASE_SEED <- 20260706
OFFSET <- 12e7
CORES <- max(1, parallel::detectCores() - 1)

p <- 8
angles <- octants()                     # degrees, LM = 360
angles_rad <- as.numeric(as_radian(as_degree(angles)))

configs <- list(                        # identical to the oracle stages
  boundary = list(zeta = rep(0.75, p), beta = c(.45, .35, .15, .05)),
  interior = list(zeta = rep(0.75, p), beta = c(.35, .30, .20, .15))
)

# One paired replicate: same R, both engines. NULL on a data/fit error;
# non-acceptance and polishing are recorded, filtered downstream (the KS
# convention from stage 1 is unpolished-only -- a removed harmonic changes q
# and df, so polished replicates answer a different calibration question).
pair_one <- function(i, cfg_idx, N_idx, N, chol0) {
  try(silent = TRUE, {
    set.seed(BASE_SEED + OFFSET + 1e6 * cfg_idx + 1e4 * N_idx + i)
    X <- matrix(stats::rnorm(N * p), nrow = N) %*% chol0     # Cov = P0
    R <- stats::cor(X)
    eu <- suppressWarnings(cpm_engine(R, angles = angles, m = 3,
                                      variant = VARIANT, scaling = "unit"))
    ef <- suppressWarnings(cpm_engine(R, angles = angles, m = 3,
                                      variant = VARIANT, scaling = "free"))
    list(
      ok_u = isTRUE(eu$accepted), ok_f = isTRUE(ef$accepted),
      pol_u = length(eu$removed_harmonics) > 0,
      pol_f = length(ef$removed_harmonics) > 0,
      T_u = (N - 1) * eu$F, T_f = (N - 1) * ef$F,
      df_u = eu$df, df_f = ef$df
    )
  })
}

# Calibration summary for one family's T vector against chi-square_df.
calib <- function(Ts, df) {
  n <- length(Ts)
  rej <- mean(Ts > stats::qchisq(0.95, df))
  # Wilson 95% interval for the rejection rate
  z <- stats::qnorm(0.975)
  ctr <- (rej + z^2 / (2 * n)) / (1 + z^2 / n)
  hw <- z * sqrt(rej * (1 - rej) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  ks <- stats::ks.test(Ts, stats::pchisq, df = df)
  c(n = n, mean_ratio = mean(Ts) / df, var_ratio = stats::var(Ts) / (2 * df),
    rej05 = rej, rej05_lo = ctr - hw, rej05_hi = ctr + hw,
    ks_D = unname(ks$statistic), ks_p = ks$p.value,
    q95_emp = unname(stats::quantile(Ts, 0.95)) / stats::qchisq(0.95, df))
}

cat("M21 T-calibration; variant =", VARIANT, "; reps =", REPS, "; cells =",
    length(configs) * length(NS_ALL), "; cores =", CORES, "\n")

results <- list()
for (cfg_idx in seq_along(configs)) {
  cfg_name <- names(configs)[cfg_idx]
  cfg <- configs[[cfg_idx]]
  P0 <- cpm_implied_cor(angles_rad, cfg$zeta, cfg$beta)
  chol0 <- chol(P0)
  for (N_idx in seq_along(NS_ALL)) {
    N <- NS_ALL[N_idx]
    reps <- parallel::mclapply(
      seq_len(REPS), pair_one, cfg_idx = cfg_idx, N_idx = N_idx, N = N,
      chol0 = chol0, mc.cores = CORES
    )
    err <- vapply(reps, function(r) inherits(r, "try-error") || is.null(r),
                  logical(1))
    reps <- reps[!err]
    # paired keep: both accepted, both unpolished, equal df
    keep <- vapply(reps, function(r) {
      r$ok_u && r$ok_f && !r$pol_u && !r$pol_f && r$df_u == r$df_f
    }, logical(1))
    used <- reps[keep]
    T_u <- vapply(used, `[[`, numeric(1), "T_u")
    T_f <- vapply(used, `[[`, numeric(1), "T_f")
    df <- used[[1]]$df_u
    cell <- sprintf("%s_N%d", cfg_name, N)
    results[[cell]] <- list(
      n_total = REPS, n_error = sum(err), n_excluded = sum(!keep),
      df = df,
      unit = calib(T_u, df), free = calib(T_f, df),
      paired = c(mean_diff = mean(T_f - T_u), max_diff = max(T_f - T_u),
                 min_diff = min(T_f - T_u), cor = stats::cor(T_u, T_f),
                 # free nests unit (sigma = 1 recovers it), so T_f <= T_u up
                 # to optimizer tolerance; count tail violations for the record
                 n_viol = sum(T_f > T_u + 1e-6)),
      T_unit = T_u, T_free = T_f          # per-replicate vectors (RB evidence)
    )
    s <- results[[cell]]
    cat(sprintf(
      paste0("%-16s df=%2d n=%3d | unit: mean/df %.3f rej05 %.3f ks_p %.3f",
             " | free: mean/df %.3f rej05 %.3f ks_p %.3f | mdiff %.2f\n"),
      cell, df, s$unit["n"], s$unit["mean_ratio"], s$unit["rej05"],
      s$unit["ks_p"], s$free["mean_ratio"], s$free["rej05"], s$free["ks_p"],
      s$paired["mean_diff"]
    ))
  }
}

# Echo the committed one-family summary KS p-values for cross-reference.
echo_committed <- function() {
  out <- list()
  f19 <- "devel/m19-free-coverage-results.rds"
  f04 <- "devel/m4-coverage-oracle-results.rds"
  if (file.exists(f19)) {
    x <- readRDS(f19)
    out$free_stage3_ks_T <- vapply(x$results, `[[`, numeric(1), "ks_T")
  }
  if (file.exists(f04)) {
    x <- readRDS(f04)
    out$diag_stage1_ks_T <- vapply(x$results, `[[`, numeric(1), "ks_T")
  }
  out
}

out <- list(results = results, reps = REPS, ns = NS_ALL, configs = configs,
            base_seed = BASE_SEED, offset = OFFSET, variant = VARIANT,
            committed = echo_committed(), date = Sys.Date())
suffix <- if (VARIANT == "A") "" else paste0("-", VARIANT)
path <- if (smoke) paste0("devel/m21-t-calibration-smoke", suffix, ".rds") else
  paste0("devel/m21-t-calibration-results", suffix, ".rds")
saveRDS(out, path)
cat("saved:", path, "\n")
