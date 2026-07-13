# M4/B6: CPM coverage oracle (design devel/m4-browne-design.md sec. 6.4;
# A-review F1 -- "the test that separates 'matches CIRCUM' from 'actually
# covers'").
#
# Simulates data from known CPM truths, fits with cpm_fit(), and measures the
# empirical coverage of the nominal-95% confidence intervals for BOTH CI
# methods on every replicate:
#   * bootstrap percentile (the shipped raw-data default), and
#   * analytic Wald (the cormat-path default), from the same engine fit.
# Also collects T = n * F-hat per replicate for the at-scale T-calibration
# (KS against chi-square at the fitted df, unpolished replicates only).
#
# Acceptance (design sec. 6.4): the DEFAULT method's coverage must lie in
# [.90, .98] at every N and parameter; the analytic method's measured coverage
# calibrates the N-conditional summary() caution. Results are recorded in
# DESIGN.md (they justify the bootstrap-default decision) and the run is
# invoked by /statistical-validation, never by R CMD check (cost: ~5 CPU-hours
# at the default settings; ~45-60 min on 8 cores).
#
# Reproducibility: every replicate derives its own seed as BASE_SEED + a
# cell/replicate offset and runs set.seed() locally, so results are identical
# for any mc.cores and scheduling.
#
# Usage:  Rscript devel/m4-coverage-oracle.R          # full run
#         CPM_COV_SMOKE=1 Rscript devel/m4-coverage-oracle.R   # ~2 min smoke
#         CPM_COV_CELLS=boundary_N250 Rscript ...      # re-run named cell(s)
#
# Record note: the committed 2026-07-06 stage-1/stage-2 results were produced
# by this script BEFORE the B6-review hardening (whole-worker try() with
# error accounting; span-based angle_covered). That run completed with zero
# worker errors (all cells report n/500), so the try() change could not have
# altered it; the angle_covered change was quantified by re-running the
# most-affected cell (boundary_N250) under the span rule: every coverage
# number reproduced to all printed decimals (2026-07-07; see DESIGN.md).

devtools::load_all(".", quiet = TRUE)

smoke <- nzchar(Sys.getenv("CPM_COV_SMOKE"))
# Stage 3 (free-scaling analytic-CI coverage, M19) is appended below and runs
# after stages 1-2 in a full pass. Set CPM_COV_FREE_ONLY=1 to run ONLY stage 3
# without re-running the ~5 CPU-hour diag stages (their results are already
# committed): the M19 oracle is analytic-only and session-runnable on its own.
free_only <- nzchar(Sys.getenv("CPM_COV_FREE_ONLY"))
REPS <- if (smoke) 25 else 500          # replications per cell
BOOTS <- if (smoke) 200 else 1000       # bootstrap replicates per fit
NS <- c(250, 500, 1000)                 # sample sizes (design sec. 6.4)
BASE_SEED <- 20260706
CORES <- max(1, parallel::detectCores() - 1)

p <- 8
angles <- octants()                     # degrees, LM = 360
scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
z <- stats::qnorm(0.975)

# Two beta configurations per the design: the coverage error flips direction
# between them (A-review F1). "boundary" is the B3-validation smoke config
# (small trailing harmonic near the 0 bound); "interior" keeps every harmonic
# well inside the simplex.
configs <- list(
  boundary = list(zeta = rep(0.75, p), beta = c(.45, .35, .15, .05)),
  interior = list(zeta = rep(0.75, p), beta = c(.35, .30, .20, .15))
)

# population objects: an exact fit at the truth doubles as the simulator
make_truth <- function(cfg) {
  P0 <- cpm_implied_cor(as.numeric(as_radian(as_degree(angles))),
                        cfg$zeta, cfg$beta)
  fit <- cpm_fit(cormat = P0, scales = scales, angles = angles, n = 10000,
                 m = 3)
  stopifnot(max(abs(fit$results$Zeta - cfg$zeta)) < 1e-6,
            max(abs(fit$betas$Beta - cfg$beta)) < 1e-6)
  fit
}

# signed shortest rotation a -> b in degrees, in (-180, 180]
ang_signed <- function(a, b) -((a - b + 180) %% 360 - 180)

# circular CI membership: truth inside the CCW arc lci -> uci (bootstrap CIs
# are wrapped and may have lci > uci). Span-based, so it needs no anchor: the
# earlier est-anchored rule silently assumed both endpoints lay within 180
# deg of the point estimate, which a replicate-mean-centered interval on a
# near-degenerate resample distribution can violate (B6 review fix; the
# 2026-07-06 recorded run used the est-anchored rule -- see the header note).
angle_covered <- function(est, lci, uci, truth) {
  ((truth - lci) %% 360) <= ((uci - lci) %% 360)
}

run_cell <- function(cfg_name, N, truth_fit, cfg) {
  one <- function(i) try(silent = TRUE, {
    set.seed(BASE_SEED + 1e6 * match(cfg_name, names(configs)) +
               1e3 * match(N, NS) + i)
    X <- cpm_simulate(truth_fit, N)
    fit <- try(suppressWarnings(
      cpm_fit(data = as.data.frame(X), scales = scales, angles = angles,
              m = 3, boots = BOOTS)
    ), silent = TRUE)
    if (inherits(fit, "try-error")) {
      return(NULL)
    }
    # analytic CIs from the same deterministic optimum (engine refit is exact)
    R <- stats::cor(X)
    eng <- suppressWarnings(cpm_engine(R, angles = angles, m = 3,
                                       variant = "A"))
    se <- try(suppressWarnings(cpm_analytic_se(eng, R, N)), silent = TRUE)
    if (inherits(se, "try-error")) se <- NULL
    res <- fit$results
    bet <- fit$betas
    polish <- length(fit$details$removed_harmonics) > 0
    list(
      accepted = isTRUE(fit$details$accepted),
      polish = polish,
      heywood = isTRUE(fit$details$heywood),
      boots_used = fit$details$boots_used,
      Tstat = fit$fit$chisq,
      df = fit$fit$df,
      # bootstrap coverage (default method)
      boot_angle = angle_covered(res$Angle, res$Angle_lci, res$Angle_uci,
                                 angles %% 360),
      boot_zeta = res$Zeta_lci <= cfg$zeta & cfg$zeta <= res$Zeta_uci,
      boot_beta = bet$Beta_lci <= cfg$beta & cfg$beta <= bet$Beta_uci,
      # one-sided miss decomposition for zeta (B3 smoke: interval above truth)
      boot_zeta_above = res$Zeta_lci > cfg$zeta,
      # analytic coverage (same optimum, Wald CIs)
      ana_angle = if (is.null(se)) rep(NA, p) else
        abs(ang_signed(res$Angle, angles %% 360)) <= z * se$angle,
      ana_zeta = if (is.null(se)) rep(NA, p) else
        abs(res$Zeta - cfg$zeta) <= z * se$zeta,
      ana_beta = if (is.null(se)) rep(NA, length(cfg$beta)) else
        abs(bet$Beta - cfg$beta) <= z * se$beta
    )
  })
  out <- parallel::mclapply(seq_len(REPS), one, mc.cores = CORES,
                            mc.preschedule = FALSE)
  # errored replicates (worker try-error, or cpm_fit try-error -> NULL) are
  # counted, not silently dropped: coverage is conditional on estimability
  # and the conditioning event must be visible in the record (B6 review fix)
  err <- vapply(out, function(x) inherits(x, "try-error") || is.null(x),
                logical(1))
  list(reps = out[!err], n_error = sum(err))
}

wilson <- function(k, n) {
  if (n == 0) return(c(NA, NA))
  ct <- stats::prop.test(k, n, correct = FALSE)
  as.numeric(ct$conf.int)
}

summarize_cell <- function(cell, cfg) {
  reps <- cell$reps
  acc <- vapply(reps, `[[`, logical(1), "accepted")
  reps <- reps[acc]                      # conditional on acceptance
  n <- length(reps)
  cov_stat <- function(field, drop_ref = FALSE) {
    m <- do.call(rbind, lapply(reps, `[[`, field))
    if (drop_ref) m <- m[, -1, drop = FALSE]  # reference angle is fixed
    k <- sum(m, na.rm = TRUE)
    nn <- sum(!is.na(m))
    c(cov = k / nn, wilson(k, nn))
  }
  list(
    n_used = n, n_total = length(acc) + cell$n_error,
    n_error = cell$n_error,
    polish_rate = mean(vapply(reps, `[[`, logical(1), "polish")),
    heywood_rate = mean(vapply(reps, `[[`, logical(1), "heywood")),
    boot_angle = cov_stat("boot_angle", drop_ref = TRUE),
    boot_zeta = cov_stat("boot_zeta"),
    boot_beta = cov_stat("boot_beta"),
    ana_angle = cov_stat("ana_angle", drop_ref = TRUE),
    ana_zeta = cov_stat("ana_zeta"),
    ana_beta = cov_stat("ana_beta"),
    zeta_above_rate = mean(unlist(lapply(reps, `[[`, "boot_zeta_above")),
                           na.rm = TRUE),
    # T-calibration on unpolished, accepted replicates (df stable at 10)
    ks_T = local({
      keep <- !vapply(reps, `[[`, logical(1), "polish")
      Ts <- vapply(reps[keep], `[[`, numeric(1), "Tstat")
      if (length(Ts) < 30) return(NA_real_)
      stats::ks.test(Ts, stats::pchisq, df = 10)$p.value
    })
  )
}

if (!free_only) {
cat(sprintf("CPM coverage oracle: reps=%d boots=%d cores=%d %s\n",
            REPS, BOOTS, CORES, if (smoke) "[SMOKE]" else ""))
results <- list()
t0 <- Sys.time()
cell_filter <- Sys.getenv("CPM_COV_CELLS")   # e.g. "boundary_N250,interior_N500"
for (cfg_name in names(configs)) {
  cfg <- configs[[cfg_name]]
  truth_fit <- NULL
  for (N in NS) {
    key <- sprintf("%s_N%d", cfg_name, N)
    if (nzchar(cell_filter) &&
        !key %in% strsplit(cell_filter, ",")[[1]]) next
    if (is.null(truth_fit)) truth_fit <- make_truth(cfg)
    cat(sprintf("[%s] %s ...\n", format(Sys.time(), "%H:%M:%S"), key))
    results[[key]] <- summarize_cell(run_cell(cfg_name, N, truth_fit, cfg),
                                     cfg)
  }
}
cat(sprintf("elapsed: %.1f min\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))

fmt <- function(x) sprintf("%.3f [%.3f, %.3f]", x[1], x[2], x[3])
for (key in names(results)) {
  s <- results[[key]]
  cat(sprintf(
    paste0("%s  (used %d/%d, %d errored; polish %.2f; heywood %.2f)\n",
           "  bootstrap: angle %s  zeta %s  beta %s\n",
           "  analytic:  angle %s  zeta %s  beta %s\n",
           "  zeta misses above truth: %.2f   T-calibration KS p: %s\n"),
    key, s$n_used, s$n_total, s$n_error, s$polish_rate, s$heywood_rate,
    fmt(s$boot_angle), fmt(s$boot_zeta), fmt(s$boot_beta),
    fmt(s$ana_angle), fmt(s$ana_zeta), fmt(s$ana_beta),
    s$zeta_above_rate,
    if (is.na(s$ks_T)) "NA (too few unpolished)" else sprintf("%.3f", s$ks_T)
  ))
}

out_file <- file.path("devel", if (smoke) {
  "m4-coverage-oracle-smoke.rds"
} else if (nzchar(cell_filter)) {
  "m4-coverage-oracle-partial.rds"     # never clobber the full record
} else {
  "m4-coverage-oracle-results.rds"
})
saveRDS(list(results = results, reps = REPS, boots = BOOTS, ns = NS,
             configs = configs, base_seed = BASE_SEED,
             date = Sys.Date()), out_file)
cat("saved:", out_file, "\n")

# ---- stage 2: analytic-CI N-threshold calibration ----------------------------
if (nzchar(cell_filter)) {
  cat("cell filter active; skipping stage 2\n")
  quit(save = "no")
}
# The summary() caution constant (`cpm_analytic_ci_n_caution`, design sec. 5.2)
# is calibrated here: analytic-only replicates (no bootstrap) are cheap, so
# the grid extends well past stage 1. Reported per parameter type; the
# threshold should sit where the worst type re-enters the [.90, .98] band.
NS2 <- c(2000, 5000, 10000, 20000, 50000)
cat("\nstage 2: analytic coverage ladder (reps =", REPS, ")\n")
ana_results <- list()
for (cfg_name in names(configs)) {
  cfg <- configs[[cfg_name]]
  truth_fit <- make_truth(cfg)
  for (N in NS2) {
    one <- function(i) try(silent = TRUE, {
      set.seed(BASE_SEED + 5e7 + 1e6 * match(cfg_name, names(configs)) +
                 1e3 * match(N, NS2) + i)
      X <- cpm_simulate(truth_fit, N)
      R <- stats::cor(X)
      eng <- suppressWarnings(cpm_engine(R, angles = angles, m = 3,
                                         variant = "A"))
      se <- try(suppressWarnings(cpm_analytic_se(eng, R, N)), silent = TRUE)
      if (!eng$accepted || inherits(se, "try-error")) return(NULL)
      list(
        angle = abs(ang_signed(eng$theta, angles %% 360)) <= z * se$angle,
        zeta = abs(eng$zeta - cfg$zeta) <= z * se$zeta,
        beta = abs(eng$beta - cfg$beta) <= z * se$beta
      )
    })
    out <- parallel::mclapply(seq_len(REPS), one, mc.cores = CORES,
                              mc.preschedule = FALSE)
    out <- out[!vapply(out, function(x) {
      inherits(x, "try-error") || is.null(x)
    }, logical(1))]
    covs <- vapply(c("angle", "zeta", "beta"), function(f) {
      m <- do.call(rbind, lapply(out, `[[`, f))
      if (f == "angle") m <- m[, -1, drop = FALSE]
      mean(m, na.rm = TRUE)
    }, numeric(1))
    key <- sprintf("%s_N%d", cfg_name, N)
    ana_results[[key]] <- covs
    cat(sprintf("  %-18s angle %.3f  zeta %.3f  beta %.3f  (n = %d)\n",
                key, covs["angle"], covs["zeta"], covs["beta"], length(out)))
  }
}
out_file2 <- file.path("devel", if (smoke) "m4-coverage-oracle-smoke-ana.rds"
                       else "m4-coverage-oracle-analytic.rds")
saveRDS(list(results = ana_results, reps = REPS, ns = NS2,
             configs = configs, base_seed = BASE_SEED, date = Sys.Date()),
        out_file2)
cat("saved:", out_file2, "\n")
}  # end if (!free_only)

# ---- stage 3: free-scaling analytic-CI coverage (M19) ------------------------
# Validates the free family's analytic (Wald) CIs for theta/zeta/beta, the CIs
# the summary() caution gates (M18-D3 shipped an unconditional caution pending
# THIS measurement). The correlation-input contract (cpm_fit fits cor(X); there
# is no covariance-input path) forces the only well-posed truth to be a
# circumplex CORRELATION P(gamma) with sigma_pop = 1 -- the free family fits the
# p sigma parameters as free NUISANCE parameters that absorb finite-N
# correlation misfit (sigma-hat = 1 only at perfect fit; m4-browne-design.md
# sec. 3.2, RR04/D-009). So this re-uses stage 1's correlation truths verbatim;
# the only change is scaling = "free" in the engine call, which adds p bordered
# nuisance parameters and thereby changes the Fisher information (hence the Wald
# SEs, hence coverage) for theta/zeta/beta. The diag-family counterpart is
# stages 1-2 above at the identical truths, so free-vs-diag is a direct read.
#
# Analytic-only (no per-fit bootstrap): each replicate is one engine fit +
# one Hessian-based SE, so the full run is session-runnable (minutes), unlike
# the bootstrap-laden stage 1. T_free = (N - 1) * F-hat is collected per
# replicate for the deferred T_diag-vs-T_free comparison (spec sec. 9; measure
# now, decide later) and a KS check against chi-square at the fitted df.
#
# Seeds: BASE_SEED + 8e7 + cell/rep offsets, disjoint from stages 1 (no offset)
# and 2 (5e7), so no replicate is shared across families/stages.
cat("\nstage 3: free-scaling analytic-CI coverage (M19; reps =", REPS, ")\n")

NS_FREE <- c(250, 1000)                     # coverage cells (spec sec. 6)
NS_FREE_LADDER <- c(2000, 5000, 20000, 50000)  # analytic-only spot ladder
angles_rad <- as.numeric(as_radian(as_degree(angles)))

# One free-scaling replicate at correlation truth P0: draw X ~ N(0, P0), fit the
# free engine to cor(X), and score Wald coverage of theta/zeta/beta at nominal
# 95%. Returns NULL on a non-accepted fit or a singular (NA) SE -- both are
# conditioning events counted in the record, never silently dropped.
free_one <- function(i, cfg_name, cfg, N, chol0, truth_deg, seed_base) {
  try(silent = TRUE, {
    set.seed(seed_base + i)
    X <- matrix(stats::rnorm(N * p), nrow = N) %*% chol0   # Cov = P0
    R <- stats::cor(X)
    eng <- suppressWarnings(cpm_engine(R, angles = angles, m = 3,
                                       variant = "A", scaling = "free"))
    if (!isTRUE(eng$accepted)) return(NULL)
    se <- try(suppressWarnings(cpm_analytic_se(eng, R, N)), silent = TRUE)
    if (inherits(se, "try-error") || anyNA(se$angle) || anyNA(se$zeta) ||
        anyNA(se$beta)) {
      return(list(se_fail = TRUE))
    }
    list(
      se_fail = FALSE,
      heywood = isTRUE(eng$heywood),
      polish = length(eng$removed_harmonics) > 0,
      max_var_ratio = max(eng$sigma^2),        # sigma-hat spread (nuisance)
      Tstat = (N - 1) * eng$F, df = eng$df,
      # coverage (angle: drop the fixed reference; beta/zeta over all entries,
      # matching the stage-1/2 convention -- a polished-out harmonic's zero-SE
      # interval is a miss, and the polish rate is reported beside it)
      angle = abs(ang_signed(eng$theta, truth_deg)) <= z * se$angle,
      zeta = abs(eng$zeta - cfg$zeta) <= z * se$zeta,
      beta = abs(eng$beta - cfg$beta) <= z * se$beta
    )
  })
}

run_free_cell <- function(cfg_name, cfg, N, chol0, truth_deg) {
  # Seed by N's INDEX, not N itself: 1e3 * N would collide the config term
  # (1e6 * cfg-index) with the N term whenever 1e3 * N == 1e6 (i.e. the
  # boundary/N=2000 vs interior/N=1000 pair), coupling those two cells'
  # replicate streams. Indexing keeps every (cfg, N) cell's seeds disjoint.
  all_ns <- c(NS_FREE, NS_FREE_LADDER)
  seed_base <- BASE_SEED + 8e7 +
    1e6 * match(cfg_name, names(configs)) + 1e3 * match(N, all_ns)
  out <- parallel::mclapply(seq_len(REPS), free_one, cfg_name = cfg_name,
                            cfg = cfg, N = N, chol0 = chol0,
                            truth_deg = truth_deg, seed_base = seed_base,
                            mc.cores = CORES, mc.preschedule = FALSE)
  err <- vapply(out, function(x) inherits(x, "try-error") || is.null(x),
                logical(1))
  out <- out[!err]
  se_fail <- vapply(out, function(x) isTRUE(x$se_fail), logical(1))
  reps <- out[!se_fail]                       # accepted AND SE-estimable
  cov_stat <- function(field, drop_ref = FALSE) {
    m <- do.call(rbind, lapply(reps, `[[`, field))
    if (drop_ref) m <- m[, -1, drop = FALSE]
    k <- sum(m, na.rm = TRUE); nn <- sum(!is.na(m))
    c(cov = k / nn, wilson(k, nn))
  }
  ks_T <- local({
    keep <- !vapply(reps, `[[`, logical(1), "polish")
    Ts <- vapply(reps[keep], `[[`, numeric(1), "Tstat")
    dfs <- vapply(reps[keep], `[[`, numeric(1), "df")
    if (length(Ts) < 30 || length(unique(dfs)) != 1) return(NA_real_)
    stats::ks.test(Ts, stats::pchisq, df = dfs[1])$p.value
  })
  list(
    n_used = length(reps), n_total = REPS,
    n_error = sum(err), n_se_fail = sum(se_fail),
    heywood_rate = mean(vapply(reps, `[[`, logical(1), "heywood")),
    polish_rate = mean(vapply(reps, `[[`, logical(1), "polish")),
    max_var_ratio = if (length(reps)) {
      stats::median(vapply(reps, `[[`, numeric(1), "max_var_ratio"))
    } else NA_real_,
    angle = cov_stat("angle", drop_ref = TRUE),
    zeta = cov_stat("zeta"), beta = cov_stat("beta"),
    ks_T = ks_T
  )
}

free_results <- list()
t3 <- Sys.time()
for (cfg_name in names(configs)) {
  cfg <- configs[[cfg_name]]
  P0 <- cpm_implied_cor(angles_rad, cfg$zeta, cfg$beta)
  chol0 <- chol(P0)
  truth_deg <- angles %% 360
  for (N in c(NS_FREE, NS_FREE_LADDER)) {
    key <- sprintf("%s_N%d", cfg_name, N)
    cat(sprintf("[%s] free %s ...\n", format(Sys.time(), "%H:%M:%S"), key))
    free_results[[key]] <- run_free_cell(cfg_name, cfg, N, chol0, truth_deg)
  }
}
cat(sprintf("stage 3 elapsed: %.1f min\n",
            as.numeric(difftime(Sys.time(), t3, units = "mins"))))

fmt <- function(x) sprintf("%.3f [%.3f, %.3f]", x[1], x[2], x[3])
for (key in names(free_results)) {
  s <- free_results[[key]]
  cat(sprintf(
    paste0("%s  (used %d/%d; %d err, %d SE-fail; heywood %.2f; polish %.2f; ",
           "med max var-ratio %.2f)\n",
           "  analytic free: angle %s  zeta %s  beta %s   KS(T,df) p: %s\n"),
    key, s$n_used, s$n_total, s$n_error, s$n_se_fail, s$heywood_rate,
    s$polish_rate, s$max_var_ratio,
    fmt(s$angle), fmt(s$zeta), fmt(s$beta),
    if (is.na(s$ks_T)) "NA" else sprintf("%.3f", s$ks_T)
  ))
}

out_file3 <- file.path("devel", if (smoke) "m19-free-coverage-smoke.rds"
                       else "m19-free-coverage-results.rds")
saveRDS(list(results = free_results, reps = REPS,
             ns_cov = NS_FREE, ns_ladder = NS_FREE_LADDER,
             configs = configs, base_seed = BASE_SEED, scaling = "free",
             date = Sys.Date()), out_file3)
cat("saved:", out_file3, "\n")
