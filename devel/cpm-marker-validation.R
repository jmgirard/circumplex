# CPM boundary-marker validation (B6 follow-up; spec in
# devel/cpm-marker-validation-brief.md).
#
# Validates the cpm_boundary_markers() set (R/cpm_fit.R) as a RUNTIME
# predictor of analytic-CI mis-coverage: among cormat-path fits where marker M
# fired, what was the empirical coverage of the nominal-95% Wald intervals,
# and among fits where it did not? Measured in the N band where summary()'s
# caution is marker-conditional (2000 <= N < 50000), with N = 50000 as the
# recovered-coverage upper control. Analytic-only by design: the markers gate
# the analytic caution, so every replicate is fitted on the literal cormat
# path (one deterministic engine fit, no bootstrap machinery).
#
# The two calibration choices under test (brief judgment calls #1/#2):
#   1. the beta = 0.10 cut for "small correlation-function weight" (swept
#      post hoc at {0.05, 0.10, 0.15} from recorded min(beta-hat)); and
#   2. the reasoned-in `multimodal` marker (coverage | fired vs not).
# The ratified N thresholds (2000 / 50000) are NOT under test.
#
# Reproducibility: every replicate derives its own seed as BASE_SEED + a
# cell/replicate offset and runs set.seed() locally (the m4-coverage-oracle.R
# discipline), so results are identical for any mc.cores and scheduling.
# BASE_SEED differs from the oracle's, so no replicate is reused, and the
# results file never touches the committed B6 records.
#
# Usage:  Rscript devel/cpm-marker-validation.R                  # full run
#         CPM_MARKER_SMOKE=1 Rscript ...      # ~2 min: fire rates + timing
#         CPM_MARKER_ANALYZE_ONLY=1 Rscript ...   # re-tabulate saved records
#         CPM_MARKER_REPS=NNN Rscript ...     # override full-run reps/cell

devtools::load_all(".", quiet = TRUE)

smoke <- nzchar(Sys.getenv("CPM_MARKER_SMOKE"))
analyze_only <- nzchar(Sys.getenv("CPM_MARKER_ANALYZE_ONLY"))
# REPS = 2000 sized by the smoke run (2026-07-08): the binding subset is
# `multimodal`, which fires only in the high-zeta provocation config at
# ~3-7% for N <= 5000, so 2000 reps/cell yields ~150-250 band firings;
# every other marker subset clears the ~200-firing MC budget easily.
# ~70k fits at ~0.035 s/fit wall (9 cores) = ~40 min.
REPS <- if (smoke) 40 else as.integer(Sys.getenv("CPM_MARKER_REPS", "2000"))
NS <- c(2000, 5000, 10000, 20000, 50000)   # the marker band + upper control
BAND <- NS[NS < 50000]                     # where the caution is conditional
BASE_SEED <- 20260708
CORES <- max(1, parallel::detectCores() - 1)

p <- 8
angles <- octants()                        # degrees, LM = 360
scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
truth_deg <- angles %% 360                 # coverage truth (LM = 0 branch)

# Trailing-harmonic ladder (brief "truth configs"): beta_0 absorbs what the
# trailing harmonic gives up, so beta_1/beta_2 stay fixed and t = .05
# reproduces the committed B6 "boundary" config exactly. The B6 "interior"
# config rides along as the second anchor. zeta = 0.75 throughout (as in B6).
ladder <- c(t000 = 0.00, t002 = 0.02, t005 = 0.05, t010 = 0.10, t015 = 0.15)
configs <- c(
  lapply(ladder, function(t) {
    list(zeta = rep(0.75, p), beta = c(0.50 - t, 0.35, 0.15, t))
  }),
  list(
    interior = list(zeta = rep(0.75, p), beta = c(.35, .30, .20, .15)),
    # Explicit scope addition per the brief's provision: under zeta = 0.75 the
    # Heywood and ill-conditioned-Hessian markers fire in ~0-7% of fits and
    # only at N = 2000 (smoke, 2026-07-08) -- far too rare for conditional
    # coverage. This high-communality config provokes them (heywood ~97%,
    # illcond ~93% at N = 2000, decaying to silence by N = 50000) while
    # keeping every beta interior, so it never trips the small-weight marker
    # and the marker subsets stay separable. It is also the only config in
    # which `multimodal` fires at a measurable rate (~3-7%, N <= 5000).
    zhigh = list(zeta = rep(0.97, p), beta = c(.35, .30, .20, .15))
  )
)

# population objects: an exact fit at the truth doubles as the simulator
# (m4-coverage-oracle.R make_truth; suppressWarnings because the t000 truth
# sits ON the boundary and legitimately polish-warns at the exact optimum)
make_truth <- function(cfg) {
  P0 <- cpm_implied_cor(as.numeric(as_radian(as_degree(angles))),
                        cfg$zeta, cfg$beta)
  fit <- suppressWarnings(
    cpm_fit(cormat = P0, scales = scales, angles = angles, n = 10000, m = 3)
  )
  stopifnot(max(abs(fit$results$Zeta - cfg$zeta)) < 1e-6,
            max(abs(fit$betas$Beta - cfg$beta)) < 1e-6)
  fit
}

# signed shortest rotation a -> b in degrees, in (-180, 180]
ang_signed <- function(a, b) -((a - b + 180) %% 360 - 180)

run_cell <- function(cfg_name, N, truth_fit, cfg) {
  one <- function(i) try(silent = TRUE, {
    set.seed(BASE_SEED + 1e7 * match(cfg_name, names(configs)) +
               1e5 * match(N, NS) + i)
    X <- cpm_simulate(truth_fit, N)
    R <- stats::cor(X)
    # the literal code path the caution applies to: cormat + analytic Wald CIs
    fit <- suppressWarnings(
      cpm_fit(cormat = R, n = N, scales = scales, angles = angles, m = 3)
    )
    d <- fit$details
    res <- fit$results
    bet <- fit$betas
    half <- (res$Angle_uci - res$Angle_lci) / 2   # z * SE(theta), per scale
    retained <- !bet$k %in% d$removed_harmonics
    list(
      config = cfg_name, N = N,
      accepted = isTRUE(d$accepted),
      # raw marker inputs (so every sweep is a re-tabulation, not a refit)
      heywood = isTRUE(d$heywood),
      n_removed = length(d$removed_harmonics),
      min_beta = min(bet$Beta),                       # shipped-marker input
      min_beta_retained = min(bet$Beta[retained]),    # zeros from polish excluded
      hessian_condition = d$hessian_condition,
      multimodal = isTRUE(d$multimodal),
      markers = cpm_boundary_markers(fit),            # the shipped verdict
      # per-parameter analytic coverage (reference angle dropped in analysis)
      ana_angle = abs(ang_signed(res$Angle, truth_deg)) <= half,
      ana_zeta = res$Zeta_lci <= cfg$zeta & cfg$zeta <= res$Zeta_uci,
      ana_beta = bet$Beta_lci <= cfg$beta & cfg$beta <= bet$Beta_uci
    )
  })
  out <- parallel::mclapply(seq_len(REPS), one, mc.cores = CORES,
                            mc.preschedule = FALSE)
  err <- vapply(out, function(x) inherits(x, "try-error") || is.null(x),
                logical(1))
  list(reps = out[!err], n_error = sum(err))
}

# Never clobber the committed record (the m4-coracle discipline): a
# REPS-overridden rerun goes to -custom.rds and never rewrites the committed
# summary; only the default full run owns the provenance files.
reps_override <- !smoke && nzchar(Sys.getenv("CPM_MARKER_REPS"))
out_file <- file.path("devel", if (smoke) {
  "cpm-marker-validation-smoke.rds"
} else if (reps_override) {
  "cpm-marker-validation-custom.rds"
} else {
  "cpm-marker-validation-results.rds"
})

if (analyze_only) {
  stopifnot(file.exists(out_file))
  saved <- readRDS(out_file)
  recs <- saved$recs
  n_error_total <- saved$n_error_total
  cell_errors <- saved$cell_errors    # NULL on records saved before 2026-07-08b
  REPS <- saved$reps                  # provenance follows the record, not env
  cat(sprintf("loaded %s: %d records (reps=%d, %s)\n",
              out_file, length(recs), saved$reps, saved$date))
} else {
  cat(sprintf("CPM marker validation: reps/cell=%d cores=%d %s\n",
              REPS, CORES, if (smoke) "[SMOKE]" else ""))
  recs <- list()
  cell_errors <- data.frame(config = character(), N = integer(),
                            n_error = integer())
  t0 <- Sys.time()
  for (cfg_name in names(configs)) {
    cfg <- configs[[cfg_name]]
    truth_fit <- make_truth(cfg)
    for (N in NS) {
      cat(sprintf("[%s] %s_N%d ...\n", format(Sys.time(), "%H:%M:%S"),
                  cfg_name, N))
      cell <- run_cell(cfg_name, N, truth_fit, cfg)
      recs <- c(recs, cell$reps)
      # per-cell error accounting (coverage is conditional on estimability;
      # the conditioning event must be visible per cell, as in the B6 oracle)
      cell_errors <- rbind(cell_errors, data.frame(
        config = cfg_name, N = N, n_error = cell$n_error
      ))
    }
  }
  n_error_total <- sum(cell_errors$n_error)
  mins <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  cat(sprintf("elapsed: %.1f min (%.2f s/fit on %d cores)\n",
              mins, mins * 60 / (REPS * length(configs) * length(NS)), CORES))
  saveRDS(list(recs = recs, n_error_total = n_error_total,
               cell_errors = cell_errors, reps = REPS,
               ns = NS, configs = configs, base_seed = BASE_SEED,
               date = Sys.Date()), out_file)
  cat("saved:", out_file, "\n")
}

# ---- analysis: everything below is a re-tabulation of the saved records ----

# per-fit flat table; family coverages are per-fit proportions, so pooled
# coverage = mean of per-fit proportions (equal parameter counts per fit) and
# MC intervals can be computed on the per-fit means, which respects the
# within-fit dependence a pooled binomial interval would ignore.
# NA coverage flags are real behavior, not artifacts: cpm_analytic_se()
# returns all-NA SEs (all three families at once) when solve() rejects its
# FD Hessian as computationally singular (R/cpm_fit.R:930-938); an
# indefinite-but-invertible Hessian instead has its negative variances
# CLAMPED to zero (pmax), yielding zero-width CIs that are legitimately
# scored as misses. Verified on the 2026-07-08 record: the per-fit NA
# pattern across families is exactly all-or-nothing (never partial), and
# every NA-CI fit carries the ill-conditioned-Hessian marker. Coverage is
# computed over the DEFINED intervals (na.rm) and the NA-CI rate is reported
# alongside as its own operating characteristic: a marker whose fits often
# have no analytic CI at all has flagged a fit the caution should cover,
# whatever the defined-CI coverage says.
nan_mean <- function(x) {
  m <- mean(x, na.rm = TRUE)
  if (is.nan(m)) NA_real_ else m
}
# marker flags come from the RECORDED cpm_boundary_markers() strings (the
# shipped verdict at fit time) -- never recomputed from raw inputs with
# re-typed constants, so a retuned package threshold cannot silently
# desynchronize the per-marker rows from the any-marker row. The raw inputs
# (min_beta*, in recs) are kept solely for the intentional beta-cut sweep.
flat <- do.call(rbind, lapply(recs, function(r) {
  data.frame(
    config = r$config, N = r$N, accepted = r$accepted,
    heywood = "Heywood communality" %in% r$markers,
    removed = "boundary harmonic removed" %in% r$markers,
    small_beta = "small correlation-function weight" %in% r$markers,
    illcond = "ill-conditioned Hessian" %in% r$markers,
    multimodal = "competing near-tied optima" %in% r$markers,
    any_marker = length(r$markers) > 0,
    min_beta = r$min_beta, min_beta_retained = r$min_beta_retained,
    cov_angle = nan_mean(r$ana_angle[-1]),  # reference angle is fixed
    cov_zeta = nan_mean(r$ana_zeta),
    cov_beta = nan_mean(r$ana_beta),
    na_ci = anyNA(c(r$ana_angle[-1], r$ana_zeta, r$ana_beta)),
    # clean = every angle & zeta CI defined AND covering (NA is not clean)
    clean_anglezeta = isTRUE(all(r$ana_angle[-1]) && all(r$ana_zeta)),
    stringsAsFactors = FALSE
  )
}))
cat(sprintf("\nrecords: %d fitted (%d errored), %d accepted (%.3f)\n",
            nrow(flat), n_error_total, sum(flat$accepted),
            mean(flat$accepted)))
if (is.null(cell_errors)) {
  cat("(per-cell error counts unavailable: record predates 2026-07-08b;\n",
      " with a zero total, every cell is necessarily error-free)\n", sep = "")
}
flat <- flat[flat$accepted, ]              # conditional on acceptance (as B6)

markers_cols <- c("heywood", "removed", "small_beta", "illcond", "multimodal",
                  "any_marker")

# Every table below is BUILT once as a data frame and then printed from it;
# the committed summary rds saves those same data frames, so the printed
# numbers and the saved aggregates cannot drift apart.

# mean of per-fit coverage proportions with a cluster-level normal MC
# interval, over fits whose family CIs are defined (n = defined-CI fits)
cov_ci <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) return(c(est = NA, lci = NA, uci = NA, n = 0))
  m <- mean(x)
  se <- if (n > 1) stats::sd(x) / sqrt(n) else NA
  c(est = m, lci = m - 1.96 * se, uci = m + 1.96 * se, n = n)
}
fmt_ci <- function(est, lci, uci) {
  if (is.na(est)) return("      --            ")
  sprintf("%.3f [%.3f,%.3f]", est, lci, uci)
}
fam_cols <- function(sub) {
  out <- lapply(c(angle = "cov_angle", zeta = "cov_zeta", beta = "cov_beta"),
                function(f) cov_ci(sub[[f]]))
  data.frame(n = nrow(sub),
             na_ci = if (nrow(sub)) mean(sub$na_ci) else NA_real_,
             t(unlist(out)))    # angle.est, angle.lci, ..., beta.n
}

# ---- tables A/B: per-cell fire rates and coverage (configs x N) -------------
cells <- do.call(rbind, lapply(names(configs), function(cfg_name) {
  do.call(rbind, lapply(NS, function(N) {
    sub <- flat[flat$config == cfg_name & flat$N == N, ]
    if (nrow(sub) == 0) return(NULL)
    err <- if (is.null(cell_errors)) NA_integer_ else {
      v <- cell_errors$n_error[cell_errors$config == cfg_name &
                                 cell_errors$N == N]
      if (length(v) == 1) v else NA_integer_
    }
    cbind(data.frame(config = cfg_name, N = N, n_error = err),
          as.data.frame(as.list(colMeans(sub[markers_cols]))),
          fam_cols(sub))
  }))
}))

cat("\n== A. marker fire rates by config x N (accepted fits) ==\n")
for (i in seq_len(nrow(cells))) {
  r <- cells[i, ]
  cat(sprintf("%-9s N=%-6d n=%-4d %s%s\n", r$config, r$N, r$n,
              paste(sprintf("%s %.2f", markers_cols,
                            unlist(r[markers_cols])), collapse = "  "),
              if (isTRUE(r$n_error > 0)) sprintf("  ERR=%d", r$n_error) else ""))
}

cat("\n== B. cell coverage (angle / zeta / beta) + NA-CI rate, accepted fits ==\n")
for (i in seq_len(nrow(cells))) {
  r <- cells[i, ]
  cat(sprintf(
    "%-9s N=%-6d n=%-4d angle %.3f  zeta %.3f  beta %.3f  na_ci %.2f\n",
    r$config, r$N, r$n, r$angle.est, r$zeta.est, r$beta.est, r$na_ci))
}

# ---- table C: the core deliverable -- conditional coverage per marker -------
# pooled over the marker band (2000 <= N < 50000), where the caution is
# marker-conditional; N = 50000 reported separately as the upper control
band <- flat[flat$N %in% BAND, ]
cond_rows <- function(dat) {
  do.call(rbind, lapply(markers_cols, function(mk) {
    do.call(rbind, lapply(c(TRUE, FALSE), function(state) {
      cbind(data.frame(marker = mk, fired = state),
            fam_cols(dat[dat[[mk]] == state, ]))
    }))
  }))
}
cond_band <- cond_rows(band)
cond_control <- cond_rows(flat[flat$N == 50000, ])
print_cond <- function(tab, label) {
  cat(sprintf("\n== C. conditional coverage | marker, %s ==\n", label))
  cat(sprintf("%-11s %-6s %-5s %-6s %-22s %-22s %-22s\n", "marker", "fired",
              "n", "na_ci", "angle (defined CIs)", "zeta", "beta"))
  for (i in seq_len(nrow(tab))) {
    r <- tab[i, ]
    cat(sprintf("%-11s %-6s %-5d %-6s %s  %s  %s%s\n", r$marker,
                if (r$fired) "yes" else "no", r$n,
                if (r$n > 0) sprintf("%.2f", r$na_ci) else "--",
                fmt_ci(r$angle.est, r$angle.lci, r$angle.uci),
                fmt_ci(r$zeta.est, r$zeta.lci, r$zeta.uci),
                fmt_ci(r$beta.est, r$beta.lci, r$beta.uci),
                if (r$n > 0 && r$angle.n < 100) "  [UNDERPOWERED]" else ""))
  }
}
print_cond(cond_band, "marker band pooled (N 2000-20000)")
print_cond(cond_control, "upper control (N = 50000)")

# per-N any-marker rows: what summary() actually gates on, N-resolved so the
# pooled discrimination cannot be a low-N composition artifact
anymarker_by_N <- do.call(rbind, lapply(NS, function(N) {
  do.call(rbind, lapply(c(TRUE, FALSE), function(state) {
    sub <- flat[flat$N == N & flat$any_marker == state, ]
    cbind(data.frame(N = N, fired = state), fam_cols(sub))
  }))
}))
cat("\n== C2. any-marker by N ==\n")
for (i in seq_len(nrow(anymarker_by_N))) {
  r <- anymarker_by_N[i, ]
  cat(sprintf("N=%-6d fired=%-3s n=%-5d angle %s  zeta %s\n", r$N,
              if (r$fired) "yes" else "no", r$n,
              fmt_ci(r$angle.est, r$angle.lci, r$angle.uci),
              fmt_ci(r$zeta.est, r$zeta.lci, r$zeta.uci)))
}

# ---- table D: beta-cut sensitivity sweep (judgment call #1) ------------------
# recomputed from recorded min(beta-hat); "shipped" semantics keep polish
# zeros in the min (so `removed` implies `small_beta`), the retained-only
# variant separates the two markers. False-alarm rate: fraction of clean
# fits (every angle & zeta CI covered) that the marker flags.
sweep_tab <- do.call(rbind, lapply(c("min_beta", "min_beta_retained"),
  function(input) do.call(rbind, lapply(c(0.05, 0.10, 0.15), function(cut) {
    fired <- band[[input]] < cut
    a1 <- cov_ci(band$cov_angle[fired]); a0 <- cov_ci(band$cov_angle[!fired])
    z1 <- cov_ci(band$cov_zeta[fired]);  z0 <- cov_ci(band$cov_zeta[!fired])
    data.frame(input = input, cut = cut,
               n_fired = sum(fired), n_quiet = sum(!fired),
               angle_fired = a1[1], angle_fired_l = a1[2], angle_fired_u = a1[3],
               angle_quiet = a0[1], angle_quiet_l = a0[2], angle_quiet_u = a0[3],
               zeta_fired = z1[1], zeta_fired_l = z1[2], zeta_fired_u = z1[3],
               zeta_quiet = z0[1], zeta_quiet_l = z0[2], zeta_quiet_u = z0[3],
               false_alarm = mean(fired[band$clean_anglezeta]))
  }))))
sweep_labels <- c(min_beta = "shipped: min over all beta-hat",
                  min_beta_retained = "retained harmonics only")
for (input in names(sweep_labels)) {
  cat(sprintf("\n== D. beta-cut sweep (%s), marker band ==\n",
              sweep_labels[[input]]))
  for (i in which(sweep_tab$input == input)) {
    r <- sweep_tab[i, ]
    cat(sprintf(paste0("cut %.2f  fired n=%-5d angle %s  zeta %s\n",
                       "          quiet n=%-5d angle %s  zeta %s\n",
                       "          false-alarm rate (fired | clean fit): %.3f\n"),
                r$cut,
                r$n_fired, fmt_ci(r$angle_fired, r$angle_fired_l, r$angle_fired_u),
                fmt_ci(r$zeta_fired, r$zeta_fired_l, r$zeta_fired_u),
                r$n_quiet, fmt_ci(r$angle_quiet, r$angle_quiet_l, r$angle_quiet_u),
                fmt_ci(r$zeta_quiet, r$zeta_quiet_l, r$zeta_quiet_u),
                r$false_alarm))
  }
}

# ---- table E: multimodality sweep (judgment call #2) is the `multimodal`
# rows of table C; its false-alarm rate for symmetry:
cat(sprintf("\n== E. multimodal false-alarm rate (fired | clean fit): %.3f (n clean = %d)\n",
            mean(band$multimodal[band$clean_anglezeta]),
            sum(band$clean_anglezeta)))

# ---- committed provenance summary (house discipline: summaries, not the
# ~1.7 MB per-fit record, which is regenerable from BASE_SEED). Saves the
# exact data frames printed above; skipped for smoke and REPS-overridden
# runs so a partial rerun can never replace the committed record. ------------
if (!smoke && !reps_override) {
  saveRDS(list(cells = cells, cond_band = cond_band,
               cond_control = cond_control, anymarker_by_N = anymarker_by_N,
               beta_sweep = sweep_tab, n_error_total = n_error_total,
               reps = REPS, ns = NS, configs = configs,
               base_seed = BASE_SEED, date = Sys.Date()),
          file.path("devel", "cpm-marker-validation-summary.rds"))
  cat("saved: devel/cpm-marker-validation-summary.rds\n")
}
