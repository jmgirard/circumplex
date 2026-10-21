# ==== PRE-REGISTRATION BEGIN ====
# M167: CPM interval coverage per component, and candidate interval families.
#
# This block is the study's pre-registration. It fixes the truths, cells,
# methods, counts, scoring rule, band and pass rule before any full-run
# output exists. Changes after the design review and before the first full
# run are recorded in cairn/milestones/M167-cpm-interval-oracle.md and in the
# verdict D-entry. Nothing in this block changes after the first full run.
#
# TRUTHS (the M4 truths, devel/m4-coverage-oracle.R): p = 8 octant scales at
# octants() angles, zeta = .75 for every scale, m = 3, variant "A", unit
# scaling, reference scale 1 (PA, fixed angle).
#   boundary: beta = .45 / .35 / .15 / .05
#   interior: beta = .35 / .30 / .20 / .15
#
# CELLS
#   Raw-data cells, N = 250, 500, 1000 (both truths). Data: cpm_simulate() of
#   the exact truth fit, seeded per replicate exactly as M4 seeded them
#   (BASE_SEED_M4 = 20260706 + 1e6 * truth index + 1e3 * N index + i). These
#   six cells are the M4 control cells (acceptance AC2).
#   Correlation-matrix cells, N = 2000, 5000, 20000 (both truths). Data: one
#   sample correlation matrix per replicate, drawn as cov2cor() of a
#   Wishart(N - 1, P_truth) draw, which is the exact distribution of cor(X)
#   for N multivariate-normal rows. Fresh seeds: 20261021 + 1e6 * truth
#   index + 1e3 * N index + i.
#   Replicates: 500 per cell. Resamples: 1000 per bootstrap of either kind.
#
# METHODS (all at nominal 95%; implementations in devel/m167-interval-helpers.R)
#   B1  shipped percentile bootstrap (cpm_fit()'s raw-data default): rows
#       resampled, warm-started refits, circular quantile for angles, plain
#       quantile for zeta and beta. Raw-data cells only.
#   B2  shipped Wald interval: estimate +/- 1.96 analytic SE (cpm_analytic_se()).
#       All cells.
#   F1  transformed Wald (browne1982 eqs. 1.6.30-1.6.32, p. 95): symmetric on
#       ln v for each communality, v = (1 - zeta^2) / zeta^2, and on logit
#       beta_k for each weight, back-transformed; angles on the identity scale
#       (so F1's angle interval is B2's). All cells.
#   F2p parametric bootstrap: 1000 correlation matrices drawn as
#       cov2cor(Wishart(N - 1, Phat)) at the fit's own N and Phat, refit with
#       the shipped warm-started loop; percentile intervals formed as in B1.
#       All cells (the correlation-matrix path).
#   F2bc the F2p replicates, bias-corrected percentile interval (acceleration
#       0) for zeta and beta only. All cells.
#   F3  the B1 replicates, bias-corrected percentile interval (acceleration 0)
#       for zeta and beta only. Raw-data cells only.
#   Bias-corrected form: z0 = qnorm(share of replicates below the estimate,
#   ties counted half, clamped to [1/(2B), 1 - 1/(2B)]); endpoints are type-7
#   quantiles at pnorm(2 z0 -/+ 1.96).
#
# PATHS
#   raw-data path: B1, F3, and B2 and F1 at the raw-data cells.
#   correlation-matrix path: B2, F1, F2p, F2bc at all six N.
#
# SCORING
#   Components: the 7 free angles (the reference angle is fixed and has no
#   interval), the 8 communalities, the 4 beta weights of the m = 3 model.
#   Each component's coverage is scored separately, never pooled.
#   Side of a miss: zeta and beta, "above" when the lower limit exceeds the
#   truth, "below" when the upper limit is under it. Angles, "ccw" when the
#   signed shortest rotation from the truth to the interval's arc midpoint is
#   positive (the interval lies counterclockwise of the truth), "cw"
#   otherwise. Arcs run counterclockwise from lower to upper limit.
#   Excluded fit: a replicate whose fit errors, is refused, or fails the
#   engine's acceptance criterion is dropped from every method; the count is
#   recorded per cell.
#   NA standard error (singular information matrix): B2 and F1 have no
#   interval for that replicate; coverage is conditional on a defined
#   interval, and the NA count is recorded per component.
#   All bootstrap resamples excluded: that method has no interval for the
#   replicate (conditional, counted the same way).
#   Removed harmonic (beta-hat_k = 0 after the engine's polish): every method
#   reports the degenerate interval [0, 0], scored as a miss below the truth
#   (every truth weight is positive), and counted per component.
#   Heywood communality (zeta-hat > .995): scored like any other fit; it is
#   not excluded. ln v stays finite because zeta-hat < 1 under the logit
#   parameterization.
#
# BAND AND PASS RULE
#   Band: [.90, .98] per component.
#   Monte Carlo allowance: a component passes at a cell when its coverage
#   point estimate lies in [.90 - .015, .98 + .015] = [.885, .995]. At 500
#   replicates the Monte Carlo SE of a coverage near .93 is about .011, so
#   the allowance is about 1.4 SE.
#   Multiplicity: a (method, path, parameter family) passes when every
#   component of that family passes at every cell listed for that path. No
#   further correction: the allowance is the only noise margin, and the
#   per-component Wilson 95% intervals are reported beside each figure.
#   Parameter families: angles, communalities (zeta), weights (beta). F2bc and
#   F3 are judged on zeta and beta only.
#   Descriptive only (not the verdict): for each (method, path, family), the
#   smallest N from which it passes at every larger listed N.
#
# CONTROL (AC2): pooled B1 and B2 coverage per family at the six raw-data
# cells, pooled as M4 pooled it, must lie within .03 of the M4 table in
# cairn/DESIGN.md, or the record names the engine change that explains it.
# ==== PRE-REGISTRATION END ====
#
# Usage:  Rscript devel/m167-interval-oracle.R                  # full run
#         M167_SMOKE=1 Rscript devel/m167-interval-oracle.R     # smoke run
#         M167_CELLS=boundary_N250,interior_N2000 Rscript ...   # named cells
#         M167_CONTROL=1 Rscript devel/m167-interval-oracle.R   # AC2 control
# The control run scores only B1 and B2 at the six raw-data cells, so it
# produces no candidate-family output before the design is final.
# The full run takes about 15 CPU-hours. Results:
# devel/m167-interval-results.rds (smoke and partial runs write other names
# and are never committed as results).
#
# Reproducibility: each replicate runs set.seed() on its own seed, so the
# results do not depend on mc.cores or scheduling (IP4 holds trivially: this
# script is not package code).

devtools::load_all(".", quiet = TRUE)
source(file.path("devel", "m167-interval-helpers.R"))

smoke <- nzchar(Sys.getenv("M167_SMOKE"))
control <- nzchar(Sys.getenv("M167_CONTROL"))
REPS <- if (smoke) 12 else 500
BOOTS <- if (smoke) 100 else 1000
NS_RAW <- c(250, 500, 1000)
NS_COR <- c(2000, 5000, 20000)
SEED_M4 <- 20260706
SEED_NEW <- 20261021
CORES <- max(1, parallel::detectCores() - 1)
LEVEL <- 0.95
BAND <- c(0.90, 0.98)
ALLOW <- 0.015

p <- 8
angles <- octants()
scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
configs <- list(
  boundary = list(zeta = rep(0.75, p), beta = c(.45, .35, .15, .05)),
  interior = list(zeta = rep(0.75, p), beta = c(.35, .30, .20, .15))
)

make_truth <- function(cfg) {
  P0 <- cpm_implied_cor(as.numeric(as_radian(as_degree(angles))),
                        cfg$zeta, cfg$beta)
  fit <- cpm_fit(cormat = P0, scales = scales, angles = angles, n = 10000,
                 m = 3)
  stopifnot(max(abs(fit$results$Zeta - cfg$zeta)) < 1e-6,
            max(abs(fit$betas$Beta - cfg$beta)) < 1e-6)
  fit
}

RAW_METHODS <- if (control) c("B1", "B2") else
  c("B1", "F3", "B2", "F1", "F2p", "F2bc")
COR_METHODS <- c("B2", "F1", "F2p", "F2bc")

# side-of-truth codes for every method on one replicate
score_ci <- function(ci, cfg, free) {
  list(angle = m167_side_angle(ci$angle, angles %% 360)[free],
       zeta = m167_side_linear(ci$zeta, cfg$zeta),
       beta = m167_side_linear(ci$beta, cfg$beta))
}

one_replicate <- function(cfg_name, N, i, raw, truth_fit, cfg) {
  ti <- match(cfg_name, names(configs))
  if (raw) {
    set.seed(SEED_M4 + 1e6 * ti + 1e3 * match(N, NS_RAW) + i)
    X <- cpm_simulate(truth_fit, N)
    R <- stats::cor(X)
  } else {
    set.seed(SEED_NEW + 1e6 * ti + 1e3 * match(N, NS_COR) + i)
    R <- stats::cov2cor(stats::rWishart(1, N - 1, truth_fit$matrices$Phat)[, , 1])
  }
  eng <- try(suppressWarnings(cpm_engine(R, angles = angles, m = 3,
                                         variant = "A")), silent = TRUE)
  if (inherits(eng, "try-error") || !isTRUE(eng$accepted)) {
    return(list(excluded = TRUE))
  }
  free <- eng$spec$free_pos
  ci <- list()
  # B1 first: its resample draw must be the first RNG use after the data, as
  # in cpm_fit(), so the M4 control cells reproduce M4's intervals.
  if (raw) {
    b1 <- suppressWarnings(m167_boot_reps(eng, X, BOOTS))
    ci$B1 <- m167_percentile(b1, LEVEL)
    if (!control) ci$F3 <- m167_bc(b1, eng, LEVEL)
  }
  se <- try(suppressWarnings(cpm_analytic_se(eng, R, N)), silent = TRUE)
  if (inherits(se, "try-error")) {
    se <- list(angle = rep(NA_real_, p), zeta = rep(NA_real_, p),
               beta = rep(NA_real_, 4))
  }
  ci$B2 <- m167_wald(eng, se, LEVEL)
  f2 <- list(n_ok = NA)
  if (!control) {
    ci$F1 <- m167_transformed_wald(eng, se, LEVEL)
    f2 <- suppressWarnings(m167_param_reps(eng, N, BOOTS))
    ci$F2p <- m167_percentile(f2, LEVEL)
    ci$F2bc <- m167_bc(f2, eng, LEVEL)
  }
  list(
    excluded = FALSE,
    heywood = isTRUE(eng$heywood),
    removed = eng$beta == 0,
    boots_used = c(B1 = if (raw) b1$n_ok else NA, F2 = f2$n_ok),
    sides = lapply(ci, score_ci, cfg = cfg, free = free)
  )
}

wilson <- function(k, n) {
  if (n == 0) return(c(NA_real_, NA_real_))
  as.numeric(stats::prop.test(k, n, correct = FALSE)$conf.int)
}

# per-component summary of one method and family: coverage, miss split,
# conditioning count, NA count, Wilson interval
summarize_family <- function(reps, method, fam) {
  m <- do.call(rbind, lapply(reps, function(r) r$sides[[method]][[fam]]))
  n_def <- colSums(!is.na(m))
  cov <- colSums(m == 0, na.rm = TRUE) / n_def
  out <- data.frame(
    component = seq_len(ncol(m)),
    coverage = cov,
    miss_low = colSums(m == -1, na.rm = TRUE) / n_def,
    miss_high = colSums(m == 1, na.rm = TRUE) / n_def,
    n = n_def,
    n_na = colSums(is.na(m))
  )
  w <- t(mapply(wilson, colSums(m == 0, na.rm = TRUE), n_def))
  out$wilson_lo <- w[, 1]
  out$wilson_hi <- w[, 2]
  out$pass <- !is.na(cov) & cov >= BAND[1] - ALLOW & cov <= BAND[2] + ALLOW
  out
}

component_names <- list(
  angle = scales[-1], zeta = scales, beta = paste0("beta", 0:3)
)

summarize_cell <- function(out, methods) {
  ok <- !vapply(out, function(r) inherits(r, "try-error") || is.null(r),
                logical(1))
  n_error <- sum(!ok)
  out <- out[ok]
  excl <- vapply(out, `[[`, logical(1), "excluded")
  reps <- out[!excl]
  fams <- c("angle", "zeta", "beta")
  by_method <- lapply(stats::setNames(methods, methods), function(meth) {
    lapply(stats::setNames(fams, fams), function(f) {
      if (meth %in% c("F3", "F2bc") && f == "angle") return(NULL)
      s <- summarize_family(reps, meth, f)
      s$component <- component_names[[f]]
      s
    })
  })
  # pooled as M4 pooled it: all components and replicates with an interval
  pooled <- lapply(by_method, function(mm) vapply(mm, function(s) {
    if (is.null(s)) NA_real_ else sum(s$coverage * s$n) / sum(s$n)
  }, numeric(1)))
  removed <- do.call(rbind, lapply(reps, `[[`, "removed"))
  list(
    n_reps = length(reps), n_excluded = sum(excl), n_error = n_error,
    heywood_rate = mean(vapply(reps, `[[`, logical(1), "heywood")),
    removed_rate = colMeans(removed),
    boots_used = colMeans(do.call(rbind, lapply(reps, `[[`, "boots_used"))),
    by_method = by_method, pooled = pooled
  )
}

run_cell <- function(cfg_name, N, raw, truth_fit) {
  cfg <- configs[[cfg_name]]
  out <- parallel::mclapply(seq_len(REPS), function(i) {
    try(one_replicate(cfg_name, N, i, raw, truth_fit, cfg), silent = TRUE)
  }, mc.cores = CORES, mc.preschedule = FALSE)
  summarize_cell(out, if (raw) RAW_METHODS else COR_METHODS)
}

# ---- verdict: applies the pre-registered pass rule -----------------------------

PATHS <- list(
  raw = list(cells = NS_RAW, methods = c("B1", "F3", "B2", "F1")),
  cor = list(cells = c(NS_RAW, NS_COR), methods = c("B2", "F1", "F2p", "F2bc"))
)

verdict_table <- function(results) {
  rows <- list()
  for (path in names(PATHS)) {
    for (meth in PATHS[[path]]$methods) {
      for (fam in c("angle", "zeta", "beta")) {
        if (meth %in% c("F3", "F2bc") && fam == "angle") next
        pass_by_n <- vapply(PATHS[[path]]$cells, function(N) {
          all(vapply(names(configs), function(cn) {
            s <- results[[sprintf("%s_N%d", cn, N)]]$by_method[[meth]][[fam]]
            !is.null(s) && all(s$pass)
          }, logical(1)))
        }, logical(1))
        ns <- PATHS[[path]]$cells
        from <- NA_real_
        for (j in rev(seq_along(ns))) if (pass_by_n[j]) from <- ns[j] else break
        rows[[length(rows) + 1]] <- data.frame(
          path = path, method = meth, family = fam, pass = all(pass_by_n),
          passes_from_N = from
        )
      }
    }
  }
  do.call(rbind, rows)
}

# ---- run -----------------------------------------------------------------------

cell_filter <- Sys.getenv("M167_CELLS")
want <- function(key) {
  !nzchar(cell_filter) || key %in% strsplit(cell_filter, ",")[[1]]
}

cat(sprintf("M167 interval oracle: reps=%d boots=%d cores=%d %s\n",
            REPS, BOOTS, CORES, if (smoke) "[SMOKE]" else ""))
t0 <- Sys.time()
results <- list()
for (cfg_name in names(configs)) {
  truth_fit <- make_truth(configs[[cfg_name]])
  for (N in if (control) NS_RAW else c(NS_RAW, NS_COR)) {
    key <- sprintf("%s_N%d", cfg_name, N)
    if (!want(key)) next
    cat(sprintf("[%s] %s ...\n", format(Sys.time(), "%H:%M:%S"), key))
    results[[key]] <- run_cell(cfg_name, N, N %in% NS_RAW, truth_fit)
    s <- results[[key]]
    cat(sprintf("  reps %d (excluded %d, errored %d), heywood %.2f\n",
                s$n_reps, s$n_excluded, s$n_error, s$heywood_rate))
    for (meth in names(s$pooled)) {
      pc <- s$pooled[[meth]]
      cat(sprintf("  %-5s pooled angle %s zeta %.3f beta %.3f\n", meth,
                  if (is.na(pc["angle"])) "  -  " else sprintf("%.3f", pc["angle"]),
                  pc["zeta"], pc["beta"]))
    }
  }
}
cat(sprintf("elapsed: %.1f min\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))

verdict <- if (!nzchar(cell_filter) && !control) verdict_table(results) else NULL
if (!is.null(verdict)) print(verdict, row.names = FALSE)

# AC2 control: pooled B1 (bootstrap) and B2 (Wald) per family against the M4
# record, whose pooled figures are the DESIGN.md table to three decimals
m4 <- readRDS(file.path("devel", "m4-coverage-oracle-results.rds"))$results
control_table <- do.call(rbind, lapply(intersect(names(results), names(m4)),
                                       function(key) {
  do.call(rbind, lapply(c("angle", "zeta", "beta"), function(f) {
    m167 <- c(B1 = results[[key]]$pooled$B1[[f]],
              B2 = results[[key]]$pooled$B2[[f]])
    m4v <- c(B1 = m4[[key]][[paste0("boot_", f)]][["cov"]],
             B2 = m4[[key]][[paste0("ana_", f)]][["cov"]])
    data.frame(cell = key, family = f, method = names(m167),
               m167 = unname(m167), m4 = unname(m4v),
               diff = unname(m167 - m4v))
  }))
}))
if (!is.null(control_table)) {
  cat("\nAC2 control (pooled, vs M4 record):\n")
  print(control_table, row.names = FALSE, digits = 3)
}

out_file <- file.path("devel", if (control) {
  "m167-interval-control.rds"
} else if (smoke) {
  "m167-interval-smoke.rds"
} else if (nzchar(cell_filter)) {
  "m167-interval-partial.rds"
} else {
  "m167-interval-results.rds"
})
saveRDS(list(results = results, verdict = verdict, control = control_table,
             reps = REPS,
             boots = BOOTS, configs = configs, ns_raw = NS_RAW,
             ns_cor = NS_COR, band = BAND, allowance = ALLOW,
             seeds = c(m4 = SEED_M4, new = SEED_NEW),
             commit = tryCatch(system("git rev-parse HEAD", intern = TRUE),
                               error = function(e) NA_character_),
             date = Sys.Date()), out_file)
cat("saved:", out_file, "\n")
