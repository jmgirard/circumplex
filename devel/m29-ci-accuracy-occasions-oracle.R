# M29 / AC2: simulation-coverage oracle for the occasions ssm_ci_accuracy()
# path (design devel/m29-design.md; D-017).
#
# WHAT IT VALIDATES. ssm_ci_accuracy() on an occasions object reports, per
# occasion (and the paired contrast), the coverage its interval procedure would
# attain in a population like the fitted estimates, at the observed n. This
# oracle checks that the diagnostic's REPORTED coverage tracks the TRUE
# empirical coverage of the object's own procedure at the same plug-in
# population -- computed independently by drawing fresh datasets from the
# object's stacked (mu-hat, Sigma-hat) and running the REAL ssm_analyze()
# occasions procedure on each (boot::boot for bootstrap; ssm_montecarlo for MC),
# then tallying coverage of the fixed plug-in truths. The diagnostic replays the
# procedure internally (shared-W weighted occasion-block means for bootstrap;
# ssm_mc_replicates(occ_k=) for MC); the direct loop replays it via the shipped
# analysis path. Agreement within Monte Carlo error confirms the M29 replay
# machinery (occ_scores re-scoring, the shared-W bootstrap, the occ_k MC draw,
# the angular displacement coverage, and the truth computation) reproduces the
# procedure it claims to assess.
#
# The plug-in truths ARE the object's own point estimates e/a/d_est (the SSM
# parameters of the stacked mean mu-hat), so both the diagnostic and the direct
# loop score coverage of the identical fixed targets -- this isolates the
# replay machinery from plug-in estimation error (the latter is the diagnostic's
# documented limitation, not what AC2 tests).
#
# PRE-REGISTERED ACCEPTANCE (fixed before the full run; smoke runs carry no
# evidence -- the committed rds sets smoke = FALSE). For every cell x engine x
# profile-row x parameter (e, a, d), with reported coverage r over R1 reps and
# empirical coverage e_emp over R2 reps:
#     |r - e_emp| <= 4 * sqrt(r(1-r)/R1 + e_emp(1-e_emp)/R2) + 0.010
# The 4-SE band (two independent binomial estimates of the same probability;
# P(|Z| > 4) ~ 6e-5) plus a 0.010 slack absorbs the bootstrap engine's genuine
# implementation difference (multinomial weights vs boot::boot's index draw --
# same law, different realization). The MC engine shares ssm_mc_replicates()
# between both sides, so its agreement is tighter. Recorded in the M29 review.
#
# Reproducibility: every replicate derives its own seed from BASE_SEED plus a
# distinct (cell x engine)-index term and a rep term that cannot alias across
# cells (LESSONS 2026-07-13, M19: index the level, never the raw value), and
# runs set.seed() locally, so results are identical for any mc.cores.
#
# Usage:  Rscript devel/m29-ci-accuracy-occasions-oracle.R          # full run
#         M29_SMOKE=1 Rscript devel/m29-ci-accuracy-occasions-oracle.R  # ~2 min

devtools::load_all(".", quiet = TRUE)

smoke <- nzchar(Sys.getenv("M29_SMOKE"))
R1 <- if (smoke) 40 else 1000    # diagnostic reps (ssm_ci_accuracy `reps`)
R2 <- if (smoke) 40 else 800     # direct empirical reps
BOOTS <- if (smoke) 80 else 300  # replicates per fit (both engines)
BASE_SEED <- 20260717
CORES <- if (smoke) 1 else max(1, parallel::detectCores() - 1)

p <- 8
angles_deg <- octants()
ang <- as.numeric(angles_deg) * pi / 180
scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
occ1 <- paste0(scales, "_1")
occ2 <- paste0(scales, "_2")
occ <- list(T1 = occ1, T2 = occ2)

angdist_deg <- function(x, y) ((x - y + 180) %% 360) - 180

# Person i draws (block1, block2) ~ MVN(mu, Sigma), Sigma isotropic within and
# rho across occasions: mu_j = e_j + a_j cos(ang - d_j). At octants the
# closed-form SSM estimator recovers (e_j, a_j, d_j) from mu_j exactly.
simulate_cell <- function(n, e1, a1, d1, e2, a2, d2, rho, s2 = 1) {
  mu1 <- e1 + a1 * cos(ang - d1 * pi / 180)
  mu2 <- e2 + a2 * cos(ang - d2 * pi / 180)
  z1 <- matrix(rnorm(n * p), n, p)
  z2 <- z1 * rho + matrix(rnorm(n * p), n, p) * sqrt(1 - rho^2)
  df <- data.frame(sweep(sqrt(s2) * z1, 2, mu1, "+"),
                   sweep(sqrt(s2) * z2, 2, mu2, "+"))
  names(df) <- c(occ1, occ2)
  df
}

# Coverage of the three profile rows (T1, T2, contrast) against fixed truths,
# from one fitted occasions object's results table. e/a: linear interval
# membership; d: angular arc membership on the estimate's branch (both sides via
# angular distance, tolerating the wrap and the contrast branch shift).
cover_rows <- function(res, truth_e, truth_a, truth_d) {
  vapply(1:3, function(i) {
    ec <- res$e_lci[i] <= truth_e[i] && res$e_uci[i] >= truth_e[i]
    ac <- res$a_lci[i] <= truth_a[i] && res$a_uci[i] >= truth_a[i]
    dc <- angdist_deg(truth_d[i], res$d_lci[i]) >= 0 &&
      angdist_deg(res$d_uci[i], truth_d[i]) >= 0
    c(e = ec, a = ac, d = dc)
  }, numeric(3))                       # 3 params x 3 rows
}

# One (cell, engine): build the plug-in object, get the diagnostic's reported
# coverage, then the direct empirical coverage from R2 fresh draws of the
# object's own (mu-hat, Sigma-hat).
run_cell <- function(cell_name, cell_i, cell, method, method_i) {
  # 1. Build the plug-in object at the target n from the generative truth.
  set.seed(BASE_SEED + 7e6 + cell_i)
  d2 <- (cell$d1 + cell$dd) %% 360
  df0 <- simulate_cell(cell$n, cell$e1, cell$a1, cell$d1,
                       cell$e1 + cell$de, cell$a1 + cell$da, d2, cell$rho)
  obj <- suppressWarnings(suppressMessages(ssm_analyze(
    df0, occasions = occ, contrast = TRUE, boots = BOOTS, method = method
  )))
  ss <- obj$details$suff_stats$groups[["All"]]
  mu_hat <- ss$mean
  root <- mvn_root(ss$cov)             # the shared draw root (mvn_root)
  # Plug-in truths = the object's own point estimates (SSM params of mu-hat).
  truth_e <- obj$results$e_est[1:3]
  truth_a <- obj$results$a_est[1:3]
  truth_d <- obj$results$d_est[1:3]

  # 2. Diagnostic reported coverage (Condition c = 1).
  set.seed(BASE_SEED + 8e6 + 1e4 * cell_i + method_i)
  acc <- suppressWarnings(ssm_ci_accuracy(
    obj, reps = R1, amplitude_factors = c(1)
  ))
  cv <- acc$coverage
  rep_lab <- c("T1", "T2", "T2 - T1")
  reported <- vapply(rep_lab, function(lab) {
    vapply(c("e", "a", "d"), function(pm) {
      cv$Coverage[cv$Profile == lab & cv$Parameter == pm & cv$Condition == 1]
    }, numeric(1))
  }, numeric(3))                       # 3 params x 3 rows

  # 3. Direct empirical coverage: R2 fresh draws from MVN(mu-hat, Sigma-hat)
  # through the REAL ssm_analyze() occasions procedure (independent of the
  # diagnostic's internal replay).
  one_rep <- function(r) {
    set.seed(BASE_SEED + 1e6 * (10 * cell_i + method_i) + r)
    Z <- matrix(rnorm(cell$n * 2 * p), cell$n, 2 * p) %*% root
    dat <- sweep(Z, 2, mu_hat, "+")
    dat <- as.data.frame(dat)
    names(dat) <- c(occ1, occ2)
    res <- suppressWarnings(suppressMessages(ssm_analyze(
      dat, occasions = occ, contrast = TRUE, boots = BOOTS, method = method
    )))$results
    cover_rows(res, truth_e, truth_a, truth_d)
  }
  reps_out <- parallel::mclapply(seq_len(R2), one_rep, mc.cores = CORES)
  ok <- !vapply(reps_out, inherits, logical(1), "try-error")
  arr <- simplify2array(reps_out[ok])  # 3 x 3 x R2
  empirical <- apply(arr, c(1, 2), mean)
  dimnames(empirical) <- dimnames(reported)

  list(reported = reported, empirical = empirical,
       n_reported = R1, n_empirical = sum(ok),
       truth_e = truth_e, truth_a = truth_a, truth_d = truth_d,
       cell = cell, method = method)
}

# Cells: an interior cell (both occasions well away from zero amplitude and the
# pole; strong cross-occasion dependence) and a boundary cell (occasion 1 peaks
# ON the 0/360 pole, exercising the angular coverage machinery at the seam).
# Both carry a paired contrast (AC2: >= 1 cell exercising the contrast row).
cells <- list(
  interior = list(n = 120, e1 = 2, a1 = 1.2, d1 = 135, dd = 40,
                  de = 0.4, da = 0.2, rho = 0.5),
  pole     = list(n = 120, e1 = 2, a1 = 1.2, d1 = 0,   dd = 70,
                  de = 0.3, da = 0.2, rho = 0.5)
)
engines <- c("bootstrap", "montecarlo")

t_start <- Sys.time()
results <- list()
for (cell_i in seq_along(cells)) {
  nm <- names(cells)[cell_i]
  for (method_i in seq_along(engines)) {
    method <- engines[method_i]
    # keep cost bounded: run the pole cell on bootstrap only (the stronger,
    # genuinely-independent engine check; MC shares ssm_mc_replicates() and is
    # covered by the interior cell)
    if (nm == "pole" && method == "montecarlo") next
    cat(sprintf("[%s] cell %s / %s ...\n",
                format(Sys.time(), "%H:%M:%S"), nm, method))
    results[[paste(nm, method, sep = ".")]] <-
      run_cell(nm, cell_i, cells[[cell_i]], method, method_i)
  }
}
elapsed <- difftime(Sys.time(), t_start, units = "mins")
cat(sprintf("total runtime: %.1f min\n", as.numeric(elapsed)))

saveRDS(
  list(results = results, cells = cells, R1 = R1, R2 = R2, boots = BOOTS,
       base_seed = BASE_SEED, smoke = smoke,
       elapsed_min = as.numeric(elapsed), timestamp = Sys.time()),
  "devel/m29-ci-accuracy-occasions-oracle-results.rds"
)

# Summary: reported vs empirical, and the pre-registered band --------------
cat("\n== reported vs empirical coverage (|diff| / band) ==\n")
rep_lab <- c("T1", "T2", "T2 - T1")
for (key in names(results)) {
  x <- results[[key]]
  cat(sprintf("-- %s --\n", key))
  for (ri in 1:3) {
    for (pm_i in 1:3) {
      pm <- c("e", "a", "d")[pm_i]
      r <- x$reported[pm_i, ri]
      e <- x$empirical[pm_i, ri]
      band <- 4 * sqrt(r * (1 - r) / x$n_reported +
                         e * (1 - e) / x$n_empirical) + 0.010
      flag <- if (abs(r - e) <= band) "ok" else "FAIL"
      cat(sprintf("  %-8s %s  rep=%.3f emp=%.3f  |d|=%.3f band=%.3f  %s\n",
                  rep_lab[ri], pm, r, e, abs(r - e), band, flag))
    }
  }
}
