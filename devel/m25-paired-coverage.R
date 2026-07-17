# M25 paired-occasion contrast coverage oracle (spec sec. 2.3)
# ==============================================================================
# Simulates wide two-occasion (and one three-occasion) samples from known
# bivariate populations with controlled within-person cross-occasion
# covariance, runs ssm_analyze(occasions=) through BOTH engines, and checks:
#   (1) simulation-coverage: paired contrast CIs (de, da, dd) cover the known
#       truths at nominal rate;
#   (2) the RR06 conditional-efficiency identities, DISCRIMINATINGLY: the
#       paired/re-paired Var(dd-hat) ratio tracks 1 - rho*cos(dd), including
#       the reversal cell (dd = 135, ratio > 1) -- the unconditional "paired
#       is narrower" claim must fail here by design;
#   (3) the degenerate-dependence invariant: re-paired persons (dependence
#       destroyed; truth unchanged) still cover at nominal;
#   (4) exact-identity check: empirical Var(de-hat) equals the textbook
#       paired identity 2*sigma^2*(1 - rho)/(p*n) (isotropic population).
#
# PRE-REGISTERED ACCEPTANCE (fixed before the full run; smoke runs exercise
# code paths only and carry no evidentiary weight):
#   - Coverage band, n = 100 cells (both engines, de/da/dd): [.91, .98]
#     at reps = 500, nominal .95 (binomial 3*SE ~ .029, plus B = 600
#     quantile noise; M19 precedent band [.90, .98]).
#   - Small-n cell (n = 30): bootstrap band [.89, .98]; the Monte Carlo arm
#     is MEASURED AND REPORTED, not gated (known small-n anticonservatism of
#     the known-Sigma-hat normal approximation; spec sec. 2.2 -- the
#     percentile bootstrap stays the small-n answer).
#   - Efficiency ratios (paired var / re-paired var of dd-hat and da-hat,
#     500 reps each arm): within [0.70, 1.30] * (1 - rho*cos(dd)) -- a
#     variance-ratio band ~4 SEs wide at reps = 500; direction (>1 vs <1)
#     must be correct at dd = 135 (reversal) and dd = 30.
#   - Var(de-hat) exact identity: empirical/theory ratio in [0.80, 1.25].
#   - k = 3 cell: per-occasion profile d coverage in [.91, .98] (MC engine).
# Seeds are level-indexed (LESSONS 2026-07-13): seed = 1e6*cell_index + rep.
# Regenerate: Rscript devel/m25-paired-coverage.R          (full, ~10-20 min)
#             M25_SMOKE=1 Rscript devel/m25-paired-coverage.R   (smoke)
# Output: devel/m25-paired-coverage-results.rds
# ==============================================================================

devtools::load_all(".", quiet = TRUE)

smoke <- nzchar(Sys.getenv("M25_SMOKE"))
reps <- if (smoke) 10 else 500
boots <- if (smoke) 100 else 600

p <- 8
angles_deg <- octants()
ang <- as.numeric(angles_deg) * pi / 180
scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
occ1 <- paste0(scales, "_1")
occ2 <- paste0(scales, "_2")

# Population: person i draws (block1, block2) ~ MVN(mu, Sigma) with
# mu_j = e_j + a_j * cos(ang - d_j), Sigma = [[s2 I, rho s2 I], [rho s2 I, s2 I]]
# (isotropic within and across occasions). At octants (equal spacing) the
# closed-form SSM estimator recovers (e_j, a_j, d_j) from mu_j exactly, so
# the population truths below are exact.
simulate_cell <- function(n, e1, a1, d1, e2, a2, d2, rho, s2 = 1) {
  mu1 <- e1 + a1 * cos(ang - d1 * pi / 180)
  mu2 <- e2 + a2 * cos(ang - d2 * pi / 180)
  z1 <- matrix(rnorm(n * p), n, p)
  z_shared <- z1 * rho + matrix(rnorm(n * p), n, p) * sqrt(1 - rho^2)
  # Cov(block1, block2) = rho * s2 * I; each block marginal s2 * I
  b1 <- sqrt(s2) * z1
  b2 <- sqrt(s2) * z_shared
  df <- data.frame(sweep(b1, 2, mu1, "+"), sweep(b2, 2, mu2, "+"))
  names(df) <- c(occ1, occ2)
  df
}

angdist_deg <- function(x, y) ((x - y + 180) %% 360) - 180

# Cell grid --------------------------------------------------------------------
cells <- list(
  base      = list(n = 100, d1 = 90,  dd = 30,  rho = 0.6, de = 0.4, da = 0.3),
  dd_near0  = list(n = 100, d1 = 90,  dd = 2,   rho = 0.6, de = 0.4, da = 0.3),
  dd_178    = list(n = 100, d1 = 90,  dd = 178, rho = 0.6, de = 0.4, da = 0.3),
  pole      = list(n = 100, d1 = 355, dd = 20,  rho = 0.6, de = 0.4, da = 0.3),
  small_n   = list(n = 30,  d1 = 90,  dd = 30,  rho = 0.6, de = 0.4, da = 0.3),
  reversal  = list(n = 100, d1 = 45,  dd = 135, rho = 0.6, de = 0.4, da = 0.3)
)
e1 <- 2; a1 <- 1.5

run_cell <- function(cell_name, cell_i, cell, repair = FALSE) {
  n <- cell$n; d1 <- cell$d1; dd <- cell$dd; rho <- cell$rho
  d2 <- (d1 + dd) %% 360
  e2 <- e1 + cell$de; a2 <- a1 + cell$da
  true_dd <- angdist_deg(d2, d1)
  out <- vector("list", reps)
  for (r in seq_len(reps)) {
    set.seed(1e6 * cell_i + r + if (repair) 5e5 else 0)
    df <- simulate_cell(n, e1, a1, d1, e2, a2, d2, rho)
    if (repair) {
      # Independent baseline: occasion 2 comes from a FRESH set of n persons
      # (an independent-groups design run through the occasions code path).
      # NOT a within-sample permutation: group means are permutation-
      # invariant, so permuting rows leaves every mean-based estimate
      # unchanged and the "re-paired" estimator distribution would stay the
      # paired one while the engines see independent data -- vacuous as an
      # estimator baseline and incoherent for coverage (caught in the first
      # full run, 2026-07-16: base_repaired overcovered at .99 and
      # reversal_repaired undercovered at .87, exactly the paired/CI-width
      # mismatch this note describes).
      df[occ2] <- simulate_cell(n, e1, a1, d1, e2, a2, d2, rho)[occ2]
    }
    row <- list()
    for (method in c("bootstrap", "montecarlo")) {
      res <- suppressWarnings(suppressMessages(ssm_analyze(
        df, occasions = list(T1 = occ1, T2 = occ2),
        contrast = TRUE, boots = boots, method = method
      )))$results
      i <- 3
      # dd truth coverage on the estimate's branch: distance from interval
      # endpoints handled via angular distance to tolerate branch shifts
      dd_cov <- angdist_deg(true_dd, res$d_lci[i]) >= 0 &&
        angdist_deg(res$d_uci[i], true_dd) >= 0
      row[[method]] <- c(
        de_est = res$e_est[i], de_cov = res$e_lci[i] <= cell$de &&
          res$e_uci[i] >= cell$de,
        da_cov = res$a_lci[i] <= cell$da && res$a_uci[i] >= cell$da,
        dd_est = res$d_est[i], dd_cov = dd_cov,
        de_width = res$e_uci[i] - res$e_lci[i],
        dd_width = res$d_uci[i] - res$d_lci[i],
        da_est = res$a_est[i]
      )
    }
    out[[r]] <- row
  }
  out
}

t_start <- Sys.time()
results <- list()
for (cell_i in seq_along(cells)) {
  nm <- names(cells)[cell_i]
  cat(sprintf("[%s] cell %s (paired) ...\n", format(Sys.time(), "%H:%M:%S"), nm))
  results[[nm]] <- run_cell(nm, cell_i, cells[[nm]], repair = FALSE)
  if (nm %in% c("base", "reversal")) {
    cat(sprintf("[%s] cell %s (re-paired) ...\n",
                format(Sys.time(), "%H:%M:%S"), nm))
    results[[paste0(nm, "_repaired")]] <-
      run_cell(nm, cell_i, cells[[nm]], repair = TRUE)
  }
}

# k = 3 profile-coverage cell (MC engine): three cosine occasions ---------------
cat(sprintf("[%s] cell k3 ...\n", format(Sys.time(), "%H:%M:%S")))
occ3 <- paste0(scales, "_3")
k3_i <- length(cells) + 1
k3 <- vector("list", reps)
for (r in seq_len(reps)) {
  set.seed(1e6 * k3_i + r)
  df <- simulate_cell(100, e1, a1, 90, e1 + 0.4, a1 + 0.3, 120, 0.6)
  b3 <- simulate_cell(100, e1 + 0.8, a1, 150, e1, a1, 150, 0)[occ1]
  names(b3) <- occ3
  df <- cbind(df, b3) # occasion 3 independent of 1-2 (a legal population)
  res <- suppressWarnings(ssm_analyze(
    df, occasions = list(T1 = occ1, T2 = occ2, T3 = occ3),
    boots = boots, method = "montecarlo"
  ))$results
  truths <- c(90, 120, 150)
  k3[[r]] <- vapply(1:3, function(i) {
    angdist_deg(truths[i], res$d_lci[i]) >= 0 &&
      angdist_deg(res$d_uci[i], truths[i]) >= 0
  }, logical(1))
}

elapsed <- difftime(Sys.time(), t_start, units = "mins")
cat(sprintf("total runtime: %.1f min\n", as.numeric(elapsed)))

saveRDS(
  list(results = results, k3 = k3, cells = cells, reps = reps, boots = boots,
       e1 = e1, a1 = a1, smoke = smoke, elapsed_min = as.numeric(elapsed),
       timestamp = Sys.time()),
  "devel/m25-paired-coverage-results.rds"
)

# Summary ----------------------------------------------------------------------
getm <- function(cell, method, field) {
  vapply(results[[cell]], function(r) r[[method]][[field]], numeric(1))
}
cat("\n== coverage (de / da / dd) ==\n")
for (nm in names(results)) {
  for (method in c("bootstrap", "montecarlo")) {
    cat(sprintf(
      "%-18s %-11s %.3f / %.3f / %.3f\n", nm, method,
      mean(getm(nm, method, "de_cov")), mean(getm(nm, method, "da_cov")),
      mean(getm(nm, method, "dd_cov"))
    ))
  }
}
cat("\n== k3 profile d coverage (MC) ==\n")
cat(sprintf("%.3f %.3f %.3f\n", mean(sapply(k3, `[`, 1)),
            mean(sapply(k3, `[`, 2)), mean(sapply(k3, `[`, 3))))

cat("\n== efficiency ratios: Var(dd-hat) paired / re-paired ==\n")
for (nm in c("base", "reversal")) {
  cell <- cells[[nm]]
  theory <- 1 - cell$rho * cos(cell$dd * pi / 180)
  for (method in c("bootstrap", "montecarlo")) {
    vr_dd <- var(getm(nm, method, "dd_est")) /
      var(getm(paste0(nm, "_repaired"), method, "dd_est"))
    vr_da <- var(getm(nm, method, "da_est")) /
      var(getm(paste0(nm, "_repaired"), method, "da_est"))
    cat(sprintf("%-9s %-11s dd: %.3f  da: %.3f  (theory %.3f)\n",
                nm, method, vr_dd, vr_da, theory))
  }
}

cat("\n== Var(de-hat) exact paired identity ==\n")
for (nm in c("base", "reversal")) {
  cell <- cells[[nm]]
  theory <- 2 * (1 - cell$rho) / (p * cell$n)
  emp <- var(getm(nm, "montecarlo", "de_est"))
  cat(sprintf("%-9s empirical %.5g theory %.5g ratio %.3f\n",
              nm, emp, theory, emp / theory))
}
