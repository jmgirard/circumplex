# M66 T4 bridge probe (AC4).
#
# RR13 BC4 asks two things that the committed fixture cannot both serve
# literally: the corrected FIML SE must divide by "the same per-parameter ratio
# evaluated at Sigma-hat", AND the calibration must be checked "against the
# committed 200-replicate fixture". The fixture stores fiml.xi1 / fiml.se and no
# Sigma-hat, so the per-replicate ratio is not reconstructible from it. RR13's
# own cited numbers (1.001/1.008/1.018) came from dividing by the POPULATION
# constant 1.4412 (its appendix says so).
#
# The plan gate chose the bridge: re-fit a subset of the stored seeds, measure
# whether the per-Sigma-hat ratio and the population constant agree within
# Monte-Carlo noise, and only then use the constant over all 600 replicates.
# If they disagree, the constant is not a valid proxy and the full regeneration
# is required -- that is the falsifier recorded in the plan.

suppressMessages(devtools::load_all("/Users/jmgirard/GitHub/circumplex", quiet = TRUE))

oct <- octants()
fx <- readRDS("tests/testthat/fixtures/m65-heavy-cells.rds")
seeds <- fx$provenance$seeds$mcar
N_BRIDGE <- 20L
POP_RATIO <- 1.4412  # RR13's derived constant at the probe population

draw <- function(n, seed) {
  set.seed(seed)
  as.matrix(axes_simulate(n, oct, 3L, .35, .10, .08))
}
items_of <- function(mat) split(colnames(mat), rep(1:8, each = 3))

bridge <- function(rate) {
  out <- vapply(seq_len(N_BRIDGE), function(r) {
    mat <- axes_mcar(draw(600L, seeds[[r]]), rate)
    res <- suppressMessages(suppressWarnings(
      axes_reliability(as.data.frame(mat), items = items_of(mat),
                       angles = oct, missing = "fiml")
    ))
    naive <- res$details$se_uncorrected[["xi1"]]
    # What the shipped code now reports: the per-Sigma-hat composition.
    per_sigma <- res$components$SE[res$components$Symbol == "xi1"]
    c(naive = naive,
      per_sigma = per_sigma,
      by_const = naive / POP_RATIO,
      ratio_at_sigma = naive / per_sigma)
  }, numeric(4))
  t(out)
}

cat("=== Bridge: per-Sigma-hat vs population-constant correction ===\n")
cat(sprintf("%-6s %10s %10s %12s %12s %10s\n",
            "rate", "mean r_hat", "pop const", "mean |diff|", "max |diff|",
            "rel diff"))
for (rate in c(0.02, 0.05, 0.10)) {
  b <- bridge(rate)
  d <- abs(b[, "per_sigma"] - b[, "by_const"])
  cat(sprintf("%-6.2f %10.5f %10.5f %12.3e %12.3e %10.5f\n",
              rate, mean(b[, "ratio_at_sigma"]), POP_RATIO,
              mean(d), max(d), mean(d) / mean(b[, "per_sigma"])))
}

cat("\n=== AC4: calibration over all 200 stored replicates per cell ===\n")
cat(sprintf("%-6s %8s %12s %12s %10s\n",
            "rate", "R", "mean corr SE", "emp SD(xi1)", "calib"))
for (i in seq_along(fx$mcar)) {
  rate <- names(fx$mcar)[[i]]
  cell <- fx$mcar[[i]]
  corr_se <- cell[, "fiml.se"] / POP_RATIO
  emp_sd <- stats::sd(cell[, "fiml.xi1"])
  cat(sprintf("%-6s %8d %12.6f %12.6f %10.4f\n",
              rate, nrow(cell), mean(corr_se), emp_sd,
              mean(corr_se) / emp_sd))
}
cat("\nAC4 band is [0.90, 1.10]; RR13 measured 1.001 / 1.008 / 1.018.\n")
