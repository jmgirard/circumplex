# M68 -- the complete-data calibration cells for the scaled global test statistic.
#
# AC3 asks two things of the scaled statistic at each of three populations:
# mean(T_s)/df in [0.97, 1.03], and the empirical rejection rate of the reported
# p-value at alpha = .05 in [.036, .064] (RR13's Q5 band, +/- 2.8 MC SE at 2000
# replicates). The two are different claims -- the first is about the centre of
# the distribution and the second about its 95th percentile -- and the scaling
# correction only ever promised the first (satorra1994 p. 407: the scaled
# statistic and chi-square "agree in mean").
#
# So this script measures BOTH, and also measures the Satterthwaite-type
# ADJUSTED statistic as a diagnostic, because that is the alternative Satorra &
# Bentler offer for exactly the case where matching the mean is not enough
# (satorra1994 pp. 407-409). Its degrees of freedom are fractional:
#
#   d' = (tr{U Gamma})^2 / tr{(U Gamma)^2}       T_a = (d' / tr{U Gamma}) T
#
# The adjusted quantities are computed ONCE PER POPULATION at the population
# correlation matrix rather than per replicate, through the explicit vech-space
# route below. That is deliberate and is a diagnostic rather than a shippable
# estimator: it answers "would matching the variance too fix the tail?" without
# paying for a p* x p* trace on every one of 6000 fits. A real adjusted
# statistic would need d'-hat per fit.
#
# Usage (from the package root):
#   Rscript devel/m68-scaled-fit-cells.R            # full run, 2000 reps
#   Rscript devel/m68-scaled-fit-cells.R 50 4       # 50 reps, 4 workers (smoke)
#   Rscript devel/m68-scaled-fit-cells.R 2000 8 verify   # re-run, compare, DO
#                                                        # NOT write
#
# Every replicate seeds itself from its own pinned seed, so the result does not
# depend on the worker count or on scheduling order.
#
# `verify` is AC9's same-environment exact-reproduction arm at full scale: it
# regenerates every cell and compares against the committed fixture instead of
# overwriting it, reporting the maximum absolute discrepancy per stored column
# and the six rejection rates. The suite carries the cheap standing version of
# the same check (a handful of replicates re-run from their own seeds, in
# test-axes-scaled-fit.R); this is the whole-fixture confirmation, run by hand
# because it costs ~5 minutes of fitting.
#
# The populations, the seed formula and the replicate function live in
# tests/testthat/helper-m68-cells.R so that the suite runs THIS generator's own
# replicate function rather than a copy (M68 review, F6/F7).

suppressMessages(devtools::load_all(quiet = TRUE))
source("tests/testthat/helper-m68-cells.R")

args <- commandArgs(trailingOnly = TRUE)
REPS <- if (length(args) >= 1) as.integer(args[[1]]) else 2000L
CORES <- if (length(args) >= 2) as.integer(args[[2]]) else 8L
VERIFY <- length(args) >= 3 && identical(args[[3]], "verify")

OUT <- "tests/testthat/fixtures/m68-scaled-fit-cells.rds"

POPS <- m68_pops
pop_items <- m68_pop_items
one_rep <- m68_one_rep

# ---- the vech-space diagnostic, at the POPULATION matrix ---------------------
#
# Deliberately the dumb explicit route, and the same construction the AC2 oracle
# in tests/testthat/test-axes-scaled-fit.R uses. It runs three times in total.

dup_matrix <- function(p) {
  pstar <- p * (p + 1) / 2
  D <- matrix(0, p * p, pstar)
  k <- 0L
  for (j in seq_len(p)) for (i in j:p) {
    k <- k + 1L
    D[(j - 1) * p + i, k] <- 1
    D[(i - 1) * p + j, k] <- 1
  }
  D
}

# tr{U Gamma_R} and tr{(U Gamma_R)^2} at a population correlation matrix, from
# which both the scaling factor and the Satterthwaite df follow.
pop_traces <- function(sigma, mats) {
  p <- nrow(sigma)
  pstar <- p * (p + 1) / 2
  D <- dup_matrix(p)
  Dp <- solve(t(D) %*% D) %*% t(D)
  si <- solve(sigma)
  V <- 0.5 * t(D) %*% kronecker(si, si) %*% D
  Gs <- 2 * Dp %*% kronecker(sigma, sigma) %*% t(Dp)

  idx <- which(lower.tri(matrix(0, p, p), diag = TRUE), arr.ind = TRUE)
  J <- matrix(0, pstar, pstar)
  for (a in seq_len(pstar)) {
    i <- idx[a, 1]; j <- idx[a, 2]
    if (i == j) next
    J[a, a] <- 1
    ai <- which(idx[, 1] == i & idx[, 2] == i)
    aj <- which(idx[, 1] == j & idx[, 2] == j)
    J[a, ai] <- J[a, ai] - 0.5 * sigma[i, j]
    J[a, aj] <- J[a, aj] - 0.5 * sigma[i, j]
  }
  Gr <- J %*% Gs %*% t(J)

  vech <- function(M) M[lower.tri(M, diag = TRUE)]
  Delta <- vapply(mats, vech, numeric(pstar))
  U <- V - V %*% Delta %*% solve(t(Delta) %*% V %*% Delta) %*% t(Delta) %*% V
  UG <- U %*% Gr
  list(tr1 = sum(diag(UG)), tr2 = sum(UG * t(UG)),
       df = pstar - ncol(Delta))
}

pop_diagnostic <- function(p) {
  pop <- axes_population_cor(p$angles, p$k, p$xi1, p$xi2, p$zeta1)
  nm <- sprintf("item_%02d", seq_len(nrow(pop$sigma)))
  dimnames(pop$sigma) <- list(nm, nm)
  d <- axes_se_derivs(rep(as.numeric(p$angles), each = p$k), pop$scale, NULL,
                      p$k > 1L, FALSE)
  tr <- pop_traces(pop$sigma, d$mats)
  list(
    df = tr$df,
    # satorra1994 eq. 16.22, p. 407.
    cfactor = tr$tr1 / tr$df,
    # The Satterthwaite degrees of freedom, satorra1994 p. 409.
    df_adj = tr$tr1^2 / tr$tr2,
    # The multiplier that takes T to the adjusted statistic.
    adj_mult = (tr$tr1^2 / tr$tr2) / tr$tr1
  )
}

# The sample-size sweep. This is the cell that separates the two candidate
# explanations for a rejection rate above nominal at N = 600: an error in the
# scaling factor, or the ML chi-square's own finite-sample upward bias. The
# factor is a function of the population matrix and does not move with N, so if
# the residual shrinks as N grows the factor is not what is producing it.
# Run at the strong-axes population, whose c is pinned independently by AC2's
# closed-form oracle.
NSWEEP <- c(600L, 1200L, 2400L, 4800L)

# ---- run --------------------------------------------------------------------

t0 <- Sys.time()
cells <- list()
diags <- list()
for (nm in names(POPS)) {
  p <- POPS[[nm]]
  message("population `", nm, "`: ", REPS, " replicates -- ", p$label)
  seeds <- m68_seeds(nm, REPS)
  cells[[nm]] <- do.call(rbind, parallel::mclapply(
    seeds, function(s) one_rep(p, s), mc.cores = CORES
  ))
  diags[[nm]] <- pop_diagnostic(p)
}

sweep <- list()
for (nn in NSWEEP) {
  message("sample-size sweep at N = ", nn, ": ", REPS, " replicates")
  p <- POPS$strong
  p$n <- nn
  sweep[[as.character(nn)]] <- do.call(rbind, parallel::mclapply(
    m68_sweep_seeds(nn, REPS), function(s) one_rep(p, s), mc.cores = CORES
  ))
}
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))

out <- list(
  provenance = list(
    source = paste(
      "M68 AC3; three complete-data populations, one of them Strack et al.",
      "(2013) Table 3 COC S16 Other (strack2013.md, p. 7)"
    ),
    generator = "devel/m68-scaled-fit-cells.R",
    populations = POPS,
    seeds = list(strong = m68_seeds("strong", REPS),
                 weak = m68_seeds("weak", REPS),
                 antic = m68_seeds("antic", REPS)),
    reps = REPS,
    elapsed_min = elapsed,
    r_version = R.version.string,
    lavaan_version = as.character(utils::packageVersion("lavaan"))
  ),
  cells = cells,
  sweep = sweep,
  sweep_n = NSWEEP,
  sweep_population = "strong",
  population_diagnostics = diags
)

report <- function(nm) {
  x <- cells[[nm]]
  ok <- stats::complete.cases(x)
  df <- x[which(ok)[1], "df"]
  dg <- diags[[nm]]
  # The adjusted statistic, applied post hoc with the population's own d'.
  t_adj <- x[ok, "chisq"] * dg$adj_mult
  message(sprintf(
    paste0("%-7s n_ok=%4d df=%3d | mean(T)/df=%.4f mean(Ts)/df=%.4f | ",
           "rej: unscaled=%.4f scaled=%.4f adjusted=%.4f | ",
           "sd(Ts)/sqrt(2df)=%.4f | c_pop=%.4f d'=%.1f"),
    nm, sum(ok), df,
    mean(x[ok, "chisq"]) / df, mean(x[ok, "chisq_scaled"]) / df,
    mean(x[ok, "p_unscaled"] < .05), mean(x[ok, "p"] < .05),
    mean(stats::pchisq(t_adj, dg$df_adj, lower.tail = FALSE) < .05),
    stats::sd(x[ok, "chisq_scaled"]) / sqrt(2 * df),
    dg$cfactor, dg$df_adj
  ))
}
for (nm in names(POPS)) report(nm)

for (nn in names(sweep)) {
  x <- sweep[[nn]]
  ok <- stats::complete.cases(x)
  df <- x[which(ok)[1], "df"]
  rej <- mean(x[ok, "p"] < .05)
  message(sprintf(
    paste0("sweep N=%5s n_ok=%4d | mean(T)/df=%.4f mean(Ts)/df=%.4f | ",
           "sd(Ts)/sqrt(2df)=%.4f | rej: unscaled=%.4f scaled=%.4f (+-%.4f)"),
    nn, sum(ok), mean(x[ok, "chisq"]) / df, mean(x[ok, "chisq_scaled"]) / df,
    stats::sd(x[ok, "chisq_scaled"]) / sqrt(2 * df),
    mean(x[ok, "p_unscaled"] < .05), rej, sqrt(rej * (1 - rej) / sum(ok))
  ))
}
message(sprintf("elapsed: %.1f min", elapsed))

if (VERIFY) {
  # AC9's exact-reproduction arm, at full scale. Same seeds, same environment =>
  # the stored numbers must come back bit-for-bit; anything above 1e-12 on a
  # stored column means the harness and the fixture have parted company, which
  # is a regression and not a tolerance question. The rejection rates are
  # reported too, because they are what AC9 fences and a reader should not have
  # to trust that they follow from the columns.
  old <- readRDS(OUT)
  message("verify against ", OUT,
          " (fixture: ", old$provenance$r_version,
          ", lavaan ", old$provenance$lavaan_version, ")")
  worst <- 0
  for (grp in c("cells", "sweep")) {
    for (nm in names(out[[grp]])) {
      a <- out[[grp]][[nm]]
      b <- old[[grp]][[nm]]
      if (!identical(dim(a), dim(b))) {
        message(sprintf("  %-6s %-6s DIMENSIONS DIFFER", grp, nm))
        next
      }
      d <- max(abs(a - b), na.rm = TRUE)
      worst <- max(worst, d)
      message(sprintf("  %-6s %-6s max|diff| = %.3e | rej new=%.4f old=%.4f",
                      grp, nm, d,
                      mean(a[, "p"] < .05, na.rm = TRUE),
                      mean(b[, "p"] < .05, na.rm = TRUE)))
    }
  }
  message(sprintf("worst discrepancy over every stored cell: %.3e", worst))
  message("verify mode: ", OUT, " NOT rewritten")
} else {
  saveRDS(out, OUT)
  message("wrote ", OUT)
}
