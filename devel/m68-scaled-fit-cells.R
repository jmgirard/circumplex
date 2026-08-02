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
#
# Every replicate seeds itself from its own pinned seed, so the result does not
# depend on the worker count or on scheduling order.

suppressMessages(devtools::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
REPS <- if (length(args) >= 1) as.integer(args[[1]]) else 2000L
CORES <- if (length(args) >= 2) as.integer(args[[2]]) else 8L

OUT <- "tests/testthat/fixtures/m68-scaled-fit-cells.rds"

# The three populations AC3 names.
#
# `strong` is RR13's own probe population, where E[T] = 261.1 against df = 273
# was measured. `weak` is Strack et al. (2013) Table 3's COC Sample 16 Other row
# -- %gen 46.7, %axes 3.2, %item 50.1 over 16 single-item positions
# (strack2013.md, p. 7) -- a real published instrument at the weak-axes,
# strong-general corner, where the metric distortion is largest. `antic` is the
# anti-conservative corner the plan names: weak axes, a dominating general
# factor, and a large item count, so df is big and the eigenvalue dispersion
# that drives the tail behaviour has the most room to show.
POPS <- list(
  strong = list(
    label = "strong-axes (RR13 probe: 8 scales x 3 items)",
    angles = octants(), k = 3L, xi1 = .35, xi2 = .10, zeta1 = .08, n = 600L
  ),
  weak = list(
    label = "weak-axes/strong-general (Strack Table 3, COC S16 Other)",
    angles = as_degree(seq(22.5, 360, by = 22.5)), k = 1L,
    xi1 = .032, xi2 = .467, zeta1 = 0, n = 600L
  ),
  antic = list(
    label = "anti-conservative corner (12 scales x 3 items, xi1 = .05)",
    angles = as_degree(seq(30, 360, by = 30)), k = 3L,
    xi1 = .05, xi2 = .60, zeta1 = .05, n = 600L
  )
)

pop_items <- function(p, nm) split(nm, rep(seq_along(p$angles), each = p$k))

# One replicate, reduced to what AC3 consumes. Both p-values are stored: `p` is
# what the package now reports and `p_unscaled` is what lavaan reported before
# the scaling, so the "with the unscaled rate recorded alongside" clause needs
# no second run.
one_rep <- function(p, seed) {
  set.seed(seed)
  mat <- as.matrix(axes_simulate(p$n, p$angles, p$k, p$xi1, p$xi2, p$zeta1))
  res <- tryCatch(
    suppressMessages(suppressWarnings(
      axes_reliability(as.data.frame(mat), items = pop_items(p, colnames(mat)),
                       angles = p$angles)
    )),
    error = function(e) NULL
  )
  if (is.null(res) || !is.null(res$details$fit_scaling_failed)) {
    return(c(chisq = NA_real_, chisq_scaled = NA_real_, df = NA_real_,
             p = NA_real_, p_unscaled = NA_real_, cfactor = NA_real_))
  }
  c(
    chisq = res$details$fit_uncorrected$chisq,
    chisq_scaled = res$fit$chisq,
    df = res$fit$df,
    p = res$fit$pvalue,
    p_unscaled = res$details$fit_uncorrected$pvalue,
    cfactor = unname(res$details$scaling_factor[["model"]])
  )
}

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

# ---- run --------------------------------------------------------------------

t0 <- Sys.time()
cells <- list()
diags <- list()
for (nm in names(POPS)) {
  p <- POPS[[nm]]
  message("population `", nm, "`: ", REPS, " replicates -- ", p$label)
  seeds <- switch(nm, strong = 10000L, weak = 20000L, antic = 30000L) +
    seq_len(REPS)
  cells[[nm]] <- do.call(rbind, parallel::mclapply(
    seeds, function(s) one_rep(p, s), mc.cores = CORES
  ))
  diags[[nm]] <- pop_diagnostic(p)
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
    seeds = list(strong = 10000L + seq_len(REPS),
                 weak = 20000L + seq_len(REPS),
                 antic = 30000L + seq_len(REPS)),
    reps = REPS,
    elapsed_min = elapsed,
    r_version = R.version.string,
    lavaan_version = as.character(utils::packageVersion("lavaan"))
  ),
  cells = cells,
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
message(sprintf("elapsed: %.1f min", elapsed))

saveRDS(out, OUT)
message("wrote ", OUT)
