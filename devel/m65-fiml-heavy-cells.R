# M65 -- the heavy evidence cells for axes_reliability(missing = "fiml").
#
# BC10 (MCAR recovery), BC13 (SE behaviour), BC11 (MAR reversal) and BC12
# (metric falsification) each need more replicates than a test suite can carry:
# the structured cfa(missing = "ml") measures 18-68 s per fit under realistic
# MAR missingness, so the four cells together run well over an hour. This script
# runs them once, seed-pinned, and commits a summary the suite asserts against
# (M65-D3, extended to BC11/BC12 at the T5 gate).
#
# What the suite does with the output: tests/testthat/test-axes-fiml.R asserts
# the stored summary against every criterion AND re-runs a live smoke, so a
# stored number is never the only thing standing between a broken estimator and
# a green suite. The live half is deliberately smaller than the stored half --
# the test says exactly what it does not cover.
#
# Usage (from the package root):
#   Rscript devel/m65-fiml-heavy-cells.R            # full run, 6 workers
#   Rscript devel/m65-fiml-heavy-cells.R 20 4       # 20 reps, 4 workers (smoke)
#
# Every replicate seeds itself from its own pinned seed, so the result does not
# depend on the worker count or on scheduling order.

suppressMessages(devtools::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
REPS <- if (length(args) >= 1) as.integer(args[[1]]) else 200L
CORES <- if (length(args) >= 2) as.integer(args[[2]]) else 6L

OUT <- "tests/testthat/fixtures/m65-heavy-cells.rds"

# --- the probe population (RR12 BC10) ----------------------------------------
OCT <- octants()
K_SCALES <- 8L
K_ITEMS <- 3L
TRUTH <- c(xi1 = .35, xi2 = .10, zeta1 = .08)

probe_draw <- function(n, seed) {
  set.seed(seed)
  as.matrix(axes_simulate(n, OCT, K_ITEMS, TRUTH[["xi1"]], TRUTH[["xi2"]],
                          TRUTH[["zeta1"]]))
}

probe_items <- function(mat) split(colnames(mat), rep(seq_len(K_SCALES),
                                                      each = K_ITEMS))

# --- the two MAR mechanisms ---------------------------------------------------
# Defined in the PACKAGE (R/axes_fiml.R), not here, so this harness and the
# suite's live re-run generate the same missingness from the same seed. A local
# copy would let the stored summary and the test that checks it drift apart
# silently, which is the one failure a stored fixture must not have.
mech_m1 <- function(mat) axes_mar_m1(mat, K_ITEMS)
mech_m2 <- function(mat) axes_mar_m2(mat, K_ITEMS)
mech_mcar <- function(mat, rate) axes_mcar(mat, rate)

# --- one fit, reduced to the numbers the criteria consume ---------------------
# NA rather than an abort on a refusal: a cell is allowed to lose replicates
# (listwise at 10% MCAR keeps only ~48 complete cases of 600), and the count of
# losses is itself reported, because silently averaging over the survivors of a
# path that failed half the time would flatter that path.
fit_xi1 <- function(mat, ...) {
  res <- tryCatch(
    suppressMessages(suppressWarnings(
      axes_reliability(as.data.frame(mat), items = probe_items(mat),
                       angles = OCT, ...)
    )),
    error = function(e) NULL
  )
  if (is.null(res) || isTRUE(res$results$boundary[[1]])) {
    return(c(xi1 = NA_real_, se = NA_real_, ols = NA_real_))
  }
  c(
    xi1 = res$results$xi1[[1]],
    se = res$components$SE[res$components$Symbol == "xi1"],
    ols = unname(res$details$ols_shadow[["xi1"]])
  )
}

# --- BC10 + BC13: MCAR recovery and SE behaviour ------------------------------
# One set of replicates serves both criteria: BC10 reads the point estimates,
# BC13 reads the reported SEs beside them. Drawing them twice would let the two
# criteria disagree about the same cell.
rep_mcar <- function(seed, rate) {
  mat <- mech_mcar(probe_draw(600L, seed), rate)
  fi <- fit_xi1(mat, missing = "fiml")
  lw <- fit_xi1(mat, missing = "listwise")
  c(fiml = fi, lw_xi1 = lw[["xi1"]], lw_se = lw[["se"]],
    n_complete = sum(stats::complete.cases(mat)))
}

# --- BC11: the MAR reversal ---------------------------------------------------
rep_m1 <- function(seed) {
  mat <- mech_m1(probe_draw(2400L, seed))
  c(fiml = fit_xi1(mat, missing = "fiml")[["xi1"]],
    listwise = fit_xi1(mat, missing = "listwise")[["xi1"]])
}

# --- BC12: metric falsification, paired on identical draws --------------------
# Three routes over the SAME data, which is the whole design: the comparison is
# between metrics, so anything that differs between them other than the metric
# would confound it. The available-case route is built here rather than in the
# package because the package refuses to offer it.
rep_m2 <- function(seed) {
  mat <- mech_m2(probe_draw(2000L, seed))
  items <- probe_items(mat)
  cvg <- axes_fiml_coverage(mat)
  kept <- mat[cvg$keep, , drop = FALSE]
  mom <- axes_fiml_moments(kept)

  shipped <- fit_xi1(mat, missing = "fiml")[["xi1"]]

  # (1) Available-case standardization, then the same one-stage FIML fit. The
  # ONLY difference from the shipped path is which moments did the
  # standardizing -- RR12's load-bearing claim is that this alone moves xi1.
  ac_mean <- colMeans(kept, na.rm = TRUE)
  ac_sd <- apply(kept, 2, stats::sd, na.rm = TRUE)
  zac <- sweep(sweep(kept, 2L, ac_mean, "-"), 2L, ac_sd, "/")
  ac <- tryCatch(
    {
      f <- suppressWarnings(axes_fit(as.data.frame(zac), items, OCT,
                                     missing = "fiml"))
      pe <- lavaan::parameterEstimates(f)
      pe$est[pe$op == "~~" & pe$lhs == "AX" & pe$rhs == "AX"][[1]]
    },
    error = function(e) NA_real_
  )

  # (2) Two-stage: fit R-hat as a covariance matrix at N_used. A metric-correct
  # route with the wrong information claim, so it should track the shipped
  # path's POINT estimate closely -- the SEs are where the two part company.
  two <- tryCatch(
    {
      f <- suppressWarnings(axes_fit_cormat(mom$R, items, OCT, n = cvg$n_used))
      pe <- lavaan::parameterEstimates(f)
      pe$est[pe$op == "~~" & pe$lhs == "AX" & pe$rhs == "AX"][[1]]
    },
    error = function(e) NA_real_
  )
  c(shipped = shipped, available_case = ac, two_stage = two)
}

# --- run ----------------------------------------------------------------------

run <- function(seeds, fn, ...) {
  rows <- parallel::mclapply(seeds, fn, ..., mc.cores = CORES)
  do.call(rbind, rows)
}

t0 <- Sys.time()
message("BC10/BC13: 3 MCAR cells x ", REPS, " replicates ...")
RATES <- c(`0.02` = 0.02, `0.05` = 0.05, `0.10` = 0.10)
mcar <- lapply(RATES, function(r) {
  message("  rate ", r)
  run(seq_len(REPS) + 1000L, rep_mcar, rate = r)
})

message("BC11: 5 M1 replicates at N = 2400 ...")
m1 <- run(500L + seq_len(5L), rep_m1)

message("BC12: 4 M2 replicates at N = 2000 ...")
m2 <- run(700L + seq_len(4L), rep_m2)

elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))

summary_of <- function(x) {
  ok <- !is.na(x)
  c(mean = mean(x[ok]), sd = stats::sd(x[ok]), n = sum(ok))
}

out <- list(
  provenance = list(
    source = "M65, RR12 BC10/BC11/BC12/BC13; probe population 8 scales x 3 items",
    generator = "devel/m65-fiml-heavy-cells.R",
    truth = TRUTH,
    seeds = list(mcar = seq_len(REPS) + 1000L, m1 = 500L + seq_len(5L),
                 m2 = 700L + seq_len(4L)),
    reps = REPS,
    elapsed_min = elapsed,
    r_version = R.version.string,
    lavaan_version = as.character(utils::packageVersion("lavaan"))
  ),
  mcar = mcar,
  m1 = m1,
  m2 = m2
)

dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
saveRDS(out, OUT)

# --- report -------------------------------------------------------------------
cat("\n=== BC10: mean xi1 vs truth", TRUTH[["xi1"]], "===\n")
for (nm in names(mcar)) {
  x <- mcar[[nm]]
  s <- summary_of(x[, "fiml.xi1"])
  mcse <- s[["sd"]] / sqrt(s[["n"]])
  cat(sprintf(
    "  %5s MCAR  mean %.4f  MCSE %.4f  |bias|/MCSE %.2f  n=%d  max|ols-cfa| %.4f\n",
    nm, s[["mean"]], mcse, abs(s[["mean"]] - TRUTH[["xi1"]]) / mcse, s[["n"]],
    max(abs(x[, "fiml.ols"] - x[, "fiml.xi1"]), na.rm = TRUE)
  ))
}
cat("\n=== BC13: reported SEs ===\n")
for (nm in names(mcar)) {
  x <- mcar[[nm]]
  cat(sprintf(
    "  %5s MCAR  mean SE fiml %.5f  listwise %.5f  ratio %.3f  sd(xi1) %.5f  SE/sd %.3f  lw n=%d\n",
    nm, mean(x[, "fiml.se"], na.rm = TRUE), mean(x[, "lw_se"], na.rm = TRUE),
    mean(x[, "fiml.se"], na.rm = TRUE) / mean(x[, "lw_se"], na.rm = TRUE),
    stats::sd(x[, "fiml.xi1"], na.rm = TRUE),
    mean(x[, "fiml.se"], na.rm = TRUE) / stats::sd(x[, "fiml.xi1"], na.rm = TRUE),
    sum(!is.na(x[, "lw_xi1"]))
  ))
}
cat("\n=== BC11: MAR reversal (M1, N = 2400) ===\n")
for (col in c("fiml", "listwise")) {
  s <- summary_of(m1[, col])
  mcse <- s[["sd"]] / sqrt(s[["n"]])
  cat(sprintf("  %-9s mean %.4f  MCSE %.4f  bias/MCSE %.2f  n=%d\n",
              col, s[["mean"]], mcse,
              (s[["mean"]] - TRUTH[["xi1"]]) / mcse, s[["n"]]))
}
cat("\n=== BC12: metric falsification (M2, N = 2000, paired) ===\n")
cat(sprintf("  available-case - shipped : mean %+.4f  paired SE %.4f\n",
            mean(m2[, "available_case"] - m2[, "shipped"], na.rm = TRUE),
            stats::sd(m2[, "available_case"] - m2[, "shipped"], na.rm = TRUE) /
              sqrt(nrow(m2))))
cat(sprintf("  |shipped - two-stage|    : mean %.4f  paired SE %.4f\n",
            mean(abs(m2[, "shipped"] - m2[, "two_stage"]), na.rm = TRUE),
            stats::sd(abs(m2[, "shipped"] - m2[, "two_stage"]), na.rm = TRUE) /
              sqrt(nrow(m2))))
cat(sprintf("\nWrote %s (%.1f min, %d workers)\n", OUT, elapsed, CORES))
