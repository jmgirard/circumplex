# M66 -- the heavy evidence cells for the corrected component standard errors.
#
# RR13 BC3 (complete-data calibration) and BC5 (beyond-mild missingness) each
# need >= 200 replicates: the band is an empirical-SD comparison, and the MC SE
# of an SD over R replicates is 1/sqrt(2(R-1)), so BC5's "MC SE of the SD <= 5%"
# is R >= 201 exactly. The committed M65 fixture carries none of these three
# cells -- it has no complete-data cell, no 15% MCAR cell, and its M1 cell is 5
# replicates with no SE column at all -- so they are generated here, seed-pinned,
# and committed as a summary the suite asserts against (the M65-D3 pattern).
#
# What the suite does with the output: tests/testthat/test-axes-corrected-se.R
# asserts the stored summary against BC3 and BC5 AND re-runs a live smoke, so a
# stored number is never the only thing between a broken correction and a green
# suite.
#
# Usage (from the package root):
#   Rscript devel/m66-heavy-cells.R             # full run, 6 workers
#   Rscript devel/m66-heavy-cells.R 8 4         # 8 reps, 4 workers (smoke)
#
# Every replicate seeds itself from its own pinned seed, so the result does not
# depend on the worker count or on scheduling order.

suppressMessages(devtools::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
REPS <- if (length(args) >= 1) as.integer(args[[1]]) else 201L
CORES <- if (length(args) >= 2) as.integer(args[[2]]) else 6L

OUT <- "tests/testthat/fixtures/m66-corrected-se-cells.rds"

OCT <- octants()
K_SCALES <- 8L
K_ITEMS <- 3L
TRUTH <- c(xi1 = .35, xi2 = .10, zeta1 = .08)

probe_draw <- function(n, seed) {
  set.seed(seed)
  as.matrix(axes_simulate(n, OCT, K_ITEMS, TRUTH[["xi1"]], TRUTH[["xi2"]],
                          TRUTH[["zeta1"]]))
}
probe_items <- function(mat) {
  split(colnames(mat), rep(seq_len(K_SCALES), each = K_ITEMS))
}

# One fit, reduced to what BC3/BC5 consume. BOTH SEs are stored: `se` is what
# the package now reports (corrected) and `se_naive` is what lavaan reported
# before the correction. Keeping the naive column is what lets a later session
# recompute the metric ratio per replicate without refitting -- the gap the M65
# fixture had, which forced M66 T4 to bridge with a population constant.
fit_row <- function(mat, ...) {
  res <- tryCatch(
    suppressMessages(suppressWarnings(
      axes_reliability(as.data.frame(mat), items = probe_items(mat),
                       angles = OCT, ...)
    )),
    error = function(e) NULL
  )
  if (is.null(res) || isTRUE(res$results$boundary[[1]])) {
    return(c(xi1 = NA_real_, se = NA_real_, se_naive = NA_real_))
  }
  c(
    xi1 = res$results$xi1[[1]],
    se = res$components$SE[res$components$Symbol == "xi1"],
    se_naive = unname(res$details$se_uncorrected[["xi1"]])
  )
}

# BC3: complete data, no missingness, the listwise path (which on complete data
# is simply the whole sample).
rep_complete <- function(seed) fit_row(probe_draw(600L, seed))

# BC5 cell 1: 15% cellwise MCAR, the FIML path. Half again the highest rate the
# M65 fixture covers, which is the point -- BC5 exists to check the correction
# past the mild-missingness regime RR13's evidence reaches.
rep_mcar15 <- function(seed) {
  fit_row(axes_mcar(probe_draw(600L, seed), 0.15), missing = "fiml")
}

# BC5 cell 2: mechanism M1 MAR at N = 2400, matching the M65 fixture's own M1
# configuration so the two cells are comparable. The expensive one: a structured
# FIML fit under realistic MAR runs 18-68 s (RR13).
rep_m1 <- function(seed) {
  fit_row(axes_mar_m1(probe_draw(2400L, seed), K_ITEMS), missing = "fiml")
}

run <- function(seeds, fn) {
  do.call(rbind, parallel::mclapply(seeds, fn, mc.cores = CORES))
}

t0 <- Sys.time()
message("BC3: ", REPS, " complete-data replicates ...")
complete <- run(3000L + seq_len(REPS), rep_complete)

message("BC5a: ", REPS, " replicates at 15% MCAR ...")
mcar15 <- run(4000L + seq_len(REPS), rep_mcar15)

message("BC5b: ", REPS, " M1 MAR replicates at N = 2400 ...")
m1 <- run(5000L + seq_len(REPS), rep_m1)

elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))

out <- list(
  provenance = list(
    source = "M66, RR13 BC3/BC5; probe population 8 scales x 3 items",
    generator = "devel/m66-heavy-cells.R",
    truth = TRUTH,
    seeds = list(complete = 3000L + seq_len(REPS),
                 mcar15 = 4000L + seq_len(REPS),
                 m1 = 5000L + seq_len(REPS)),
    reps = REPS,
    elapsed_min = elapsed,
    r_version = R.version.string,
    lavaan_version = as.character(utils::packageVersion("lavaan"))
  ),
  complete = complete,
  mcar15 = mcar15,
  m1 = m1
)

calib <- function(x) {
  ok <- !is.na(x[, "se"]) & !is.na(x[, "xi1"])
  mean(x[ok, "se"]) / stats::sd(x[ok, "xi1"])
}
message(sprintf("BC3  complete-data calibration : %.4f  (band [0.90, 1.10])",
                calib(complete)))
message(sprintf("BC5a 15%% MCAR calibration      : %.4f  (band [0.85, 1.15])",
                calib(mcar15)))
message(sprintf("BC5b M1 MAR calibration        : %.4f  (band [0.85, 1.15])",
                calib(m1)))
message(sprintf("elapsed: %.1f min", elapsed))

saveRDS(out, OUT)
message("wrote ", OUT)
