# M68 -- the FIML calibration cells for the scaled global test statistic.
#
# AC3 asks whether the scaled statistic is calibrated in mean on the FIML path:
# mean(T_s)/df in [0.95, 1.05] at each of the M65 fixture's 2 / 5 / 10 % cellwise
# MCAR cells and the M66 fixture's 201-replicate M1 MAR cell. That is the ONLY
# oracle behind M68-D1, the decision to scale this path with the complete-data
# Gamma_R at Sigma-hat rather than one rebuilt from the FIML fit's own saturated
# stage: no complete-data reference value covers the choice, so if the FIML mean
# calibration misses its band under this construction, the decision is falsified
# and escalates rather than being patched.
#
# The band is wider than the complete-data one ([0.97, 1.03], AC7) for a stated
# reason: these cells are 200-201 replicates against that harness's 2000, so the
# Monte-Carlo error on mean(T)/df is about sqrt(2/df)/sqrt(200) = 0.0061 at
# df = 273 rather than 0.0019, and the band has to clear its own noise.
#
# WHY THE SEEDS ARE REUSED. Every cell regenerates from the seeds already stored
# in the two committed fixtures rather than drawing fresh ones. The missingness
# mechanisms live in the package (R/axes_fiml.R: axes_mcar(), axes_mar_m1()), so
# the same seed reproduces the same data and the same missingness, and these
# cells are therefore the SAME draws M65's point estimates and M66's standard
# errors were measured on. A fresh draw would make the three sets of evidence
# three different samples that merely look comparable.
#
# Usage (from the package root):
#   Rscript devel/m68-fiml-scaled-cells.R          # full run, all four cells
#   Rscript devel/m68-fiml-scaled-cells.R 8 4      # 8 reps/cell, 4 workers
#
# The M1 MAR cell fits at N = 2400 and is the expensive one (RR13 measured
# 18-68 s per structured FIML fit there).

suppressMessages(devtools::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
CAP <- if (length(args) >= 1) as.integer(args[[1]]) else NA_integer_
CORES <- if (length(args) >= 2) as.integer(args[[2]]) else 8L

OUT <- "tests/testthat/fixtures/m68-fiml-scaled-cells.rds"

OCT <- octants()
K_ITEMS <- 3L
K_SCALES <- 8L
TRUTH <- c(xi1 = .35, xi2 = .10, zeta1 = .08)

# The seeds, read off the committed fixtures rather than restated. If either
# fixture is regenerated with different seeds these cells follow it, which is
# the point -- restating them here is exactly how two records of one draw drift.
m65 <- readRDS("tests/testthat/fixtures/m65-heavy-cells.rds")
m66 <- readRDS("tests/testthat/fixtures/m66-corrected-se-cells.rds")
SEEDS_MCAR <- m65$provenance$seeds$mcar
SEEDS_M1 <- m66$provenance$seeds$m1
if (!is.na(CAP)) {
  SEEDS_MCAR <- utils::head(SEEDS_MCAR, CAP)
  SEEDS_M1 <- utils::head(SEEDS_M1, CAP)
}

probe_draw <- function(n, seed) {
  set.seed(seed)
  as.matrix(axes_simulate(n, OCT, K_ITEMS, TRUTH[["xi1"]], TRUTH[["xi2"]],
                          TRUTH[["zeta1"]]))
}
probe_items <- function(mat) {
  split(colnames(mat), rep(seq_len(K_SCALES), each = K_ITEMS))
}

# One FIML fit, reduced to what AC3 consumes. A refusal or a failed scaling
# yields NA rather than aborting the cell, and the survivor count is reported,
# because averaging silently over the survivors of a path that failed often
# would flatter that path (the M65 harness's own rule).
fiml_row <- function(mat) {
  res <- tryCatch(
    suppressMessages(suppressWarnings(
      axes_reliability(as.data.frame(mat), items = probe_items(mat),
                       angles = OCT, missing = "fiml")
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

rep_mcar <- function(seed, rate) fiml_row(axes_mcar(probe_draw(600L, seed), rate))
rep_m1 <- function(seed) fiml_row(axes_mar_m1(probe_draw(2400L, seed), K_ITEMS))

run <- function(seeds, fn, ...) {
  do.call(rbind, parallel::mclapply(seeds, fn, ..., mc.cores = CORES))
}

RATES <- c(`0.02` = 0.02, `0.05` = 0.05, `0.10` = 0.10)

t0 <- Sys.time()
cells <- list()
for (nm in names(RATES)) {
  message("MCAR ", nm, ": ", length(SEEDS_MCAR), " replicates (M65 seeds)")
  cells[[nm]] <- run(SEEDS_MCAR, rep_mcar, rate = RATES[[nm]])
}
message("M1 MAR at N = 2400: ", length(SEEDS_M1), " replicates (M66 seeds)")
cells[["m1"]] <- run(SEEDS_M1, rep_m1)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))

out <- list(
  provenance = list(
    source = paste(
      "M68 AC3; FIML cells regenerated from the M65 fixture's MCAR seeds and",
      "the M66 fixture's M1 MAR seeds, so the draws are shared with those",
      "fixtures rather than merely comparable"
    ),
    generator = "devel/m68-fiml-scaled-cells.R",
    truth = TRUTH,
    seeds = list(mcar = SEEDS_MCAR, m1 = SEEDS_M1),
    seed_source = list(mcar = "m65-heavy-cells.rds", m1 = "m66-corrected-se-cells.rds"),
    rates = RATES,
    elapsed_min = elapsed,
    r_version = R.version.string,
    lavaan_version = as.character(utils::packageVersion("lavaan"))
  ),
  cells = cells
)

for (nm in names(cells)) {
  x <- cells[[nm]]
  ok <- stats::complete.cases(x)
  df <- x[which(ok)[1], "df"]
  message(sprintf(
    "%-5s n_ok=%3d df=%3d | mean(T)/df=%.4f mean(Ts)/df=%.4f | rej: u=%.4f s=%.4f",
    nm, sum(ok), df, mean(x[ok, "chisq"]) / df,
    mean(x[ok, "chisq_scaled"]) / df,
    mean(x[ok, "p_unscaled"] < .05), mean(x[ok, "p"] < .05)
  ))
}
message(sprintf("elapsed: %.1f min", elapsed))

saveRDS(out, OUT)
message("wrote ", OUT)
