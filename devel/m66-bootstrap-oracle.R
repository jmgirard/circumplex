# M66 -- the independent pipeline-bootstrap oracle (RR13 BC6).
#
# BC6 asks that a nonparametric bootstrap that RE-STANDARDIZES per resample
# agree with the analytic corrected SE within 15% relative, on >= 2 complete-
# data draws. lavaan's own se = "bootstrap" is forbidden and would be wrong
# here: it resamples the already-standardized columns, so it reproduces the
# covariance-metric variability the correction exists to remove. The whole
# point is to re-compute the correlation matrix inside the resampling loop.
#
# Why this is a script and not a plain test: BC6's comparison is only as sharp
# as the bootstrap's own Monte-Carlo noise, and the SD of a bootstrap SD over B
# resamples is ~1/sqrt(2B) -- about 5% at B = 200. Measured at M66: on seed
# 1001 the B = 200 SD came out 0.013625 and the converged value is 0.012967, so
# the noise alone moved a genuine 9.5% gap to 15.06% and tipped it over BC6's
# bar. B = 1000 puts that noise near 2.2% and the running SD is stable from
# B = 400 on. That costs ~1000 fits per draw, too slow for every suite run, so
# it runs here and the suite asserts the stored result plus a live smoke -- the
# same split M65 used for its heavy cells.
#
# Usage (from the package root):
#   Rscript devel/m66-bootstrap-oracle.R          # 3 draws, B = 1000
#   Rscript devel/m66-bootstrap-oracle.R 2 200    # smoke

suppressMessages(devtools::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
N_DRAWS <- if (length(args) >= 1) as.integer(args[[1]]) else 3L
B <- if (length(args) >= 2) as.integer(args[[2]]) else 1000L
CORES <- 6L

OUT <- "tests/testthat/fixtures/m66-bootstrap-oracle.rds"

OCT <- octants()
N <- 600L
SEEDS <- c(1001L, 1002L, 1003L)[seq_len(N_DRAWS)]

one_draw <- function(seed) {
  set.seed(seed)
  dat <- as.matrix(axes_simulate(N, OCT, 3L, .35, .10, .08))
  items <- split(colnames(dat), rep(1:8, each = 3))

  fit0 <- suppressMessages(
    axes_reliability(as.data.frame(dat), items = items, angles = OCT)
  )
  analytic <- fit0$components$SE[fit0$components$Symbol == "xi1"]
  naive <- unname(fit0$details$se_uncorrected[["xi1"]])

  # The pipeline bootstrap: resample RESPONDENTS, then re-compute the
  # correlation matrix from the resampled rows. That second step is the one
  # that matters -- it is where the in-sample standardization is redone, and it
  # is exactly what lavaan's built-in bootstrap does not do.
  est <- unlist(parallel::mclapply(seq_len(B), function(b) {
    set.seed(seed * 1000L + b)
    idx <- sample.int(N, N, replace = TRUE)
    r <- stats::cor(dat[idx, , drop = FALSE])
    f <- tryCatch(suppressWarnings(axes_fit_cormat(r, items, OCT, n = N)),
                  error = function(e) NULL)
    if (is.null(f)) return(NA_real_)
    pe <- lavaan::parameterEstimates(f)
    pe$est[pe$op == "~~" & pe$lhs == "AX" & pe$rhs == "AX"][[1]]
  }, mc.cores = CORES))

  c(analytic = analytic, naive = naive,
    boot = stats::sd(est, na.rm = TRUE), kept = sum(!is.na(est)))
}

t0 <- Sys.time()
rows <- do.call(rbind, lapply(SEEDS, function(s) {
  message("draw ", s, " (B = ", B, ") ...")
  one_draw(s)
}))
rownames(rows) <- as.character(SEEDS)

out <- list(
  provenance = list(
    source = "M66, RR13 BC6; pipeline bootstrap re-standardizing per resample",
    generator = "devel/m66-bootstrap-oracle.R",
    n = N, b = B, seeds = SEEDS,
    elapsed_min = as.numeric(difftime(Sys.time(), t0, units = "mins")),
    r_version = R.version.string,
    lavaan_version = as.character(utils::packageVersion("lavaan"))
  ),
  draws = rows
)

for (i in seq_len(nrow(rows))) {
  message(sprintf(
    "  seed %s: analytic %.6f  boot %.6f  rel %.4f  (naive %.6f, rel %.4f)",
    rownames(rows)[i], rows[i, "analytic"], rows[i, "boot"],
    abs(rows[i, "boot"] - rows[i, "analytic"]) / rows[i, "analytic"],
    rows[i, "naive"],
    abs(rows[i, "boot"] - rows[i, "naive"]) / rows[i, "naive"]
  ))
}
message(sprintf("elapsed: %.1f min", out$provenance$elapsed_min))

saveRDS(out, OUT)
message("wrote ", OUT)
