# Per-block CRAN-mode test timings, used by M120 to choose what to skip on CRAN
# and to re-measure afterwards. Run from the package root:
#
#   Rscript tools/m120-test-timings.R [out.rds]
#
# NOT_CRAN is cleared so skip_on_cran() fires, matching what CRAN runs. The
# figures are wall-clock seconds on the machine that runs it, so they compare
# only against another run of this script on the same machine.

library(testthat)
pkgload::load_all(".", quiet = TRUE)
Sys.setenv(NOT_CRAN = "")

res <- test_dir("tests/testthat",
  reporter = "silent", stop_on_failure = FALSE,
  package = "circumplex", load_package = "none"
)
df <- as.data.frame(res)

out <- commandArgs(trailingOnly = TRUE)[1]
if (!is.na(out)) {
  saveRDS(df[, c("file", "test", "real", "skipped", "failed", "error", "nb")], out)
}

live <- df[!df$skipped, ]
by_file <- do.call(rbind, lapply(split(live, live$file), function(d) {
  data.frame(file = d$file[[1]], blocks = nrow(d), seconds = round(sum(d$real), 1))
}))
print(head(by_file[order(-by_file$seconds), ], 25), row.names = FALSE)

cat(sprintf(
  "\nlive %.1f s over %d blocks; %d blocks skipped; %d failures, %d errors\n",
  sum(live$real), nrow(live), sum(df$skipped), sum(df$failed), sum(df$error)
))
