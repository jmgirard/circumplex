# Renders the pre-computed vignettes: each vignettes/<name>.Rmd.orig is knitted
# to vignettes/<name>.Rmd, which ships already carrying its output. The .orig
# sources and this script are .Rbuildignore'd, so R CMD build sees only the
# rendered .Rmd and re-building a vignette costs no model fits.
#
#   Rscript tools/precompute-vignettes.R            # all of them
#   Rscript tools/precompute-vignettes.R <name>     # just one
#
# circumplex must be INSTALLED (the vignettes call library(circumplex)); a
# pkgload::load_all() shadow is not enough. Each vignette knits in its own R
# process so chunk options and attached packages cannot leak between them.

VIGNETTES <- c(
  "evaluating-circumplex-structure",
  "sem-based-ssm-analysis",
  "advanced-visualization",
  "intermediate-ssm-analysis",
  "introduction-to-ssm-analysis",
  "growth-ssm-analysis",
  "axes-reliability"
)

knit_one <- function(name) {
  stopifnot(name %in% VIGNETTES)
  owd <- setwd("vignettes")
  on.exit(setwd(owd), add = TRUE)
  # Figures land beside the rendered .Rmd and ship with it; the per-vignette
  # prefix keeps two vignettes from overwriting each other's chunk figures.
  # An empty default caption keeps knitr from wrapping every out.width figure
  # in a <div class="figure"> captioned "plot of chunk <label>"; rendering the
  # .Rmd live never produced those. Chunks that set fig.cap themselves win.
  knitr::opts_chunk$set(
    fig.path = file.path("figures", paste0(name, "-")),
    fig.cap = ""
  )
  set.seed(12345)
  knitr::knit(paste0(name, ".Rmd.orig"), paste0(name, ".Rmd"), quiet = TRUE)
  invisible(NULL)
}

# Sourced by tools/m120-vignette-parity.R for VIGNETTES and knit_one(); the
# command-line block below runs only when this file is the script being run.
if (identical(environment(), globalenv())) {

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 1L) {
  knit_one(args[[1]])
} else if (length(args) == 0L) {
  rscript <- file.path(R.home("bin"), "Rscript")
  for (name in VIGNETTES) {
    cat("knitting", name, "\n")
    status <- system2(rscript, c("tools/precompute-vignettes.R", name))
    if (status != 0L) stop("failed to knit ", name, call. = FALSE)
  }
} else {
  stop("usage: Rscript tools/precompute-vignettes.R [<vignette-name>]", call. = FALSE)
}

}
