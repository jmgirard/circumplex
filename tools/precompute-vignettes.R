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
  "cpm-boundary-fits",
  "ci-accuracy",
  "structure-tests",
  "sem-based-ssm-analysis",
  "sem-latent-contrasts",
  "advanced-visualization",
  "intermediate-ssm-analysis",
  "introduction-to-ssm-analysis",
  "growth-ssm-analysis",
  "axes-reliability"
)

knit_one <- function(name) {
  stopifnot(name %in% VIGNETTES)
  # Checked before knitting, so a missing svglite stops the run with an error
  # that names it before any chunk runs.
  if (!requireNamespace("svglite", quietly = TRUE)) {
    stop("svglite is required to render the pre-computed vignette figures; ",
         "install it with install.packages(\"svglite\")", call. = FALSE)
  }
  owd <- setwd("vignettes")
  on.exit(setwd(owd), add = TRUE)
  # Figures land beside the rendered .Rmd and ship with it; the per-vignette
  # prefix keeps two vignettes from overwriting each other's chunk figures.
  # An empty default caption keeps knitr from wrapping every out.width figure
  # in a <div class="figure"> captioned "plot of chunk <label>"; rendering the
  # .Rmd live never produced those. Chunks that set fig.cap themselves win.
  #
  # The alt text is a separate matter from the caption: with fig.cap = "" and no
  # fig.alt, knitr writes alt="" and the figure becomes invisible to a screen
  # reader, where a live build wrote alt="plot of chunk <label>" (M120 review
  # F6). The hook restores exactly that default, and only where this script's
  # own empty caption is what is in force -- a chunk that writes its own
  # fig.cap or fig.alt keeps it.
  #
  # Figures are SVG (svglite), not PNG. Each figure ships twice in the
  # tarball, once as a file and once base64 in the built HTML, and the 144-dpi
  # PNGs (hotfix #152) were the bulk of the first 2.1.0 tarball, which crossed
  # the 1e7-byte size NOTE of CRAN's incoming check. The SVGs are vector, so
  # they need no dpi and are a fraction of the size. svglite is needed only
  # here (hence the check at the top of this function), never by R CMD build,
  # which reads the committed files. Where a chunk sets out.width, it governs
  # display size. Plotmath symbols such as Delta are drawn in Arial rather than
  # the Symbol font, which Windows encodes without them.
  #
  # error = FALSE makes a chunk error stop the knit; knitr's default writes the
  # error into the output and exits 0, which would ship it.
  knitr::opts_chunk$set(
    error = FALSE,
    fig.path = file.path("figures", paste0(name, "-")),
    fig.cap = "",
    dev = "svglite",
    fig.ext = "svg",
    dev.args = list(system_fonts = list(symbol = "Arial"))
  )
  knitr::opts_hooks$set(fig.cap = function(options) {
    if (is.null(options$fig.alt) && identical(options$fig.cap, "")) {
      options$fig.alt <- paste("plot of chunk", options$label)
    }
    options
  })
  # When a knit fails, knitr writes the output produced so far to its output
  # file before it stops, so a knit that fails part-way would leave the shipped
  # .Rmd cut short. It knits to a temporary file instead, which replaces the
  # .Rmd only once knit() returns.
  out <- paste0(name, ".Rmd")
  tmp <- paste0(out, ".tmp")
  # after = FALSE: the relative path must be removed before setwd() restores.
  on.exit(unlink(tmp), add = TRUE, after = FALSE)
  set.seed(12345)
  knitr::knit(paste0(name, ".Rmd.orig"), tmp, quiet = TRUE)
  if (!file.rename(tmp, out)) stop("could not replace ", out, call. = FALSE)
  invisible(NULL)
}

# Sourced by tools/check-vignette-staleness.R, tools/check-vignette-width.R and
# tools/m120-vignette-parity.R, which read only VIGNETTES; the command-line
# block below runs only when this file is the script being run.
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
