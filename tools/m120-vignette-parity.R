# AC5 instrument for M120: proves a pre-computed vignette says the same thing
# its source does. For each vignettes/<name>.Rmd.orig it knits and renders the
# source, renders the committed vignettes/<name>.Rmd, strips both HTML outputs
# to visible text (images collapse to a placeholder, since figure bytes are not
# reproducible across platforms), and reports any line that differs.
#
#   Rscript tools/m120-vignette-parity.R [<name> ...]
#
# circumplex must be INSTALLED at the version under test. A rendering error on
# either side is a failure, not a skip -- an empty comparison would otherwise
# pass vacuously. The source side is rendered from a scratch copy inside
# vignettes/ so relative paths resolve exactly as they do for the real build.

env <- new.env()
sys.source("tools/precompute-vignettes.R", env)
VIGNETTES <- env$VIGNETTES

visible_text <- function(html_file) {
  x <- paste(readLines(html_file, warn = FALSE), collapse = "\n")
  x <- gsub("(?s)<script.*?</script>|<style.*?</style>", "", x, perl = TRUE)
  x <- gsub("(?s)<img[^>]*>", "\n[IMG]\n", x, perl = TRUE)
  x <- gsub("(?s)<[^>]+>", " ", x, perl = TRUE)
  x <- xml2::xml_text(xml2::read_html(paste0("<x>", x, "</x>")))
  x <- trimws(strsplit(x, "\n", fixed = TRUE)[[1]])
  x <- x[nzchar(x)]
  # ssm_ci_accuracy()'s summary prints its own wall-clock time (R/ssm_ci_oop.R),
  # which differs run to run on the same machine. Normalize that one number so
  # the comparison is over content; everything else must match exactly.
  sub("^(#&gt;|#>)?\\s*Elapsed:.*$", "<elapsed>", x)
}

render_text <- function(input, dir) {
  out <- rmarkdown::render(input, output_dir = dir, quiet = TRUE)
  if (!file.exists(out)) stop("no output from ", input, call. = FALSE)
  visible_text(out)
}

render_source <- function(name, dir) {
  scratch <- file.path("vignettes", paste0(".parity-", name, ".Rmd"))
  file.copy(file.path("vignettes", paste0(name, ".Rmd.orig")), scratch, overwrite = TRUE)
  on.exit(unlink(scratch), add = TRUE)
  render_text(scratch, dir)
}

names_wanted <- commandArgs(trailingOnly = TRUE)
if (!length(names_wanted)) names_wanted <- VIGNETTES
stopifnot(length(names_wanted) > 0L, all(names_wanted %in% VIGNETTES))

tmp_src <- tempfile("src"); tmp_pre <- tempfile("pre")
dir.create(tmp_src); dir.create(tmp_pre)

bad <- character()
for (name in names_wanted) {
  set.seed(12345)
  src <- render_source(name, tmp_src)
  pre <- render_text(file.path("vignettes", paste0(name, ".Rmd")), tmp_pre)
  if (!length(src) || !length(pre)) stop("empty comparison for ", name, call. = FALSE)
  same <- identical(src, pre)
  cat(sprintf("%-36s %s (%d source lines)\n", name, if (same) "match" else "MISMATCH", length(src)))
  if (!same) {
    bad <- c(bad, name)
    d <- setdiff(union(src, pre), intersect(src, pre))
    cat(paste0("    ", utils::head(d, 25), collapse = "\n"), "\n")
  }
}
if (length(bad)) stop("mismatched: ", paste(bad, collapse = ", "), call. = FALSE)
cat("all", length(names_wanted), "pre-computed vignettes match their sources\n")
