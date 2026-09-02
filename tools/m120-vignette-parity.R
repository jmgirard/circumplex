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
#
# Beyond that it needs only what the vignettes themselves need, so it runs on a
# clean checkout with the package's own Suggests installed. It used to reach for
# xml2, which is in no dependency surface here (M120 review F7); xml2 was only
# decoding HTML entities, and both sides are rendered by the same pandoc, so
# entity decoding cannot separate them -- a small base-R decoder keeps the
# mismatch report readable and drags in nothing.

env <- new.env()
sys.source("tools/precompute-vignettes.R", env)
VIGNETTES <- env$VIGNETTES

# The named entities pandoc emits in text. Numeric references are left verbatim
# -- decoding them buys nothing here, and this is applied to both sides
# identically, so an entity left encoded cannot separate them.
decode_entities <- function(x) {
  named <- c("&lt;" = "<", "&gt;" = ">", "&quot;" = '"', "&apos;" = "'",
             "&nbsp;" = " ", "&amp;" = "&")   # &amp; last: it un-escapes the rest
  for (e in names(named)) x <- gsub(e, named[[e]], x, fixed = TRUE)
  x
}

visible_text <- function(html_file) {
  x <- paste(readLines(html_file, warn = FALSE), collapse = "\n")
  x <- gsub("(?s)<script.*?</script>|<style.*?</style>", "", x, perl = TRUE)
  # Figures collapse to a placeholder: their bytes are not reproducible across
  # platforms, and their alt text legitimately differs between the two sides --
  # the shipped copy is knitted by tools/precompute-vignettes.R, which sets an
  # alt of "plot of chunk <label>", while a plain render of the source emits an
  # <img> with no alt attribute at all. So alt is not compared here; it is
  # asserted directly over the shipped vignettes below (M120 review F6).
  x <- gsub("(?s)<img[^>]*>", "\n[IMG]\n", x, perl = TRUE)
  x <- gsub("(?s)<[^>]+>", " ", x, perl = TRUE)
  x <- decode_entities(x)
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

# Every figure in a SHIPPED vignette must carry alt text. Nothing else here sees
# this: the comparison above collapses figures, and R CMD check does not read
# alt attributes. An empty alt makes the figure invisible to a screen reader,
# which is what shipping fig.cap = "" without a fig.alt did (M120 review F6).
check_alt_text <- function(names_wanted) {
  bad <- character()
  for (name in names_wanted) {
    path <- file.path("vignettes", paste0(name, ".Rmd"))
    lines <- readLines(path, warn = FALSE)
    tags <- unlist(regmatches(lines, gregexpr("<img[^>]*>", lines)))
    for (tg in tags) {
      alt <- regmatches(tg, regexpr('alt="[^"]*"', tg))
      if (!length(alt) || identical(alt, 'alt=""')) bad <- c(bad, paste(name, tg))
    }
  }
  if (length(bad)) {
    stop("shipped figure(s) with no alt text:\n  ", paste(bad, collapse = "\n  "),
         call. = FALSE)
  }
  cat("every shipped figure carries alt text\n")
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
check_alt_text(names_wanted)
