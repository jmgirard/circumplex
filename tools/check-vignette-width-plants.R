#!/usr/bin/env Rscript

# Shows that tools/check-vignette-width.R fails on each kind of line it
# claims to read (M164). Each plant adds one wide line, an unclosed fence or a
# chunk error to a fresh copy of vignettes/, runs the guard on the copy, and
# requires the guard to fail and to name the plant in the planted file: the
# line's width and text, the fence that never closes, or the error message.
# Each control adds nothing the guard measures, and requires the guard to
# pass.
#
#   Rscript tools/check-vignette-width-plants.R
#
# Run it from the repository root, after the pre-computed vignettes are
# rendered and with the branch's package installed, as the guard needs. The
# live-vignette plants knit, so they read the installed package too.
#
# Run by .github/workflows/vignette-precompute.yaml after the guard. Base R
# only.

GUARD <- "tools/check-vignette-width.R"
PRE <- "evaluating-circumplex-structure"  # a pre-computed vignette
LIVE_A <- "using-instruments"             # live vignettes, knitted by the guard
LIVE_B <- "bayesian-ssm-analysis"

if (!file.exists(GUARD)) stop("run from the repository root", call. = FALSE)

fence <- function(...) c("", "```", ..., "```", "")
chunk <- function(header, ...) c("", paste0("```{r, ", header, "}"), ..., "```", "")

# Each plant: the vignette to edit, the lines to append, and the text the
# guard's report must contain. A control has no expected text.
plants <- list(
  `output line in a backtick fence` = list(
    file = PRE, add = fence(paste0("#> ", strrep("a", 80))),
    expect = paste0("(83 columns): #> ", strrep("a", 80))
  ),
  `output line in a tilde fence` = list(
    file = PRE, add = c("", "~~~", paste0("#> ", strrep("b", 80)), "~~~", ""),
    expect = paste0("(83 columns): #> ", strrep("b", 80))
  ),
  `4-backtick fence holding an inner 3-backtick line` = list(
    file = PRE,
    add = c("", "````", "```", paste0("#> ", strrep("c", 80)), "````", ""),
    expect = paste0("(83 columns): #> ", strrep("c", 80))
  ),
  `fence that never closes` = list(
    file = PRE, add = c("", "```r", "x <- 1"),
    expect = "never closes"
  ),
  `output under comment = "##"` = list(
    file = LIVE_A,
    add = chunk('comment = "##"', 'cat(strrep("d", 85), "\\n", sep = "")'),
    expect = paste0("(88 columns): ## ", strrep("d", 85))
  ),
  `output under comment = ""` = list(
    file = LIVE_A,
    add = chunk('comment = ""', 'cat(strrep("e", 85), "\\n", sep = "")'),
    expect = paste0("(85 columns): ", strrep("e", 85))
  ),
  `indented output line` = list(
    file = PRE, add = fence(paste0("   #> ", strrep("f", 80))),
    expect = paste0("(86 columns):    #> ", strrep("f", 80))
  ),
  `line wide only after tab expansion` = list(
    file = PRE, add = fence(paste0(strrep("\t", 9), strrep("g", 10))),
    expect = paste0("(82 columns): ", strrep("\t", 9), strrep("g", 10))
  ),
  `line of double-width characters` = list(
    file = PRE, add = fence(paste0("#> ", strrep("あ", 40))),
    expect = paste0("(83 columns): #> ", strrep("あ", 40))
  ),
  `wide line first in a block` = list(
    file = PRE, add = fence(paste0("#> ", strrep("h", 80)), "x"),
    expect = paste0("(83 columns): #> ", strrep("h", 80))
  ),
  `wide line last in a block` = list(
    file = PRE, add = fence("x", paste0("#> ", strrep("i", 80))),
    expect = paste0("(83 columns): #> ", strrep("i", 80))
  ),
  `echoed source line` = list(
    file = PRE, add = c("", "```r", paste0("x <- c(", strrep("1, ", 26), "1)"), "```", ""),
    expect = paste0("(87 columns): x <- c(", strrep("1, ", 26), "1)")
  ),
  `fence written by a results = "asis" chunk` = list(
    file = LIVE_A,
    add = chunk('echo = FALSE, results = "asis"',
                'cat("```\\n", strrep("j", 85), "\\n```\\n", sep = "")'),
    expect = paste0("(85 columns): ", strrep("j", 85))
  ),
  `output line in a knitted live vignette` = list(
    file = LIVE_B,
    add = chunk('echo = FALSE', 'cat(strrep("k", 85), "\\n", sep = "")'),
    expect = paste0("(88 columns): #> ", strrep("k", 85))
  ),
  `chunk error in a knitted live vignette` = list(
    file = LIVE_B,
    add = chunk('echo = FALSE', 'stop("planted knit error")'),
    expect = "planted knit error"
  ),
  `control: wide prose line outside a fence` = list(
    file = PRE, add = c("", strrep("l", 120), ""), expect = NULL
  ),
  `control: the unchanged tree` = list(
    file = NULL, add = NULL, expect = NULL
  )
)

# The Rscript of the R that runs this script, so the guard runs under the same
# R and library.
RSCRIPT <- file.path(R.home("bin"), "Rscript")

# Copies vignettes/ to a fresh directory, applies one plant, and returns the
# guard's exit status and report. The copy is deleted afterwards.
run_plant <- function(p) {
  copy <- tempfile("width-plant-")
  dir.create(copy)
  on.exit(unlink(copy, recursive = TRUE), add = TRUE)
  file.copy(list.files("vignettes", full.names = TRUE, all.files = TRUE, no.. = TRUE),
            copy, recursive = TRUE)
  if (!is.null(p$file)) {
    path <- file.path(copy, paste0(p$file, ".Rmd"))
    if (!file.exists(path)) stop("plant target missing: ", path, call. = FALSE)
    con <- file(path, open = "a", encoding = "UTF-8")
    writeLines(p$add, con)
    close(con)
  }
  out <- suppressWarnings(system2(RSCRIPT, c(GUARD, copy), stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  list(status = if (is.null(status)) 0L else status, out = out)
}

n_vignettes <- length(list.files("vignettes", pattern = "\\.Rmd$"))
if (n_vignettes == 0L) stop("no vignette to plant in", call. = FALSE)

# TRUE when the report ties `expect` to `file`. Where the guard printed a
# summary line for the file, that line must say TOO WIDE and `expect` must sit
# in the file's own block, before the next summary line: every report names
# every file, so the file name alone would prove nothing. Where the guard
# stopped before any summary (a fence that never closes, a knit error), a
# line that is not a summary line must name the file, and `expect` must appear.
names_in_file <- function(out, file, expect) {
  label <- paste0(file, ".Rmd")
  summaries <- grep("^[^ ].* code-box lines, ", out)
  own <- summaries[startsWith(out[summaries], label)]
  if (length(own) == 1L) {
    stop_at <- c(summaries[summaries > own], length(out) + 1L)[[1]] - 1L
    grepl("TOO WIDE", out[[own]], fixed = TRUE) &&
      any(grepl(expect, out[own:stop_at], fixed = TRUE))
  } else {
    others <- setdiff(seq_along(out), summaries)
    any(grepl(label, out[others], fixed = TRUE)) &&
      any(grepl(expect, out, fixed = TRUE))
  }
}

bad <- character(0)
for (nm in names(plants)) {
  p <- plants[[nm]]
  res <- run_plant(p)
  report <- paste(res$out, collapse = "\n")
  ok <- if (is.null(p$expect)) {
    # A control passes, and the guard reports every vignette it read.
    res$status == 0L &&
      grepl(paste("all", n_vignettes, "vignettes print within"), report, fixed = TRUE)
  } else {
    # A plant fails, and the report names the planted line, fence or error in
    # the planted file.
    res$status != 0L && names_in_file(res$out, p$file, p$expect)
  }
  cat(sprintf("%-52s %s\n", nm,
              if (ok) (if (is.null(p$expect)) "passed, as required" else "failed, as required")
              else "WRONG"))
  if (!ok) {
    bad <- c(bad, nm)
    cat(paste0("    ", tail(res$out, 15)), sep = "\n")
  }
}

if (length(bad)) {
  stop("the width guard did not behave as required for: ", paste(bad, collapse = "; "),
       call. = FALSE)
}
cat("the width guard failed on all", sum(!vapply(plants, function(p) is.null(p$expect), TRUE)),
    "plants and passed both controls\n")
