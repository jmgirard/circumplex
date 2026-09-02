#!/usr/bin/env Rscript

# Guard: every shipped vignettes/<name>.Rmd must be what re-rendering its
# .Rmd.orig source produces. The shipped .Rmd is a generated file (see
# tools/precompute-vignettes.R), and a generated file nothing regenerates goes
# stale silently -- the prose keeps describing output the code no longer emits.
#
# Run by .github/workflows/vignette-precompute.yaml AFTER the re-render, so the
# working tree holds the fresh render and git holds the committed copy:
#
#   Rscript tools/precompute-vignettes.R
#   Rscript tools/check-vignette-staleness.R
#
# Why this is not `git diff --exit-code`. One chunk's output is not reproducible
# across machines: ssm_ci_accuracy() in evaluating-circumplex-structure refits an
# ill-conditioned CPM, whose solution moves in the third digit between BLAS
# implementations, and all 200 simulated replications move with it. A byte-exact
# comparison there can never go green (M120 review F1).
#
# So a vignette source may mark a region:
#
#   <!-- precompute:volatile-numbers start -- <reason> -->
#   ...
#   <!-- precompute:volatile-numbers end -->
#
# and INSIDE a marked region only, numeric literals in knitr output lines (those
# beginning `#>`) are replaced by <n> on both sides before comparing. Everything
# else is byte-exact: all prose, all chunk source, every other vignette in full,
# and inside the marked region the output's line count, line positions and every
# non-numeric word. The blind spot is exactly one thing -- a stale digit inside a
# declared region -- and the marker states in the file why it is there.
#
# Base R only: this runs before the package's own dependencies matter.

env <- new.env()
sys.source("tools/precompute-vignettes.R", env)
VIGNETTES <- env$VIGNETTES

START <- "<!--\\s*precompute:volatile-numbers start\\b"
END <- "<!--\\s*precompute:volatile-numbers end\\s*-->"
NUMBER <- "[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?"

# Returns the comparison form of a rendered vignette, plus a per-region count of
# the output lines each marked region masked. An unbalanced marker, a marker
# carrying no reason, and a region masking nothing are all errors: each would
# leave a mask in place that no longer says what it covers.
maskable <- function(lines, what) {
  out <- lines
  open <- FALSE
  masked <- integer(0)
  for (i in seq_along(lines)) {
    ln <- lines[[i]]
    if (grepl(START, ln)) {
      if (open) stop(what, ": nested precompute:volatile-numbers start at line ", i, call. = FALSE)
      if (!grepl("start\\s+--\\s*\\S", ln))
        stop(what, ": the marked region at line ", i, " states no reason; write ",
             "`start -- <why this output is not reproducible>`.", call. = FALSE)
      open <- TRUE
      masked <- c(masked, 0L)
      next
    }
    if (grepl(END, ln)) {
      if (!open) stop(what, ": precompute:volatile-numbers end without a start at line ", i, call. = FALSE)
      open <- FALSE
      next
    }
    if (open && grepl("^#>", ln)) {
      masked[length(masked)] <- masked[length(masked)] + 1L
      out[[i]] <- gsub(NUMBER, "<n>", ln)
    }
  }
  if (open) stop(what, ": a precompute:volatile-numbers region is never closed.", call. = FALSE)
  if (length(masked) && any(masked == 0L))
    stop(what, ": a precompute:volatile-numbers region masks no output line; ",
         "delete the marker rather than leaving it to cover nothing.", call. = FALSE)
  list(text = out, masked = masked)
}

committed <- function(path) {
  txt <- suppressWarnings(system2("git", c("show", paste0("HEAD:", path)), stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(txt, "status")) && attr(txt, "status") != 0L)
    stop("no committed copy of ", path, call. = FALSE)
  txt
}

stale <- character(0)
for (name in VIGNETTES) {
  path <- file.path("vignettes", paste0(name, ".Rmd"))
  if (!file.exists(path)) stop("missing ", path, " -- run tools/precompute-vignettes.R first", call. = FALSE)
  now <- maskable(readLines(path, warn = FALSE), path)
  was <- maskable(committed(path), paste0("HEAD:", path))
  regions <- if (length(now$masked)) {
    paste0(", ", length(now$masked), " masked region(s), ",
           sum(now$masked), " output line(s) compared without their digits")
  } else ""
  same <- identical(now$text, was$text)
  cat(sprintf("%-36s %s%s\n", name, if (same) "up to date" else "STALE", regions))
  if (!same) {
    stale <- c(stale, name)
    n <- max(length(now$text), length(was$text))
    pad <- function(x) c(x, rep("<absent>", n - length(x)))
    a <- pad(was$text)
    b <- pad(now$text)
    for (i in utils::head(which(a != b), 12)) {
      cat("    committed: ", a[[i]], "\n", sep = "")
      cat("    rendered : ", b[[i]], "\n", sep = "")
    }
    if (length(now$text) != length(was$text))
      cat("    (", length(was$text), " committed lines vs ", length(now$text), " rendered)\n", sep = "")
  }
}

if (length(stale)) {
  stop("stale pre-computed vignette(s): ", paste(stale, collapse = ", "),
       "\nRun `Rscript tools/precompute-vignettes.R` and commit the result.", call. = FALSE)
}
cat("all", length(VIGNETTES), "pre-computed vignettes are up to date with their sources\n")
