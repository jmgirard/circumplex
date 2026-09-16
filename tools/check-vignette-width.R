#!/usr/bin/env Rscript

# Guard: no printed output line in a pre-computed vignette is wider than the
# website's code box. The pkgdown code box scrolls sideways (overflow-x: auto)
# past its width, measured on the live site on 2026-09-15 at 81 monospace
# characters. The limit here is 80 display columns, counting knitr's "#> "
# prefix. The vignette sources set options(width = 77) for this reason.
#
#   Rscript tools/check-vignette-width.R              # reads vignettes/
#   Rscript tools/check-vignette-width.R <directory>  # reads a copy elsewhere
#
# The files read are the rendered vignettes/<name>.Rmd of every name that
# tools/precompute-vignettes.R renders. The two live vignettes ship without
# output, so there is nothing here for this guard to read.
#
# Width is display columns (nchar(type = "width")), not bytes or characters,
# so a line of Greek letters and superscripts is measured as it looks.
#
# Output that no width setting reaches, such as text printed with cat(), may be
# exempted by a marked region in the vignette source:
#
#   <!-- vignette-width:exempt start -- <reason> -->
#   ...
#   <!-- vignette-width:exempt end -->
#
# A region that states no reason, is never closed, or exempts no over-wide line
# is an error, so a marker cannot outlive the output it was written for.
#
# Run by .github/workflows/vignette-precompute.yaml after the re-render.
# Base R only.

LIMIT <- 80L

env <- new.env()
sys.source("tools/precompute-vignettes.R", env)
VIGNETTES <- env$VIGNETTES

START <- "<!--\\s*vignette-width:exempt start\\b"
END <- "<!--\\s*vignette-width:exempt end\\s*-->"

# Returns, for one rendered vignette, the number of output lines read, the
# over-wide lines outside any region, and the over-wide lines each region
# exempted. Marker misuse stops with an error naming the file and line.
scan_widths <- function(lines, what) {
  out_lines <- 0L
  wide <- integer(0)
  regions <- list()
  open <- FALSE
  for (i in seq_along(lines)) {
    ln <- lines[[i]]
    if (grepl(START, ln)) {
      if (open) stop(what, ": nested vignette-width:exempt start at line ", i, call. = FALSE)
      if (!grepl("start\\s+--\\s*[^-[:space:]]", ln))
        stop(what, ": the exempt region at line ", i, " states no reason; write ",
             "`start -- <why no width setting reaches this output>`.", call. = FALSE)
      open <- TRUE
      regions[[length(regions) + 1L]] <- integer(0)
      next
    }
    if (grepl(END, ln)) {
      if (!open) stop(what, ": vignette-width:exempt end without a start at line ", i, call. = FALSE)
      open <- FALSE
      next
    }
    if (!startsWith(ln, "#>")) next
    out_lines <- out_lines + 1L
    if (nchar(ln, type = "width") <= LIMIT) next
    if (open) {
      regions[[length(regions)]] <- c(regions[[length(regions)]], i)
    } else {
      wide <- c(wide, i)
    }
  }
  if (open) stop(what, ": a vignette-width:exempt region is never closed.", call. = FALSE)
  if (any(lengths(regions) == 0L))
    stop(what, ": a vignette-width:exempt region exempts no line wider than ", LIMIT,
         " columns; delete the marker.", call. = FALSE)
  list(out_lines = out_lines, wide = wide, regions = regions)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) stop("usage: Rscript tools/check-vignette-width.R [<directory>]", call. = FALSE)
dir <- if (length(args)) args[[1]] else "vignettes"

failed <- character(0)
for (name in VIGNETTES) {
  path <- file.path(dir, paste0(name, ".Rmd"))
  if (!file.exists(path)) stop("missing ", path, " -- run tools/precompute-vignettes.R first", call. = FALSE)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  res <- scan_widths(lines, path)
  if (res$out_lines == 0L)
    stop(path, ": no output line read; the guard has nothing to check.", call. = FALSE)
  exempted <- if (length(res$regions)) {
    paste0(" (lines ", paste(vapply(res$regions, paste, "", collapse = ", "), collapse = "; "), ")")
  } else ""
  cat(sprintf("%-36s %4d output lines, %d exemption(s)%s, %s\n",
              name, res$out_lines, length(res$regions), exempted,
              if (length(res$wide)) paste(length(res$wide), "TOO WIDE") else "all fit"))
  for (i in res$wide) {
    cat(sprintf("    line %d (%d columns): %s\n", i, nchar(lines[[i]], type = "width"), lines[[i]]))
  }
  if (length(res$wide)) failed <- c(failed, name)
}

if (length(failed)) {
  stop("vignette output wider than ", LIMIT, " columns in: ", paste(failed, collapse = ", "),
       "\nSet options(width = 77) in the vignette's setup chunk, or mark output no ",
       "width setting reaches with a vignette-width:exempt region.", call. = FALSE)
}
cat("all", length(VIGNETTES), "pre-computed vignettes print within", LIMIT, "columns\n")
