#!/usr/bin/env Rscript

# Guard: no "#>" output line in a pre-computed vignette is wider than the
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
# Only lines that start with "#>" are read. The guard does not read output
# printed under another knitr `comment` prefix, output of a chunk with
# results = "asis", or indented output (a "#>" line that does not start at
# column 1).
#
# Width is display columns (nchar(type = "width")), not bytes or characters,
# so a line of Greek letters and superscripts is measured as it looks. Tabs
# are expanded to 8-column stops before the line is measured.
#
# Output that no width setting reaches, such as text printed with cat(), is
# exempted by an entry in EXEMPT below: a vignette name, a regular expression
# anchored at the start of the line, and the reason. An entry exempts only the
# lines its pattern matches in its own vignette. An entry that matches no
# over-wide "#>" line is an error, so an entry cannot outlive the output it was
# written for.
#
# Run by .github/workflows/vignette-precompute.yaml after the re-render.
# Base R only.

LIMIT <- 80L

EXEMPT <- list(
  list(vignette = "sem-based-ssm-analysis", pattern = "^#> cx =~ ",
       reason = "the loading line lists every scale name; no width setting reaches cat() output"),
  list(vignette = "sem-based-ssm-analysis", pattern = "^#> cy =~ ",
       reason = "the loading line lists every scale name; no width setting reaches cat() output")
)

env <- new.env()
sys.source("tools/precompute-vignettes.R", env)
VIGNETTES <- env$VIGNETTES

for (e in EXEMPT) {
  if (!startsWith(e$pattern, "^"))
    stop("exemption pattern `", e$pattern, "` is not anchored at the start of the line.", call. = FALSE)
  if (!e$vignette %in% VIGNETTES)
    stop("exemption `", e$pattern, "` names `", e$vignette,
         "`, which tools/precompute-vignettes.R does not render.", call. = FALSE)
}

# Replaces each tab with the spaces that reach the next 8-column stop.
expand_tabs <- function(ln) {
  if (!grepl("\t", ln, fixed = TRUE)) return(ln)
  pieces <- regmatches(ln, gregexpr("\t", ln, fixed = TRUE), invert = TRUE)[[1]]
  out <- pieces[[1]]
  for (piece in pieces[-1]) {
    out <- paste0(out, strrep(" ", 8L - nchar(out, type = "width") %% 8L), piece)
  }
  out
}

display_width <- function(ln) nchar(expand_tabs(ln), type = "width")

# Returns, for one rendered vignette, the number of output lines read, the
# over-wide lines no entry exempts, and the over-wide lines each entry exempts.
scan_widths <- function(lines, entries) {
  out <- which(startsWith(lines, "#>"))
  wide <- out[vapply(lines[out], display_width, 0L) > LIMIT]
  hits <- lapply(entries, function(e) wide[grepl(e$pattern, lines[wide], perl = TRUE)])
  list(out_lines = length(out), wide = setdiff(wide, unlist(hits)), hits = hits)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) stop("usage: Rscript tools/check-vignette-width.R [<directory>]", call. = FALSE)
dir <- if (length(args)) args[[1]] else "vignettes"

failed <- character(0)
for (name in VIGNETTES) {
  path <- file.path(dir, paste0(name, ".Rmd"))
  if (!file.exists(path)) stop("missing ", path, " -- run tools/precompute-vignettes.R first", call. = FALSE)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  entries <- Filter(function(e) identical(e$vignette, name), EXEMPT)
  res <- scan_widths(lines, entries)
  if (res$out_lines == 0L)
    stop(path, ": no output line read; the guard has nothing to check.", call. = FALSE)
  for (k in seq_along(entries)) {
    if (!length(res$hits[[k]]))
      stop(path, ": the exemption `", entries[[k]]$pattern, "` matches no output line wider than ",
           LIMIT, " columns; delete the entry from EXEMPT.", call. = FALSE)
  }
  exempted <- sort(unlist(res$hits))
  cat(sprintf("%-36s %4d output lines, %d exempted%s, %s\n",
              name, res$out_lines, length(exempted),
              if (length(exempted)) paste0(" (lines ", paste(exempted, collapse = ", "), ")") else "",
              if (length(res$wide)) paste(length(res$wide), "TOO WIDE") else "all fit"))
  for (i in exempted) {
    cat(sprintf("    exempt line %d (%d columns): %s\n", i, display_width(lines[[i]]), lines[[i]]))
  }
  for (i in res$wide) {
    cat(sprintf("    line %d (%d columns): %s\n", i, display_width(lines[[i]]), lines[[i]]))
  }
  if (length(res$wide)) failed <- c(failed, name)
}

if (length(failed)) {
  stop("vignette output wider than ", LIMIT, " columns in: ", paste(failed, collapse = ", "),
       "\nSet options(width = 77) in the vignette's setup chunk, or add an entry to ",
       "EXEMPT in tools/check-vignette-width.R for output no width setting reaches.", call. = FALSE)
}
cat("all", length(VIGNETTES), "pre-computed vignettes print within", LIMIT, "columns\n")
