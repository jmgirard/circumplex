#!/usr/bin/env Rscript

# Guard: no line in a vignette's code boxes is wider than the website's code
# box. The pkgdown code box scrolls sideways (overflow-x: auto) past its width,
# measured on the live site on 2026-09-15 at 81 monospace characters. The
# limit here is 80 display columns. The vignette sources set
# options(width = 77) so that output, with knitr's "#> " prefix, fits in 80.
#
#   Rscript tools/check-vignette-width.R              # reads vignettes/
#   Rscript tools/check-vignette-width.R <directory>  # reads a copy elsewhere
#
# The files read are every <name>.Rmd in the directory (M164). A name that
# tools/precompute-vignettes.R renders is read as it stands on disk, because
# that file already carries its output. Any other vignette is live: it ships
# without output, so the guard knits it into a temporary copy of the directory
# and reads the knitted markdown. The knit runs library(circumplex), so it
# reads the INSTALLED package. Install the branch first (R CMD INSTALL .), or
# the guard measures the output of whatever build is installed.
#
# The guard measures every line inside a code fence, because every such line
# is drawn in a code box. That is echoed source and printed output alike,
# whatever knitr `comment` prefix or indent the output carries, and a fence
# that a results = "asis" chunk writes. A fence opens at a line that starts at
# column 1 with three or more backticks or tildes. It closes at the next line
# that starts at column 1 with the same character, at least as many of them,
# and nothing else. A fence that never closes is an error. A line outside a
# fence is not measured: asis text outside a fence renders as prose or a
# table, not in a code box. A fence indented by 1 to 3 spaces, or a code block
# made by a 4-space indent, is not read either.
#
# Width is display columns (nchar(type = "width")), not bytes or characters,
# so a line of Greek letters and superscripts is measured as it looks. Tabs
# are expanded to 8-column stops before the line is measured.
#
# Output that no width setting reaches, such as text printed with cat(), is
# exempted by an entry in EXEMPT below: a vignette name, a regular expression
# anchored at the start of the line, and the reason. An entry exempts only the
# lines its pattern matches in its own vignette, at any width and in any
# fence: an exempted line has no width cap. An entry that matches no over-wide
# line is an error, so an entry cannot outlive the output it was written for.
# Table and data-frame output is never wrapped to make it fit; such a line
# gets an entry here instead.
#
# Run by .github/workflows/vignette-precompute.yaml after the re-render.
# tools/check-vignette-width-plants.R shows that the guard fails on each kind
# of line it claims to read. Base R and knitr only.

LIMIT <- 80L

EXEMPT <- list(
  list(vignette = "sem-based-ssm-analysis", pattern = "^#> cx =~ ",
       reason = "the loading line lists every scale name; no width setting reaches cat() output"),
  list(vignette = "sem-based-ssm-analysis", pattern = "^#> cy =~ ",
       reason = "the loading line lists every scale name; no width setting reaches cat() output")
)

env <- new.env()
sys.source("tools/precompute-vignettes.R", env)
PRECOMPUTED <- env$VIGNETTES

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) stop("usage: Rscript tools/check-vignette-width.R [<directory>]", call. = FALSE)
dir <- if (length(args)) args[[1]] else "vignettes"

NAMES <- sub("\\.Rmd$", "", list.files(dir, pattern = "\\.Rmd$"))
if (!length(NAMES)) stop("no .Rmd file in ", dir, "; the guard has nothing to check.", call. = FALSE)

missing <- setdiff(PRECOMPUTED, NAMES)
if (length(missing))
  stop("missing ", paste0(file.path(dir, missing), ".Rmd", collapse = ", "),
       " -- run tools/precompute-vignettes.R first", call. = FALSE)

for (e in EXEMPT) {
  if (!startsWith(e$pattern, "^"))
    stop("exemption pattern `", e$pattern, "` is not anchored at the start of the line.", call. = FALSE)
  if (!e$vignette %in% NAMES)
    stop("exemption `", e$pattern, "` names `", e$vignette,
         "`, which is not a vignette in ", dir, ".", call. = FALSE)
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

# Returns the numbers of the lines inside code fences. A fence line itself is
# not inside. Stops on a fence that never closes.
fenced_lines <- function(lines, path) {
  inside <- logical(length(lines))
  open <- NULL
  open_at <- NA_integer_
  for (i in seq_along(lines)) {
    run <- regmatches(lines[[i]], regexpr("^(`{3,}|~{3,})", lines[[i]]))
    if (is.null(open)) {
      if (length(run)) {
        open <- run
        open_at <- i
      }
    } else if (length(run) && identical(lines[[i]], run) &&
               substr(run, 1, 1) == substr(open, 1, 1) &&
               nchar(run) >= nchar(open)) {
      open <- NULL
    } else {
      inside[[i]] <- TRUE
    }
  }
  if (!is.null(open))
    stop(path, ": the code fence opened at line ", open_at, " never closes.", call. = FALSE)
  which(inside)
}

# Knits each live vignette into a temporary copy of `dir`, so the figures and
# files it writes stay out of the source tree, and returns the paths of the
# knitted markdown files, named by vignette.
knit_live <- function(names) {
  if (!length(names)) return(character(0))
  copy <- tempfile("vignette-width-")
  dir.create(copy)
  file.copy(list.files(dir, full.names = TRUE, all.files = TRUE, no.. = TRUE),
            copy, recursive = TRUE)
  owd <- setwd(copy)
  on.exit(setwd(owd), add = TRUE)
  out <- character(0)
  for (name in names) {
    md <- paste0(name, ".md")
    knitr::knit(paste0(name, ".Rmd"), md, quiet = TRUE, envir = new.env(parent = globalenv()))
    out[[name]] <- file.path(copy, md)
  }
  out
}

# Returns, for one file, the number of fenced lines read, the over-wide lines
# no entry exempts, and the over-wide lines each entry exempts.
scan_widths <- function(lines, path, entries) {
  fenced <- fenced_lines(lines, path)
  wide <- fenced[vapply(lines[fenced], display_width, 0L) > LIMIT]
  hits <- lapply(entries, function(e) wide[grepl(e$pattern, lines[wide], perl = TRUE)])
  list(fenced = length(fenced), wide = setdiff(wide, unlist(hits)), hits = hits)
}

live <- setdiff(NAMES, PRECOMPUTED)
knitted <- knit_live(live)

failed <- character(0)
for (name in NAMES) {
  path <- if (name %in% live) knitted[[name]] else file.path(dir, paste0(name, ".Rmd"))
  label <- paste0(name, ".Rmd", if (name %in% live) " (knitted)" else "")
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  entries <- Filter(function(e) identical(e$vignette, name), EXEMPT)
  res <- scan_widths(lines, label, entries)
  if (res$fenced == 0L)
    stop(label, ": no line inside a code fence; the guard has nothing to check.", call. = FALSE)
  for (k in seq_along(entries)) {
    if (!length(res$hits[[k]]))
      stop(label, ": the exemption `", entries[[k]]$pattern, "` matches no code-box line wider than ",
           LIMIT, " columns; delete the entry from EXEMPT.", call. = FALSE)
  }
  exempted <- sort(unlist(res$hits))
  cat(sprintf("%-47s %4d code-box lines, %d exempted%s, %s\n",
              label, res$fenced, length(exempted),
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
  stop("code-box lines wider than ", LIMIT, " columns in: ", paste(failed, collapse = ", "),
       "\nSet options(width = 77) in the vignette's setup chunk, wrap the code or prose that ",
       "prints the line, or add an entry to EXEMPT in tools/check-vignette-width.R for output ",
       "no width setting reaches.", call. = FALSE)
}
cat("all", length(NAMES), "vignettes print within", LIMIT, "columns in their code boxes\n")
