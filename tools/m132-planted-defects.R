#!/usr/bin/env Rscript

# M132 evidence: tools/check-vignette-width.R goes red on each planted defect
# form, in each of the seven pre-computed vignettes, one file at a time, and
# stays green on the controls. Every plant is made in a temporary copy of
# vignettes/; the working tree is never touched.
#
#   Rscript tools/m132-planted-defects.R          # run from the repo root
#
# Forms (each planted in one file while the other six stay as rendered):
#   ascii    an 81-column ASCII output line
#   unicode  an 81-column output line built from "ζ" and "²" (159 bytes)
#   outside  a long line just after the end marker of an exemption region
#   rerender a chunk added to the .Rmd.orig source that prints a 93-column
#            line, then the vignette is re-rendered
# Controls (each must stay green):
#   unicode80  an 80-column line of "ζ" and "²" (157 bytes): a guard that
#              counted bytes rather than display columns would go red here
#   inside     a long line just before the end marker of an exemption region
#
# A red counts only when the guard names the planted line by its line number
# and column count and names no other file. A green counts only when the
# guard exits 0. circumplex must be installed, as for precompute-vignettes.R.

env <- new.env()
sys.source("tools/precompute-vignettes.R", env)
VIGNETTES <- env$VIGNETTES
REPO <- normalizePath(".")
RSCRIPT <- file.path(R.home("bin"), "Rscript")

START <- "<!--\\s*vignette-width:exempt start\\b"
END <- "<!--\\s*vignette-width:exempt end\\s*-->"

stopifnot(system2(RSCRIPT, "tools/check-vignette-width.R", stdout = FALSE, stderr = FALSE) == 0L)

fresh_copy <- function() {
  root <- tempfile("m132-")
  dir.create(root)
  file.copy(file.path(REPO, "vignettes"), root, recursive = TRUE)
  file.path(root, "vignettes")
}

run_guard <- function(dir) {
  out <- suppressWarnings(system2(RSCRIPT, c("tools/check-vignette-width.R", dir),
                                  stdout = TRUE, stderr = TRUE))
  list(status = if (is.null(attr(out, "status"))) 0L else attr(out, "status"), out = out)
}

in_region <- function(lines) {
  inside <- logical(length(lines))
  open <- FALSE
  for (i in seq_along(lines)) {
    if (grepl(START, lines[[i]])) open <- TRUE
    inside[[i]] <- open
    if (grepl(END, lines[[i]])) open <- FALSE
  }
  inside
}

insert_after <- function(lines, at, new) append(lines, new, after = at)

# A file without a region of its own gets one planted after its first output
# line, holding a long line so the region is valid. Returns the lines and the
# index of the region's end marker.
with_region <- function(lines) {
  ends <- grep(END, lines)
  if (length(ends)) return(list(lines = lines, end = ends[[1]]))
  first <- which(startsWith(lines, "#>") & !in_region(lines))[[1]]
  region <- c("", "<!-- vignette-width:exempt start -- m132 planted region -->",
              "#> ", paste0("#> ", strrep("r", 85)),
              "<!-- vignette-width:exempt end -->", "")
  lines <- insert_after(lines, first, region)
  list(lines = lines, end = first + 5L)
}

plant <- function(name, form) {
  dir <- fresh_copy()
  path <- file.path(dir, paste0(name, ".Rmd"))
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  first_out <- which(startsWith(lines, "#>") & !in_region(lines))[[1]]
  wide <- paste0("#> ", strrep("w", 86))
  at <- NA_integer_
  if (form == "ascii") {
    new <- paste0("#> ", strrep("a", 78))
    lines <- insert_after(lines, first_out, new); at <- first_out + 1L
  } else if (form == "unicode") {
    new <- paste0("#> ", strrep("\u03b6\u00b2", 39))
    lines <- insert_after(lines, first_out, new); at <- first_out + 1L
  } else if (form == "unicode80") {
    new <- paste0("#> ", strrep("\u03b6\u00b2", 38), "u")
    lines <- insert_after(lines, first_out, new)
  } else if (form == "outside") {
    r <- with_region(lines); lines <- r$lines
    new <- wide
    lines <- insert_after(lines, r$end, new); at <- r$end + 1L
  } else if (form == "inside") {
    r <- with_region(lines); lines <- r$lines
    new <- wide
    lines <- insert_after(lines, r$end - 1L, new)
  } else if (form == "rerender") {
    orig <- file.path(dir, paste0(name, ".Rmd.orig"))
    src <- readLines(orig, warn = FALSE, encoding = "UTF-8")
    src <- c(src, "", "```{r m132-plant, eval = TRUE}", "cat(strrep(\"x\", 90), \"\\n\")", "```")
    writeLines(src, orig, useBytes = TRUE)
    code <- sprintf("setwd(%s); e <- new.env(); sys.source(%s, e); e$knit_one(%s)",
                    deparse(dirname(dir)), deparse(file.path(REPO, "tools/precompute-vignettes.R")),
                    deparse(name))
    status <- system2(RSCRIPT, c("-e", shQuote(code)), stdout = FALSE, stderr = FALSE)
    if (status != 0L) stop("re-render of ", name, " failed", call. = FALSE)
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    new <- paste0("#> ", strrep("x", 90))
    at <- which(lines == new)
    if (length(at) != 1L) stop("re-render of ", name, " did not print the planted line once", call. = FALSE)
  }
  if (form != "rerender") writeLines(enc2utf8(lines), path, useBytes = TRUE)
  cols <- nchar(new, type = "width")
  res <- run_guard(dir)
  unlink(dirname(dir), recursive = TRUE)
  expect_red <- !is.na(at)
  pass <- if (expect_red) {
    named <- any(grepl(sprintf("^    line %d \\(%d columns\\)", at, cols), res$out))
    others <- setdiff(VIGNETTES, name)
    blamed <- any(vapply(others, function(o) any(grepl(paste0("^", o, " .*TOO WIDE"), res$out)), TRUE))
    res$status != 0L && named && !blamed
  } else {
    res$status == 0L
  }
  data.frame(vignette = name, form = form, columns = cols,
             expected = if (expect_red) "red" else "green",
             guard_exit = res$status, pass = pass)
}

FORMS <- c("ascii", "unicode", "outside", "rerender", "unicode80", "inside")
results <- do.call(rbind, lapply(VIGNETTES, function(name) {
  do.call(rbind, lapply(FORMS, function(form) {
    cat("planting", form, "in", name, "\n")
    plant(name, form)
  }))
}))
print(results, row.names = FALSE)
if (!all(results$pass)) stop(sum(!results$pass), " plant(s) did not give the expected result", call. = FALSE)
cat("all", nrow(results), "plants gave the expected result\n")
