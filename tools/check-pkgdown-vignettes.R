#!/usr/bin/env Rscript

# Guard: the articles index in _pkgdown.yml lists every vignette in vignettes/
# exactly once, grouped into the levels of the map in
# tests/testthat/helper-vignette-frame.R, in the map's level order and, inside
# a level, its page order. Each group also carries a `navbar:` key equal to its
# title, which is the heading pkgdown puts over that group in the navbar's
# generated Articles menu.
#
# There is no hand-written vignette menu to check: since M144 pkgdown builds
# the menu from this index, so a page listed once in the right group is enough
# for the index, the menu and the reading map to agree.
#
#   Rscript tools/check-pkgdown-vignettes.R
#
# Exit status 0 when the index, the map and vignettes/ agree, 1 on any
# difference. Needs the yaml package, which the package does not depend on; the
# script says so and exits 2 when it is absent. pkgdown::check_pkgdown() is a
# separate check: it reports a vignette missing from the index, and reads
# neither the grouping nor the order.

if (!requireNamespace("yaml", quietly = TRUE)) {
  message("check-pkgdown-vignettes: the yaml package is not installed")
  quit(status = 2L)
}

# The level map is `frame_levels` in the frame test's helper, the one place it
# is defined; a vignette that does not appear there fails the check, so a new
# page is placed there and in _pkgdown.yml together. Level order and the page
# order within a level are the map's own order.
frame <- new.env()
sys.source("tests/testthat/helper-vignette-frame.R", envir = frame)
level_pages <- split(names(frame$frame_levels), factor(frame$frame_levels, levels = unique(frame$frame_levels)))
EXPECTED <- names(frame$frame_levels)

vignette_title <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  hit <- regmatches(lines, regexec("\\\\VignetteIndexEntry\\{([^}]*)\\}", lines))
  hit <- Filter(length, hit)
  if (length(hit) != 1L) stop(path, ": expected one VignetteIndexEntry", call. = FALSE)
  hit[[1]][[2]]
}

failures <- character(0)
fail <- function(...) failures <<- c(failures, paste0(...))

rmd <- sort(list.files("vignettes", pattern = "\\.Rmd$"))
on_disk <- sub("\\.Rmd$", "", rmd)
# Every page's title is read, so a file carrying no index entry, or two, stops
# the guard by name. pkgdown takes each menu entry's text from the same entry.
invisible(vapply(file.path("vignettes", rmd), vignette_title, character(1)))

if (!setequal(on_disk, EXPECTED)) {
  fail("vignettes/ and the level map differ: on disk only [",
       paste(setdiff(on_disk, EXPECTED), collapse = ", "), "], in the map only [",
       paste(setdiff(EXPECTED, on_disk), collapse = ", "), "]")
}

cfg <- yaml::read_yaml("_pkgdown.yml")

# The articles index: one group per level, in the map's order, holding that
# level's pages in the map's order, and carrying the navbar heading pkgdown
# puts over the group.
groups <- cfg$articles
if (is.null(groups)) {
  fail("_pkgdown.yml has no articles: section")
} else {
  got_titles <- vapply(groups, function(g) g$title, character(1))
  if (!identical(got_titles, names(level_pages))) {
    fail("articles groups are [", paste(got_titles, collapse = ", "),
         "], expected [", paste(names(level_pages), collapse = ", "), "]")
  }
  for (g in groups) {
    want <- level_pages[[g$title]]
    got <- unlist(g$contents)
    if (!is.null(want) && !identical(got, want)) {
      fail("articles group ", g$title, " lists [", paste(got, collapse = ", "),
           "], expected [", paste(want, collapse = ", "), "]")
    }
    nav <- g$navbar
    if (!identical(nav, g$title)) {
      fail("articles group ", g$title, " has navbar heading ",
           if (is.null(nav)) "none, so the group gets no heading in the menu" else paste0("\"", nav, "\""),
           ", expected \"", g$title, "\"")
    }
  }
  listed <- unlist(lapply(groups, function(g) g$contents))
  dup <- unique(listed[duplicated(listed)])
  if (length(dup)) fail("listed more than once in articles: ", paste(dup, collapse = ", "))
}

if (length(failures)) {
  cat(paste0("FAIL: ", failures, "\n"), sep = "")
  quit(status = 1L)
}
cat("the articles index, the level map and vignettes/ agree on", length(EXPECTED), "pages\n")
