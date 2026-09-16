#!/usr/bin/env Rscript

# Guard: the vignette index and the Vignettes navbar menu in _pkgdown.yml list
# every vignette in vignettes/ exactly once, in the level order below, and the
# menu text of each entry is the vignette's title.
#
#   Rscript tools/check-pkgdown-vignettes.R
#
# Exit status 0 when the three lists agree, 1 on any difference. Needs the
# yaml package, which the package does not depend on; the script says so and
# exits 2 when it is absent. pkgdown::check_pkgdown() is a separate check: it
# reports a vignette missing from the index but reads no navbar.

if (!requireNamespace("yaml", quietly = TRUE)) {
  message("check-pkgdown-vignettes: the yaml package is not installed")
  quit(status = 2L)
}

# The level map. A vignette that does not appear here fails the check, so a
# new page is placed here and in _pkgdown.yml together.
LEVELS <- list(
  Introductory = c("using-instruments", "introduction-to-ssm-analysis"),
  Intermediate = c("intermediate-ssm-analysis", "evaluating-circumplex-structure"),
  Advanced = c(
    "advanced-visualization", "sem-based-ssm-analysis", "axes-reliability",
    "bayesian-ssm-analysis", "growth-ssm-analysis"
  )
)
EXPECTED <- unlist(LEVELS, use.names = FALSE)

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
titles <- vapply(file.path("vignettes", rmd), vignette_title, character(1))
names(titles) <- on_disk

if (!setequal(on_disk, EXPECTED)) {
  fail("vignettes/ and the level map differ: on disk only [",
       paste(setdiff(on_disk, EXPECTED), collapse = ", "), "], in the map only [",
       paste(setdiff(EXPECTED, on_disk), collapse = ", "), "]")
}

cfg <- yaml::read_yaml("_pkgdown.yml")

# The articles index: one group per level, the level map's pages in its order.
groups <- cfg$articles
if (is.null(groups)) {
  fail("_pkgdown.yml has no articles: section")
} else {
  got_titles <- vapply(groups, function(g) g$title, character(1))
  if (!identical(got_titles, names(LEVELS))) {
    fail("articles groups are [", paste(got_titles, collapse = ", "),
         "], expected [", paste(names(LEVELS), collapse = ", "), "]")
  }
  for (g in groups) {
    want <- LEVELS[[g$title]]
    got <- unlist(g$contents)
    if (!is.null(want) && !identical(got, want)) {
      fail("articles group ", g$title, " lists [", paste(got, collapse = ", "),
           "], expected [", paste(want, collapse = ", "), "]")
    }
  }
  listed <- unlist(lapply(groups, function(g) g$contents))
  dup <- unique(listed[duplicated(listed)])
  if (length(dup)) fail("listed more than once in articles: ", paste(dup, collapse = ", "))
}

# The navbar menu: the same pages in the same order, each with its title.
menus <- Filter(function(item) identical(item$text, "Vignettes"), cfg$navbar$left)
if (length(menus) != 1L) {
  fail("expected one navbar menu named Vignettes, found ", length(menus))
} else {
  entries <- menus[[1]]$menu
  hrefs <- vapply(entries, function(e) e$href, character(1))
  names_from_href <- sub("^articles/(.*)\\.html$", "\\1", hrefs)
  if (!identical(names_from_href, EXPECTED)) {
    fail("navbar Vignettes menu lists [", paste(names_from_href, collapse = ", "),
         "], expected [", paste(EXPECTED, collapse = ", "), "]")
  }
  for (e in entries) {
    name <- sub("^articles/(.*)\\.html$", "\\1", e$href)
    if (name %in% names(titles) && !identical(e$text, titles[[name]])) {
      fail("navbar text for ", name, " is \"", e$text, "\", the vignette title is \"",
           titles[[name]], "\"")
    }
  }
}

if (length(failures)) {
  cat(paste0("FAIL: ", failures, "\n"), sep = "")
  quit(status = 1L)
}
cat("the articles index, the navbar menu and vignettes/ agree on", length(EXPECTED), "pages\n")
