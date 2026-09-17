#!/usr/bin/env Rscript

# Guard: the vignette index and the Vignettes navbar menu in _pkgdown.yml list
# every vignette in vignettes/ exactly once, in the level order below, and the
# menu text of each entry is the vignette's title. The navbar menu is grouped
# by level, so a page must also sit under the heading for its own level, and
# the menu's headings must be exactly the level map's, in its order: an extra
# heading, a missing one or a reordered one fails, whatever the pages under it.
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
  Intermediate = c(
    "intermediate-ssm-analysis", "evaluating-circumplex-structure",
    "ci-accuracy", "structure-tests"
  ),
  Advanced = c(
    "cpm-boundary-fits", "advanced-visualization", "sem-based-ssm-analysis",
    "sem-latent-contrasts", "axes-reliability", "axes-reliability-caveats",
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

# The navbar menu: one heading per level, and under each heading the same pages
# in the same order, each with its title. Divider placement is not checked.
# An entry with no href whose text is three or more dashes is a divider, which
# this check skips, so a shorter run of dashes is a heading here, as pkgdown
# renders it. The dash pattern is pkgdown's own `^\s*-{3,}\s*$`, but pkgdown's
# menu_type() tests it BEFORE it looks at href, and this check tests it only
# among the href-less entries: an entry carrying both dash text and an href is
# a separator to pkgdown and a page here, so it fails rather than passing
# wrongly. An entry with text and no href is a heading. An entry with an href
# is a page under the heading above it.
menus <- Filter(function(item) identical(item$text, "Vignettes"), cfg$navbar$left)
if (length(menus) != 1L) {
  fail("expected one navbar menu named Vignettes, found ", length(menus))
} else {
  entries <- menus[[1]]$menu
  headings <- character(0)
  under <- list()
  current <- NA_character_
  for (e in entries) {
    text <- if (is.null(e$text)) "" else e$text
    if (is.null(e$href)) {
      if (grepl("^\\s*-{3,}\\s*$", text)) next
      current <- text
      headings <- c(headings, text)
      under[[text]] <- character(0)
      next
    }
    name <- sub("^articles/(.*)\\.html$", "\\1", e$href)
    if (is.na(current)) {
      fail("navbar entry ", name, " sits above the first level heading")
      next
    }
    under[[current]] <- c(under[[current]], name)
    if (name %in% names(titles) && !identical(text, titles[[name]])) {
      fail("navbar text for ", name, " is \"", text, "\", the vignette title is \"",
           titles[[name]], "\"")
    }
  }
  if (!identical(headings, names(LEVELS))) {
    fail("navbar level headings are [", paste(headings, collapse = ", "),
         "], expected [", paste(names(LEVELS), collapse = ", "), "]")
  }
  for (level in intersect(headings, names(LEVELS))) {
    want <- LEVELS[[level]]
    got <- under[[level]]
    if (!identical(got, want)) {
      fail("navbar menu under ", level, " lists [", paste(got, collapse = ", "),
           "], expected [", paste(want, collapse = ", "), "]")
    }
  }
}

if (length(failures)) {
  cat(paste0("FAIL: ", failures, "\n"), sep = "")
  quit(status = 1L)
}
cat("the articles index, the navbar menu and vignettes/ agree on", length(EXPECTED), "pages\n")
