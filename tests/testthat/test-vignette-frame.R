# Guards for the frame every vignette carries: a Level line under the setup
# chunk, a numbered Overview that names the sections after it, consecutive
# section numbers, a Wrap-up that names the next page, and References last
# where the page cites any. The reading map below is the one the pkgdown index
# follows (tools/check-pkgdown-vignettes.R holds the level order).
#
# The frame is prose, so it survives pre-computation unchanged: the source tree
# is read under devtools::test() (the .Rmd.orig where one exists) and inst/doc
# under R CMD check. A build installed without vignettes skips.

frame_levels <- c(
  "using-instruments" = "Introductory",
  "introduction-to-ssm-analysis" = "Introductory",
  "intermediate-ssm-analysis" = "Intermediate",
  "evaluating-circumplex-structure" = "Intermediate",
  "advanced-visualization" = "Advanced",
  "sem-based-ssm-analysis" = "Advanced",
  "axes-reliability" = "Advanced",
  "bayesian-ssm-analysis" = "Advanced",
  "growth-ssm-analysis" = "Advanced"
)

# Reading order: each row is one page and the page that follows it.
frame_next <- rbind(
  c("using-instruments", "introduction-to-ssm-analysis"),
  c("introduction-to-ssm-analysis", "intermediate-ssm-analysis"),
  c("intermediate-ssm-analysis", "evaluating-circumplex-structure"),
  c("evaluating-circumplex-structure", "advanced-visualization"),
  c("evaluating-circumplex-structure", "sem-based-ssm-analysis"),
  c("sem-based-ssm-analysis", "axes-reliability"),
  c("intermediate-ssm-analysis", "bayesian-ssm-analysis"),
  c("bayesian-ssm-analysis", "growth-ssm-analysis")
)

frame_path <- function(name) {
  candidates <- c(
    test_path("..", "..", "vignettes", paste0(name, ".Rmd.orig")),
    test_path("..", "..", "vignettes", paste0(name, ".Rmd")),
    system.file("doc", paste0(name, ".Rmd"), package = "circumplex")
  )
  hit <- candidates[nzchar(candidates) & file.exists(candidates)]
  skip_if(length(hit) == 0L, "vignette source unavailable in this build")
  hit[[1]]
}

frame_lines <- function(name) {
  readLines(frame_path(name), warn = FALSE, encoding = "UTF-8")
}

frame_title <- function(lines) {
  hit <- regmatches(lines, regexec("\\\\VignetteIndexEntry\\{([^}]*)\\}", lines))
  hit <- Filter(length, hit)
  expect_length(hit, 1L)
  hit[[1]][[2]]
}

frame_titles <- function() {
  vapply(names(frame_levels), function(n) frame_title(frame_lines(n)), character(1))
}

# Page titles named in a piece of text: double-quoted strings that begin with
# a capital letter. A quoted question or phrase in lower case is not a title.
quoted <- function(text) {
  m <- gregexpr("\"([A-Z][^\"]*)\"", text)
  unlist(lapply(regmatches(text, m), function(x) gsub("\"", "", x)))
}

# The first prose line after the chunk that attaches the package. A fenced
# block between the two is skipped, so a note chunk under the setup chunk does
# not count as prose.
first_prose_after_setup <- function(lines) {
  start <- which(grepl("^library\\(circumplex\\)", lines))[[1]]
  i <- start + 1L
  # The scan starts inside the setup chunk, so the first fence it meets closes
  # that chunk.
  fence <- TRUE
  while (i <= length(lines)) {
    ln <- lines[[i]]
    if (grepl("^```", ln)) {
      fence <- !fence
    } else if (!fence && nzchar(trimws(ln))) {
      return(ln)
    }
    i <- i + 1L
  }
  NA_character_
}

section_body <- function(lines, heading_re) {
  heads <- which(grepl("^## ", lines))
  start <- heads[grepl(heading_re, lines[heads])]
  expect_length(start, 1L)
  after <- heads[heads > start]
  end <- if (length(after)) after[[1]] - 1L else length(lines)
  # Runs of whitespace collapse to one space, so a heading or a title that
  # wraps across source lines still matches.
  gsub("\\s+", " ", paste(lines[seq(start + 1L, end)], collapse = " "))
}

test_that("every vignette opens with a Level line naming its level and prerequisites", {
  titles <- frame_titles()
  for (name in names(frame_levels)) {
    lines <- frame_lines(name)
    line <- first_prose_after_setup(lines)
    expect_match(line, "^\\*\\*Level:\\*\\* (Introductory|Intermediate|Advanced)\\. ",
                 info = name)
    level <- sub("^\\*\\*Level:\\*\\* (\\w+)\\..*$", "\\1", line)
    expect_identical(level, unname(frame_levels[[name]]), info = name)
    named <- quoted(line)
    if (length(named) == 0L) {
      expect_match(line, "[Nn]o earlier page", info = name)
    } else {
      expect_true(all(named %in% titles), info = paste(name, "names", toString(named)))
    }
  }
})

test_that("headings run Overview, numbered sections, Wrap-up, References", {
  for (name in names(frame_levels)) {
    lines <- frame_lines(name)
    heads <- lines[grepl("^## ", lines)]
    n <- length(heads)
    has_refs <- identical(heads[[n]], "## References")
    numbered <- heads[seq_len(n - 1L - has_refs)]
    expect_identical(numbered[[1]], "## 1. Overview", info = name)
    expect_identical(heads[[n - has_refs]], "## Wrap-up", info = name)
    nums <- sub("^## ([0-9]+)\\. .*$", "\\1", numbered)
    expect_identical(nums, as.character(seq_along(numbered)), info = name)
    overview <- section_body(lines, "^## 1\\. Overview$")
    for (h in heads[-1]) {
      text <- sub("^## ([0-9]+\\. )?", "", h)
      expect_true(grepl(text, overview, fixed = TRUE),
                  info = paste(name, "Overview lacks", text))
    }
  }
})

test_that("every Wrap-up names the next page, and the reading order is followed", {
  titles <- frame_titles()
  for (name in names(frame_levels)) {
    wrap <- section_body(frame_lines(name), "^## Wrap-up$")
    named <- quoted(wrap)
    expect_gt(length(named), 0L, label = paste(name, "Wrap-up titles"))
    expect_true(all(named %in% titles), info = paste(name, "names", toString(named)))
    following <- frame_next[frame_next[, 1] == name, 2]
    for (nxt in following) {
      expect_true(titles[[nxt]] %in% named,
                  info = paste(name, "Wrap-up does not name", titles[[nxt]]))
    }
  }
})
