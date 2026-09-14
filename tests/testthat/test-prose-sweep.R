# tools/prose-sweep.R is a source-tree tool: tools/ is .Rbuildignore'd, so the
# script exists only where devtools::test() reads the source tree.

sweep_script <- function() {
  path <- testthat::test_path("..", "..", "tools", "prose-sweep.R")
  if (!file.exists(path)) testthat::skip("tools/prose-sweep.R is not in this build")
  normalizePath(path)
}

run_sweep <- function(lines, args = character(0), env = "LC_ALL=en_US.UTF-8") {
  script <- sweep_script()
  fixture <- tempfile(fileext = ".Rmd")
  if (is.raw(lines)) {
    writeBin(lines, fixture)
  } else {
    con <- file(fixture, open = "w", encoding = "UTF-8")
    writeLines(lines, con)
    close(con)
  }
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c(shQuote(script), args, shQuote(fixture)),
    stdout = TRUE, stderr = TRUE,
    env = env
  ))
  status <- attr(out, "status")
  list(out = as.character(out), status = if (is.null(status)) 0L else status)
}

# Each report line reads "<file>:<line>: <kind>...". Returns "line kind" keys.
finding_keys <- function(out) {
  m <- regmatches(out, regexec(":([0-9]+): (long sentence|dash|semicolon|id)", out))
  vapply(Filter(length, m), function(x) paste(x[[2]], x[[3]]), character(1))
}

long_text <- function(tag) paste(tag, paste(rep("word", 30), collapse = " "))

planted_page <- function() {
  c(
    "---",
    "title: \"Planted page; with a semicolon — and a dash in YAML\"",
    "---",
    "",
    "Opening sentence of the page.",
    "",
    paste0("- ", long_text("PLANT-list"), "."),
    "",
    paste0("## ", long_text("PLANT-heading")),
    "",
    paste0("> ", long_text("PLANT-quote"), "."),
    "",
    "| name | text |",
    "|---|:---:|",
    paste0("| short | ", long_text("PLANT-cell"), " |"),
    "",
    "PLANT-emdash a pause — then more.",
    "",
    "PLANT-triple a pause --- then more.",
    "",
    "PLANT-double a pause -- then more.",
    "",
    "PLANT-entity a pause &mdash; then more.",
    "",
    "PLANT-semicolon one clause; another clause.",
    "",
    "SILENT-code the span `a; b --- c` stays quiet.",
    "",
    "SILENT-math the span $a; b$ stays quiet.",
    "",
    "SILENT-entity salt &amp; pepper stay quiet.",
    "",
    "PLANT-id-prose the work in M123 shipped.",
    "",
    "PLANT-id-link see [D-015](https://example.org/notes) for more.",
    "",
    "PLANT-id-url see [the notes](https://example.org/RR22) for more.",
    "",
    "SILENT-initial J. Smith wrote this with a short sentence, e.g. this one.",
    "",
    "```{r, echo = FALSE}",
    paste0("# ", long_text("SILENT-fence-info"), "; a — dash --- here"),
    "```",
    "",
    "```",
    paste0(long_text("SILENT-fence-bare"), "; a — dash --- here"),
    "```",
    "",
    "~~~",
    paste0(long_text("SILENT-fence-tilde"), "; a — dash --- here"),
    "~~~",
    "",
    "<!--",
    paste0(long_text("SILENT-comment"), "; a — dash --- here M123"),
    "-->",
    "",
    "Closing sentence of the page.",
    "",
    "## References",
    "",
    paste0(long_text("SILENT-references"), "; a — dash --- here.")
  )
}

test_that("each planted finding is reported, and only at its plant", {
  page <- planted_page()
  res <- run_sweep(page)
  expect_identical(res$status, 1L)

  at <- function(tag) which(grepl(tag, page, fixed = TRUE))[[1]]
  expected <- c(
    paste(at("PLANT-list"), "long sentence"),
    paste(at("PLANT-heading"), "long sentence"),
    paste(at("PLANT-quote"), "long sentence"),
    paste(at("PLANT-cell"), "long sentence"),
    paste(at("PLANT-emdash"), "dash"),
    paste(at("PLANT-triple"), "dash"),
    paste(at("PLANT-double"), "dash"),
    paste(at("PLANT-entity"), "dash"),
    paste(at("PLANT-semicolon"), "semicolon"),
    paste(at("PLANT-id-prose"), "id"),
    paste(at("PLANT-id-link"), "id"),
    paste(at("PLANT-id-url"), "id")
  )
  expect_setequal(finding_keys(res$out), expected)
  expect_length(finding_keys(res$out), length(expected))
})

test_that("a long sentence in a paragraph is counted across source lines", {
  words <- rep("word", 26)
  page <- c(
    "Intro line.",
    "",
    paste(words[1:13], collapse = " "),
    paste(paste(words[14:26], collapse = " "), "end.")
  )
  res <- run_sweep(page)
  expect_identical(res$status, 1L)
  expect_identical(finding_keys(res$out), "3 long sentence")
  expect_match(res$out, "(27 words)", fixed = TRUE)

  # 25 words is the limit, not over it; a code span and a math span each count
  # as one word however long they are.
  at_limit <- paste(paste(rep("word", 23), collapse = " "), "`a b c d e f` $x + y + z$.")
  res <- run_sweep(c("Intro line.", "", at_limit))
  expect_identical(res$status, 0L)
  expect_identical(res$out, character(0))

  # Two 20-word sentences in one paragraph are two sentences, not one of 40.
  twenty <- paste(paste(rep("word", 19), collapse = " "), "end.")
  res <- run_sweep(c("Intro line.", "", paste(twenty, twenty)))
  expect_identical(res$status, 0L)
  expect_identical(res$out, character(0))
})

test_that("a table under a knitr Table: caption is not swept, other tables are", {
  # knitr::kable(caption = ...) knits to a "Table:" caption line, a blank line
  # and a pipe table. That text comes from chunk code, so the sweep skips it.
  page <- c(
    "Intro line.",
    "",
    "Table: SILENT-caption; with a semicolon",
    "",
    "|Question |Confounds |",
    "|:--------|:---------|",
    paste0("|short |", long_text("SILENT-kable"), "; a — dash |"),
    "",
    "PLANT-after a pause — then more.",
    "",
    "| name | text |",
    "|---|---|",
    paste0("| short | ", long_text("PLANT-bare-cell"), " |"),
    "",
    "Table: PLANT-caption-prose; a caption line with no table under it.",
    "",
    "A closing sentence."
  )
  res <- run_sweep(page)
  expect_identical(res$status, 1L)
  at <- function(tag) which(grepl(tag, page, fixed = TRUE))[[1]]
  expected <- c(
    paste(at("PLANT-after"), "dash"),
    paste(at("PLANT-bare-cell"), "long sentence"),
    paste(at("PLANT-caption-prose"), "semicolon")
  )
  expect_setequal(finding_keys(res$out), expected)
  expect_length(finding_keys(res$out), length(expected))

  # The skipped table leaves no sentence behind in --prose either.
  res <- run_sweep(page, "--prose")
  expect_false(any(grepl("SILENT", res$out, fixed = TRUE)))
  expect_true(any(grepl("PLANT-bare-cell", res$out, fixed = TRUE)))
})

test_that("a clean page exits 0 and a page with no sentences exits 2", {
  clean <- c("---", "title: x", "---", "", "# Heading", "", "A short sentence. Another one.")
  res <- run_sweep(clean)
  expect_identical(res$status, 0L)
  expect_identical(res$out, character(0))

  empty <- c("---", "title: x", "---", "", "```{r}", "1 + 1", "```")
  res <- run_sweep(empty)
  expect_identical(res$status, 2L)
  expect_match(res$out, "no sentences")
})

test_that("a usage error exits 3", {
  script <- sweep_script()
  run <- function(args) {
    out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c(shQuote(script), args),
                                    stdout = TRUE, stderr = TRUE))
    list(out = as.character(out), status = attr(out, "status"))
  }
  none <- run(character(0))
  expect_identical(none$status, 3L)
  expect_match(none$out, "no files given", all = FALSE)
  flag <- run(c("--nope", "x.Rmd"))
  expect_identical(flag$status, 3L)
  expect_match(flag$out, "unknown flag --nope", all = FALSE)
  both <- run(c("--prose", "--chunks", "x.Rmd"))
  expect_identical(both$status, 3L)
  expect_match(both$out, "at most one mode flag", all = FALSE)
})

test_that("--prose splits sentences, but not at an initial or e.g.", {
  page <- c("J. Smith wrote this, e.g. here. Second sentence!", "", "- item one", "- item two")
  res <- run_sweep(page, "--prose")
  expect_identical(res$status, 0L)
  expect_identical(res$out, c("J. Smith wrote this, e.g. here.", "Second sentence!", "item one", "item two"))
})

test_that("--chunks prints every fenced block without #> lines", {
  page <- c(
    "Some prose.", "",
    "```{r named, echo = FALSE}", "x <- 1", "x", "#> [1] 1", "```", "",
    "~~~", "raw", "~~~"
  )
  res <- run_sweep(page, "--chunks")
  expect_identical(res$out, c("```{r named, echo = FALSE}", "x <- 1", "x", "```", "~~~", "raw", "~~~"))

  # A page with no fenced block prints nothing and exits 0, not an R error.
  res <- run_sweep(c("Only prose here.", "", "More prose."), "--chunks")
  expect_identical(res$status, 0L)
  expect_identical(res$out, character(0))
})

test_that("--inventory lists numbers, degrees, code spans and precision terms", {
  terms <- tempfile(fileext = ".md")
  writeLines(c("# Rules", "", "## Precision list", "", "- interval", "- credible", "", "## Ledger", "- ignored"), terms)
  page <- c(
    "The 95% credible intervals for `ssm_draws()` straddle 0°/360° at 90 degrees.",
    "",
    "```{r}", "c(7, 8)", "```"
  )
  res <- run_sweep(page, c("--inventory", "--terms", shQuote(terms)))
  expect_identical(res$status, 0L)
  expect_identical(res$out, sort(c(
    "code: ssm_draws()", "degree: 0", "degree: 360", "degree: 90",
    "number: 0", "number: 360", "number: 90", "number: 95",
    "term: credible", "term: interval"
  )))

  # A sign flip must change the inventory: a leading minus (hyphen or U+2212)
  # stays on its number, but a hyphen inside a word does not become a sign.
  page <- c("A turn of -30° is not −340° or 340 degrees, and x-3 is a label.")
  res <- run_sweep(page, c("--inventory", "--terms", shQuote(terms)))
  expect_identical(res$status, 0L)
  expect_identical(res$out, sort(c(
    "degree: -30", "degree: -340", "degree: 340",
    "number: -30", "number: -340", "number: 340", "number: 3"
  )))
})

test_that("a file that is not valid UTF-8 is refused, not silently truncated", {
  # Line 3 holds a dash and a semicolon after a Latin-1 byte on line 1. A reader
  # that stops at the bad byte would miss them and exit 0.
  bytes <- c(charToRaw("Caf"), as.raw(0xe9), charToRaw(" line.\n\nA pause "),
             as.raw(c(0xe2, 0x80, 0x94)), charToRaw(" then; more.\n"))
  res <- run_sweep(bytes)
  expect_identical(res$status, 3L)
  expect_match(res$out, "line 1 is not valid UTF-8", all = FALSE)
})

test_that("an em dash is still found outside a UTF-8 locale", {
  res <- run_sweep(c("Intro line.", "", "A pause — then more."), env = "LC_ALL=C")
  expect_identical(res$status, 1L)
  expect_identical(finding_keys(res$out), "3 dash")

  # The inventory folds U+2212 into a hyphen; that must not crash either.
  terms <- tempfile(fileext = ".md")
  writeLines(c("## Precision list", "", "- interval"), terms)
  res <- run_sweep("A turn of −340° here.", c("--inventory", "--terms", shQuote(terms)), env = "LC_ALL=C")
  expect_identical(res$status, 0L)
  expect_identical(res$out, c("degree: -340", "number: -340"))
})
