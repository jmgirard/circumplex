# Guard: Rd files must not carry characters LaTeX cannot typeset.
#
# CRAN builds a PDF version of the manual, and `R CMD check` only attempts it
# when the manual is NOT skipped. The repo's routine check command passes
# `--no-manual`, so this whole failure class is invisible locally and surfaces
# only on win-builder/CRAN (M7: a Greek theta/zeta pair in
# plot.circumplex_cpm.Rd produced "Unicode character not set up for use with
# LaTeX" -> 1 ERROR, 1 WARNING, after two clean local 0/0/0 checks).
#
# Greek letters and mathematical-operator characters are the classes that break:
# write them as Rd math (\eqn{\theta}{theta}) instead, which typesets in the PDF
# and degrades to ASCII in text and HTML. Latin-1 punctuation that the standard
# inputenc setup does handle (en/em dashes, curly quotes) is deliberately NOT
# flagged -- this guard targets what actually fails, not all non-ASCII.

test_that("Rd files contain no LaTeX-hostile characters", {
  rd_dir <- test_path("..", "..", "man")
  skip_if_not(dir.exists(rd_dir), "man/ not available (installed package)")

  rd_files <- list.files(rd_dir, pattern = "\\.Rd$", full.names = TRUE)
  expect_gt(length(rd_files), 0)

  # Greek and Coptic (U+0370-U+03FF), mathematical operators (U+2200-U+22FF),
  # superscripts and subscripts (U+2070-U+209F), and the three legacy Latin-1
  # superscripts (U+00B9/B2/B3), which inputenc also leaves undefined -- the
  # zeta-squared in the M7 case was one of these and would otherwise slip past.
  hostile <- "[Ͱ-Ͽ∀-⋿⁰-₟¹²³]"

  offenders <- character()
  for (f in rd_files) {
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    hits <- grep(hostile, lines)
    for (i in hits) {
      chars <- unique(unlist(regmatches(
        lines[i], gregexpr(hostile, lines[i])
      )))
      offenders <- c(offenders, sprintf(
        "%s:%d: %s", basename(f), i, paste(chars, collapse = " ")
      ))
    }
  }

  expect_identical(
    offenders, character(),
    info = paste0(
      "Write these as Rd math, e.g. \\eqn{\\theta}{theta}. Found:\n",
      paste(offenders, collapse = "\n")
    )
  )
})
