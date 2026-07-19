# Guard: Rd files must not carry characters LaTeX cannot typeset.
#
# CRAN builds a PDF version of the manual, and `R CMD check` only attempts it
# when the manual is NOT skipped. The repo's routine check command passes
# `--no-manual`, so this whole failure class is invisible locally and surfaces
# only on win-builder/CRAN (M7: a Greek theta/zeta pair in
# plot.circumplex_cpm.Rd produced "Unicode character not set up for use with
# LaTeX" -> 1 ERROR, 1 WARNING, after two clean local 0/0/0 checks).
#
# Write such characters as Rd math (\eqn{\theta}{theta}) instead, which
# typesets in the PDF and degrades to ASCII in text and HTML.
#
# Two design points, both learned the hard way in M7 review:
#
# 1. WHERE THE Rd COMES FROM. A source checkout has `man/`, but under
#    `R CMD check` the package is installed and `man/` is gone (there is a
#    compiled `help/` instead). Keying this guard to `man/` alone made it
#    skip under `R CMD check` -- that is, in CI, in `devtools::check()`, and
#    on CRAN, leaving it live only under a bare `devtools::test()`. It ran
#    nowhere that gates a release. So: prefer `man/` when present, else read
#    the installed Rd database, which IS available at check time.
#
# 2. DENY BY DEFAULT. The first version enumerated the ranges believed
#    hostile (Greek, math operators, super/subscripts). Review found the
#    enumeration leaked -- arrows, primes, letterlike symbols, vulgar
#    fractions, daggers and Greek Extended all passed, and `->` is entirely
#    plausible in this package's occasions/trajectory prose. We know what
#    win-builder's LaTeX rejected; we do not know its full hostile set, so
#    enumerating it will always lag. Flag every non-ASCII character except a
#    short allowlist of punctuation already shipping in `man/` and already
#    proven to survive win-builder. A false positive costs one line (write it
#    as Rd math, or extend the allowlist deliberately); a false negative
#    costs a rejected CRAN submission.

test_that("Rd files contain no LaTeX-hostile characters", {
  rd_dir <- test_path("..", "..", "man")

  rd_text <- if (dir.exists(rd_dir)) {
    files <- list.files(rd_dir, pattern = "\\.Rd$", full.names = TRUE)
    stats::setNames(
      lapply(files, readLines, warn = FALSE, encoding = "UTF-8"),
      basename(files)
    )
  } else {
    db <- tools::Rd_db("circumplex")
    lapply(db, function(rd) {
      strsplit(paste(as.character(rd), collapse = ""), "\n", fixed = TRUE)[[1]]
    })
  }

  # Fail loudly rather than pass vacuously if no Rd could be found at all.
  expect_gt(length(rd_text), 0)

  # En/em dash and curly quotes: present in man/ today and accepted by
  # win-builder. Everything else outside printable ASCII is flagged.
  safe <- "–—‘’“”"
  hostile <- sprintf("[^ -~%s]", safe)

  offenders <- character()
  for (nm in names(rd_text)) {
    lines <- rd_text[[nm]]
    hits <- grep(hostile, lines, perl = TRUE)
    for (i in hits) {
      chars <- unique(unlist(regmatches(
        lines[i], gregexpr(hostile, lines[i], perl = TRUE)
      )))
      offenders <- c(offenders, sprintf(
        "%s:%d: %s", nm, i, paste(chars, collapse = " ")
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
