# Fixtures for the printed-caution width tests.
#
# One entry per emitter row in the M131 census. Each entry names the emitter
# and holds a quoted expression that, when evaluated, prints the output that
# carries that caution or note. The same list is read by
# test-print-width.R and by tools/m131-caution-word-parity.R, so the width
# test and the word-for-word comparison against the pre-change output can
# never drift onto different fixtures.
#
# Adding an emitter means adding a row here. A row whose expression stops
# firing its caution is caught by caution_fixture_fires(), which every width
# test runs first: a fixture that prints nothing would otherwise pass every
# width assertion silently.

# Capture what an expression prints, at a given console width. The width
# option is restored on the way out, including when the expression fails, so
# one broken fixture cannot leave every later test running at the wrong width.
caution_fixture_output <- function(expr, width, env = parent.frame()) {
  old <- options(width = width)
  on.exit(options(old), add = TRUE)
  utils::capture.output(eval(expr, envir = env))
}

# The marker text that proves a fixture fired its caution, not merely printed.
caution_fixture_fires <- function(lines, marker) {
  any(grepl(marker, paste(lines, collapse = " "), fixed = TRUE))
}

# Lines of printed output wider than `width` display columns.
caution_overlong_lines <- function(lines, width) {
  lines[nchar(lines, type = "width") > width]
}
