# wrap_prose() is the one wrapping rule behind every caution and note the
# package prints, so it is tested directly: its logic is independent of any
# caller, and a defect here would reach every caution at once.

test_that("wrap_prose() keeps every line inside the requested width", {
  text <- paste(
    "Note: analytic (Wald) confidence intervals may materially mis-cover at",
    "this sample size; prefer the bootstrap on the raw-data path when",
    "available."
  )
  for (width in c(40, 60, 80, 120)) {
    lines <- wrap_prose(text, prefix = "  ", width = width)
    expect_true(
      all(nchar(lines, type = "width") <= width),
      info = paste("width", width)
    )
  }
})

test_that("wrap_prose() counts the prefix against the width", {
  text <- paste(rep("word", 40), collapse = " ")
  bare <- wrap_prose(text, width = 50)
  indented <- wrap_prose(text, prefix = strrep(" ", 20), width = 50)
  expect_true(all(nchar(indented, type = "width") <= 50))
  # The indent takes 20 of the 50 columns, so the indented form needs more
  # lines. Were the indent added on top of the width instead, the two would
  # break at the same points.
  expect_gt(length(indented), length(bare))
})

test_that("wrap_prose() counts display columns, not characters", {
  # A double-width character is what makes the two counts differ. The "²",
  # "ζ" and "ℹ" the cautions carry are each one column wide, so on
  # those alone a character count and a column count agree and this property
  # would be untestable.
  wide <- strrep("中", 5) # five characters, ten columns
  text <- paste(rep(wide, 20), collapse = " ")
  lines <- wrap_prose(text, width = 30)
  expect_true(all(nchar(lines, type = "width") <= 30))
  # Three ten-column words plus two spaces is 32 columns, so a column count
  # fits two per line where a character count would fit three.
  expect_identical(nchar(lines[[1]]), 11L)
  expect_identical(nchar(lines[[1]], type = "width"), 21L)
})

test_that("wrap_prose() puts the prefix on the first line and the continuation after", {
  text <- paste(rep("word", 30), collapse = " ")
  lines <- wrap_prose(
    text,
    prefix = paste0("    ", format("Guardrail", width = 15)),
    continuation = strrep(" ", 19),
    width = 60
  )
  expect_gt(length(lines), 1)
  expect_match(lines[[1]], "^    Guardrail      ")
  expect_true(all(startsWith(lines[-1], strrep(" ", 19))))
  expect_false(any(grepl("Guardrail", lines[-1], fixed = TRUE)))
  expect_true(all(nchar(lines, type = "width") <= 60))
})

test_that("wrap_prose() never splits an atomic unit across lines", {
  markers <- c(
    "Heywood communality;",
    "small correlation-function weight;",
    "flat amplitude."
  )
  lines <- wrap_prose(markers, prefix = "  ", width = 45, atomic = TRUE)
  # Each marker label survives whole on some line.
  for (marker in markers) {
    expect_true(
      any(grepl(marker, lines, fixed = TRUE)),
      info = marker
    )
  }
  expect_true(all(nchar(lines, type = "width") <= 45))
})

test_that("wrap_prose() preserves the words and their order", {
  text <- paste(
    "What has been measured about these markers covers analytic intervals",
    "only, and not every marker was measured."
  )
  for (width in c(30, 55, 100)) {
    lines <- wrap_prose(text, prefix = "  ", width = width)
    rejoined <- paste(trimws(lines), collapse = " ")
    expect_identical(rejoined, text, info = paste("width", width))
  }
})

test_that("wrap_prose() handles the degenerate inputs", {
  expect_identical(wrap_prose(character(0)), character(0))
  expect_identical(wrap_prose(""), character(0))
  expect_identical(wrap_prose("   \n  "), character(0))
  expect_identical(wrap_prose("word", width = 80), "word")
  # A word wider than the room left is placed alone rather than split.
  expect_identical(
    wrap_prose("incomprehensibilities", prefix = "  ", width = 5),
    "  incomprehensibilities"
  )
})

test_that("width = NULL reads the reader's console width", {
  text <- paste(rep("word", 40), collapse = " ")
  old <- options(width = 55L)
  on.exit(options(old), add = TRUE)
  expect_identical(wrap_prose(text, width = NULL), wrap_prose(text, width = 55))
  expect_false(identical(
    wrap_prose(text, width = NULL), wrap_prose(text, width = 80)
  ))
})

test_that("wrap_prose() rejects arguments of the wrong type", {
  expect_error(wrap_prose(1:3))
  expect_error(wrap_prose("text", prefix = c("a", "b")))
  expect_error(wrap_prose("text", atomic = "yes"))
})

test_that("cat_prose() prints what wrap_prose() returns, one line each", {
  text <- paste(rep("word", 20), collapse = " ")
  lines <- wrap_prose(text, prefix = "  ", width = 40)
  expect_output(
    cat_prose(text, prefix = "  ", width = 40),
    paste(lines, collapse = "\n"),
    fixed = TRUE
  )
  expect_silent(cat_prose(character(0)))
})

test_that("cat_prose() adds no blank line of its own", {
  # cat() appends its separator after the last element, so an extra newline
  # here would put a blank line after every caution and silently change the
  # layout. The next thing printed must land on the very next line.
  text <- paste(rep("word", 20), collapse = " ")
  printed <- utils::capture.output({
    cat_prose(text, prefix = "  ", width = 40)
    cat("NEXT\n")
  })
  expect_identical(printed[[length(printed)]], "NEXT")
  expect_false(any(printed == ""))
  expect_identical(
    length(printed),
    length(wrap_prose(text, prefix = "  ", width = 40)) + 1L
  )
})

# ---- M131 T7: element boundaries are paragraph boundaries --------------------

test_that("each element of x is its own paragraph in non-atomic mode", {
  # The regression this pins. wrap_prose() replaced strwrap(), which treats
  # each element of its input as a separate paragraph. Collapsing them instead
  # merged three short settings sentences into one flowed paragraph in
  # summary.circumplex_ci_accuracy(), which no census row emits and AC4
  # required to stay byte-identical (M131 review, findings O1 and O2).
  short <- c("First sentence.", "Second sentence.", "Third sentence.")
  expect_identical(wrap_prose(short, width = 80), short)
  # The same three would fit on one line if they were flowed together.
  expect_lt(sum(nchar(short)) + 2L, 80L)
})

test_that("a long element still wraps, and later elements start a new line", {
  first <- paste(rep("alpha", 12), collapse = " ")
  lines <- wrap_prose(c(first, "Tail."), width = 30)
  expect_gt(length(lines), 2L)
  expect_identical(lines[[length(lines)]], "Tail.")
  expect_true(all(nchar(lines, type = "width") <= 30L))
})

test_that("prefix opens every paragraph and continuation carries its rest", {
  long <- paste(rep("beta", 10), collapse = " ")
  lines <- wrap_prose(
    c(long, long), prefix = ">> ", continuation = "   ", width = 20
  )
  opens <- grep("^>> ", lines)
  expect_identical(length(opens), 2L)
  expect_true(all(nchar(lines, type = "width") <= 20L))
})

test_that("empty and whitespace-only elements drop out", {
  # Not strwrap()'s behavior: it emits an empty line for such an element.
  # These cautions never want a blank line they did not ask for.
  expect_identical(
    wrap_prose(c("Kept.", "", "   ", "Also kept."), width = 80),
    c("Kept.", "Also kept.")
  )
  expect_identical(wrap_prose(c("", "  "), width = 80), character(0))
})

test_that("atomic mode keeps element-as-unit semantics, not paragraphs", {
  # In atomic mode an element is a unit that must not split, and several
  # units share a line. That is the marker-note layout and must not become
  # one paragraph per marker.
  lines <- wrap_prose(c("aa", "bb", "cc"), width = 80, atomic = TRUE)
  expect_identical(lines, "aa bb cc")
})

# ---- M131 T10: argument validation ------------------------------------------

test_that("a width the caller states is validated, not silently repaired", {
  expect_error(wrap_prose("word", width = 0))
  expect_error(wrap_prose("word", width = -5))
  expect_error(wrap_prose("word", width = "80"))
  expect_error(wrap_prose("word", width = c(40, 80)))
  expect_error(wrap_prose("word", width = NA_integer_))
  expect_error(wrap_prose("word", width = NA))
  expect_error(cat_prose("word", width = 0))
})

test_that("the console width needs no fallback, because R guarantees it", {
  # Why wrap_prose() validates the width it is given but never repairs the
  # one it reads: R refuses both ways of making getOption("width") unusable,
  # so there is no unusable option to defend against and a fallback here
  # would be a branch no test could reach.
  old <- options(width = 80L)
  on.exit(options(old), add = TRUE)
  expect_error(options(width = NULL))
  expect_error(options(width = -1))
  expect_true(is_scalar_count(getOption("width")))
})

test_that("atomic must be TRUE or FALSE, never NA", {
  expect_error(wrap_prose("word", atomic = NA))
  expect_error(wrap_prose("word", atomic = c(TRUE, FALSE)))
})

# ---- M146: the remaining width and prefix edge cases -------------------------

test_that("an infinite width is refused, because it wraps nothing", {
  # is_scalar_count() accepts Inf (ceiling and floor agree there), so the
  # finiteness check is its own condition and the error names the width.
  expect_error(wrap_prose("word", width = Inf), "width")
  expect_error(cat_prose("word", width = Inf), "width")
})

test_that("a tab in the prefix or continuation is refused", {
  # disp_width() counts a tab as 0 columns, but a console draws it up to 8
  # wide, so a tabbed lead would push lines past the width it was counted in.
  expect_error(wrap_prose("word", prefix = "\t"), "prefix")
  expect_error(wrap_prose("word", continuation = "  \t"), "continuation")
  # A tab in the prose itself is a word separator and stays legal.
  expect_identical(wrap_prose("a\tb", width = 40), "a b")
})

test_that("ssm_ci_cat_line() keeps its label when the text is empty", {
  out <- capture.output(ssm_ci_cat_line("Guardrail", ""))
  expect_identical(out, "    Guardrail")
  out_ws <- capture.output(ssm_ci_cat_line("Guardrail", "   "))
  expect_identical(out_ws, out)
})
