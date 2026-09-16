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

test_that("wrap_prose() falls back to 80 columns when the width option is unusable", {
  text <- paste(rep("word", 40), collapse = " ")
  expect_identical(
    wrap_prose(text, width = NULL),
    wrap_prose(text, width = 80)
  )
  expect_identical(
    wrap_prose(text, width = NA_integer_),
    wrap_prose(text, width = 80)
  )
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
