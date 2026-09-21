# M146: the ssm_sem() ladder and verdict text wraps through wrap_prose().
#
# The four wrapping paths of the printed invariance ladder -- the Delta-CFI
# note, the Verdict: line and labeled facts of sem_format_verdict(), the rung
# notes, and the stored-verdict fallback -- use the package's one wrapping
# rule. Before, the Verdict: line was not wrapped at all and a labeled value
# kept at least 20 columns of room, so both ran past narrow widths. (strwrap(),
# used before, already counts display columns through nchar(type = "w"), so
# the double-width text below guards the column count rather than reproducing
# a defect.) Each path is driven directly over widths 30 to 120.

wide <- enc2utf8("漢字")  # two double-width characters, 4 columns

# The Delta-CFI note is fixed text with no double-width character, so its
# test guards the width but cannot tell a column count from a character count.

# Every line fits the width, except a line whose text after its lead is one
# word too wide for the room.
expect_lines_fit <- function(lines, width, info) {
  over <- lines[nchar(lines, type = "width") > width]
  lone_word <- !grepl(" ", trimws(sub("^\\s*\\S+:\\s+", "", over)))
  expect_true(all(lone_word), info = info)
}

test_that("the Delta-CFI note fits every width from 30 to 120", {
  for (w in 30:120) {
    lines <- strsplit(sem_dcfi_note(width = w), "\n", fixed = TRUE)[[1]]
    expect_lines_fit(lines, w, paste("width", w))
  }
})

test_that("sem_format_verdict() wraps the Verdict: line and facts in columns", {
  facts <- list(
    decision = paste(rep(paste0(wide, " decision"), 8), collapse = " "),
    test = paste(rep(paste0(wide, " test"), 10), collapse = " "),
    result = paste(rep(paste0("result ", wide), 10), collapse = " ")
  )
  for (w in 30:120) {
    lines <- sem_format_verdict(facts, width = w)
    expect_lines_fit(lines, w, paste("width", w))
    expect_match(lines[[1]], "^Verdict:")
    # Nothing is lost: the words come back in order.
    words <- unlist(strsplit(trimws(sub("^\\s*(Verdict|Test|Result):", "",
                                        lines)), "\\s+"))
    expect_identical(
      words,
      unlist(strsplit(c(facts$decision, facts$test, facts$result), " "))
    )
  }
})

test_that("rung notes and the stored-verdict fallback fit every width", {
  inv <- list(
    gate = "metric",
    alpha = 0.05,
    table = data.frame(
      rung = c("configural", "metric"),
      chisq = c(10, 12), df = c(4L, 6L), cfi = c(0.99, 0.98),
      rmsea = c(0.03, 0.04), dchisq = c(NA, 2), ddf = c(NA, 2L),
      p = c(NA, 0.37),
      # Dense double-width text: a character count would read each line at
      # half its display width.
      note = c("", paste(rep(wide, 30), collapse = " "))
    ),
    required = NA_character_,
    verdict = paste(rep(wide, 30), collapse = " "),
    comparable = TRUE
  )
  old <- options(width = 80)
  on.exit(options(old), add = TRUE)
  for (w in 30:120) {
    options(width = w)
    out <- capture.output(sem_print_invariance(inv))
    notes <- out[grepl("^\\s*note \\[", out) | seq_along(out) >
                   grep("note \\[", out)[1]]
    expect_lines_fit(notes, w, paste("width", w))
    expect_true(any(startsWith(out, "Verdict:")), info = paste("width", w))
  }
})
