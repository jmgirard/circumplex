# M146: items() and scales(items = TRUE) wrap item text to the console width,
# and scales(items = TRUE) no longer stops on an instrument that ships a
# notice in place of its items.

test_that("scales(items = TRUE) prints a notice-only instrument's notice once", {
  for (nm in c("iip32", "iip64")) {
    x <- instrument_object(nm)
    notice <- x$Items$Text[[1]]
    expect_true(is.na(x$Items$Number[[1]]))
    out <- capture.output(res <- scales(x, items = TRUE))
    expect_identical(res, x)
    # The header, one line per scale, then the notice once.
    expect_length(out, 1L + nrow(x$Scales) + 1L)
    expect_identical(out[[length(out)]], paste0("    ", notice))
    expect_identical(sum(grepl(notice, out, fixed = TRUE)), 1L)
    expect_identical(
      out[1L + seq_len(nrow(x$Scales))],
      paste0(x$Scales$Abbrev, ": ", x$Scales$Label, " (", x$Scales$Angle,
             " degrees)")
    )
  }
})

# A line may pass the width only when the text after its indent or number is
# one word too wide for the room.
lone_word <- function(line) {
  !grepl(" ", sub("^\\s*(\\d+\\.\\s+|Prefix:\\s+|Suffix:\\s+)?", "", line))
}

test_that("item, Prefix and Suffix lines fit widths 40, 60 and 80", {
  old <- options(width = 80)
  on.exit(options(old), add = TRUE)
  for (nm in instrument_names()) {
    x <- instrument_object(nm)
    header_items <- paste0("The ", x$Details$Abbrev, " contains ")
    scale_lines <- paste0(x$Scales$Abbrev, ": ", x$Scales$Label, " (",
                          x$Scales$Angle, " degrees)")
    for (w in c(40, 60, 80)) {
      options(width = w)
      for (out in list(capture.output(items(x)),
                       capture.output(scales(x, items = TRUE)))) {
        body <- out[!startsWith(out, header_items) & !out %in% scale_lines]
        over <- body[nchar(body, type = "width") > w]
        expect_true(all(vapply(over, lone_word, logical(1))),
                    info = paste(nm, "width", w))
      }
      # The width check above reads only lines that are too wide, so it would
      # pass if item text stopped printing. The items() output must give back
      # every item's words, in order.
      numbered <- !is.na(x$Items$Number)
      if (any(numbered)) {
        out <- capture.output(items(x))
        block <- out[seq(grep("^\\d+\\. ", out)[1], length(out))]
        words <- unlist(strsplit(trimws(sub("^\\d+\\. ", "", block)), "\\s+"))
        expect_identical(
          words,
          unlist(strsplit(trimws(x$Items$Text[numbered]), "\\s+")),
          info = paste(nm, "width", w)
        )
      }
    }
  }
})

# Every line that follows an item's numbered line and is not itself a lead is
# a continuation. Returns, for each continuation, its indent and the indent it
# should have (the width of the lead, "12. " or "    12. ").
continuation_indents <- function(out) {
  lead_re <- "^(\\s*\\d+\\. )"
  got <- want <- integer(0)
  cur <- NA_integer_
  for (line in out) {
    if (grepl(lead_re, line)) {
      cur <- nchar(sub(paste0(lead_re, ".*"), "\\1", line))
    } else if (!is.na(cur) && grepl("^\\s+\\S", line)) {
      got <- c(got, regexpr("\\S", line)[[1]] - 1L)
      want <- c(want, cur)
    } else {
      cur <- NA_integer_
    }
  }
  list(got = got, want = want)
}

test_that("a wrapped item continues under the first character of its text", {
  # At width 40, csip items 2 ("2. Acting rude and inconsiderate toward
  # others") and 11 wrap in items(), and item 1 wraps in scales(). The test
  # asserts that continuation lines exist, so it cannot pass on an empty set.
  x <- instrument_object("csip")
  old <- options(width = 40)
  on.exit(options(old), add = TRUE)
  out <- capture.output(items(x))
  ind <- continuation_indents(out)
  expect_gt(length(ind$got), 0)
  expect_identical(ind$got, ind$want)
  # One-digit and two-digit leads both hang (3 and 4 columns).
  expect_match(out[grep("^2\\. ", out) + 1L], "^   \\S")
  expect_match(out[grep("^11\\. ", out) + 1L], "^    \\S")
  # scales() indents its item lines by four and hangs them under the text.
  s <- capture.output(scales(x, items = TRUE))
  ind <- continuation_indents(s)
  expect_gt(length(ind$got), 0)
  expect_identical(ind$got, ind$want)
  expect_match(s[grep("^    1\\. ", s) + 1L], "^       \\S")
})
