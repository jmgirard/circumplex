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
    }
  }
})

test_that("a wrapped item continues under the first character of its text", {
  x <- instrument_object("iis32")
  old <- options(width = 40)
  on.exit(options(old), add = TRUE)
  out <- capture.output(items(x))
  first <- grep("^1\\. ", out)
  expect_length(first, 1)
  # Item 1's continuation lines, if any, sit under the text after "1. ".
  nxt <- out[first + 1L]
  if (!grepl("^\\d+\\. ", nxt)) expect_match(nxt, "^   \\S")
  # Two-digit numbers indent their continuation one column further.
  ten <- grep("^10\\. ", out)
  if (!grepl("^\\d+\\. ", out[ten + 1L])) {
    expect_match(out[ten + 1L], "^    \\S")
  }
  # scales() indents its item lines by four and hangs them under the text.
  s <- capture.output(scales(x, items = TRUE))
  i <- grep("^    \\d+\\. ", s)[1]
  lead <- nchar(sub("^(    \\d+\\. ).*", "\\1", s[i]))
  if (!grepl("^    \\d+\\. |^[A-Z]", s[i + 1L])) {
    expect_identical(regexpr("\\S", s[i + 1L])[[1]], lead + 1L)
  }
})
