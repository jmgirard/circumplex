# Three printed lines that ran past the console width: the CPM fit line, the
# instruments() list and norm_standardize()'s disclosure message. Each now
# wraps to getOption("width"). The width guard over the vignettes found the
# last two on the using-instruments page (M164).

split_lines <- function(text) {
  strsplit(sub("\n$", "", text), "\n", fixed = TRUE)[[1]]
}

# Base R rather than withr, which is not in Suggests.
with_width <- function(width, expr) {
  old <- options(width = width)
  on.exit(options(old), add = TRUE)
  expr
}

squash_words <- function(x) {
  unlist(strsplit(paste(x, collapse = " "), "[ \t\r\n]+"))
}

# ---- the CPM fit line -------------------------------------------------------

# Each case pairs a fit with the one-line text it printed before the line
# wrapped. The text is written out here, not rebuilt from cpm_fit_line(), so
# the comparison holds one fact that does not come from the code under test.
fit_line_cases <- list(
  saturated = list(
    fit = list(df = 0),
    digits = 3,
    text = "Fit: saturated model (df = 0); fit indices undefined."
  ),
  # The fit line that the width ledger of test-print-width.R recorded at 89
  # columns before M164.
  ledger = list(
    fit = list(
      df = 10, chisq = 81.169, pvalue = 1e-6, rmsea = 0.078,
      rmsea_ci = c(0.063, 0.094), srmr = 0.042, cfi = 0.984
    ),
    digits = 3,
    text = paste0(
      "Fit: χ²(10) = 81.169, p = <1e-04; RMSEA = 0.078 ",
      "[0.063, 0.094]; SRMR = 0.042; CFI = 0.984"
    )
  ),
  wide = list(
    fit = list(
      df = 1234, chisq = 123456.789012, pvalue = 0.0123456789,
      rmsea = 0.123456789, rmsea_ci = c(0.101234321, 0.145678987),
      srmr = 0.0567891234, cfi = 0.912345678
    ),
    digits = 6,
    text = paste0(
      "Fit: χ²(1234) = 123456.789012, p = 0.0123457; ",
      "RMSEA = 0.123457 [0.101234, 0.145679]; SRMR = 0.056789; ",
      "CFI = 0.912346"
    )
  )
)

test_that("the constructed wide fit line is wider than 100 columns", {
  expect_gt(nchar(fit_line_cases$wide$text, type = "width"), 100)
})

test_that("the CPM fit line wraps after a semicolon to the console width", {
  for (nm in names(fit_line_cases)) {
    case <- fit_line_cases[[nm]]
    for (width in c(60, 77, 120)) {
      label <- paste0(nm, " at width ", width)
      lines <- with_width(
        width,
        split_lines(cpm_fit_line(case$fit, case$digits))
      )
      expect_true(
        all(nchar(lines, type = "width") <= width),
        label = paste(label, "fits the width")
      )
      expect_true(startsWith(lines[[1]], "Fit: "), label = label)
      if (length(lines) > 1) {
        expect_true(
          all(endsWith(lines[-length(lines)], ";")),
          label = paste(label, "breaks only after a semicolon")
        )
        expect_true(
          all(grepl("^ {5}[^ ]", lines[-1])),
          label = paste(label, "indents continuation lines by five spaces")
        )
      }
      expect_identical(
        paste(trimws(lines), collapse = " "), case$text,
        label = paste(label, "rejoins to the one-line text")
      )
    }
  }
})

test_that("the ledger fit line and the wide one do break at width 60", {
  # Without this, a line that never wraps would pass the block above at every
  # width where it happens to fit, and the break rules would check nothing.
  for (nm in c("ledger", "wide")) {
    case <- fit_line_cases[[nm]]
    lines <- with_width(
      60,
      split_lines(cpm_fit_line(case$fit, case$digits))
    )
    expect_gt(length(lines), 1, label = nm)
  }
})

# ---- instruments() ----------------------------------------------------------

instruments_lines <- function(width) {
  with_width(width, capture.output(instruments()))
}

test_that("instruments() wraps each entry to the console width", {
  codes <- instrument_names()
  expect_gt(length(codes), 0)
  wrapped_somewhere <- FALSE

  # Width 40 is below the header's 57 columns, so the header must wrap too.
  for (width in c(40, 60, 80, 120)) {
    lines <- instruments_lines(width)
    expect_true(
      all(nchar(lines, type = "width") <= width),
      label = paste("every line fits at width", width)
    )
    expect_false(
      any(grepl("^ +[0-9]+\\. ", lines)),
      label = paste("an entry number opens with a space at width", width)
    )

    starts <- grep("^[0-9]+\\. ", lines)
    expect_length(starts, length(codes))
    ends <- c(starts[-1] - 1L, length(lines))

    for (i in seq_along(codes)) {
      entry <- lines[starts[[i]]:ends[[i]]]
      lead <- regmatches(entry[[1]], regexpr("^[0-9]+\\. ", entry[[1]]))
      if (length(entry) > 1) {
        wrapped_somewhere <- TRUE
        indent <- strrep(" ", nchar(lead))
        expect_true(
          all(startsWith(entry[-1], indent) &
                !startsWith(entry[-1], paste0(indent, " "))),
          label = paste(codes[[i]], "continuation sits under its text")
        )
      }
      d <- instrument_object(codes[[i]])$Details
      expect_identical(
        squash_words(trimws(entry)),
        squash_words(paste0(i, ". ", d$Abbrev, ": ", d$Name, " (", codes[[i]], ")")),
        label = paste(codes[[i]], "at width", width)
      )
    }
  }
  # At least one entry must have wrapped, or the indent check ran on nothing.
  expect_true(wrapped_somewhere)
})

# ---- norm_standardize()'s disclosure message --------------------------------

norm_message_text <- function(obj, smp, width) {
  probe <- as.data.frame(matrix(2, nrow = 2, ncol = nrow(obj$Scales)))
  names(probe) <- obj$Scales$Abbrev
  msg <- with_width(
    width,
    capture_messages(norm_standardize(
      probe, scales = names(probe), angles = obj$Scales$Angle,
      instrument = obj, sample = smp, append = FALSE
    ))
  )
  expect_length(msg, 1)
  msg[[1]]
}

test_that("norm_standardize()'s message wraps to the console width", {
  cases <- 0L
  for (nm in instrument_names()) {
    obj <- instrument_object(nm)
    for (smp in unique(obj$Norms[[2]]$Sample)) {
      if (!norm_sample_usable(obj, smp)) next
      cases <- cases + 1L
      label <- paste0(nm, " sample ", smp)
      one_line <- norm_message_text(obj, smp, 10000)
      expect_length(split_lines(one_line), 1)
      for (width in c(60, 120)) {
        lines <- split_lines(norm_message_text(obj, smp, width))
        expect_true(
          all(nchar(lines, type = "width") <= width),
          label = paste(label, "fits width", width)
        )
        # The word check below collapses whitespace, so it cannot see a blank
        # line added by the wrap.
        expect_true(all(nzchar(trimws(lines))), label = paste(label, "has no blank line"))
        expect_identical(
          squash_words(lines), squash_words(one_line),
          label = paste(label, "keeps its words at width", width)
        )
      }
      # One fact stated apart from the message under test.
      expect_match(
        one_line,
        paste0("Standardized against ", obj$Details$Abbrev,
               " normative sample ", smp, ": N = "),
        fixed = TRUE
      )
    }
  }
  expect_gt(cases, 0L)
})

test_that("a long norms message does break at width 60", {
  # The message for IIP-SC sample 1 printed at 173 columns on the
  # using-instruments page, so it must span several lines at width 60.
  lines <- split_lines(norm_message_text(instrument_object("iipsc"), 1, 60))
  expect_gt(length(lines), 1)
})
