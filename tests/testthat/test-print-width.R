# M131 T3: one fixture per emitter row in the census (see
# helper-caution-fixtures.R's caution_fixtures()). Each block asserts that the
# fixture actually FIRES its caution, and that no printed line is wider than
# the console width in force.
#
# Both assertions run in every block, in this order:
#   1. the caution's own marker text is present in the printed output. This
#      proves the fixture reached the emitter. A silent fixture would
#      otherwise pass the width check with nothing to check.
#   2. no printed line is wider than the console width in force, except the
#      lines in the ledger below.
#
# Widths 60 and 120 bracket the shipped default of 80. At 60 every fixed-width
# mechanism in the census breaks the budget. At 120 the fixed columns already
# fit, so only a line that is not wrapped at all can fail there.
#
# THE LEDGER. This milestone wraps prose cautions and notes. It deliberately
# leaves fit lines, test-statistic lines and section headings alone, because
# wrapping them destroys their columns. A few of those are wider than 60
# columns, so they are listed here, classified, rather than silently excused
# by a pattern that could also excuse a caution this milestone failed to wrap.
# The ledger is matched on the whole line, after trailing spaces are dropped.
#
# Two tests at the foot of this file guard the ledger itself, because nothing
# else does. Comparing against the pre-change output would not: a caution
# nobody wrapped is byte-identical to how it printed before, so a line-by-line
# comparison reports no change at all and has nothing to object to. The
# guards are that no ledger entry carries a known caution's marker text, and
# that every entry is one some fixture really produces, so a dead entry
# cannot sit here widening what is excused.
#
# The first guard catches a whole caution parked here, which is the case that
# matters, because an emitter nobody wrapped prints its caution on one line.
# It does not catch a single continuation line of a caution, which carries no
# marker. Nothing here does.
#
# What the guards cannot decide, and what stands in for it (M131 review, O5).
# A caution added later, with no census row and so no registered marker, and
# printed incidentally by one of the fixtures below, would pass both guards.
# No textual rule separates such a line from a legitimate entry: an unwrapped
# caution does not go through cat_prose(), so instrumenting the helper does
# not see it either, and the shape pattern that would catch it was rejected at
# T3 because it excuses as much as it catches. What stands in for it is the
# classification: every entry is NAMED by its kind, a third guard requires
# that kind to be one of the four this milestone leaves alone, and there is no
# kind a caution could honestly be filed under. Parking one here now means
# writing down a false kind, which is a visible act in the diff rather than a
# silent one.

caution_width_ledger <- c(
  `fit line` = paste0(
    "Fit: χ²(10) = 81.169, p = <1e-04; RMSEA = 0.078 ",
    "[0.063, 0.094]; SRMR = 0.042; CFI = 0.984"
  ),
  `test-statistic line` =
    "  Correspondence index = 0.694, p = 0.0167 (exact, 120 relabelings)",
  `test-statistic line` =
    "  Correspondence index = 0.868, p = 0.000397 (exact, 5040 relabelings)",
  heading =
    "Circumplex Axes Reliability (Strack, Jacobs & Grosse Holtforth, 2013)",
  heading = paste0(
    "SSM CI accuracy, simulated at your n and settings (3 replications per ",
    "condition; bootstrap intervals with 40 replicates at level 0.95)"
  ),
  heading =
    "Verdicts (c = 1, as estimated), Bradley (1978) liberal band, 95% Wilson CIs:",
  heading =
    "Coverage by condition (d_cert: d when certified; cert: certification rate):",
  `section heading` =
    "  # Profile [All] (n = 150; 95% bootstrap CIs, 40 replicates):",
  `section heading` =
    "  # Profile [Female] (n = 71; 95% bootstrap CIs, 40 replicates):",
  `section heading` =
    "  # Profile [Male] (n = 79; 95% bootstrap CIs, 40 replicates):",
  `section heading` =
    "  # Contrast [Male - Female] (95% bootstrap CIs, 40 replicates):"
)

# The kinds of line this ledger may excuse. The milestone's Scope names the
# families it leaves unwrapped -- tables, column headers, fit lines and
# section headings -- and these four are the kinds the fixtures below actually
# print over-long. Two of them are not the Scope's own words: a top-level
# heading, and a test-statistic line. Neither is column-structured, so neither
# is a table or a column header; what puts them here is what puts the Scope's
# four here, that their layout is written into the line rather than flowed,
# and re-flowing them would change what they are rather than where they break.
# Nothing else may be excused, and no caution is any of these.
caution_ledger_kinds <- c(
  "fit line", "test-statistic line", "heading", "section heading"
)

# Lines over the width that the ledger does not account for.
caution_unexcused_lines <- function(lines, width) {
  over <- caution_overlong_lines(lines, width)
  over[!sub("[ \t]+$", "", over) %in% caution_width_ledger]
}

# Every over-long line any fixture produced, gathered as the blocks run, so
# the ledger guard below can check its entries against a real domain.
caution_seen_overlong <- new.env(parent = emptyenv())
caution_seen_overlong$lines <- character(0)

fixtures <- caution_fixtures()

for (row_name in names(fixtures)) {
  local({
    nm <- row_name
    fx <- fixtures[[nm]]

    test_that(paste0(nm, ": fires its caution and wraps to the width"), {
      obj <- fx$build()
      for (width in c(60, 120)) {
        lines <- caution_fixture_output(quote(fx$print(obj)), width)
        caution_seen_overlong$lines <- union(
          caution_seen_overlong$lines,
          sub("[ \t]+$", "", caution_overlong_lines(lines, width))
        )
        expect_true(
          caution_fixture_fires(lines, fx$marker),
          info = paste0(nm, ": marker did not fire at width ", width)
        )
        expect_identical(
          caution_unexcused_lines(lines, width), character(0),
          info = paste0(nm, ": lines exceed width ", width)
        )
      }
    })
  })
}

test_that("no ledger entry carries the text of a known caution", {
  # The check the pre-change comparison cannot make. An over-long caution that
  # nobody wrapped prints exactly as it always did, so comparing against the
  # old output finds nothing to report, and parking that line here would
  # excuse it everywhere. A ledger entry must therefore carry no caution's
  # own words.
  squash <- function(x) paste(unlist(strsplit(x, "[ \t\r\n]+")), collapse = " ")
  markers <- vapply(fixtures, function(fx) fx$marker, character(1))
  expect_gt(length(markers), 0)
  for (entry in caution_width_ledger) {
    for (marker in markers) {
      expect_false(
        grepl(squash(marker), squash(entry), fixed = TRUE),
        info = paste0("ledger entry carries a caution: ", entry)
      )
    }
  }
})

test_that("every ledger entry is a line some fixture really prints", {
  # A ledger whose entries no longer occur has stopped being a record of what
  # is excused and become room to grow.
  #
  # The domain is normally what the blocks above gathered as they ran, which
  # under testthat they always have: it filters at file granularity and a
  # sourced file runs its blocks in order. The case that reaches an empty
  # environment is this block re-run by hand in an interactive session, where
  # reading it anyway fails for a reason that has nothing to do with the
  # ledger (M131 review, O6). So the guard rebuilds the domain itself in that
  # case, and says which one it used.
  seen <- caution_seen_overlong$lines
  rebuilt <- length(seen) == 0
  if (rebuilt) {
    for (fx in fixtures) {
      obj <- fx$build()
      for (width in c(60, 120)) {
        lines <- caution_fixture_output(quote(fx$print(obj)), width)
        seen <- union(seen, sub("[ \t]+$", "", caution_overlong_lines(lines, width)))
      }
    }
  }
  expect_gt(length(seen), 0)
  for (entry in caution_width_ledger) {
    expect_true(
      entry %in% seen,
      info = paste0(
        "ledger entry never printed (domain ",
        if (rebuilt) "rebuilt here" else "gathered by the blocks above",
        "): ", entry
      )
    )
  }
})

test_that("every ledger entry is named by a kind this milestone leaves alone", {
  # The classification is what stands in for a guard that cannot be written
  # (see the header). An entry with no kind, or with a kind outside the four
  # the Scope leaves unwrapped, is a caution being smuggled in under a label.
  kinds <- names(caution_width_ledger)
  expect_false(is.null(kinds))
  expect_true(all(nzchar(kinds)))
  expect_true(all(kinds %in% caution_ledger_kinds))
})
