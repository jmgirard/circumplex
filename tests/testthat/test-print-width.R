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
# THE LEDGER. M131 wraps prose cautions and notes. It deliberately leaves
# tables, column headers, fit lines and section headings alone, because
# wrapping a table destroys its columns. A few of those are wider than 60
# columns, so they are listed here, classified, rather than silently excused
# by a pattern that could also excuse a caution this milestone failed to wrap.
# The ledger is matched on the whole line, after trailing spaces are dropped.
#
# The ledger cannot hide an unwrapped caution: AC4 checks every line that no
# census row emits byte for byte against master 26bd64ac, so a caution that
# reached the ledger would have to be byte-identical to its pre-change form,
# which is what the ledger entries are and a wrapped caution is not.

caution_width_ledger <- c(
  # Filled in at T4, once the migration is done and the remaining over-long
  # lines are the out-of-scope ones. Each entry carries its class in a
  # comment: heading, fit line, table row, or column header.
)

# Lines over the width that the ledger does not account for.
caution_unexcused_lines <- function(lines, width) {
  over <- caution_overlong_lines(lines, width)
  over[!sub("[ \t]+$", "", over) %in% caution_width_ledger]
}

fixtures <- caution_fixtures()

for (row_name in names(fixtures)) {
  local({
    nm <- row_name
    fx <- fixtures[[nm]]

    test_that(paste0(nm, ": fires its caution and wraps to the width"), {
      obj <- fx$build()
      for (width in c(60, 120)) {
        lines <- caution_fixture_output(quote(fx$print(obj)), width)
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
