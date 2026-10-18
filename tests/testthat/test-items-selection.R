# Column selection by `items`, and by `scales` in self_standardize() (M160) ----
#
# score(), ipsatize() and axes_reliability() refuse an `items` selection, and
# self_standardize() a `scales` selection, in the three forms the `scales`
# guards refuse (test-scales-selection.R): a number that truncates below 1, a
# name that occurs more than once in the data, and one column selected twice.
# Four other probes (1.5 in place of 1, an NA, an absent name, a number past
# the last column) keep the result or error they gave before the guards
# (71d44034); each expected message below was recorded at that commit.

# One case per function: its data, a valid numeric selection, its argument
# name, and a call taking (data, selection). axes_reliability() is probed on
# its list form here, with the selection standing for the first scale's items.
it_axes_map <- function(first) {
  map <- split(seq_len(32), rep(1:8, each = 4))
  map[[1]] <- first
  unname(map)
}

it_cases <- function() {
  list(
    score = list(
      data = raw_iipsc, pos = 1:32, arg = "items",
      call = function(d, s) score(d, s, instrument = iipsc)
    ),
    ipsatize = list(
      data = raw_iipsc, pos = 1:32, arg = "items",
      call = function(d, s) ipsatize(d, s)
    ),
    self_standardize = list(
      data = aw2009, pos = 1:8, arg = "scales",
      call = function(d, s) self_standardize(d, s)
    ),
    axes_reliability = list(
      data = simulated_items, pos = 1:4, arg = "items", lavaan = TRUE,
      call = function(d, s) {
        axes_reliability(d, it_axes_map(s), angles = octants())
      }
    )
  )
}

it_run <- function(case, d, s) {
  suppressWarnings(suppressMessages(case$call(d, s)))
}

it_skip_lavaan <- function(case) {
  if (isTRUE(case$lavaan)) skip_if_not_installed("lavaan")
}

# The comparable part of a result: axes_reliability() stores its call
it_key <- function(x) if (is.data.frame(x)) x else x$results

# The message each probe gave at 71d44034
it_baseline_message <- function(name, probe) {
  if (name != "axes_reliability") return("undefined columns selected")
  switch(probe,
         na = "Item column(s) not found in `data`: .",
         absent = "Item column(s) not found in `data`: ZZ.",
         past = "Item column(s) not found in `data`: .")
}

test_that("probes the items guards do not target keep their result or error", {
  skip_on_cran()
  cases <- it_cases()
  expect_length(cases, 4)
  for (name in names(cases)) {
    case <- cases[[name]]
    it_skip_lavaan(case)
    d <- case$data
    pos <- case$pos
    n <- length(pos)

    # 1.5 truncates to 1, so the result equals the whole-number selection's
    whole <- it_run(case, d, pos)
    nonwhole <- it_run(case, d, c(1.5, pos[-1]))
    expect_equal(it_key(nonwhole), it_key(whole), label = name)

    probes <- list(
      na = c(NA, pos[-1]),
      absent = c("ZZ", names(d)[pos[-1]]),
      past = c(pos[-n], 99)
    )
    for (probe in names(probes)) {
      expect_error(it_run(case, d, probes[[probe]]),
                   it_baseline_message(name, probe), fixed = TRUE,
                   label = paste(name, probe))
    }
  }
})

# The helper, on `items` -------------------------------------------------------

test_that("check_scales_selection names the argument and source it is given", {
  d <- data.frame(PA = 1:3, BC = 4:6, PA = 7:9, check.names = FALSE)
  expect_error(check_scales_selection(d, c(1, 0), "items"),
               "`items` holds the position 0, below 1 once truncated",
               fixed = TRUE)
  expect_error(check_scales_selection(d, c(2, 2), "items"),
               "`items` selects the column \"BC\" more than once.",
               fixed = TRUE)
  expect_error(check_scales_selection(d, "PA", "items", "`cormat`"),
               paste0("`items` names a column that occurs more than once in ",
                      "`cormat`: \"PA\"."),
               fixed = TRUE)
  # The defaults keep the `scales` wording
  expect_error(check_scales_selection(d, "PA"),
               "`scales` names a column that occurs more than once in `data`",
               fixed = TRUE)
  expect_error(check_scales_positions(-1, "items"),
               "`items` holds the position -1,", fixed = TRUE)
})
