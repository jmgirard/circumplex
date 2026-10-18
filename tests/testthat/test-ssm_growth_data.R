# ssm_growth_data(): the stacked long table for the growth recipe (M151) -----
#
# Every expected value is derived in-test from ssm_parameters_id(), the
# function the long table wraps, or stated by construction; none is copied
# from the function under test.

test_that("ssm_growth_data returns the four-column long table in row-then-dv order", {
  data("simulated_growth")
  long <- ssm_growth_data(
    simulated_growth, scales = PANO(), id = "person", time = "wave"
  )

  expect_s3_class(long, "data.frame")
  expect_identical(names(long), c("person", "wave", "dv", "value"))
  expect_equal(nrow(long), 3 * nrow(simulated_growth))

  expect_true(is.factor(long$person))
  expect_identical(levels(long$person),
                   levels(factor(simulated_growth$person)))
  expect_true(is.numeric(long$wave))
  expect_true(is.factor(long$dv))
  expect_identical(levels(long$dv), c("e", "x", "y"))
  expect_true(is.numeric(long$value))

  # Rows are ordered by input row, then dv: row 1 e, row 1 x, row 1 y, row 2 e,
  # ... so dv cycles with period three and the id/time columns repeat each
  # input row three times consecutively.
  n <- nrow(simulated_growth)
  expect_identical(as.character(long$dv), rep(c("e", "x", "y"), times = n))
  expect_identical(as.character(long$person),
                   rep(as.character(simulated_growth$person), each = 3))
  expect_equal(long$wave, rep(simulated_growth$wave, each = 3))

  # Each value is the Elev, Xval or Yval that ssm_parameters_id() gives that
  # input row (no id: one profile per row).
  ref <- ssm_parameters_id(simulated_growth, scales = PANO(), angles = octants())
  expect_equal(long$value[long$dv == "e"], ref$Elev)
  expect_equal(long$value[long$dv == "x"], ref$Xval)
  expect_equal(long$value[long$dv == "y"], ref$Yval)
  # And by input row: row i's three values are that row's (Elev, Xval, Yval).
  expect_equal(long$value, as.vector(t(as.matrix(ref[c("Elev", "Xval", "Yval")]))))
})

test_that("ssm_growth_data forwards angles to the scoring transform", {
  # Rotating every scale by 90 degrees rotates (x, y) by 90 degrees:
  # (x, y) -> (-y, x). e is unchanged. Derived from the same transform on
  # the rotated angles, so the angles argument is shown to reach it.
  data("simulated_growth")
  d <- simulated_growth[1:6, ]
  base <- ssm_growth_data(d, scales = PANO(), id = "person", time = "wave")
  rot <- ssm_growth_data(d, scales = PANO(), angles = octants() + 90,
                         id = "person", time = "wave")
  expect_equal(rot$value[rot$dv == "e"], base$value[base$dv == "e"])
  expect_equal(rot$value[rot$dv == "x"], -base$value[base$dv == "y"])
  expect_equal(rot$value[rot$dv == "y"], base$value[base$dv == "x"])
})

test_that("ssm_growth_data takes id and time as column names only", {
  data("simulated_growth")
  d <- simulated_growth[1:3, ]
  # A column number is refused for both, with the message naming the argument.
  expect_error(ssm_growth_data(d, PANO(), id = 1, time = "wave"), "`id`")
  expect_error(ssm_growth_data(d, PANO(), id = "person", time = 2), "`time`")
  expect_error(ssm_growth_data(d, PANO(), id = c("person", "wave"),
                               time = "wave"), "`id`")
})

test_that("ssm_growth_data refuses reserved and duplicate id or time names", {
  # `dv` and `value` are output columns; a time or id under either name, or
  # the same name for both, would give a table with a duplicated column that
  # the formula reads without error.
  data("simulated_growth")
  d <- simulated_growth[1:3, ]
  d$dv <- d$wave
  d$value <- d$wave
  expect_error(ssm_growth_data(d, PANO(), id = "person", time = "dv"),
               "`time`.*reserves")
  expect_error(ssm_growth_data(d, PANO(), id = "value", time = "wave"),
               "`id`.*reserves")
  expect_error(ssm_growth_data(d, PANO(), id = "wave", time = "wave"),
               "`id` and `time`")
  expect_error(ssm_growth_data(d, PANO(), id = "person", time = ""), "`time`")
})

test_that("ssm_growth_data refuses a non-tabular data, a bad scales and a bad angles", {
  data("simulated_growth")
  d <- simulated_growth[1:3, ]
  expect_error(ssm_growth_data(list(d), PANO(), id = "person", time = "wave"),
               "data")
  expect_error(ssm_growth_data(d, scales = TRUE, id = "person", time = "wave"),
               "scales")
  expect_error(ssm_growth_data(d, PANO(), angles = as.character(octants()),
                               id = "person", time = "wave"),
               "angles")
})

test_that("ssm_growth_data passes a scorer warning other than undefined displacement through", {
  # Only the undefined-displacement warning is muffled. A different warning
  # raised inside the scorer must surface, shown here by raising one from
  # the scales argument's evaluation.
  data("simulated_growth")
  d <- simulated_growth[1:3, ]
  expect_warning(
    ssm_growth_data(d, scales = { warning("other"); PANO() },
                    id = "person", time = "wave"),
    "other"
  )
})

test_that("ssm_growth_data refuses a scales and angles length mismatch", {
  data("simulated_growth")
  expect_error(ssm_growth_data(simulated_growth[1:3, ], PANO(),
                               angles = octants()[1:7],
                               id = "person", time = "wave"),
               "`scales` and `angles`")
})

test_that("ssm_growth_data refuses an id or time name absent from data", {
  data("simulated_growth")
  d <- simulated_growth[1:3, ]
  expect_error(ssm_growth_data(d, PANO(), id = "subject", time = "wave"),
               "`id`.*subject")
  expect_error(ssm_growth_data(d, PANO(), id = "person", time = "t"),
               "`time`.*\"t\"")
})

test_that("ssm_growth_data refuses a Date or character time column", {
  data("simulated_growth")
  d <- simulated_growth[1:3, ]
  d_date <- d
  d_date$wave <- as.Date("2026-01-01") + d$wave
  expect_error(ssm_growth_data(d_date, PANO(), id = "person", time = "wave"),
               "`time`.*numeric")
  d_chr <- d
  d_chr$wave <- as.character(d$wave)
  expect_error(ssm_growth_data(d_chr, PANO(), id = "person", time = "wave"),
               "`time`.*numeric")
})

test_that("ssm_growth_data refuses an NA in id or time", {
  data("simulated_growth")
  d <- simulated_growth[1:3, ]
  d_id <- d
  d_id$person[2] <- NA
  expect_error(ssm_growth_data(d_id, PANO(), id = "person", time = "wave"),
               "`id`.*missing")
  d_time <- d
  d_time$wave[3] <- NA
  expect_error(ssm_growth_data(d_time, PANO(), id = "person", time = "wave"),
               "`time`.*missing")
})

test_that("ssm_growth_data keeps a flat profile's zero coordinates and an all-missing profile's NA", {
  # Edge rows: a flat profile scores (e, 0, 0) with defined values, and a
  # profile with a scale entirely missing scores NA on every coordinate.
  # ssm_parameters_id() warns about the undefined displacement of both; the
  # long table carries no displacement, so the warning does not surface here.
  dat <- as.data.frame(rbind(rep(1.5, 8), c(NA, 1, 2, 3, 4, 5, 6, 7)))
  colnames(dat) <- PANO()
  dat$person <- c("A", "B")
  dat$wave <- c(0, 0)
  expect_silent(
    long <- ssm_growth_data(dat, PANO(), id = "person", time = "wave")
  )
  expect_equal(long$value[long$person == "A"], c(1.5, 0, 0))
  expect_true(all(is.na(long$value[long$person == "B"])))
})

test_that("ssm_growth_data returns an empty long table for zero rows", {
  data("simulated_growth")
  long <- ssm_growth_data(simulated_growth[0, ], PANO(),
                          id = "person", time = "wave")
  expect_identical(names(long), c("person", "wave", "dv", "value"))
  expect_equal(nrow(long), 0)
  expect_identical(levels(long$dv), c("e", "x", "y"))
  expect_type(long$value, "double")
  expect_type(long$wave, "double")
})

# Input guards (M157) ---------------------------------------------------------
# Each refusal's message names its argument and, where there is one, the
# offending column or value; the tests assert that text, so a refusal from
# elsewhere cannot pass for it.

test_that("ssm_growth_data refuses a referenced column name that occurs twice", {
  data("simulated_growth")
  d <- simulated_growth[1:3, ]
  dup <- function(d, nm) {
    out <- cbind(d, d[nm])
    names(out) <- c(names(d), nm)
    out
  }
  expect_error(
    ssm_growth_data(dup(d, "person"), PANO(), id = "person", time = "wave"),
    "`id`.*more than once.*\"person\""
  )
  expect_error(
    ssm_growth_data(dup(d, "wave"), PANO(), id = "person", time = "wave"),
    "`time`.*more than once.*\"wave\""
  )
  expect_error(
    ssm_growth_data(dup(d, "PA"), PANO(), id = "person", time = "wave"),
    "`scales`.*more than once.*\"PA\""
  )
  # A matrix keeps duplicated column names through conversion and is refused
  # the same way.
  m <- as.matrix(dup(d, "PA"))
  expect_identical(sum(colnames(m) == "PA"), 2L)
  expect_error(
    ssm_growth_data(m, PANO(), id = "person", time = "wave"),
    "`scales`.*more than once.*\"PA\""
  )
})

test_that("ssm_growth_data judges a matrix's names after conversion", {
  # as.data.frame() names a blank matrix column "V<n>", which here duplicates
  # column 11's name; `id = "V1"` would resolve to the blank column 1.
  data("simulated_growth")
  m <- cbind(as.matrix(simulated_growth[1:3, ]), V1 = 7)
  colnames(m)[1] <- ""
  expect_identical(sum(names(as.data.frame(m)) == "V1"), 2L)
  expect_error(ssm_growth_data(m, PANO(), id = "V1", time = "wave"),
               "`id`.*more than once.*\"V1\"")
  # With column 4 named "V1" instead, scales = 3:10 reaches column 4; the
  # refusal is the duplicated id, not a false "selects the `id` column".
  m4 <- as.matrix(simulated_growth[1:3, ])
  colnames(m4)[c(1, 4)] <- c("", "V1")
  expect_error(ssm_growth_data(m4, 3:10, id = "V1", time = "wave"),
               "`id`.*more than once.*\"V1\"")
  # An NA scale name with two NA column names reaches the scorer's own
  # refusal, not the duplicate-name message.
  m_na <- as.matrix(simulated_growth[1:3, ])
  colnames(m_na)[4:5] <- NA
  expect_error(
    ssm_growth_data(m_na, c("PA", NA, NA, PANO()[4:8]),
                    id = "person", time = "wave"),
    "undefined columns"
  )
})

test_that("ssm_growth_data accepts duplicated column names no argument references by name", {
  data("simulated_growth")
  d <- simulated_growth[1:3, ]
  ref <- ssm_growth_data(d, PANO(), id = "person", time = "wave")
  # An unreferenced duplicate leaves the table as it was.
  d_extra <- cbind(d, extra = 1, extra = 2)
  expect_identical(sum(names(d_extra) == "extra"), 2L)
  expect_identical(
    ssm_growth_data(d_extra, PANO(), id = "person", time = "wave"),
    ref
  )
  # Numeric scales select by position, so a duplicated scale name is no
  # ambiguity: columns 3:10 are the first PA..NO, the trailing PA is unused.
  d_pa <- cbind(d, d["PA"])
  names(d_pa) <- c(names(d), "PA")
  expect_identical(
    ssm_growth_data(d_pa, 3:10, id = "person", time = "wave"),
    ref
  )
})

test_that("ssm_growth_data refuses scales that select the id or time column", {
  data("simulated_growth")
  d <- simulated_growth[1:3, ]
  # By name.
  expect_error(
    ssm_growth_data(d, c("person", PANO()[-1]), id = "person", time = "wave"),
    "`scales`.*`id`.*\"person\""
  )
  expect_error(
    ssm_growth_data(d, c("wave", PANO()[-1]), id = "person", time = "wave"),
    "`scales`.*`time`.*\"wave\""
  )
  # By integer number: person is column 1, wave column 2.
  expect_identical(names(d)[1:2], c("person", "wave"))
  expect_error(
    ssm_growth_data(d, c(1L, 4:10), id = "person", time = "wave"),
    "`scales`.*`id`.*\"person\""
  )
  expect_error(
    ssm_growth_data(d, c(2L, 4:10), id = "person", time = "wave"),
    "`scales`.*`time`.*\"wave\""
  )
  # By a non-integer double, which R truncates to the column before it.
  expect_error(
    ssm_growth_data(d, c(1.9, 4:10), id = "person", time = "wave"),
    "`scales`.*`id`.*\"person\""
  )
  expect_error(
    ssm_growth_data(d, c(2.5, 4:10), id = "person", time = "wave"),
    "`scales`.*`time`.*\"wave\""
  )
})

test_that("ssm_growth_data takes scales by number to the same table as by name", {
  data("simulated_growth")
  expect_identical(names(simulated_growth)[3:10], PANO())
  expect_identical(
    ssm_growth_data(simulated_growth, 3:10, id = "person", time = "wave"),
    ssm_growth_data(simulated_growth, PANO(), id = "person", time = "wave")
  )
})

test_that("ssm_growth_data refuses an infinite time value", {
  data("simulated_growth")
  d <- simulated_growth[1:3, ]
  d$wave <- as.numeric(d$wave)
  d_pos <- d
  d_pos$wave[2] <- Inf
  expect_error(ssm_growth_data(d_pos, PANO(), id = "person", time = "wave"),
               "`time`.*infinite")
  d_neg <- d
  d_neg$wave[3] <- -Inf
  expect_error(ssm_growth_data(d_neg, PANO(), id = "person", time = "wave"),
               "`time`.*infinite")
})

test_that("ssm_growth_data refuses two rows for one person at one time", {
  data("simulated_growth")
  base <- simulated_growth[c(1, 2, 3), ]
  base$wave <- c(0, 1, 1)
  pattern <- "more than one row.*`id` \"B\".*`time` 1;"
  ids <- list(
    character = c("A", "B", "B"),
    factor = factor(c("A", "B", "B")),
    integer = c(1L, 2L, 2L),
    double = c(1.5, 2.5, 2.5)
  )
  labels <- c(character = "B", factor = "B", integer = "2", double = "2.5")
  for (kind in names(ids)) {
    d <- base
    d$person <- ids[[kind]]
    expect_error(
      ssm_growth_data(d, PANO(), id = "person", time = "wave"),
      sub("B", labels[[kind]], pattern, fixed = TRUE),
      label = kind
    )
  }
  # Double ids that differ in the last bits but share one as.character() form
  # become one factor level in the output, so they are one person there and
  # are refused.
  d <- base
  d$person <- c(1, 0.1 + 0.2, 0.3)
  expect_false(identical(d$person[2], d$person[3]))
  expect_error(ssm_growth_data(d, PANO(), id = "person", time = "wave"),
               "more than one row.*`id` \"0.3\".*`time` 1;")
})

test_that("ssm_growth_data accepts one id at two times and two ids at one time", {
  data("simulated_growth")
  d <- simulated_growth[1:4, ]
  d$person <- c("A", "A", "B", "C")
  d$wave <- c(0, 1, 1, 1)
  expect_silent(long <- ssm_growth_data(d, PANO(), id = "person", time = "wave"))
  expect_equal(nrow(long), 12)
})

test_that("ssm_growth_data passes both bundled growth datasets with the scorer's values", {
  for (nm in c("simulated_growth", "simulated_growth_origin")) {
    d <- get(nm, envir = asNamespace("circumplex"))
    long <- ssm_growth_data(d, PANO(), id = "person", time = "wave")
    ref <- ssm_parameters_id(d, scales = PANO(), angles = octants())
    expect_equal(long$value,
                 as.vector(t(as.matrix(ref[c("Elev", "Xval", "Yval")]))),
                 label = nm)
  }
})
