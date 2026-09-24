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
