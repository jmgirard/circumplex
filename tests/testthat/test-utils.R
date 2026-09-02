test_that("Percentages are converted to strings properly", {
  expect_equal(str_percent(0.95, 2), "95%")
  expect_equal(str_percent(0.999, 2), "99.9%")
  expect_equal(str_percent(0.9999, 2), "99.99%")
  expect_equal(str_percent(0.99999, 2), "99.99%")
})

test_that("pretty max outputs the correct values", {
  expect_warning(pretty_max(NA))
  expect_equal(pretty_max(0), 0.05)
  expect_equal(pretty_max(1), 2)
  expect_equal(pretty_max(2.4), 4)
  expect_equal(pretty_max(5), 7.5)
})

test_that("angle convenience functions work", {
  expect_equal(octants(), as_degree(c(90, 135, 180, 225, 270, 315, 360, 45)))
  expect_equal(poles(), as_degree(c(90, 180, 270, 360)))
  expect_equal(quadrants(), as_degree(c(135, 225, 315, 45)))
})

test_that("angle_dist reports an exact half-turn as +pi, not -pi (F3)", {
  skip_on_cran()
  # Contract: shortest signed rotation on the principal branch (-pi, pi].
  # Pre-fix the wrap had range [-pi, pi), so the exact half-turn atom was
  # reported as -pi. The half-turn must be +pi.
  expect_identical(angle_dist(pi, 0), pi)
  expect_identical(angle_dist(0, pi), pi)
  expect_identical(angle_dist(-pi / 2, pi / 2), pi)
  expect_identical(angle_dist(3 * pi / 2, pi / 2), pi)
  # How the atom arises: displacements are atan2 outputs, and exactly
  # sign-flipped profiles give atan2(-y, -x) vs atan2(y, x). These raw
  # differences are float-exact +/-pi, so the wrap lands bit-exactly on the
  # atom and they must all report +pi. (In the full pipeline, upstream wrapping
  # to [0, 2pi) can leave some true half-turns 1-2 ulp off the atom; those are
  # not remapped and correctly report just inside the branch -- see angle_dist.)
  set.seed(42)
  for (i in 1:8) {
    y <- rnorm(1)
    x <- rnorm(1)
    expect_identical(angle_dist(atan2(-y, -x), atan2(y, x)), pi)
  }
  # Near-boundary values strictly inside (-pi, pi] are legitimate contrasts
  # and must NOT be flipped by any tolerance band: a hair short of the
  # half-turn stays on its own side.
  eps <- 1e-12
  d_neg <- angle_dist(-pi / 2 + eps, pi / 2) # true distance -pi + eps
  expect_true(d_neg > -pi && d_neg < -pi + 1e-9)
  d_pos <- angle_dist(-pi / 2 - eps, pi / 2) # true distance +pi - eps
  expect_true(d_pos > pi - 1e-9 && d_pos <= pi)
  # Non-boundary values are byte-identical to the plain wrap (no collateral
  # change away from the atom)
  x <- c(0.3, 5.9, -2.0, 10, -7.25)
  y <- c(1.2, 0.1, 2.5, -3, 0.4)
  expect_identical(angle_dist(x, y), ((x - y + pi) %% (2 * pi)) - pi)
  # Standard shortest-rotation checks (statistical-validation #3)
  expect_equal(as.numeric(angle_dist(10 * pi / 180, 350 * pi / 180)),
               20 * pi / 180)
  expect_equal(as.numeric(angle_dist(350 * pi / 180, 10 * pi / 180)),
               -20 * pi / 180)
  expect_equal(as.numeric(angle_dist(179 * pi / 180, -179 * pi / 180)),
               -2 * pi / 180)
  # NA propagates without erroring
  expect_identical(angle_dist(NA_real_, 0), NA_real_)
  # Vectorized input with one atom entry: only the atom is remapped
  out <- angle_dist(c(pi, 0.5), c(0, 0.2))
  expect_identical(out, c(pi, ((0.5 - 0.2 + pi) %% (2 * pi)) - pi))
})

test_that("param_diff reports an exact half-turn displacement contrast as +pi", {
  # Parameter vectors in ssm_param_names() order: e, x, y, a, d, fit.
  # Displacements exactly pi apart -> contrast displacement must be +pi.
  p1 <- c(0.5, 0.0, 1.0, 1.0, pi, 0.9)
  p2 <- c(0.2, 0.0, -1.0, 1.0, 0.0, 0.8)
  expect_identical(param_diff(p1, p2)[[5]], pi)
  # Matrix method (Monte Carlo replicate path) shares the same convention
  m1 <- rbind(p1, p1)
  m2 <- rbind(p2, p2)
  expect_identical(unname(param_diff(m1, m2)[, 5]), c(pi, pi))
})

test_that("scales shortcut functions work", {
  expect_equal(PANO(), c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"))
})

test_that("assertions work", {
  expect_true(is_num(1))
  expect_false(is_num("A"))
  expect_false(is_count(-1))
})

test_that("is_scalar_count validates a single non-negative whole number", {
  # Accepts a valid scalar count
  expect_true(is_scalar_count(1))
  expect_true(is_scalar_count(3L))
  expect_true(is_scalar_count(1000))
  # min floor: default 1L rejects 0; min = 0L accepts it
  expect_false(is_scalar_count(0))
  expect_true(is_scalar_count(0, min = 0L))
  expect_true(is_scalar_count(5, min = 0L))
  # Rejects length != 1 (the guard is_count() lacks)
  expect_false(is_scalar_count(c(1, 2)))
  expect_false(is_scalar_count(integer(0)))
  # Rejects NA, returning FALSE rather than NA (usable in && / stopifnot)
  expect_false(is_scalar_count(NA))
  expect_false(is_scalar_count(NA_real_))
  expect_identical(is_scalar_count(NA_integer_), FALSE)
  # Rejects non-integer and negative
  expect_false(is_scalar_count(1.5))
  expect_false(is_scalar_count(-1))
  # Rejects non-numeric
  expect_false(is_scalar_count("1"))
  expect_false(is_scalar_count(TRUE))
})

test_that("is_null_or_char enforces the n argument", {
  # NULL is always accepted, with or without n
  expect_true(is_null_or_char(NULL))
  expect_true(is_null_or_char(NULL, n = 2))
  # Without n, any character vector is accepted
  expect_true(is_null_or_char(c("a", "b")))
  # With n, length must match (regression: n was silently dropped)
  expect_true(is_null_or_char(c("a", "b"), n = 2))
  expect_false(is_null_or_char(c("a", "b"), n = 1))
  expect_false(is_null_or_char("a", n = 2))
  # Non-character input is still rejected
  expect_false(is_null_or_char(1, n = 1))
})

test_that("rescale works", {
  expect_equal(rescale(1:3), c(0.0, 0.5, 1.0))
})

test_that("prettifying works", {
  expect_equal(pretty_min(0.5), 0.2)
  expect_equal(pretty_min(-0.5), -1.0)
  expect_equal(pretty_min(-10), -15)
})
