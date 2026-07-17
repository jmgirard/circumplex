test_that("angle_unwrap unwraps a pole-crossing sequence (spec fixture)", {
  # Closed-form fixture pinned by the longitudinal spec (sec. 4.2):
  # 350 -> 10 -> 30 crosses the 0/360 pole ascending
  expect_identical(angle_unwrap(c(350, 10, 30)), c(350, 370, 390))
})

test_that("angle_unwrap descends across the pole", {
  expect_identical(angle_unwrap(c(10, 350, 330)), c(10, -10, -30))
})

test_that("angle_unwrap leaves a monotone within-branch sequence alone", {
  expect_identical(angle_unwrap(c(30, 60, 90)), c(30, 60, 90))
})

test_that("exact 180-degree steps ascend (+180 convention)", {
  # angle_dist() reports an exact half-turn as +180, never -180, so unwrap
  # ascends through it (documented convention; spec sec. 4.2)
  expect_identical(angle_unwrap(c(0, 180)), c(0, 180))
  expect_identical(angle_unwrap(c(90, 270)), c(90, 270))
  expect_identical(angle_unwrap(c(270, 90)), c(270, 450))
  expect_identical(angle_unwrap(c(180, 0)), c(180, 360))
})

test_that("arbitrary reals are wrapped to [0, 360) before unwrapping", {
  # -10 wraps to 350; 370 wraps to 10 -- same as the pole-crossing fixture
  expect_identical(angle_unwrap(c(-10, 370, 390)), c(350, 370, 390))
  # a multiple-turn input wraps first, then unwraps from the wrapped anchor
  expect_identical(angle_unwrap(c(720, 725)), c(0, 5))
  expect_identical(angle_unwrap(c(-370, -365)), c(350, 355))
})

test_that("NA propagates from the missing wave onward", {
  # Every value after a missing wave is branch-ambiguous (spec sec. 4.2)
  expect_identical(angle_unwrap(c(350, NA, 30)), c(350, NA_real_, NA_real_))
  expect_identical(
    angle_unwrap(c(NA, 10, 30)),
    c(NA_real_, NA_real_, NA_real_)
  )
  expect_identical(
    angle_unwrap(c(10, 20, NA)),
    c(10, 20, NA_real_)
  )
})

test_that("angle_unwrap handles trivial lengths", {
  expect_identical(angle_unwrap(numeric(0)), numeric(0))
  expect_identical(angle_unwrap(90), 90)
  expect_identical(angle_unwrap(-90), 270)
  expect_identical(angle_unwrap(NA_real_), NA_real_)
})

test_that("the anchor stays in [0, 360): a 360 input anchors at 0", {
  # The unwrapped scale is anchored at the first wave's wrapped value; the
  # LM = 360 reporting convention applies to displacements, not to the
  # unwrapped branch, so 360 wraps to 0 here
  expect_identical(angle_unwrap(c(360, 10)), c(0, 10))
})

test_that("angle_unwrap rejects non-numeric input", {
  expect_error(angle_unwrap("north"))
  expect_error(angle_unwrap(list(10, 20)))
  expect_error(angle_unwrap(TRUE))
})

test_that("unwrap is exact on integer-degree inputs (no float drift)", {
  # Steps are computed in degrees, so integer-degree fixtures are bit-exact
  x <- seq(0, 3600, by = 45) %% 360
  expect_identical(angle_unwrap(x), seq(0, 3600, by = 45))
})
