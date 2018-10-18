context("test-tidying_functions")

test_that("ipsatize works", {
  data("aw2009")
  new <- ipsatize(aw2009, PA:NO)
  expect_equal(new$PA_i, c(-1.5225, 0.8975, 0.9800, -0.1550, -0.4450))
  expect_equal(new$BC_i, c(-1.4725, -1.2725, -0.5800, -1.0750, -0.5750))
  expect_equal(new$DE_i, c(-1.4025, -1.2025, -0.7300, -0.9150, -0.4650))
  expect_equal(new$FG_i, c(0.1775, -1.0225, -0.8900, -0.3650, 0.0650))
  expect_equal(new$HI_i, c(0.9775, -0.7925, -0.1600, 0.9450, 0.3550))
  expect_equal(new$JK_i, c(2.0575, 0.5575, -0.2700, 0.7350, 0.5150))
  expect_equal(new$LM_i, c(1.3475, 1.5475, 1.3100, 0.9750, 0.5950))
  expect_equal(new$NO_i, c(-0.1625, 1.2875, 0.3400, -0.1450, -0.0450))
})

test_that("score works", {
  set.seed(12345)
  old <- data.frame(
    matrix(
      sample(0:4, size = 32 * 5, replace = TRUE),
      nrow = 5,
      ncol = 32
    )
  )
  new <- score(old, X1:X32, instrument("iipsc"))
  expect_equal(new$PA, c(3.00, 2.50, 1.75, 2.50, 1.50))
  expect_equal(new$BC, c(1.50, 2.25, 1.00, 2.00, 2.50))
  expect_equal(new$DE, c(2.00, 2.75, 2.00, 1.75, 2.75))
  expect_equal(new$FG, c(2.50, 2.50, 1.25, 0.50, 2.50))
  expect_equal(new$HI, c(2.00, 2.00, 3.25, 3.00, 2.25))
  expect_equal(new$JK, c(2.50, 3.50, 2.00, 2.25, 2.00))
  expect_equal(new$LM, c(3.25, 1.25, 1.75, 1.50, 1.25))
  expect_equal(new$NO, c(2.75, 3.75, 2.75, 2.25, 1.00))
})