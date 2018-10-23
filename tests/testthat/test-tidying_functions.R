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
  
  ow <- ipsatize(aw2009, PA:NO, overwrite = TRUE)
  expect_equal(ow$PA, c(-1.5225, 0.8975, 0.9800, -0.1550, -0.4450))
  expect_equal(ow$BC, c(-1.4725, -1.2725, -0.5800, -1.0750, -0.5750))
  expect_equal(ow$DE, c(-1.4025, -1.2025, -0.7300, -0.9150, -0.4650))
  expect_equal(ow$FG, c(0.1775, -1.0225, -0.8900, -0.3650, 0.0650))
  expect_equal(ow$HI, c(0.9775, -0.7925, -0.1600, 0.9450, 0.3550))
  expect_equal(ow$JK, c(2.0575, 0.5575, -0.2700, 0.7350, 0.5150))
  expect_equal(ow$LM, c(1.3475, 1.5475, 1.3100, 0.9750, 0.5950))
  expect_equal(ow$NO, c(-0.1625, 1.2875, 0.3400, -0.1450, -0.0450))
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
  expect_error(score(old, X1:X30, instrument("iipsc")))
})

test_that("standardize works", {
  instrument("iipsc")
  set.seed(12345)
  old <- data.frame(
    matrix(
      runif(8 * 5, min = 0, max = 4),
      nrow = 5,
      ncol = 8
    )
  )
  new <- standardize(old, X1:X8, octants(), iipsc, sample = 1)
  expect_equal(round(new$X1_z, 4), c(3.2176, 4.1562, 3.4605, 4.2189, 1.6150))
  expect_equal(round(new$X2_z, 4), c(-0.1841, 0.7361, 1.8035, 3.0700, 4.5891))
  expect_equal(round(new$X3_z, 4), c(-0.8911, -0.3398, 2.3892, -1.0473, 0.7776))
  expect_equal(round(new$X4_z, 4), c(0.8469, 0.5331, 0.5936, -0.3500, 2.9120))
  expect_equal(round(new$X5_z, 4), c(0.4316, -0.1235, 2.6685, 1.5409, 1.2658))
  expect_equal(round(new$X6_z, 4), c(0.2045, 1.6530, 0.9281, -0.5620, 0.6490))
  expect_equal(round(new$X7_z, 4), c(2.0691, -1.7467, -0.8656, 1.5301, 0.0187))
  expect_equal(round(new$X8_z, 4), c(0.5269, 3.0627, 3.2395, 1.8059, -0.6111))
})