test_that("ipsatize works", {
  data("raw_iipsc")
  items <- 1:32
  datin <- ipsatize(raw_iipsc, items = items, append = FALSE)
  datia <- ipsatize(raw_iipsc, items = items, append = TRUE)
  expect_equal(ncol(datin), length(items))
  expect_equal(ncol(datia), ncol(raw_iipsc) + length(items))
  expect_equal(datin[[1]][1], -1.0)
  expect_equal(datin[[2]][7], -0.5)
})

test_that("score works", {
  set.seed(12345)
  old <- data.frame(
    matrix(sample(0:4, size = 32 * 5, replace = TRUE), nrow = 5, ncol = 32)
  )
  new <- score(old, items = 1:32, instrument = instrument("iipsc"))
  new2 <- score(old, items = 1:32, instrument = instrument("iipsc"), 
                append = FALSE)
    
  expect_equal(new$PA, c(0.5, 1.25, 2, 0.75, 3.5))
  expect_equal(new$BC, c(1.75, 0.75, 1.5, 1.75, 1.75))
  expect_equal(new$DE, c(1.75, 2.25, 1.75, 3, 3))
  expect_equal(new$FG, c(2.5, 1.75, 3.5, 1.5, 1.75))
  expect_equal(new$HI, c(2.25, 2.25, 3, 2, 1.25))
  expect_equal(new$JK, c(1.5, 2.5, 1.75, 2.25, 1.75))
  expect_equal(new$LM, c(2, 2.5, 1.5, 2.75, 1.5))
  expect_equal(new$NO, c(1.75, 2, 1.75, 2.25, 2.5))
  expect_error(score(old, 1:30, instrument("iipsc")))
  expect_equal(ncol(new), ncol(new2) + ncol(old))
})

test_that("norm_standardize works", {
  set.seed(12345)
  old <- data.frame(
    matrix(runif(8 * 5, min = 0, max = 4), nrow = 5, ncol = 8)
  )
  new <- norm_standardize(
    old, 
    scales = 1:8, 
    instrument = instrument("iipsc"), 
    sample = 1
  )
  new2 <- norm_standardize(
    old, 
    scales = 1:8,
    instrument = instrument("iipsc"),
    sample = 1,
    append = FALSE
  )
  expect_equal(round(new$X1_z, 4), c(3.2176, 4.1562, 3.4605, 4.2189, 1.6150))
  expect_equal(round(new$X2_z, 4), c(-0.1841, 0.7361, 1.8035, 3.07, 4.5891))
  expect_equal(round(new$X3_z, 4), c(-0.8911, -0.3398, 2.3892, -1.0473, 0.7776))
  expect_equal(round(new$X4_z, 4), c(0.8469, 0.5331, 0.5936, -0.35, 2.912))
  expect_equal(round(new$X5_z, 4), c(0.4316, -0.1235, 2.6685, 1.5409, 1.2658))
  expect_equal(round(new$X6_z, 4), c(0.2045, 1.653, 0.9281, -0.562, 0.649))
  expect_equal(round(new$X7_z, 4), c(2.0691, -1.7467, -0.8656, 1.5301, 0.0187))
  expect_equal(round(new$X8_z, 4), c(0.5269, 3.0627, 3.2395, 1.8059, -0.6111))
  expect_error(norm_standardize(
    old, scales = 1:7, instrument = instrument("iipsc"), sample = 1
  ))
  expect_equal(ncol(new), ncol(new2) + ncol(old))
})
