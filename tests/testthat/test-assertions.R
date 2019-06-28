test_that("is_numvec works", {
  vec_numeric <- 1:4
  df_numeric_11 <- data.frame(x = 1)
  df_numeric_12 <- data.frame(x = 1, y = 1)
  df_numeric_21 <- data.frame(x = 1:2)
  df_numeric_22 <- data.frame(x = 1:2, y = 1:2)
  mat_numeric_11 <- matrix(1, 1, 1)
  mat_numeric_12 <- matrix(1:2, 1, 2)
  mat_numeric_21 <- matrix(1:2, 2, 1)
  mat_numeric_22 <- matrix(1:4, 2, 2)
  vec_char <- c("a", "b", "c", "d")
  df_char <- data.frame(x = vec_char)
  mat_char <- matrix(vec_char, 1, 4)
  df_mixed <- data.frame(x = 1:4, y = vec_char)
  
  expect_true(is_numvec(vec_numeric))
  expect_true(is_numvec(df_numeric_11))
  expect_true(is_numvec(df_numeric_12))
  expect_true(is_numvec(df_numeric_21))
  expect_false(is_numvec(df_numeric_22))
  expect_true(is_numvec(mat_numeric_11))
  expect_true(is_numvec(mat_numeric_21))
  expect_true(is_numvec(mat_numeric_12))
  expect_false(is_numvec(mat_numeric_22))
  expect_false(is_numvec(vec_char))
  expect_false(is_numvec(df_char))
  expect_false(is_numvec(mat_char))
  expect_false(is_numvec(df_mixed))
})

test_that("is_provided works", {
  data(jz2017)
  f <- function(df, n = NULL) {
    is_provided(n)
  }
  expect_true(f(jz2017, n = 1))
  expect_false(f(jz2017, n = NULL))
  expect_false(f(jz2017))
  
  f2 <- function(df, n = 1) {
    is_provided(n)
  }
  expect_true(f2(jz2017, n = 1))
  expect_false(f2(jz2017, n = NULL))
  expect_true(f2(jz2017))

  f3 <- function(df, vars) {
    is_provided(rlang::enquo(vars))
  }
  expect_true(f3(jz2017, vars = PA:NO))
  expect_false(f3(jz2017, vars = NULL))
  expect_false(f3(jz2017))
})

