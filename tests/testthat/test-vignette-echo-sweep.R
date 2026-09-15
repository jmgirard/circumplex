# tools/vignette-echo-sweep.R is a source-tree tool: tools/ is .Rbuildignore'd,
# so the script and its fixtures exist only where devtools::test() reads the
# source tree.

test_that("the echo sweep prints exactly the expected lines on its fixtures", {
  root <- testthat::test_path("..", "..")
  script <- file.path(root, "tools", "vignette-echo-sweep.R")
  if (!file.exists(script)) testthat::skip("tools/vignette-echo-sweep.R is not in this build")
  owd <- setwd(root)
  on.exit(setwd(owd), add = TRUE)
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("tools/vignette-echo-sweep.R", "--self-test"),
    stdout = TRUE, stderr = TRUE
  ))
  expect_null(attr(out, "status"))
  expect_match(out, "^self-test passed: 44 expected lines from 2 sources$", all = FALSE)
})
