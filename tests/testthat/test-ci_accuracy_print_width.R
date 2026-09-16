# M133: print() of an ssm_ci_accuracy() result fits the reader's width. The
# header line used to print as one unwrapped line of about 130 columns.

test_that("print() of each accuracy fixture fits in 77 columns", {
  skip_on_cran()
  fx <- caution_fixtures()
  bases <- environment(fx[[1]]$build)
  objs <- list(
    ci = bases$ci_base(),
    ci_guard = bases$ci_guardrail_base(),
    ci_cpm = suppressWarnings(bases$ci_cpm_base()),
    ci_contrast = bases$ci_contrast_base()
  )
  for (nm in names(objs)) {
    lines <- caution_fixture_output(quote(print(objs[[nm]])), 77)
    expect_true(
      all(nchar(lines, type = "width") <= 77),
      info = paste(nm, lines[nchar(lines, type = "width") > 77], collapse = "\n")
    )
    expect_match(paste(lines, collapse = " "), "SSM CI accuracy, simulated",
                 fixed = TRUE, info = nm)
  }
})
