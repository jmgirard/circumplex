# Shared checks for the length contract of summary.circumplex_ci_accuracy().
#
# `old_lines` is the number of lines summary() printed for the same object at
# width 80 before the layout was shortened. Each call site pins its value with
# the procedure that measured it: length(capture.output(summary(res))) under
# options(width = 80), with the package loaded from commit 845fb5e7.
expect_short_ci_summary <- function(res, old_lines) {
  local_reproducible_output(width = 80)
  squash <- function(lines) gsub("\\s+", " ", trimws(paste(lines, collapse = " ")))
  out <- utils::capture.output(ret <- summary(res))
  expect_identical(ret, res)
  # At most half as many lines as the old layout printed
  expect_lte(length(out), floor(old_lines / 2))
  flat <- squash(out)
  # The verdict blocks and the structure note print in full; only the line
  # breaks may differ
  expect_true(grepl(squash(utils::capture.output(ssm_ci_verdict_blocks(res))),
                    flat, fixed = TRUE))
  expect_true(grepl(squash(utils::capture.output(ssm_ci_structure_note(res))),
                    flat, fixed = TRUE))
  # The columns the summary no longer prints stay in the returned object
  expect_true(all(c(
    "Profile", "Parameter", "Condition", "Coverage", "MC_se", "Left_miss",
    "Right_miss", "Median_width", "Coverage_conditional", "N_conditional",
    "Structural", "N_reps"
  ) %in% names(res$coverage)))
  expect_true(all(c(
    "Profile", "Condition", "Cert_rate", "Cert_lci", "Cert_uci", "Benchmark",
    "Caution", "Fit_pass_rate", "Branch_pathology_rate", "N_reps"
  ) %in% names(res$guardrail)))
  invisible(flat)
}
