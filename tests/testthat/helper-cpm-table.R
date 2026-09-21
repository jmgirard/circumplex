# M133: the printed CPM results table. print() and summary() show a display
# copy of `results` with shorter headers so the table fits in 77 columns; the
# returned object keeps its names (D-056, D-057).

# The old-to-new header map the printed table uses. Names absent here print
# unchanged (Scale, Angle, Zeta, Communality, VarRatio).
cpm_table_header_map <- c(
  Angle_theory = "Theory",
  Angle_lci = "lci", Angle_uci = "uci",
  Zeta_lci = "lci", Zeta_uci = "uci"
)

# Capture `printer(fit)` at width 77 and check that the results table prints
# as one block: one header row naming the mapped columns, then one row per
# scale, then a blank line. Each printed value equals cpm_round_df(results)
# column by column, in the same order.
expect_cpm_table_one_block <- function(fit, printer, digits = 3) {
  old <- options(width = 77)
  on.exit(options(old), add = TRUE)
  out <- suppressWarnings(capture.output(printer(fit)))
  # The printed table leaves out Communality (M146); the object keeps it.
  expect_true("Communality" %in% names(fit$results))
  res <- fit$results
  res$Communality <- NULL
  expected_header <- names(res)
  hit <- expected_header %in% names(cpm_table_header_map)
  expected_header[hit] <- cpm_table_header_map[expected_header[hit]]

  start <- which(grepl("^\\s*Scale\\s", out))
  expect_length(start, 1)
  expect_identical(strsplit(trimws(out[start]), "\\s+")[[1]], expected_header)
  rows <- out[start + seq_len(nrow(res))]
  after <- out[start + nrow(res) + 1]
  expect_true(is.na(after) || !nzchar(trimws(after)))
  expect_true(all(nchar(out[start:(start + nrow(res))], type = "width") <= 77))

  cells <- do.call(rbind, strsplit(trimws(rows), "\\s+"))
  expect_identical(ncol(cells), ncol(res))
  rounded <- cpm_round_df(res, digits)
  for (j in seq_along(res)) {
    if (is.numeric(res[[j]])) {
      expect_equal(suppressWarnings(as.numeric(cells[, j])), rounded[[j]],
                   tolerance = 0, info = names(res)[j])
    } else {
      expect_identical(cells[, j], as.character(res[[j]]),
                       info = names(res)[j])
    }
  }
}
