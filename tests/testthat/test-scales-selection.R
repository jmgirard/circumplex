# Column selection by `scales` (M159) -----------------------------------------
#
# Every export that selects columns of `data` with `scales` refuses three
# selections: one column selected twice, a number that truncates below 1, and
# a name that occurs more than once in the data. Four other probes (a
# non-whole number of 1 or more, an NA, an absent name, a number past the
# last column) keep the result or error they gave before the guards
# (447a74dd); each expected message below was recorded at that commit.

# One case per data-selecting export: its data, the positions of PA..NO in
# that data, and a call taking (data, scales). `key` extracts the part of the
# result that a probe compares, leaving out the stored call.
sel_frame <- function() jz2017[1:300, c("Gender", PANO())]

sel_long <- function() {
  set.seed(159)
  out <- data.frame(id = rep(1:60, each = 2),
                    occasion = rep(c("T1", "T2"), 60))
  cbind(out, as.data.frame(matrix(rnorm(960), 120, 8,
                                  dimnames = list(NULL, PANO()))))
}

sel_cases <- function() {
  list(
    ssm_analyze = list(
      data = sel_frame(), pos = 2:9,
      call = function(d, s) ssm_analyze(d, s, boots = 20),
      key = function(x) x$results
    ),
    ssm_analyze_long = list(
      data = sel_long(), pos = 3:10,
      call = function(d, s) ssm_analyze_long(d, s, id = "id",
                                             occasion = "occasion",
                                             boots = 20),
      key = function(x) x$results
    ),
    ssm_score = list(
      data = sel_frame(), pos = 2:9,
      call = function(d, s) ssm_score(d, s),
      key = function(x) x
    ),
    ssm_parameters_id = list(
      data = sel_frame(), pos = 2:9,
      call = function(d, s) ssm_parameters_id(d, s),
      key = function(x) x
    ),
    ssm_growth_data = list(
      data = simulated_growth, pos = 3:10,
      call = function(d, s) ssm_growth_data(d, s, id = "person",
                                            time = "wave"),
      key = function(x) x
    ),
    ssm_sem = list(
      data = sel_frame(), pos = 2:9, lavaan = TRUE,
      call = function(d, s) ssm_sem(d, s, grouping = "Gender", boots = 5),
      key = function(x) x$results
    ),
    cpm_fit = list(
      data = sel_frame(), pos = 2:9,
      call = function(d, s) cpm_fit(d, s, ci_method = "analytic"),
      key = function(x) x$results
    ),
    fit_structure = list(
      data = sel_frame(), pos = 2:9,
      call = function(d, s) fit_structure(d, s),
      key = function(x) x$results
    ),
    norm_standardize = list(
      data = sel_frame(), pos = 2:9,
      call = function(d, s) norm_standardize(d, s, instrument = csip,
                                             sample = 1),
      key = function(x) x
    )
  )
}

# Run a case quietly with a fixed seed, so two calls with the same selection
# give the same bootstrap draws
sel_run <- function(case, d, s) {
  set.seed(1)
  suppressWarnings(suppressMessages(case$call(d, s)))
}

sel_skip_lavaan <- function(case) {
  if (isTRUE(case$lavaan)) skip_if_not_installed("lavaan")
}

# The message each probe gave at 447a74dd. ssm_analyze_long() checks names
# itself; every other export reaches `[.data.frame`.
sel_baseline_message <- function(name) {
  if (name == "ssm_analyze_long") {
    "must all name columns in `data`"
  } else {
    "undefined columns selected"
  }
}

test_that("probes the guards do not target keep their result or error", {
  skip_on_cran()
  cases <- sel_cases()
  expect_length(cases, 9)
  for (name in names(cases)) {
    case <- cases[[name]]
    sel_skip_lavaan(case)
    d <- case$data
    pos <- case$pos

    # A non-whole number of 1 or more selects the column R's truncation
    # reaches, so the result equals the whole-number selection's
    whole <- sel_run(case, d, pos)
    nonwhole <- sel_run(case, d, c(pos[[1]] + 0.5, pos[-1]))
    expect_equal(case$key(nonwhole), case$key(whole), label = name)

    msg <- sel_baseline_message(name)
    expect_error(sel_run(case, d, c(NA, pos[-1])), msg, fixed = TRUE,
                 label = paste(name, "NA"))
    expect_error(sel_run(case, d, c("ZZ", PANO()[-1])), msg, fixed = TRUE,
                 label = paste(name, "absent"))
    expect_error(sel_run(case, d, c(99, pos[-1])), msg, fixed = TRUE,
                 label = paste(name, "past"))
  }
})

test_that("fit_structure keeps its matrix-input errors for untargeted probes", {
  skip_on_cran()
  m <- as.matrix(sel_frame()[PANO()])
  ref <- fit_structure(m, 1:8)
  expect_equal(fit_structure(m, c(1.5, 2:8))$results, ref$results)
  expect_error(fit_structure(m, c(NA, 2:8)), "undefined columns selected",
               fixed = TRUE)
  expect_error(fit_structure(m, c("ZZ", PANO()[-1])),
               "subscript out of bounds", fixed = TRUE)
  expect_error(fit_structure(m, c(99, 2:8)), "subscript out of bounds",
               fixed = TRUE)
})

# The helper ------------------------------------------------------------------

test_that("check_scales_selection refuses a column selected twice", {
  d <- data.frame(id = 1:3, PA = 1:3, BC = 4:6, DE = 7:9)
  expect_error(check_scales_selection(d, c("PA", "BC", "PA")),
               "`scales` selects the column \"PA\" more than once.",
               fixed = TRUE)
  expect_error(check_scales_selection(d, c(2, 3, 2)),
               "`scales` selects the column \"PA\" more than once.",
               fixed = TRUE)
  # 3 and 3.5 both reach BC
  expect_error(check_scales_selection(d, c(2, 3, 3.5)),
               "`scales` selects the column \"BC\" more than once.",
               fixed = TRUE)
  # A matrix with no column names names the position instead
  m <- matrix(1:12, 3, 4)
  expect_error(check_scales_selection(m, c(1, 2, 2)),
               "`scales` selects the column at position 2 more than once.",
               fixed = TRUE)
})

test_that("check_scales_selection refuses a position below 1", {
  d <- data.frame(PA = 1:3, BC = 4:6, DE = 7:9)
  for (bad in c(0, 0.5, -0.5, -1)) {
    expect_error(check_scales_selection(d, c(1, bad, 3)),
                 paste0("`scales` holds the position ", format(bad),
                        ", below 1 once truncated"),
                 fixed = TRUE, label = format(bad))
  }
  # The position check comes before the repeat check, so c(-1, -1) names
  # the position, not a repeat
  expect_error(check_scales_selection(d, c(-1, -1)), "holds the position -1",
               fixed = TRUE)
})

test_that("check_scales_selection refuses a name that occurs twice in data", {
  d <- data.frame(PA = 1:3, BC = 4:6, PA = 7:9, check.names = FALSE)
  expect_error(check_scales_selection(d, c("PA", "BC")),
               paste0("`scales` names a column that occurs more than once ",
                      "in `data`: \"PA\"."),
               fixed = TRUE)
  # Numbers reach one column each, so the repeated name is no obstacle
  expect_invisible(check_scales_selection(d, c(1, 2, 3)))
  # The same holds for a matrix's own column names
  m <- matrix(1:9, 3, 3, dimnames = list(NULL, c("PA", "BC", "PA")))
  expect_error(check_scales_selection(m, c("PA", "BC")),
               "occurs more than once in `data`: \"PA\".", fixed = TRUE)
})

test_that("check_scales_selection passes untargeted entries through", {
  d <- data.frame(PA = 1:3, BC = 4:6, DE = 7:9)
  expect_invisible(check_scales_selection(d, c("PA", "BC", "DE")))
  expect_invisible(check_scales_selection(d, c(1.5, 2, 3)))
  # NA, absent names and positions past the end are the caller's to refuse,
  # also when they repeat
  expect_invisible(check_scales_selection(d, c(NA, NA, 1)))
  expect_invisible(check_scales_selection(d, c("ZZ", "ZZ", "PA")))
  expect_invisible(check_scales_selection(d, c(9, 9, 1)))
  expect_invisible(check_scales_selection(d, c(NA_character_, "PA")))
})

test_that("check_scales_labels refuses a repeated label", {
  expect_error(check_scales_labels(c("PA", "BC", "PA")),
               "`scales` lists \"PA\" more than once.", fixed = TRUE)
  expect_error(check_scales_labels(c("PA", "PA"), "`colnames(cormat)`"),
               "`colnames(cormat)` lists \"PA\" more than once.", fixed = TRUE)
  expect_invisible(check_scales_labels(c("PA", "BC")))
})
