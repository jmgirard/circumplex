# Case tables for the column-selection guards, shared by
# test-scales-selection.R (M159), test-items-selection.R (M160) and the
# export-listing test there.

# One case per data-selecting export with a `scales` argument, except
# self_standardize(), which is in it_cases() below: its data, the positions
# of PA..NO in that data, and a call taking (data, scales). `key`
# extracts the part of the result that a probe compares, leaving out the
# stored call.
sel_frame <- function() jz2017[1:300, c("Gender", PANO())]

sel_long <- function() {
  set.seed(159)
  out <- data.frame(id = rep(1:60, each = 2),
                    occasion = rep(c("T1", "T2"), 60))
  cbind(out, as.data.frame(matrix(rnorm(960), 120, 8,
                                  dimnames = list(NULL, PANO()))))
}

# The columns an appending export adds, leaving out the input columns, so a
# comparison is not thrown off by an extra input column
sel_new_columns <- function(x) x[!(names(x) %in% c("Gender", PANO()))]

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
      key = sel_new_columns
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
      key = sel_new_columns
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

# One case per function with an `items` selection, plus self_standardize():
# its data, a valid numeric selection, its argument name, and a call taking
# (data, selection). axes_reliability() is probed on its list form, with the
# selection standing for the first scale's items.
it_axes_map <- function(first) {
  map <- split(seq_len(32), rep(1:8, each = 4))
  map[[1]] <- first
  unname(map)
}

it_cases <- function() {
  list(
    score = list(
      data = raw_iipsc, pos = 1:32, arg = "items",
      call = function(d, s) score(d, s, instrument = iipsc)
    ),
    ipsatize = list(
      data = raw_iipsc, pos = 1:32, arg = "items",
      call = function(d, s) ipsatize(d, s)
    ),
    self_standardize = list(
      data = aw2009, pos = 1:8, arg = "scales",
      call = function(d, s) self_standardize(d, s)
    ),
    axes_reliability = list(
      data = simulated_items, pos = 1:4, arg = "items", lavaan = TRUE,
      call = function(d, s) {
        axes_reliability(d, it_axes_map(s), angles = octants())
      }
    )
  )
}

it_run <- function(case, d, s) {
  suppressWarnings(suppressMessages(case$call(d, s)))
}

it_skip_lavaan <- function(case) {
  if (isTRUE(case$lavaan)) skip_if_not_installed("lavaan")
}
