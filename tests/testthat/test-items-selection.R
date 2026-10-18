# Column selection by `items`, and by `scales` in self_standardize() (M160) ----
#
# score(), ipsatize() and axes_reliability() refuse an `items` selection, and
# self_standardize() a `scales` selection, in the three forms the `scales`
# guards refuse (test-scales-selection.R): a number that truncates below 1, a
# name that occurs more than once in the data, and one column selected twice.
# Four other probes (1.5 in place of 1, an NA, an absent name, a number past
# the last column) keep the result or error they gave before the guards
# (71d44034); each expected message below was recorded at that commit.

# The case tables (it_cases() and friends) are in helper-selection.R.

# The comparable part of a result: axes_reliability() stores its call
it_key <- function(x) if (is.data.frame(x)) x else x$results

# The message each probe gave at 71d44034
it_baseline_message <- function(name, probe) {
  if (name != "axes_reliability") return("undefined columns selected")
  switch(probe,
         na = "Item column(s) not found in `data`: .",
         absent = "Item column(s) not found in `data`: ZZ.",
         past = "Item column(s) not found in `data`: .")
}

test_that("probes the items guards do not target keep their result or error", {
  skip_on_cran()
  cases <- it_cases()
  expect_length(cases, 4)
  for (name in names(cases)) {
    case <- cases[[name]]
    it_skip_lavaan(case)
    d <- case$data
    pos <- case$pos
    n <- length(pos)

    # 1.5 truncates to 1, so the result equals the whole-number selection's
    whole <- it_run(case, d, pos)
    nonwhole <- it_run(case, d, c(1.5, pos[-1]))
    expect_equal(it_key(nonwhole), it_key(whole), label = name)

    probes <- list(
      na = c(NA, pos[-1]),
      absent = c("ZZ", names(d)[pos[-1]]),
      past = c(pos[-n], 99)
    )
    for (probe in names(probes)) {
      expect_error(it_run(case, d, probes[[probe]]),
                   it_baseline_message(name, probe), fixed = TRUE,
                   label = paste(name, probe))
    }
  }
})

# The helper, on `items` -------------------------------------------------------

test_that("check_scales_selection names the argument and source it is given", {
  d <- data.frame(PA = 1:3, BC = 4:6, PA = 7:9, check.names = FALSE)
  expect_error(check_scales_selection(d, c(1, 0), "items"),
               "`items` holds the position 0, below 1 once truncated",
               fixed = TRUE)
  expect_error(check_scales_selection(d, c(2, 2), "items"),
               "`items` selects the column \"BC\" more than once.",
               fixed = TRUE)
  expect_error(check_scales_selection(d, "PA", "items", "`cormat`"),
               paste0("`items` names a column that occurs more than once in ",
                      "`cormat`: \"PA\"."),
               fixed = TRUE)
  # The defaults keep the `scales` wording
  expect_error(check_scales_selection(d, "PA"),
               "`scales` names a column that occurs more than once in `data`",
               fixed = TRUE)
  expect_error(check_scales_positions(-1, "items"),
               "`items` holds the position -1,", fixed = TRUE)
})

# The refusals: score(), ipsatize(), self_standardize() -----------------------

it_tidy_cases <- function() it_cases()[c("score", "ipsatize", "self_standardize")]

test_that("score refuses a repeated item instead of shifting the scales", {
  # Before the guard, items = c(1, 1:32) moved every later item one place on,
  # so BC to NO were each scored from the previous octant's items (and PA
  # from IIP01 and three NO items), with no message
  expect_error(score(raw_iipsc, c(1, 1:32), instrument = iipsc),
               "`items` selects the column \"IIP01\" more than once.",
               fixed = TRUE)
})

test_that("the tidying functions refuse a column selected twice", {
  for (name in names(it_tidy_cases())) {
    case <- it_tidy_cases()[[name]]
    d <- case$data
    pos <- case$pos
    first <- names(d)[pos[[1]]]
    msg <- paste0("`", case$arg, "` selects the column \"", first,
                  "\" more than once.")
    rep_name <- names(d)[pos]
    rep_name[2] <- first
    forms <- list(
      name = rep_name,
      number = c(pos[1], pos[1], pos[-(1:2)]),
      truncated = c(pos[1], pos[1] + 0.5, pos[-(1:2)])
    )
    shapes <- list(frame = d, matrix = as.matrix(d))
    for (shape in names(shapes)) {
      for (form in names(forms)) {
        expect_error(it_run(case, shapes[[shape]], forms[[form]]), msg,
                     fixed = TRUE, label = paste(name, shape, form))
      }
    }
  }
})

test_that("the tidying functions refuse a position below 1", {
  for (name in names(it_tidy_cases())) {
    case <- it_tidy_cases()[[name]]
    d <- case$data
    pos <- case$pos
    shapes <- list(frame = d, matrix = as.matrix(d))
    for (shape in names(shapes)) {
      for (bad in c(0, 0.5, -1)) {
        expect_error(it_run(case, shapes[[shape]], c(pos[1], bad, pos[-(1:2)])),
                     paste0("`", case$arg, "` holds the position ",
                            format(bad), ", below 1 once truncated"),
                     fixed = TRUE, label = paste(name, shape, format(bad)))
      }
    }
  }
})

test_that("the tidying functions refuse a name that occurs twice", {
  for (name in names(it_tidy_cases())) {
    case <- it_tidy_cases()[[name]]
    d <- case$data
    pos <- case$pos
    first <- names(d)[pos[[1]]]
    d2 <- cbind(d, d[first])
    names(d2) <- c(names(d), first)
    msg <- paste0("`", case$arg, "` names a column that occurs more than ",
                  "once in `data`: \"", first, "\".")
    expect_error(it_run(case, d2, names(d)[pos]), msg, fixed = TRUE,
                 label = name)
    expect_error(it_run(case, as.matrix(d2), names(d)[pos]), msg,
                 fixed = TRUE, label = paste(name, "matrix"))
  }
})

test_that("the tidying functions accept a number reaching a repeated name", {
  for (name in names(it_tidy_cases())) {
    case <- it_tidy_cases()[[name]]
    d <- case$data
    pos <- case$pos
    first <- names(d)[pos[[1]]]
    # The extra column comes first, so the selection by number skips it and
    # the scores equal those from the data without it
    d2 <- cbind(d[first], d)
    names(d2) <- c(first, names(d))
    expect_true(anyDuplicated(names(d2)) > 0)
    with_extra <- it_run(case, d2, pos + 1)
    without <- it_run(case, d, pos)
    added <- setdiff(names(without), names(d))
    expect_gt(length(added), 0)
    expect_equal(with_extra[added], without[added], label = name)
  }
})

# The refusals: axes_reliability() --------------------------------------------

# The ways in: the flat `items` vector with `instrument`, and the per-scale
# list, each with data-frame `data`, matrix `data`, and `cormat`. `first`
# stands for the first scale's four items: the flat form puts it before items
# 5 to 32, and the list form puts it under the first scale. `d` defaults to
# the 32 items; a test may pass extra columns after them.
it_axes_call <- function(form, path, first, d = simulated_items) {
  src <- switch(path,
    data = list(data = d),
    matrix = list(data = as.matrix(d)),
    cormat = list(cormat = stats::cor(d), n = nrow(d))
  )
  if (form == "flat") {
    rest <- seq_len(32)[-seq_along(first)]
    items <- c(first, if (is.character(first)) names(d)[rest] else rest)
    do.call(axes_reliability, c(src, list(items = items, instrument = iipsc)))
  } else {
    do.call(axes_reliability, c(src, list(items = it_axes_map(first),
                                          angles = octants())))
  }
}

it_axes_ways <- expand.grid(form = c("flat", "list"),
                            path = c("data", "matrix", "cormat"),
                            stringsAsFactors = FALSE)

# What the messages call the source on each path
it_axes_where <- function(path) if (path == "cormat") "`cormat`" else "`data`"

test_that("axes_reliability refuses the three forms on every way in", {
  skip_if_not_installed("lavaan")
  for (i in seq_len(nrow(it_axes_ways))) {
    form <- it_axes_ways$form[[i]]
    path <- it_axes_ways$path[[i]]
    lab <- paste(form, path)
    run <- function(first) suppressMessages(it_axes_call(form, path, first))
    expect_error(run(c(1, 1, 3, 4)),
                 "`items` selects the column \"PA_1\" more than once.",
                 fixed = TRUE, label = paste(lab, "number"))
    expect_error(run(c(1, 1.5, 3, 4)),
                 "`items` selects the column \"PA_1\" more than once.",
                 fixed = TRUE, label = paste(lab, "truncated"))
    expect_error(run(c("PA_1", "PA_1", "PA_3", "PA_4")),
                 "`items` selects the column \"PA_1\" more than once.",
                 fixed = TRUE, label = paste(lab, "name"))
    for (bad in c(0, 0.5, -1)) {
      expect_error(run(c(1, bad, 3, 4)),
                   paste0("`items` holds the position ", format(bad),
                          ", below 1 once truncated"),
                   fixed = TRUE, label = paste(lab, format(bad)))
    }
  }
})

test_that("axes_reliability refuses a repeated name on every way in", {
  skip_if_not_installed("lavaan")
  d <- simulated_items
  d2 <- cbind(d, PA_1 = rev(d$PA_1))
  expect_identical(anyDuplicated(colnames(stats::cor(d2))), 33L)
  first <- c("PA_1", "PA_2", "PA_3", "PA_4")
  for (i in seq_len(nrow(it_axes_ways))) {
    form <- it_axes_ways$form[[i]]
    path <- it_axes_ways$path[[i]]
    expect_error(
      suppressMessages(it_axes_call(form, path, first, d2)),
      paste0("`items` names a column that occurs more than once in ",
             it_axes_where(path), ": \"PA_1\"."),
      fixed = TRUE, label = paste(form, path)
    )
  }
})

test_that("axes_reliability refuses a number reaching a repeated name", {
  skip_if_not_installed("lavaan")
  # Column 33 is PA_1 reversed. Before the guard, the number 33 was turned
  # into the name PA_1 and the fit used column 1 instead, with no message.
  d2 <- cbind(simulated_items, PA_1 = rev(simulated_items$PA_1))
  for (i in seq_len(nrow(it_axes_ways))) {
    form <- it_axes_ways$form[[i]]
    path <- it_axes_ways$path[[i]]
    expect_error(
      suppressMessages(it_axes_call(form, path, c(33, 2, 3, 4), d2)),
      paste0("`items` selects position 33, whose name \"PA_1\" occurs more ",
             "than once in ", it_axes_where(path)),
      fixed = TRUE, label = paste(form, path)
    )
  }
  # A number reaching column 1, the first PA_1, is refused too: the name is
  # what the fit reads, and it does not say which column was meant
  expect_error(
    suppressMessages(it_axes_call("list", "data", 1:4, d2)),
    "`items` selects position 1, whose name \"PA_1\"", fixed = TRUE
  )
})

test_that("axes_reliability refuses one column under two scales", {
  skip_if_not_installed("lavaan")
  map <- unname(split(names(simulated_items), rep(1:8, each = 4)))
  map[[2]][1] <- "PA_1"
  expect_error(
    suppressMessages(axes_reliability(simulated_items, map,
                                      angles = octants())),
    "`items` lists the column \"PA_1\" under more than one scale.",
    fixed = TRUE
  )
  # A name in one scale and a number in another that reach one column
  map2 <- unname(split(seq_len(32), rep(1:8, each = 4)))
  map2[[1]] <- c("PA_1", "PA_2", "PA_3", "PA_4")
  map2[[2]][1] <- 1L
  expect_error(
    axes_reliability(cormat = stats::cor(simulated_items), n = 500,
                     items = map2, angles = octants()),
    "`items` lists the column \"PA_1\" under more than one scale.",
    fixed = TRUE
  )
  # Matrix `data`, by name and by number
  m <- as.matrix(simulated_items)
  for (sel in list(map, map2)) {
    expect_error(
      suppressMessages(axes_reliability(m, sel, angles = octants())),
      "`items` lists the column \"PA_1\" under more than one scale.",
      fixed = TRUE
    )
  }
})

test_that("axes_reliability judges a factor in the list form by its labels", {
  skip_if_not_installed("lavaan")
  # A factor's integer codes follow its sorted levels, so judging the codes
  # would name PA_2 here, a column listed once
  map <- unname(split(names(simulated_items), rep(1:8, each = 4)))
  map[[1]] <- factor(c("PA_3", "PA_3", "PA_2", "PA_4"))
  expect_error(
    suppressMessages(axes_reliability(simulated_items, map,
                                      angles = octants())),
    "`items` selects the column \"PA_3\" more than once.", fixed = TRUE
  )
  # A factor naming a column whose name repeats is refused like a name
  d2 <- cbind(simulated_items, PA_1 = rev(simulated_items$PA_1))
  map2 <- unname(split(names(simulated_items), rep(1:8, each = 4)))
  map2[[1]] <- factor(map2[[1]])
  expect_error(
    suppressMessages(axes_reliability(d2, map2, angles = octants())),
    "`items` names a column that occurs more than once in `data`: \"PA_1\".",
    fixed = TRUE
  )
  # A factor with distinct labels runs as the same names do
  skip_on_cran()
  map3 <- unname(split(names(simulated_items), rep(1:8, each = 4)))
  ref <- suppressMessages(axes_reliability(simulated_items, map3,
                                           angles = octants()))
  map3[[1]] <- factor(map3[[1]])
  fac <- suppressMessages(axes_reliability(simulated_items, map3,
                                           angles = octants()))
  expect_equal(fac$results, ref$results)
})
