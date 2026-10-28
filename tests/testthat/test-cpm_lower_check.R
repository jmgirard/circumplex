# The lower-discrepancy check: cpm_fit()'s extra starts after the engine's fit
# (the help section "Which solution is reported"). The fits are the CircE
# verbal and vocational matrices (helper-cpm-oracles.R) and jz2017's octants.

lower_case <- function(name) {
  switch(name,
    verbal = {
      v <- cpm_oracle_verbal()
      list(R = v$R, N = v$N, names = v$names, angles = 360 * (0:5) / 6, m = 2)
    },
    voc = {
      v <- cpm_oracle_voc()
      list(R = v$R, N = v$N, names = v$names, angles = v$th_start, m = 1)
    },
    jz2017 = {
      rows <- stats::na.omit(jz2017[PANO()])
      list(R = stats::cor(rows), N = nrow(rows), names = PANO(),
           angles = octants(), m = 3)
    }
  )
}

lower_engine <- function(case) {
  suppressWarnings(cpm_engine(case$R, angles = case$angles, m = case$m))
}

lower_fit <- function(case, ...) {
  suppressWarnings(cpm_fit(cormat = case$R, scales = case$names, n = case$N,
                           angles = case$angles, m = case$m, ...))
}

# ---- the start set -------------------------------------------------------

test_that("the extra starts number p + 1 + 3(p - 1) + 3 choose(p - 1, 2) + p and are finite", {
  for (name in c("verbal", "voc", "jz2017")) {
    case <- lower_case(name)
    starts <- cpm_lower_starts(lower_engine(case), case$R)
    p <- nrow(case$R)
    expect_length(starts, p + 1 + 3 * (p - 1) + 3 * choose(p - 1, 2) + p)
    expect_true(all(vapply(starts, function(g) all(is.finite(g)), logical(1))))
  }
})

# ---- published and real fits -------------------------------------------------

test_that("the check flags the verbal matrix's reordered lower solution", {
  # Basin reached by nlminb() can differ by platform.
  skip_on_cran()
  lc <- lower_fit(lower_case("verbal"))$details$lower_check
  expect_true(lc$checked)
  expect_true(lc$found)
  expect_true(lc$order_changed)
  expect_lt(abs(lc$gap - 3.158), 0.01)
})

test_that("the check does not fire on the jz2017 and vocational fits", {
  for (name in c("jz2017", "voc")) {
    lc <- lower_fit(lower_case(name))$details$lower_check
    expect_true(lc$checked)
    expect_false(lc$found)
  }
})

# ---- the printed note ------------------------------------------------------

lower_note <- "extra starts reached a solution"

test_that("print() and summary() show one note when the check fires", {
  skip_on_cran()
  fit <- lower_fit(lower_case("verbal"))
  for (out in list(capture.output(print(fit)), capture.output(summary(fit)))) {
    text <- paste(out, collapse = " ")
    text <- gsub("\\s+", " ", text)
    expect_identical(lengths(regmatches(text, gregexpr(lower_note, text))), 1L)
    expect_match(text, paste0("lower by 3\\.1[5-6][0-9]\\. ",
                              "It puts the scales in a different order"))
    expect_match(text, "communality ind(ex|ices) at the upper bound")
    expect_match(text, "The estimates shown are unchanged")
    expect_match(text, "\"Which solution is reported\"", fixed = TRUE)
  }
})

test_that("the note gives a differing df and each shape of the lower solution", {
  details <- function(df, order_changed, more_wall) {
    list(accepted = TRUE, spec = list(df = 10),
         lower_check = list(checked = TRUE, found = TRUE, gap = 1.23456,
                            df = df, order_changed = order_changed,
                            more_wall = more_wall, failed_starts = 0L))
  }
  note_text <- function(d) {
    gsub("\\s+", " ", paste(cpm_diagnostic_lines(d), collapse = " "))
  }
  expect_match(note_text(details(11, FALSE, 1L)),
               paste0("lower by 1.235 \\(at df = 11\\)\\. It keeps the ",
                      "scales' order around the circle and has 1 more ",
                      "communality index at the upper bound"))
  same_df <- note_text(details(10, TRUE, -2L))
  expect_false(grepl("at df", same_df, fixed = TRUE))
  expect_match(same_df, "different order .* and has 2 fewer communality indices")
  expect_match(note_text(details(10, NA, NA_integer_)),
               "Its scale order and communality bounds could not be read")
})

test_that("no note prints when the check does not fire or does not run", {
  fits <- list(lower_fit(lower_case("jz2017")),
               lower_fit(lower_case("verbal"), check_lower = FALSE))
  for (fit in fits) {
    out <- c(capture.output(print(fit)), capture.output(summary(fit)))
    expect_false(any(grepl(lower_note, out, fixed = TRUE)))
  }
})

test_that("the check adds no boundary marker", {
  labels <- cpm_marker_labels()
  expect_identical(names(labels), c("heywood", "removed", "small_beta",
                                    "illcond", "multimodal"))
  for (name in c("verbal", "jz2017")) {
    case <- lower_case(name)
    expect_identical(cpm_boundary_markers(lower_fit(case)),
                     cpm_boundary_markers(lower_fit(case, check_lower = FALSE)))
  }
})

# ---- no random numbers, no change to what the fit reports -------------------

test_that("the check leaves the random stream and the reported fit unchanged", {
  case <- lower_case("jz2017")
  set.seed(101)
  s0 <- .Random.seed
  fit <- lower_fit(case, ci_method = "analytic")
  expect_true(fit$details$lower_check$checked)
  expect_identical(.Random.seed, s0)

  rows <- stats::na.omit(jz2017[PANO()])
  set.seed(102)
  on <- suppressWarnings(
    cpm_fit(rows, scales = PANO(), boots = 20, check_lower = TRUE)
  )
  s_on <- .Random.seed
  set.seed(102)
  off <- suppressWarnings(
    cpm_fit(rows, scales = PANO(), boots = 20, check_lower = FALSE)
  )
  expect_identical(.Random.seed, s_on)
  expect_true(on$details$lower_check$checked)
  expect_false(off$details$lower_check$checked)
  for (part in c("results", "betas", "fit")) {
    expect_identical(on[[part]], off[[part]])
  }
  keep <- setdiff(names(on$details), "lower_check")
  expect_identical(on$details[keep], off$details[keep])
})

# ---- the argument and where the check runs ----------------------------------

test_that("check_lower must be a single TRUE or FALSE", {
  case <- lower_case("jz2017")
  expect_error(lower_fit(case, check_lower = NA), "check_lower")
  expect_error(lower_fit(case, check_lower = c(TRUE, TRUE)), "check_lower")
})

test_that("the check runs only for the quasi-circumplex model with unit scaling", {
  case <- lower_case("verbal")
  not_run <- cpm_lower_not_run()
  expect_identical(lower_fit(case, check_lower = FALSE)$details$lower_check,
                   not_run)
  for (model in c("constrained-angles", "equal-communality", "circulant")) {
    expect_identical(lower_fit(case, model = model)$details$lower_check,
                     not_run)
  }
  expect_identical(lower_fit(case, scaling = "free")$details$lower_check,
                   not_run)
})

# ---- failed starts and a failed finish -------------------------------------

# Runs the check with cpm_optimize_one() failing on the starts that `fail`
# selects, by throwing or (with `nonfinite = TRUE`) by returning a NaN F.
lower_probe <- function(case, eng, fail, nonfinite = FALSE) {
  real <- cpm_optimize_one
  local_mocked_bindings(cpm_optimize_one = function(gstar0, R, spec) {
    if (fail(gstar0)) {
      if (nonfinite) return(list(par = gstar0, F = NaN, code = 1L))
      stop("planted refit failure")
    }
    real(gstar0, R, spec)
  })
  cpm_lower_check(eng, case$R, case$N)
}

test_that("a start whose refit throws or returns a non-finite F is skipped", {
  skip_on_cran()
  case <- lower_case("verbal")
  eng <- lower_engine(case)
  starts <- cpm_lower_starts(eng, case$R)
  base <- cpm_lower_check(eng, case$R, case$N)
  expect_true(base$found)
  expect_identical(base$failed_starts, 0L)
  matches <- function(j) function(g) identical(g, starts[[j]])
  n_like <- function(j) sum(vapply(starts, identical, logical(1), starts[[j]]))

  # the lowest start, found by running every start here
  spec <- cpm_lower_spec(eng)
  Fs <- vapply(starts, function(g) cpm_optimize_one(g, case$R, spec)$F,
               numeric(1))
  lowest <- which.min(Fs)
  expect_false(lowest %in% c(1L, length(starts)))

  for (j in c(1L, length(starts))) {
    for (nonfinite in c(FALSE, TRUE)) {
      res <- lower_probe(case, eng, matches(j), nonfinite)
      expect_identical(res$failed_starts, as.integer(n_like(j)))
      expect_identical(res[setdiff(names(res), "failed_starts")],
                       base[setdiff(names(base), "failed_starts")])
    }
  }

  res <- lower_probe(case, eng, matches(lowest))
  expect_identical(res$failed_starts, as.integer(n_like(lowest)))
  failed <- vapply(starts, identical, logical(1), starts[[lowest]])
  rest <- (case$N - 1) * (eng$F - min(Fs[!failed]))
  expect_identical(res$found, rest > 0.01)
  if (rest > 0.01) expect_equal(res$gap, rest, tolerance = 1e-6)
})

test_that("the check runs and does not fire when every start fails", {
  case <- lower_case("jz2017")
  eng <- lower_engine(case)
  n <- length(cpm_lower_starts(eng, case$R))
  res <- lower_probe(case, eng, function(g) TRUE)
  expect_true(res$checked)
  expect_false(res$found)
  expect_identical(res$failed_starts, as.integer(n))
  expect_true(is.na(res$gap))
})

test_that("a failed finish keeps the flag on the raw gap with order and wall NA", {
  skip_on_cran()
  case <- lower_case("verbal")
  eng <- lower_engine(case)
  base <- cpm_lower_check(eng, case$R, case$N)
  local_mocked_bindings(
    cpm_polish_beta = function(fit, R, spec) stop("planted finish failure")
  )
  res <- cpm_lower_check(eng, case$R, case$N)
  expect_true(res$found)
  expect_equal(res$gap, base$gap, tolerance = 1e-6)
  expect_identical(res$df, cpm_lower_spec(eng)$df)
  expect_true(is.na(res$order_changed))
  expect_true(is.na(res$more_wall))
})
