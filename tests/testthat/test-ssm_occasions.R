# Occasions (repeated-measures) SSM analyses: intake validation, scoring,
# contrasts, and engines (M25; binding spec devel/longitudinal-ssm-spec.md
# sec. 1-2). Fixture provenance: make_occ_data() below generates all wide
# two-plus-occasion fixtures from a named seed (no committed data files).

# Generate wide occasions data: one row per person, k occasion blocks of the
# p = 8 octant scales, named <scale>_<j> (stem-matchable). `rho` controls the
# within-person cross-occasion dependence (shared person effect); occasion 2
# gets `shift` added to every scale so the occasions genuinely differ.
make_occ_data <- function(n = 50, k = 2, seed = 123, rho = 0.6, shift = 0.5,
                          scales = c("PA", "BC", "DE", "FG",
                                     "HI", "JK", "LM", "NO")) {
  set.seed(seed)
  p <- length(scales)
  person <- matrix(rnorm(n * p), n, p) # shared person effect
  blocks <- lapply(seq_len(k), function(j) {
    block <- sqrt(rho) * person + sqrt(1 - rho) * matrix(rnorm(n * p), n, p) +
      2 + (j - 1) * shift
    colnames(block) <- paste0(scales, "_", j)
    block
  })
  df <- as.data.frame(do.call(cbind, blocks))
  df$Gender <- factor(rep(c("F", "M"), length.out = n))
  df
}

occ_names <- function(j, scales = c("PA", "BC", "DE", "FG",
                                    "HI", "JK", "LM", "NO")) {
  paste0(scales, "_", j)
}

# Intake validation (spec sec. 1.1/1.3) ----------------------------------------

test_that("occasions and scales are mutually exclusive spellings", {
  data <- make_occ_data()
  expect_error(
    ssm_analyze(data,
      scales = occ_names(1),
      occasions = list(T1 = occ_names(1), T2 = occ_names(2))
    ),
    "mutually exclusive"
  )
  expect_error(ssm_analyze(data), "either `scales` or `occasions`")
})

test_that("occasions rejects the correlation (measures) path", {
  data <- make_occ_data()
  data$NARPD <- rnorm(nrow(data))
  expect_error(
    ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
      measures = "NARPD"
    ),
    "occasions.*measures|measures.*occasions"
  )
})

test_that("occasions blocks are validated for shape", {
  data <- make_occ_data()
  # not a list
  expect_error(
    ssm_analyze(data, occasions = occ_names(1)),
    "list"
  )
  # fewer than two occasions
  expect_error(
    ssm_analyze(data, occasions = list(T1 = occ_names(1))),
    "at least two"
  )
  # block length must match angles
  expect_error(
    ssm_analyze(data,
      occasions = list(T1 = occ_names(1)[1:7], T2 = occ_names(2)[1:7])
    ),
    "length"
  )
  # unequal block lengths caught even before the angle check fires
  expect_error(
    ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T2 = occ_names(2)[1:7])
    ),
    "length"
  )
  # partially named lists are ambiguous
  expect_error(
    ssm_analyze(data,
      occasions = stats::setNames(list(occ_names(1), occ_names(2)), c("T1", ""))
    ),
    "name"
  )
  # duplicate labels are ambiguous
  expect_error(
    ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T1 = occ_names(2))
    ),
    "name"
  )
})

test_that("occasions requires listwise deletion (estimand grounds)", {
  data <- make_occ_data()
  expect_error(
    ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
      listwise = FALSE
    ),
    "within-person"
  )
})

test_that("stem-order mismatch (rotation) errors naming the block", {
  data <- make_occ_data()
  # same stems, rotated order: exactly the silent-rotation bug (spec sec. 1.1)
  rotated <- occ_names(2)[c(2:8, 1)]
  expect_error(
    ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T2 = rotated)
    ),
    "T2.*order|order.*T2"
  )
})

test_that("stem mismatch (different stems) errors naming the block", {
  data <- make_occ_data()
  names(data)[names(data) == "PA_2"] <- "ZZ_2"
  bad <- c("ZZ_2", occ_names(2)[-1])
  expect_error(
    ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T2 = bad)
    ),
    "stem"
  )
})

test_that("no stem structure falls back to a positional-alignment message", {
  data <- make_occ_data()
  # occasion-2 names with no common prefix or suffix
  flat <- c("alpha", "bravo", "charlie", "delta",
            "echo", "foxtrot", "golf", "hotel")
  names(data)[9:16] <- flat
  expect_message(
    res <- ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T2 = flat),
      boots = 10
    ),
    "positional"
  )
  expect_s3_class(res, "circumplex_ssm")
})

# Scoring and result assembly (spec sec. 1.1/1.2) ------------------------------

test_that("occasions profiles are correct and ordered occasion-minor", {
  data <- make_occ_data(n = 60, k = 3)
  occ <- list(T1 = occ_names(1), T2 = occ_names(2), T3 = occ_names(3))
  set.seed(1)
  res <- ssm_analyze(data, occasions = occ, boots = 10)
  expect_s3_class(res, "circumplex_ssm")
  expect_true("Occasion" %in% names(res$results))
  expect_equal(res$results$Occasion, c("T1", "T2", "T3"))
  expect_equal(res$results$Label, c("T1", "T2", "T3"))
  # per-occasion point estimates equal the equivalent single-scales run
  # exactly (complete data, same rows) -- the exact invariant of AC2
  for (j in 1:3) {
    set.seed(2)
    ref <- ssm_analyze(data, scales = occ_names(j), boots = 10)
    expect_equal(
      unlist(res$results[j, c("e_est", "x_est", "y_est", "a_est", "d_est",
                              "fit_est")]),
      unlist(ref$results[1, c("e_est", "x_est", "y_est", "a_est", "d_est",
                              "fit_est")]),
      tolerance = 1e-12
    )
  }
  # details carry the occasions metadata
  expect_equal(res$details$occasions, c("T1", "T2", "T3"))
})

test_that("unnamed occasions default to T1..Tk labels", {
  data <- make_occ_data()
  set.seed(1)
  res <- ssm_analyze(data,
    occasions = list(occ_names(1), occ_names(2)), boots = 10
  )
  expect_equal(res$results$Occasion, c("T1", "T2"))
})

test_that("occasions cross grouping: group-major, occasion-minor rows", {
  data <- make_occ_data(n = 80)
  set.seed(1)
  res <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
    grouping = "Gender", boots = 10
  )
  expect_equal(res$results$Group, c("F", "F", "M", "M"))
  expect_equal(res$results$Occasion, c("T1", "T2", "T1", "T2"))
  expect_equal(res$results$Label, c("T1: F", "T2: F", "T1: M", "T2: M"))
  # per-cell estimates equal the single-scales grouped run, occasion by
  # occasion (complete data)
  for (j in 1:2) {
    set.seed(2)
    ref <- ssm_analyze(data,
      scales = occ_names(j), grouping = "Gender", boots = 10
    )
    got <- res$results[res$results$Occasion == paste0("T", j), ]
    expect_equal(got$e_est, ref$results$e_est, tolerance = 1e-12)
    expect_equal(got$d_est, ref$results$d_est, tolerance = 1e-12)
  }
})

test_that("scores table has one profile row per group x occasion", {
  data <- make_occ_data(n = 80)
  set.seed(1)
  res <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
    grouping = "Gender", boots = 10
  )
  expect_equal(nrow(res$scores), 4)
  expect_equal(res$scores$Occasion, c("T1", "T2", "T1", "T2"))
  # scale columns are the validated stems
  expect_true(all(c("PA", "BC", "NO") %in% names(res$scores)))
})

test_that("listwise deletion across waves is messaged with the count", {
  data <- make_occ_data(n = 50)
  data[1, "PA_2"] <- NA # person 1 missing only occasion 2
  data[2, "NO_1"] <- NA # person 2 missing only occasion 1
  set.seed(1)
  expect_message(
    res <- ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
      boots = 10
    ),
    "2 person"
  )
  # a person missing any occasion is dropped from all occasions
  set.seed(1)
  ref <- ssm_analyze(data[-c(1, 2), ],
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)), boots = 10
  )
  expect_equal(res$results$e_est, ref$results$e_est, tolerance = 1e-12)
})
