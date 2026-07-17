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

# Paired occasion contrasts (spec sec. 1.2/2.1) --------------------------------

# Wide data with known within-person change: occasion 1 is a cosine profile
# peaked at d1 with elevation e1; occasion 2 is peaked at d2 = d1 + dd with
# elevation e1 + de. Shared person effect induces within-person dependence.
make_contrast_data <- function(n = 200, d1 = 90, dd = 45, e1 = 2, de = 0.5,
                               amp = 1.5, seed = 42) {
  set.seed(seed)
  scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  ang <- octants() * pi / 180
  person <- rnorm(n, 0, 0.3)
  block <- function(d, e) {
    profile <- e + amp * cos(ang - d * pi / 180)
    t(vapply(seq_len(n), function(i) {
      profile + person[i] + rnorm(8, 0, 0.2)
    }, numeric(8)))
  }
  b1 <- block(d1, e1)
  b2 <- block(d1 + dd, e1 + de)
  colnames(b1) <- paste0(scales, "_1")
  colnames(b2) <- paste0(scales, "_2")
  cbind(as.data.frame(b1), as.data.frame(b2))
}

test_that("occasion contrast is second listed minus first listed", {
  data <- make_contrast_data(de = 0.5, dd = 45)
  set.seed(3)
  res <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
    contrast = TRUE, boots = 100
  )
  expect_equal(nrow(res$results), 3)
  expect_equal(res$results$Occasion[[3]], "T2 - T1")
  expect_equal(res$results$Label[[3]], "T2 - T1")
  # elevation contrast equals the hand-computed occasion-2-minus-occasion-1
  # grand mean difference exactly (linear statistic, complete data)
  exp_de <- mean(as.matrix(data[occ_names(2)])) -
    mean(as.matrix(data[occ_names(1)]))
  expect_equal(res$results$e_est[[3]], exp_de, tolerance = 1e-12)
  expect_gt(res$results$e_est[[3]], 0) # sign: T2 is higher by construction
  # displacement contrast is the signed angular distance d2 - d1 (~ +45)
  expect_equal(res$results$d_est[[3]], 45, tolerance = 5)
  # and matches angle_dist of the two profile rows exactly
  exp_dd <- ((res$results$d_est[[2]] - res$results$d_est[[1]] + 180) %%
               360) - 180
  expect_equal(res$results$d_est[[3]], exp_dd, tolerance = 1e-9)
})

test_that("contrast order is list order, never alphabetical (T10 vs T2)", {
  data <- make_contrast_data(de = 0.5)
  names(data) <- c(paste0(occ_names(""), "T2"), paste0(occ_names(""), "T10"))
  # supplied order: T2 first, T10 second; alphabetical sorting would put
  # "T10" before "T2" and silently flip the contrast sign
  set.seed(3)
  res <- ssm_analyze(data,
    occasions = list(
      T2 = paste0(occ_names(""), "T2"),
      T10 = paste0(occ_names(""), "T10")
    ),
    contrast = TRUE, boots = 10
  )
  expect_equal(res$results$Occasion, c("T2", "T10", "T10 - T2"))
  # T10 (the later-listed occasion) has the higher elevation by construction
  expect_gt(res$results$e_est[[3]], 0)
})

test_that("contrast composition rules for occasions are enforced", {
  data3 <- make_occ_data(k = 3)
  expect_error(
    ssm_analyze(data3,
      occasions = list(T1 = occ_names(1), T2 = occ_names(2),
                       T3 = occ_names(3)),
      contrast = TRUE
    ),
    "2 occasions"
  )
  data2 <- make_occ_data()
  expect_error(
    ssm_analyze(data2,
      occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
      grouping = "Gender", contrast = TRUE
    ),
    "single group"
  )
})

# Boundary battery (CLAUDE.md; spec sec. 2.3 item 4) --------------------------

test_that("occasion profile CI straddling the 0/360 pole wraps, both engines", {
  # occasion 1 peaks exactly on the pole; with this n and noise the CI is
  # several degrees wide on each side, so it must straddle and wrap
  data <- make_contrast_data(n = 40, d1 = 0, dd = 90, seed = 7)
  # inflate noise for a wide displacement CI
  set.seed(8)
  noisy <- data + matrix(rnorm(prod(dim(data)), 0, 0.6), nrow(data))
  for (method in c("bootstrap", "montecarlo")) {
    set.seed(9)
    res <- ssm_analyze(noisy,
      occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
      boots = 500, method = method
    )
    d_est <- res$results$d_est[[1]]
    # estimate reports near the pole on either label (D-003/M20 convention)
    expect_true(d_est < 15 || d_est > 345)
    # a wrapped CI reports lci > uci (e.g., 350 to 8)
    expect_gt(res$results$d_lci[[1]], res$results$d_uci[[1]])
    # and the occasion-2 profile (90 degrees) is unaffected
    expect_equal(res$results$d_est[[2]], 90, tolerance = 10)
  }
})

test_that("occasion contrast near +/-180 keeps sign and branch, both engines", {
  for (dd in c(175, -175)) {
    data <- make_contrast_data(n = 200, d1 = 90, dd = dd, seed = 11)
    for (method in c("bootstrap", "montecarlo")) {
      set.seed(12)
      res <- ssm_analyze(data,
        occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
        contrast = TRUE, boots = 300, method = method
      )
      d_con <- res$results$d_est[[3]]
      expect_equal(d_con, dd, tolerance = 5)
      # CI is on the same branch as its estimate and covers the truth
      # (endpoints may legitimately exceed +/-180 near the boundary)
      expect_true(res$results$d_lci[[3]] < dd && res$results$d_uci[[3]] > dd)
      expect_lt(res$results$d_uci[[3]] - res$results$d_lci[[3]], 90)
    }
  }
})

test_that("a flat occasion reports NA displacement with a warning, both engines", {
  data <- make_contrast_data(n = 60, seed = 13)
  # occasion 2 becomes exactly flat: constant score for every person and scale
  data[occ_names(2)] <- 2
  for (method in c("bootstrap", "montecarlo")) {
    set.seed(14)
    w <- testthat::capture_warnings(
      res <- ssm_analyze(data,
        occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
        contrast = TRUE, boots = 50, method = method
      )
    )
    expect_true(any(grepl("flat or.*zero", w)))
    # the flat occasion's displacement (and fit) are NA; the healthy
    # occasion is untouched; the contrast inherits NA displacement
    expect_true(is.na(res$results$d_est[[2]]))
    expect_true(is.na(res$results$fit_est[[2]]))
    expect_false(is.na(res$results$d_est[[1]]))
    expect_true(is.na(res$results$d_est[[3]]))
    # linear contrast parameters stay defined
    expect_false(is.na(res$results$e_est[[3]]))
  }
})

test_that("k = 3 occasions agree across engines on point estimates", {
  data <- make_occ_data(n = 60, k = 3, seed = 15)
  occ <- list(T1 = occ_names(1), T2 = occ_names(2), T3 = occ_names(3))
  set.seed(16)
  res_bs <- ssm_analyze(data, occasions = occ, boots = 20)
  set.seed(16)
  res_mc <- ssm_analyze(data, occasions = occ, boots = 20,
                        method = "montecarlo")
  expect_equal(res_bs$results$d_est, res_mc$results$d_est, tolerance = 1e-12)
  expect_equal(res_bs$results$a_est, res_mc$results$a_est, tolerance = 1e-12)
})

test_that("occasion contrast runs through both engines consistently", {
  data <- make_contrast_data(n = 300)
  set.seed(4)
  res_bs <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
    contrast = TRUE, boots = 200
  )
  set.seed(4)
  res_mc <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
    contrast = TRUE, boots = 200, method = "montecarlo"
  )
  # identical point estimates (same closed-form transform of the same means)
  expect_equal(res_bs$results$d_est, res_mc$results$d_est, tolerance = 1e-12)
  expect_equal(res_bs$results$e_est, res_mc$results$e_est, tolerance = 1e-12)
  # both engines' contrast CIs cover the construction truths (de = 0.5,
  # dd = 45) and agree on sign
  for (res in list(res_bs, res_mc)) {
    expect_lt(res$results$e_lci[[3]], 0.5 + 0.2)
    expect_gt(res$results$e_uci[[3]], 0.5 - 0.2)
    expect_true(res$results$d_lci[[3]] < 45 && res$results$d_uci[[3]] > 45)
  }
})
