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
