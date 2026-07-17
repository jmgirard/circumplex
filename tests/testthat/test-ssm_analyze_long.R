# Long-format occasions wrapper ssm_analyze_long() (M28; binding spec
# devel/longitudinal-ssm-spec.md sec. 1.1). The wrapper only reshapes long to
# wide and delegates to ssm_analyze(occasions=), so the decisive oracle is the
# round-trip invariant: it must reproduce the equivalent wide-format call
# exactly (AC1). Fixtures are generated from named seeds (no committed data).

SC8 <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")

# Long-format generator: n persons x length(occ) occasions, p = 8 octant
# scales, a shared person effect (`rho`) for within-person dependence, and a
# per-occasion `shift` so occasions genuinely differ. Rows are stacked in `occ`
# order, so first-appearance order == `occ` (exercised by the T10/T2 test).
make_long_data <- function(n = 40, occ = c("T1", "T2"), seed = 321,
                           rho = 0.6, shift = 0.5, group = TRUE,
                           scales = SC8) {
  set.seed(seed)
  p <- length(scales)
  person <- matrix(stats::rnorm(n * p), n, p)
  gender <- factor(rep(c("F", "M"), length.out = n))
  parts <- lapply(seq_along(occ), function(j) {
    block <- sqrt(rho) * person +
      sqrt(1 - rho) * matrix(stats::rnorm(n * p), n, p) + 2 + (j - 1) * shift
    d <- as.data.frame(block)
    names(d) <- scales
    d$id <- seq_len(n)
    d$occasion <- occ[[j]]
    if (group) d$Gender <- gender
    d
  })
  do.call(rbind, parts)
}

# Independent wide construction (mirrors the wrapper's person ordering) so the
# equivalence test compares against a hand-built wide call, not the wrapper's
# own reshape.
to_wide <- function(long, occ_order, scales = SC8, group = TRUE) {
  ids <- unique(long$id)
  blocks <- lapply(occ_order, function(o) {
    sub <- long[long$occasion == o, , drop = FALSE]
    m <- sub[match(ids, sub$id), scales, drop = FALSE]
    names(m) <- paste0(scales, "_", o)
    m
  })
  w <- do.call(cbind, blocks)
  if (group) w$Gender <- long$Gender[match(ids, long$id)]
  w
}

occasions_list <- function(occ_order, scales = SC8) {
  stats::setNames(lapply(occ_order, function(o) paste0(scales, "_", o)), occ_order)
}

# AC1 -- round-trip equivalence with the wide occasions path ---------------

test_that("long wrapper reproduces the equivalent wide occasions call (grouped)", {
  long <- make_long_data(occ = c("T1", "T2"))
  wide <- to_wide(long, c("T1", "T2"))
  occ <- occasions_list(c("T1", "T2"))

  set.seed(9)
  a <- suppressMessages(ssm_analyze_long(
    long, scales = SC8, id = "id", occasion = "occasion",
    grouping = "Gender", boots = 300
  ))
  set.seed(9)
  b <- suppressMessages(ssm_analyze(
    wide, occasions = occ, grouping = "Gender", boots = 300
  ))
  expect_equal(a$results, b$results)
})

test_that("long wrapper reproduces the wide paired contrast (single group)", {
  long <- make_long_data(occ = c("T1", "T2"), group = FALSE)
  wide <- to_wide(long, c("T1", "T2"), group = FALSE)
  occ <- occasions_list(c("T1", "T2"))

  set.seed(11)
  a <- suppressMessages(ssm_analyze_long(
    long, scales = SC8, id = "id", occasion = "occasion",
    contrast = TRUE, boots = 300
  ))
  set.seed(11)
  b <- suppressMessages(ssm_analyze(
    wide, occasions = occ, contrast = TRUE, boots = 300
  ))
  expect_equal(a$results, b$results)
})

test_that("long wrapper reproduces the wide Monte Carlo call", {
  long <- make_long_data(occ = c("T1", "T2"), group = FALSE)
  wide <- to_wide(long, c("T1", "T2"), group = FALSE)
  occ <- occasions_list(c("T1", "T2"))

  set.seed(13)
  a <- suppressMessages(ssm_analyze_long(
    long, scales = SC8, id = "id", occasion = "occasion",
    method = "montecarlo", boots = 300
  ))
  set.seed(13)
  b <- suppressMessages(ssm_analyze(
    wide, occasions = occ, method = "montecarlo", boots = 300
  ))
  expect_equal(a$results, b$results)
})

test_that("numeric scale indexes select the same columns as names", {
  long <- make_long_data(occ = c("T1", "T2"), group = FALSE)
  set.seed(17)
  by_name <- suppressMessages(ssm_analyze_long(
    long, scales = SC8, id = "id", occasion = "occasion", boots = 200
  ))
  set.seed(17)
  by_index <- suppressMessages(ssm_analyze_long(
    long, scales = match(SC8, names(long)), id = "id", occasion = "occasion",
    boots = 200
  ))
  expect_equal(by_name$results, by_index$results)
})

# AC2 -- occasion order is temporal, never alphabetical ---------------------

test_that("occasion order follows first appearance, not alphabetical (T10/T2)", {
  # Rows stacked T2 then T10, so the temporal (as-supplied) order is T2, T10;
  # alphabetical sorting would flip it to T10, T2 and reverse the contrast.
  long <- make_long_data(occ = c("T2", "T10"), group = FALSE)
  wide <- to_wide(long, c("T2", "T10"), group = FALSE)

  set.seed(21)
  a <- suppressMessages(ssm_analyze_long(
    long, scales = SC8, id = "id", occasion = "occasion",
    contrast = TRUE, boots = 300
  ))
  set.seed(21)
  temporal <- suppressMessages(ssm_analyze(
    wide, occasions = occasions_list(c("T2", "T10")), contrast = TRUE, boots = 300
  ))
  set.seed(21)
  alphabetical <- suppressMessages(ssm_analyze(
    wide, occasions = occasions_list(c("T10", "T2")), contrast = TRUE, boots = 300
  ))

  # matches the temporal call, and is NOT the alphabetical one
  expect_equal(a$results, temporal$results)
  expect_false(isTRUE(all.equal(a$results, alphabetical$results)))
  # profile-row occasion labels appear in supplied order
  occ_col <- a$results$Occasion[!is.na(a$results$Occasion)]
  expect_equal(occ_col[!duplicated(occ_col)][1:2], c("T2", "T10"))
})

test_that("a factor occasion column uses its levels for order", {
  long <- make_long_data(occ = c("T2", "T10"), group = FALSE)
  # Rows are T2-then-T10, but the factor levels declare T10 before T2.
  long$occasion <- factor(long$occasion, levels = c("T10", "T2"))
  wide <- to_wide(long, c("T10", "T2"), group = FALSE)

  set.seed(23)
  a <- suppressMessages(ssm_analyze_long(
    long, scales = SC8, id = "id", occasion = "occasion", boots = 200
  ))
  set.seed(23)
  b <- suppressMessages(ssm_analyze(
    wide, occasions = occasions_list(c("T10", "T2")), boots = 200
  ))
  expect_equal(a$results, b$results)
})

# AC3 -- input validation ---------------------------------------------------

test_that("duplicate (id, occasion) rows error informatively", {
  long <- make_long_data(occ = c("T1", "T2"), group = FALSE)
  dup <- rbind(long, long[1, , drop = FALSE]) # person 1 at T1 twice
  expect_error(
    ssm_analyze_long(dup, scales = SC8, id = "id", occasion = "occasion"),
    "Duplicate rows|at most once"
  )
})

test_that("fewer than two occasions errors", {
  long <- make_long_data(occ = c("T1", "T2"), group = FALSE)
  one <- long[long$occasion == "T1", , drop = FALSE]
  expect_error(
    ssm_analyze_long(one, scales = SC8, id = "id", occasion = "occasion"),
    "at least two occasions"
  )
})

test_that("time-varying grouping errors informatively", {
  long <- make_long_data(occ = c("T1", "T2"), group = TRUE)
  # flip person 1's group at T2 only
  flip <- long$id == 1 & long$occasion == "T2"
  long$Gender[flip] <- ifelse(long$Gender[flip] == "F", "M", "F")
  expect_error(
    ssm_analyze_long(
      long, scales = SC8, id = "id", occasion = "occasion", grouping = "Gender"
    ),
    "time-invariant"
  )
})

test_that("unknown column references error", {
  long <- make_long_data(occ = c("T1", "T2"), group = FALSE)
  expect_error(
    ssm_analyze_long(long, scales = SC8, id = "nope", occasion = "occasion"),
    "name columns"
  )
})

test_that("missing id/occasion rows are dropped with a message", {
  long <- make_long_data(occ = c("T1", "T2"), group = FALSE)
  long$occasion[1] <- NA
  expect_message(
    suppressWarnings(try(
      ssm_analyze_long(
        long, scales = SC8, id = "id", occasion = "occasion", boots = 100
      ),
      silent = TRUE
    )),
    "removed due to missing"
  )
})
