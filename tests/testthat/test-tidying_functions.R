test_that("ipsatize works", {
  data("raw_iipsc")
  items <- 1:32
  datin <- ipsatize(raw_iipsc, items = items, append = FALSE)
  datia <- ipsatize(raw_iipsc, items = items, append = TRUE)
  expect_equal(ncol(datin), length(items))
  expect_equal(ncol(datia), ncol(raw_iipsc) + length(items))
  expect_equal(datin[[1]][1], -1.0)
  expect_equal(datin[[2]][7], -0.5)
})


test_that("score works", {
  set.seed(12345)
  old <- data.frame(
    matrix(sample(0:4, size = 32 * 5, replace = TRUE), nrow = 5, ncol = 32)
  )
  new <- score(old, items = 1:32, instrument = iipsc)
  new2 <- score(old, items = 1:32, instrument = iipsc, append = FALSE)
    
  expect_equal(new$PA, c(0.5, 1.25, 2, 0.75, 3.5))
  expect_equal(new$BC, c(1.75, 0.75, 1.5, 1.75, 1.75))
  expect_equal(new$DE, c(1.75, 2.25, 1.75, 3, 3))
  expect_equal(new$FG, c(2.5, 1.75, 3.5, 1.5, 1.75))
  expect_equal(new$HI, c(2.25, 2.25, 3, 2, 1.25))
  expect_equal(new$JK, c(1.5, 2.5, 1.75, 2.25, 1.75))
  expect_equal(new$LM, c(2, 2.5, 1.5, 2.75, 1.5))
  expect_equal(new$NO, c(1.75, 2, 1.75, 2.25, 2.5))
  expect_error(score(old, 1:30, iipsc))
  expect_equal(ncol(new), ncol(new2) + ncol(old))
})


test_that("norm_standardize works", {
  set.seed(12345)
  old <- data.frame(
    matrix(runif(8 * 5, min = 0, max = 4), nrow = 5, ncol = 8)
  )
  new <- norm_standardize(
    old,
    scales = 1:8,
    instrument = iipsc,
    sample = 1,
    quiet = TRUE
  )
  new2 <- norm_standardize(
    old,
    scales = 1:8,
    instrument = iipsc,
    sample = 1,
    append = FALSE,
    quiet = TRUE
  )
  expect_equal(round(new$X1_z, 4), c(3.2176, 4.1562, 3.4605, 4.2189, 1.6150))
  expect_equal(round(new$X2_z, 4), c(-0.1841, 0.7361, 1.8035, 3.07, 4.5891))
  expect_equal(round(new$X3_z, 4), c(-0.8911, -0.3398, 2.3892, -1.0473, 0.7776))
  expect_equal(round(new$X4_z, 4), c(0.8469, 0.5331, 0.5936, -0.35, 2.912))
  expect_equal(round(new$X5_z, 4), c(0.4316, -0.1235, 2.6685, 1.5409, 1.2658))
  expect_equal(round(new$X6_z, 4), c(0.2045, 1.653, 0.9281, -0.562, 0.649))
  expect_equal(round(new$X7_z, 4), c(2.0691, -1.7467, -0.8656, 1.5301, 0.0187))
  expect_equal(round(new$X8_z, 4), c(0.5269, 3.0627, 3.2395, 1.8059, -0.6111))
  expect_error(norm_standardize(
    old, scales = 1:5, instrument = iipsc, sample = 1
  ))
  expect_equal(ncol(new), ncol(new2) + ncol(old))
})

test_that("norm_standardize matches 0 and 360 degrees as the same angle", {
  set.seed(12345)
  old <- data.frame(
    matrix(runif(8 * 5, min = 0, max = 4), nrow = 5, ncol = 8)
  )
  # LM stored as 360 in the norms; passing 0 must give identical results
  with360 <- norm_standardize(
    old, scales = 1:8, angles = octants(), instrument = iipsc, sample = 1,
    append = FALSE, quiet = TRUE
  )
  with0 <- norm_standardize(
    old, scales = 1:8, angles = c(90, 135, 180, 225, 270, 315, 0, 45),
    instrument = iipsc, sample = 1, append = FALSE, quiet = TRUE
  )
  expect_equal(with0, with360)
})

test_that("norm_standardize errors clearly on an unmatched angle", {
  set.seed(12345)
  old <- data.frame(
    matrix(runif(8 * 5, min = 0, max = 4), nrow = 5, ncol = 8)
  )
  expect_error(
    norm_standardize(
      old, scales = 1:8,
      angles = c(90, 135, 180, 225, 270, 315, 360, 100),
      instrument = iipsc, sample = 1
    ),
    "100"
  )
})

test_that("norm_standardize errors clearly on duplicate-angle norms", {
  set.seed(12345)
  old <- data.frame(
    matrix(runif(8 * 5, min = 0, max = 4), nrow = 5, ncol = 8)
  )
  # Corrupt the norms so angle 90 appears twice (and 45 not at all)
  dup <- iipsc
  norms <- dup$Norms[[1]]
  norms$Angle[norms$Sample == 1 & norms$Angle == 45] <- 90
  dup$Norms[[1]] <- norms
  expect_error(
    norm_standardize(old, scales = 1:8, instrument = dup, sample = 1),
    "[Mm]ultiple|[Dd]uplicate"
  )
})


test_that("tidying functions accept matrix input", {
  ref <- self_standardize(aw2009, scales = 1:8, append = FALSE)
  out <- self_standardize(as.matrix(aw2009), scales = 1:8, append = FALSE)
  expect_equal(out, ref)

  ips_ref <- ipsatize(raw_iipsc, items = 1:32, append = FALSE)
  ips_out <- ipsatize(as.matrix(raw_iipsc), items = 1:32, append = FALSE)
  expect_equal(ips_out, ips_ref)
})

test_that("self_standardize works", {
  old <- aw2009
  new <- self_standardize(
    old,
    scales = 1:8
  )
  new2 <- self_standardize(
    old,
    scales = 1:8,
    append = FALSE
  )
  expect_equal(round(new$PA_z, 4), c(-1.6857, 0.8705, 0.6172, 0.1105, 0.0875))
  expect_equal(round(new$BC_z, 4), c(-0.8356, -0.8356, -0.1393, 0.2178, 1.5927))
  expect_error(self_standardize(aw2009, 2:9))
  expect_equal(ncol(new), ncol(new2) + ncol(old))
})


# The shipped norms of a multi-sample instrument are keyed by `Sample`, and
# norm_standardize() subsets on that key. A miscoded key is invisible to a
# row-count check -- the frame still has 8 rows per sample -- and shows up only
# as which octants land in each subset, so these two tests assert the key's
# CONTENT rather than the frame's shape. They sweep every shipped instrument
# rather than naming the one that was broken, because the defect is a way of
# writing the column and not a fact about one instrument.
# The enumeration lives in helper-norms.R as shipped_instruments(); this file
# previously carried a second, identical copy of it.

test_that("each shipped norm sample keys every scale exactly once", {
  for (nm in shipped_instruments()) {
    obj <- get(nm)
    norms <- obj$Norms[[1]]
    key <- if ("Scale" %in% names(norms)) "Scale" else "Abbrev"
    for (s in obj$Norms[[2]]$Sample) {
      rows <- norms[norms$Sample == s, ]
      expect_equal(nrow(rows), nrow(obj$Scales),
                   info = paste(nm, "sample", s))
      expect_setequal(as.character(rows[[key]]), as.character(obj$Scales$Abbrev))
      expect_equal(anyDuplicated(rows$Angle %% 360), 0L,
                   info = paste(nm, "sample", s, "has a repeated angle"))
    }
  }
})

test_that("norm_standardize runs on every shipped instrument and sample", {
  # End-to-end rather than structural only: a key that survives the shape
  # assertions above but still mixes samples would produce numbers here.
  #
  # A sample whose means fall outside its instrument's response range is
  # refused rather than standardized (see test-norms-anchor-range.R for the
  # invariant and the one shipped violation). The expectation below is derived
  # from that same predicate rather than hand-listing the exception, so it
  # stays correct when the CAIS adult sample is corrected or withdrawn, and
  # when a new instrument is added.
  probe <- as.data.frame(matrix(2, nrow = 2, ncol = 8))
  for (nm in shipped_instruments()) {
    obj <- get(nm)
    names(probe) <- obj$Scales$Abbrev
    for (s in obj$Norms[[2]]$Sample) {
      key <- obj$Norms[[1]]
      m <- key$M[key$Sample == s]
      in_range <- all(
        m >= min(obj$Anchors$Value) & m <= max(obj$Anchors$Value),
        na.rm = TRUE
      )
      standardize_it <- function() {
        norm_standardize(probe, scales = names(probe),
                         angles = obj$Scales$Angle, instrument = obj,
                         sample = s, append = FALSE, quiet = TRUE)
      }
      if (in_range) {
        expect_no_error(standardize_it())
      } else {
        expect_error(standardize_it(), "response range")
      }
    }
  }

  # And pin the values for one multi-sample instrument against its published
  # source, so the sweep above cannot pass on norms that run but are wrong.
  # iei sample 1 is Horner, Locke & Hulsey's Study 1 (N = 1223): PA M = 2.00,
  # SD = 0.71 and BC M = 1.21, SD = 0.61, so an all-2 probe gives PA_z = 0 and
  # BC_z = (2 - 1.21) / 0.61.
  names(probe) <- iei$Scales$Abbrev
  z1 <- norm_standardize(probe, scales = names(probe),
                         angles = iei$Scales$Angle, instrument = iei,
                         sample = 1, append = FALSE, quiet = TRUE)
  expect_equal(z1$PA_z[[1]], 0)
  expect_equal(z1$BC_z[[1]], (2 - 1.21) / 0.61)
})
