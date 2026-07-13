test_that("S3 degree functions work as expected", {
  x <- c(0, 90, 180, 360)
  y <- c(0, pi / 2, pi, pi * 2)

  x1 <- as_degree(x)
  expect_s3_class(x1, "circumplex_degree")
  expect_equal(as.numeric(x1), x)

  x2 <- as_degree(as_degree(x))
  expect_s3_class(x2, "circumplex_degree")
  expect_equal(as.numeric(x2), x)

  x3 <- as_radian(as_degree(x))
  expect_s3_class(x3, "circumplex_radian")
  expect_equal(as.numeric(x3), y)

  y1 <- as_radian(y)
  expect_s3_class(y1, "circumplex_radian")
  expect_equal(as.numeric(y1), y)

  y2 <- as_radian(as_radian(y))
  expect_s3_class(y2, "circumplex_radian")
  expect_equal(as.numeric(y2), y)

  y3 <- as_degree(as_radian(y))
  expect_s3_class(y3, "circumplex_degree")
  expect_equal(as.numeric(y3), x)
})

test_that("new_contrast_radian() is byte-identical to the former inline tag", {
  x <- c(-3, 0, 1.5, NA_real_)

  cr <- new_contrast_radian(x)
  expect_s3_class(cr, "circumplex_contrast_radian")
  expect_equal(as.numeric(cr), x)

  # The constructor must reproduce the exact object the two call sites used to
  # build inline (ssm_bootstrap.R / ssm_ci_accuracy.R), so the DRY extraction
  # is provably behaviour-preserving.
  inline <- structure(x, class = c("circumplex_contrast_radian", "numeric"))
  expect_identical(cr, inline)

  # And its plain-radian sibling likewise dispatches to the standard method.
  r <- new_radian(x)
  expect_s3_class(r, "circumplex_radian")
  expect_identical(r, structure(x, class = c("circumplex_radian", "numeric")))
})

test_that("The ssm display methods is working", {
  skip_on_cran()

  data("aw2009")
  res <- ssm_analyze(aw2009, scales = 1:8)
  expect_output(print(res), "# Profile \\[All\\]:")
  expect_output(summary(res), "Statistical Basis:\\t Mean Scores")
  expect_output(summary(res), "Bootstrap Resamples:\\t 2000")
  expect_output(summary(res), "Confidence Level:\\t 0\\.95")
  expect_output(summary(res), "Listwise Deletion:\\t TRUE")
  expect_output(summary(res), "Scale Displacements:\\t 90 135 180 225 270 315 360 45")

  data("jz2017")
  res <- ssm_analyze(jz2017, scales = 2:9, grouping = "Gender")
  expect_output(print(res), "# Profile \\[Female\\]:")
  expect_output(print(res), "# Profile \\[Male\\]:")

  res <- ssm_analyze(
    jz2017, 
    scales = 2:9,
    grouping = "Gender",
    contrast = TRUE
  )
  expect_output(print(res), "# Profile \\[Female\\]:")
  expect_output(print(res), "# Profile \\[Male\\]:")
  expect_output(print(res), "# Contrast \\[Male - Female\\]:")
  expect_output(print(res), "\u0394 Elevation")

  res <- ssm_analyze(
    jz2017, 
    scales = 2:9,
    measures = "PARPD",
    grouping = "Gender", 
    contrast = TRUE
  )
  expect_output(print(res), "# Contrast \\[PARPD: Male - Female\\]:")
  expect_output(summary(res), "Statistical Basis:\\t Correlation Scores")
})

test_that("print notes when a profile is not interpretable", {
  skip_on_cran()
  data("jz2017")

  # Low-fit profile: note advising to interpret only elevation
  set.seed(1)
  low <- suppressWarnings(
    ssm_analyze(jz2017, scales = 2:9, measures = "OCPD", boots = 200)
  )
  out_low <- capture.output(print(low))
  expect_true(any(grepl("only the elevation", out_low, ignore.case = TRUE)))

  # Healthy profile (good fit, amplitude well above zero): no note
  data("aw2009")
  set.seed(1)
  good <- ssm_analyze(aw2009, scales = 1:8, boots = 200)
  out_good <- capture.output(print(good))
  expect_false(any(grepl("not interpretable|only the elevation", out_good,
                         ignore.case = TRUE)))

  # Flat (degenerate) profile: amplitude CI includes zero -> displacement note
  flat <- as.data.frame(matrix(1, nrow = 20, ncol = 8))
  colnames(flat) <- PANO()
  set.seed(1)
  deg <- suppressWarnings(ssm_analyze(flat, scales = 1:8, boots = 50))
  out_deg <- capture.output(print(deg))
  expect_true(any(grepl("displacement is not interpretable", out_deg,
                        ignore.case = TRUE)))

  # summary() inherits the note (it delegates to print)
  out_sum <- capture.output(summary(low))
  expect_true(any(grepl("only the elevation", out_sum, ignore.case = TRUE)))
})

test_that("interpretation notes are not applied to the contrast row", {
  skip_on_cran()
  data("jz2017")
  set.seed(1)
  res <- suppressWarnings(ssm_analyze(
    jz2017, scales = 2:9, measures = c("OCPD", "NARPD"), contrast = TRUE,
    boots = 200
  ))
  out <- capture.output(print(res))
  # Find the contrast block and confirm no interpretation note appears in it
  contrast_start <- grep("# Contrast", out)
  contrast_block <- out[contrast_start:length(out)]
  expect_false(any(grepl("not interpretable|only the elevation", contrast_block,
                         ignore.case = TRUE)))
})

test_that("ssm_certified() applies the scale-free lower-bound ratio rule (D-007)", {
  # r = a_lci / (a_uci - a_lci); certify iff is.finite(r) & r >= k (k = 0.35).
  # Pure function of the amplitude CI *pair* -- no `digits`, no `a_est`.
  expect_false("digits" %in% names(formals(ssm_certified)))
  expect_true(all(c("a_lci", "a_uci") %in% names(formals(ssm_certified))))

  # Certify above the threshold, refuse below it, include the boundary (>=).
  expect_true(ssm_certified(0.5, 1.0))          # r = 1.00
  expect_true(ssm_certified(0.35, 1.35))        # r = 0.35 exactly -> certified
  expect_false(ssm_certified(0.1, 1.0))         # r = 0.111 -> refused

  # Scale-free: multiplying the amplitude metric by any positive constant
  # leaves the verdict unchanged (numerator and denominator co-scale).
  expect_identical(ssm_certified(0.5, 1.0), ssm_certified(500, 1000))
  expect_identical(ssm_certified(0.1, 1.0), ssm_certified(100, 1000))

  # Vectorized.
  expect_equal(
    ssm_certified(c(0.5, 0.1), c(1.0, 1.0)),
    c(TRUE, FALSE)
  )

  # Edge contract (RR03 Q6): NA lower bound and degenerate zero-width CIs
  # fail closed (a guardrail's failure mode is silence, not endorsement).
  expect_false(ssm_certified(NA_real_, 1.0))    # flat / zero-variance profile
  expect_false(ssm_certified(0.5, 0.5))         # width 0 -> Inf -> not finite
  expect_false(ssm_certified(0.0, 0.0))         # width 0 at 0 -> NaN -> not finite

  # Equivalent closed form a_lci >= (k/(1+k)) * a_uci = 0.259 * a_uci.
  expect_equal(ssm_certified(0.26, 1.0), 0.26 >= 0.35 / 1.35 * 1.0)
})

test_that("displacement certification is print-independent and scale-free (AC1, AC2)", {
  skip_on_cran()
  data("jz2017")
  amp_note <- function(res, ...) {
    any(grepl("displacement is not interpretable",
              capture.output(print(res, ...)), ignore.case = TRUE))
  }

  # AC1 print-independence: the certification verdict does not move with the
  # display `digits`. (Under the superseded rule OCPD's note flipped between
  # digits = 2 and 3.)
  set.seed(1)
  near0 <- suppressWarnings(
    ssm_analyze(jz2017, scales = 2:9, measures = "OCPD", boots = 200)
  )
  notes <- vapply(c(2, 3, 5), function(d) amp_note(near0, digits = d), logical(1))
  expect_true(all(notes == notes[[1]]))

  # AC2 scale-invariance: rescaling the score metric by a positive constant
  # leaves the verdict unchanged. aw2009 (well-differentiated) stays certified
  # at raw scale and at x1000.
  data("aw2009")
  set.seed(1); base <- ssm_analyze(aw2009, scales = 1:8, boots = 200)
  big <- aw2009; big[, 1:8] <- big[, 1:8] * 1000
  set.seed(1); scaled <- ssm_analyze(big, scales = 1:8, boots = 200)
  expect_identical(amp_note(base), amp_note(scaled))
})

test_that("near-zero amplitude now flips certified -> not interpretable (D-007 regression)", {
  skip_on_cran()
  data("jz2017"); data("aw2009")
  amp_note <- function(res) {
    any(grepl("displacement is not interpretable",
              capture.output(print(res)), ignore.case = TRUE))
  }

  # OCPD: a_lci/width ~ 0.06 << 0.35 -> the displacement is NOT interpretable.
  # The superseded round(a_lci, 3) > 0 rule certified it (a_lci ~ 0.003).
  set.seed(1)
  near0 <- suppressWarnings(
    ssm_analyze(jz2017, scales = 2:9, measures = "OCPD", boots = 200)
  )
  expect_true(amp_note(near0))

  # aw2009: a_lci/width ~ 1.07 >= 0.35 -> stays certified (no amplitude note).
  set.seed(1)
  healthy <- ssm_analyze(aw2009, scales = 1:8, boots = 200)
  expect_false(amp_note(healthy))
})

test_that("certification is angle-blind at the 0/360 pole (D-007)", {
  skip_on_cran()
  data("aw2009")
  amp_note <- function(res) {
    any(grepl("displacement is not interpretable",
              capture.output(print(res)), ignore.case = TRUE))
  }
  # Cyclically rotating the scale->angle assignment rotates the profile's peak
  # (its displacement) by whole octants while leaving amplitude, its CI, and
  # model fit exactly invariant per resample. The rule reads only the amplitude
  # CI, so certification must be identical regardless of where the peak sits --
  # including on the 0/360 pole.
  set.seed(1); plain <- ssm_analyze(aw2009, scales = 1:8, boots = 200)
  rot <- aw2009[, c(8, 1:7)]
  set.seed(1); rotated <- ssm_analyze(rot, scales = 1:8, boots = 200)
  expect_identical(plain$results$a_lci, rotated$results$a_lci)
  expect_identical(amp_note(plain), amp_note(rotated))
})

test_that("unit classes are working", {
  expect_snapshot(octants())
  expect_snapshot(as_radian(octants()))
})
