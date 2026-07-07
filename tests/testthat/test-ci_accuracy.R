# Tests for ssm_ci_accuracy() (M4/Z1 + Z2, spec devel/m4-ci-accuracy-spec.md).
# Z1 sections cover the sec. 3 core loop, the sec. 4.1 ladder construction,
# the c = 0 machinery pin, the RNG contract, and the engine-replay
# equivalences. Z2 sections cover the sec. 4-5 analysis layer: the sec. 10
# direction oracles, band-edge verdict classification, the degenerate-ladder
# margin rung, the guardrail false-certification measurement, the sec. 5.2
# verdict wording (and its wording bar), and the plot method.

deg2rad <- function(x) x * pi / 180

# ---- ladder construction (sec. 4.1 functional-targeted 3x3 solve) -----------

test_that("ladder correction hits the estimator functional exactly, any spacing (F3)", {
  angle_sets <- list(
    octants = as.numeric(octants()),
    unequal = c(0, 30, 80, 145, 190, 220, 305, 340)
  )
  for (nm in names(angle_sets)) {
    theta <- deg2rad(angle_sets[[nm]])
    set.seed(11)
    prof <- 1 + 2 * cos(theta - 1.1) + 0.4 * cos(2 * theta) + rnorm(8, 0, 0.2)
    t1 <- as.numeric(ssm_parameters_cpp(prof, theta))
    corr <- ssm_ci_ladder_correction(prof, theta)
    for (cc in c(1, 0.5, 0.25, 0)) {
      tc <- as.numeric(ssm_parameters_cpp(prof - (1 - cc) * corr, theta))
      expect_equal(tc[1], t1[1], tolerance = 1e-10)        # e on target
      expect_equal(tc[2], cc * t1[2], tolerance = 1e-10)   # x scales
      expect_equal(tc[3], cc * t1[3], tolerance = 1e-10)   # y scales
      expect_equal(tc[4], cc * t1[4], tolerance = 1e-10)   # a scales
      if (cc > 0) {
        expect_equal(tc[5], t1[5], tolerance = 1e-8)       # d invariant
      } else {
        expect_true(is.na(tc[5]))                          # a0 = 0 -> d0 NA
      }
    }
  }
})

test_that("ladder is refused for a singular functional matrix", {
  # All angles coincident: the {1, cos, sin} images are collinear
  theta <- rep(deg2rad(90), 8)
  expect_error(
    ssm_ci_ladder_correction(rnorm(8), theta),
    "singular"
  )
})

# ---- circular interval membership (sec. 3.4 angular membership) -------------

test_that("displacement coverage uses angular membership modulo 360", {
  r <- deg2rad
  # Profile-row interval straddling the 0/360 pole: [350, 10] wrapped
  lci <- r(350); uci <- r(10)
  expect_true(ssm_ci_d_cover(r(5), lci, uci)$cover)
  expect_true(ssm_ci_d_cover(r(355), lci, uci)$cover)
  expect_true(ssm_ci_d_cover(r(350), lci, uci)$cover)
  expect_true(ssm_ci_d_cover(r(10), lci, uci)$cover)
  # Truth at the pole itself, reported as either 0 or 360 (DESIGN G2)
  expect_true(ssm_ci_d_cover(r(0), lci, uci)$cover)
  expect_true(ssm_ci_d_cover(r(360), lci, uci)$cover)
  # Outside, with the shorter-angular-direction miss side
  out_hi <- ssm_ci_d_cover(r(20), lci, uci)
  expect_false(out_hi$cover)
  expect_equal(out_hi$side, 1)   # just above uci
  out_lo <- ssm_ci_d_cover(r(340), lci, uci)
  expect_false(out_lo$cover)
  expect_equal(out_lo$side, -1)  # just below lci
  # Width is the arc width
  expect_equal(ssm_ci_d_cover(r(5), lci, uci)$width, r(20), tolerance = 1e-12)
})

test_that("contrast membership works on branch-shifted intervals near +/-180", {
  r <- deg2rad
  # Branch-aligned contrast interval [170, 186] (endpoints exceed +180);
  # a truth reported at -178 (= 182) is geometrically inside
  lci <- r(170); uci <- r(186)
  expect_true(ssm_ci_d_cover(r(-178), lci, uci, contrast = TRUE)$cover)
  expect_true(ssm_ci_d_cover(r(178), lci, uci, contrast = TRUE)$cover)
  miss <- ssm_ci_d_cover(r(160), lci, uci, contrast = TRUE)
  expect_false(miss$cover)
  expect_equal(miss$side, -1)
  expect_equal(
    ssm_ci_d_cover(r(-178), lci, uci, contrast = TRUE)$width,
    r(16), tolerance = 1e-12
  )
})

# ---- lean interval assembly == ssm_replicate_intervals() ---------------------

test_that("lean interval assembly matches ssm_replicate_intervals()", {
  set.seed(5)
  boots <- 300
  make_params <- function(d_center) {
    e <- rnorm(boots, 1, 0.1)
    x <- rnorm(boots, 0.4, 0.1)
    y <- rnorm(boots, -0.2, 0.1)
    a <- sqrt(x^2 + y^2)
    d <- (d_center + rnorm(boots, 0, 0.2)) %% (2 * pi)
    fit <- runif(boots, 0.7, 1)
    cbind(e, x, y, a, d, fit)
  }
  m1 <- make_params(0.1)          # straddles the 0/2pi pole
  m2 <- make_params(pi + 0.05)
  # Degenerate replicates: NA displacement/fit in a few rows (conditional CIs)
  m1[c(3, 17), 5] <- NA
  m1[3, 6] <- NA
  t <- cbind(m1, m2, param_diff(m2, m1))
  t0 <- c(colMeans(m1[, 1:6], na.rm = TRUE), colMeans(m2, na.rm = TRUE),
          param_diff(colMeans(m2), colMeans(m1, na.rm = TRUE)))
  real <- suppressWarnings(
    ssm_replicate_intervals(t0, t, interval = 0.95, contrast = TRUE,
                            replicate_label = "test replicates")
  )
  lean <- ssm_ci_intervals_lean(t0, t, interval = 0.95, contrast = TRUE)

  pnames <- ssm_param_names()
  for (p in c("e", "x", "y", "a", "fit")) {
    j <- which(pnames == p)
    expect_equal(lean$est[, j], real[[paste0(p, "_est")]],
                 tolerance = 1e-12, ignore_attr = TRUE,
                 label = paste("est", p))
  }
  # Displacement estimates: real is degrees, lean stays radian
  expect_equal(lean$est[, which(pnames == "d")] * 180 / pi, real$d_est,
               tolerance = 1e-10, ignore_attr = TRUE)
  for (p in c("e", "x", "y", "a")) {
    j <- which(pnames == p)
    expect_equal(lean$lci[, j], real[[paste0(p, "_lci")]],
                 tolerance = 1e-12, ignore_attr = TRUE)
    expect_equal(lean$uci[, j], real[[paste0(p, "_uci")]],
                 tolerance = 1e-12, ignore_attr = TRUE)
  }
  # Displacement: real is degrees, lean stays radian
  j <- which(pnames == "d")
  expect_equal(lean$lci[, j] * 180 / pi, real$d_lci, tolerance = 1e-10,
               ignore_attr = TRUE)
  expect_equal(lean$uci[, j] * 180 / pi, real$d_uci, tolerance = 1e-10,
               ignore_attr = TRUE)
  # Degenerate accounting matches the engine's warning count
  expect_equal(lean$n_degenerate, 2)
})

# ---- weighted-moment bootstrap statistics ------------------------------------

test_that("multinomial-weight means and correlations equal expanded resamples", {
  set.seed(601)
  n <- 40
  X <- matrix(rnorm(n * 3, mean = 5), n, 3)  # nonzero means: centering matters
  Y <- matrix(rnorm(n * 2, mean = -2), n, 2)
  W <- stats::rmultinom(5, n, rep(1 / n, n))
  wm <- ssm_ci_wboot_means(X, W)
  wr <- ssm_ci_wboot_cors(X, Y, W)
  for (b in 1:5) {
    idx <- rep(seq_len(n), W[, b])
    expect_equal(wm[b, ], colMeans(X[idx, , drop = FALSE]), tolerance = 1e-10,
                 ignore_attr = TRUE)
    r_direct <- stats::cor(Y[idx, , drop = FALSE], X[idx, , drop = FALSE])
    for (m in 1:2) {
      expect_equal(wr[[m]][b, ], r_direct[m, ], tolerance = 1e-10,
                   ignore_attr = TRUE)
    }
  }
  # Zero-variance resample -> NA correlation (degenerate replicate), as cor()
  W0 <- matrix(0L, n, 1)
  W0[7, 1] <- n  # every draw is row 7
  wr0 <- ssm_ci_wboot_cors(X, Y, W0)
  expect_true(all(is.na(wr0[[1]][1, ])))
})

# ---- the c = 0 machinery pin (sec. 4.2) --------------------------------------

test_that("c = 0 amplitude coverage is identically 0 with all misses below (machinery pin)", {
  # NOT evidence the module works (a percentile interval of positive
  # amplitudes cannot contain 0 -- a theorem); pins the machinery only.
  theta <- deg2rad(as.numeric(octants()))
  set.seed(101)
  dat <- as.data.frame(t(sapply(1:120, function(i) {
    1 + 1.5 * cos(theta - 2) + 0.5 * cos(2 * theta) + rnorm(8, 0, 1)
  })))
  colnames(dat) <- PANO()
  set.seed(102)
  obj <- ssm_analyze(dat, scales = PANO(), boots = 60)
  set.seed(103)
  res <- ssm_ci_accuracy(obj, reps = 15, amplitude_factors = c(1, 0),
                         structure = "observed")
  a0 <- res$coverage[res$coverage$Parameter == "a" & res$coverage$Condition == 0, ]
  expect_equal(a0$Coverage, 0)
  expect_equal(a0$Left_miss, 1)   # every miss has truth below the interval
  expect_equal(a0$Right_miss, 0)
  # Displacement truth is undefined at c = 0: coverage NA, no crash
  d0 <- res$coverage[res$coverage$Parameter == "d" & res$coverage$Condition == 0, ]
  expect_true(is.na(d0$Coverage))
  # Guardrail characteristics are still produced at c = 0
  g0 <- res$guardrail[res$guardrail$Condition == 0, ]
  expect_true(is.finite(g0$Cert_rate))
})

test_that("suff-stats fallback resolves recorded-call args from the caller's env", {
  # Regression (milestone-close review): a pre-storage object whose ssm_analyze()
  # call referenced a *local* variable for `scales`. The fallback must evaluate
  # that symbol in the caller's frame, not ssm_ci_accuracy()'s -- else it aborts
  # "object not found" even though `data =` was supplied as instructed.
  theta <- deg2rad(as.numeric(octants()))
  set.seed(701)
  dat <- as.data.frame(t(sapply(1:120, function(i) {
    1 + 1.5 * cos(theta - 2) + 0.5 * cos(2 * theta) + rnorm(8, 0, 1)
  })))
  colnames(dat) <- PANO()

  run <- function(d) {
    sc <- PANO()                       # local var, NOT in globalenv
    obj <- ssm_analyze(d, scales = sc, boots = 40)
    obj$details$suff_stats <- NULL     # emulate an object predating Z0 storage
    set.seed(702)
    ssm_ci_accuracy(obj, data = d, reps = 2, amplitude_factors = 1,
                    structure = "observed")
  }
  expect_no_error(run(dat))
})

test_that("contrast row carries no false-certification guardrail (print never gates it)", {
  # Milestone-close review #3: print.circumplex_ssm() applies no certification
  # gate to a contrast, so the diagnostic must not report a false-cert verdict
  # for the contrast row -- only the joint-certification rate that conditions
  # its certified-displacement coverage.
  data("jz2017")
  jz <- jz2017[1:240, ]
  set.seed(311)
  obj <- ssm_analyze(jz, scales = PANO(), grouping = "Gender",
                     contrast = TRUE, boots = 60)
  set.seed(312)
  res <- ssm_ci_accuracy(obj, reps = 12, amplitude_factors = c(1, 0),
                         structure = "observed")

  con_lab <- names(res$details$row_n)[length(res$details$row_n)]
  con_g <- res$guardrail[res$guardrail$Profile == con_lab, ]
  prof_g0 <- res$guardrail[res$guardrail$Profile != con_lab &
                             res$guardrail$Condition == 0, ]
  # Contrast Caution is NA at every rung; profile rows still carry the c = 0
  # logical decision; the contrast's conditioning Cert_rate is still reported.
  expect_true(all(is.na(con_g$Caution)))
  expect_true(all(!is.na(prof_g0$Caution)))
  expect_true(all(is.finite(con_g$Cert_rate[con_g$Condition == 0])))

  # print()/summary() never frame the contrast as certified; wording bar holds.
  out <- paste(c(capture.output(print(res)), capture.output(summary(res))),
               collapse = "\n")
  expect_false(any(grepl("contrast displacement would", out, fixed = TRUE)))
  expect_false(any(grepl("significan", out)))
})

# ---- end-to-end: mean-based group contrast on octant data --------------------

test_that("end-to-end run on octant data: object contract (sec. 7)", {
  data("jz2017")
  jz <- jz2017[1:240, ]
  set.seed(201)
  obj <- ssm_analyze(jz, scales = PANO(), grouping = "Gender",
                     contrast = TRUE, boots = 80)
  set.seed(202)
  res <- ssm_ci_accuracy(obj, reps = 20, amplitude_factors = c(1, 0.25))

  expect_s3_class(res, "circumplex_ci_accuracy")
  expect_named(
    res,
    c("coverage", "guardrail", "verdict", "cpm", "population", "details",
      "call"),
    ignore.order = TRUE
  )
  expect_s3_class(res$cpm, "circumplex_cpm")

  cov <- res$coverage
  # 3 rows (2 groups + contrast) x 5 CI'd parameters x 2 conditions
  expect_equal(nrow(cov), 3 * 5 * 2)
  ok <- !is.na(cov$Coverage)
  expect_true(all(cov$Coverage[ok] >= 0 & cov$Coverage[ok] <= 1))
  expect_true(all(cov$MC_se[ok] >= 0))
  # Coverage + misses partition the assessable replicates
  expect_equal(cov$Coverage[ok] + cov$Left_miss[ok] + cov$Right_miss[ok],
               rep(1, sum(ok)), tolerance = 1e-12)
  # Elevation is textbook-friendly here; even at 20 reps it should not be 0
  e1 <- cov[cov$Parameter == "e" & cov$Condition == 1, ]
  expect_true(all(e1$Coverage > 0.5))

  gr <- res$guardrail
  expect_equal(unique(gr$Threshold), 0.5 * 10^-3)
  expect_equal(unique(gr$Benchmark), 0.025)
  expect_true(all(gr$Cert_rate >= 0 & gr$Cert_rate <= 1, na.rm = TRUE))

  vd <- res$verdict
  expect_true(all(vd$Class %in% c("adequate", "borderline", "inadequate")))
  expect_true(any(vd$Parameter == "overall"))

  expect_true(is.finite(res$details$elapsed))
  expect_identical(res$details$structure, "cpm")
  expect_identical(res$details$method, "bootstrap")
  expect_output(print(res), "adequate|borderline|inadequate")
  expect_output(summary(res), "Coverage")
})

# ---- correlation path with measure contrast ----------------------------------

test_that("correlation path: measure contrast, PSD repair, branch pathology fields", {
  data("jz2017")
  jz <- jz2017[1:200, ]
  set.seed(301)
  obj <- ssm_analyze(jz, scales = PANO(), measures = c("NARPD", "ASPD"),
                     contrast = TRUE, boots = 60)
  set.seed(302)
  res <- ssm_ci_accuracy(obj, reps = 12, amplitude_factors = c(1, 0.5),
                         structure = "observed")

  cov <- res$coverage
  expect_equal(nrow(cov), 3 * 5 * 2)
  # Conditional displacement coverage exists only on d rows
  d_rows <- cov$Parameter == "d"
  expect_true(all(is.na(cov$Coverage_conditional[!d_rows])))
  # PSD repair magnitude is recorded and non-negative
  deltas <- unlist(lapply(res$population, function(x) x$psd_delta))
  expect_true(all(deltas >= 0))
  # Branch pathology rate present for every row x condition
  expect_true(all(res$guardrail$Branch_pathology_rate >= 0 &
                    res$guardrail$Branch_pathology_rate <= 1, na.rm = TRUE))
})

# ---- Monte Carlo engine replay ------------------------------------------------

test_that("a montecarlo-method object is assessed with the Monte Carlo engine", {
  data("jz2017")
  jz <- jz2017[1:200, ]
  set.seed(401)
  obj <- ssm_analyze(jz, scales = PANO(), boots = 60, method = "montecarlo")
  set.seed(402)
  res <- ssm_ci_accuracy(obj, reps = 10, amplitude_factors = 1,
                         structure = "observed")
  expect_identical(res$details$method, "montecarlo")
  e1 <- res$coverage[res$coverage$Parameter == "e", ]
  expect_true(all(e1$Coverage > 0.5))
})

# ---- boundary: population displacement at the 0/360 pole ----------------------

test_that("coverage counts correctly when the population peaks at 0/360", {
  theta <- deg2rad(as.numeric(octants()))
  set.seed(501)
  dat <- as.data.frame(t(sapply(1:150, function(i) {
    1 + 1.2 * cos(theta) + rnorm(8, 0, 1)   # true displacement at the pole
  })))
  colnames(dat) <- PANO()
  set.seed(502)
  obj <- ssm_analyze(dat, scales = PANO(), boots = 100)
  set.seed(503)
  res <- ssm_ci_accuracy(obj, reps = 25, amplitude_factors = 1,
                         structure = "observed")
  d1 <- res$coverage[res$coverage$Parameter == "d" &
                       res$coverage$Condition == 1, ]
  # Nominal .95; anything near 0 would mean wrapped intervals were mishandled
  expect_gt(d1$Coverage, 0.6)
})

# ---- RNG contract --------------------------------------------------------------

test_that("results are seed-reproducible and the caller RNG state is restored", {
  theta <- deg2rad(as.numeric(octants()))
  set.seed(801)
  dat <- as.data.frame(t(sapply(1:60, function(i) {
    1 + 1.5 * cos(theta - 2) + rnorm(8, 0, 1)
  })))
  colnames(dat) <- PANO()
  set.seed(802)
  obj <- ssm_analyze(dat, scales = PANO(), boots = 40)

  set.seed(9)
  r1 <- ssm_ci_accuracy(obj, reps = 6, amplitude_factors = 1,
                        structure = "observed")
  set.seed(9)
  r2 <- ssm_ci_accuracy(obj, reps = 6, amplitude_factors = 1,
                        structure = "observed")
  expect_identical(r1$coverage, r2$coverage)
  expect_identical(r1$guardrail, r2$guardrail)

  # Caller state: advanced by exactly the one documented sample.int() draw,
  # then restored -- kind included (no lingering L'Ecuyer-CMRG)
  set.seed(9)
  invisible(sample.int(.Machine$integer.max, 1))
  s_expected <- .Random.seed
  set.seed(9)
  invisible(ssm_ci_accuracy(obj, reps = 2, amplitude_factors = 1,
                            structure = "observed"))
  expect_identical(.Random.seed, s_expected)
  expect_identical(RNGkind()[1], "Mersenne-Twister")
})

test_that("parallel path yields results identical to serial at a fixed seed", {
  skip_on_os("windows")   # multicore forks; mclapply is serial on Windows
  theta <- deg2rad(as.numeric(octants()))
  set.seed(811)
  dat <- as.data.frame(t(sapply(1:60, function(i) {
    1 + 1.5 * cos(theta - 2) + rnorm(8, 0, 1)
  })))
  colnames(dat) <- PANO()
  set.seed(812)
  obj <- ssm_analyze(dat, scales = PANO(), boots = 40)

  set.seed(21)
  ser <- ssm_ci_accuracy(obj, reps = 8, amplitude_factors = c(1, 0),
                         structure = "observed")
  set.seed(21)
  par <- ssm_ci_accuracy(obj, reps = 8, amplitude_factors = c(1, 0),
                         structure = "observed",
                         parallel = "multicore", ncpus = 2)
  expect_identical(ser$coverage, par$coverage)
  expect_identical(ser$guardrail, par$guardrail)
  expect_identical(ser$verdict, par$verdict)
})

# ---- guards and fallbacks -------------------------------------------------------

test_that("a flat-profile population is refused with a clear error", {
  dat <- as.data.frame(matrix(1, nrow = 30, ncol = 8))
  colnames(dat) <- PANO()
  obj <- suppressWarnings(ssm_analyze(dat, scales = PANO(), boots = 20))
  expect_error(
    ssm_ci_accuracy(obj, reps = 5, structure = "observed"),
    "flat"
  )
})

test_that("objects predating suff-stats storage work through the data fallback", {
  data("jz2017")
  jz <- jz2017[1:150, ]
  set.seed(901)
  obj <- ssm_analyze(jz, scales = PANO(), boots = 40)
  obj$details$suff_stats <- NULL
  expect_error(
    ssm_ci_accuracy(obj, reps = 3, amplitude_factors = 1,
                    structure = "observed"),
    "data"
  )
  set.seed(902)
  res <- ssm_ci_accuracy(obj, reps = 3, amplitude_factors = 1,
                         structure = "observed", data = jz)
  expect_s3_class(res, "circumplex_ci_accuracy")
})

test_that("a pre-fitted CPM is reused rather than refit", {
  data("jz2017")
  jz <- jz2017[1:200, ]
  set.seed(911)
  obj <- ssm_analyze(jz, scales = PANO(), boots = 40)
  stats <- obj$details$suff_stats
  Rw <- stats$cormats[[1]]
  pre <- cpm_fit(cormat = Rw, n = stats$n[[1]], scales = PANO(),
                 angles = as.numeric(octants()))
  set.seed(912)
  res <- ssm_ci_accuracy(obj, reps = 3, amplitude_factors = 1, cpm = pre)
  expect_identical(res$cpm$matrices$Phat, pre$matrices$Phat)
})

test_that("input validation rejects bad arguments", {
  data("aw2009")
  set.seed(921)
  obj <- ssm_analyze(aw2009, scales = 1:8, boots = 20)
  expect_error(ssm_ci_accuracy(list()), "circumplex_ssm")
  expect_error(ssm_ci_accuracy(obj, reps = 0))
  expect_error(ssm_ci_accuracy(obj, amplitude_factors = c(0.5, 0)), "1")
  expect_error(ssm_ci_accuracy(obj, amplitude_factors = c(1, 2)))
  expect_error(ssm_ci_accuracy(obj, digits = -1))
  expect_error(ssm_ci_accuracy(obj, parallel = "bogus"))
})

# ==== Z2: amplitude-near-zero module + verdict (spec sec. 4-5, sec. 10) =======

# ---- Bradley/Wilson classification at the band edges (sec. 5.1) --------------

test_that("Bradley classification is correct at the band edges", {
  # Nominal .95 -> Bradley liberal band [.925, .975]
  # Wilson interval entirely inside the band
  expect_identical(ssm_ci_bradley_class(950, 1000, 0.95),
                   c("adequate", NA_character_))
  # Wilson interval overlapping the lower band edge
  expect_identical(ssm_ci_bradley_class(930, 1000, 0.95)[1], "borderline")
  # Wilson interval overlapping the upper band edge
  expect_identical(ssm_ci_bradley_class(975, 1000, 0.95)[1], "borderline")
  # Entirely below the band: inadequate with under-coverage direction
  expect_identical(ssm_ci_bradley_class(880, 1000, 0.95),
                   c("inadequate", "under"))
  # Entirely above the band: inadequate with over-coverage direction
  expect_identical(ssm_ci_bradley_class(999, 1000, 0.95),
                   c("inadequate", "over"))
  # Small reps rarely clear the band: 19/20 = .95 must not come back adequate
  expect_identical(ssm_ci_bradley_class(19, 20, 0.95)[1], "borderline")
  # Not assessable
  expect_identical(ssm_ci_bradley_class(NA, 0, 0.95),
                   c(NA_character_, NA_character_))
  # The classification is keyed to the nominal level: the same counts move
  # with the band ([.85, .95] at 90% nominal)
  expect_identical(ssm_ci_bradley_class(900, 1000, 0.90)[1], "adequate")
})

# ---- sec. 10 known-good oracle ------------------------------------------------

test_that("known-good oracle: healthy elevation CIs are classified adequate", {
  # Mean-based elevation is a plain mean of means; at moderate n under MVN,
  # percentile-bootstrap coverage is textbook-adequate. A diagnostic that
  # flags healthy elevation CIs is broken (spec sec. 10).
  skip_on_cran()
  theta <- deg2rad(as.numeric(octants()))
  set.seed(1001)
  dat <- as.data.frame(t(sapply(1:300, function(i) {
    2 + 1.5 * cos(theta - 2) + rnorm(8, 0, 1)
  })))
  colnames(dat) <- PANO()
  set.seed(1002)
  obj <- ssm_analyze(dat, scales = PANO(), boots = 1000)
  set.seed(1003)
  res <- ssm_ci_accuracy(obj, reps = 1000, amplitude_factors = 1,
                         structure = "observed")
  ev <- res$verdict[res$verdict$Parameter == "e", ]
  expect_identical(ev$Class, "adequate")
})

# ---- sec. 10 known-bad direction oracle ----------------------------------------

test_that("known-bad direction oracle: near-zero amplitude under-covers, misses below; c = 0 false-certifies above benchmark", {
  # At a small c > 0 rung (NOT c = 0, where amplitude coverage is a theorem,
  # not a measurement -- sec. 4.2) the nonnegative, upward-biased amplitude
  # estimator makes percentile CIs sit above a small truth: coverage must fall
  # below nominal (one-sided binomial test) with misses concentrated on the
  # truth-below-interval side. At c = 0 the shipped guardrail's
  # false-certification rate must exceed the alpha/2 user-expectation
  # benchmark (directional only; no magnitude is pinned -- oracle rule).
  skip_on_cran()
  theta <- deg2rad(as.numeric(octants()))
  set.seed(1101)
  dat <- as.data.frame(t(sapply(1:80, function(i) {
    1 + 0.5 * cos(theta - 2) + rnorm(8, 0, 1)
  })))
  colnames(dat) <- PANO()
  set.seed(1102)
  obj <- ssm_analyze(dat, scales = PANO(), boots = 200)
  set.seed(1103)
  res <- ssm_ci_accuracy(obj, reps = 200, amplitude_factors = c(1, 0.15, 0),
                         structure = "observed")

  arow <- res$coverage[res$coverage$Parameter == "a" &
                         res$coverage$Condition == 0.15, ]
  k <- round(arow$Coverage * arow$N_reps)
  bt <- stats::binom.test(k, arow$N_reps, p = 0.95, alternative = "less")
  expect_lt(bt$p.value, 0.05)
  expect_gt(arow$Left_miss, arow$Right_miss)

  g0 <- res$guardrail[res$guardrail$Condition == 0, ]
  expect_gt(g0$Cert_lci, g0$Benchmark)
})

# ---- sec. 10 boundary: branch pathology manufactured at a small-c rung --------

test_that("a contrast at a small-c rung produces branch-pathology events", {
  # The F2-corrected joint row ladder drives both rows' amplitudes toward
  # zero, manufacturing the near-uniform contrast-displacement regime where
  # the point estimate falls geometrically outside its own reported interval.
  # The pathology is a rare event even where it lives (rates of a few per
  # thousand at near-zero rungs; measured while pinning this seed), so the
  # rung set includes c = 0 and the counter is summed over the two
  # near-zero rungs of the contrast row.
  skip_on_cran()
  theta <- deg2rad(as.numeric(octants()))
  set.seed(3001)
  dat1 <- t(sapply(1:30, function(i) 1 + 0.8 * cos(theta - 1) + rnorm(8, 0, 1)))
  dat2 <- t(sapply(1:30, function(i) 1 + 0.8 * cos(theta - 3) + rnorm(8, 0, 1)))
  dat <- as.data.frame(rbind(dat1, dat2))
  colnames(dat) <- PANO()
  dat$Group <- factor(rep(c("A", "B"), each = 30))
  set.seed(3002)
  obj <- ssm_analyze(dat, scales = PANO(), grouping = "Group",
                     contrast = TRUE, boots = 60)
  set.seed(3003)
  res <- ssm_ci_accuracy(obj, reps = 300, amplitude_factors = c(1, 0.05, 0),
                         structure = "observed")
  con_lab <- res$guardrail$Profile[nrow(res$guardrail)]
  br <- res$guardrail[res$guardrail$Profile == con_lab &
                        res$guardrail$Condition %in% c(0.05, 0), ]
  expect_gt(sum(br$Branch_pathology_rate), 0)
})

# ---- sec. 10 engine parity spot-check ------------------------------------------

test_that("bootstrap and Monte Carlo engines agree within combined MC error", {
  skip_on_cran()
  theta <- deg2rad(as.numeric(octants()))
  set.seed(1301)
  dat <- as.data.frame(t(sapply(1:150, function(i) {
    1 + 1.2 * cos(theta - 2) + rnorm(8, 0, 1)
  })))
  colnames(dat) <- PANO()
  set.seed(1302)
  obj_b <- ssm_analyze(dat, scales = PANO(), boots = 200)
  set.seed(1302)
  obj_m <- ssm_analyze(dat, scales = PANO(), boots = 200,
                       method = "montecarlo")
  set.seed(1303)
  res_b <- ssm_ci_accuracy(obj_b, reps = 150, amplitude_factors = 1,
                           structure = "observed")
  set.seed(1304)
  res_m <- ssm_ci_accuracy(obj_m, reps = 150, amplitude_factors = 1,
                           structure = "observed")
  for (pm in c("e", "a")) {
    cb <- res_b$coverage[res_b$coverage$Parameter == pm, "Coverage"]
    cm <- res_m$coverage[res_m$coverage$Parameter == pm, "Coverage"]
    expect_lt(abs(cb - cm), 0.12)
  }
})

# ---- degenerate ladder: the sec. 4.1 margin rung -------------------------------

test_that("an amplitude estimate below half its CI width adds the margin rung", {
  theta <- deg2rad(as.numeric(octants()))
  set.seed(1401)
  dat <- as.data.frame(t(sapply(1:60, function(i) {
    1 + 0.05 * cos(theta - 2) + rnorm(8, 0, 1)
  })))
  colnames(dat) <- PANO()
  set.seed(1402)
  obj <- ssm_analyze(dat, scales = PANO(), boots = 100)
  # Precondition for the trigger: a_hat below half the observed CI width
  stopifnot(obj$results$a_est < (obj$results$a_uci - obj$results$a_lci) / 2)
  set.seed(1403)
  res <- ssm_ci_accuracy(obj, reps = 8, amplitude_factors = c(1, 0),
                         structure = "observed")
  mr <- res$details$margin_rung
  expect_true(is.numeric(mr) && length(mr) == 1 && mr > 1)
  # c * a_hat equals the observed amplitude-CI half-width
  expect_equal(mr * obj$results$a_est,
               (obj$results$a_uci - obj$results$a_lci) / 2,
               tolerance = 1e-10)
  expect_true(mr %in% res$coverage$Condition)
  expect_true(mr %in% res$guardrail$Condition)
  # details$conditions is the full simulated ladder, margin rung included
  expect_identical(res$details$conditions, c(1, 0, mr))
  # The population truth at the margin rung is the half-width (linearity)
  tr <- res$population[[1]]$truths
  expect_equal(tr$a[tr$Condition == mr],
               (obj$results$a_uci - obj$results$a_lci) / 2,
               tolerance = 1e-8)
  # summary() names the regime (whitespace normalized: the phrase may wrap)
  out <- gsub("\\s+", " ", paste(capture.output(summary(res)), collapse = " "))
  expect_match(out, "near-zero regime")
  # The verdict stays keyed to c = 1 (margin rung adds no verdict rows)
  expect_identical(unique(res$verdict$Profile), res$coverage$Profile[1])
})

test_that("healthy amplitudes add no margin rung", {
  data("jz2017")
  jz <- jz2017[1:150, ]
  set.seed(1451)
  obj <- ssm_analyze(jz, scales = PANO(), boots = 60)
  set.seed(1452)
  res <- ssm_ci_accuracy(obj, reps = 3, amplitude_factors = 1,
                         structure = "observed")
  expect_null(res$details$margin_rung)
  expect_identical(sort(unique(res$coverage$Condition)), 1)
})

# ---- guardrail measurement columns (sec. 4.3) ----------------------------------

test_that("guardrail table carries Wilson bounds and reps; coverage carries N_conditional and Structural", {
  theta <- deg2rad(as.numeric(octants()))
  set.seed(1501)
  dat <- as.data.frame(t(sapply(1:100, function(i) {
    1 + 1.2 * cos(theta - 2) + 0.4 * cos(2 * theta) + rnorm(8, 0, 1)
  })))
  colnames(dat) <- PANO()
  set.seed(1502)
  obj <- ssm_analyze(dat, scales = PANO(), boots = 60)
  set.seed(1503)
  res <- ssm_ci_accuracy(obj, reps = 20, amplitude_factors = c(1, 0),
                         structure = "observed")

  gr <- res$guardrail
  expect_true(all(c("Cert_lci", "Cert_uci", "N_reps") %in% names(gr)))
  ok <- !is.na(gr$Cert_rate)
  expect_true(all(gr$Cert_lci[ok] <= gr$Cert_rate[ok] + 1e-12))
  expect_true(all(gr$Cert_uci[ok] >= gr$Cert_rate[ok] - 1e-12))
  expect_true(all(gr$N_reps == 20))
  # Wilson bounds match the shared helper at the observed counts
  g1 <- gr[gr$Condition == 1, ]
  w <- ssm_ci_wilson(round(g1$Cert_rate * g1$N_reps), g1$N_reps)
  expect_equal(c(g1$Cert_lci, g1$Cert_uci), w, tolerance = 1e-12)
  # The false-certification caution decision is stored, and only at c = 0
  expect_true(all(is.na(gr$Caution[gr$Condition != 0])))
  g0 <- gr[gr$Condition == 0, ]
  expect_identical(g0$Caution, unname(g0$Cert_lci > g0$Benchmark))

  cov <- res$coverage
  expect_true(all(c("N_conditional", "Structural") %in% names(cov)))
  # Structural flags exactly the mean-path amplitude rows at a zero truth
  expect_identical(cov$Structural,
                   cov$Parameter == "a" & cov$Condition == 0)
  # N_conditional exists only on displacement rows, bounded by N_reps
  d_rows <- cov$Parameter == "d"
  expect_true(all(is.na(cov$N_conditional[!d_rows])))
  expect_true(all(cov$N_conditional[d_rows & cov$Condition == 1] <=
                    cov$N_reps[d_rows & cov$Condition == 1]))
})

test_that("a contrast row's zero-amplitude condition is not flagged structural", {
  # The contrast amplitude is a signed, unconstrained difference: its
  # percentile interval CAN contain 0, so the c = 0 theorem does not apply
  data("jz2017")
  jz <- jz2017[1:200, ]
  set.seed(1551)
  obj <- ssm_analyze(jz, scales = PANO(), grouping = "Gender",
                     contrast = TRUE, boots = 60)
  set.seed(1552)
  res <- ssm_ci_accuracy(obj, reps = 10, amplitude_factors = c(1, 0),
                         structure = "observed")
  cov <- res$coverage
  con_lab <- utils::tail(cov$Profile, 1)
  con_a0 <- cov$Profile == con_lab & cov$Parameter == "a" & cov$Condition == 0
  expect_false(any(cov$Structural[con_a0]))
  # And the contrast's amplitude-difference interval can in fact cover 0
  expect_gt(cov$Coverage[con_a0], 0)
})

# ---- verdict wording (sec. 5.2) -------------------------------------------------

test_that("summary() carries the false-certification caution and wording bar", {
  theta <- deg2rad(as.numeric(octants()))
  set.seed(1601)
  dat <- as.data.frame(t(sapply(1:100, function(i) {
    1 + 0.6 * cos(theta - 2) + rnorm(8, 0, 1)
  })))
  colnames(dat) <- PANO()
  set.seed(1602)
  obj <- ssm_analyze(dat, scales = PANO(), boots = 100)
  set.seed(1603)
  res <- ssm_ci_accuracy(obj, reps = 40, amplitude_factors = c(1, 0.25, 0),
                         structure = "observed")
  # Whitespace normalized: the wrapped verdict lines may break mid-phrase
  out <- gsub("\\s+", " ", paste(capture.output(summary(res)), collapse = " "))
  # The false-certification caution line is present (theory predicts the
  # rate far exceeds the benchmark at this configuration)
  expect_match(out, "if the true amplitude were zero")
  # The user-expectation benchmark is named as such, never as a nominal level
  expect_match(out, "its wording suggests")
  # Wording bar (sec. 5.2): an angular CI excluding 0 is never described as
  # a significance test, anywhere in the printed verdict
  expect_false(grepl("significan", out, ignore.case = TRUE))
  # Structural c = 0 note present
  expect_match(out, "structurally")
  # print() shows the per-profile verdict lines
  pout <- paste(capture.output(print(res)), collapse = "\n")
  expect_match(pout, "Elevation")
  expect_match(pout, "Verdict")
  expect_false(grepl("significan", pout, ignore.case = TRUE))
})

test_that("print and summary snapshots (seeded)", {
  data("jz2017")
  jz <- jz2017[1:120, ]
  set.seed(1701)
  obj <- ssm_analyze(jz, scales = PANO(), boots = 60)
  set.seed(1702)
  res <- ssm_ci_accuracy(obj, reps = 30, amplitude_factors = c(1, 0.25, 0))
  mask_elapsed <- function(lines) {
    sub("Elapsed:.*$", "Elapsed:\t\t<masked>", lines)
  }
  expect_snapshot(print(res))
  expect_snapshot(summary(res), transform = mask_elapsed)
})

# ---- plot method (spec sec. 7) --------------------------------------------------

test_that("plot.circumplex_ci_accuracy builds a faceted coverage plot", {
  theta <- deg2rad(as.numeric(octants()))
  set.seed(1801)
  dat <- as.data.frame(t(sapply(1:100, function(i) {
    1 + 1.0 * cos(theta - 2) + rnorm(8, 0, 1)
  })))
  colnames(dat) <- PANO()
  set.seed(1802)
  obj <- ssm_analyze(dat, scales = PANO(), boots = 60)
  set.seed(1803)
  res <- ssm_ci_accuracy(obj, reps = 25, amplitude_factors = c(1, 0.5, 0.25, 0),
                         structure = "observed")
  p <- plot(res)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  # One panel per parameter, including the certified-displacement panel
  expect_equal(length(unique(built$layout$layout$PANEL)), 6)
  vdiffr::expect_doppelganger("ci accuracy ladder plot", p)
})
