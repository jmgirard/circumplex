# T3: ssm_draws() dispatch contract (spec sec. 5.1) ----------------------------

test_that("ssm_draws dispatches shape B when angles are supplied", {
  # Two profile draws over octants; each row goes through group_parameters()
  # exactly as a bootstrap replicate would. Row 1 is the pure first harmonic
  # 1 + 2*cos(theta) + 2*sin(theta): e = 1, x = 2, y = 2, a = sqrt(8),
  # d = atan2(2, 2) = 45 deg, fit = 1. Row 2 is 2 - 1*cos(theta):
  # e = 2, x = -1, y = 0, a = 1, d = atan2(0, -1) = 180 deg, fit = 1.
  theta <- as.numeric(octants()) * pi / 180
  draws <- rbind(
    1 + 2 * cos(theta) + 2 * sin(theta),
    2 - 1 * cos(theta) + 0 * sin(theta)
  )
  res <- ssm_draws(draws, angles = octants())
  expect_s3_class(res, "circumplex_ssm_draws")
  expect_equal(res$details$shape, "profiles")
  expect_equal(nrow(res$draws), 2)
  # Per-row transforms: draws table holds (e, x, y, a, d, fit) per draw
  expect_equal(res$draws[, "e"], c(1, 2))
  expect_equal(res$draws[, "x"], c(2, -1))
  expect_equal(res$draws[, "y"], c(2, 0))
  expect_equal(res$draws[, "a"], c(sqrt(8), 1))
  expect_equal(res$draws[, "d"], c(45, 180))
  expect_equal(res$draws[, "fit"], c(1, 1))
})

test_that("ssm_draws shape B requires ncol(draws) == length(angles)", {
  draws <- matrix(rnorm(20), ncol = 4)
  expect_error(
    ssm_draws(draws, angles = octants()),
    "length"
  )
})

test_that("ssm_draws with angles = NULL and ncol != 3 errors naming both shapes", {
  draws <- matrix(rnorm(40), ncol = 8)
  err <- tryCatch(ssm_draws(draws), error = function(e) conditionMessage(e))
  expect_match(err, "parameter draws", ignore.case = TRUE)
  expect_match(err, "profile draws", ignore.case = TRUE)
  expect_match(err, "angles")
})

test_that("ssm_draws with angles = NULL and ncol == 3 requires explicit type", {
  # A p = 3 instrument's profile draws are indistinguishable from (e, x, y)
  # parameter draws by shape alone; silence would risk a silently wrong
  # transform, so an explicit type = "parameters" is required.
  draws <- matrix(rnorm(30), ncol = 3)
  err <- tryCatch(ssm_draws(draws), error = function(e) conditionMessage(e))
  expect_match(err, "type")
  expect_match(err, "ambigu", ignore.case = TRUE)

  res <- ssm_draws(draws, type = "parameters")
  expect_s3_class(res, "circumplex_ssm_draws")
  expect_equal(res$details$shape, "parameters")
})

test_that("ssm_draws rejects contradictory type/angles combinations", {
  draws3 <- matrix(rnorm(30), ncol = 3)
  draws8 <- matrix(rnorm(80), ncol = 8)
  # profile draws require angles
  expect_error(ssm_draws(draws3, type = "profiles"), "angles")
  # parameter draws must not carry angles
  expect_error(
    ssm_draws(draws3, angles = c(90, 210, 330), type = "parameters"),
    "angles"
  )
  # parameter draws have exactly three columns
  expect_error(ssm_draws(draws8, type = "parameters"), "3 columns")
})

test_that("ssm_draws messages the assumed column mapping when names are odd", {
  draws <- matrix(rnorm(30), ncol = 3)

  # Recognizably (intercept, cos, sin)-like names: no message
  colnames(draws) <- c("b_Intercept", "b_cos", "b_sin")
  expect_no_message(res1 <- ssm_draws(draws, type = "parameters"))
  # (e, x, y)-like names: no message
  colnames(draws) <- c("e", "x", "y")
  expect_no_message(res2 <- ssm_draws(draws, type = "parameters"))
  # Unrecognized names: message states the assumed (e, x, y) mapping
  colnames(draws) <- c("alpha", "beta", "gamma")
  expect_message(
    res3 <- ssm_draws(draws, type = "parameters"),
    "e, x, y"
  )
  # Unnamed draws: documented assumption, no message
  colnames(draws) <- NULL
  expect_no_message(res4 <- ssm_draws(draws, type = "parameters"))

  # The mapping itself never changes with the names
  expect_equal(res1$results, res3$results)
  expect_equal(res1$results, res4$results)
})

test_that("ssm_draws shape A applies the closed-form transform per row", {
  # Hand arithmetic: row 1 (e, x, y) = (1, 3, 4) -> a = 5,
  # d = atan2(4, 3) = 53.13010 deg; row 2 (0, 0, -2) -> a = 2,
  # d = atan2(-2, 0) = -90 -> 270 deg; fit is synthesized as NA (parameter
  # draws carry no profile to measure fit against).
  draws <- rbind(c(1, 3, 4), c(0, 0, -2))
  res <- ssm_draws(draws, type = "parameters")
  expect_equal(res$draws[, "e"], c(1, 0))
  expect_equal(res$draws[, "x"], c(3, 0))
  expect_equal(res$draws[, "y"], c(4, -2))
  expect_equal(res$draws[, "a"], c(5, 2))
  expect_equal(res$draws[, "d"], c(atan2(4, 3) * 180 / pi, 270))
  expect_true(all(is.na(res$draws[, "fit"])))
})

test_that("ssm_draws validates draws and interval", {
  draws <- matrix(rnorm(30), ncol = 3)
  expect_error(ssm_draws("draws", type = "parameters"))
  expect_error(ssm_draws(draws, type = "parameters", interval = 95))
  expect_error(ssm_draws(draws, type = "parameters", interval = c(0.5, 0.9)))
  expect_error(ssm_draws(matrix(numeric(0), ncol = 3), type = "parameters"))
  # data frame input is accepted
  res <- ssm_draws(as.data.frame(draws), type = "parameters")
  expect_s3_class(res, "circumplex_ssm_draws")
})

# T4: adapter summary path (managed leaks, snapshots) ---------------------------

test_that("point summaries are per-parameter medians plus the circular mean", {
  # t0 is the adapter's own point summaries (there is no observed estimate
  # for posterior draws): medians for e, x, y, a; circular mean for d,
  # recomputed by hand here (atan2 of summed sines/cosines).
  draws <- rbind(
    c(1.0, 3.0, 4.0),
    c(1.2, 2.0, 4.4),
    c(0.8, 3.5, 3.6),
    c(1.1, 2.5, 4.1)
  )
  res <- ssm_draws(draws, type = "parameters")
  expect_equal(res$results$e_est, median(draws[, 1]))
  expect_equal(res$results$x_est, median(draws[, 2]))
  expect_equal(res$results$y_est, median(draws[, 3]))
  a_draws <- sqrt(draws[, 2]^2 + draws[, 3]^2)
  expect_equal(res$results$a_est, median(a_draws))
  d_draws <- atan2(draws[, 3], draws[, 2])
  d_hand <- atan2(sum(sin(d_draws)), sum(cos(d_draws))) %% (2 * pi) * 180 / pi
  expect_equal(as.numeric(res$results$d_est), d_hand)
  # Marginal summaries are not jointly coherent (documented caveat): the
  # reported a is the median amplitude, not the amplitude of the medians
  expect_false(isTRUE(all.equal(
    res$results$a_est,
    sqrt(res$results$x_est^2 + res$results$y_est^2)
  )))
  # fit is structurally NA for parameter draws
  expect_true(is.na(res$results$fit_est))
})

test_that("degenerate-draw warnings say posterior draws and credible interval", {
  # Row 2 has exactly zero amplitude -> undefined displacement for that draw
  draws <- rbind(c(1, 3, 4), c(1, 0, 0), c(1, 2, 2))
  w <- capture_warnings(res <- ssm_draws(draws, type = "parameters"))
  expect_length(w, 1)
  expect_match(w, "1 of 3 posterior draws")
  expect_match(w, "credible interval")
  expect_no_match(w, "bootstrap")
  expect_no_match(w, "confidence")
})

test_that("shape A's synthesized NA fit is never counted as degenerate", {
  # Every parameter draw carries fit = NA by construction; that must not
  # trip the degenerate-replicate warning (which keys on genuine NA
  # parameters like displacement)
  draws <- rbind(c(1, 3, 4), c(2, 1, 1))
  expect_no_warning(ssm_draws(draws, type = "parameters"))
})

test_that("an undefined displacement point summary warns honestly", {
  # All draws have zero amplitude: no displacement draw is defined, so the
  # displacement point summary and interval are NA
  draws <- rbind(c(1, 0, 0), c(2, 0, 0))
  w <- capture_warnings(res <- ssm_draws(draws, type = "parameters"))
  expect_match(w, "displacement point summary is undefined", all = FALSE)
  expect_true(is.na(res$results$d_est))
  expect_true(is.na(res$results$d_lci))
  expect_true(is.na(res$results$d_uci))
  expect_no_match(w, "observed profiles")
})

test_that("print and summary output for draws objects is stable", {
  draws <- rbind(
    c(1.0, 3.0, 4.0),
    c(1.2, 2.0, 4.4),
    c(0.8, 3.5, 3.6),
    c(1.1, 2.5, 4.1)
  )
  res <- ssm_draws(draws, type = "parameters")
  expect_snapshot(print(res))
  expect_snapshot(summary(res))

  theta <- as.numeric(octants()) * pi / 180
  pdraws <- rbind(
    1 + 2 * cos(theta) + 2 * sin(theta),
    2 - 1 * cos(theta),
    1 + 1 * cos(theta) + 1 * sin(theta)
  )
  resb <- ssm_draws(pdraws, angles = octants(), interval = 0.9)
  expect_snapshot(print(resb))
  expect_snapshot(summary(resb))
})

# M27 T2: per-draws D-007 certification caution ---------------------------------

test_that("a certified draws object stores the flag and prints no caution", {
  # Handcrafted amplitude draws in [0.5, 1.5]: a_lci ~ 0.525, width ~ 0.95,
  # r = a_lci / (a_uci - a_lci) ~ 0.55 >= 0.35 -- certified under D-007
  draws <- cbind(0, seq(0.5, 1.5, length.out = 200), 0)
  res <- ssm_draws(draws, type = "parameters")
  expect_true(res$details$certified)
  out <- paste(utils::capture.output(print(res)), collapse = "\n")
  expect_no_match(out, "not interpretable")
})

test_that("an uncertified draws object is flagged and prints the caution", {
  # Amplitude CrI hugging zero: a_lci ~ 0.026, width ~ 0.95, r ~ 0.03 << 0.35
  draws <- cbind(0, seq(0.001, 1, length.out = 200), 0)
  res <- ssm_draws(draws, type = "parameters")
  expect_false(res$details$certified)
  out <- paste(utils::capture.output(print(res)), collapse = "\n")
  expect_match(out, "not interpretable")
  # summary() prints the same table, so it carries the caution too
  outs <- paste(utils::capture.output(summary(res)), collapse = "\n")
  expect_match(outs, "not interpretable")
})

test_that("a degenerate zero-width amplitude CrI fails certification closed", {
  # All draws at exactly zero amplitude: a_lci = a_uci = 0, ratio NaN -> the
  # is.finite() guard in ssm_certified() fails closed (D-007 edge contract)
  draws <- rbind(c(1, 0, 0), c(2, 0, 0))
  suppressWarnings(res <- ssm_draws(draws, type = "parameters"))
  expect_false(res$details$certified)
  out <- paste(utils::capture.output(print(res)), collapse = "\n")
  expect_match(out, "not interpretable")
})

test_that("profile-shape draws objects carry the certification flag too", {
  # The rule is a pure function of the amplitude interval, so both adapter
  # shapes are gated identically
  theta <- as.numeric(octants()) * pi / 180
  pdraws <- rbind(
    1 + 2 * cos(theta) + 2 * sin(theta),
    2 - 1 * cos(theta),
    1 + 1 * cos(theta) + 1 * sin(theta)
  )
  res <- ssm_draws(pdraws, angles = octants(), interval = 0.9)
  expect_true(res$details$certified)
})

test_that("the draws caution and flag agree with ssm_certified by construction", {
  # Single-definition rule (D-007): the stored flag must equal ssm_certified()
  # applied to the object's own amplitude interval
  draws <- cbind(0, seq(0.001, 1, length.out = 50), 0)
  res <- ssm_draws(draws, type = "parameters")
  expect_identical(
    res$details$certified,
    unname(ssm_certified(res$results$a_lci, res$results$a_uci))
  )
})

# T5: adapter oracle suite (spec sec. 5.5) --------------------------------------

test_that("feeding a run's bootstrap replicates reproduces its intervals exactly", {
  skip_on_cran()

  # Oracle 1 (invariant, the decisive one): the adapter fed the bootstrap
  # replicate matrix of an existing ssm_analyze() run must reproduce that
  # run's intervals exactly -- same replicate values, same quantile path.
  # The circumplex_ssm object stores no replicate matrix, so the bootstrap
  # is reconstructed here: boot::boot() draws its index array from the
  # master RNG before dispatching, so the same seed, data, R, and strata
  # reproduce the identical replicate matrix (t) the run used internally.
  data("aw2009")
  set.seed(12345)
  res <- ssm_analyze(aw2009, scales = 1:8)

  bs_input <- cbind(aw2009[1:8], Group = factor(rep("All", nrow(aw2009))))
  angles_rad <- as_radian(as_degree(octants()))
  set.seed(12345)
  bs <- boot::boot(
    data = bs_input,
    statistic = function(.data, index) {
      resample <- .data[index, ]
      mat <- as.matrix(resample[1:8])
      grp <- as.integer(resample[[9]])
      ssm_by_group(mean_scores(mat, grp, TRUE), angles_rad, FALSE)
    },
    R = 2000,
    strata = bs_input$Group
  )
  # Replicate columns are (e, x, y, a, d, fit); the adapter recomputes a and
  # d from (e, x, y) by the identical formulas, so the draws are bit-equal
  # (compare in degrees -- both sides converted once by the same expression;
  # a radian roundtrip would add a rounding step to only one side).
  adapter <- ssm_draws(bs$t[, 1:3], type = "parameters")
  expect_identical(as.numeric(adapter$draws[, "a"]), as.numeric(bs$t[, 4]))
  expect_identical(as.numeric(adapter$draws[, "d"]),
                   as.numeric(as_degree(as_radian(bs$t[, 5]))))
  for (p in c("e", "x", "y", "a", "d")) {
    expect_identical(
      as.numeric(adapter$results[[paste0(p, "_lci")]]),
      as.numeric(res$results[[paste0(p, "_lci")]])
    )
    expect_identical(
      as.numeric(adapter$results[[paste0(p, "_uci")]]),
      as.numeric(res$results[[paste0(p, "_uci")]])
    )
  }
})

test_that("shape B equals shape A applied to the per-row (e, x, y)", {
  # Oracle 3 (shape consistency): for any profile-draws matrix, shape B must
  # equal shape A applied to the per-row (e, x, y) computed from those
  # profiles -- exact by construction, and the only oracle exercising the
  # dispatch/column-mapping channels together.
  set.seed(7)
  pdraws <- matrix(rnorm(10 * 8, mean = 1), ncol = 8)
  resB <- ssm_draws(pdraws, angles = octants())
  resA <- ssm_draws(resB$draws[, c("e", "x", "y")], type = "parameters")
  expect_identical(resA$draws[, c("e", "x", "y", "a", "d")],
                   resB$draws[, c("e", "x", "y", "a", "d")])
  for (p in c("e", "x", "y", "a", "d")) {
    for (s in c("est", "lci", "uci")) {
      expect_identical(
        as.numeric(resA$results[[paste0(p, "_", s)]]),
        as.numeric(resB$results[[paste0(p, "_", s)]])
      )
    }
  }
})

test_that("repeated observed profiles reproduce the point estimates", {
  # Oracle 1, second clause: shape-B draws all equal to one observed profile
  # must reproduce that profile's point estimates (constant draws: medians
  # and circular mean collapse to the single value).
  theta <- as.numeric(octants()) * pi / 180
  profile <- 1 + 2 * cos(theta) + 3 * sin(theta)
  obs <- ssm_parameters(profile)
  pdraws <- matrix(rep(profile, 4), nrow = 4, byrow = TRUE)
  res <- ssm_draws(pdraws, angles = octants())
  expect_equal(res$results$e_est, obs$Elev)
  expect_equal(res$results$x_est, obs$Xval)
  expect_equal(res$results$y_est, obs$Yval)
  expect_equal(res$results$a_est, obs$Ampl)
  expect_equal(as.numeric(res$results$d_est), as.numeric(obs$Disp))
  expect_equal(res$results$fit_est, obs$Fit)
})

test_that("a pole-straddling draw pair wraps and its circular mean reports 360", {
  # Closed-form 4-row fixture (arithmetic in comments). Parameter draws at
  # unit amplitude with directions 340, 350, 10, 20 degrees:
  #   circular mean: sin sums cancel in symmetric pairs (sum = 0) and the
  #   cos sum is positive, so atan2(0, +) = 0 -> the exact pole, reported
  #   as 360 (D-003/M20 convention).
  #   circular quantiles (quantile.circumplex_radian): center on the
  #   circular mean (0/360), centered angles = (-20, -10, 10, 20); R type-7
  #   quantiles of 4 values: p = .025 -> h = 3*.025 + 1 = 1.075 ->
  #   -20 + 0.075*10 = -19.25; p = .975 -> h = 3.925 -> 10 + 0.925*10 =
  #   19.25; re-wrapped: lci = 340.75, uci = 19.25 -- the interval wraps
  #   through 0/360 (naive linear quantiles would instead report ~[10, 350]
  #   and exclude the true concentration region).
  degs <- c(340, 350, 10, 20)
  rads <- degs * pi / 180
  draws <- cbind(1, cos(rads), sin(rads))
  res <- ssm_draws(draws, type = "parameters")
  expect_equal(as.numeric(res$results$d_est), 360)
  expect_equal(as.numeric(res$results$d_lci), 340.75)
  expect_equal(as.numeric(res$results$d_uci), 19.25)
  expect_gt(as.numeric(res$results$d_lci), as.numeric(res$results$d_uci))
})

test_that("an all-flat profile-draws matrix honors the all-NA contract", {
  # Every draw is a flat profile: displacement and fit are undefined for
  # every draw, so their point summaries and interval endpoints are all NA
  # (never invented); e/x/y/a remain defined (a = 0).
  pdraws <- matrix(rep(c(1, 2), each = 8), nrow = 2, byrow = TRUE)
  w <- capture_warnings(res <- ssm_draws(pdraws, angles = octants()))
  expect_match(w, "displacement point summary is undefined", all = FALSE)
  expect_match(w, "2 of 2 posterior draws", all = FALSE)
  expect_true(is.na(res$results$d_est))
  expect_true(is.na(res$results$d_lci))
  expect_true(is.na(res$results$d_uci))
  expect_true(is.na(res$results$fit_est))
  expect_equal(res$results$e_est, 1.5)
  expect_equal(res$results$a_est, 0)
  expect_equal(res$results$a_lci, 0)
  expect_equal(res$results$a_uci, 0)
})

test_that("draws concentrated at the exact pole summarize as 360 throughout", {
  # Parameter draws exactly on the positive x-axis: every d draw is the
  # exact pole. Point summary and both interval endpoints report 360, the
  # package's LM = 360 label for the 0/360 pole (D-003/M20).
  draws <- cbind(c(1, 2, 1, 2), 1, 0)
  res <- ssm_draws(draws, type = "parameters")
  expect_equal(as.numeric(res$results$d_est), 360)
  expect_equal(as.numeric(res$results$d_lci), 360)
  expect_equal(as.numeric(res$results$d_uci), 360)
})

test_that("a tiny-negative-direction draw wraps to 360, never 0 (modu parity)", {
  # atan2(-1e-17, 1) = -1e-17: the kernel's modu() adds 2*pi (reporting the
  # pole as 360, D-003); R's %% would second-reduce it to 0 and break
  # bit-parity with kernel-computed replicates. Regression for the adapter's
  # single-correction wrap.
  draws <- rbind(c(1, 1, -1e-17), c(2, 1, -1e-17))
  res <- ssm_draws(draws, type = "parameters")
  expect_equal(as.numeric(res$draws[, "d"]), c(360, 360))
  expect_equal(as.numeric(res$results$d_est), 360)
})
