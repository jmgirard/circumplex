# ssm_trajectory(): coefficients or draws to a certified trajectory (M152) ---
#
# The reference below is the growth vignette's hidden draw function and
# per-wave loop as they stood before the vignette was rewritten on the
# helper (vignettes/growth-ssm-analysis.Rmd.orig at the M151 merge, chunks
# `draws` and `trajectory`). The statements are copied verbatim; three things
# differ. The per-wave loop is wrapped in a function of the draws matrix and
# the waves, in place of the vignette's `waves <- 0:4` and `trajectory <-`
# assignments. The seed is set by each test. The draws come from the fixed
# effects that tests/testthat/fixtures/growth-fixef.rds holds (generator
# data-raw/growth-fixef.R) in place of a live glmmTMB fit, so the tests need
# no glmmTMB.
#
# Seeds are pinned in this file. The seed 20260716 is the growth vignette's.

fixture <- readRDS(test_path("fixtures", "growth-fixef.rds"))
fx_growth <- fixture$simulated_growth
fx_origin <- fixture$simulated_growth_origin
coef_names <- c("dve", "dvx", "dvy", "dve:wave", "dvx:wave", "dvy:wave")
traj_cols <- c(
  "wave",
  paste0(rep(c("e", "x", "y", "a", "d"), each = 3), c("_est", "_lci", "_uci")),
  "certified"
)
# A printed table row is a mark or a space, the time, then a number; the
# notes below the table never carry two leading numbers.
row_re <- "^[ *]\\s+[0-9]+\\s+-?[0-9]"

# The vignette's reference, verbatim ------------------------------------------

# MVN draws of the coefficient vector (eigen root, robust to tiny
# negative eigenvalues from floating point)
mvn_draw <- function(n_draws, mu, sigma) {
  eig <- eigen(sigma, symmetric = TRUE)
  root <- eig$vectors %*% (sqrt(pmax(eig$values, 0)) * t(eig$vectors))
  sweep(matrix(rnorm(n_draws * length(mu)), nrow = n_draws) %*% root,
        2, mu, "+")
}

# The per-wave loop, given the draws matrix B with the fixed effects' names
# as its column names.
vignette_trajectory <- function(B, waves) {
  per_wave <- lapply(waves, function(t) {
    draws_t <- cbind(
      e = B[, "dve"] + t * B[, "dve:wave"],
      x = B[, "dvx"] + t * B[, "dvx:wave"],
      y = B[, "dvy"] + t * B[, "dvy:wave"]
    )
    ssm_draws(draws_t, type = "parameters")
  })

  data.frame(
    wave = waves,
    a_est = sapply(per_wave, function(s) s$results$a_est),
    a_lci = sapply(per_wave, function(s) s$results$a_lci),
    a_uci = sapply(per_wave, function(s) s$results$a_uci),
    d_est = sapply(per_wave, function(s) as.numeric(s$results$d_est)),
    d_lci = sapply(per_wave, function(s) as.numeric(s$results$d_lci)),
    d_uci = sapply(per_wave, function(s) as.numeric(s$results$d_uci)),
    certified = sapply(per_wave, function(s) s$details$certified)
  )
}

# AC1: shape, class, the time attribute, and the vignette loop ----------------

test_that("ssm_trajectory returns the certified table with one row per time", {
  set.seed(20260716)
  out <- ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = 0:4)

  expect_s3_class(out, "circumplex_ssm_trajectory")
  expect_s3_class(out, "data.frame")
  expect_identical(attr(out, "time"), "wave")
  expect_identical(names(out), traj_cols)
  expect_equal(nrow(out), 5L)
  expect_equal(out$wave, 0:4)
  expect_true(is.logical(out$certified))
  for (col in traj_cols[-c(1, 17)]) {
    expect_true(is.numeric(out[[col]]), info = col)
    expect_false(inherits(out[[col]], "circumplex_degree"), info = col)
  }
})

test_that("the time argument names the time column and the attribute", {
  set.seed(1)
  coef <- fx_growth$coef
  names(coef) <- sub("wave", "month", names(coef))
  V <- fx_growth$vcov
  dimnames(V) <- list(names(coef), names(coef))
  out <- ssm_trajectory(coef, V, times = c(0, 6, 12), time = "month")
  expect_identical(names(out)[1], "month")
  expect_identical(attr(out, "time"), "month")
  expect_equal(out$month, c(0, 6, 12))
})

test_that("ssm_trajectory matches the vignette's per-wave loop to 1e-12", {
  waves <- 0:4
  set.seed(20260716)
  B <- mvn_draw(4000, fx_growth$coef, fx_growth$vcov)
  colnames(B) <- names(fx_growth$coef)
  ref <- vignette_trajectory(B, waves)

  set.seed(20260716)
  out <- ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = waves)

  ad_cols <- c("a_est", "a_lci", "a_uci", "d_est", "d_lci", "d_uci")
  expect_equal(as.data.frame(out)[ad_cols], ref[ad_cols], tolerance = 1e-12)
  expect_identical(out$certified, ref$certified)

  # The same on the near-origin fit, whose wave 2 is uncertified.
  set.seed(20260716)
  B2 <- mvn_draw(4000, fx_origin$coef, fx_origin$vcov)
  colnames(B2) <- names(fx_origin$coef)
  ref2 <- vignette_trajectory(B2, waves)
  set.seed(20260716)
  out2 <- ssm_trajectory(fx_origin$coef, fx_origin$vcov, times = waves)
  expect_equal(as.data.frame(out2)[ad_cols], ref2[ad_cols], tolerance = 1e-12)
  expect_identical(out2$certified, ref2$certified)
})

test_that("a non-symmetric vcov is refused by name", {
  V <- fx_growth$vcov
  V[1, 2] <- V[1, 2] + 1e-3
  expect_error(
    ssm_trajectory(fx_growth$coef, V, times = 0:4),
    "`vcov` must be a symmetric"
  )
})

test_that("coef and vcov must agree in length, names and content", {
  expect_error(
    ssm_trajectory(fx_growth$coef, fx_growth$vcov[1:5, 1:5], times = 0:4),
    "`vcov` must be a square matrix with one row and column per `coef`"
  )
  V <- fx_growth$vcov
  dimnames(V) <- list(rev(coef_names), rev(coef_names))
  expect_error(
    ssm_trajectory(fx_growth$coef, V, times = 0:4),
    "dimnames of `vcov` must equal `names\\(coef\\)`"
  )
  expect_error(
    ssm_trajectory(unname(fx_growth$coef), fx_growth$vcov, times = 0:4),
    "`coef` must be a named numeric vector"
  )
  coef_na <- fx_growth$coef
  coef_na[[2]] <- NA
  expect_error(
    ssm_trajectory(coef_na, fx_growth$vcov, times = 0:4),
    "`coef` must be a named numeric vector with no missing"
  )
  V_na <- fx_growth$vcov
  V_na[2, 3] <- V_na[3, 2] <- NA
  expect_error(
    ssm_trajectory(fx_growth$coef, V_na, times = 0:4),
    "`vcov` must be a symmetric numeric matrix with no missing"
  )
})

test_that("times, time, interval and n_draws are validated by name", {
  expect_error(
    ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = numeric(0)),
    "`times` must be a numeric vector"
  )
  expect_error(
    ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = c(0, NA)),
    "`times` must be a numeric vector"
  )
  expect_error(
    ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = 0:4, time = ""),
    "`time` must be a single column name"
  )
  expect_error(
    ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = 0:4,
                   time = "a_est"),
    "`time` cannot be \"a_est\""
  )
  expect_error(
    ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = 0:4,
                   interval = 1),
    "`interval` must be a single number strictly between 0 and 1"
  )
  expect_error(
    ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = 0:4,
                   n_draws = 1),
    "`n_draws` must be a single whole number of at least 2"
  )
})

# AC2: the draws shape and the argument combinations ------------------------

test_that("a draws matrix skips the drawing step and matches shape one", {
  set.seed(7)
  out1 <- ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = 0:4)
  set.seed(7)
  B <- circumplex:::mvn_draws(4000, fx_growth$coef, fx_growth$vcov)
  colnames(B) <- names(fx_growth$coef)
  out2 <- ssm_trajectory(times = 0:4, draws = B)
  # The two shapes differ only in the recorded input shape (M155).
  expect_identical(attr(out1, "input"), "coef_vcov")
  expect_identical(attr(out2, "input"), "draws")
  attr(out1, "input") <- attr(out2, "input") <- NULL
  expect_identical(out1, out2)

  # No random number is drawn on the draws path.
  set.seed(99)
  before <- .Random.seed
  invisible(ssm_trajectory(times = 0:4, draws = B))
  expect_identical(.Random.seed, before)
})

test_that("partial argument combinations are refused by name", {
  B <- matrix(rnorm(60), 10, 6, dimnames = list(NULL, coef_names))
  expect_error(
    ssm_trajectory(coef = fx_growth$coef, times = 0:4),
    "Supply `coef` and `vcov` together, or `draws` alone; received `coef`\\."
  )
  expect_error(
    ssm_trajectory(vcov = fx_growth$vcov, times = 0:4),
    "Supply `coef` and `vcov` together, or `draws` alone; received `vcov`\\."
  )
  expect_error(
    ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = 0:4, draws = B),
    "received `coef` and `vcov` and `draws`\\."
  )
  expect_error(
    ssm_trajectory(times = 0:4),
    "received none of them\\."
  )
})

test_that("a draws matrix is validated by name", {
  B <- matrix(rnorm(60), 10, 6, dimnames = list(NULL, coef_names))
  expect_error(
    ssm_trajectory(times = 0:4, draws = unname(B)),
    "`draws` must have a name on every column"
  )
  expect_error(
    ssm_trajectory(times = 0:4, draws = B[1, , drop = FALSE]),
    "`draws` must be a numeric matrix of at least two rows"
  )
  B_na <- B
  B_na[1, 1] <- NA
  expect_error(
    ssm_trajectory(times = 0:4, draws = B_na),
    "`draws` must be a numeric matrix of at least two rows with no missing"
  )
  B_dup <- cbind(B, b_dve = 1)
  expect_error(
    ssm_trajectory(times = 0:4, draws = B_dup),
    "duplicated column names once any `b_` prefix is dropped"
  )
})

# AC3: numeric oracles ----------------------------------------------------------

test_that("closed form: zero vcov gives atan2 and the norm at every time", {
  # x(t) = 1, y(t) = -0.5 + 0.3 t: the direction crosses 0/360 between
  # t = 1 and t = 2, never landing on the pole itself.
  coef <- c(dve = 0.4, dvx = 1, dvy = -0.5,
            "dve:wave" = 0.1, "dvx:wave" = 0, "dvy:wave" = 0.3)
  V <- matrix(0, 6, 6, dimnames = list(names(coef), names(coef)))
  times <- 0:4
  set.seed(3)
  out <- ssm_trajectory(coef, V, times = times, n_draws = 50)

  x_t <- 1 + 0 * times
  y_t <- -0.5 + 0.3 * times
  d_t <- (atan2(y_t, x_t) * 180 / pi) %% 360
  expect_true(all(out$d_est >= 0 & out$d_est < 360))
  expect_equal(out$d_est, d_t, tolerance = 1e-8)
  expect_equal(out$a_est, sqrt(x_t^2 + y_t^2), tolerance = 1e-8)
  expect_equal(out$e_est, 0.4 + 0.1 * times, tolerance = 1e-8)
  expect_true(any(diff(d_t) < 0))
})

test_that("linear interval: x and y bounds match the normal-theory bounds", {
  skip_on_cran()
  times <- 0:4
  set.seed(20260716)
  out <- ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = times,
                        n_draws = 2e5)
  z <- stats::qnorm(0.975)
  for (par in c("x", "y")) {
    int <- paste0("dv", par)
    slp <- paste0("dv", par, ":wave")
    for (i in seq_along(times)) {
      L <- setNames(numeric(6), coef_names)
      L[int] <- 1
      L[slp] <- times[i]
      est <- sum(L * fx_growth$coef)
      se <- sqrt(as.numeric(t(L) %*% fx_growth$vcov %*% L))
      # An absolute bound of 0.01, as the criterion states. Dropping one
      # slope covariance from V moves a wave-4 bound by about 0.017, so the
      # bound has power (measured 2026-09-24 on the fixture).
      expect_lt(abs(out[[paste0(par, "_lci")]][i] - (est - z * se)), 0.01,
                label = paste(par, times[i], "lci"))
      expect_lt(abs(out[[paste0(par, "_uci")]][i] - (est + z * se)), 0.01,
                label = paste(par, times[i], "uci"))
    }
  }
})

test_that("boundary cases: the seam, the origin and the flat profile", {
  set.seed(20260716)
  out <- ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = 0:4)
  for (col in c("d_est", "d_lci", "d_uci")) {
    expect_true(all(out[[col]] >= 0 & out[[col]] < 360), info = col)
  }
  # Which waves straddle the seam is decided independently of the storage
  # convention under test: the delta-method interval of the direction, on
  # the signed branch (-180, 180], contains 0 at exactly those waves. Under
  # this seed the margins are over 0.4 degrees (wave 1's upper bound at
  # -0.6, wave 2's lower bound at -0.4).
  z <- stats::qnorm(0.975)
  straddle_expected <- vapply(0:4, function(t) {
    Lx <- setNames(numeric(6), coef_names)
    Lx[c("dvx", "dvx:wave")] <- c(1, t)
    Ly <- setNames(numeric(6), coef_names)
    Ly[c("dvy", "dvy:wave")] <- c(1, t)
    x <- sum(Lx * fx_growth$coef)
    y <- sum(Ly * fx_growth$coef)
    g <- (-y * Lx + x * Ly) / (x^2 + y^2)
    se <- sqrt(as.numeric(t(g) %*% fx_growth$vcov %*% g)) * 180 / pi
    d <- atan2(y, x) * 180 / pi
    d - z * se < 0 && d + z * se > 0
  }, logical(1))
  expect_identical(straddle_expected, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  straddles <- out$d_lci > out$d_uci
  expect_identical(straddles, straddle_expected)
  expect_true(all(out$certified))

  set.seed(20260716)
  out2 <- ssm_trajectory(fx_origin$coef, fx_origin$vcov, times = 0:4)
  expect_identical(out2$certified, c(TRUE, TRUE, FALSE, TRUE, TRUE))

  # A flat trajectory: zero x and y at every time, zero covariance.
  coef <- c(dve = 0.4, dvx = 0, dvy = 0,
            "dve:wave" = 0, "dvx:wave" = 0, "dvy:wave" = 0)
  V <- matrix(0, 6, 6, dimnames = list(names(coef), names(coef)))
  # ssm_draws() warns at each time that the displacement is undefined and
  # that every draw is degenerate; both pass through, and neither is an
  # error.
  warned <- capture_warnings(
    flat <- ssm_trajectory(coef, V, times = 0:2, n_draws = 20)
  )
  expect_match(warned, "displacement point summary is undefined", all = FALSE)
  expect_match(warned, "degenerate", all = FALSE)
  expect_true(all(is.na(flat$d_est)))
  expect_identical(flat$certified, c(FALSE, FALSE, FALSE))
  expect_equal(flat$a_est, c(0, 0, 0))
  expect_equal(flat$e_est, c(0.4, 0.4, 0.4))
})

# AC4: the joint-fit refusal --------------------------------------------------

test_that("an all-zero x-y cross block is refused as a separate fit", {
  V <- fx_growth$vcov
  xs <- c("dvx", "dvx:wave")
  ys <- c("dvy", "dvy:wave")
  V[xs, ys] <- 0
  V[ys, xs] <- 0
  expect_error(
    ssm_trajectory(fx_growth$coef, V, times = 0:4),
    "zero covariance between x\\(t\\) and y\\(t\\) at every time.*joint fit"
  )
  # The unmodified fixture passes.
  set.seed(1)
  expect_no_error(ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = 0:4))
})

test_that("one nonzero cross entry at any of the four positions passes", {
  xs <- c("dvx", "dvx:wave")
  ys <- c("dvy", "dvy:wave")
  for (i in xs) {
    for (j in ys) {
      V <- matrix(0, 6, 6, dimnames = list(coef_names, coef_names))
      diag(V) <- 1e-4
      V[i, j] <- V[j, i] <- 1e-5
      expect_true(isSymmetric(V))
      set.seed(1)
      expect_no_error(
        ssm_trajectory(fx_growth$coef, V, times = 0:4, n_draws = 50)
      )
    }
  }
  # And the diagonal-only matrix, the same four positions all zero, stops.
  V <- matrix(0, 6, 6, dimnames = list(coef_names, coef_names))
  diag(V) <- 1e-4
  expect_error(
    ssm_trajectory(fx_growth$coef, V, times = 0:4, n_draws = 50),
    "joint fit"
  )
})

test_that("the refusal exempts an all-zero vcov and the draws shape", {
  V <- matrix(0, 6, 6, dimnames = list(coef_names, coef_names))
  expect_no_error(
    ssm_trajectory(fx_growth$coef, V, times = 0:4, n_draws = 20)
  )
  # Draws whose x and y columns are independent are summarized as given.
  set.seed(5)
  B <- sapply(coef_names, function(nm) rnorm(200, fx_growth$coef[[nm]], 0.01))
  expect_no_error(ssm_trajectory(times = 0:4, draws = B))
})

test_that("a custom contrast sharing no covarying coefficient is refused", {
  # Two separate two-coefficient models for x and y, each with its own
  # (nonzero) covariance, assembled block-diagonally: the x and y rows of
  # the contrast never share a coefficient with nonzero covariance.
  coef <- c(e0 = 0.4, x0 = 0.6, x1 = 0.01, y0 = -0.1, y1 = 0.05)
  V <- matrix(0, 5, 5, dimnames = list(names(coef), names(coef)))
  V["e0", "e0"] <- 1e-3
  V[c("x0", "x1"), c("x0", "x1")] <- matrix(c(1e-3, 2e-4, 2e-4, 1e-4), 2)
  V[c("y0", "y1"), c("y0", "y1")] <- matrix(c(1e-3, 3e-4, 3e-4, 1e-4), 2)
  ctr <- function(t) {
    rbind(e = c(1, 0, 0, 0, 0), x = c(0, 1, t, 0, 0), y = c(0, 0, 0, 1, t))
  }
  expect_error(
    ssm_trajectory(coef, V, times = 0:4, contrast = ctr),
    "joint fit"
  )
  V["x0", "y0"] <- V["y0", "x0"] <- 1e-5
  set.seed(1)
  expect_no_error(ssm_trajectory(coef, V, times = 0:4, contrast = ctr,
                                 n_draws = 50))
})

# AC5: the default contrast's names, the brms prefix, a custom contrast --------

test_that("a brms-shaped draws matrix passes and extra columns are ignored", {
  set.seed(11)
  B <- circumplex:::mvn_draws(300, fx_growth$coef, fx_growth$vcov)
  colnames(B) <- names(fx_growth$coef)
  ref <- ssm_trajectory(times = 0:4, draws = B)

  brms <- cbind(
    lp__ = rnorm(300), B[, 4:6], sd_person__dve = abs(rnorm(300)),
    B[, 1:3], cor_person__dve__dvx = runif(300, -1, 1),
    sigma_dve = abs(rnorm(300))
  )
  colnames(brms)[colnames(brms) %in% coef_names] <-
    paste0("b_", colnames(brms)[colnames(brms) %in% coef_names])
  expect_true(all(c("b_dve", "b_dvx:wave", "lp__") %in% colnames(brms)))
  out <- ssm_trajectory(times = 0:4, draws = brms)
  expect_identical(out, ref)
})

test_that("a missing coefficient name is refused by name", {
  coef <- fx_growth$coef
  names(coef)[names(coef) == "dvy:wave"] <- "dvy:time"
  V <- fx_growth$vcov
  dimnames(V) <- list(names(coef), names(coef))
  expect_error(
    ssm_trajectory(coef, V, times = 0:4),
    "Coefficient name `dvy:wave` not found in `names\\(coef\\)`"
  )
  B <- matrix(rnorm(60), 10, 6, dimnames = list(NULL, names(coef)))
  expect_error(
    ssm_trajectory(times = 0:4, draws = B),
    "Coefficient name `dvy:wave` not found in `colnames\\(draws\\)`"
  )
  # With `time = "month"` the slope names change with it.
  expect_error(
    ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = 0:4,
                   time = "month"),
    "Coefficient names `dve:month`, `dvx:month`, `dvy:month` not found"
  )
})

test_that("a custom contrast with a quadratic term recovers x(t)", {
  coef <- c(e = 0.3, x0 = 0.5, x1 = 0.1, x2 = -0.02, y0 = 0.2, y1 = 0.05)
  V <- matrix(0, 6, 6)
  ctr <- function(t) {
    rbind(e = c(1, 0, 0, 0, 0, 0),
          x = c(0, 1, t, t^2, 0, 0),
          y = c(0, 0, 0, 0, 1, t))
  }
  times <- c(0, 1.5, 3, 4.5)
  set.seed(2)
  out <- ssm_trajectory(coef, V, times = times, contrast = ctr, n_draws = 30,
                        time = "t")
  expect_equal(out$x_est, 0.5 + 0.1 * times - 0.02 * times^2,
               tolerance = 1e-8)
  expect_equal(out$y_est, 0.2 + 0.05 * times, tolerance = 1e-8)
  expect_equal(out$t, times)
})

test_that("a contrast returning the wrong shape is refused", {
  bad <- function(t) matrix(0, 2, 6)
  expect_error(
    ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = 0:4,
                   contrast = bad),
    "`contrast` must return a numeric 3 by 6 matrix.*returned a 2 by 6 matrix"
  )
  expect_error(
    ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = 0:4,
                   contrast = "x"),
    "`contrast` must be NULL or a function"
  )
})

# AC6: print and plot ---------------------------------------------------------

test_that("print rounds to digits, marks uncertified rows, states the caution", {
  set.seed(20260716)
  out2 <- ssm_trajectory(fx_origin$coef, fx_origin$vcov, times = 0:4)
  txt <- capture.output(res <- print(out2))
  expect_identical(res, out2)

  # Every value on a table row prints with `digits` decimals; the time column
  # is printed as given.
  body <- txt[grepl(row_re, txt)]
  expect_length(body, 5L)
  expect_true(all(grepl("[0-9]\\.[0-9]{2}( |$)", body)))
  expect_false(any(grepl("[0-9]\\.[0-9]{3}", body)))
  # Wave 2 is the uncertified row and carries the mark; the others do not.
  marked <- grepl("^\\*", body)
  expect_identical(marked, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_match(txt, "Uncertified", all = FALSE)
  expect_match(txt, "Caution: intervals", all = FALSE)
  expect_match(txt, "Section 7", all = FALSE)

  txt4 <- capture.output(print(out2, digits = 4))
  body4 <- txt4[grepl(row_re, txt4)]
  expect_true(any(grepl("[0-9]\\.[0-9]{4}( |$)", body4)))

  # A fully certified table prints no mark and no uncertified note, and the
  # caution still.
  set.seed(20260716)
  out <- ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = 0:4)
  txt_ok <- capture.output(print(out))
  expect_false(any(grepl("^\\*", txt_ok)))
  expect_false(any(grepl("Uncertified", txt_ok)))
  expect_match(txt_ok, "Caution: intervals", all = FALSE)

  # A column subset keeps the class and may drop `certified`; it prints with
  # no mark.
  txt_sub <- capture.output(print(out2[, 1:4]))
  expect_false(any(grepl("^\\*", txt_sub)))
  expect_length(txt_sub[grepl(row_re, txt_sub)], 5L)
})

test_that("the object records its input shape and print branches on it", {
  # M155 AC1 and AC2. The REML caution is the M152 text, pinned here so a
  # wording change under `coef`/`vcov` reddens.
  reml <- paste0(
    "Caution: intervals from a fitted model's fixed-effect covariance ",
    "condition on its estimated variance components, and are too narrow ",
    "at small samples. See vignette(\"growth-ssm-analysis\"), Section 7."
  )
  # The caution is the last text printed: nothing but blank lines follows it.
  caution_of <- function(txt) {
    i <- grep("^  Caution:", txt)
    expect_length(i, 1L)
    j <- i
    while (j < length(txt) && nzchar(txt[j + 1])) j <- j + 1
    expect_false(any(nzchar(txt[-seq_len(j)])))
    paste(trimws(txt[i:j]), collapse = " ")
  }

  set.seed(20260716)
  out_cv <- ssm_trajectory(fx_origin$coef, fx_origin$vcov, times = 0:4)
  expect_identical(attr(out_cv, "input"), "coef_vcov")
  expect_identical(names(out_cv), traj_cols)
  txt_cv <- capture.output(print(out_cv))
  expect_identical(caution_of(txt_cv), reml)
  expect_false(any(grepl("supplied draws", txt_cv)))

  # The draws twin: the same origin fixture drawn by the reference, so its
  # wave-2 row is uncertified too.
  set.seed(20260716)
  B <- mvn_draw(4000, fx_origin$coef, fx_origin$vcov)
  colnames(B) <- coef_names
  out_dr <- ssm_trajectory(times = 0:4, draws = B)
  expect_identical(attr(out_dr, "input"), "draws")
  expect_identical(names(out_dr), traj_cols)
  txt_dr <- capture.output(print(out_dr))
  body <- txt_dr[grepl(row_re, txt_dr)]
  expect_identical(grepl("^\\*", body), c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_match(txt_dr, "Uncertified", all = FALSE)
  cd <- caution_of(txt_dr)
  expect_match(cd, "summarize the supplied draws as given")
  expect_match(cd, "coverage depends on how the draws were produced")
  expect_match(cd, "joint fit was not checked")
  expect_match(cd, "Sections 7 and 10")
  expect_false(any(grepl("fixed-effect covariance", txt_dr)))
  # The table lines are the same under both shapes' print paths: the same
  # object printed with the attribute swapped gives identical rows.
  swapped <- out_dr
  attr(swapped, "input") <- "coef_vcov"
  txt_sw <- capture.output(print(swapped))
  expect_identical(txt_sw[grepl(row_re, txt_sw)], body)
  expect_identical(caution_of(txt_sw), reml)

  # A column subset drops the attribute: the shape-neutral caution, from
  # either shape.
  for (obj in list(out_dr, out_cv)) {
    sub <- obj[, 1:4]
    expect_null(attr(sub, "input"))
    cs <- caution_of(capture.output(print(sub)))
    expect_match(cs, "input shape is not recorded")
    expect_match(cs, "Sections 7 and 10")
    expect_false(grepl("fixed-effect covariance", cs))
    expect_false(grepl("supplied draws", cs))
  }

  # rbind keeps a shared shape and drops a mixed one.
  same <- rbind(out_cv, out_cv)
  expect_s3_class(same, "circumplex_ssm_trajectory")
  expect_identical(attr(same, "input"), "coef_vcov")
  expect_identical(nrow(same), 10L)
  mixed <- rbind(out_cv, out_dr)
  expect_null(attr(mixed, "input"))
  expect_identical(attr(mixed, "time"), "wave")
  expect_match(caution_of(capture.output(print(mixed))),
               "input shape is not recorded")
})

test_that("an undecided certified verdict prints as uncertified", {
  set.seed(20260716)
  out <- ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = 0:4)
  out$certified[4] <- NA
  txt <- capture.output(print(out))
  body <- txt[grepl(row_re, txt)]
  expect_identical(grepl("^\\*", body), c(FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_match(txt, "Uncertified", all = FALSE)
})

test_that("a vcov that is not positive semidefinite is refused", {
  V <- fx_growth$vcov
  # A cross covariance ten times the product of the standard deviations.
  V["dvx", "dvy"] <- V["dvy", "dvx"] <- 10 * sqrt(V["dvx", "dvx"] * V["dvy", "dvy"])
  expect_error(
    ssm_trajectory(fx_growth$coef, V, times = 0:4),
    "`vcov` is not a valid covariance matrix"
  )
  # The fitted fixture, whose smallest eigenvalue is positive, passes.
  set.seed(1)
  expect_no_error(ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = 0:4,
                                 n_draws = 20))
})

test_that("an infinite draw is refused", {
  B <- matrix(rnorm(60), 10, 6, dimnames = list(NULL, coef_names))
  B[3, 2] <- Inf
  expect_error(
    ssm_trajectory(times = 0:4, draws = B),
    "no missing or infinite values"
  )
})

test_that("a contrast whose rows are named out of order is refused", {
  swapped <- function(t) {
    rbind(x = c(0, 1, 0, 0, t, 0), e = c(1, 0, 0, t, 0, 0),
          y = c(0, 0, 1, 0, 0, t))
  }
  expect_error(
    ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = 0:4,
                   contrast = swapped),
    "rows in the order e, x, y"
  )
  # Unnamed rows are taken in order.
  unnamed <- function(t) unname(swapped(t))[c(2, 1, 3), ]
  set.seed(4)
  out <- ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = 0:4,
                        contrast = unnamed, n_draws = 20)
  set.seed(4)
  ref <- ssm_trajectory(fx_growth$coef, fx_growth$vcov, times = 0:4,
                        n_draws = 20)
  expect_identical(out, ref)
})

test_that("the all-zero exemption reads the coefficients the contrast uses", {
  # The six-name block is zero; a seventh coefficient carries variance.
  coef <- c(fx_growth$coef, age = 0.1)
  V <- matrix(0, 7, 7, dimnames = list(names(coef), names(coef)))
  V["age", "age"] <- 1e-3
  expect_no_error(ssm_trajectory(coef, V, times = 0:4, n_draws = 20))
})

test_that("n_draws is not used, and not checked, on the draws path", {
  B <- matrix(rnorm(60), 10, 6, dimnames = list(NULL, coef_names))
  expect_no_error(ssm_trajectory(times = 0:4, draws = B, n_draws = 1))
})

test_that("the plot method reads the time attribute and matches the table method", {
  skip_on_cran()
  set.seed(20260716)
  out2 <- ssm_trajectory(fx_origin$coef, fx_origin$vcov, times = 0:4)
  p_method <- ssm_plot_trajectory(out2)
  p_table <- ssm_plot_trajectory(as.data.frame(out2), time = "wave")
  expect_s3_class(p_method, "ggplot")
  expect_identical(ggplot2::layer_data(p_method), ggplot2::layer_data(p_table))
  expect_equal(p_method$labels$x, "wave")

  # Options pass through, and a table stripped of its attribute is named.
  p_drop <- ssm_plot_trajectory(out2, drop_xy = TRUE)
  expect_identical(
    ggplot2::layer_data(p_drop),
    ggplot2::layer_data(ssm_plot_trajectory(as.data.frame(out2),
                                            time = "wave", drop_xy = TRUE))
  )
  sub <- out2[1:3, ]
  attr(sub, "time") <- NULL
  expect_error(ssm_plot_trajectory(sub), "no `time` attribute")
  expect_s3_class(ssm_plot_trajectory(sub, time = "wave"), "ggplot")
})

test_that("the trajectory object's plot renders as expected", {
  skip_if_not_installed("vdiffr")
  # A rendering guard only; the layer_data() identity above is the fence.
  set.seed(20260716)
  out2 <- ssm_trajectory(fx_origin$coef, fx_origin$vcov, times = 0:4)
  vdiffr::expect_doppelganger("ssm_trajectory object", ssm_plot_trajectory(out2))
})
