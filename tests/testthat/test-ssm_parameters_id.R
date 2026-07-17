test_that("ssm_parameters_id reproduces hand-computed closed-form fixtures", {
  # Octant angles theta_j = octants() = (90, 135, 180, 225, 270, 315, 360,
  # 45) degrees -- a full equally spaced set. Closed-form estimator
  # (src/parameters.cpp): e = mean(s); x = (2/8) * sum(s * cos(theta));
  # y = (2/8) * sum(s * sin(theta)); a = sqrt(x^2 + y^2);
  # d = atan2(y, x) mod 360; fit = 1 - SS_res / (var(s) * 7).
  #
  # Person 1 (pure first harmonic): s_j = 2 + 3*cos(theta_j) + 4*sin(theta_j).
  #   Over any full equally spaced octant set sum(cos) = sum(sin) =
  #   sum(cos*sin) = 0 and sum(cos^2) = sum(sin^2) = 4, so:
  #   e = 2 + (3*0 + 4*0)/8 = 2
  #   x = (2/8) * 3 * sum(cos^2) = (2/8) * 3 * 4 = 3
  #   y = (2/8) * 4 * sum(sin^2) = (2/8) * 4 * 4 = 4
  #   a = sqrt(3^2 + 4^2) = 5
  #   d = atan2(4, 3) = 0.9272952 rad = 53.13010 deg
  #   fit = 1 (exact first harmonic, zero residual)
  # Person 2 (exactly flat): s_j = 1.5 for all j.
  #   e = 1.5; x = y = a = 0; sd = 0 -> displacement and fit undefined (NA).
  # Person 3 (pure second harmonic): s_j = (1, 0, -1, 0, 1, 0, -1, 0) =
  #   cos(2*theta_j + 180), a pure second harmonic over these angles.
  #   e = mean(s) = 0; the second harmonic is orthogonal to the first over
  #   octants, so x = y = a = 0; variance is real (var = 4/7) but the
  #   first-harmonic amplitude is zero -> d = NA and fit = 0 by convention.
  theta <- as.numeric(octants()) * pi / 180
  p1 <- 2 + 3 * cos(theta) + 4 * sin(theta)
  p2 <- rep(1.5, 8)
  p3 <- c(1, 0, -1, 0, 1, 0, -1, 0)
  dat <- as.data.frame(rbind(p1, p2, p3))
  colnames(dat) <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  rownames(dat) <- NULL

  expect_warning(
    res <- ssm_parameters_id(dat, scales = 1:8),
    "2 of 3 person\\(s\\) have undefined displacement"
  )

  expect_s3_class(res, "circumplex_ssm_id")
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  expect_equal(res$id, 1:3)
  expect_equal(res$n_obs, c(1L, 1L, 1L))
  expect_equal(res$na_rate, c(0, 0, 0))
  expect_equal(res$Elev, c(2, 1.5, 0))
  expect_equal(res$Xval, c(3, 0, 0))
  expect_equal(res$Yval, c(4, 0, 0))
  expect_equal(res$Ampl, c(5, 0, 0))
  expect_equal(res$Disp, c(atan2(4, 3) * 180 / pi, NA, NA))
  expect_equal(res$Fit, c(1, NA, 0))
  # Displacement is reported in degrees [0, 360)
  expect_true(res$Disp[1] > 0 && res$Disp[1] < 360)
})

test_that("ssm_parameters_id aggregates within person via id before scoring", {
  # Person A appears at two occasions whose scores are the pure first
  # harmonic 2 + 3*cos + 4*sin shifted by +1 and -1: the within-person mean
  # is the harmonic itself, so A must recover (e, x, y, a) = (2, 3, 4, 5)
  # exactly. Person B is flat (1.5) at both occasions -> NA displacement.
  # Rows are interleaved (A, B, A, B) to prove first-appearance ordering and
  # that aggregation happens before (not after) the transform.
  theta <- as.numeric(octants()) * pi / 180
  h <- 2 + 3 * cos(theta) + 4 * sin(theta)
  dat <- as.data.frame(rbind(h + 1, rep(1.5, 8), h - 1, rep(1.5, 8)))
  colnames(dat) <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  dat$person <- c("A", "B", "A", "B")

  expect_warning(
    res <- ssm_parameters_id(dat, scales = 1:8, id = "person"),
    "1 of 2 person\\(s\\) have undefined displacement"
  )

  expect_equal(nrow(res), 2)
  expect_equal(res$person, c("A", "B"))
  expect_equal(res$n_obs, c(2L, 2L))
  expect_equal(res$Elev, c(2, 1.5))
  expect_equal(res$Xval, c(3, 0))
  expect_equal(res$Yval, c(4, 0))
  expect_equal(res$Ampl, c(5, 0))
  expect_equal(res$Disp, c(atan2(4, 3) * 180 / pi, NA))
})

test_that("ssm_parameters_id reports missingness and uses available occasions", {
  # Person A: two occasions, PA missing at occasion 2. The within-person mean
  # of PA uses the available occasion only (PA = 5); every other scale
  # averages both occasions. na_rate = 1 missing cell / 16 cells = 0.0625.
  # Person B: complete rows -> na_rate = 0.
  dat <- data.frame(
    PA = c(5, NA, 2, 2),
    BC = c(1, 1, 2, 2),
    DE = c(1, 1, 2, 2),
    FG = c(1, 1, 2, 2),
    HI = c(1, 1, 2, 2),
    JK = c(1, 1, 2, 2),
    LM = c(1, 1, 2, 2),
    NO = c(1, 1, 2, 2),
    person = c("A", "A", "B", "B")
  )
  expect_warning(
    res <- ssm_parameters_id(dat, scales = 1:8, id = "person"),
    "1 of 2 person\\(s\\) have undefined displacement"
  )
  expect_equal(res$na_rate, c(1 / 16, 0))
  # A's profile is (5, 1, 1, 1, 1, 1, 1, 1) with PA at theta_1 = 90 degrees:
  # e = 12/8 = 1.5;
  # x = (2/8) * (5*cos(90) + 1*sum(cos(theta_j), j != 1))
  #   = (2/8) * (0 + 1*(0 - 0)) = 0            [sum of all cos = 0]
  # y = (2/8) * (5*sin(90) + 1*sum(sin(theta_j), j != 1))
  #   = (2/8) * (5 + 1*(0 - 1)) = (2/8) * 4 = 1 [sum of all sin = 0]
  # a = 1; d = atan2(1, 0) = 90 degrees.
  expect_equal(res$Elev[1], 1.5)
  expect_equal(res$Xval[1], 0)
  expect_equal(res$Yval[1], 1)
  expect_equal(res$Ampl[1], 1)
  expect_equal(res$Disp[1], 90)
  # B's profile is exactly flat -> NA displacement, NA fit
  expect_true(is.na(res$Disp[2]))
  expect_true(is.na(res$Fit[2]))
})

test_that("ssm_parameters_id returns all-NA parameters for an all-NA scale", {
  # A scale with no observed values for a person has no within-person mean:
  # the whole profile is undefined and every parameter is NA (never a silent
  # drop -- the person keeps their row and na_rate exposes the cause).
  dat <- data.frame(
    PA = c(NA, NA),
    BC = c(1, 2), DE = c(1, 2), FG = c(1, 2),
    HI = c(1, 2), JK = c(1, 2), LM = c(1, 2), NO = c(1, 2),
    person = c("A", "A")
  )
  expect_warning(
    res <- ssm_parameters_id(dat, scales = 1:8, id = "person"),
    "1 of 1 person\\(s\\) have undefined displacement"
  )
  expect_equal(res$na_rate, 2 / 16)
  expect_true(all(is.na(res[c("Elev", "Xval", "Yval", "Ampl", "Disp", "Fit")])))
})

test_that("ssm_parameters_id validates its inputs", {
  dat <- data.frame(
    PA = 1, BC = 2, DE = 3, FG = 4, HI = 5, JK = 6, LM = 7, NO = 8,
    person = "A"
  )
  # data must be a data frame or matrix
  expect_error(ssm_parameters_id(1:8, scales = 1:8))
  # scales and angles must have equal length
  expect_error(ssm_parameters_id(dat, scales = 1:8, angles = c(0, 90)))
  # scales must be numeric columns
  expect_error(ssm_parameters_id(dat, scales = c(1:7, 9)))
  # id must be NULL or a single variable name/index
  expect_error(ssm_parameters_id(dat, scales = 1:8, id = c("person", "PA")))
  expect_error(ssm_parameters_id(dat, scales = 1:8, id = TRUE))
  # a missing id would be silently dropped by split(); error instead
  dat2 <- rbind(dat, dat)
  dat2$person <- c("A", NA)
  expect_error(
    ssm_parameters_id(dat2, scales = 1:8, id = "person"),
    "missing values in `id`"
  )
  # an id variable named like an output column would duplicate that column
  # name and be silently picked up by `$Disp` in summary() (a circular mean
  # of person ids); refuse the collision instead
  dat3 <- dat
  dat3$Disp <- "A"
  expect_error(
    ssm_parameters_id(dat3, scales = 1:8, id = "Disp"),
    "reserved"
  )
})

test_that("ssm_parameters_id handles zero rows and matrix input", {
  dat0 <- data.frame(
    PA = numeric(0), BC = numeric(0), DE = numeric(0), FG = numeric(0),
    HI = numeric(0), JK = numeric(0), LM = numeric(0), NO = numeric(0)
  )
  res0 <- ssm_parameters_id(dat0, scales = 1:8)
  expect_s3_class(res0, "circumplex_ssm_id")
  expect_equal(nrow(res0), 0)

  # Matrix input is accepted like ssm_score()
  theta <- as.numeric(octants()) * pi / 180
  m <- matrix(2 + 3 * cos(theta) + 4 * sin(theta), nrow = 1)
  colnames(m) <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  res <- ssm_parameters_id(m, scales = 1:8)
  expect_equal(res$Ampl, 5)
})

# T2: group-level summary layer + spec sec. 3.3 invariants ---------------------

test_that("summary.circumplex_ssm_id computes circular group summaries", {
  # Person 1: pure first harmonic with (x, y) = (0, 2) -> a = 2, d = 90.
  # Person 2: pure first harmonic with (x, y) = (-3, 0) -> a = 3, d = 180.
  # Person 3: exactly flat -> a = 0, d = NA (stripped, counted).
  theta <- as.numeric(octants()) * pi / 180
  dat <- as.data.frame(rbind(
    1 + 2 * sin(theta),
    2 - 3 * cos(theta),
    rep(0.5, 8)
  ))
  colnames(dat) <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  expect_warning(res <- ssm_parameters_id(dat, scales = 1:8))
  smr <- summary(res)

  expect_s3_class(smr, "data.frame")
  expect_equal(smr$n, 3L)
  expect_equal(smr$n_na_d, 1L)
  # Arithmetic means over persons: e = (1 + 2 + 0.5)/3; x = (0 - 3 + 0)/3;
  # y = (2 + 0 + 0)/3; a = (2 + 3 + 0)/3.
  expect_equal(smr$e_mean, 3.5 / 3)
  expect_equal(smr$x_mean, -1)
  expect_equal(smr$y_mean, 2 / 3)
  expect_equal(smr$a_mean, 5 / 3)
  # Circular mean of the defined d_i, recomputed by hand (atan2 of summed
  # sines/cosines of 90 and 180 degrees), never via angle_mean():
  # atan2(sin(90) + sin(180), cos(90) + cos(180)) = atan2(1, -1) = 135 deg.
  expect_equal(smr$d_mean, 135)
  # Resultant length of two unit vectors 90 degrees apart:
  # sqrt((1/2)^2 + (-1/2)^2)... = sqrt(mean(cos)^2 + mean(sin)^2)
  # = sqrt(0.25 + 0.25) = sqrt(0.5).
  expect_equal(smr$d_res, sqrt(0.5))
})

test_that("summary.circumplex_ssm_id handles all-NA displacement", {
  dat <- as.data.frame(rbind(rep(1, 8), rep(2, 8)))
  colnames(dat) <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  expect_warning(res <- ssm_parameters_id(dat, scales = 1:8))
  smr <- summary(res)
  expect_equal(smr$n, 2L)
  expect_equal(smr$n_na_d, 2L)
  expect_true(is.na(smr$d_mean))
  expect_true(is.na(smr$d_res))
  expect_equal(smr$a_mean, 0)
})

test_that("per-person parameters obey the spec 3.3 invariants", {
  # Heterogeneous synthetic sample (seeded, reproducible)
  set.seed(42)
  scales_mat <- matrix(rnorm(6 * 8, mean = 1, sd = 1.5), nrow = 6)
  colnames(scales_mat) <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  dat <- as.data.frame(scales_mat)
  res <- ssm_parameters_id(dat, scales = 1:8)

  # Linearity (exact): e, x, y are linear in scores, so the person-mean of
  # (e_i, x_i, y_i) equals the group transform of the mean profile.
  grp <- ssm_parameters(colMeans(scales_mat))
  expect_equal(mean(res$Elev), grp$Elev)
  expect_equal(mean(res$Xval), grp$Xval)
  expect_equal(mean(res$Yval), grp$Yval)

  # Jensen: group amplitude <= mean per-person amplitude, strictly under
  # directional dispersion (random directions disperse almost surely).
  expect_lt(grp$Ampl, mean(res$Ampl))

  # Identical profiles: every person identical to the mean profile
  # reproduces the group parameters exactly, person by person.
  same <- as.data.frame(matrix(rep(colMeans(scales_mat), 3),
                               nrow = 3, byrow = TRUE))
  colnames(same) <- colnames(scales_mat)
  res_same <- ssm_parameters_id(same, scales = 1:8)
  expect_equal(res_same$Elev, rep(grp$Elev, 3))
  expect_equal(res_same$Xval, rep(grp$Xval, 3))
  expect_equal(res_same$Yval, rep(grp$Yval, 3))
  expect_equal(res_same$Ampl, rep(grp$Ampl, 3))
  expect_equal(res_same$Disp, rep(grp$Disp, 3))
  expect_equal(res_same$Fit, rep(grp$Fit, 3))

  # Circular mean recomputed by hand (atan2 of summed sines/cosines of the
  # per-person displacements), never via angle_mean()
  smr <- summary(res)
  d_rad <- res$Disp * pi / 180
  d_hand <- atan2(sum(sin(d_rad)), sum(cos(d_rad))) %% (2 * pi) * 180 / pi
  expect_equal(smr$d_mean, d_hand)

  # Anti-confusion regression: the circular mean of per-person d_i (equal
  # weight per direction) is a different quantity from the displacement of
  # the group mean profile (amplitude-weighted). Fixture: person 1 has
  # a = 5 at d = 90; person 2 has a = 1 at d = 180. Circular mean of
  # directions = 135; group mean profile has (x, y) = (-0.5, 2.5), so
  # d = atan2(2.5, -0.5) = 101.31 deg. They must differ.
  theta <- as.numeric(octants()) * pi / 180
  two <- as.data.frame(rbind(1 + 5 * sin(theta), 1 - 1 * cos(theta)))
  colnames(two) <- colnames(scales_mat)
  res_two <- ssm_parameters_id(two, scales = 1:8)
  smr_two <- summary(res_two)
  grp_two <- ssm_parameters(colMeans(as.matrix(two)))
  expect_equal(smr_two$d_mean, 135)
  expect_equal(grp_two$Disp, atan2(2.5, -0.5) * 180 / pi, ignore_attr = TRUE)
  expect_gt(abs(smr_two$d_mean - grp_two$Disp), 30)
})
