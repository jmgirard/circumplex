# M27: deterministic miniatures of the growth-recipe oracle invariants.
# Oracle registry: the full simulation-coverage run + invariants live in
# devel/m27-coverage-oracle.R (committed results devel/m27-coverage-results.rds,
# pre-registered acceptance in the script header; level-indexed seeds). These
# tests pin seeded, single-replicate versions so a regression in the
# angle/adapter machinery fails fast in the suite.

test_that("point-collapsed parameter draws reproduce the closed-form direction", {
  # All draws identical -> the adapter must return exactly the atan2
  # direction and the D-007 rule must fail closed on the zero-width interval
  e <- 0.5; x <- 0.3; y <- 0.4
  draws <- matrix(rep(c(e, x, y), each = 50), ncol = 3)
  res <- suppressWarnings(ssm_draws(draws, type = "parameters"))
  d_expected <- (atan2(y, x) * 180 / pi) %% 360
  expect_equal(as.numeric(res$results$d_est), d_expected)
  expect_equal(res$results$a_est, sqrt(x^2 + y^2))
  # zero-width amplitude interval: ratio is Inf/NaN -> not certified
  expect_false(res$details$certified)
})

test_that("unwrap-then-average agrees with the (x, y) framing when concentrated", {
  # Invariant A miniature (spec sec. 4.2): in the concentrated common-branch
  # regime, mean-of-unwrapped-directions and direction-of-mean trajectories
  # agree within tolerance. Balanced design, so wave means stand in for the
  # LMM fixed effects. Full version: devel/m27-coverage-oracle.R inv_unwrap.
  set.seed(20260716)
  n <- 200
  waves <- 0:4
  d_start <- 40 * pi / 180; d_end <- 80 * pi / 180
  x_t <- 0.8 * cos(seq(d_start, d_end, length.out = 5))
  y_t <- 0.8 * sin(seq(d_start, d_end, length.out = 5))
  vx <- rnorm(n, 0, 0.10); vy <- rnorm(n, 0, 0.10)
  wide_x <- outer(rep(1, n), x_t) + vx + matrix(rnorm(n * 5, 0, 0.1), n)
  wide_y <- outer(rep(1, n), y_t) + vy + matrix(rnorm(n * 5, 0, 0.1), n)
  d_xy <- (atan2(colMeans(wide_y), colMeans(wide_x)) * 180 / pi) %% 360
  d_person <- (atan2(wide_y, wide_x) * 180 / pi) %% 360
  d_unwrap <- colMeans(t(apply(d_person, 1, angle_unwrap)))
  diff <- abs(((d_xy - d_unwrap + 180) %% 360) - 180)
  expect_lt(max(diff), 2)
})

test_that("two-occasion zero-slope growth agrees with the paired contrast", {
  # Invariant B miniature (spec sec. 4.2, RR06 Q6): the model-based growth
  # pipeline and M25's paired occasions machinery are different estimators
  # that agree asymptotically under correct specification. One large-n
  # well-specified replicate; full version: devel/m27-coverage-oracle.R
  # inv_2occ (200 reps, pre-registered tolerances).
  skip_on_cran()
  skip_if_not_installed("glmmTMB")

  set.seed(20260717)
  n <- 2000
  theta <- as.numeric(octants()) * pi / 180
  p <- length(theta)
  mu <- 0.5 + 0.4 * cos(theta - 45 * pi / 180)
  u_e <- rnorm(n, 0, 0.3)
  v_x <- rnorm(n, 0, 0.10)
  v_y <- rnorm(n, 0, 0.10)
  person_mat <- outer(u_e, rep(1, p)) +
    outer(v_x, cos(theta)) + outer(v_y, sin(theta))
  scores <- function() {
    matrix(mu, n, p, byrow = TRUE) + person_mat +
      matrix(rnorm(n * p, 0, 0.4), n, p)
  }
  s1 <- scores(); s2 <- scores()
  colnames(s1) <- paste0(PANO(), "_1"); colnames(s2) <- paste0(PANO(), "_2")

  occ <- ssm_analyze(
    data.frame(s1, s2),
    occasions = list(T1 = colnames(s1), T2 = colnames(s2)),
    angles = octants(), contrast = TRUE, method = "montecarlo", boots = 2000
  )
  dd_paired <- as.numeric(occ$results[nrow(occ$results), ]$d_est)

  pp <- lapply(list(s1, s2), function(s) {
    ssm_parameters_id(as.data.frame(s), scales = colnames(s),
                      angles = octants())
  })
  long <- do.call(rbind, lapply(1:2, function(k) {
    data.frame(person = seq_len(n), wave = k - 1L,
               e = pp[[k]]$Elev, x = pp[[k]]$Xval, y = pp[[k]]$Yval)
  }))
  long <- reshape(long, direction = "long",
                  varying = list(c("e", "x", "y")), v.names = "value",
                  timevar = "dv", times = c("e", "x", "y"),
                  idvar = c("person", "wave"))
  long$dv <- factor(long$dv, levels = c("e", "x", "y"))
  long$person <- factor(long$person)
  fit <- glmmTMB::glmmTMB(
    value ~ 0 + dv + dv:wave + us(0 + dv | person),
    dispformula = ~ 0 + dv, data = long, REML = TRUE
  )
  fe <- glmmTMB::fixef(fit)$cond
  dd_growth <- (((atan2(fe["dvy"] + fe["dvy:wave"], fe["dvx"] + fe["dvx:wave"]) -
                    atan2(fe["dvy"], fe["dvx"])) * 180 / pi + 180) %% 360) - 180

  # Point estimates agree well under 1 degree at n = 2000 (full-run mean
  # ~0.02 deg); true delta_d is 0
  expect_lt(abs(((dd_growth - dd_paired + 180) %% 360) - 180), 0.5)
})
