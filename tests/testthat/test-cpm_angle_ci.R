# Oracle coverage for the CPM bootstrap angle-CI path (R/cpm_fit.R:1119-1121),
# where cpm_bootstrap() reuses the SSM circular-quantile primitive
# quantile.circumplex_radian() and converts the result to degrees. The primitive
# itself is oracle-tested at the SSM bootstrap boundary (test-ssm_bootstrap.R);
# here we cover its reuse at the CPM call site for a 0/360-straddling
# displacement, backed by two independent oracle types.

test_that("CPM angle-CI transform matches a dumb circular-quantile oracle at the 0/360 straddle (M13)", {
  # theta_reps[ok, i] for a variable whose accepted bootstrap displacement
  # straddles the 0/2*pi pole: four replicates just above 0 rad, four just
  # below 2*pi. This is the input cpm_fit.R:1119 feeds to the circular quantile.
  reps  <- c(0.02, 0.05, 0.09, 0.13, 6.16, 6.20, 6.24, 6.27)
  probs <- c(0.025, 0.975)

  # The verbatim composition cpm_bootstrap() applies at cpm_fit.R:1119-1121.
  got <- as.numeric(as_degree(
    quantile.circumplex_radian(new_radian(reps), probs = probs)
  ))

  # Oracle A (live, deliberately-dumb): a circular quantile recomputed from
  # scratch in DEGREES -- center on the circular mean, unwrap to (-180, 180],
  # type-7 quantile, rewrap to [0, 360), snapping a 360 pole to 0 as the method
  # does. Independent of the package's radian implementation.
  deg <- reps * 180 / pi
  mu  <- atan2(mean(sin(reps)), mean(cos(reps))) * 180 / pi
  cen <- ((deg - mu + 180) %% 360) - 180
  qc  <- as.numeric(stats::quantile(cen, probs = probs))
  oracle <- (qc + mu) %% 360
  oracle[abs(oracle - 360) < 1e-9] <- 0
  expect_equal(got, oracle, tolerance = 1e-9)

  # Oracle B (invariant, rotation-equivariance): rotate every replicate by a
  # fixed offset, take the circular quantile, rotate back. Circular quantiles
  # are rotation-equivariant, so this independent route through the wrap logic
  # (from a different phase origin) must agree on the circle.
  off <- 2.0
  rot <- as.numeric(as_degree(
    quantile.circumplex_radian(new_radian((reps + off) %% (2 * pi)), probs = probs)
  ))
  back  <- (rot - off * 180 / pi) %% 360
  align <- function(z) ((z + 180) %% 360) - 180   # onto (-180, 180]
  expect_equal(align(got), align(back), tolerance = 1e-7)

  # The interval is a TIGHT arc, not the ~357deg span a linear quantile would
  # (wrongly) report for these straddling replicates.
  expect_lt((align(got[2]) - align(got[1])) %% 360, 30)
})

test_that("cpm_fit() bootstrap angle CI wraps a pole-straddling item, not linearizes it (M13)", {
  skip_on_cran()
  # End-to-end guard with teeth on the real cpm_fit.R:1119 call site. We drive a
  # population whose last item sits at the 0/360 pole (Angle_theory = 360, a
  # non-reference item so it is estimated, not fixed) and sample enough noise
  # that its bootstrap angle replicates straddle the pole. The CIRCULAR quantile
  # then reports that item's CI as a WRAPPED short arc (Angle_lci > Angle_uci,
  # crossing 0/360); a LINEAR quantile at the call site would instead report a
  # ~357deg non-wrapped span [~1, ~358] with the estimate outside it. Verified
  # out-of-band (assignInNamespace linearization) that all three pole-row
  # assertions below flip to FAIL when line 1119 is linearized.
  deg <- c(45, 90, 135, 180, 225, 270, 315, 360)   # item 8 on the pole
  th  <- as.numeric(as_radian(as_degree(deg)))
  P   <- cpm_implied_cor(th, rep(0.7, 8), c(0.6, 0.25, 0.15))
  Lc  <- chol(P)                                    # base-R sampler, cov = P
  set.seed(42)
  X <- as.data.frame(matrix(stats::rnorm(80 * 8), 80, 8) %*% Lc)
  colnames(X) <- paste0("V", seq_len(8))

  fit <- suppressWarnings(cpm_fit(X, scales = seq_len(8), angles = as_degree(deg),
                                  ci_method = "bootstrap", boots = 300,
                                  reference = 1))
  res <- fit$results
  pole <- which(res$Angle_theory == 360)            # the straddling item
  lci <- res$Angle_lci[pole]
  uci <- res$Angle_uci[pole]
  est <- res$Angle[pole]

  # The circular signature of a genuine 0/360 straddle:
  expect_gt(lci, uci)                               # WRAPPED (lci > uci)
  expect_lt((uci - lci) %% 360, 180)                # short arc, not a ~357deg span
  expect_lte((est - lci) %% 360, (uci - lci) %% 360 + 1e-8)  # estimate inside, on the circle

  # And no scale's estimate falls outside its own CI on the circle.
  e <- res$Angle; l <- res$Angle_lci; u <- res$Angle_uci
  ok <- is.finite(e) & is.finite(l) & is.finite(u)
  expect_true(all(((e - l) %% 360)[ok] <= ((u - l) %% 360)[ok] + 1e-8))
})
