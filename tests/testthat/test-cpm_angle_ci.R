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

test_that("cpm_fit() bootstrap angle CIs are circular-consistent across scales (M13)", {
  skip_on_cran()
  # End-to-end guard on the real cpm_fit.R:1119 call site: every reported angle
  # CI must be a short arc that contains its point estimate ON THE CIRCLE. A
  # linear quantile at a near-pole straddle would push the estimate outside its
  # interval and inflate the width -- both caught here.
  data("jz2017")
  set.seed(42)
  fit <- suppressWarnings(cpm_fit(jz2017, scales = 2:9, angles = octants(),
                                  ci_method = "bootstrap", boots = 300,
                                  reference = 1))
  res <- fit$results
  est <- res$Angle
  lci <- res$Angle_lci
  uci <- res$Angle_uci
  ok  <- is.finite(est) & is.finite(lci) & is.finite(uci)

  width  <- (uci - lci) %% 360
  inside <- ((est - lci) %% 360) <= width + 1e-8
  expect_true(all(inside[ok]))       # estimate within its CI on the circle
  expect_true(all(width[ok] < 180))  # each CI is a short arc, not a wrapped span
})
