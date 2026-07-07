# User-facing fit_structure() API (M4.5/T7): the single entry point over the
# five Acton & Revelle (2004) structure tests, plus its print/summary/plot
# methods. The criterion statistics and their interpretation/inference are
# validated in test-fit_structure.R; here we test the wrapper's orchestration,
# scoring behavior, object contract, and rendering.

octants_jz <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")

# Independent RANDALL correspondence index via the draft counting loop, on an
# arbitrary correlation matrix whose variables are in circular order.
draft_randall <- function(r) {
  nv <- ncol(r)
  n_away <- function(a, b) {
    s <- rep(1:nv, 2)
    min(abs(outer(which(s == a), which(s == b), "-")))
  }
  m <- matrix(NA, nv, nv)
  for (i in 1:nv) for (j in 1:nv) m[i, j] <- n_away(i, j)
  hyp <- m[lower.tri(m)]
  vals <- r[lower.tri(r)]
  nc <- 0
  nt <- 0
  for (i in seq_along(vals)) {
    lr <- hyp > hyp[i]
    nc <- nc + sum(vals[i] > vals[lr])
    nt <- nt + sum(lr)
  }
  (nc / nt) - ((nt - nc) / nt)
}

# ---- object contract --------------------------------------------------------

test_that("fit_structure returns a circumplex_structure with the documented shape", {
  data("jz2017")
  res <- fit_structure(jz2017, scales = octants_jz)
  expect_s3_class(res, "circumplex_structure")
  expect_named(res, c("results", "randall", "loadings", "details", "call"))
  expect_identical(res$results$Test, c("Fisher", "Gap", "Variance", "Rotation"))
  expect_identical(
    colnames(res$results),
    c("Test", "Hypothesis", "Statistic", "Almost", "Thrice", "Twice", "Category")
  )
  expect_identical(res$details$nv, 8L)
  expect_identical(res$details$scoring, "deviation")
  expect_true(res$details$calibrated)
  expect_equal(dim(res$loadings), c(8L, 2L))
  expect_identical(rownames(res$loadings), octants_jz)
})

# ---- scoring behavior -------------------------------------------------------

test_that("deviation scoring row-mean-centers and matches the calibration path", {
  data("jz2017")
  res <- fit_structure(jz2017, scales = octants_jz, scoring = "deviation")
  # The calibration (data-raw/structure-test-cutoffs.R) deviation-scores with
  # x - rowMeans(x) and factors at ridge 0. fit_structure must reproduce that
  # exact statistic, not something ridge-repaired.
  x <- as.matrix(jz2017[octants_jz])
  lam <- structure_loadings(as.data.frame(x - rowMeans(x)), octants_jz, ridge = 0)
  expect_equal(res$results$Statistic[[1]], structure_fisher(lam))
  expect_equal(res$results$Statistic[[2]], structure_gap(lam))
  expect_equal(res$results$Statistic[[3]], structure_vt(lam))
  expect_equal(res$results$Statistic[[4]], structure_rt(lam))
  # Default deviation categories on jz2017: the general factor removed, the
  # instrument reads as a circumplex on every criterion.
  expect_identical(res$results$Category, c("thrice", "thrice", "almost", "thrice"))
})

test_that("raw scoring leaves scores untouched and selects the raw cutoffs", {
  data("jz2017")
  res <- fit_structure(jz2017, scales = octants_jz, scoring = "raw")
  lam <- structure_loadings(jz2017, octants_jz, ridge = 0)
  expect_equal(res$results$Statistic[[1]], structure_fisher(lam))
  # The raw octant scores carry a strong general factor, so every criterion is
  # unsupported -- and the cutoffs shown are the raw ones.
  expect_identical(res$results$Category, rep("weak", 4))
  expect_identical(res$results$Almost, unname(vapply(
    c("fisher", "gap", "vt", "rt"),
    function(k) structure_cutoffs[["8"]][[k]][["raw"]][["almost"]], numeric(1)
  )))
})

test_that("RANDALL is included, honors the scoring, and reports the exact p", {
  data("jz2017")
  res <- fit_structure(jz2017, scales = octants_jz, scoring = "deviation")
  x <- as.matrix(jz2017[octants_jz])
  r_dev <- stats::cor(x - rowMeans(x))
  # The index is computed on the same (deviation-scored) correlations as the
  # factor-analytic criteria -- one scoring decision applied to all five tests.
  expect_equal(res$randall$statistic, draft_randall(r_dev))
  expect_identical(res$randall$method, "exact")
  # The ipsatized ordering is strong enough that only the two dihedral
  # relabelings reach the observed index, so the exact p is at its lower bound.
  expect_equal(res$randall$p_value, 2 / factorial(7))
})

# ---- uncalibrated scale counts ----------------------------------------------

test_that("an uncalibrated nv reports statistics but withholds interpretation", {
  data("jz2017")
  res <- fit_structure(jz2017, scales = octants_jz[1:6])
  expect_identical(res$details$nv, 6L)
  expect_false(res$details$calibrated)
  # Statistics are finite; cutoffs and categories are NA (no nv = 6 calibration).
  expect_true(all(is.finite(res$results$Statistic)))
  expect_true(all(is.na(res$results$Category)))
  expect_true(all(is.na(res$results$Almost)))
  # RANDALL needs no cutoffs, so it still runs (exact for nv < 10).
  expect_identical(res$randall$method, "exact")
  expect_false(is.na(res$randall$p_value))
})

# ---- Monte Carlo RANDALL and the RNG contract -------------------------------

test_that("n_perm switches RANDALL to the reproducible Monte Carlo path", {
  data("jz2017")
  set.seed(20260707)
  res <- fit_structure(jz2017, scales = octants_jz, n_perm = 199)
  expect_identical(res$randall$method, "monte carlo")
  expect_identical(res$randall$n_perm, 199L)
  expect_gte(res$randall$p_value, 1 / 200)
  set.seed(20260707)
  res2 <- fit_structure(jz2017, scales = octants_jz, n_perm = 199)
  expect_identical(res2$randall$p_value, res$randall$p_value)
})

test_that("the default path is deterministic (leaves the global RNG untouched)", {
  data("jz2017")
  set.seed(1)
  before <- .Random.seed
  invisible(fit_structure(jz2017, scales = octants_jz))
  expect_identical(.Random.seed, before)
  # The Monte Carlo path consumes the stream.
  invisible(fit_structure(jz2017, scales = octants_jz, n_perm = 9))
  expect_false(identical(.Random.seed, before))
})

# ---- validation -------------------------------------------------------------

test_that("fit_structure validates its arguments", {
  data("jz2017")
  expect_error(fit_structure(jz2017, scales = octants_jz[1:3])) # RANDALL floor
  expect_error(fit_structure(jz2017, scales = octants_jz, scoring = "ipsative"))
  expect_error(fit_structure(jz2017, scales = octants_jz, ridge = -1))
  expect_error(fit_structure(jz2017, scales = octants_jz, n_perm = 0))
  # A non-numeric selected column is caught at the wrapper with a clear message,
  # not left to surface as a cryptic error inside cor()/rowMeans().
  df <- jz2017[octants_jz]
  df$label <- "x"
  expect_error(
    fit_structure(df, scales = c("label", octants_jz[1:4])),
    "numeric"
  )
})

# ---- print / summary snapshots ----------------------------------------------

test_that("print and summary render as expected", {
  data("jz2017")
  res <- fit_structure(jz2017, scales = octants_jz)
  expect_snapshot(print(res))
  expect_snapshot(summary(res))
})

test_that("print names the uncalibrated case in plain language", {
  data("jz2017")
  res <- fit_structure(jz2017, scales = octants_jz[1:6])
  out <- paste(capture.output(print(res)), collapse = "\n")
  expect_match(out, "no interpretive cutoffs are calibrated for 6 scales")
})

# ---- plot -------------------------------------------------------------------

test_that("plot draws one point per scale at its communality radius", {
  data("jz2017")
  res <- fit_structure(jz2017, scales = octants_jz)
  p <- plot(res)
  expect_true(ggplot2::is_ggplot(p))
  b <- ggplot2::ggplot_build(p)
  pts <- b$data[[length(b$data)]]
  expect_equal(nrow(pts), 8)
  # Radius = communality * 5 / amax with amax = 1 (the canvas transform).
  r <- sqrt(pts$x^2 + pts$y^2)
  expect_equal(sort(r), sort(unname(rowSums(res$loadings^2)) * 5), tolerance = 1e-6)
})

test_that("plot is a stable visual and warns on unknown dots", {
  data("jz2017")
  res <- fit_structure(jz2017, scales = octants_jz)
  vdiffr::expect_doppelganger("structure config plot", plot(res))
  vdiffr::expect_doppelganger("structure config no legend", plot(res, legend = FALSE))
  expect_warning(plot(res, bogus = TRUE))
})

test_that("plot gives every scale a distinct fill beyond the 8-colour palette", {
  # More than eight scales exceed Set2's range; the hue fallback must colour all
  # of them without the "n too large for palette Set2" warning or a collapse of
  # the extra scales onto the neutral fill.
  set.seed(1)
  nv <- 10
  ang <- (seq_len(nv) - 1) * 2 * pi / nv
  # Build a genuine circular signal so factoring is well defined.
  f1 <- stats::rnorm(200)
  f2 <- stats::rnorm(200)
  dat <- as.data.frame(lapply(ang, function(a) {
    cos(a) * f1 + sin(a) * f2 + 0.4 * stats::rnorm(200)
  }))
  names(dat) <- paste0("V", seq_len(nv))
  res <- fit_structure(dat, scales = names(dat), n_perm = 99)
  expect_warning(p <- plot(res), NA) # no palette warning
  b <- ggplot2::ggplot_build(p)
  fills <- unique(b$data[[length(b$data)]]$fill)
  expect_length(fills, nv) # all ten scales distinctly coloured
})
