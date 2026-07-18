# Occasions (repeated-measures) SSM analyses: intake validation, scoring,
# contrasts, and engines (M25; binding spec devel/longitudinal-ssm-spec.md
# sec. 1-2). Fixture provenance: make_occ_data() below generates all wide
# two-plus-occasion fixtures from a named seed (no committed data files).

# Generate wide occasions data: one row per person, k occasion blocks of the
# p = 8 octant scales, named <scale>_<j> (stem-matchable). `rho` controls the
# within-person cross-occasion dependence (shared person effect); occasion 2
# gets `shift` added to every scale so the occasions genuinely differ.
make_occ_data <- function(n = 50, k = 2, seed = 123, rho = 0.6, shift = 0.5,
                          scales = c("PA", "BC", "DE", "FG",
                                     "HI", "JK", "LM", "NO")) {
  set.seed(seed)
  p <- length(scales)
  person <- matrix(rnorm(n * p), n, p) # shared person effect
  blocks <- lapply(seq_len(k), function(j) {
    block <- sqrt(rho) * person + sqrt(1 - rho) * matrix(rnorm(n * p), n, p) +
      2 + (j - 1) * shift
    colnames(block) <- paste0(scales, "_", j)
    block
  })
  df <- as.data.frame(do.call(cbind, blocks))
  df$Gender <- factor(rep(c("F", "M"), length.out = n))
  df
}

occ_names <- function(j, scales = c("PA", "BC", "DE", "FG",
                                    "HI", "JK", "LM", "NO")) {
  paste0(scales, "_", j)
}

# Intake validation (spec sec. 1.1/1.3) ----------------------------------------

test_that("occasions and scales are mutually exclusive spellings", {
  data <- make_occ_data()
  expect_error(
    ssm_analyze(data,
      scales = occ_names(1),
      occasions = list(T1 = occ_names(1), T2 = occ_names(2))
    ),
    "mutually exclusive"
  )
  expect_error(ssm_analyze(data), "either `scales` or `occasions`")
})

test_that("occasions rejects the correlation (measures) path", {
  data <- make_occ_data()
  data$NARPD <- rnorm(nrow(data))
  expect_error(
    ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
      measures = "NARPD"
    ),
    "occasions.*measures|measures.*occasions"
  )
})

test_that("occasions blocks are validated for shape", {
  data <- make_occ_data()
  # not a list
  expect_error(
    ssm_analyze(data, occasions = occ_names(1)),
    "list"
  )
  # fewer than two occasions
  expect_error(
    ssm_analyze(data, occasions = list(T1 = occ_names(1))),
    "at least two"
  )
  # block length must match angles
  expect_error(
    ssm_analyze(data,
      occasions = list(T1 = occ_names(1)[1:7], T2 = occ_names(2)[1:7])
    ),
    "length"
  )
  # unequal block lengths caught even before the angle check fires
  expect_error(
    ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T2 = occ_names(2)[1:7])
    ),
    "length"
  )
  # partially named lists are ambiguous
  expect_error(
    ssm_analyze(data,
      occasions = stats::setNames(list(occ_names(1), occ_names(2)), c("T1", ""))
    ),
    "name"
  )
  # duplicate labels are ambiguous
  expect_error(
    ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T1 = occ_names(2))
    ),
    "name"
  )
})

test_that("occasions requires listwise deletion (estimand grounds)", {
  data <- make_occ_data()
  expect_error(
    ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
      listwise = FALSE
    ),
    "within-person"
  )
})

test_that("stem-order mismatch (rotation) errors naming the block", {
  data <- make_occ_data()
  # same stems, rotated order: exactly the silent-rotation bug (spec sec. 1.1)
  rotated <- occ_names(2)[c(2:8, 1)]
  expect_error(
    ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T2 = rotated)
    ),
    "T2.*order|order.*T2"
  )
})

test_that("stem mismatch (different stems) errors naming the block", {
  data <- make_occ_data()
  names(data)[names(data) == "PA_2"] <- "ZZ_2"
  bad <- c("ZZ_2", occ_names(2)[-1])
  expect_error(
    ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T2 = bad)
    ),
    "stem"
  )
})

test_that("no stem structure falls back to a positional-alignment message", {
  data <- make_occ_data()
  # occasion-2 names with no common prefix or suffix
  flat <- c("alpha", "bravo", "charlie", "delta",
            "echo", "foxtrot", "golf", "hotel")
  names(data)[9:16] <- flat
  expect_message(
    res <- ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T2 = flat),
      boots = 10
    ),
    "positional"
  )
  expect_s3_class(res, "circumplex_ssm")
})

test_that("numeric occasion indices stay positional under duplicated names", {
  # Review F1 regression (2026-07-16): cbind-ing two waves that keep the SAME
  # scale names is a natural wide layout; numeric blocks must subset by
  # position, never by resolved name (name resolution collapses every block
  # onto the first match and silently zeroes the contrast)
  scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  base <- make_occ_data(n = 60, seed = 51)
  wave1 <- base[occ_names(1)]
  wave2 <- base[occ_names(2)] + 1 # true elevation shift of +1
  names(wave1) <- scales
  names(wave2) <- scales
  dat <- cbind(wave1, wave2)
  set.seed(52)
  res <- suppressMessages(ssm_analyze(dat,
    occasions = list(T1 = 1:8, T2 = 9:16),
    contrast = TRUE, boots = 50
  ))
  # the occasions must be genuinely distinct blocks, not first-match copies
  exp_de <- mean(as.matrix(dat[9:16])) - mean(as.matrix(dat[1:8]))
  expect_equal(res$results$e_est[[3]], exp_de, tolerance = 1e-12)
  # truth ~ 1.5 (make_occ_data's built-in +0.5 shift plus the +1 here);
  # the broken name-resolution path reported exactly 0
  expect_gt(exp_de, 1)
  expect_false(isTRUE(all.equal(res$results$e_est[[1]],
                                res$results$e_est[[2]])))
})

test_that("occasions blocks selecting overlapping columns error", {
  data <- make_occ_data()
  expect_error(
    ssm_analyze(data,
      occasions = list(T1 = 1:8, T2 = c(3, 9:15))
    ),
    "overlap"
  )
  # same literal column name in two blocks is the same trap via characters
  expect_error(
    ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T2 = c("PA_1", occ_names(2)[-1]))
    ),
    "overlap"
  )
})

test_that("bad occasion column references error informatively", {
  data <- make_occ_data()
  expect_error(
    ssm_analyze(data, occasions = list(T1 = 1:8, T2 = 11:18)),
    "out of range"
  )
  expect_error(
    ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T2 = c("NOPE_2", occ_names(2)[-1]))
    ),
    "NOPE_2"
  )
})

# Scoring and result assembly (spec sec. 1.1/1.2) ------------------------------

test_that("occasions profiles are correct and ordered occasion-minor", {
  data <- make_occ_data(n = 60, k = 3)
  occ <- list(T1 = occ_names(1), T2 = occ_names(2), T3 = occ_names(3))
  set.seed(1)
  res <- ssm_analyze(data, occasions = occ, boots = 10)
  expect_s3_class(res, "circumplex_ssm")
  expect_true("Occasion" %in% names(res$results))
  expect_equal(res$results$Occasion, c("T1", "T2", "T3"))
  expect_equal(res$results$Label, c("T1", "T2", "T3"))
  # per-occasion point estimates equal the equivalent single-scales run
  # exactly (complete data, same rows) -- the exact invariant of AC2
  for (j in 1:3) {
    set.seed(2)
    ref <- ssm_analyze(data, scales = occ_names(j), boots = 10)
    expect_equal(
      unlist(res$results[j, c("e_est", "x_est", "y_est", "a_est", "d_est",
                              "fit_est")]),
      unlist(ref$results[1, c("e_est", "x_est", "y_est", "a_est", "d_est",
                              "fit_est")]),
      tolerance = 1e-12
    )
  }
  # details carry the occasions metadata
  expect_equal(res$details$occasions, c("T1", "T2", "T3"))
})

test_that("unnamed occasions default to T1..Tk labels", {
  data <- make_occ_data()
  set.seed(1)
  res <- ssm_analyze(data,
    occasions = list(occ_names(1), occ_names(2)), boots = 10
  )
  expect_equal(res$results$Occasion, c("T1", "T2"))
})

test_that("occasions cross grouping: group-major, occasion-minor rows", {
  data <- make_occ_data(n = 80)
  set.seed(1)
  res <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
    grouping = "Gender", boots = 10
  )
  expect_equal(res$results$Group, c("F", "F", "M", "M"))
  expect_equal(res$results$Occasion, c("T1", "T2", "T1", "T2"))
  expect_equal(res$results$Label, c("T1: F", "T2: F", "T1: M", "T2: M"))
  # per-cell estimates equal the single-scales grouped run, occasion by
  # occasion (complete data)
  for (j in 1:2) {
    set.seed(2)
    ref <- ssm_analyze(data,
      scales = occ_names(j), grouping = "Gender", boots = 10
    )
    got <- res$results[res$results$Occasion == paste0("T", j), ]
    expect_equal(got$e_est, ref$results$e_est, tolerance = 1e-12)
    expect_equal(got$d_est, ref$results$d_est, tolerance = 1e-12)
  }
})

test_that("scores table has one profile row per group x occasion", {
  data <- make_occ_data(n = 80)
  set.seed(1)
  res <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
    grouping = "Gender", boots = 10
  )
  expect_equal(nrow(res$scores), 4)
  expect_equal(res$scores$Occasion, c("T1", "T2", "T1", "T2"))
  # scale columns are the validated stems
  expect_true(all(c("PA", "BC", "NO") %in% names(res$scores)))
})

test_that("listwise deletion across waves is messaged with the count", {
  data <- make_occ_data(n = 50)
  data[1, "PA_2"] <- NA # person 1 missing only occasion 2
  data[2, "NO_1"] <- NA # person 2 missing only occasion 1
  set.seed(1)
  expect_message(
    res <- ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
      boots = 10
    ),
    "2 person"
  )
  # a person missing any occasion is dropped from all occasions
  set.seed(1)
  ref <- ssm_analyze(data[-c(1, 2), ],
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)), boots = 10
  )
  expect_equal(res$results$e_est, ref$results$e_est, tolerance = 1e-12)
})

# Paired occasion contrasts (spec sec. 1.2/2.1) --------------------------------

# Wide data with known within-person change: occasion 1 is a cosine profile
# peaked at d1 with elevation e1; occasion 2 is peaked at d2 = d1 + dd with
# elevation e1 + de. Shared person effect induces within-person dependence.
make_contrast_data <- function(n = 200, d1 = 90, dd = 45, e1 = 2, de = 0.5,
                               amp = 1.5, seed = 42) {
  set.seed(seed)
  scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  ang <- octants() * pi / 180
  person <- rnorm(n, 0, 0.3)
  block <- function(d, e) {
    profile <- e + amp * cos(ang - d * pi / 180)
    t(vapply(seq_len(n), function(i) {
      profile + person[i] + rnorm(8, 0, 0.2)
    }, numeric(8)))
  }
  b1 <- block(d1, e1)
  b2 <- block(d1 + dd, e1 + de)
  colnames(b1) <- paste0(scales, "_1")
  colnames(b2) <- paste0(scales, "_2")
  cbind(as.data.frame(b1), as.data.frame(b2))
}

test_that("occasion contrast is second listed minus first listed", {
  data <- make_contrast_data(de = 0.5, dd = 45)
  set.seed(3)
  res <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
    contrast = TRUE, boots = 100
  )
  expect_equal(nrow(res$results), 3)
  expect_equal(res$results$Occasion[[3]], "T2 - T1")
  expect_equal(res$results$Label[[3]], "T2 - T1")
  # elevation contrast equals the hand-computed occasion-2-minus-occasion-1
  # grand mean difference exactly (linear statistic, complete data)
  exp_de <- mean(as.matrix(data[occ_names(2)])) -
    mean(as.matrix(data[occ_names(1)]))
  expect_equal(res$results$e_est[[3]], exp_de, tolerance = 1e-12)
  expect_gt(res$results$e_est[[3]], 0) # sign: T2 is higher by construction
  # displacement contrast is the signed angular distance d2 - d1 (~ +45)
  expect_equal(res$results$d_est[[3]], 45, tolerance = 5)
  # and matches angle_dist of the two profile rows exactly
  exp_dd <- ((res$results$d_est[[2]] - res$results$d_est[[1]] + 180) %%
               360) - 180
  expect_equal(res$results$d_est[[3]], exp_dd, tolerance = 1e-9)
})

test_that("contrast order is list order, never alphabetical (T10 vs T2)", {
  data <- make_contrast_data(de = 0.5)
  names(data) <- c(paste0(occ_names(""), "T2"), paste0(occ_names(""), "T10"))
  # supplied order: T2 first, T10 second; alphabetical sorting would put
  # "T10" before "T2" and silently flip the contrast sign
  set.seed(3)
  res <- ssm_analyze(data,
    occasions = list(
      T2 = paste0(occ_names(""), "T2"),
      T10 = paste0(occ_names(""), "T10")
    ),
    contrast = TRUE, boots = 10
  )
  expect_equal(res$results$Occasion, c("T2", "T10", "T10 - T2"))
  # T10 (the later-listed occasion) has the higher elevation by construction
  expect_gt(res$results$e_est[[3]], 0)
})

test_that("contrast composition rules for occasions are enforced", {
  data3 <- make_occ_data(k = 3)
  expect_error(
    ssm_analyze(data3,
      occasions = list(T1 = occ_names(1), T2 = occ_names(2),
                       T3 = occ_names(3)),
      contrast = TRUE
    ),
    "2 occasions"
  )
  data2 <- make_occ_data()
  expect_error(
    ssm_analyze(data2,
      occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
      grouping = "Gender", contrast = TRUE
    ),
    "single group"
  )
})

# Oracle: closed-form paired-elevation interval (spec sec. 2.3 item 3) --------

test_that("MC paired-elevation CI matches the textbook paired interval", {
  skip_on_cran()
  # The paired de contrast is a linear statistic: its MC interval must match
  # the paired-difference normal interval computed with deliberately dumb
  # code on the same data. Pre-registered tolerance: quantile MC error at
  # B = 5000 has sd ~ 0.038 * SE_D per endpoint; we allow 4x that (0.15).
  data <- make_contrast_data(n = 150, de = 0.5, seed = 21)
  D <- rowMeans(data[occ_names(2)]) - rowMeans(data[occ_names(1)])
  se_d <- sd(D) / sqrt(length(D))
  cf_lci <- mean(D) - qnorm(0.975) * se_d
  cf_uci <- mean(D) + qnorm(0.975) * se_d
  set.seed(22)
  res <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
    contrast = TRUE, boots = 5000, method = "montecarlo"
  )
  expect_equal(res$results$e_est[[3]], mean(D), tolerance = 1e-12)
  expect_lt(abs(res$results$e_lci[[3]] - cf_lci), 0.15 * se_d)
  expect_lt(abs(res$results$e_uci[[3]] - cf_uci), 0.15 * se_d)
})

test_that("bootstrap and MC paired-contrast CIs agree within tolerance", {
  skip_on_cran()
  # Pre-registered SE-based tolerance (spec sec. 2.3 item 2, never a
  # build-time judgment call): at n = 500 and B = 2000 the two engines'
  # contrast CI endpoints must agree within 0.30 * SE (SE = the parameter's
  # replicate spread, proxied by CI width / 3.92). Not an independent oracle
  # for the shared downstream quantile path (both engines flow through
  # param_diff and the same interval assembly); the coverage oracle in
  # devel/m25-paired-coverage.R carries the branch-handling weight.
  data <- make_contrast_data(n = 500, de = 0.5, dd = 45, seed = 23)
  set.seed(24)
  res_bs <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
    contrast = TRUE, boots = 2000
  )
  set.seed(25)
  res_mc <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
    contrast = TRUE, boots = 2000, method = "montecarlo"
  )
  for (par in c("e", "a", "d")) {
    lci_b <- res_bs$results[[paste0(par, "_lci")]][[3]]
    uci_b <- res_bs$results[[paste0(par, "_uci")]][[3]]
    lci_m <- res_mc$results[[paste0(par, "_lci")]][[3]]
    uci_m <- res_mc$results[[paste0(par, "_uci")]][[3]]
    se <- (uci_b - lci_b) / 3.92
    expect_lt(abs(lci_b - lci_m), 0.30 * se)
    expect_lt(abs(uci_b - uci_m), 0.30 * se)
  }
})

test_that("committed coverage-oracle results satisfy the registered bands", {
  # Pins the devel/m25-paired-coverage.R run (reps = 500, boots = 600): a
  # regenerated rds that drifted out of its pre-registered acceptance would
  # fail here. Skipped where devel/ is absent (built tarball).
  rds <- testthat::test_path("..", "..", "devel",
                             "m25-paired-coverage-results.rds")
  skip_if_not(file.exists(rds), "devel oracle results not present")
  x <- readRDS(rds)
  skip_if(isTRUE(x$smoke), "smoke-run rds carries no evidence")
  getm <- function(cell, method, field) {
    vapply(x$results[[cell]], function(r) r[[method]][[field]], numeric(1))
  }
  n100 <- c("base", "dd_near0", "dd_178", "pole", "reversal",
            "base_repaired", "reversal_repaired")
  for (nm in n100) {
    for (method in c("bootstrap", "montecarlo")) {
      for (f in c("de_cov", "da_cov", "dd_cov")) {
        cov <- mean(getm(nm, method, f))
        expect_gte(cov, 0.91)
        expect_lte(cov, 0.98)
      }
    }
  }
  # small-n: bootstrap gated at [.89, .98]; MC measured, not gated
  for (f in c("de_cov", "da_cov", "dd_cov")) {
    cov <- mean(getm("small_n", "bootstrap", f))
    expect_gte(cov, 0.89)
    expect_lte(cov, 0.98)
  }
  # k = 3 profile displacement coverage (MC)
  for (i in 1:3) {
    cov <- mean(vapply(x$k3, `[`, logical(1), i))
    expect_gte(cov, 0.91)
    expect_lte(cov, 0.98)
  }
  # conditional-efficiency identities, discriminating: Var(dd-hat)
  # paired/re-paired tracks 1 - rho*cos(dd); direction must REVERSE at 135
  for (nm in c("base", "reversal")) {
    theory <- 1 - x$cells[[nm]]$rho * cos(x$cells[[nm]]$dd * pi / 180)
    for (par in c("dd_est", "da_est")) {
      ratio <- var(getm(nm, "montecarlo", par)) /
        var(getm(paste0(nm, "_repaired"), "montecarlo", par))
      expect_gt(ratio / theory, 0.70)
      expect_lt(ratio / theory, 1.30)
    }
  }
  # paired is NARROWER at dd = 30 and WIDER at dd = 135 (the reversal):
  # the unconditional claim fails here by design
  r30 <- var(getm("base", "montecarlo", "dd_est")) /
    var(getm("base_repaired", "montecarlo", "dd_est"))
  r135 <- var(getm("reversal", "montecarlo", "dd_est")) /
    var(getm("reversal_repaired", "montecarlo", "dd_est"))
  expect_lt(r30, 1)
  expect_gt(r135, 1)
  # exact paired-elevation variance identity (isotropic population)
  for (nm in c("base", "reversal")) {
    theory <- 2 * (1 - x$cells[[nm]]$rho) / (8 * x$cells[[nm]]$n)
    ratio <- var(getm(nm, "montecarlo", "de_est")) / theory
    expect_gt(ratio, 0.80)
    expect_lt(ratio, 1.25)
  }
})

# Output surfaces (spec sec. 7 Build A acceptance; RR06 R12) ------------------

test_that("print and summary render occasions objects (snapshots)", {
  data <- make_contrast_data(n = 100, seed = 31)
  set.seed(32)
  res <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
    contrast = TRUE, boots = 100
  )
  expect_snapshot(print(res))
  expect_snapshot(summary(res))
})

test_that("ssm_table renders occasion rows with occasion labels", {
  data <- make_contrast_data(n = 100, seed = 33)
  set.seed(34)
  res <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
    contrast = TRUE, boots = 50
  )
  tab <- ssm_table(res, render = FALSE)
  expect_equal(nrow(tab), 3)
  expect_equal(tab[[1]], c("T1", "T2", "T2 - T1"))
  expect_equal(names(tab)[[1]], "Contrast")
})

test_that("ssm_plot_circle and ssm_plot_curve accept occasions objects", {
  data <- make_contrast_data(n = 100, seed = 35)
  set.seed(36)
  res <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)), boots = 50
  )
  p1 <- ssm_plot_circle(res)
  expect_s3_class(p1, "ggplot")
  p2 <- ssm_plot_curve(res)
  expect_s3_class(p2, "ggplot")
  # data-level check (M15 lesson): the curve layer carries both occasions
  # and only genuine scale columns (no leaked Occasion info column)
  curve_data <- ggplot2::ggplot_build(p2)$data[[1]]
  expect_equal(length(unique(curve_data$group)), 2)
  point_layer <- p2$layers[[3]]$data
  expect_false(any(c("Occasion", "T1", "T2") %in% point_layer$Scale))
})

test_that("ssm_plot_contrast plots an occasion contrast", {
  data <- make_contrast_data(n = 100, seed = 37)
  set.seed(38)
  res <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
    contrast = TRUE, boots = 50
  )
  p <- ssm_plot_contrast(res)
  expect_s3_class(p, "ggplot")
  # and a profiles-only occasions object refuses with the updated message
  set.seed(39)
  res2 <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)), boots = 50
  )
  expect_error(ssm_plot_contrast(res2), "two occasions")
})

# ssm_ci_accuracy() occasions path (M29; D-017) -------------------------------

test_that("occasions ci_accuracy population is the stacked cross-occasion covariance (AC1)", {
  # A dependent fixture (rho = 0.7) and an independent one (rho = 0), same
  # marginals: the stored stacked covariance must carry the within-person
  # cross-occasion dependence in its off-diagonal p x p blocks, not zero them.
  dep <- make_occ_data(n = 90, k = 2, rho = 0.7, seed = 101)
  ind <- make_occ_data(n = 90, k = 2, rho = 0, seed = 101)
  occ <- list(T1 = occ_names(1), T2 = occ_names(2))
  p <- 8L
  set.seed(201)
  res_dep <- ssm_analyze(dep, occasions = occ, boots = 50)
  set.seed(202)
  res_ind <- ssm_analyze(ind, occasions = occ, boots = 50)

  ss <- res_dep$details$suff_stats
  expect_equal(ss$occ_k, 2L)
  cov_dep <- ss$groups[["All"]]$cov
  expect_equal(dim(cov_dep), c(2L * p, 2L * p))
  cross_dep <- cov_dep[seq_len(p), p + seq_len(p)]
  cross_ind <- res_ind$details$suff_stats$groups[["All"]]$cov[seq_len(p), p + seq_len(p)]
  # dependent: the same-scale cross-occasion covariances (the block diagonal)
  # track rho ~ 0.7; independent: they collapse toward zero
  expect_gt(mean(diag(cross_dep)), 0.3)
  expect_lt(mean(diag(cross_ind)), 0.15)

  # the diagnostic consumes exactly that stacked covariance (its draw root is
  # mvn_root of the stored cov), runs, and records the observed structure
  set.seed(1)
  acc <- ssm_ci_accuracy(res_dep, reps = 12, amplitude_factors = c(1))
  expect_s3_class(acc, "circumplex_ci_accuracy")
  expect_identical(acc$details$structure, "observed")
  expect_equal(acc$details$occ_k, 2L)
  expect_null(acc$cpm)
  expect_true(all(c("T1", "T2") %in% acc$coverage$Profile))
})

test_that("occasions ci_accuracy runs both engines and reports a paired-contrast row", {
  data <- make_contrast_data(n = 120, seed = 40)
  occ <- list(T1 = occ_names(1), T2 = occ_names(2))
  for (method in c("bootstrap", "montecarlo")) {
    set.seed(40)
    res <- ssm_analyze(data, occasions = occ, contrast = TRUE,
                       boots = 150, method = method)
    set.seed(41)
    acc <- ssm_ci_accuracy(res, reps = 30, amplitude_factors = c(1, 0.25))
    expect_s3_class(acc, "circumplex_ci_accuracy")
    # the paired contrast row is present and its displacement verdict is
    # unconditional (Parameter "d", not "d_conditional") -- M15-D1
    con_lab <- "T2 - T1"
    expect_true(con_lab %in% acc$coverage$Profile)
    vd <- acc$verdict[acc$verdict$Profile == con_lab, ]
    expect_true("d" %in% vd$Parameter)
    expect_false("d_conditional" %in% vd$Parameter)
    # coverage near nominal for the elevation contrast (loose, small reps)
    ce <- acc$coverage[acc$coverage$Profile == con_lab &
                         acc$coverage$Parameter == "e" &
                         acc$coverage$Condition == 1, "Coverage"]
    expect_gt(ce, 0.7)
  }
})

test_that("occasions ci_accuracy refuses an explicit structure/cpm and a legacy object", {
  data <- make_contrast_data(n = 100, seed = 40)
  res <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)), boots = 30
  )
  expect_error(ssm_ci_accuracy(res, structure = "cpm"),
               "does not accept an explicit")
  expect_error(ssm_ci_accuracy(res, structure = "observed"),
               "does not accept an explicit")
  expect_error(ssm_ci_accuracy(res, cpm = structure(list(), class = "circumplex_cpm")),
               "does not accept an explicit")
  # a pre-M29 occasions object (no stored stacked statistics) refuses
  legacy <- res
  legacy$details$suff_stats <- NULL
  expect_error(ssm_ci_accuracy(legacy), "re-run ssm_analyze")
})

test_that("occasions ci_accuracy refuses a flat occasion by name (AC4)", {
  data <- make_contrast_data(n = 60, seed = 12)
  data[occ_names(2)] <- 3.0 # occasion 2 constant -> flat profile
  res <- suppressWarnings(
    ssm_analyze(data, occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
                boots = 20)
  )
  err <- expect_error(ssm_ci_accuracy(res, reps = 5))
  expect_match(conditionMessage(err), "[Ff]lat")
  expect_match(conditionMessage(err), "T2")
})

test_that("occasions ci_accuracy warns (not refuses) on a rank-deficient stacked covariance (AC4)", {
  # n <= k*p = 16: the stacked covariance is singular but a proper degenerate
  # normal; the run proceeds with a fit-statistic caveat rather than erroring
  data <- make_contrast_data(n = 14, seed = 5)
  set.seed(5)
  res <- suppressWarnings(
    ssm_analyze(data, occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
                boots = 20)
  )
  set.seed(6)
  expect_warning(
    acc <- ssm_ci_accuracy(res, reps = 6, amplitude_factors = c(1)),
    "[Rr]ank-deficient"
  )
  expect_s3_class(acc, "circumplex_ci_accuracy")
  expect_true(isTRUE(acc$details$rank_deficiency[["All"]]$deficient))
})

test_that("occasions ci_accuracy flags Structural rows at c=0 and runs a near-zero occasion honestly (AC4)", {
  # (a) the c = 0 amplitude-ladder rung zeroes every occasion's amplitude, so
  # each occasion's amplitude coverage is structurally 0 (a percentile interval
  # of positive amplitude replicates cannot contain a zero truth) and must be
  # flagged Structural; the run does not error and reports certification.
  data <- make_contrast_data(n = 100, seed = 22)
  set.seed(23)
  res <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
    contrast = TRUE, boots = 100
  )
  set.seed(24)
  acc <- suppressWarnings(
    ssm_ci_accuracy(res, reps = 20, amplitude_factors = c(1, 0))
  )
  struct0 <- acc$coverage[acc$coverage$Condition == 0 &
                            acc$coverage$Parameter == "a" &
                            acc$coverage$Profile %in% c("T1", "T2"), ]
  expect_true(all(struct0$Structural))       # occasion amplitude rows flagged
  expect_true(all(struct0$Coverage == 0))    # structural-zero coverage
  # certification is reported honestly at c = 0 (the guardrail Caution column
  # is populated for the occasion rows on that rung, NA for the contrast)
  g0 <- acc$guardrail[acc$guardrail$Condition == 0, ]
  expect_true(all(is.finite(g0$Cert_rate)))
  expect_false(any(is.na(g0$Caution[g0$Profile %in% c("T1", "T2")])))

  # (b) a genuinely near-zero-amplitude occasion (occasion 2 nearly flat, but
  # not zero-variance) runs without erroring and reports a certification rate.
  set.seed(25)
  data2 <- make_contrast_data(n = 100, seed = 25)
  data2[occ_names(2)] <- 2 + matrix(rnorm(100 * 8, 0, 0.5), 100, 8)  # amp ~ 0
  res2 <- suppressWarnings(ssm_analyze(data2,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)), boots = 100
  ))
  set.seed(26)
  acc2 <- suppressWarnings(ssm_ci_accuracy(res2, reps = 20,
                                           amplitude_factors = c(1)))
  expect_s3_class(acc2, "circumplex_ci_accuracy")
  cr <- acc2$guardrail$Cert_rate[acc2$guardrail$Profile == "T2"]
  expect_true(all(is.finite(cr)))  # every condition rung reports a rate
})

test_that("occasions ci_accuracy handles a pole-straddling occasion without error (AC4)", {
  # occasion 1 peaks on the 0/360 pole; the diagnostic's angular coverage
  # (mod-360 arc membership) must run and report a finite displacement row
  data <- make_contrast_data(n = 80, d1 = 0, dd = 90, seed = 7)
  for (method in c("bootstrap", "montecarlo")) {
    set.seed(8)
    res <- ssm_analyze(data,
      occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
      boots = 150, method = method
    )
    set.seed(9)
    acc <- ssm_ci_accuracy(res, reps = 15, amplitude_factors = c(1))
    dcov <- acc$coverage[acc$coverage$Profile == "T1" &
                           acc$coverage$Parameter == "d" &
                           acc$coverage$Condition == 1, "Coverage"]
    expect_true(is.finite(dcov))
  }
})

# Boundary battery (CLAUDE.md; spec sec. 2.3 item 4) --------------------------

test_that("occasion profile CI straddling the 0/360 pole wraps, both engines", {
  # occasion 1 peaks exactly on the pole; with this n and noise the CI is
  # several degrees wide on each side, so it must straddle and wrap
  data <- make_contrast_data(n = 40, d1 = 0, dd = 90, seed = 7)
  # inflate noise for a wide displacement CI
  set.seed(8)
  noisy <- data + matrix(rnorm(prod(dim(data)), 0, 0.6), nrow(data))
  for (method in c("bootstrap", "montecarlo")) {
    set.seed(9)
    res <- ssm_analyze(noisy,
      occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
      boots = 500, method = method
    )
    d_est <- res$results$d_est[[1]]
    # estimate reports near the pole on either label (D-003/M20 convention)
    expect_true(d_est < 15 || d_est > 345)
    # a wrapped CI reports lci > uci (e.g., 350 to 8)
    expect_gt(res$results$d_lci[[1]], res$results$d_uci[[1]])
    # and the occasion-2 profile (90 degrees) is unaffected
    expect_equal(res$results$d_est[[2]], 90, tolerance = 10)
  }
})

test_that("occasion contrast near +/-180 keeps sign and branch, both engines", {
  for (dd in c(175, -175)) {
    data <- make_contrast_data(n = 200, d1 = 90, dd = dd, seed = 11)
    for (method in c("bootstrap", "montecarlo")) {
      set.seed(12)
      res <- ssm_analyze(data,
        occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
        contrast = TRUE, boots = 300, method = method
      )
      d_con <- res$results$d_est[[3]]
      expect_equal(d_con, dd, tolerance = 5)
      # CI is on the same branch as its estimate and covers the truth
      # (endpoints may legitimately exceed +/-180 near the boundary)
      expect_true(res$results$d_lci[[3]] < dd && res$results$d_uci[[3]] > dd)
      expect_lt(res$results$d_uci[[3]] - res$results$d_lci[[3]], 90)
    }
  }
})

test_that("a flat occasion reports NA displacement with a warning, both engines", {
  data <- make_contrast_data(n = 60, seed = 13)
  # occasion 2 becomes exactly flat: constant score for every person and scale
  data[occ_names(2)] <- 2
  for (method in c("bootstrap", "montecarlo")) {
    set.seed(14)
    w <- testthat::capture_warnings(
      res <- ssm_analyze(data,
        occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
        contrast = TRUE, boots = 50, method = method
      )
    )
    expect_true(any(grepl("flat or.*zero", w)))
    # the flat occasion's displacement (and fit) are NA; the healthy
    # occasion is untouched; the contrast inherits NA displacement
    expect_true(is.na(res$results$d_est[[2]]))
    expect_true(is.na(res$results$fit_est[[2]]))
    expect_false(is.na(res$results$d_est[[1]]))
    expect_true(is.na(res$results$d_est[[3]]))
    # linear contrast parameters stay defined
    expect_false(is.na(res$results$e_est[[3]]))
  }
})

test_that("k = 3 occasions agree across engines on point estimates", {
  data <- make_occ_data(n = 60, k = 3, seed = 15)
  occ <- list(T1 = occ_names(1), T2 = occ_names(2), T3 = occ_names(3))
  set.seed(16)
  res_bs <- ssm_analyze(data, occasions = occ, boots = 20)
  set.seed(16)
  res_mc <- ssm_analyze(data, occasions = occ, boots = 20,
                        method = "montecarlo")
  expect_equal(res_bs$results$d_est, res_mc$results$d_est, tolerance = 1e-12)
  expect_equal(res_bs$results$a_est, res_mc$results$a_est, tolerance = 1e-12)
})

test_that("occasion contrast runs through both engines consistently", {
  data <- make_contrast_data(n = 300)
  set.seed(4)
  res_bs <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
    contrast = TRUE, boots = 200
  )
  set.seed(4)
  res_mc <- ssm_analyze(data,
    occasions = list(T1 = occ_names(1), T2 = occ_names(2)),
    contrast = TRUE, boots = 200, method = "montecarlo"
  )
  # identical point estimates (same closed-form transform of the same means)
  expect_equal(res_bs$results$d_est, res_mc$results$d_est, tolerance = 1e-12)
  expect_equal(res_bs$results$e_est, res_mc$results$e_est, tolerance = 1e-12)
  # both engines' contrast CIs cover the construction truths (de = 0.5,
  # dd = 45) and agree on sign
  for (res in list(res_bs, res_mc)) {
    expect_lt(res$results$e_lci[[3]], 0.5 + 0.2)
    expect_gt(res$results$e_uci[[3]], 0.5 - 0.2)
    expect_true(res$results$d_lci[[3]] < 45 && res$results$d_uci[[3]] > 45)
  }
})

# ssm_ci_accuracy() occasions oracle (M29 T3/AC2) -----------------------------

test_that("committed AC2 simulation-coverage oracle satisfies the registered band", {
  # Pins devel/m29-ci-accuracy-occasions-oracle.R (R1 = 1000 diagnostic reps,
  # R2 = 800 empirical reps, boots = 300): the diagnostic's REPORTED coverage
  # must track the direct EMPIRICAL coverage of the object's own procedure at
  # the same plug-in population, per occasion and for the paired contrast, on
  # both engines. Regeneration that drifts out of the pre-registered band fails
  # here. Skipped where devel/ is absent (built tarball).
  rds <- testthat::test_path("..", "..", "devel",
                             "m29-ci-accuracy-occasions-oracle-results.rds")
  skip_if_not(file.exists(rds), "devel oracle results not present")
  x <- readRDS(rds)
  skip_if(isTRUE(x$smoke), "smoke-run rds carries no evidence")
  # every (cell x engine) must have exercised the paired-contrast row
  expect_true(all(c("interior.bootstrap", "interior.montecarlo",
                    "pole.bootstrap") %in% names(x$results)))
  for (key in names(x$results)) {
    r <- x$results[[key]]
    # the paired-contrast row is present (3rd column of the 3x3 matrices)
    expect_equal(dim(r$reported), c(3L, 3L))
    expect_equal(dim(r$empirical), c(3L, 3L))
    for (ri in 1:3) {
      for (pm_i in 1:3) {
        rep_c <- r$reported[pm_i, ri]
        emp_c <- r$empirical[pm_i, ri]
        band <- 4 * sqrt(rep_c * (1 - rep_c) / r$n_reported +
                           emp_c * (1 - emp_c) / r$n_empirical) + 0.010
        expect_lte(abs(rep_c - emp_c), band)
      }
    }
  }
})

test_that("committed AC3 discrimination oracle satisfies the registered gates", {
  # Pins devel/m29-ci-accuracy-occasions-discrimination.R. Coverage alone is
  # blind to dependence-dropping; interval WIDTH discriminates. (A) the zeroed-
  # cross-blocks occasions run reproduces the two-group independent diagnostic
  # (invariant); (B) the dependent/zeroed elevation-contrast width ratio matches
  # the closed-form sqrt(w'Sigma w / w'Sigma0 w) target; (C) the displacement
  # ratio reverses sign across |Delta d| = 90. Skipped where devel/ is absent.
  rds <- testthat::test_path("..", "..", "devel",
                             "m29-ci-accuracy-occasions-discrimination-results.rds")
  skip_if_not(file.exists(rds), "devel oracle results not present")
  x <- readRDS(rds)
  skip_if(isTRUE(x$smoke), "smoke-run rds carries no evidence")
  base <- x$results$base
  rev <- x$results$reversal

  # (B) closed-form elevation width target, both cells (+/- 8%)
  for (cell in list(base, rev)) {
    obs <- cell$widthA[["e"]] / cell$widthB[["e"]]
    expect_lt(abs(obs / cell$target_e - 1), 0.08)
  }

  # (C) displacement reversal: paired narrower at dd = 40, wider at dd = 135
  expect_lt(base$widthA[["d"]] / base$widthB[["d"]], 1)
  expect_gt(rev$widthA[["d"]] / rev$widthB[["d"]], 1)

  # (A) invariant: zeroed occasions run (B) reproduces the two-group reference
  # (C) on the contrast row -- coverage within the 4-SE band, Median_width
  # within [0.90, 1.11] -- for the base cell.
  for (pm in c("e", "a", "d")) {
    cb <- base$covB[[pm]]; cc <- base$covC[[pm]]
    band <- 4 * sqrt(cb * (1 - cb) / base$n_repsB +
                       cc * (1 - cc) / base$n_repsC) + 0.010
    expect_lte(abs(cb - cc), band)
    wr <- base$widthB[[pm]] / base$widthC[[pm]]
    expect_gt(wr, 0.90)
    expect_lt(wr, 1.11)
  }
})
