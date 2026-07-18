# M29 / AC3: discrimination oracle for the occasions ssm_ci_accuracy() path
# (design devel/m29-design.md sec. 6; D-017). Coverage alone is provably blind
# to a dependence-dropping population (both replayed procedures cover at nominal
# even from a wrongly-independent population -- RR07); the discriminating
# observable is interval WIDTH. Two arms, meeting the >= 2-independent-oracle
# bar together with the AC2 simulation-coverage oracle:
#
#   (invariant)  An occasions run with the cross-occasion blocks ZEROED must
#                reproduce the already-validated (M4) two-group independent-
#                groups diagnostic built from the same marginals -- on both
#                coverage AND Median_width -- because a block-diagonal stacked
#                covariance makes the paired contrast an independent-difference.
#
#   (closed-form) The dependent-vs-zeroed paired ELEVATION-contrast Median_width
#                ratio must match the deterministic pre-simulation target
#                sqrt(w' Sigma w / w' Sigma0 w), where w is the elevation-
#                difference contrast weight (1/p)[-1_p ; +1_p] on the stacked 2p
#                person vector, Sigma the object's stacked covariance, and
#                Sigma0 its block-diagonal (cross-blocks zeroed). A REVERSAL
#                cell (Delta d = 135 deg, |Delta d| > 90) additionally expects
#                the paired DISPLACEMENT contrast to be WIDER than the zeroed
#                one (the D-013 / RR06 sign reversal: paired is narrower only
#                for cos(Delta d) > 0).
#
# The elevation identity is EXACT (elevation is a linear functional of the
# profile, so Var(Delta e-hat) = w' Sigma w / n regardless of angle spacing);
# the displacement reversal is directional (its magnitude tracks cos Delta d
# only under isotropy, so it is asserted as a sign, not a value).
#
# PRE-REGISTERED ACCEPTANCE (fixed before the full run; committed rds sets
# smoke = FALSE):
#   A. invariant: for the base cell's contrast row, |cov_B - cov_C| within the
#      4-SE binomial band (as AC2) for e/a/d, AND the Median_width ratio B/C is
#      in [0.90, 1.11] for e/a/d.
#   B. closed-form (base + reversal): the observed dependent/zeroed elevation-
#      contrast Median_width ratio matches the target sqrt(w'Sigma w/w'Sigma0 w)
#      within +/- 8% (|observed/target - 1| <= 0.08).
#   C. reversal sign: at Delta d = 135, the dependent/zeroed DISPLACEMENT-
#      contrast Median_width ratio > 1 (paired wider); at the base cell
#      (Delta d = 40) it is < 1 (paired narrower).
#
# Reproducibility: level-indexed seeds (LESSONS 2026-07-13, M19).
#
# Usage:  Rscript devel/m29-ci-accuracy-occasions-discrimination.R          # full
#         M29_SMOKE=1 Rscript devel/m29-ci-accuracy-occasions-discrimination.R  # smoke

devtools::load_all(".", quiet = TRUE)

smoke <- nzchar(Sys.getenv("M29_SMOKE"))
REPS <- if (smoke) 60 else 1000
BOOTS <- if (smoke) 120 else 400
BASE_SEED <- 20260717

p <- 8
angles_deg <- octants()
ang <- as.numeric(angles_deg) * pi / 180
scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
occ1 <- paste0(scales, "_1")
occ2 <- paste0(scales, "_2")
occ <- list(T1 = occ1, T2 = occ2)

# Stacked two-occasion draw MVN(mu, Sigma), isotropic marginals (s2 I) and
# cross-occasion correlation rho (Cov(block1, block2) = rho s2 I). Identical
# marginal structure across occasions makes the two-group reference's pooled
# structure an exact match for the zeroed occasions run.
simulate_wide <- function(n, mu1, mu2, rho, s2 = 1) {
  z1 <- matrix(rnorm(n * p), n, p)
  z2 <- z1 * rho + matrix(rnorm(n * p), n, p) * sqrt(1 - rho^2)
  df <- data.frame(sweep(sqrt(s2) * z1, 2, mu1, "+"),
                   sweep(sqrt(s2) * z2, 2, mu2, "+"))
  names(df) <- c(occ1, occ2)
  df
}

# elevation-difference contrast weight on the stacked 2p vector
w_elev <- c(rep(-1 / p, p), rep(1 / p, p))

# The contrast row is the last profile label (occasions: "T2 - T1"; the
# two-group reference: "B - A"). Derive it from the object so both arms work.
con_label <- function(acc) names(acc$details$row_n)[length(acc$details$row_n)]
contrast_width <- function(acc, param) {
  cv <- acc$coverage
  cv$Median_width[cv$Profile == con_label(acc) & cv$Parameter == param &
                    cv$Condition == 1]
}
contrast_cov <- function(acc, param) {
  cv <- acc$coverage
  cv$Coverage[cv$Profile == con_label(acc) & cv$Parameter == param &
                cv$Condition == 1]
}

run_cell <- function(cell_name, cell_i, d1, dd) {
  n <- 150
  e1 <- 2; a1 <- 1.2
  d2 <- (d1 + dd) %% 360
  mu1 <- e1 + a1 * cos(ang - d1 * pi / 180)
  mu2 <- (e1 + 0.3) + (a1 + 0.2) * cos(ang - d2 * pi / 180)
  rho <- 0.5

  # Plug-in object (dependent)
  set.seed(BASE_SEED + 1e6 * cell_i)
  df0 <- simulate_wide(n, mu1, mu2, rho)
  obj_A <- suppressWarnings(suppressMessages(ssm_analyze(
    df0, occasions = occ, contrast = TRUE, boots = BOOTS
  )))
  Sig <- obj_A$details$suff_stats$groups[["All"]]$cov
  Sig0 <- Sig
  Sig0[seq_len(p), p + seq_len(p)] <- 0
  Sig0[p + seq_len(p), seq_len(p)] <- 0

  # Closed-form elevation width target (dependent / zeroed)
  target_e <- sqrt(as.numeric(t(w_elev) %*% Sig %*% w_elev) /
                     as.numeric(t(w_elev) %*% Sig0 %*% w_elev))

  # Arm A: dependent diagnostic
  set.seed(BASE_SEED + 2e6 + cell_i)
  accA <- suppressWarnings(ssm_ci_accuracy(obj_A, reps = REPS,
                                           amplitude_factors = c(1)))
  # Arm B: same object with the cross-occasion blocks zeroed
  obj_B <- obj_A
  obj_B$details$suff_stats$groups[["All"]]$cov <- Sig0
  set.seed(BASE_SEED + 3e6 + cell_i)
  accB <- suppressWarnings(ssm_ci_accuracy(obj_B, reps = REPS,
                                           amplitude_factors = c(1)))

  # Arm C: two-group independent reference with the same per-occasion marginals
  # and means (occasion 1 -> group A, occasion 2 -> group B; fresh units),
  # classic mean contrast diagnostic with structure = "observed".
  set.seed(BASE_SEED + 4e6 + cell_i)
  gA <- simulate_wide(n, mu1, mu2, rho)[occ1]; names(gA) <- scales
  gB <- simulate_wide(n, mu1, mu2, rho)[occ2]; names(gB) <- scales
  ref <- rbind(
    cbind(gA, Grp = "A"),
    cbind(gB, Grp = "B")
  )
  obj_C <- suppressWarnings(suppressMessages(ssm_analyze(
    ref, scales = scales, grouping = "Grp", contrast = TRUE, boots = BOOTS
  )))
  set.seed(BASE_SEED + 5e6 + cell_i)
  accC <- suppressWarnings(ssm_ci_accuracy(obj_C, reps = REPS,
                                           structure = "observed",
                                           amplitude_factors = c(1)))

  list(
    target_e = target_e,
    widthA = vapply(c("e", "a", "d"), function(pm) contrast_width(accA, pm), numeric(1)),
    widthB = vapply(c("e", "a", "d"), function(pm) contrast_width(accB, pm), numeric(1)),
    widthC = vapply(c("e", "a", "d"), function(pm) contrast_width(accC, pm), numeric(1)),
    covB = vapply(c("e", "a", "d"), function(pm) contrast_cov(accB, pm), numeric(1)),
    covC = vapply(c("e", "a", "d"), function(pm) contrast_cov(accC, pm), numeric(1)),
    n_repsB = accB$coverage$N_reps[1], n_repsC = accC$coverage$N_reps[1],
    d1 = d1, dd = dd
  )
}

t_start <- Sys.time()
cells <- list(base = list(d1 = 135, dd = 40), reversal = list(d1 = 45, dd = 135))
results <- list()
for (cell_i in seq_along(cells)) {
  nm <- names(cells)[cell_i]
  cat(sprintf("[%s] cell %s ...\n", format(Sys.time(), "%H:%M:%S"), nm))
  results[[nm]] <- run_cell(nm, cell_i, cells[[nm]]$d1, cells[[nm]]$dd)
}
elapsed <- difftime(Sys.time(), t_start, units = "mins")
cat(sprintf("total runtime: %.1f min\n", as.numeric(elapsed)))

saveRDS(
  list(results = results, cells = cells, reps = REPS, boots = BOOTS,
       base_seed = BASE_SEED, smoke = smoke,
       elapsed_min = as.numeric(elapsed), timestamp = Sys.time()),
  "devel/m29-ci-accuracy-occasions-discrimination-results.rds"
)

# Summary -----------------------------------------------------------------
cat("\n== discrimination ==\n")
for (nm in names(results)) {
  x <- results[[nm]]
  ratio_e <- x$widthA["e"] / x$widthB["e"]
  ratio_d <- x$widthA["d"] / x$widthB["d"]
  cat(sprintf("-- %s (dd=%d) --\n", nm, x$dd))
  cat(sprintf("  elev width ratio A/B = %.3f  target = %.3f  (obs/target = %.3f)\n",
              ratio_e, x$target_e, ratio_e / x$target_e))
  cat(sprintf("  disp width ratio A/B = %.3f  (%s)\n",
              ratio_d, if (x$dd > 90) "expect > 1" else "expect < 1"))
  cat(sprintf("  invariant B~C cov (e/a/d): %.3f/%.3f/%.3f vs %.3f/%.3f/%.3f\n",
              x$covB["e"], x$covB["a"], x$covB["d"],
              x$covC["e"], x$covC["a"], x$covC["d"]))
  cat(sprintf("  invariant B~C width ratio (e/a/d): %.3f/%.3f/%.3f\n",
              x$widthB["e"] / x$widthC["e"], x$widthB["a"] / x$widthC["a"],
              x$widthB["d"] / x$widthC["d"]))
}
