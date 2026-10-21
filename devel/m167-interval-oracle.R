# ==== PRE-REGISTRATION BEGIN ====
# M167: CPM interval coverage per component, and candidate interval families.
#
# This block is the study's pre-registration. It fixes the truths, cells,
# methods, counts, scoring rule, band and pass rule before any full-run
# output exists. Changes after the design review and before the first full
# run are listed at the end of this block, recorded in
# cairn/milestones/M167-cpm-interval-oracle.md, and listed again in the
# verdict D-entry. After the first full run, nothing in the truths, cells,
# methods, scoring rule, band or pass rule changes; the corrections listed at
# the end of this block change none of them.
# "Full run" means a run with none of the M167_* mode variables set; the
# control, smoke, named-cell and confirmation modes below are not full runs.
#
# TRUTHS (the M4 truths, devel/m4-coverage-oracle.R): p = 8 octant scales at
# octants() angles, zeta = .75 for every scale, m = 3, variant "A", unit
# scaling, reference scale 1 (PA, fixed angle).
#   boundary: beta = .45 / .35 / .15 / .05
#   interior: beta = .35 / .30 / .20 / .15
#
# CELLS
#   Raw-data cells, N = 250, 500, 1000 (both truths). Data: cpm_simulate() of
#   the exact truth fit, seeded per replicate exactly as M4 seeded them
#   (BASE_SEED_M4 = 20260706 + 1e6 * truth index + 1e3 * N index + i). These
#   six cells are the M4 control cells (acceptance AC2).
#   Correlation-matrix cells, N = 2000, 5000, 20000 (both truths). Data: one
#   sample correlation matrix per replicate, drawn as cov2cor() of a
#   Wishart(N - 1, P_truth) draw, which is the exact distribution of cor(X)
#   for N multivariate-normal rows. Fresh seeds: 20261021 + 1e6 * truth
#   index + 1e3 * N index + i.
#   Replicates: 500 per cell. Resamples: 1000 per bootstrap of either kind.
#
# METHODS (all at nominal 95%; implementations in devel/m167-interval-helpers.R)
#   B1  shipped percentile bootstrap (cpm_fit()'s raw-data default): rows
#       resampled, warm-started refits, circular quantile for angles, plain
#       quantile for zeta and beta. Raw-data cells only.
#   B2  shipped Wald interval: estimate +/- 1.96 analytic SE (cpm_analytic_se()).
#       All cells.
#   F1  transformed Wald (browne1982 eqs. 1.6.30-1.6.32, p. 95): symmetric on
#       ln v for each communality, v = (1 - zeta^2) / zeta^2, and on logit
#       beta_k for each weight, back-transformed; angles on the identity scale
#       (so F1's angle interval is B2's). All cells.
#   F2p parametric bootstrap: 1000 correlation matrices drawn as
#       cov2cor(Wishart(N - 1, Phat)) at the fit's own N and Phat, refit with
#       the shipped warm-started loop; percentile intervals formed as in B1.
#       All cells (the correlation-matrix path).
#   F2bc the F2p replicates, bias-corrected percentile interval (acceleration
#       0) for zeta and beta only. All cells.
#   F3  the B1 replicates, bias-corrected percentile interval (acceleration 0)
#       for zeta and beta only. Raw-data cells only.
#   Bias-corrected form: z0 = qnorm(share of replicates below the estimate,
#   ties counted half, clamped to [1/(2B), 1 - 1/(2B)]); endpoints are type-7
#   quantiles at pnorm(2 z0 -/+ 1.96).
#   Bb  basic bootstrap on the B1 replicates, [2 est - q(.975), 2 est -
#       q(.025)], unclamped, zeta and beta only. Raw-data cells only. (RR26)
#   F2b basic bootstrap on the F2 replicates, as Bb. All cells. (RR26)
#   P1  profile likelihood for zeta and beta (RR26 Q2): the set of t with
#       (N - 1) (F_prof(t) - F_hat) <= qchisq(.95, 1) = 3.84, where F_prof(t)
#       minimizes F over every other free parameter with the component held
#       at t; each side followed outward from the estimate and root-found on
#       the logit scale; the natural bound (0 or 1) is the endpoint when the
#       statistic stays under 3.84 out to logit 14. The 3.84 cutoff is the
#       nominal one; at a Heywood wall the statistic is a chi-square mixture,
#       so it is approximate there, and it is not tuned after the run. A
#       removed weight gets [0, 0]. All cells. (RR26)
#   VB  diagnostic arm, not an interval: a variant-B refit (angles fixed at
#       theory) of each raw-data replicate, recording zeta-hat, beta-hat and
#       the Heywood flag. Reading fixed now: if the variant-B beta_0 bias is
#       under 0.25 of its SD and its Heywood rate is under 5% at a cell, the
#       angle and communality mode is confirmed as the cause there. (RR26)
#
# PATHS
#   raw-data path: B1, F3, Bb, B2, F1, P1 at the raw-data cells.
#   correlation-matrix path: B2, F1, F2p, F2bc, F2b, P1 at all six N.
#
# SCORING
#   Components: the 7 free angles (the reference angle is fixed and has no
#   interval), the 8 communalities, the 4 beta weights of the m = 3 model.
#   Each component's coverage is scored separately, never pooled.
#   Side of a miss: zeta and beta, "above" when the lower limit exceeds the
#   truth, "below" when the upper limit is under it. Angles, "ccw" when the
#   signed shortest rotation from the truth to the interval's arc midpoint is
#   positive (the interval lies counterclockwise of the truth), "cw"
#   otherwise. Arcs run counterclockwise from lower to upper limit.
#   Excluded fit: a replicate whose fit errors, is refused, or fails the
#   engine's acceptance criterion is dropped from every method; the count is
#   recorded per cell.
#   NA standard error (singular information matrix): B2 and F1 have no
#   interval for that replicate; coverage is conditional on a defined
#   interval, and the NA count is recorded per component.
#   All bootstrap resamples excluded: that method has no interval for the
#   replicate (conditional, counted the same way).
#   Removed harmonic (beta-hat_k = 0 after the engine's polish): every method
#   reports the degenerate interval [0, 0], scored as a miss below the truth
#   (every truth weight is positive), and counted per component.
#   Heywood communality (zeta-hat > .995): scored like any other fit; it is
#   not excluded. zeta-hat can equal 1 in double precision (plogis of a large
#   logit), where F1's ln v interval is undefined (NaN); that component then
#   has no F1 interval and counts as undefined, like an NA standard error.
#   (RR26 rec 10)
#   Width per component: the arc span in degrees for angles (a Wald span of
#   360 or more counts as 360), upper minus lower limit for zeta and beta.
#   Vacuous interval: an angle arc over 180 degrees, or a zeta or beta width
#   over .5. (RR26 rec 9)
#
# BAND AND PASS RULE
#   Band: [.90, .98] per component.
#   Monte Carlo allowance, scaled to the realized count n of defined
#   intervals (RR26 rec 8): a(n) = 1.4 * sqrt(.93 * .07 / n), .016 at
#   n = 500. A component passes at a cell when its coverage lies in
#   [.90 - a(n), .98 + a(n)] AND at most 5% of its intervals are vacuous.
#   Multiplicity: a (method, path, parameter family) passes at a cell when
#   every component of that family passes there. No further correction: the
#   allowance is the only noise margin, and the per-component Wilson 95%
#   intervals are reported beside each figure.
#   Conditioning limit (RR26 rec 7): a pass at a cell is unconditional when
#   at most 5% of the accepted fits lack an interval for each component of
#   the family; otherwise it is reported as conditional on a defined
#   interval, with the undefined share.
#   Verdict, both forms reported (RR26 rec 6): (a) strict, the (method, path,
#   family) passes at every cell listed for its path; (b) the ladder, the
#   smallest listed N from which it passes at every larger listed N
#   ("never" if it fails at the largest). Each form is given unconditional
#   and conditional.
#   Parameter families: angles, communalities (zeta), weights (beta). F3,
#   F2bc, Bb, F2b and P1 are judged on zeta and beta only.
#   Descriptive only (not the verdict): coverage on the fits with a defined
#   standard error, coverage split by the Heywood flag, beta coverage on the
#   fits that keep the weight, the estimator's bias and SD per component, the
#   bootstrap's own bias estimate, and the VB arm.
#
# CONFIRMATION (RR26 rec 12): any (method, path, family) that passes, under
# either verdict form, at a raw-data cell (N <= 1000) is re-run at those
# cells on fresh seeds (30261022 + 1e6 * truth index + 1e3 * N index + i,
# 500 replicates, M167_CONFIRM=1), and both figures are reported. A pass the
# confirmation fails is reported as not confirmed.
#
# EXPECTED OUTCOME, written before the run (RR26 rec 11): the beta_0
# shortfall is an estimator bias of about one SD at N <= 1000 that no
# re-centering of the present estimate removes, so "no family passes beta on
# the raw-data path" is the expected strict reading, and the ladder is where
# a ship step reads its N.
#
# CONTROL (AC2): pooled B1 and B2 coverage per family at the six raw-data
# cells, pooled as M4 pooled it, must lie within .03 of the M4 table in
# cairn/DESIGN.md, or the record names the engine change that explains it.
#
# CHANGES AFTER THE DESIGN REVIEW (RR26, 2026-10-21), before any full run:
#   added Bb, F2b, P1 and the VB arm; per-replicate records; width and the
#   vacuous-interval limit; the allowance scaled to n (was a fixed .015);
#   the conditioning limit; the ladder as a verdict form beside the strict
#   one (was descriptive); the fresh-seed confirmation; the zeta-hat = 1
#   wording (was "ln v stays finite"); the expected outcome. The control
#   mode ran before these changes and scores only B1 and B2.
#
# CORRECTIONS AFTER THE FIRST FULL RUN (M167 review, 2026-10-21). None
# changes the truths, cells, methods, scoring rule, band or pass rule; the
# full study and the confirmation are re-run after them.
#   The confirmation seed base was 20261022, which sits 316 above the M4 base
#   and 1 above the correlation-cell base, so up to 184 of 500 confirmation
#   replicates per cell repeated main-run data. It is now 30261022, which no
#   seed of the main run reaches.
#   The implementation now follows the rules above where it did not: P1's
#   profile keeps the lower of a start from the estimate and a warm start
#   (a warm start alone gave false endpoints near a Heywood wall); an error
#   inside one method or one resample no longer drops the replicate from
#   every method (an erroring resample counts as nonconvergent); B2 gives
#   [0, 0] for a removed weight whose SE is undefined; the VB arm counts
#   accepted fits only.
#   The correlation-cell base (20261021) still sits 315 above the M4 base,
#   so a correlation-cell replicate can start from the RNG state of a
#   raw-data replicate. The two draw different things (a Wishart matrix
#   against simulated rows) at different N, and each cell is scored on its
#   own, so this is left as it is and recorded here.
# ==== PRE-REGISTRATION END ====
#
# Usage:  Rscript devel/m167-interval-oracle.R                  # full run
#         M167_SMOKE=1 Rscript devel/m167-interval-oracle.R     # smoke run
#         M167_CELLS=boundary_N250,interior_N2000 Rscript ...   # named cells
#         M167_CONTROL=1 Rscript devel/m167-interval-oracle.R   # AC2 control
#         M167_CONFIRM=1 Rscript devel/m167-interval-oracle.R   # confirmation
# The control run scores only B1 and B2 at the six raw-data cells, so it
# produces no candidate-family output. The confirmation run reads
# devel/m167-interval-results.rds and re-runs the raw-data cells where a
# raw-data-cell pass needs confirming. The full run takes about 2 hours on
# 17 cores. Results: devel/m167-interval-results.rds (smoke and partial runs
# write other names and are never committed as results).
#
# Reproducibility: each replicate runs set.seed() on its own seed, so the
# results do not depend on mc.cores or scheduling (IP4 binds package code,
# not this script).

devtools::load_all(".", quiet = TRUE)
source(file.path("devel", "m167-interval-helpers.R"))

smoke <- nzchar(Sys.getenv("M167_SMOKE"))
control <- nzchar(Sys.getenv("M167_CONTROL"))
confirm <- nzchar(Sys.getenv("M167_CONFIRM"))
REPS <- if (smoke) 12 else 500
BOOTS <- if (smoke) 100 else 1000
NS_RAW <- c(250, 500, 1000)
NS_COR <- c(2000, 5000, 20000)
SEED_M4 <- 20260706
SEED_NEW <- 20261021
SEED_CONF <- 30261022
# the code commit this run uses, read at start (the save happens much later)
RUN_COMMIT <- tryCatch(system("git rev-parse HEAD", intern = TRUE),
                       error = function(e) NA_character_)
CORES <- max(1, parallel::detectCores() - 1)
LEVEL <- 0.95
BAND <- c(0.90, 0.98)
VACUOUS_MAX <- 0.05
UNDEFINED_MAX <- 0.05
allowance <- function(n) 1.4 * sqrt(0.93 * 0.07 / n)

p <- 8
angles <- octants()
scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
configs <- list(
  boundary = list(zeta = rep(0.75, p), beta = c(.45, .35, .15, .05)),
  interior = list(zeta = rep(0.75, p), beta = c(.35, .30, .20, .15))
)

make_truth <- function(cfg) {
  P0 <- cpm_implied_cor(as.numeric(as_radian(as_degree(angles))),
                        cfg$zeta, cfg$beta)
  fit <- cpm_fit(cormat = P0, scales = scales, angles = angles, n = 10000,
                 m = 3)
  stopifnot(max(abs(fit$results$Zeta - cfg$zeta)) < 1e-6,
            max(abs(fit$betas$Beta - cfg$beta)) < 1e-6)
  fit
}

RAW_METHODS <- if (control) c("B1", "B2") else
  c("B1", "F3", "Bb", "B2", "F1", "F2p", "F2bc", "F2b", "P1")
COR_METHODS <- c("B2", "F1", "F2p", "F2bc", "F2b", "P1")
NO_ANGLE <- c("F3", "F2bc", "Bb", "F2b", "P1")
FAMS <- c("angle", "zeta", "beta")

# side-of-truth codes and widths for one method on one replicate
score_ci <- function(ci, cfg, free) {
  span <- ifelse(ci$angle[, 2] - ci$angle[, 1] >= 360, 360,
                 (ci$angle[, 2] - ci$angle[, 1]) %% 360)
  list(
    side = list(angle = m167_side_angle(ci$angle, angles %% 360)[free],
                zeta = m167_side_linear(ci$zeta, cfg$zeta),
                beta = m167_side_linear(ci$beta, cfg$beta)),
    width = list(angle = span[free],
                 zeta = ci$zeta[, 2] - ci$zeta[, 1],
                 beta = ci$beta[, 2] - ci$beta[, 1])
  )
}

boot_summary <- function(reps, eng) {
  if (reps$n_ok == 0) return(NULL)
  z0 <- function(r, est) {
    B <- length(r)
    sh <- (sum(r < est) + 0.5 * sum(r == est)) / B
    stats::qnorm(min(max(sh, 1 / (2 * B)), 1 - 1 / (2 * B)))
  }
  list(
    zeta_mean = colMeans(reps$zeta), zeta_sd = apply(reps$zeta, 2, stats::sd),
    beta_mean = colMeans(reps$beta), beta_sd = apply(reps$beta, 2, stats::sd),
    zeta_z0 = vapply(seq_len(ncol(reps$zeta)),
                     function(i) z0(reps$zeta[, i], eng$zeta[i]), numeric(1)),
    beta_z0 = vapply(seq_len(ncol(reps$beta)),
                     function(k) z0(reps$beta[, k], eng$beta[k]), numeric(1)),
    heywood_share = mean(apply(reps$zeta, 1, function(z) any(z > 0.995)))
  )
}

one_replicate <- function(cfg_name, N, i, raw, truth_fit, cfg, seed_base) {
  ti <- match(cfg_name, names(configs))
  if (raw) {
    set.seed(seed_base + 1e6 * ti + 1e3 * match(N, NS_RAW) + i)
    X <- cpm_simulate(truth_fit, N)
    R <- stats::cor(X)
  } else {
    set.seed(seed_base + 1e6 * ti + 1e3 * match(N, NS_COR) + i)
    R <- stats::cov2cor(stats::rWishart(1, N - 1, truth_fit$matrices$Phat)[, , 1])
  }
  eng <- try(suppressWarnings(cpm_engine(R, angles = angles, m = 3,
                                         variant = "A")), silent = TRUE)
  if (inherits(eng, "try-error") || !isTRUE(eng$accepted)) {
    return(list(excluded = TRUE))
  }
  free <- eng$spec$free_pos
  ci <- list()
  boots <- list()
  # An error inside one method leaves that method with no interval for this
  # replicate (counted as undefined) and the other methods untouched, so a
  # method's failure never removes the replicate from every method.
  safe_ci <- function(expr) {
    tryCatch(expr, error = function(e) m167_empty_ci(p, 4))
  }
  no_reps <- list(theta = matrix(NA_real_, 0, p), zeta = matrix(NA_real_, 0, p),
                  beta = matrix(NA_real_, 0, 4), n_ok = 0L,
                  n_degenerate = NA_integer_, n_nonconvergent = NA_integer_)
  safe_reps <- function(expr) tryCatch(expr, error = function(e) no_reps)
  # B1 first: its resample draw must be the first RNG use after the data, as
  # in cpm_fit(), so the M4 control cells reproduce M4's intervals.
  if (raw) {
    b1 <- safe_reps(suppressWarnings(m167_boot_reps(eng, X, BOOTS)))
    ci$B1 <- safe_ci(m167_percentile(b1, LEVEL))
    if (!control) {
      ci$F3 <- safe_ci(m167_bc(b1, eng, LEVEL))
      ci$Bb <- safe_ci(m167_basic(b1, eng, LEVEL))
      boots$B1 <- boot_summary(b1, eng)
    }
  }
  se <- try(suppressWarnings(cpm_analytic_se(eng, R, N)), silent = TRUE)
  if (inherits(se, "try-error")) {
    se <- list(angle = rep(NA_real_, p), zeta = rep(NA_real_, p),
               beta = rep(NA_real_, 4))
  }
  se_defined <- !anyNA(c(se$angle[free], se$zeta, se$beta))
  ci$B2 <- safe_ci(m167_wald(eng, se, LEVEL))
  vb <- NULL
  if (!control) {
    ci$F1 <- safe_ci(m167_transformed_wald(eng, se, LEVEL))
    f2 <- safe_reps(suppressWarnings(m167_param_reps(eng, N, BOOTS)))
    ci$F2p <- safe_ci(m167_percentile(f2, LEVEL))
    ci$F2bc <- safe_ci(m167_bc(f2, eng, LEVEL))
    ci$F2b <- safe_ci(m167_basic(f2, eng, LEVEL))
    boots$F2 <- boot_summary(f2, eng)
    ci$P1 <- safe_ci(suppressWarnings(m167_profile_ci(eng, R, N, LEVEL)))
    if (raw) {
      e_b <- try(suppressWarnings(cpm_engine(R, angles = angles, m = 3,
                                             variant = "B")), silent = TRUE)
      if (!inherits(e_b, "try-error")) {
        vb <- list(zeta = e_b$zeta, beta = e_b$beta,
                   heywood = isTRUE(e_b$heywood),
                   accepted = isTRUE(e_b$accepted))
      }
    }
  }
  scored <- lapply(ci, score_ci, cfg = cfg, free = free)
  list(
    excluded = FALSE,
    est = list(angle = eng$theta[free], zeta = eng$zeta, beta = eng$beta),
    heywood = isTRUE(eng$heywood),
    n_heywood = sum(eng$zeta > 0.995),
    se_defined = se_defined,
    removed = eng$beta == 0,
    ci = ci,
    sides = lapply(scored, `[[`, "side"),
    widths = lapply(scored, `[[`, "width"),
    boots = boots,
    vb = vb
  )
}

wilson <- function(k, n) {
  if (n == 0) return(c(NA_real_, NA_real_))
  as.numeric(stats::prop.test(k, n, correct = FALSE)$conf.int)
}

cover_rate <- function(m, rows) {
  m <- m[rows, , drop = FALSE]
  colSums(m == 0, na.rm = TRUE) / colSums(!is.na(m))
}

# per-component summary of one method and family
summarize_family <- function(reps, method, fam) {
  m <- do.call(rbind, lapply(reps, function(r) r$sides[[method]][[fam]]))
  w <- do.call(rbind, lapply(reps, function(r) r$widths[[method]][[fam]]))
  n_def <- colSums(!is.na(m))
  k <- colSums(m == 0, na.rm = TRUE)
  cov <- k / n_def
  vac_lim <- if (fam == "angle") 180 else 0.5
  vacuous <- colSums(w > vac_lim, na.rm = TRUE) / n_def
  out <- data.frame(
    component = seq_len(ncol(m)),
    coverage = cov,
    miss_low = colSums(m == -1, na.rm = TRUE) / n_def,
    miss_high = colSums(m == 1, na.rm = TRUE) / n_def,
    n = n_def,
    n_na = colSums(is.na(m)),
    undefined_share = colSums(is.na(m)) / nrow(m),
    width_median = apply(w, 2, stats::median, na.rm = TRUE),
    vacuous_share = vacuous
  )
  wl <- t(mapply(wilson, k, n_def))
  out$wilson_lo <- wl[, 1]
  out$wilson_hi <- wl[, 2]
  out$allowance <- allowance(n_def)
  out$pass <- n_def > 0 & !is.na(cov) &
    cov >= BAND[1] - out$allowance & cov <= BAND[2] + out$allowance &
    vacuous <= VACUOUS_MAX
  out$unconditional <- out$undefined_share <= UNDEFINED_MAX
  # descriptive splits
  sed <- vapply(reps, `[[`, logical(1), "se_defined")
  hw <- vapply(reps, `[[`, logical(1), "heywood")
  out$cov_se_defined <- cover_rate(m, sed)
  out$cov_heywood <- cover_rate(m, hw)
  out$cov_not_heywood <- cover_rate(m, !hw)
  if (fam == "beta") {
    kept <- !do.call(rbind, lapply(reps, `[[`, "removed"))
    mk <- m
    mk[!kept] <- NA
    out$cov_kept <- colSums(mk == 0, na.rm = TRUE) / colSums(!is.na(mk))
  }
  out
}

component_names <- list(
  angle = scales[-1], zeta = scales, beta = paste0("beta", 0:3)
)

# estimator bias and SD, bootstrap bias estimates, and the VB arm per cell
summarize_bias <- function(reps, cfg) {
  truth <- list(angle = (angles %% 360)[-1], zeta = cfg$zeta, beta = cfg$beta)
  est <- lapply(stats::setNames(FAMS, FAMS), function(f) {
    m <- do.call(rbind, lapply(reps, function(r) r$est[[f]]))
    if (f == "angle") {
      d <- ((sweep(m, 2, truth$angle) + 180) %% 360) - 180
      return(data.frame(component = component_names[[f]],
                        bias = colMeans(d), sd = apply(d, 2, stats::sd)))
    }
    data.frame(component = component_names[[f]],
               bias = colMeans(m) - truth[[f]], sd = apply(m, 2, stats::sd))
  })
  boot_bias <- lapply(c(B1 = "B1", F2 = "F2"), function(b) {
    bs <- lapply(reps, function(r) r$boots[[b]])
    keep <- !vapply(bs, is.null, logical(1))
    if (!any(keep)) return(NULL)
    zb <- do.call(rbind, lapply(which(keep), function(j) {
      bs[[j]]$zeta_mean - reps[[j]]$est$zeta
    }))
    bb <- do.call(rbind, lapply(which(keep), function(j) {
      bs[[j]]$beta_mean - reps[[j]]$est$beta
    }))
    list(zeta = colMeans(zb), beta = colMeans(bb),
         replicate_heywood_share = mean(vapply(bs[keep], `[[`, numeric(1),
                                               "heywood_share")),
         beta_z0_mean = colMeans(do.call(rbind, lapply(bs[keep], `[[`,
                                                       "beta_z0"))))
  })
  vbs <- lapply(reps, `[[`, "vb")
  vbs <- vbs[!vapply(vbs, is.null, logical(1))]
  # the VB summaries read accepted variant-B fits only; the share accepted is
  # reported beside them
  n_vb <- length(vbs)
  vbs <- vbs[vapply(vbs, `[[`, logical(1), "accepted")]
  vb <- NULL
  if (length(vbs) > 1) {
    bm <- do.call(rbind, lapply(vbs, `[[`, "beta"))
    zm <- do.call(rbind, lapply(vbs, `[[`, "zeta"))
    sd0 <- stats::sd(bm[, 1])
    hey <- mean(vapply(vbs, `[[`, logical(1), "heywood"))
    vb <- list(beta_bias = colMeans(bm) - cfg$beta,
               beta_sd = apply(bm, 2, stats::sd),
               zeta_bias = colMeans(zm) - cfg$zeta,
               heywood_rate = hey,
               n_accepted = length(vbs),
               accepted_rate = length(vbs) / n_vb,
               mechanism_confirmed = abs(mean(bm[, 1]) - cfg$beta[1]) <
                 0.25 * sd0 && hey < 0.05)
  }
  list(estimator = est, boot_bias = boot_bias, vb = vb)
}

# Per-replicate records (RR26 rec 1) packed as rounded matrices, so the
# committed results file stays a few MB: estimates, flags, every method's
# interval endpoints (component x lower/upper x method), the two bootstraps'
# replicate means, SDs, z0 and replicate Heywood share, and the VB arm.
# Sides and widths are derivable from the endpoints and the truths.
pack_replicates <- function(reps, methods) {
  comp <- c(paste0("angle_", component_names$angle),
            paste0("zeta_", component_names$zeta),
            component_names$beta)
  est <- t(vapply(reps, function(r) unlist(r$est, use.names = FALSE),
                  numeric(length(comp))))
  colnames(est) <- comp
  flags <- data.frame(
    heywood = vapply(reps, `[[`, logical(1), "heywood"),
    n_heywood = vapply(reps, `[[`, integer(1), "n_heywood"),
    se_defined = vapply(reps, `[[`, logical(1), "se_defined")
  )
  flags <- cbind(flags, setNames(as.data.frame(
    do.call(rbind, lapply(reps, `[[`, "removed"))),
    paste0("removed_beta", 0:3)))
  free <- 2:p
  ends <- array(NA_real_, c(length(reps), length(comp), 2, length(methods)),
                dimnames = list(NULL, comp, c("lower", "upper"), methods))
  for (j in seq_along(reps)) {
    for (meth in methods) {
      ci <- reps[[j]]$ci[[meth]]
      if (is.null(ci)) next
      ends[j, , , meth] <- rbind(ci$angle[free, , drop = FALSE], ci$zeta,
                                 ci$beta)
    }
  }
  boot_mat <- function(b) {
    rows <- lapply(reps, function(r) {
      s <- r$boots[[b]]
      if (is.null(s)) return(rep(NA_real_, 37))
      c(s$zeta_mean, s$zeta_sd, s$beta_mean, s$beta_sd, s$zeta_z0, s$beta_z0,
        s$heywood_share)
    })
    m <- do.call(rbind, rows)
    colnames(m) <- c(paste0("zeta_mean_", 1:8), paste0("zeta_sd_", 1:8),
                     paste0("beta_mean_", 0:3), paste0("beta_sd_", 0:3),
                     paste0("zeta_z0_", 1:8), paste0("beta_z0_", 0:3),
                     "replicate_heywood_share")
    round(m, 5)
  }
  vb <- do.call(rbind, lapply(reps, function(r) {
    v <- r$vb
    if (is.null(v)) return(rep(NA_real_, 14))
    c(v$zeta, v$beta, v$heywood, v$accepted)
  }))
  colnames(vb) <- c(paste0("zeta_", 1:8), paste0("beta_", 0:3), "heywood",
                    "accepted")
  list(est = round(est, 6), flags = flags, endpoints = round(ends, 5),
       boot_B1 = boot_mat("B1"), boot_F2 = boot_mat("F2"), vb = round(vb, 6))
}

summarize_cell <- function(out, methods, cfg) {
  ok <- !vapply(out, function(r) inherits(r, "try-error") || is.null(r),
                logical(1))
  n_error <- sum(!ok)
  out <- out[ok]
  excl <- vapply(out, `[[`, logical(1), "excluded")
  reps <- out[!excl]
  by_method <- lapply(stats::setNames(methods, methods), function(meth) {
    lapply(stats::setNames(FAMS, FAMS), function(f) {
      if (meth %in% NO_ANGLE && f == "angle") return(NULL)
      s <- summarize_family(reps, meth, f)
      s$component <- component_names[[f]]
      s
    })
  })
  # pooled as M4 pooled it: all components and replicates with an interval
  pooled <- lapply(by_method, function(mm) vapply(mm, function(s) {
    if (is.null(s)) NA_real_ else sum(s$coverage * s$n) / sum(s$n)
  }, numeric(1)))
  removed <- do.call(rbind, lapply(reps, `[[`, "removed"))
  list(
    n_reps = length(reps), n_excluded = sum(excl), n_error = n_error,
    heywood_rate = mean(vapply(reps, `[[`, logical(1), "heywood")),
    se_defined_rate = mean(vapply(reps, `[[`, logical(1), "se_defined")),
    removed_rate = colMeans(removed),
    by_method = by_method, pooled = pooled,
    bias = if (control) NULL else summarize_bias(reps, cfg),
    replicates = if (control) NULL else pack_replicates(reps, methods)
  )
}

run_cell <- function(cfg_name, N, raw, truth_fit, seed_base) {
  cfg <- configs[[cfg_name]]
  out <- parallel::mclapply(seq_len(REPS), function(i) {
    try(one_replicate(cfg_name, N, i, raw, truth_fit, cfg, seed_base),
        silent = TRUE)
  }, mc.cores = CORES, mc.preschedule = FALSE)
  summarize_cell(out, if (raw) RAW_METHODS else COR_METHODS, cfg)
}

# ---- verdict: applies the pre-registered pass rule -----------------------------

PATHS <- list(
  raw = list(cells = NS_RAW, methods = c("B1", "F3", "Bb", "B2", "F1", "P1")),
  cor = list(cells = c(NS_RAW, NS_COR),
             methods = c("B2", "F1", "F2p", "F2bc", "F2b", "P1"))
)

cell_status <- function(results, meth, fam, N) {
  ss <- lapply(names(configs), function(cn) {
    results[[sprintf("%s_N%d", cn, N)]]$by_method[[meth]][[fam]]
  })
  if (any(vapply(ss, is.null, logical(1)))) return(c(pass = NA, uncond = NA))
  pass <- all(vapply(ss, function(s) all(s$pass), logical(1)))
  uncond <- all(vapply(ss, function(s) all(s$unconditional), logical(1)))
  c(pass = pass, uncond = pass && uncond)
}

ladder_from <- function(ns, ok) {
  from <- NA_real_
  for (j in rev(seq_along(ns))) if (isTRUE(ok[j])) from <- ns[j] else break
  from
}

verdict_table <- function(results) {
  rows <- list()
  for (path in names(PATHS)) {
    ns <- PATHS[[path]]$cells
    for (meth in PATHS[[path]]$methods) {
      for (fam in FAMS) {
        if (meth %in% NO_ANGLE && fam == "angle") next
        st <- vapply(ns, function(N) cell_status(results, meth, fam, N),
                     logical(2))
        rows[[length(rows) + 1]] <- data.frame(
          path = path, method = meth, family = fam,
          strict_pass = all(st["pass", ]),
          strict_pass_unconditional = all(st["uncond", ]),
          passes_from_N = ladder_from(ns, st["pass", ]),
          passes_from_N_unconditional = ladder_from(ns, st["uncond", ]),
          cells_passed = paste(ns[st["pass", ]], collapse = " ")
        )
      }
    }
  }
  do.call(rbind, rows)
}

# ---- run -----------------------------------------------------------------------

cell_filter <- Sys.getenv("M167_CELLS")
want <- function(key) {
  !nzchar(cell_filter) || key %in% strsplit(cell_filter, ",")[[1]]
}

confirm_cells <- NULL
if (confirm) {
  main <- readRDS(file.path("devel", "m167-interval-results.rds"))
  v <- main$verdict
  hits <- v[v$cells_passed != "", ]
  raw_hit_n <- unique(unlist(lapply(strsplit(hits$cells_passed, " "),
                                    as.numeric)))
  raw_hit_n <- intersect(raw_hit_n, NS_RAW)
  confirm_cells <- as.vector(outer(names(configs), raw_hit_n,
                                   function(a, b) sprintf("%s_N%d", a, b)))
  cat("confirmation cells:", if (length(confirm_cells)) confirm_cells else
    "none (no raw-data-cell pass)", "\n")
}

cat(sprintf("M167 interval oracle: reps=%d boots=%d cores=%d %s%s%s\n",
            REPS, BOOTS, CORES, if (smoke) "[SMOKE]" else "",
            if (control) "[CONTROL]" else "", if (confirm) "[CONFIRM]" else ""))
t0 <- Sys.time()
results <- list()
for (cfg_name in names(configs)) {
  truth_fit <- make_truth(configs[[cfg_name]])
  for (N in if (control || confirm) NS_RAW else c(NS_RAW, NS_COR)) {
    key <- sprintf("%s_N%d", cfg_name, N)
    if (!want(key)) next
    if (confirm && !key %in% confirm_cells) next
    cat(sprintf("[%s] %s ...\n", format(Sys.time(), "%H:%M:%S"), key))
    results[[key]] <- run_cell(cfg_name, N, N %in% NS_RAW, truth_fit,
                               if (confirm) SEED_CONF else if (N %in% NS_RAW)
                                 SEED_M4 else SEED_NEW)
    s <- results[[key]]
    cat(sprintf("  reps %d (excluded %d, errored %d), heywood %.2f\n",
                s$n_reps, s$n_excluded, s$n_error, s$heywood_rate))
    for (meth in names(s$pooled)) {
      pc <- s$pooled[[meth]]
      cat(sprintf("  %-5s pooled angle %s zeta %.3f beta %.3f\n", meth,
                  if (is.na(pc["angle"])) "  -  " else sprintf("%.3f", pc["angle"]),
                  pc["zeta"], pc["beta"]))
    }
  }
}
cat(sprintf("elapsed: %.1f min\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))

full_run <- !nzchar(cell_filter) && !control && !confirm && !smoke
verdict <- if (full_run || smoke) verdict_table(results) else NULL
if (!is.null(verdict)) print(verdict, row.names = FALSE)

# AC2 control: pooled B1 (bootstrap) and B2 (Wald) per family against the M4
# record, whose pooled figures are the DESIGN.md table to three decimals
m4 <- readRDS(file.path("devel", "m4-coverage-oracle-results.rds"))$results
control_table <- if (confirm) NULL else
  do.call(rbind, lapply(intersect(names(results), names(m4)), function(key) {
    do.call(rbind, lapply(FAMS, function(f) {
      m167 <- c(B1 = results[[key]]$pooled$B1[[f]],
                B2 = results[[key]]$pooled$B2[[f]])
      m4v <- c(B1 = m4[[key]][[paste0("boot_", f)]][["cov"]],
               B2 = m4[[key]][[paste0("ana_", f)]][["cov"]])
      data.frame(cell = key, family = f, method = names(m167),
                 m167 = unname(m167), m4 = unname(m4v),
                 diff = unname(m167 - m4v))
    }))
  }))
if (!is.null(control_table)) {
  cat("\nAC2 control (pooled, vs M4 record):\n")
  print(control_table, row.names = FALSE, digits = 3)
}

# the confirmation's own per-cell verdict, for the (method, path, family)
# entries the main verdict passed at a raw-data cell
confirm_table <- NULL
if (confirm && length(results) > 0) {
  confirm_table <- do.call(rbind, lapply(seq_len(nrow(hits)), function(j) {
    h <- hits[j, ]
    ns <- intersect(as.numeric(strsplit(h$cells_passed, " ")[[1]]), NS_RAW)
    do.call(rbind, lapply(ns, function(N) {
      st <- cell_status(results, h$method, h$family, N)
      data.frame(path = h$path, method = h$method, family = h$family, N = N,
                 confirmed = st[["pass"]],
                 confirmed_unconditional = st[["uncond"]])
    }))
  }))
  if (!is.null(confirm_table)) print(confirm_table, row.names = FALSE)
}

out_file <- file.path("devel", if (control) {
  "m167-interval-control.rds"
} else if (confirm) {
  "m167-interval-confirm.rds"
} else if (smoke) {
  "m167-interval-smoke.rds"
} else if (nzchar(cell_filter)) {
  "m167-interval-partial.rds"
} else {
  "m167-interval-results.rds"
})
saveRDS(list(results = results, verdict = verdict, control = control_table,
             confirm = confirm_table,
             reps = REPS, boots = BOOTS, configs = configs, ns_raw = NS_RAW,
             ns_cor = NS_COR, band = BAND,
             allowance = "1.4 * sqrt(.93 * .07 / n)",
             vacuous_max = VACUOUS_MAX, undefined_max = UNDEFINED_MAX,
             seeds = c(m4 = SEED_M4, new = SEED_NEW, confirm = SEED_CONF),
             commit = RUN_COMMIT,
             date = Sys.Date()), out_file, compress = "xz")
cat("saved:", out_file, "\n")
