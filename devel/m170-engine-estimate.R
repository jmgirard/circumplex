# ==== PRE-REGISTRATION BEGIN ====
# M170: how often and by how much a lower discrepancy than cpm_engine()'s
# estimate exists, how the engine's estimate compares with two alternatives,
# and which solution the estimate should be.
#
# This block is the study's pre-registration. It fixes the truths, cells,
# seeds, estimators, metrics, saturation rule and decision rule before any
# full-run output exists. Changes after the design review (RB28/RR28) and
# before the timing pilot are listed under CORRECTIONS at the end of this
# block. After the first full run, nothing in this block changes except a
# correction that leaves the truths, cells, seeds, estimators, metrics,
# saturation rule and decision rule as they are, listed there with its date.
# "Full run" means a run with none of the M170_* mode variables set.
#
# TRUTHS (M167's, from the M4 truths): p = 8 octant scales at octants()
# angles, communality .75 for every scale, m = 3, variant A, unit scaling,
# reference scale 1 (PA, fixed angle).
#   boundary: beta = .45 / .35 / .15 / .05
#   interior: beta = .35 / .30 / .20 / .15
#
# CELLS (10): both truths at
#   raw-data N = 250, 500, 1000: rows from cpm_simulate() of the exact truth
#     fit, R = cor(rows) (M167's raw-data generator);
#   correlation-matrix N = 2000, 5000: R = cov2cor() of one
#     Wishart(N - 1, P_truth) draw, the exact distribution of cor() of N
#     normal rows (M167's correlation-matrix generator).
#   Fresh seeds: replicate i of truth t (1 boundary, 2 interior) at the j-th
#   N of the list 250, 500, 1000, 2000, 5000 runs set.seed(40261027 + 1e6 t +
#   1e3 j + i) before drawing its data. H's random starts (below) use their
#   own seed, the replicate's seed + 5e8. No seed of M4 or M167 (bases
#   20260706, 20261021, 30261022, each below 3.1e7 + 6e6) is reused.
#
# ESTIMATORS (devel/m170-engine-estimate-helpers.R). Coordinates are the
# engine's unconstrained ones: free angles in radians, communality logits u,
# weight logits v (v_0 = 0).
#   E  cpm_engine() as shipped (variant A, unit scaling). Only fits E accepts
#      enter the study; an E fit that errors or is not accepted is excluded
#      from every estimator, and the count is recorded per cell.
#   W  E plus a fixed deterministic set of 101 extra starts (p = 8). Around
#      E's estimate, placed in the full m = 3 layout (a weight the polish
#      removed is floored at 1e-4 and the weights renormalized, so its logit
#      is finite; each communality logit is E's own, clipped to [-30, 30], so
#      a communality at 1 in double precision is placed at logit 30): one
#      communality-wall start per scale (that scale's u set to logit .99, 8),
#      one all-communality-level start (every u set to logit .99), one start
#      per free angle and shift, each free angle moved by +90, -90 and 180
#      degrees (21), one start per pair of free angles with their two values
#      swapped (21), and each swap again with the wall on one of the two
#      swapped scales (42). Around g0, the battery's theory start: one
#      communality-wall start per scale (8). Each start is refit with
#      cpm_optimize_one() (the engine's own nlminb settings). W keeps the
#      lowest discrepancy among E and these refits.
#   H  the lowest discrepancy among E, W's 101 starts, and two families of 2K
#      random starts around g0, one at sd .7 and one at sd 1.2: each start is
#      g0 plus independent N(0, sd^2) draws on every coordinate. K = 100, so
#      H runs 501 starts.
#   H_K  as H with only the first K random starts of each family (301 starts).
#   A refit replaces E in W or H only when it finds a lower discrepancy (the
#   THRESHOLD below), so the fits where W or H differs from E are exactly its
#   lower fits. W and H finish their point as E does: the beta boundary polish
#   (cpm_polish_beta()), then cpm_canonicalize(). A start whose refit throws
#   is skipped and counted. A point whose finish throws leaves that estimator
#   at E for that fit, and is counted.
#
# THRESHOLD. T = (N - 1) F, the scale of the fit statistic. A refit "finds a
# lower discrepancy" on a fit when (N - 1) (F_E - F_refit) > 0.01, F_refit
# read before the polish. W's and H's gaps are read after their polish, as
# E's F is. A lower refit counts whether or not its point meets the engine's
# gradient criterion (a lower value of F exists either way); the count of
# finished W and H points that fail it is reported per cell.
#
# SATURATION RULE (per cell). Two parts, both needed for a pass.
#   S1 (doubling): the share of accepted E fits on which H's lowest refit lies
#      more than the threshold below H_K's, both read before the polish, is at
#      most .02.
#   S2 (independent family): on the subsample of the first 50 accepted E fits
#      of the cell, in replicate order, 4K = 400 starts drawn with no centre
#      (each free angle uniform on [0, 2 pi), each communality uniform on
#      [.3, .99], each weight logit N(0, 1); m170_uniform_starts()), drawn
#      from H's seeded stream after H's own starts, find a discrepancy more
#      than the threshold below H's on at most 1 of the 50 fits. The family
#      runs on replicates 1 to 80; if fewer than 50 of them are accepted, S2
#      is a fail. Recorded beside S2: the family's median hit rate (the share
#      of its 400 runs ending within the threshold of H's lowest F), read with
#      a pass because a family that seldom reaches H's basin cannot certify
#      that nothing lies below it; and, on the fits where the family finds a
#      point below H, its finished best point's share with a changed cyclic
#      order and with more Heywood scales than E.
#   A cell whose verdict is a fail has H's shares reported as lower bounds,
#   in the record and in the D-entry. H's estimate-quality figures describe
#   H, never the global minimum, in every cell.
#   Expected outcome, written before the run (RR27, RR28): S1 or S2 fails at
#   N <= 1000 at both truths and at interior N = 2000, so branches 1 and 2
#   below are expected to be unreachable and the decision is expected among
#   branches 3 to 6. A fail is a result, not a defect of the study.
#
# METRICS. Every metric is computed per cell over accepted E fits, from the
# results file devel/m170-engine-estimate-results.rds. "Lower fits of X" are
# the accepted E fits on which X finds a lower discrepancy, which are the fits
# where X differs from E.
#   M1  (AC2) share_lower(H_K) and share_lower(H): the share of accepted E
#       fits on which H_K, and H, find a lower discrepancy.
#   M2  (AC2) saturation verdict: S1, S2 and their figures.
#   M3  (AC3, for each of W and H)
#       (a) share_lower(X), and split into the share whose lower point keeps
#           E's cyclic order and the share whose point reorders it;
#       (b) gap_median(X) and gap_max(X): the median and largest
#           (N - 1) (F_E - F_X) over the lower fits of X; and
#           gap_median_lcb(X), the one-sided 95% lower confidence bound on
#           the median, the order statistic of the sorted gaps at rank
#           qbinom(.05, n_lower(X), .5) (undefined when that rank is 0);
#       (c) heywood_more(X): the share of the lower fits of X whose point has
#           more scales with communality above .995 than E's estimate;
#       (d) order_changed(X): the share of the lower fits of X whose cyclic
#           angle order, read counterclockwise from the reference on the
#           cpm_canonicalize()d solution, differs from E's;
#       (e) cpu_W and cpu_E: the median CPU seconds per fit of W (including
#           E) and of E alone, from the timing pilot (one worker, nothing else
#           running), stored in the results file.
#       (f) W's detection power: recall(W) = the share of H's lower fits that
#           are also W's; share_missed_H(W) = the share of accepted fits on
#           which H finds a lower discrepancy and W does not; and on S2's
#           subsample, the share on which H or the independent family finds
#           one and W does not. The D-entry carries these beside any
#           diagnostic, because a W-based diagnostic's silence is what a user
#           will over-read.
#   M4  (AC4, for each of E, W and H, per parameter: 7 free angles, 8
#       communalities, 4 weights)
#       (a) bias and RMSE: angles as the signed circular difference from the
#           truth in degrees, in (-180, 180]; communalities and weights as the
#           plain difference;
#       (b) se_defined: the share of fits whose cpm_analytic_se() standard
#           error at that estimator's point is finite; wald_coverage: on those
#           fits, the share whose interval estimate +/- 1.96 SE covers the
#           truth (an angle covers when its circular difference is at most
#           1.96 SE; a weight removed by the polish has SE 0 and the interval
#           [0, 0], a miss);
#       (c) t_exceed: the share of fits whose T at that estimator's point
#           exceeds qchisq(.95, df), df the degrees of freedom of the model as
#           fitted at that point (after its polish). It measures the fit test
#           at that point, not the estimate; at a Heywood point the
#           chi-square reference is approximate for every estimator.
#   Also reported per cell: excluded fits; replicates that errored after E
#   (counted, with their messages, in the record and in the D-entry when any
#   cell has one); failed starts; finish errors; gradient failures.
#   Family summaries the decision rule reads, per cell and estimator X:
#       rmse_angle(X), rmse_zeta(X), rmse_beta(X): M4(a)'s RMSE averaged over
#         the family's parameters;
#       cov_dev(X): M4(b)'s |wald_coverage - .95| averaged over the 19
#         parameters;
#       se_def(X): M4(b)'s se_defined averaged over the 19 parameters;
#       t_dev(X): |t_exceed(X) - .05|.
#   no_worse(X, c), X in {W, H}: in cell c, every one of
#       rmse_angle(X) <= rmse_angle(E), rmse_zeta(X) <= rmse_zeta(E),
#       rmse_beta(X) <= rmse_beta(E), cov_dev(X) <= cov_dev(E) + .01,
#       se_def(X) >= se_def(E) - .02, t_dev(X) <= t_dev(E) + .01.
#   improves(X, c): in cell c, at least one of the three RMSE figures is at
#     most .95 times E's.
#   favors(X): no_worse(X, c) in every cell and improves(X, c) in at least
#     one. An NA among their inputs makes the part it enters FALSE, so
#     adoption fails closed.
#   sat(c): the saturation verdict of cell c is a pass.
#   cpu_ok(X): the pilot's median CPU seconds per fit of X (including E) is at
#     most 10 in every cell.
#
# DECISION RULE. Read in order; the first branch whose condition holds
# selects the option. Every cell means all ten cells. The options that change
# the estimate are judged on every cell, because a changed estimator would
# change every fit the package makes.
#   0. If share_lower(W) is undefined (NA) in any cell, or gap_median_lcb(W)
#      in a cell where branch 5 reads it, or the pilot's CPU figure for W in
#      any cell: KEEP E UNCHANGED (fail closed). An NA that only branches 1
#      and 2 read closes those branches instead (favors() and cpu_ok()).
#   1. If sat(c) holds in every cell, favors(W), and cpu_ok(W): REPORT W.
#   2. If sat(c) holds in every cell, favors(H), and cpu_ok(H): REPORT H.
#      "Report H" means a later milestone ships a search shown to reach the
#      same point whatever its seed in every cell; the study's H is that
#      search's reference, not its definition.
#   3. If share_lower(W) < .05 in every cell: KEEP E UNCHANGED. (W, the only
#      search a shipped version can run deterministically at this cost,
#      seldom finds a lower point, so a diagnostic built on it would seldom
#      fire; the help page's statement covers the rest.)
#   4. If not cpu_ok(W): KEEP E UNCHANGED.
#   5. If some cell has share_lower(W) >= .05 and gap_median_lcb(W) >= 2.0:
#      REPORT E WITH THE LOWEST DISCREPANCY FOUND BESIDE IT. (Where W's
#      typical gap is clearly 2 T units or more, the size of the
#      overstatement is itself what a reader needs: at df = 10, T = 15 moves
#      from p = .13 to p = .22 at T = 13. The bound keeps a median near 2.0
#      from deciding the branch by Monte Carlo noise.)
#   6. Otherwise (default): KEEP E AND ADD A DIAGNOSTIC FOR A LOWER OPTIMUM
#      OUTSIDE THE THEORETICAL CONFIGURATION.
#   Tie-break under GP4 and IP3. Branch 6 is reached when W finds a lower
#   point on 5% or more of the fits in some cell but its median gap is not
#   clearly 2 T units or more wherever it does. There both the diagnostic and
#   the report-beside option would tell a reader that T is overstated, and
#   the gap is too small for its size to carry the reading, so the figures do
#   not choose between them. The diagnostic is chosen. Under GP4 it adds one
#   marker and one caution line to the returned object and the printed
#   output, where the report-beside option commits the package to a second
#   fit statistic. Under IP3 that second statistic would be a shipped numeric
#   result needing two independent oracles and a T-calibration re-run, while
#   a flag is a diagnostic. And W's lowest discrepancy is not the model's
#   minimum either, so a second T printed beside E's would be read as the
#   true fit when it is not.
#   A shipped diagnostic or report-beside option runs W's deterministic
#   starts, never H's random ones (IP4). "Lowest discrepancy found" then means
#   W's.
#
# TIMING PILOT AND REPLICATE COUNT. The pilot runs the first 10 replicate
# seeds of every cell on one worker, nothing else running, and records per
# fit the CPU seconds of E, of W (including E), of the whole replicate
# (E, W, H, the analytic SEs at each point) and of the independent family.
# Its other output is not reported. The replicate count per cell is
#   R = the largest multiple of 50 in [100, 500] with
#       1.2 * sum over cells of (R * fit_c + 80 * fam_c) <= 12 * 3600,
#   fit_c and fam_c the pilot's median CPU seconds per replicate and per
#   independent-family run in cell c. If R = 100 does not fit, the full run
#   does not start and the milestone stops for the user. R is written to
#   the pilot file, which the full run reads.
#
# REAL AND PUBLISHED FITS (AC5), each at the settings its oracle test or
# vignette uses, with H's seed 50261027 + j:
#   j = 1 jz2017's PANO() octants, raw-data path (listwise rows, R = cor(rows),
#         N = rows), octants(), m = 3, variant A;
#   j = 2 cpm_oracle_voc(): R, N = 175, angles th_start, m = 1, variant A;
#   j = 3 cpm_oracle_verbal(): R, N = 1046, angles 360 (0:5) / 6, m = 2,
#         variant A;
#   j = 4 the cpm-boundary-fits vignette's `sim`: set.seed(12345), the
#         jz2017 fit with boots = 500, then set.seed(2026) and
#         cpm_simulate(that fit, n = 2500);
#         R = cor(sim), N = 2500, octants(), m = 3, variant A (the `demo`
#         fit). The vignette marks this chunk volatile across BLAS builds,
#         so the record names the machine the sample was drawn on.
#   For each: E's T and estimates; W's and H's gaps; where a gap exceeds the
#   threshold, that estimator's estimates and any change in angle order.
#
# CORRECTIONS after the design review (RR28, 2026-10-27), before the pilot.
# The block above already reads as corrected; this list names each change.
#   1. E's estimate is placed in the full layout with each communality logit
#      clipped to [-30, 30]; a communality at 1 in double precision had made
#      every W start and every E-centred random start infinite, so W silently
#      equalled E on those fits (RR28 rec 1).
#   2. W grows from 30 to 101 starts: 21 pairwise angle swaps, 42 swaps with
#      the wall on one swapped scale, and 8 wall starts around g0 (rec 2).
#   3. A refit replaces E only when it finds a lower discrepancy (more than
#      0.01 T units), not when it lies 1e-8 below (rec 3).
#   4. Branch 0 reads only what branches 3 to 5 read; an NA that only
#      branches 1 and 2 read closes them (rec 4).
#   5. favors(X) splits: no_worse in every cell, improves in at least one
#      (rec 5).
#   6. Branch 5 reads the lower confidence bound on W's median gap, not the
#      median (rec 6).
#   7. The `demo` fit's sample is drawn after set.seed(2026), as the
#      vignette draws it (rec 7).
#   8. M3 gains (f), W's detection power, and the order split of (a); S2
#      records the family's hit rate and its lower points' order and Heywood
#      figures (rec 8).
#   9. The expected outcome of the saturation rule is stated (rec 9).
#  10. "Report H" is defined for shipping (rec 10).
#  11. S1 reads both values before the polish; a lower refit counts whatever
#      its gradient; errors after E are reported with their messages
#      (rec 11).
#  12. The tie-break names IP3 beside GP4 (rec 12).
#  13. H's random starts: both families around g0, at sd .7 and 1.2, in
#      place of E's estimate at sd .7 and g0 at sd .7. On RR27's six cases,
#      boundary replicate 6 and 11 interior N = 250 fits (M167's seeds), the
#      new design reached a lower point than the old on 1 fit (by 0.087 T
#      units) and the old design on none (rec 13).
#
# CORRECTIONS after the full run (M170 review, 2026-10-27). None changes the
# truths, cells, seeds, estimators, metric definitions, saturation rule or
# decision rule. Each makes the code follow the definitions above. The
# metrics and the decision are recomputed from the stored per-replicate
# records (M170_RECOMPUTE=1), with nothing re-run.
#   1. Wald coverage (M4(b)) is computed only on fits whose SE is defined.
#      The full run forced a miss for a removed weight even where the fit's
#      SE was undefined, which put such fits in the denominator.
#   2. improves() is FALSE when any of its inputs is NA, as favors() states.
#      No input was NA.
#   3. M3(f)'s subsample share counts a family point that lies more than the
#      threshold below E, not below H.
#   4. The family hit rate counts a failed start as a run that missed H's
#      basin. No start failed, so the stored rates are unchanged.
#   5. The D-entry cites M3(f)'s figures in the record's W table rather than
#      carrying them, because the repo's tracking rules keep counts and
#      measurements out of D-entries.
# ==== PRE-REGISTRATION END ====
#
# Usage:  M170_PILOT=1 Rscript devel/m170-engine-estimate.R  # timing pilot
#         Rscript devel/m170-engine-estimate.R               # full run
#         M170_SMOKE=1 Rscript devel/m170-engine-estimate.R  # smoke run
#         M170_REAL=1 Rscript devel/m170-engine-estimate.R   # AC5 fits only
#         M170_RECOMPUTE=1 Rscript devel/m170-engine-estimate.R  # metrics from
#                                                  # the stored replicates
# The pilot writes devel/m170-engine-estimate-pilot.rds, which the full run
# reads for R and for M3(e). The full run writes
# devel/m170-engine-estimate-results.rds; the smoke run (4 replicates per
# cell, K = 5) and the AC5-only run write other names and are never results.
#
# Reproducibility: each replicate runs set.seed() on its own seed, so the
# results do not depend on mc.cores or scheduling.

devtools::load_all(".", quiet = TRUE)
source(file.path("devel", "m170-engine-estimate-helpers.R"))
source(file.path("tests", "testthat", "helper-cpm-oracles.R"))

pilot <- nzchar(Sys.getenv("M170_PILOT"))
smoke <- nzchar(Sys.getenv("M170_SMOKE"))
real_only <- nzchar(Sys.getenv("M170_REAL"))
K <- if (smoke) 5L else 100L
THRESH <- 0.01
NS <- c(250, 500, 1000, 2000, 5000)
NS_RAW <- c(250, 500, 1000)
SEED_DATA <- 40261027
SEED_H <- 5e8
SEED_REAL <- 50261027
SUBSAMPLE <- 50L
FAMILY_WINDOW <- 80L
BUDGET <- 12 * 3600
RUN_COMMIT <- tryCatch(system("git rev-parse HEAD", intern = TRUE),
                       error = function(e) NA_character_)
RUN_STATUS <- tryCatch(system("git status --porcelain", intern = TRUE),
                       error = function(e) NA_character_)
CORES <- if (pilot) 1L else max(1, parallel::detectCores() - 1)

p <- 8
angles <- octants()
scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
free <- 2:p
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

cpu_now <- function() {
  pt <- proc.time()
  pt[["user.self"]] + pt[["sys.self"]]
}

# ---- one estimator's per-fit record --------------------------------------------

PAR_NAMES <- c(paste0("angle_", scales[free]), paste0("zeta_", scales),
               paste0("beta", 0:3))

# estimates, standard errors, Wald coverage of the truth, T and df at one
# estimator's point (E's own engine object, or a finished W or H point)
point_record <- function(est, R, N, cfg) {
  se <- tryCatch(suppressWarnings(cpm_analytic_se(est, R, N)),
                 error = function(e) NULL)
  if (is.null(se)) {
    se <- list(angle = rep(NA_real_, p), zeta = rep(NA_real_, p),
               beta = rep(NA_real_, 4))
  }
  err <- m170_errors(est, angles %% 360, cfg$zeta, cfg$beta, free)
  e <- c(err$angle, err$zeta, err$beta)
  s <- c(se$angle[free], se$zeta, se$beta)
  s[!is.finite(s)] <- NA_real_
  # A removed weight has SE 0 and the interval [0, 0], which misses every
  # truth weight (all > 0) without an override. Where the SE is undefined
  # (NA), coverage is undefined too: the fit is outside M4(b)'s set.
  covered <- abs(e) <= 1.96 * s
  list(err = stats::setNames(e, PAR_NAMES), se = stats::setNames(s, PAR_NAMES),
       covered = stats::setNames(covered, PAR_NAMES),
       T = (N - 1) * est$F, df = est$df, n_heywood = m170_n_heywood(est),
       order = m170_cyclic_order(est))
}

# ---- one replicate ---------------------------------------------------------------

replicate_seed <- function(ti, N, i) SEED_DATA + 1e6 * ti + 1e3 * match(N, NS) + i

draw_R <- function(truth_fit, N) {
  if (N %in% NS_RAW) {
    stats::cor(cpm_simulate(truth_fit, N))
  } else {
    stats::cov2cor(stats::rWishart(1, N - 1, truth_fit$matrices$Phat)[, , 1])
  }
}

one_replicate <- function(cfg_name, N, i, truth_fit, with_family) {
  cfg <- configs[[cfg_name]]
  seed <- replicate_seed(match(cfg_name, names(configs)), N, i)
  set.seed(seed)
  R <- draw_R(truth_fit, N)
  c0 <- cpu_now()
  eng <- m170_engine_e(R, angles, 3)
  cpu_E <- cpu_now() - c0
  if (is.null(eng) || !isTRUE(eng$accepted)) {
    return(list(excluded = TRUE, error = is.null(eng)))
  }
  extra <- if (with_family) list(family = m170_uniform_starts(4L * K)) else list()
  est <- m170_estimators(R, eng, 3, K = K, seed = seed + SEED_H, n1 = N - 1,
                         thresh = THRESH, extra = extra)
  c1 <- cpu_now()
  recs <- list(E = point_record(eng, R, N, cfg))
  recs$W <- if (is.null(est$W)) recs$E else point_record(est$W, R, N, cfg)
  recs$H <- if (is.null(est$H)) recs$E else point_record(est$H, R, N, cfg)
  cpu_se <- cpu_now() - c1
  fam <- if (with_family) est$extra$family else NULL
  list(
    excluded = FALSE, i = i,
    F = c(E = est$F_E, W = est$F_W, H = est$F_H, H_raw = est$F_H_raw,
          HK_raw = est$F_HK_raw,
          family = if (is.null(fam)) NA_real_ else fam$F),
    # an estimator differs from E exactly where it found a lower discrepancy
    replaced = c(W = !is.null(est$W), H = !is.null(est$H)),
    family = if (!is.null(fam)) {
      list(hit_rate = fam$hit_rate,
           n_heywood = if (is.null(fam$point)) NA else m170_n_heywood(fam$point),
           order_changed = if (is.null(fam$point)) NA else
             !identical(m170_cyclic_order(fam$point), recs$E$order))
    },
    recs = recs,
    n_failed = est$n_failed, finish_error = est$finish_error,
    grad_ok = c(W = if (is.null(est$W)) NA else est$W$grad_ok,
                H = if (is.null(est$H)) NA else est$H$grad_ok),
    cpu = c(E = cpu_E, W = cpu_E + est$cpu[["W"]], H = cpu_E + est$cpu[["H"]],
            replicate = cpu_E + est$cpu[["H"]] + cpu_se,
            family = est$cpu[["extra"]])
  )
}

run_cell <- function(cfg_name, N, truth_fit, reps) {
  out <- parallel::mclapply(seq_len(reps), function(i) {
    # the independent family runs on the first SUBSAMPLE accepted fits; the
    # count of accepted fits is not known in advance, so it runs on the first
    # FAMILY_WINDOW replicates and the summary keeps the first SUBSAMPLE
    # accepted ones (the pilot runs it on every replicate, for timing)
    try(one_replicate(cfg_name, N, i, truth_fit,
                      with_family = pilot || i <= FAMILY_WINDOW),
        silent = TRUE)
  }, mc.cores = CORES, mc.preschedule = FALSE)
  errored <- vapply(out, function(r) inherits(r, "try-error"), logical(1))
  list(reps = out[!errored], n_error = sum(errored),
       error_messages = unique(vapply(out[errored], as.character,
                                      character(1))))
}

# ---- metrics (the block's METRICS) -----------------------------------------------

cell_metrics <- function(cell, N, cfg) {
  reps <- cell$reps
  excl <- vapply(reps, `[[`, logical(1), "excluded")
  acc <- reps[!excl]
  n <- length(acc)
  Fm <- do.call(rbind, lapply(acc, `[[`, "F"))
  rep_m <- do.call(rbind, lapply(acc, `[[`, "replaced"))
  gap <- function(col) (N - 1) * (Fm[, "E"] - Fm[, col])
  lower <- list(W = rep_m[, "W"], H = rep_m[, "H"],
                HK = gap("HK_raw") > THRESH)

  # saturation: S1 on both lowest values before any polish; S2 on the first
  # SUBSAMPLE accepted fits that ran the family, in replicate order
  s1_share <- mean((N - 1) * (Fm[, "HK_raw"] - Fm[, "H_raw"]) > THRESH)
  has_fam <- which(!is.na(Fm[, "family"]))
  fam_rows <- has_fam[seq_len(min(SUBSAMPLE, length(has_fam)))]
  fam_lower <- (N - 1) * (Fm[fam_rows, "H_raw"] - Fm[fam_rows, "family"]) > THRESH
  s2_count <- sum(fam_lower)
  fam_rec <- lapply(acc[fam_rows], `[[`, "family")
  sat <- list(s1_share = s1_share, s1 = s1_share <= 0.02,
              s2_n = length(fam_rows), s2_count = s2_count,
              s2 = length(fam_rows) == SUBSAMPLE && s2_count <= 1,
              pass = s1_share <= 0.02 && length(fam_rows) == SUBSAMPLE &&
                s2_count <= 1,
              # read with a pass: a family that seldom reaches H's basin
              # cannot certify that nothing lies below it (RR28 Q3)
              family_hit_rate_median = stats::median(vapply(
                fam_rec, `[[`, numeric(1), "hit_rate")),
              family_order_changed = if (any(fam_lower)) mean(vapply(
                fam_rec[fam_lower], `[[`, logical(1), "order_changed")) else
                NA_real_,
              family_heywood_more = if (any(fam_lower)) mean(vapply(
                which(fam_lower), function(j) {
                  fam_rec[[j]]$n_heywood > acc[[fam_rows[j]]]$recs$E$n_heywood
                }, logical(1))) else NA_real_)

  search <- lapply(c(W = "W", H = "H"), function(x) {
    lw <- lower[[x]]
    g <- sort(gap(x)[lw])
    hey <- vapply(acc[lw], function(r) r$recs[[x]]$n_heywood > r$recs$E$n_heywood,
                  logical(1))
    ord <- vapply(acc[lw], function(r) !identical(r$recs[[x]]$order,
                                                  r$recs$E$order), logical(1))
    # one-sided 95% lower confidence bound on the median gap: the order
    # statistic at rank qbinom(.05, n, .5); undefined when that rank is 0
    r <- stats::qbinom(0.05, length(g), 0.5)
    list(share_lower = mean(lw), n_lower = sum(lw),
         gap_median = if (any(lw)) stats::median(g) else NA_real_,
         gap_median_lcb = if (r >= 1) g[r] else NA_real_,
         gap_max = if (any(lw)) max(g) else NA_real_,
         heywood_more = if (any(lw)) mean(hey) else NA_real_,
         order_changed = if (any(lw)) mean(ord) else NA_real_,
         share_lower_same_order = mean(lw) * (1 - if (any(lw)) mean(ord) else 0),
         share_lower_reordered = mean(lw) * (if (any(lw)) mean(ord) else 0))
  })
  # W's detection power against H, and against H or the family on the
  # subsample (RR28 Q6)
  search$W$recall <- if (any(lower$H)) sum(lower$W & lower$H) / sum(lower$H) else
    NA_real_
  search$W$share_missed_H <- mean(lower$H & !lower$W)
  fam_lower_E <- (N - 1) * (Fm[fam_rows, "E"] - Fm[fam_rows, "family"]) > THRESH
  search$W$share_missed_subsample <- mean((lower$H[fam_rows] | fam_lower_E) &
                                            !lower$W[fam_rows])

  quality <- lapply(c(E = "E", W = "W", H = "H"), function(x) {
    err <- do.call(rbind, lapply(acc, function(r) r$recs[[x]]$err))
    se <- do.call(rbind, lapply(acc, function(r) r$recs[[x]]$se))
    # coverage from the stored errors and SEs, defined only where the SE is
    # (M4(b)); the stored `covered` of the full run forced a miss for a
    # removed weight with an undefined SE (M170 review return 2)
    cov <- abs(err) <= 1.96 * se
    Tx <- vapply(acc, function(r) r$recs[[x]]$T, numeric(1))
    dfx <- vapply(acc, function(r) r$recs[[x]]$df, numeric(1))
    se_def <- colMeans(!is.na(se))
    wald <- colSums(cov, na.rm = TRUE) / colSums(!is.na(cov))
    per <- data.frame(parameter = PAR_NAMES, bias = colMeans(err),
                      rmse = sqrt(colMeans(err^2)), se_defined = se_def,
                      wald_coverage = wald, row.names = NULL)
    fam <- function(prefix) {
      mean(per$rmse[startsWith(per$parameter, prefix)])
    }
    t_exceed <- mean(Tx > stats::qchisq(0.95, dfx))
    list(per_parameter = per, t_exceed = t_exceed,
         rmse_angle = fam("angle_"), rmse_zeta = fam("zeta_"),
         rmse_beta = fam("beta"),
         cov_dev = mean(abs(wald - 0.95)), se_def = mean(se_def),
         t_dev = abs(t_exceed - 0.05))
  })

  list(n_reps = length(reps), n_excluded = sum(excl), n_error = cell$n_error,
       n_accepted = n,
       share_lower_HK = mean(lower$HK), share_lower_H = mean(lower$H),
       saturation = sat, search = search, quality = quality,
       n_failed_starts = colSums(do.call(rbind, lapply(acc, `[[`, "n_failed"))),
       n_finish_error = colSums(do.call(rbind, lapply(acc, `[[`,
                                                      "finish_error"))),
       grad_fail = colSums(!do.call(rbind, lapply(acc, `[[`, "grad_ok")),
                           na.rm = TRUE))
}

# favors(X, c) in two parts (RR28 Q4): no_worse() must hold in every cell and
# improves() in at least one. An NA among their inputs makes the part FALSE,
# so adoption fails closed.
no_worse <- function(q, x) {
  e <- q$E
  a <- q[[x]]
  isTRUE(all(a$rmse_angle <= e$rmse_angle, a$rmse_zeta <= e$rmse_zeta,
             a$rmse_beta <= e$rmse_beta, a$cov_dev <= e$cov_dev + 0.01,
             a$se_def >= e$se_def - 0.02, a$t_dev <= e$t_dev + 0.01))
}
improves <- function(q, x) {
  e <- q$E
  a <- q[[x]]
  x <- c(a$rmse_angle <= 0.95 * e$rmse_angle,
         a$rmse_zeta <= 0.95 * e$rmse_zeta,
         a$rmse_beta <= 0.95 * e$rmse_beta)
  !anyNA(x) && any(x)
}

# The block's DECISION RULE over the cells' metrics and the pilot's timing.
# Returns the option and the branch that selected it.
decide <- function(metrics, cpu) {
  wshare <- vapply(metrics, function(m) m$search$W$share_lower, numeric(1))
  # the gap bound is read only where share_lower(W) >= .05 (branch 5)
  gap_read <- vapply(metrics, function(m) {
    if (isTRUE(m$search$W$share_lower >= 0.05)) m$search$W$gap_median_lcb else 0
  }, numeric(1))
  # branch 0 reads only what branches 3 to 5 read; an NA in a favors() input
  # or in H's CPU closes branches 1 and 2 instead
  if (anyNA(c(wshare, gap_read, cpu$W))) {
    return(list(option = "keep E unchanged", branch = 0L))
  }
  sat <- vapply(metrics, function(m) isTRUE(m$saturation$pass), logical(1))
  favors <- function(x) {
    all(vapply(metrics, function(m) no_worse(m$quality, x), logical(1))) &&
      any(vapply(metrics, function(m) improves(m$quality, x), logical(1)))
  }
  cpu_ok <- function(x) isTRUE(all(cpu[[x]] <= 10))
  if (all(sat) && favors("W") && cpu_ok("W")) {
    return(list(option = "report W", branch = 1L))
  }
  if (all(sat) && favors("H") && cpu_ok("H")) {
    return(list(option = "report H", branch = 2L))
  }
  if (all(wshare < 0.05)) {
    return(list(option = "keep E unchanged", branch = 3L))
  }
  if (!cpu_ok("W")) {
    return(list(option = "keep E unchanged", branch = 4L))
  }
  if (any(wshare >= 0.05 & gap_read >= 2.0)) {
    return(list(option = "report E with the lowest discrepancy found beside it",
                branch = 5L))
  }
  list(option = paste("keep E and add a diagnostic for a lower optimum",
                      "outside the theoretical configuration"), branch = 6L)
}

# ---- AC5: real and published fits -----------------------------------------------

real_cases <- function() {
  data("jz2017", package = "circumplex", envir = environment())
  rows <- stats::na.omit(jz2017[PANO()])
  voc <- cpm_oracle_voc()
  verbal <- cpm_oracle_verbal()
  # the vignette's chunks: the jz2017 fit under seed 12345, then seed 2026
  # immediately before the draw (RR28 Q9.4)
  set.seed(12345)
  cpm <- suppressWarnings(cpm_fit(jz2017, scales = PANO(), angles = octants(),
                                  boots = 500))
  set.seed(2026)
  sim <- cpm_simulate(cpm, n = 2500)
  list(
    jz2017 = list(R = stats::cor(rows), N = nrow(rows), angles = octants(),
                  m = 3),
    voc = list(R = voc$R, N = voc$N, angles = voc$th_start, m = 1),
    verbal = list(R = verbal$R, N = verbal$N, angles = 360 * (0:5) / 6,
                  m = 2),
    demo = list(R = stats::cor(sim), N = 2500, angles = octants(), m = 3)
  )
}

real_fit <- function(case, j) {
  eng <- m170_engine_e(case$R, case$angles, case$m)
  n1 <- case$N - 1
  est <- m170_estimators(case$R, eng, case$m, K = K, seed = SEED_REAL + j,
                         n1 = n1, thresh = THRESH)
  show <- function(x) {
    list(theta = x$theta, zeta = x$zeta, beta = x$beta, T = n1 * x$F,
         df = x$df, order = m170_cyclic_order(x))
  }
  out <- list(N = case$N, m = case$m, accepted = eng$accepted, E = show(eng),
              gap = c(W = n1 * (est$F_E - est$F_W), H = n1 * (est$F_E - est$F_H)))
  for (x in c("W", "H")) {
    if (out$gap[[x]] > THRESH && !is.null(est[[x]])) {
      out[[x]] <- show(est[[x]])
      out[[x]]$order_changed <- !identical(out[[x]]$order, out$E$order)
    }
  }
  out
}

# ---- recompute (the block's CORRECTIONS after the full run) -------------------------

# Recomputes every cell's metrics and the decision from the per-replicate
# records the full run stored, with this commit's cell_metrics() and
# decide(), and writes them back into the results file beside a note naming
# the commit. The replicates, the pilot and the AC5 fits are left as stored.
if (nzchar(Sys.getenv("M170_RECOMPUTE"))) {
  res_file <- file.path("devel", "m170-engine-estimate-results.rds")
  res <- readRDS(res_file)
  old <- res$decision
  res$metrics <- lapply(res$cells, function(cell) {
    cell_metrics(cell, cell$N, configs[[cell$cfg]])
  })
  res$decision <- decide(res$metrics, res$pilot$cpu)
  res$recomputed <- list(commit = RUN_COMMIT, git_status = RUN_STATUS,
                         date = Sys.Date(), decision_before = old)
  cat(sprintf("decision before: %s (branch %d); after: %s (branch %d)\n",
              old$option, old$branch, res$decision$option,
              res$decision$branch))
  saveRDS(res, res_file, compress = "xz")
  cat("saved:", res_file, "\n")
  quit(save = "no")
}

# ---- run -------------------------------------------------------------------------

t0 <- Sys.time()
cases <- real_cases()
real <- lapply(seq_along(cases), function(j) real_fit(cases[[j]], j))
names(real) <- names(cases)
for (nm in names(real)) {
  r <- real[[nm]]
  cat(sprintf("%-7s N %5d m %d accepted %s  T_E %.3f (df %d)  gap W %.3f  H %.3f\n",
              nm, r$N, r$m, r$accepted, r$E$T, r$E$df, r$gap[["W"]],
              r$gap[["H"]]))
}
if (real_only) {
  saveRDS(list(real = real, K = K, commit = RUN_COMMIT, git_status = RUN_STATUS,
               date = Sys.Date()),
          file.path("devel", "m170-engine-estimate-real.rds"))
  quit(save = "no")
}

pilot_file <- file.path("devel", "m170-engine-estimate-pilot.rds")
if (pilot || smoke) {
  REPS <- if (pilot) 10L else 4L
} else {
  pl <- readRDS(pilot_file)
  if (is.na(pl$R)) stop("the pilot's replicate rule gave no R: stop for the user")
  REPS <- pl$R
}
cat(sprintf("M170 engine estimate: reps=%d K=%d cores=%d %s%s\n", REPS, K,
            CORES, if (pilot) "[PILOT]" else "", if (smoke) "[SMOKE]" else ""))

cells <- list()
for (cfg_name in names(configs)) {
  truth_fit <- make_truth(configs[[cfg_name]])
  for (N in NS) {
    key <- sprintf("%s_N%d", cfg_name, N)
    cat(sprintf("[%s] %s ...\n", format(Sys.time(), "%H:%M:%S"), key))
    cells[[key]] <- run_cell(cfg_name, N, truth_fit, REPS)
    cells[[key]]$N <- N
    cells[[key]]$cfg <- cfg_name
  }
}
cat(sprintf("elapsed: %.1f min\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))

cpu_table <- function(cells) {
  do.call(rbind, lapply(names(cells), function(key) {
    acc <- Filter(function(r) !r$excluded, cells[[key]]$reps)
    cp <- do.call(rbind, lapply(acc, `[[`, "cpu"))
    data.frame(cell = key, E = stats::median(cp[, "E"]),
               W = stats::median(cp[, "W"]), H = stats::median(cp[, "H"]),
               replicate = stats::median(cp[, "replicate"]),
               family = stats::median(cp[, "family"], na.rm = TRUE))
  }))
}

if (pilot) {
  ct <- cpu_table(cells)
  print(ct, row.names = FALSE, digits = 3)
  proj <- function(R) 1.2 * sum(R * ct$replicate + FAMILY_WINDOW * ct$family)
  fits <- Filter(function(R) proj(R) <= BUDGET, seq(100, 500, by = 50))
  R <- if (length(fits)) max(fits) else NA_integer_
  cat(sprintf("replicate rule: R = %s (projected %.2f CPU-hours at R)\n", R,
              if (is.na(R)) proj(100) / 3600 else proj(R) / 3600))
  saveRDS(list(cpu = ct, R = R, K = K, budget = BUDGET, commit = RUN_COMMIT,
               git_status = RUN_STATUS, date = Sys.Date(),
               machine = Sys.info()[["machine"]],
               cores = parallel::detectCores()), pilot_file)
  quit(save = "no")
}

metrics <- lapply(cells, function(cell) {
  cell_metrics(cell, cell$N, configs[[cell$cfg]])
})
cpu <- if (smoke) cpu_table(cells) else pl$cpu
decision <- decide(metrics, cpu)
cat(sprintf("decision: %s (branch %d)\n", decision$option, decision$branch))

out_file <- file.path("devel", if (smoke) "m170-engine-estimate-smoke.rds" else
  "m170-engine-estimate-results.rds")
saveRDS(list(cells = cells, metrics = metrics, decision = decision, real = real,
             pilot = if (smoke) NULL else pl, reps = REPS, K = K,
             random = "2K around g0 at sd .7, 2K around g0 at sd 1.2",
             threshold = THRESH, subsample = SUBSAMPLE, configs = configs,
             ns = NS, seeds = c(data = SEED_DATA, h = SEED_H, real = SEED_REAL),
             commit = RUN_COMMIT, git_status = RUN_STATUS, date = Sys.Date(),
             machine = Sys.info()[["machine"]]),
        out_file, compress = "xz")
cat("saved:", out_file, "\n")
