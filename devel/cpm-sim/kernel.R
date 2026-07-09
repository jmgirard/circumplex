# CPM CI simulation study -- per-fit fit-and-score kernel (plan sec. 10.3).
#
# One replicate = simulate a dataset from the cell's population, fit the cell's
# model once (deterministic engine fit -> the reported point estimate), build
# every applicable interval from ONE shared bootstrap pass, and score coverage
# by the sec. 2.5 rules. Emits one per-fit record; the summarizer aggregates.
#
# Populations are simulated directly from the (Gaussian) population correlation
# P0 by a Cholesky draw -- this covers CPM-implied AND out-of-family P0 (which
# has no exact CPM fit for cpm_simulate to key on) uniformly, and keeps the
# whole study Gaussian (sec. 11). The RNG contract is the package invariant:
# set.seed(BASE_SEED + offset) locally per replicate (sec. 7.1).

# Simulate an N x p standardized Gaussian dataset with population correlation P0.
simulate_dataset <- function(P0, N) {
  p <- nrow(P0)
  L <- chol(P0)                      # P0 is PD (engine PD check / oof eigen-clip)
  matrix(stats::rnorm(N * p), N, p) %*% L
}

# Score one linear family (zeta or a set of kept beta harmonics) across methods.
# cis: named list of method -> list(lci, uci) (numeric vectors over indicators).
# Returns per-method per-indicator coverage + one-sided miss sides + widths.
score_linear <- function(truth, est, cis) {
  methods <- names(cis)
  ni <- length(truth)
  cover <- matrix(NA, length(methods), ni, dimnames = list(methods, NULL))
  miss  <- matrix(NA_character_, length(methods), ni, dimnames = list(methods, NULL))
  width <- matrix(NA_real_, length(methods), ni, dimnames = list(methods, NULL))
  trunc <- setNames(numeric(length(methods)), methods)   # exits [0,1]/simplex
  for (mth in methods) {
    lci <- cis[[mth]]$lci; uci <- cis[[mth]]$uci
    cover[mth, ] <- lin_covered(lci, uci, truth)
    miss[mth, ]  <- lin_miss_side(lci, uci, truth)
    width[mth, ] <- uci - lci
    # truncation geometry (basic/studentized raw intervals, sec. 4.1/4.4):
    # fraction of indicators whose raw interval exits the natural [0,1] bound.
    trunc[mth] <- mean((lci < 0 | uci > 1), na.rm = TRUE)
  }
  list(cover = cover, miss = miss, width = width, trunc = trunc)
}

# fit_and_score: the kernel. `cell` is a resolved config cell; `i` the replicate
# index; `params` = list(boots, jack_g, studentized (logical), levels). Returns
# one record list, or NULL on a fit error (counted upstream).
fit_and_score <- function(cell, i, params) {
  stopifnot(i <= SEED_MAX_I)                    # range guard (review M1)
  offset <- SEED_MULT * cell$cell_index + i     # unique per (cell, replicate)
  set.seed(BASE_SEED + offset)
  X <- simulate_dataset(cell$P0, cell$N)
  R <- suppressWarnings(stats::cor(X))
  if (anyNA(R)) return(NULL)                    # degenerate sample (counted)

  eng <- tryCatch(suppressWarnings(
    cpm_engine(R, angles = cell$fit_angles, m = cell$m_fit,
               variant = cell$variant_fit, reference = cell$reference)),
    error = function(e) NULL)
  if (is.null(eng)) return(NULL)

  spec <- eng$spec
  p <- spec$p
  z_primary <- stats::qnorm(1 - (1 - PRIMARY_LEVEL) / 2)
  free_pos <- spec$free_pos                     # estimated angle positions
  keep_k <- spec$keep_k                         # kept harmonics (0-based)
  removed_k <- setdiff(0:spec$m, keep_k)
  th_truth <- cell$truth$theta                  # deg, length p
  z_truth  <- cell$truth$zeta                   # length p
  b_truth  <- cell$truth$beta                   # length m_fit + 1

  # analytic (Wald) SEs from the same optimum (deterministic)
  se <- tryCatch(suppressWarnings(cpm_analytic_se(eng, R, cell$N)),
                 error = function(e) NULL)
  se_na <- is.null(se) || anyNA(c(se$zeta[seq_len(p)], se$beta[keep_k + 1L],
                                  if (length(free_pos)) se$angle[free_pos]))

  # marker verdict (the shipped cpm_boundary_markers on the reported fit)
  mobj <- list(
    details = list(heywood = eng$heywood, removed_harmonics = eng$removed_harmonics,
                   hessian_condition = eng$hessian_condition,
                   multimodal = eng$multimodal),
    betas = list(Beta = eng$beta))
  markers <- cpm_boundary_markers(mobj)

  Tstat <- (cell$N - 1L) * eng$F                # package n = N - 1 convention
  boot <- isTRUE(cell$bootstrap)

  # ---- build the shared bootstrap pass (bootstrap cells only) ---------------
  reps <- NULL; jack <- NULL; jack_full <- NULL
  if (boot) {
    reps <- sim_replicates(eng, X, params$boots, with_se = isTRUE(params$studentized))
    jack <- grouped_jackknife(eng, X, g = params$jack_g)
    # stage-3a validation: also compute the full delete-1 jackknife so the
    # grouped acceleration can be gated against it (sec. 4.3; review M6).
    if (isTRUE(cell$flags$jack_validate))
      jack_full <- grouped_jackknife(eng, X, g = cell$N)
  }

  # ---- score theta (percentile + Wald), free angles only --------------------
  theta_cov <- list(); theta_miss <- list(); theta_ep <- list()
  theta_width <- NULL
  if (length(free_pos)) {
    tt <- th_truth[free_pos] %% 360
    if (boot && reps$boots_used > 0) {
      pth <- ci_percentile_theta(reps$theta_rad, reps$ok, PRIMARY_LEVEL)
      lo <- pth$lci[free_pos]; hi <- pth$uci[free_pos]
      cov_p <- angle_covered(lo, hi, tt)
      theta_cov$percentile <- cov_p
      theta_miss$percentile <- angle_miss_side(lo, hi, tt)
      theta_ep$percentile <- list(lci = lo, uci = hi)
      theta_width <- (hi - lo) %% 360           # angular width (sec. 5.2)
    }
    if (!se_na) {
      est_free <- eng$theta[free_pos]
      cov_w <- abs(ang_signed(est_free, tt)) <= z_primary * se$angle[free_pos]
      theta_cov$wald <- cov_w
      theta_ep$wald <- list(est = est_free, se = se$angle[free_pos])
      # miss side names where the TRUTH lies vs the estimate (review M5): truth
      # clockwise of the estimate (tt - est < 0, i.e. ang_signed(est, tt) < 0)
      # is the "lower" side, matching lin_miss_side / angle_miss_side.
      theta_miss$wald <- ifelse(cov_w, NA_character_,
        ifelse(ang_signed(est_free, tt) < 0, "lower", "upper"))
    }
  }

  # ---- score zeta -----------------------------------------------------------
  zeta_cis <- list(); student_na_zeta <- NA_real_
  if (!se_na) zeta_cis$wald <- ci_wald_linear(eng$zeta, se$zeta, PRIMARY_LEVEL)
  if (boot && reps$boots_used >= 2) {
    zeta_cis$percentile <- ci_percentile_linear(reps$zeta, reps$ok, PRIMARY_LEVEL)
    zeta_cis$basic <- ci_basic_linear(eng$zeta, reps$zeta, reps$ok, PRIMARY_LEVEL)
    bca <- ci_bca_linear(eng$zeta, reps$zeta, reps$ok, jack$a_zeta, PRIMARY_LEVEL)
    zeta_cis$bca <- list(lci = vapply(bca, `[[`, 0, "lci"),
                         uci = vapply(bca, `[[`, 0, "uci"))
    if (isTRUE(params$studentized)) {
      st <- lapply(seq_len(p), function(j) studentized_one(
        eng$zeta[j], if (!se_na) se$zeta[j] else NA_real_,
        reps$zeta[reps$ok, j], reps$se_zeta[reps$ok, j], PRIMARY_LEVEL))
      zeta_cis$studentized <- list(lci = vapply(st, `[[`, 0, "lci"),
                                   uci = vapply(st, `[[`, 0, "uci"))
      student_na_zeta <- mean(vapply(st, `[[`, 0, "na_rate"), na.rm = TRUE)
    }
  }
  zeta_sc <- if (length(zeta_cis)) score_linear(z_truth, eng$zeta, zeta_cis) else NULL

  # ---- score beta: kept harmonics get method intervals; removed harmonics are
  # scored ONCE (sec. 2.5), covering iff the truth is exactly 0, attributed
  # identically to every bootstrap-family method and excluded from contrasts.
  kept_idx <- keep_k + 1L
  beta_cis <- list(); student_na_beta <- NA_real_
  if (!se_na) beta_cis$wald <- ci_wald_linear(eng$beta[kept_idx], se$beta[kept_idx],
                                              PRIMARY_LEVEL)
  if (boot && reps$boots_used >= 2) {
    rk <- reps$beta[, kept_idx, drop = FALSE]
    beta_cis$percentile <- ci_percentile_linear(rk, reps$ok, PRIMARY_LEVEL)
    beta_cis$basic <- ci_basic_linear(eng$beta[kept_idx], rk, reps$ok, PRIMARY_LEVEL)
    bcab <- ci_bca_linear(eng$beta[kept_idx], rk, reps$ok, jack$a_beta[kept_idx],
                          PRIMARY_LEVEL)
    beta_cis$bca <- list(lci = vapply(bcab, `[[`, 0, "lci"),
                         uci = vapply(bcab, `[[`, 0, "uci"))
    if (isTRUE(params$studentized)) {
      stb <- lapply(seq_along(kept_idx), function(j) {
        col <- kept_idx[j]
        studentized_one(eng$beta[col], if (!se_na) se$beta[col] else NA_real_,
          reps$beta[reps$ok, col], reps$se_beta[reps$ok, col], PRIMARY_LEVEL)
      })
      beta_cis$studentized <- list(lci = vapply(stb, `[[`, 0, "lci"),
                                   uci = vapply(stb, `[[`, 0, "uci"))
      student_na_beta <- mean(vapply(stb, `[[`, 0, "na_rate"), na.rm = TRUE)
    }
  }
  beta_sc <- if (length(beta_cis)) score_linear(b_truth[kept_idx], eng$beta[kept_idx],
                                                beta_cis) else NULL
  # removed-harmonic single score (sec. 2.5): cover iff truth beta_k == 0; a
  # miss is attributed to the side of the truth vs the degenerate [0,0] interval
  # (truth > 0 -> upper), for the one-sided decomposition (sec. 5.1 / review S3).
  removed_truth <- if (length(removed_k)) b_truth[removed_k + 1L] else numeric(0)
  removed_cover <- removed_truth == 0
  removed_miss <- ifelse(removed_cover, NA_character_,
                         ifelse(removed_truth > 0, "upper", "lower"))

  # ---- BCa accounting (sec. 4.2/5.2) ----------------------------------------
  bca_acct <- NULL
  if (boot && reps$boots_used >= 2) {
    all_bca <- c(bca, bcab)
    bca_acct <- list(
      saturated = mean(vapply(all_bca, function(x) isTRUE(x$saturated), NA)),
      clamped = mean(vapply(all_bca, function(x)
        isTRUE(x$clamped_lo) || isTRUE(x$clamped_hi), NA)),
      na = mean(vapply(all_bca, function(x) isTRUE(x$na), NA)),
      g_used = jack$g_used, g_fail = jack$g_fail)
  }

  # ---- coverage proportions across ALL levels (secondary 90/99 reuse the same
  # replicate set; Wald/percentile/basic/BCa recomputed per level) ------------
  level_table <- NULL
  if (length(params$levels) > 1) {
    level_table <- score_levels(eng, se, reps, jack, cell, params, se_na,
                                free_pos, keep_k, th_truth, z_truth, b_truth,
                                removed_cover)
  }

  list(
    meta = list(id = cell$id, cell_index = cell$cell_index, N = cell$N,
                stage = cell$stage, arm = cell$arm, spec_note = cell$spec_note,
                angle_set = cell$angle_set, equal_spaced = cell$equal_spaced,
                boundary_status = cell$boundary_status, bootstrap = boot),
    status = list(
      accepted = isTRUE(eng$accepted), polish = length(eng$removed_harmonics) > 0,
      n_removed = length(eng$removed_harmonics), heywood = isTRUE(eng$heywood),
      multimodal = isTRUE(eng$multimodal), hessian_condition = eng$hessian_condition,
      markers = markers, se_na = se_na,
      boots_used = if (boot) reps$boots_used else NA_integer_,
      boots_degenerate = if (boot) reps$boots_degenerate else NA_integer_,
      boots_nonconvergent = if (boot) reps$boots_nonconvergent else NA_integer_,
      student_na_rate = mean(c(student_na_zeta, student_na_beta), na.rm = TRUE),
      Tstat = Tstat, df = eng$df),
    # point estimates (sec. 5.3 estimator behavior) + primary-level interval
    # endpoints by method (sec. 10.3 record schema; review M4)
    estimates = list(theta = eng$theta, zeta = eng$zeta, beta = eng$beta,
                     keep_k = keep_k, removed_k = removed_k, free_pos = free_pos),
    endpoints = list(theta = theta_ep, zeta = zeta_cis, beta = beta_cis),
    geometry = list(theta_width = theta_width,
                    zeta_width = if (!is.null(zeta_sc)) zeta_sc$width else NULL,
                    beta_width = if (!is.null(beta_sc)) beta_sc$width else NULL,
                    zeta_trunc = if (!is.null(zeta_sc)) zeta_sc$trunc else NULL,
                    beta_trunc = if (!is.null(beta_sc)) beta_sc$trunc else NULL),
    theta = list(cover = theta_cov, miss = theta_miss, n_free = length(free_pos)),
    zeta  = zeta_sc, beta = beta_sc,
    beta_removed = list(k = removed_k, cover = removed_cover, miss = removed_miss),
    bca_acct = bca_acct, level_table = level_table,
    jack_validate = if (!is.null(jack_full)) list(
      grouped = list(a_zeta = jack$a_zeta, a_beta = jack$a_beta,
                     g_used = jack$g_used),
      full = list(a_zeta = jack_full$a_zeta, a_beta = jack_full$a_beta,
                  g_used = jack_full$g_used)) else NULL,
    heywood_zeta = eng$zeta                       # for the RQ3 pile-up companion
  )
}

# Secondary-level coverage proportions (90/99): the plan takes these as extra
# quantiles over the same replicate set (sec. 3.2). Returns a data frame of
# (family, method, level, prop, n) rows -- proportions only (per-indicator
# vectors are kept at the primary level for pairing/contrasts).
score_levels <- function(eng, se, reps, jack, cell, params, se_na,
                         free_pos, keep_k, th_truth, z_truth, b_truth,
                         removed_cover = logical(0)) {
  spec <- eng$spec; p <- spec$p; kept_idx <- keep_k + 1L
  boot <- isTRUE(cell$bootstrap) && !is.null(reps) && reps$boots_used >= 2
  rows <- list()
  # beta folds in the level-independent removed-harmonic single score (sec. 2.5)
  # at every level, matching the primary-level fold (review S4).
  push <- function(family, method, level, cover_vec) {
    if (family == "beta") cover_vec <- c(cover_vec, removed_cover)
    ok <- !is.na(cover_vec)
    rows[[length(rows) + 1L]] <<- data.frame(
      family = family, method = method, level = level,
      prop = if (any(ok)) mean(cover_vec[ok]) else NA_real_, n = sum(ok),
      stringsAsFactors = FALSE)
  }
  for (L in params$levels) {
    if (identical(L, PRIMARY_LEVEL)) next        # primary handled elsewhere
    zL <- stats::qnorm(1 - (1 - L) / 2)
    # theta
    if (length(free_pos)) {
      tt <- th_truth[free_pos] %% 360
      if (boot) {
        pth <- ci_percentile_theta(reps$theta_rad, reps$ok, L)
        push("theta", "percentile", L,
             angle_covered(pth$lci[free_pos], pth$uci[free_pos], tt))
      }
      if (!se_na) push("theta", "wald", L,
        abs(ang_signed(eng$theta[free_pos], tt)) <= zL * se$angle[free_pos])
    }
    # zeta
    if (!se_na) { w <- ci_wald_linear(eng$zeta, se$zeta, L)
      push("zeta", "wald", L, lin_covered(w$lci, w$uci, z_truth)) }
    if (boot) {
      pc <- ci_percentile_linear(reps$zeta, reps$ok, L)
      push("zeta", "percentile", L, lin_covered(pc$lci, pc$uci, z_truth))
      bc <- ci_basic_linear(eng$zeta, reps$zeta, reps$ok, L)
      push("zeta", "basic", L, lin_covered(bc$lci, bc$uci, z_truth))
      bca <- ci_bca_linear(eng$zeta, reps$zeta, reps$ok, jack$a_zeta, L)
      push("zeta", "bca", L, lin_covered(vapply(bca, `[[`, 0, "lci"),
                                        vapply(bca, `[[`, 0, "uci"), z_truth))
    }
    # beta (kept)
    bt <- b_truth[kept_idx]
    if (!se_na) { w <- ci_wald_linear(eng$beta[kept_idx], se$beta[kept_idx], L)
      push("beta", "wald", L, lin_covered(w$lci, w$uci, bt)) }
    if (boot) {
      rk <- reps$beta[, kept_idx, drop = FALSE]
      pc <- ci_percentile_linear(rk, reps$ok, L)
      push("beta", "percentile", L, lin_covered(pc$lci, pc$uci, bt))
      bc <- ci_basic_linear(eng$beta[kept_idx], rk, reps$ok, L)
      push("beta", "basic", L, lin_covered(bc$lci, bc$uci, bt))
      bca <- ci_bca_linear(eng$beta[kept_idx], rk, reps$ok, jack$a_beta[kept_idx], L)
      push("beta", "bca", L, lin_covered(vapply(bca, `[[`, 0, "lci"),
                                        vapply(bca, `[[`, 0, "uci"), bt))
    }
  }
  if (length(rows)) do.call(rbind, rows) else NULL
}
