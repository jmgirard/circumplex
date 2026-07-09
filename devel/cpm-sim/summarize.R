# CPM CI simulation study -- cluster-level summarization + decision rules
# (plan sec. 6, sec. 10.4).
#
# Every MC inference interval is CLUSTER-LEVEL: each fitted dataset contributes
# ONE per-fit coverage proportion, and the interval is a normal-theory t
# interval on the mean of those proportions (sec. 6.2). No pooled-indicator
# binomial interval is used anywhere (the B6 tables' Wilson intervals were naive
# and anti-conservative; this study upgrades the rule). Verdicts apply the
# Bradley bands from common.R (sec. 6.1). Paired method contrasts are per-fit
# differences over KEPT parameters only (sec. 2.5/6.2).

# ---- per-fit coverage proportions -------------------------------------------
# Extract, for one record, the per-fit coverage proportion for (family, method)
# at the primary level. beta folds in the removed-harmonic single score, which
# is attributed identically to every method (sec. 2.5).
fit_prop <- function(rec, family, method) {
  if (family == "theta") {
    cv <- rec$theta$cover[[method]]
    if (is.null(cv)) return(NA_real_)
    return(if (any(!is.na(cv))) mean(cv[!is.na(cv)]) else NA_real_)
  }
  sc <- rec[[family]]
  # A method that scored NOTHING in this fit (no row in the cover matrix) has no
  # coverage -- return NA rather than fabricating one from the removed-harmonic
  # score alone (review M3). The sec. 2.5 fold applies only when the method
  # actually produced kept-parameter scores.
  if (is.null(sc) || !(method %in% rownames(sc$cover))) return(NA_real_)
  cv <- sc$cover[method, ]
  if (family == "beta") cv <- c(cv, rec$beta_removed$cover)   # level-independent
  if (!any(!is.na(cv))) return(NA_real_)
  mean(cv[!is.na(cv)])
}

# per-fit one-sided miss proportions (lower/upper) for a linear family + method.
# Denominator = indicators with a DEFINED interval (non-NA coverage), matching
# fit_prop; beta folds in the removed-harmonic single score's side (review S3).
fit_miss_prop <- function(rec, family, method, side) {
  if (family == "theta") {
    cov <- rec$theta$cover[[method]]
    ms  <- rec$theta$miss[[method]]
    if (is.null(cov)) return(NA_real_)
  } else {
    sc <- rec[[family]]
    if (is.null(sc) || !(method %in% rownames(sc$cover))) return(NA_real_)
    cov <- sc$cover[method, ]
    ms  <- sc$miss[method, ]
    if (family == "beta") {
      cov <- c(cov, rec$beta_removed$cover)
      ms  <- c(ms, rec$beta_removed$miss)
    }
  }
  scored <- sum(!is.na(cov))
  if (scored == 0) return(NA_real_)
  sum(ms == side, na.rm = TRUE) / scored
}

# ---- cluster interval + Bradley verdict -------------------------------------
cluster_ci <- function(props, conf = 0.95) {
  x <- props[is.finite(props)]
  n <- length(x)
  if (n == 0) return(c(est = NA, lci = NA, uci = NA, n = 0))
  m <- mean(x)
  se <- if (n > 1) stats::sd(x) / sqrt(n) else NA_real_
  tcrit <- if (n > 1) stats::qt(1 - (1 - conf) / 2, n - 1) else NA_real_
  c(est = m, lci = m - tcrit * se, uci = m + tcrit * se, n = n)
}

# adequate / non-nominal / borderline against a two-sided band (sec. 6.1).
bradley_verdict <- function(ci, band) {
  if (is.na(ci["lci"]) || is.na(ci["uci"])) return("undetermined")
  if (ci["lci"] >= band["lower"] && ci["uci"] <= band["upper"]) return("adequate")
  if (ci["uci"] < band["lower"] || ci["lci"] > band["upper"]) return("non-nominal")
  "borderline"
}

# ---- paired method contrast (sec. 6.2) --------------------------------------
# Per fit: mean per-indicator coverage difference (m1 - m2) over indicators both
# methods score (kept parameters; removed excluded). Cluster-robust t interval.
contrast_ci <- function(records, family, m1, m2, conf = 0.95) {
  diffs <- vapply(records, function(rec) {
    sc <- rec[[family]]
    if (is.null(sc) || !all(c(m1, m2) %in% rownames(sc$cover))) return(NA_real_)
    d <- sc$cover[m1, ] - sc$cover[m2, ]
    d <- d[!is.na(d)]
    if (!length(d)) return(NA_real_)
    mean(d)
  }, numeric(1))
  ci <- cluster_ci(diffs, conf)
  data.frame(family = family, m1 = m1, m2 = m2,
             diff = ci["est"], lci = ci["lci"], uci = ci["uci"], n = ci["n"],
             row.names = NULL)
}

# ---- per-cell summary -------------------------------------------------------
# records: accepted-or-not per-fit records for ONE cell (a list). n_error passed
# from the driver. Conditions on acceptance for the primary claim; also reports
# the worst-case bound (non-accepted scored as misses) for headline families.
summarize_cell <- function(cell, records, n_error = 0L,
                          methods = list(theta = c("percentile", "wald"),
                                         zeta = c("percentile", "basic", "bca",
                                                  "wald", "studentized"),
                                         beta = c("percentile", "basic", "bca",
                                                  "wald", "studentized"))) {
  acc <- vapply(records, function(r) isTRUE(r$status$accepted), logical(1))
  n_total <- length(records) + n_error
  ar <- records[acc]                                   # accepted subset
  n_acc <- length(ar)
  rate <- function(f) if (n_acc) mean(vapply(ar, f, logical(1))) else NA_real_

  band <- lapply(LEVELS, bradley_band); names(band) <- as.character(LEVELS)

  # studentized feasibility (sec. 4.4): a cell whose per-replicate NA-SE rate
  # exceeds 20% reports the method infeasible rather than its coverage (S5).
  student_na <- if (n_acc)
    mean(vapply(ar, function(r) r$status$student_na_rate, numeric(1)),
         na.rm = TRUE) else NA_real_
  student_infeasible <- isTRUE(student_na > 0.20)

  # coverage table (primary level): cluster CI + verdict per family x method
  cov_rows <- list()
  for (fam in names(methods)) for (mth in methods[[fam]]) {
    props <- vapply(ar, fit_prop, numeric(1), family = fam, method = mth)
    if (all(is.na(props))) next                        # method not applicable
    ci <- cluster_ci(props)
    verdict <- bradley_verdict(ci, bradley_band(PRIMARY_LEVEL))
    if (mth == "studentized" && student_infeasible)
      verdict <- "infeasible (NA-SE rate > 20%)"
    cov_rows[[length(cov_rows) + 1L]] <- data.frame(
      family = fam, method = mth, level = PRIMARY_LEVEL,
      est = ci["est"], lci = ci["lci"], uci = ci["uci"], n = ci["n"],
      verdict = verdict, row.names = NULL, stringsAsFactors = FALSE)
  }
  cov_primary <- if (length(cov_rows)) do.call(rbind, cov_rows) else NULL

  # worst-case bound (sec. 6.3.3): non-accepted fits (and worker errors) scored
  # as coverage 0, for the headline families x bootstrap-default + Wald (S7).
  wc_rows <- list()
  for (fam in c("theta", "zeta", "beta")) for (mth in c("percentile", "wald")) {
    props <- c(vapply(records, function(r) if (isTRUE(r$status$accepted))
      fit_prop(r, fam, mth) else 0, numeric(1)), rep(0, n_error))
    if (all(is.na(props))) next
    ci <- cluster_ci(props)
    wc_rows[[length(wc_rows) + 1L]] <- data.frame(
      family = fam, method = mth, est = ci["est"], lci = ci["lci"],
      uci = ci["uci"], n = ci["n"],
      verdict = bradley_verdict(ci, bradley_band(PRIMARY_LEVEL)),
      row.names = NULL, stringsAsFactors = FALSE)
  }
  cov_worstcase <- if (length(wc_rows)) do.call(rbind, wc_rows) else NULL

  # secondary levels from the per-fit level tables (kept-family proportions)
  cov_secondary <- NULL
  lt <- lapply(ar, `[[`, "level_table")
  lt <- lt[!vapply(lt, is.null, logical(1))]
  if (length(lt)) {
    all_lt <- do.call(rbind, Map(function(tab, k) cbind(tab, .fit = k),
                                 lt, seq_along(lt)))
    keys <- unique(all_lt[c("family", "method", "level")])
    rows <- lapply(seq_len(nrow(keys)), function(i) {
      k <- keys[i, ]
      sub <- all_lt[all_lt$family == k$family & all_lt$method == k$method &
                      all_lt$level == k$level, ]
      ci <- cluster_ci(sub$prop)
      data.frame(k, est = ci["est"], lci = ci["lci"], uci = ci["uci"],
                 n = ci["n"],
                 verdict = bradley_verdict(ci, bradley_band(k$level)),
                 row.names = NULL)
    })
    cov_secondary <- do.call(rbind, rows)
  }

  # one-sided decomposition (zeta directional story, theta too) at primary level
  side_rows <- list()
  for (fam in c("theta", "zeta", "beta")) for (mth in methods[[fam]]) {
    for (sd in c("lower", "upper")) {
      props <- vapply(ar, fit_miss_prop, numeric(1), family = fam,
                      method = mth, side = sd)
      if (all(is.na(props))) next
      ci <- cluster_ci(props)
      side_rows[[length(side_rows) + 1L]] <- data.frame(
        family = fam, method = mth, side = sd,
        est = ci["est"], lci = ci["lci"], uci = ci["uci"], n = ci["n"],
        verdict = bradley_verdict(ci, bradley_side_band(PRIMARY_LEVEL)),
        row.names = NULL, stringsAsFactors = FALSE)
    }
  }
  onesided <- if (length(side_rows)) do.call(rbind, side_rows) else NULL

  # paired contrasts vs the shipped percentile default (zeta/beta only)
  contrasts <- NULL
  if (isTRUE(cell$bootstrap) && n_acc > 0) {
    ct <- list()
    for (fam in c("zeta", "beta")) for (mth in c("basic", "bca", "wald")) {
      ct[[length(ct) + 1L]] <- contrast_ci(ar, fam, "percentile", mth)
    }
    contrasts <- do.call(rbind, ct)
  }

  # trustworthiness surface (sec. 5.4)
  marker_rate <- function(mk) if (n_acc)
    mean(vapply(ar, function(r) mk %in% r$status$markers, logical(1))) else NA_real_
  markers_all <- c("Heywood communality", "boundary harmonic removed",
                   "small correlation-function weight", "ill-conditioned Hessian",
                   "competing near-tied optima")
  na_ci_rate <- if (n_acc) mean(vapply(ar, function(r) isTRUE(r$status$se_na),
                                       logical(1))) else NA_real_

  # T-calibration (sec. 5.4): KS vs chi-square(df) on unpolished accepted fits in
  # correctly specified cells; descriptive noncentral otherwise (F* stored).
  ks_T <- NA_real_
  if (cell$spec_note %in% c("correct", "correct-fixed") && n_acc > 0) {
    keep <- !vapply(ar, function(r) isTRUE(r$status$polish), logical(1))
    Ts <- vapply(ar[keep], function(r) r$status$Tstat, numeric(1))
    df <- if (length(Ts)) ar[keep][[1]]$status$df else NA
    if (length(Ts) >= 30) ks_T <- suppressWarnings(
      stats::ks.test(Ts, stats::pchisq, df = df)$p.value)
  }

  # BCa accounting + Heywood pile-up mass (RQ3 companion)
  bca <- if (isTRUE(cell$bootstrap) && n_acc > 0) {
    accts <- lapply(ar, `[[`, "bca_acct"); accts <- accts[!vapply(accts, is.null, logical(1))]
    if (length(accts)) list(
      saturated = mean(vapply(accts, `[[`, 0, "saturated"), na.rm = TRUE),
      clamped = mean(vapply(accts, `[[`, 0, "clamped"), na.rm = TRUE),
      na = mean(vapply(accts, `[[`, 0, "na"), na.rm = TRUE),
      g_used = mean(vapply(accts, `[[`, 0, "g_used"))) else NULL
  } else NULL
  heywood_pileup <- if (n_acc) mean(vapply(ar, function(r)
    any(r$heywood_zeta > 0.995), logical(1))) else NA_real_
  # per-item pile-up mass P(zeta_i > .995) (sec. 5.3)
  heywood_pileup_item <- if (n_acc)
    colMeans(do.call(rbind, lapply(ar, function(r) r$heywood_zeta > 0.995))) else NA

  # interval geometry (sec. 5.2): median width + truncation rate per method (S7)
  # method sets vary across fits (se_na fits lack Wald, only armed cells have
  # studentized), so aggregate over the UNION of methods with membership guards.
  med_width <- function(field) {
    mats <- lapply(ar, function(r) r$geometry[[field]])
    mats <- mats[!vapply(mats, is.null, logical(1))]
    if (!length(mats)) return(NULL)
    meths <- unique(unlist(lapply(mats, rownames)))
    vapply(meths, function(m) stats::median(unlist(lapply(mats,
      function(M) if (m %in% rownames(M)) M[m, ] else NULL)), na.rm = TRUE),
      numeric(1))
  }
  mean_trunc <- function(field) {
    v <- lapply(ar, function(r) r$geometry[[field]])
    v <- v[!vapply(v, is.null, logical(1))]
    if (!length(v)) return(NULL)
    meths <- unique(unlist(lapply(v, names)))
    vapply(meths, function(m) mean(vapply(v, function(x)
      if (m %in% names(x)) x[[m]] else NA_real_, 0), na.rm = TRUE), numeric(1))
  }
  geometry <- if (n_acc) list(
    theta_width_median = stats::median(unlist(lapply(ar,
      function(r) r$geometry$theta_width)), na.rm = TRUE),
    zeta_width_median = med_width("zeta_width"),
    beta_width_median = med_width("beta_width"),
    zeta_trunc_rate = mean_trunc("zeta_trunc"),
    beta_trunc_rate = mean_trunc("beta_trunc")) else NULL

  # RQ4 conditional coverage given each marker (+ any-marker), Wald family (S7)
  marker_conditional <- if (n_acc) do.call(rbind, lapply(
    c(markers_all, "any-marker"), function(mk) {
      fired <- vapply(ar, function(r) if (mk == "any-marker")
        length(r$status$markers) > 0 else mk %in% r$status$markers, logical(1))
      do.call(rbind, lapply(c(TRUE, FALSE), function(state) {
        sub <- ar[fired == state]
        do.call(rbind, lapply(c("theta", "zeta", "beta"), function(fam) {
          ci <- cluster_ci(vapply(sub, fit_prop, numeric(1),
                                  family = fam, method = "wald"))
          data.frame(marker = mk, fired = state, family = fam,
                     est = ci["est"], lci = ci["lci"], uci = ci["uci"],
                     n = ci["n"], row.names = NULL)
        }))
      }))
    })) else NULL

  list(
    id = cell$id, N = cell$N, stage = cell$stage, arm = cell$arm,
    spec_note = cell$spec_note, angle_set = cell$angle_set,
    equal_spaced = cell$equal_spaced, boundary_status = cell$boundary_status,
    rmsea_pop = cell$rmsea_pop, Fstar = cell$Fstar,
    n_total = n_total, n_accepted = n_acc, n_error = n_error,
    error_flag = n_error / max(1L, n_total) > 0.02,      # sec. 6.3.2 (S7)
    acceptance_rate = n_acc / max(1L, length(records)),
    heywood_rate = rate(function(r) isTRUE(r$status$heywood)),
    polish_rate = rate(function(r) isTRUE(r$status$polish)),
    multimodal_rate = rate(function(r) isTRUE(r$status$multimodal)),
    marker_rates = setNames(vapply(markers_all, marker_rate, 0), markers_all),
    na_ci_rate = na_ci_rate, heywood_pileup = heywood_pileup,
    heywood_pileup_item = heywood_pileup_item,
    student_na_rate = student_na, student_infeasible = student_infeasible,
    ks_T = ks_T, bca = bca, geometry = geometry,
    cov_primary = cov_primary, cov_secondary = cov_secondary,
    cov_worstcase = cov_worstcase, marker_conditional = marker_conditional,
    onesided = onesided, contrasts = contrasts)
}

# ---- region aggregation (sec. 6.1) ------------------------------------------
# A named region is a contiguous cell set declared BEFORE stage 1 runs. Given
# the per-cell primary-level verdicts for one (family, method), the region is
# adequate iff >= 95% of cells are adequate and none is non-nominal; inadequate
# iff >= 95% non-nominal; else a described mixed surface (sec. 6.1). Errs
# conservative (fails-to-claim, never false-claims). The expected false-flag
# count under the global null is printed beside every region claim by the
# caller; this returns the verdict + the tallies to compute it.
region_verdict <- function(verdicts) {
  v <- verdicts[verdicts != "undetermined"]
  n <- length(v)
  if (n == 0) return(list(verdict = "undetermined", n = 0))
  p_adeq <- mean(v == "adequate"); p_non <- mean(v == "non-nominal")
  verdict <- if (p_adeq >= 0.95 && p_non == 0) "adequate" else
    if (p_non >= 0.95) "inadequate" else "mixed"
  list(verdict = verdict, n = n, n_adequate = sum(v == "adequate"),
       n_nonnominal = sum(v == "non-nominal"), n_borderline = sum(v == "borderline"))
}
