# SSM CI-trustworthiness diagnostic (Zimmermann & Wright, 2017) ----------------
# Implements the core simulation loop of devel/m4-ci-accuracy-spec.md (M4/Z1):
# one cpm_fit() on the pooled within-group scale correlations defines a plug-in
# population; `reps` simulated datasets at the user's exact n replay the
# object's own CI procedure (same engine, boots, and interval); coverage of the
# closed-form population truths is tallied per parameter and amplitude-ladder
# condition. The sec. 4-5 analysis layer (miss-decomposition reporting,
# guardrail false-certification measurement, verdict wording, plot method)
# lands with M4/Z2.

#' Assess the accuracy of SSM confidence intervals by simulation
#'
#' Estimate, by simulation, whether the confidence intervals of a fitted
#' [ssm_analyze()] object would cover the true SSM parameters at their nominal
#' rate if the population looked like the fitted estimates, at the observed
#' sample size(s). Following Zimmermann & Wright (2017), the population's scale
#' intercorrelation structure is characterized by fitting Browne's (1992)
#' circular process model ([cpm_fit()]) to the pooled within-group scale
#' correlations; each of `reps` datasets is then simulated from that plug-in
#' population at the object's exact group sizes and the object's own interval
#' procedure (same engine, same `boots`, same `interval`) is rerun on it.
#' Coverage is reported per profile row, parameter, and amplitude condition,
#' with 95% Wilson score intervals classified against Bradley's (1978) liberal
#' robustness band at the as-estimated condition.
#'
#' Displacement coverage is angular: the truth is inside the reported interval
#' as an arc (membership modulo 360 degrees), so populations peaking at the
#' 0/360 boundary and contrast intervals reported beyond +/-180 degrees are
#' handled without special-casing. Because displacement is only interpreted
#' when the printed amplitude guardrail certifies it, displacement coverage is
#' also reported conditional on certification under the shipped decision rule
#' `a_lci / (a_uci - a_lci) >= 0.35` (the rule the printed [ssm_analyze()]
#' output applies): the amplitude CI's lower bound must sit at least 0.35 CI
#' widths above zero. The rule is scale-free (invariant to the score metric)
#' and print-independent, so no scale-dependent threshold is reported; the
#' 0.35 constant is calibrated for the default 95% confidence interval. A
#' contrast row is a
#' signed difference, not a prototypicality measure, so
#' `print.circumplex_ssm()` never certification-gates it; its displacement
#' verdict and printed coverage are therefore reported unconditionally
#' (matching that profiles-only stance). Its certification-conditional
#' coverage -- where "certified" means both profile rows were certified -- is
#' still computed and retained in the returned object as a descriptive that no
#' display consumes.
#'
#' The `amplitude_factors` ladder manufactures populations whose closed-form
#' amplitude is scaled toward zero (the regime where percentile amplitude
#' intervals are theoretically weakest) while keeping the residual profile
#' content fixed. The ladder is defined through the estimator functional (a
#' 3x3 solve on the images of 1, cos, and sin), so the condition-`c` truths are
#' exact for any angle spacing: elevation is unchanged and the amplitude is
#' exactly `c` times the estimate. Truths are nevertheless recomputed from
#' each condition's population profile. At `c = 0` amplitude coverage is
#' structurally zero (a percentile interval of strictly positive amplitude
#' replicates cannot contain 0; such rows are flagged in the `Structural`
#' column) and displacement truth is undefined (reported `NA`); the guardrail
#' certification rate carries the inferential weight there, and the
#' informative rungs for amplitude coverage are the small `c > 0` ones.
#'
#' When a profile row's amplitude estimate is itself below half its observed
#' CI width, the relative ladder degenerates: the analysis already sits in
#' the near-zero regime. One absolute rung is then added at the certification
#' margin (`c` chosen so `c` times the amplitude estimate equals the observed
#' amplitude-CI half-width, the largest such `c` across affected rows) and
#' `summary()` notes the regime. On the correlation path this rung is dropped
#' with a warning if it would push a population correlation to +/-1.
#'
#' @param ssm_object Required. A `circumplex_ssm` object from [ssm_analyze()].
#' @param reps Optional. The number of simulated datasets per amplitude
#'   condition (default = 1000, binomial SE about 0.7 percentage points;
#'   500 is a reasonable quick-look floor).
#' @param amplitude_factors Optional. Numeric vector of amplitude scaling
#'   factors in `[0, 1]` defining the ladder conditions (default =
#'   `c(1, 0.5, 0.25, 0)`). Must include 1, the as-estimated condition that
#'   the verdict is keyed to. Applied to every profile row jointly.
#' @param structure Optional. `"cpm"` (default) simulates from the Browne
#'   model's model-implied scale correlations; `"observed"` bypasses the model
#'   and uses the pooled within-group correlation matrix directly (a
#'   sensitivity switch: if the two verdicts differ, structure uncertainty is
#'   itself material).
#' @param m Optional. The number of harmonics passed to [cpm_fit()] (default
#'   `NULL` uses `min(3, floor((p - 1) / 2))`).
#' @param cpm Optional. A pre-fitted `circumplex_cpm` object to reuse for the
#'   population structure instead of refitting (its scales must match the ssm
#'   object's). Ignored when `structure = "observed"`.
#' @param data Optional. The original data set, required only for ssm objects
#'   created before sufficient statistics were stored at analysis time; the
#'   statistics are then recomputed and checked against the stored profile
#'   vectors.
#' @param parallel Optional. `"no"` (default), `"multicore"`, or `"snow"`;
#'   distributes the simulation replicates across `ncpus` cores. Results are
#'   identical for a given seed regardless of these settings (see
#'   Reproducibility).
#' @param ncpus Optional. Number of cores when `parallel` is not `"no"`
#'   (default = 1).
#' @return A `circumplex_ci_accuracy` object: a list with `coverage` (per
#'   Profile x Parameter x Condition: coverage, its Monte Carlo SE, the
#'   one-sided miss rates, median CI width, and for displacement the
#'   certification-conditional coverage with the number of certified
#'   replicates behind it -- for a contrast row this conditional column is
#'   retained as a joint-certification descriptive that no display consumes;
#'   `Structural` flags the amplitude rows whose zero
#'   coverage is a theorem rather than a measurement), `guardrail` (per
#'   Profile x Condition: certification rate with its 95% Wilson score
#'   interval, the user-expectation benchmark `(1 - interval) / 2`, the
#'   stored false-certification caution decision at the `c = 0` rung
#'   (`Caution`, true when the Wilson lower bound exceeds the benchmark;
#'   `NA` off that rung, and `NA` for a contrast row, which
#'   `print.circumplex_ssm()` never gates), fit-pass
#'   rate, and the
#'   branch-pathology rate -- the rate at which a displacement point estimate
#'   falls geometrically outside its own interval), `verdict`
#'   (Wilson-vs-Bradley classification of elevation, amplitude, and
#'   displacement coverage at the as-estimated condition -- a profile's
#'   displacement is classified certification-conditionally (`Parameter`
#'   `"d_conditional"`), a contrast's unconditionally (`Parameter` `"d"`) --
#'   plus an
#'   overall worst-of row per profile; note the printed verdict headline
#'   additionally elevates to CAUTION whenever the guardrail `Caution` fired,
#'   so it can read worse than the overall coverage class),
#'   `cpm` (the embedded [cpm_fit()] object, or `NULL` when
#'   `structure = "observed"`), `population` (per profile row: the population
#'   profile vectors, truth parameters, and any positive-semidefiniteness
#'   repair magnitude, by condition), and `details`. The `plot()` method
#'   draws coverage against the amplitude ladder with the Bradley band
#'   shaded; `summary()` adds a plain-language verdict (see
#'   [summary.circumplex_ci_accuracy()]).
#' @section Reproducibility:
#'   This function is stochastic: call `set.seed()` immediately before it.
#'   It draws one `sample.int()` value from the caller's random number stream
#'   to seed an internal L'Ecuyer-CMRG stream, gives every simulated dataset
#'   its own deterministic substream, and then restores the caller's
#'   `.Random.seed` and generator kind on exit, so results for a given seed
#'   are identical regardless of `parallel`/`ncpus` and the caller's stream
#'   is advanced by exactly that one draw.
#' @section Limitations:
#'   Coverage is evaluated at the fitted structure, not the unknown truth
#'   ("would the procedure work in a population like your estimates", not
#'   "did your interval cover"). Simulated populations are multivariate
#'   normal with the fitted correlation structure; heavy tails or skew in the
#'   real data can degrade coverage further than reported. The diagnostic
#'   assesses the complete-data procedure (missing data are not simulated),
#'   and groups are assumed to share one circumplex structure. When the
#'   Browne model fits poorly, the simulated population may misrepresent the
#'   data; the embedded fit and its diagnostics are returned for inspection.
#' @references Zimmermann, J., & Wright, A. G. C. (2017). Beyond description
#'   in interpersonal construct validation: Methodological advances in the
#'   circumplex Structural Summary Approach. \emph{Assessment, 24}(1), 3-23.
#'
#'   Browne, M. W. (1992). Circumplex models for correlation matrices.
#'   \emph{Psychometrika, 57}(4), 469-497.
#'
#'   Bradley, J. V. (1978). Robustness? \emph{British Journal of Mathematical
#'   and Statistical Psychology, 31}(2), 144-152.
#' @family ssm functions
#' @family analysis functions
#' @export
#' @examples
#' \donttest{
#' data("jz2017")
#' set.seed(12345)
#' res <- ssm_analyze(
#'   jz2017[1:200, ],
#'   scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
#'   boots = 100
#' )
#' # Small reps/boots keep the example fast; use the defaults in practice
#' set.seed(23456)
#' acc <- ssm_ci_accuracy(res, reps = 25, amplitude_factors = c(1, 0.25))
#' acc
#' summary(acc)
#' }
ssm_ci_accuracy <- function(ssm_object, reps = 1000,
                            amplitude_factors = c(1, 0.5, 0.25, 0),
                            structure = c("cpm", "observed"),
                            m = NULL, cpm = NULL, data = NULL,
                            parallel = "no", ncpus = 1) {

  call <- match.call()
  t_start <- proc.time()[["elapsed"]]

  # ---- validate arguments ----
  stopifnot(inherits(ssm_object, "circumplex_ssm"))
  if (inherits(ssm_object, "circumplex_ssm_sem")) {
    stop(
      "ssm_ci_accuracy() replays observed-data resampling procedures and ",
      "cannot assess the latent (SEM-based) estimand of an ssm_sem() ",
      "object; assessing it requires a lavaan refit per simulated dataset. ",
      "A seeded coverage harness for the SEM layer lives in the package's ",
      "development repository (devel/m5-coverage-oracle.R on GitHub).",
      call. = FALSE
    )
  }
  stopifnot(is_scalar_count(reps))
  stopifnot(is.numeric(amplitude_factors), length(amplitude_factors) >= 1,
            all(is.finite(amplitude_factors)),
            all(amplitude_factors >= 0), all(amplitude_factors <= 1))
  amplitude_factors <- unique(amplitude_factors)
  if (!any(amplitude_factors == 1)) {
    stop("`amplitude_factors` must include 1 (the as-estimated condition ",
         "that the verdict is keyed to).", call. = FALSE)
  }
  structure <- match.arg(structure)
  stopifnot(is.null(m) || is_scalar_count(m))
  stopifnot(is.null(cpm) || inherits(cpm, "circumplex_cpm"))
  parallel <- match.arg(parallel, c("no", "multicore", "snow"))
  stopifnot(is_scalar_count(ncpus))
  # The dimensionless certification constant, single-sourced from the rule it
  # measures (ssm_certified, R/ssm_oop.R): r = a_lci/(a_uci - a_lci) >= rule_k
  # (D-007). Echoed in the summary header; the rule is print-independent and
  # scale-free, so there is no scale-dependent threshold to report. (Named
  # rule_k, not cert_k -- the loop below reuses cert_k for a certified count.)
  rule_k <- eval(formals(ssm_certified)$k)

  # ---- unpack the analysis to be assessed ----
  dts <- ssm_object$details
  method <- if (is.null(dts$method)) "bootstrap" else dts$method
  # Positive capability check, not fall-through defaults: this function
  # replays the object's own procedure, so an unknown method or score type
  # must refuse rather than silently replay the wrong procedure (the subclass
  # guard above catches today's known case; this catches the next one).
  if (!method %in% c("bootstrap", "montecarlo")) {
    stop(
      "ssm_ci_accuracy() does not know how to replay method \"", method,
      "\"; only \"bootstrap\" and \"montecarlo\" analyses can be assessed.",
      call. = FALSE
    )
  }
  if (!identical(dts$score_type, "Mean") &&
    !identical(dts$score_type, "Correlation")) {
    stop(
      "ssm_ci_accuracy() does not know how to replay score type \"",
      dts$score_type, "\"; only mean-based and correlation-based analyses ",
      "can be assessed.",
      call. = FALSE
    )
  }
  boots <- dts$boots
  interval <- dts$interval
  contrast <- isTRUE(dts$contrast)
  angles_deg <- as.numeric(dts$angles)
  theta <- as.numeric(as_radian(as_degree(angles_deg)))
  p <- length(theta)
  corr_based <- identical(dts$score_type, "Correlation")

  # Forward the caller's environment so the fallback can resolve any recorded
  # ssm_analyze() call arguments (scales/measures/grouping) that were passed as
  # variables in the user's scope, not this function's frame.
  stats_ss <- ssm_suff_stats(ssm_object, data = data, envir = parent.frame())
  n_g <- stats_ss$n
  G <- length(n_g)

  scores <- ssm_object$scores
  scale_names <- setdiff(colnames(scores), c("Label", "Group", "Measure"))
  stopifnot(length(scale_names) == p)
  # scores holds one row per profile plus, when contrasting, one contrast row
  n_rows <- nrow(scores)
  n_prof <- n_rows - as.integer(contrast)
  prof_mat <- as.matrix(scores[seq_len(n_prof), scale_names, drop = FALSE])
  row_labels <- as.character(scores$Label)

  # Parameter columns of the {e, x, y, a, d, fit} blocks, resolved by name
  # (the M2 convention; hard-coded positions would silently corrupt if the
  # parameter order ever changed)
  pnames <- ssm_param_names()
  lin_cols <- which(pnames %in% c("e", "x", "y", "a"))
  a_col <- which(pnames == "a")
  d_col <- which(pnames == "d")
  fit_col <- which(pnames == "fit")

  if (corr_based) {
    measure_names <- setdiff(colnames(stats_ss$cormats[[1]]), scale_names)
    q <- length(measure_names)
    stopifnot(n_prof == G * q)
  } else {
    measure_names <- NULL
    q <- 0L
  }

  # ---- population truths at c = 1; refuse flat profiles ----
  truth1 <- t(vapply(
    seq_len(n_prof),
    function(i) as.numeric(ssm_parameters_cpp(prof_mat[i, ], theta)),
    numeric(6)
  ))
  if (any(is.na(truth1[, fit_col]))) {
    stop("One or more profile vectors are flat (zero variance); a flat ",
         "population has no CI behavior to assess.", call. = FALSE)
  }

  # ---- pooled within-group scale correlations (spec sec. 3.2, step 1) ----
  Rw <- matrix(0, p, p, dimnames = list(scale_names, scale_names))
  wsum <- 0
  for (g in seq_len(G)) {
    Rw <- Rw + (n_g[[g]] - 1) *
      stats_ss$cormats[[g]][scale_names, scale_names]
    wsum <- wsum + (n_g[[g]] - 1)
  }
  Rw <- Rw / wsum

  cpm_obj <- NULL
  if (structure == "cpm") {
    if (!is.null(cpm)) {
      if (!identical(cpm$details$scales, scale_names)) {
        stop("The supplied `cpm` was fitted to different scales than this ",
             "ssm object.", call. = FALSE)
      }
      cpm_obj <- cpm
    } else {
      m_eff <- if (is.null(m)) min(3, floor((p - 1) / 2)) else m
      # The n device yields the pooled-within Wishart df sum(n_g - 1):
      # cpm_fit()'s internal multiplier is n - 1 (spec sec. 3.2)
      if (sum(n_g) - G + 1 <= p) {
        stop("Too few observations to characterize the population structure: ",
             "the pooled within-group sample size (", sum(n_g) - G + 1,
             ") must exceed the number of scales (", p, ").", call. = FALSE)
      }
      cpm_obj <- cpm_fit(
        cormat = Rw, n = sum(n_g) - G + 1, scales = scale_names,
        angles = angles_deg, m = m_eff, model = "quasi-circumplex",
        ci_method = "analytic"
      )
    }
    P <- cpm_obj$matrices$Phat[scale_names, scale_names]
  } else {
    P <- Rw
  }

  # ---- degenerate-ladder check (spec sec. 4.1): the margin rung ----
  # If a profile row's amplitude estimate is below half its own CI width, the
  # user's analysis already sits in the near-zero regime and the relative
  # ladder degenerates (every rung is small next to the amplitude's sampling
  # error). One absolute rung is then added at the certification margin: c
  # chosen so c * a_hat equals the observed amplitude-CI half-width. With
  # several such rows the joint ladder takes the largest such c (the neediest
  # row lands exactly at its margin, the others at or above theirs) -- an
  # adopted default recorded in MILESTONES.md.
  res_prof <- ssm_object$results[seq_len(n_prof), , drop = FALSE]
  a_half <- (res_prof$a_uci - res_prof$a_lci) / 2
  near_zero <- is.finite(a_half) & is.finite(res_prof$a_est) &
    res_prof$a_est > 1e-12 & res_prof$a_est < a_half
  margin_rung <- if (any(near_zero)) {
    max(a_half[near_zero] / res_prof$a_est[near_zero])
  }
  near_zero_rows <- row_labels[seq_len(n_prof)][near_zero]

  # ---- amplitude ladder (spec sec. 4.1, functional-targeted 3x3 solve) ----
  corr_vecs <- t(vapply(
    seq_len(n_prof),
    function(i) ssm_ci_ladder_correction(prof_mat[i, ], theta),
    numeric(p)
  ))

  # One condition's population: profiles (post-repair on the correlation
  # path), recomputed truths (spec sec. 3.3), and simulation ingredients
  build_pop <- function(cc) {
    profk <- prof_mat - (1 - cc) * corr_vecs
    if (!corr_based) {
      truths <- t(apply(profk, 1, function(v) {
        as.numeric(ssm_parameters_cpp(v, theta))
      }))
      list(profiles = profk, truths = truths,
           deltas = stats::setNames(rep(0, G), names(n_g)))
    } else {
      roots <- vector("list", G)
      deltas <- stats::setNames(numeric(G), names(n_g))
      prof_rep <- profk
      vars <- c(scale_names, measure_names)
      for (g in seq_len(G)) {
        J <- stats_ss$cormats[[g]][vars, vars]
        J[scale_names, scale_names] <- P
        for (mm in seq_len(q)) {
          i_row <- (g - 1) * q + mm
          J[measure_names[mm], scale_names] <- profk[i_row, ]
          J[scale_names, measure_names[mm]] <- profk[i_row, ]
        }
        cross <- J[measure_names, scale_names, drop = FALSE]
        if (any(!is.finite(cross)) || any(abs(cross) >= 1 - 1e-12)) {
          stop("One or more population scale-measure correlations are ",
               "undefined or equal to +/-1; the plug-in population is ",
               "degenerate.", call. = FALSE)
        }
        repair <- ssm_ci_psd_repair(J)
        deltas[g] <- repair$delta
        if (repair$delta > ssm_ci_psd_warn) {
          warning("The positive-semidefiniteness repair of the joint ",
                  "population matrix changed a correlation by ",
                  round(repair$delta, 4), " (group ", names(n_g)[g],
                  ", c = ", cc, "); population realism is reduced.",
                  call. = FALSE)
        }
        # Truth is computed from the matrix actually simulated from
        for (mm in seq_len(q)) {
          prof_rep[(g - 1) * q + mm, ] <-
            repair$S[measure_names[mm], scale_names]
        }
        roots[[g]] <- mvn_root(repair$S)
      }
      truths <- t(apply(prof_rep, 1, function(v) {
        as.numeric(ssm_parameters_cpp(v, theta))
      }))
      list(profiles = prof_rep, truths = truths,
           deltas = deltas, roots = roots)
    }
  }

  conds <- amplitude_factors
  pop_cond <- lapply(conds, build_pop)
  if (!is.null(margin_rung)) {
    # The margin rung amplifies the first-harmonic content (c > 1), which on
    # the correlation path can push a population cross-correlation past the
    # |r| < 1 guard; the rung is then dropped, not the whole run. The
    # asymmetry with the unguarded lapply above is deliberate: a rung the
    # user asked for must fail hard rather than silently vanish from the
    # tables, while this rung is an automatic addition.
    mp <- tryCatch(build_pop(margin_rung), error = function(e) e)
    if (inherits(mp, "error")) {
      warning("The near-zero margin rung (c = ", round(margin_rung, 3),
              ") was dropped: ", conditionMessage(mp), call. = FALSE)
      margin_rung <- NULL
    } else {
      conds <- c(conds, margin_rung)
      pop_cond <- c(pop_cond, list(mp))
    }
  }
  n_cond <- length(conds)
  truth_con <- if (contrast) {
    lapply(pop_cond, function(pc) {
      as.numeric(param_diff(pc$truths[2, ], pc$truths[1, ]))
    })
  }

  # ---- mean-path simulation ingredients ----
  if (!corr_based) {
    sds <- lapply(seq_len(G), function(g) {
      as.numeric(stats_ss$sds[[g]][scale_names])
    })
    # Precompute the draw-invariant ingredients once, mirroring the observed
    # path's root_P: the CPM loadings depend only on the fitted object, not on n
    # or the draw, so cpm_sim_draw() consumes the same RNG stream cpm_simulate()
    # would (results unchanged at a fixed seed).
    root_P <- if (structure == "observed") mvn_root(P)
    sim_root <- if (structure == "cpm") cpm_sim_root(cpm_obj)
    sim_scales <- function(n) {
      if (structure == "cpm") {
        cpm_sim_draw(sim_root, n)
      } else {
        matrix(stats::rnorm(n * p), n, p) %*% root_P
      }
    }
  }

  # ---- one simulation replicate: simulate, replay, record ----
  grp_vec <- rep(seq_len(G), times = as.integer(n_g))
  run_one <- function(k, stream) {
    assign(".Random.seed", stream, envir = globalenv())
    pc <- pop_cond[[k]]
    tryCatch({
      suppressWarnings({
        # 1. Simulate every group at its exact n (spec sec. 3.2, step 2)
        sims <- vector("list", G)
        for (g in seq_len(G)) {
          if (!corr_based) {
            Z <- sim_scales(n_g[[g]])
            sims[[g]] <- sweep(
              sweep(Z, 2, sds[[g]], "*"), 2, pc$profiles[g, ], "+"
            )
            colnames(sims[[g]]) <- scale_names
          } else {
            sims[[g]] <- matrix(
              stats::rnorm(n_g[[g]] * (p + q)), n_g[[g]]
            ) %*% pc$roots[[g]]
            colnames(sims[[g]]) <- c(scale_names, measure_names)
          }
        }

        # 2. Point estimates on the simulated data (the replicate's t0)
        t0_prof <- matrix(NA_real_, n_prof, p)
        for (g in seq_len(G)) {
          if (!corr_based) {
            t0_prof[g, ] <- colMeans(sims[[g]])
          } else {
            r_hat <- stats::cor(
              sims[[g]][, measure_names, drop = FALSE],
              sims[[g]][, scale_names, drop = FALSE]
            )
            t0_prof[(g - 1) * q + seq_len(q), ] <- r_hat
          }
        }
        t0_par <- matrix(group_parameters(t0_prof, theta), ncol = 6,
                         byrow = TRUE)
        t0_all <- if (contrast) {
          rbind(t0_par, as.numeric(param_diff(t0_par[2, ], t0_par[1, ])))
        } else {
          t0_par
        }

        # 3. Replay the object's own interval procedure (spec sec. 3.4)
        if (method == "montecarlo") {
          cs <- do.call(rbind, lapply(sims, function(S) {
            S[, scale_names, drop = FALSE]
          }))
          mv <- if (corr_based) {
            do.call(rbind, lapply(sims, function(S) {
              S[, measure_names, drop = FALSE]
            }))
          }
          t_mat <- ssm_mc_replicates(cs, mv, grp_vec, t0_prof, boots, theta,
                                     contrast)
        } else {
          # Direct multinomial-weight resampling: the resample counts of
          # boot::boot()'s iid index draw are multinomial(n, 1/n), and the
          # statistic depends on the resample only through its counts, so the
          # law of the replicates is exactly the object's stratified
          # nonparametric bootstrap (implementation freedom per spec sec. 3.4)
          rep_mats <- vector("list", n_prof)
          for (g in seq_len(G)) {
            W <- ssm_ci_boot_weights(n_g[[g]], boots)
            if (!corr_based) {
              means <- ssm_ci_wboot_means(sims[[g]], W)
              rep_mats[[g]] <- matrix(group_parameters(means, theta),
                                      ncol = 6, byrow = TRUE)
            } else {
              wr <- ssm_ci_wboot_cors(
                sims[[g]][, scale_names, drop = FALSE],
                sims[[g]][, measure_names, drop = FALSE], W
              )
              for (mm in seq_len(q)) {
                rep_mats[[(g - 1) * q + mm]] <- matrix(
                  group_parameters(wr[[mm]], theta), ncol = 6, byrow = TRUE
                )
              }
            }
          }
          t_mat <- do.call(cbind, rep_mats)
          if (contrast) {
            t_mat <- cbind(t_mat, param_diff(rep_mats[[2]], rep_mats[[1]]))
          }
        }
        lean <- ssm_ci_intervals_lean(as.vector(t(t0_all)), t_mat, interval,
                                      contrast)

        # 4. Record coverage, misses, widths, guardrail events (sec. 3.4)
        cover <- miss <- width <- matrix(NA_real_, n_rows, 6)
        for (r in seq_len(n_rows)) {
          tr <- if (r <= n_prof) pc$truths[r, ] else truth_con[[k]]
          for (j in lin_cols) {
            l <- lean$lci[r, j]; u <- lean$uci[r, j]; tv <- tr[j]
            if (is.na(l) || is.na(u) || is.na(tv)) next
            width[r, j] <- u - l
            cover[r, j] <- as.numeric(tv >= l && tv <= u)
            miss[r, j] <- if (tv < l) -1 else if (tv > u) 1 else 0
          }
          dc <- ssm_ci_d_cover(tr[d_col], lean$lci[r, d_col],
                               lean$uci[r, d_col],
                               contrast = contrast && r == n_rows)
          cover[r, d_col] <- as.numeric(dc$cover)
          miss[r, d_col] <- dc$side
          width[r, d_col] <- dc$width
        }
        # Profile rows: the shipped guardrail rule, identical to the one
        # print.circumplex_ssm() applies. The contrast row is NOT gated by print
        # (a contrast's amplitude is a difference, not a prototypicality
        # measure; M15-D1), so cert[1] && cert[2] here conditions no displayed
        # number -- it only populates the retained `Coverage_conditional` /
        # `Cert_rate` object columns (documented joint-certification
        # descriptives). Its guardrail Caution is NA'd below.
        cert <- ssm_certified(lean$lci[, a_col], lean$uci[, a_col])
        if (contrast) cert[n_rows] <- cert[1] && cert[2]
        fitpass <- !is.na(t0_all[, fit_col]) & t0_all[, fit_col] >= 0.70
        if (contrast) fitpass[n_rows] <- NA
        branch <- vapply(seq_len(n_rows), function(r) {
          dcv <- ssm_ci_d_cover(t0_all[r, d_col], lean$lci[r, d_col],
                                lean$uci[r, d_col],
                                contrast = contrast && r == n_rows)
          if (is.na(dcv$cover)) NA else !dcv$cover
        }, logical(1))

        list(cover = cover, miss = miss, width = width, cert = cert,
             fitpass = fitpass, branch = branch,
             dcov_cert = ifelse(cert, cover[, d_col], NA),
             degen = lean$n_degenerate, failed = FALSE)
      })
    }, error = function(e) list(failed = TRUE, message = conditionMessage(e)))
  }

  # ---- RNG bracket (spec sec. 7): CMRG substreams, caller state restored ----
  # One documented draw from the caller's stream seeds the master; the
  # caller's post-draw .Random.seed (kind included) is restored on exit, so
  # the caller's stream advances by exactly this one draw
  master_seed <- sample.int(.Machine$integer.max, 1)
  saved_seed <- get(".Random.seed", envir = globalenv())
  on.exit(assign(".Random.seed", saved_seed, envir = globalenv()), add = TRUE)
  RNGkind("L'Ecuyer-CMRG")
  set.seed(master_seed)

  n_jobs <- n_cond * reps
  streams <- vector("list", n_jobs)
  s <- get(".Random.seed", envir = globalenv())
  for (j in seq_len(n_jobs)) {
    s <- parallel::nextRNGStream(s)
    streams[[j]] <- s
  }
  job <- function(j) run_one((j - 1) %/% reps + 1, streams[[j]])
  res_list <- switch(parallel,
    no = lapply(seq_len(n_jobs), job),
    multicore = parallel::mclapply(seq_len(n_jobs), job, mc.cores = ncpus,
                                   mc.set.seed = FALSE),
    snow = {
      cl <- parallel::makePSOCKcluster(ncpus)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::parLapply(cl, seq_len(n_jobs), job)
    }
  )

  # ---- aggregate (spec sec. 3.5) ----
  param_keys <- c("e", "x", "y", "a", "d")
  cov_frames <- vector("list", n_cond)
  grd_frames <- vector("list", n_cond)
  failed_reps <- stats::setNames(integer(n_cond), as.character(conds))
  degen_reps <- stats::setNames(numeric(n_cond), as.character(conds))
  dcond_at_1 <- NULL
  for (k in seq_len(n_cond)) {
    jr <- res_list[(k - 1) * reps + seq_len(reps)]
    ok <- !vapply(jr, function(x) isTRUE(x$failed), logical(1))
    failed_reps[k] <- sum(!ok)
    jr <- jr[ok]
    if (length(jr) == 0) {
      first_msg <- res_list[[(k - 1) * reps + 1]]$message
      stop("All simulation replicates failed at condition c = ", conds[k],
           " (first error: ", first_msg, ").", call. = FALSE)
    }
    degen_reps[k] <- sum(vapply(jr, `[[`, numeric(1), "degen"))
    cover_a <- simplify2array(lapply(jr, `[[`, "cover"))
    miss_a <- simplify2array(lapply(jr, `[[`, "miss"))
    width_a <- simplify2array(lapply(jr, `[[`, "width"))
    # vapply collapses a 1-row template to a vector; re-matrix uniformly
    grab_rows <- function(field, template) {
      out <- vapply(jr, `[[`, template, field)
      if (n_rows == 1) matrix(out, nrow = 1) else out
    }
    cert_m <- grab_rows("cert", logical(n_rows))
    fitp_m <- grab_rows("fitpass", logical(n_rows))
    branch_m <- grab_rows("branch", logical(n_rows))
    dcov_m <- grab_rows("dcov_cert", numeric(n_rows))
    if (conds[k] == 1) dcond_at_1 <- dcov_m

    grid <- expand.grid(j = seq_along(param_keys), r = seq_len(n_rows))
    stats_rows <- lapply(seq_len(nrow(grid)), function(i) {
      r <- grid$r[i]
      j <- grid$j[i]
      is_d <- param_keys[j] == "d"
      cv <- cover_a[r, j, ]
      n_eff <- sum(!is.na(cv))
      cvg <- if (n_eff > 0) mean(cv, na.rm = TRUE) else NA_real_
      w <- width_a[r, j, ]
      if (is_d) w <- w * 180 / pi
      n_cnd <- if (is_d) sum(!is.na(dcov_m[r, ])) else NA_integer_
      dcc <- if (is_d && n_cnd > 0) {
        mean(dcov_m[r, ], na.rm = TRUE)
      } else {
        NA_real_
      }
      tr <- if (r <= n_prof) pop_cond[[k]]$truths[r, ] else truth_con[[k]]
      data.frame(
        Profile = row_labels[r], Parameter = param_keys[j],
        Condition = conds[k],
        Coverage = cvg,
        MC_se = if (n_eff > 0) sqrt(cvg * (1 - cvg) / n_eff) else NA_real_,
        Left_miss = if (n_eff > 0) mean(miss_a[r, j, ] == -1, na.rm = TRUE)
                    else NA_real_,
        Right_miss = if (n_eff > 0) mean(miss_a[r, j, ] == 1, na.rm = TRUE)
                     else NA_real_,
        Median_width = stats::median(w, na.rm = TRUE),
        Coverage_conditional = dcc,
        N_conditional = n_cnd,
        # The sec. 4.2 theorem flag: a percentile interval of strictly
        # positive amplitude replicates cannot contain a zero truth, so this
        # row's coverage is structural, not informative. Zero-amplitude
        # populations are exactly those with an undefined displacement truth
        # (flat populations are refused up front); a contrast's amplitude
        # difference is unconstrained, so its rows are never structural.
        Structural = param_keys[j] == "a" && r <= n_prof && is.na(tr[d_col]),
        N_reps = n_eff,
        stringsAsFactors = FALSE
      )
    })
    cov_frames[[k]] <- do.call(rbind, stats_rows)

    # rowMeans of an all-NA row (e.g. the contrast row's fit-pass, which is
    # not a prototypicality measure) is NaN; report NA
    rate <- function(m) {
      out <- rowMeans(m, na.rm = TRUE)
      out[is.nan(out)] <- NA_real_
      out
    }
    # False-certification measurement (spec sec. 4.3): the certification
    # rate carries its own 95% Wilson interval so the summary() caution can
    # trigger on the interval's lower bound exceeding the user-expectation
    # benchmark, never on Monte Carlo noise
    cert_k <- rowSums(cert_m, na.rm = TRUE)
    cert_n <- rowSums(!is.na(cert_m))
    cert_w <- t(vapply(seq_len(n_rows), function(r) {
      ssm_ci_wilson(cert_k[r], cert_n[r])
    }, numeric(2)))
    # The false-certification caution is a property of the c = 0 rung
    # (P(certified | a0 = 0)); it is stored here so print()/summary() and any
    # programmatic consumer share one decision (NA off that rung). The contrast
    # row is NA'd even at c = 0: print.circumplex_ssm() applies no certification
    # gate to a contrast, so a false-certification verdict does not apply to it
    # (M15-D1). Its Cert_rate is retained as the documented joint-certification
    # rate -- the denominator provenance for the retained Coverage_conditional
    # column -- not a conditioning device for any displayed number.
    caution_col <- if (conds[k] == 0) {
      cau <- ssm_ci_guardrail_caution(cert_w[, 1], (1 - interval) / 2)
      if (contrast) cau[n_rows] <- NA
      cau
    } else {
      NA
    }
    grd_frames[[k]] <- data.frame(
      Profile = row_labels[seq_len(n_rows)],
      Condition = conds[k],
      Cert_rate = rate(cert_m),
      Cert_lci = cert_w[, 1],
      Cert_uci = cert_w[, 2],
      Benchmark = (1 - interval) / 2,
      Caution = caution_col,
      Fit_pass_rate = rate(fitp_m),
      Branch_pathology_rate = rate(branch_m),
      N_reps = cert_n,
      stringsAsFactors = FALSE
    )
  }
  coverage <- do.call(rbind, cov_frames)
  rownames(coverage) <- NULL
  guardrail <- do.call(rbind, grd_frames)
  rownames(guardrail) <- NULL

  # ---- verdict at c = 1 (spec sec. 5.1): Wilson-95 vs Bradley liberal ----
  verdict <- ssm_ci_verdict(coverage, dcond_at_1, row_labels[seq_len(n_rows)],
                            interval,
                            contrast_lab = if (contrast) row_labels[n_rows])

  # ---- population record (spec sec. 7) ----
  population <- lapply(seq_len(n_rows), function(r) {
    tm <- t(vapply(seq_len(n_cond), function(k) {
      if (r <= n_prof) pop_cond[[k]]$truths[r, ] else truth_con[[k]]
    }, numeric(6)))
    colnames(tm) <- pnames
    truths <- data.frame(Condition = conds, e = tm[, "e"], x = tm[, "x"],
                         y = tm[, "y"], a = tm[, "a"],
                         d = tm[, "d"] * 180 / pi, fit = tm[, "fit"])
    profiles <- if (r <= n_prof) {
      out <- t(vapply(seq_len(n_cond), function(k) {
        pop_cond[[k]]$profiles[r, ]
      }, numeric(p)))
      dimnames(out) <- list(as.character(conds), scale_names)
      out
    }
    deltas <- if (r <= n_prof) {
      g <- if (corr_based) (r - 1) %/% q + 1 else r
      stats::setNames(
        vapply(seq_len(n_cond), function(k) pop_cond[[k]]$deltas[[g]],
               numeric(1)),
        as.character(conds)
      )
    }
    list(label = row_labels[r], profiles = profiles, truths = truths,
         psd_delta = deltas)
  })
  names(population) <- row_labels[seq_len(n_rows)]

  # Per-row sample size for the verdict blocks (NA on the contrast row: its
  # inputs are the two profile rows' samples)
  row_n <- vapply(seq_len(n_rows), function(r) {
    if (r > n_prof) return(NA_real_)
    g <- if (corr_based) (r - 1) %/% q + 1 else r
    as.numeric(n_g[[g]])
  }, numeric(1))
  names(row_n) <- row_labels

  details <- list(
    reps = reps,
    amplitude_factors = amplitude_factors,
    conditions = conds,
    margin_rung = margin_rung,
    near_zero_rows = near_zero_rows,
    structure = structure,
    method = method,
    boots = boots,
    interval = interval,
    score_type = dts$score_type,
    contrast = contrast,
    angles = dts$angles,
    cert_k = rule_k,
    n = n_g,
    row_n = row_n,
    max_psd_delta = max(vapply(pop_cond, function(pc) max(pc$deltas),
                               numeric(1))),
    structure_matrix = P,
    cpm_diagnostics = if (!is.null(cpm_obj)) {
      list(
        accepted = cpm_obj$details$accepted,
        markers = cpm_boundary_markers(cpm_obj),
        m = cpm_obj$details$m,
        rmsea = cpm_obj$fit$rmsea,
        srmr = cpm_obj$fit$srmr
      )
    },
    failed_reps = failed_reps,
    degenerate_replicates = degen_reps,
    parallel = parallel,
    ncpus = ncpus,
    elapsed = proc.time()[["elapsed"]] - t_start
  )

  new_ci_accuracy(
    coverage = coverage,
    guardrail = guardrail,
    verdict = verdict,
    cpm = cpm_obj,
    population = population,
    details = details,
    call = call
  )
}

# Conventional global-fit benchmarks for the structure-note wording of
# summary.circumplex_ci_accuracy() (spec sec. 5.2 -- cited, not invented):
# RMSEA <= .08 indicates reasonable and > .10 poor fit (Browne & Cudeck,
# 1993, "Alternative ways of assessing model fit", in Bollen & Long, Testing
# Structural Equation Models, pp. 136-162); SRMR <= .08 indicates good fit
# (Hu & Bentler, 1999, Structural Equation Modeling, 6(1), 1-55). They gate
# wording only, never estimation.
ssm_ci_rmsea_reasonable <- 0.08
ssm_ci_rmsea_poor <- 0.10
ssm_ci_srmr_good <- 0.08

# PSD-repair magnitude above which population realism is flagged (spec
# sec. 3.2's 0.01 bar), shared by the construction-time warning and the
# summary() structure-note annotation so the two cannot drift
ssm_ci_psd_warn <- 0.01

# Amplitude-ladder correction vector (spec sec. 4.1) ---------------------------
# The closed-form SSM estimator is linear in the profile, so its (e, x, y)
# images of the basis {1, cos theta, sin theta} form a 3x3 system M. Solving
# M (gamma, alpha, beta)' = (0, x_hat, y_hat)' gives the profile direction
# whose removal scales the estimator's own (x, y) -- hence amplitude -- toward
# zero while leaving elevation exactly on target, FOR ANY ANGLE SPACING (off
# equal spacing the naive first-harmonic decomposition does not scale the
# closed-form amplitude; B-review F3). profile(c) = profile - (1 - c) * out.
ssm_ci_ladder_correction <- function(profile, theta) {
  basis <- cbind(1, cos(theta), sin(theta))
  M <- vapply(1:3, function(j) {
    as.numeric(ssm_parameters_cpp(basis[, j], theta))[1:3]
  }, numeric(3))
  t1 <- as.numeric(ssm_parameters_cpp(profile, theta))
  coef <- tryCatch(
    solve(M, c(0, t1[2], t1[3])),
    error = function(e) {
      stop("The amplitude ladder is refused: the estimator-functional ",
           "matrix is singular for these analysis angles.", call. = FALSE)
    }
  )
  as.numeric(basis %*% coef)
}

# Angular interval membership (spec sec. 3.4) ----------------------------------
# Coverage of a displacement truth is membership of the reported interval as
# an arc, modulo 2*pi, so a truth at the 0/360 pole and branch-shifted
# contrast intervals need no special-casing. Profile intervals arrive wrapped
# to [0, 2*pi) (lci > uci means the arc crosses the pole); contrast intervals
# arrive branch-aligned with lci <= uci. `side` gives the one-sided miss
# direction (-1 truth below lci, +1 above uci) by the shorter angular path.
ssm_ci_d_cover <- function(x, lci, uci, contrast = FALSE) {
  if (is.na(x) || is.na(lci) || is.na(uci)) {
    return(list(cover = NA, side = NA_real_, width = NA_real_))
  }
  two_pi <- 2 * pi
  width <- if (contrast) min(uci - lci, two_pi) else (uci - lci) %% two_pi
  if (((x - lci) %% two_pi) <= width) {
    return(list(cover = TRUE, side = 0, width = width))
  }
  below <- (lci - x) %% two_pi
  above <- (x - uci) %% two_pi
  list(cover = FALSE, side = if (below <= above) -1 else 1, width = width)
}

# Lean interval assembly -------------------------------------------------------
# The quantile logic of ssm_replicate_intervals() (percentile bounds with
# na.rm-conditional intervals, circular quantiles for displacement, contrast
# branch alignment) without its data-frame assembly, warnings, or degree
# conversion -- the simulation loop calls this reps x conditions times.
# Equality with ssm_replicate_intervals() is pinned by test-ci_accuracy.R.
# t0 is the observed parameter vector (6 per row, displacement in radians)
# and t the replicate matrix with matching columns.
ssm_ci_intervals_lean <- function(t0, t, interval, contrast) {
  pnames <- ssm_param_names()
  npar <- length(pnames)
  n_rows <- length(t0) / npar
  est <- matrix(as.numeric(t0), nrow = n_rows, ncol = npar, byrow = TRUE)
  lci <- uci <- matrix(NA_real_, n_rows, npar)
  probs <- c((1 - interval) / 2, 1 - (1 - interval) / 2)
  d_col <- which(pnames == "d")
  fit_col <- which(pnames == "fit")
  for (r in seq_len(n_rows)) {
    block <- t[, (r - 1) * npar + seq_len(npar), drop = FALSE]
    for (j in seq_len(npar)) {
      if (j == fit_col) next
      col <- block[, j]
      if (all(is.na(col))) next
      if (j == d_col) {
        is_con <- contrast && r == n_rows
        col_obj <- if (is_con) new_contrast_radian(col) else new_radian(col)
        qs <- quantile(col_obj, probs = probs, na.rm = TRUE)
        if (length(qs) == 1 && is.na(qs)) next
        qs <- as.numeric(qs)
        if (is_con && all(is.finite(c(est[r, j], qs)))) {
          # Report the contrast CI on the branch of its estimate (the same
          # 2*pi*k shift ssm_replicate_intervals() applies)
          qs <- qs + 2 * pi * round((est[r, j] - mean(qs)) / (2 * pi))
        }
      } else {
        qs <- as.numeric(stats::quantile(col, probs = probs, na.rm = TRUE))
      }
      lci[r, j] <- qs[1]
      uci[r, j] <- qs[2]
    }
  }
  list(est = est, lci = lci, uci = uci,
       n_degenerate = sum(!stats::complete.cases(t)))
}

# Multinomial bootstrap weights -------------------------------------------------
# The per-resample counts of an iid index draw (boot::boot's ordinary
# nonparametric bootstrap) are multinomial(n, 1/n); any statistic that depends
# on the resample only through its counts (means, correlations) therefore has
# exactly the bootstrap law. One n x boots count matrix per group implements
# the object's stratified resampling.
ssm_ci_boot_weights <- function(n, boots) {
  stats::rmultinom(boots, n, rep.int(1 / n, n))
}

# Resampled group means for every bootstrap column in one crossproduct
ssm_ci_wboot_means <- function(X, W) {
  crossprod(W, X) / nrow(X)
}

# Resampled measure-scale Pearson correlations from weighted moments -----------
# Equals cor() on the expanded resample (pinned by test); columns are centered
# once so the moment differences are numerically stable. Returns one
# boots x p matrix per measure (the correlation-path profile replicates).
# A (near-)zero-variance resample yields NA, matching the analysis path where
# cor() on a degenerate resample yields NaN and the parameter is excluded from
# the interval (conditional-on-estimability convention).
ssm_ci_wboot_cors <- function(X, Y, W) {
  n <- nrow(X)
  p <- ncol(X)
  q <- ncol(Y)
  Xc <- sweep(X, 2, colMeans(X))
  Yc <- sweep(Y, 2, colMeans(Y))
  mi <- rep(seq_len(q), each = p)
  ji <- rep(seq_len(p), times = q)
  A <- cbind(Xc, Yc, Xc^2, Yc^2, Yc[, mi, drop = FALSE] * Xc[, ji, drop = FALSE])
  S <- crossprod(W, A) / n
  sx <- S[, seq_len(p), drop = FALSE]
  sy <- S[, p + seq_len(q), drop = FALSE]
  sxx <- S[, p + q + seq_len(p), drop = FALSE]
  syy <- S[, 2 * p + q + seq_len(q), drop = FALSE]
  sxy <- S[, 2 * p + 2 * q + seq_len(p * q), drop = FALSE]
  vx <- sxx - sx^2
  vy <- syy - sy^2
  # True-constant resamples leave only float-cancellation noise in the
  # variance; below this relative floor the correlation is undefined
  vx[vx <= 1e-12 * sxx] <- NA_real_
  vy[vy <= 1e-12 * syy] <- NA_real_
  out <- vector("list", q)
  for (m in seq_len(q)) {
    covv <- sxy[, which(mi == m), drop = FALSE] - sy[, m] * sx
    r <- covv / sqrt(vx * vy[, m])
    r[!is.finite(r)] <- NA_real_
    out[[m]] <- r
  }
  out
}

# Eigenvalue-clamping PSD repair with unit-diagonal rescaling (spec sec. 3.2);
# reports the largest absolute correlation change it introduced
ssm_ci_psd_repair <- function(S) {
  eig <- eigen(S, symmetric = TRUE)
  if (all(eig$values >= 0)) {
    return(list(S = S, delta = 0))
  }
  V <- eig$vectors %*% (pmax(eig$values, 0) * t(eig$vectors))
  d <- sqrt(diag(V))
  V <- V / tcrossprod(d)
  diag(V) <- 1
  dimnames(V) <- dimnames(S)
  list(S = V, delta = max(abs(V - S)))
}

# 95% Wilson score interval for a binomial proportion (spec sec. 5.1)
ssm_ci_wilson <- function(k, n, conf = 0.95) {
  if (is.na(k) || is.na(n) || n <= 0) {
    return(c(NA_real_, NA_real_))
  }
  z <- stats::qnorm(1 - (1 - conf) / 2)
  ph <- k / n
  den <- 1 + z^2 / n
  ctr <- (ph + z^2 / (2 * n)) / den
  hw <- z * sqrt(ph * (1 - ph) / n + z^2 / (4 * n^2)) / den
  c(max(0, ctr - hw), min(1, ctr + hw))
}

# Classify one empirical coverage against Bradley's (1978) liberal band --------
# [1 - 1.5*alpha, 1 - 0.5*alpha] via the 95% Wilson interval: adequate when
# the Wilson interval sits entirely inside the band, inadequate when entirely
# outside (with the under-/over-coverage direction), borderline otherwise.
ssm_ci_bradley_class <- function(k, n, nominal) {
  w <- ssm_ci_wilson(k, n)
  if (any(is.na(w))) {
    return(c(NA_character_, NA_character_))
  }
  alpha <- 1 - nominal
  band <- c(1 - 1.5 * alpha, 1 - 0.5 * alpha)
  if (w[1] >= band[1] && w[2] <= band[2]) {
    c("adequate", NA_character_)
  } else if (w[2] < band[1]) {
    c("inadequate", "under")
  } else if (w[1] > band[2]) {
    c("inadequate", "over")
  } else {
    c("borderline", NA_character_)
  }
}

# Assemble the verdict table at the as-estimated condition (spec sec. 5.1):
# elevation and amplitude unconditional; a profile's displacement is
# conditional on certification, but the contrast's displacement is
# UNCONDITIONAL (M15-D1: print.circumplex_ssm() never certification-gates a
# contrast, so the verdict classifies the coverage the package actually shows
# -- Parameter "d", not "d_conditional"). x and y are reported in `coverage`
# but do not drive the verdict; the overall row is the worst of the three.
ssm_ci_verdict <- function(coverage, dcond_at_1, labels, interval,
                           contrast_lab = NULL) {
  rank <- c(adequate = 1, borderline = 2, inadequate = 3)
  rows <- list()
  for (r in seq_along(labels)) {
    lab <- labels[r]
    cells <- list()
    for (pm in c("e", "a")) {
      cc <- coverage[coverage$Profile == lab & coverage$Parameter == pm &
                       coverage$Condition == 1, ]
      k <- if (is.na(cc$Coverage)) NA_real_ else round(cc$Coverage * cc$N_reps)
      cells[[pm]] <- list(param = pm, cov = cc$Coverage, k = k, n = cc$N_reps)
    }
    if (!is.null(contrast_lab) && identical(lab, contrast_lab)) {
      # Contrast: classify on the unconditional displacement coverage row.
      cd <- coverage[coverage$Profile == lab & coverage$Parameter == "d" &
                       coverage$Condition == 1, ]
      kd <- if (is.na(cd$Coverage)) NA_real_ else round(cd$Coverage * cd$N_reps)
      cells[["d"]] <- list(param = "d", cov = cd$Coverage, k = kd, n = cd$N_reps)
    } else {
      dv <- dcond_at_1[r, ]
      nd <- sum(!is.na(dv))
      kd <- if (nd > 0) sum(dv, na.rm = TRUE) else NA_real_
      cells[["d"]] <- list(param = "d_conditional",
                           cov = if (nd > 0) kd / nd else NA_real_,
                           k = kd, n = nd)
    }
    classes <- lapply(cells, function(cl) {
      ssm_ci_bradley_class(cl$k, cl$n, interval)
    })
    for (i in seq_along(cells)) {
      cl <- cells[[i]]
      w <- ssm_ci_wilson(cl$k, cl$n)
      rows[[length(rows) + 1]] <- data.frame(
        Profile = lab, Parameter = cl$param, Coverage = cl$cov,
        N_reps = cl$n, Wilson_lci = w[1], Wilson_uci = w[2],
        Class = classes[[i]][1], Direction = classes[[i]][2],
        stringsAsFactors = FALSE
      )
    }
    cls <- vapply(classes, `[`, character(1), 1)
    overall <- if (all(is.na(cls))) NA_character_ else {
      names(rank)[max(rank[cls], na.rm = TRUE)]
    }
    rows[[length(rows) + 1]] <- data.frame(
      Profile = lab, Parameter = "overall", Coverage = NA_real_,
      N_reps = NA_real_, Wilson_lci = NA_real_, Wilson_uci = NA_real_,
      Class = overall, Direction = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
