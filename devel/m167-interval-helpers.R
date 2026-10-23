# M167: interval-method helpers shared by devel/m167-interval-oracle.R and
# devel/m167-interval-checks.R. Prototypes only: nothing here is exported or
# called by the package. Run under devtools::load_all() so the CPM internals
# (cpm_optimize_one, cpm_gradient, cpm_mirror_guard, cpm_unpack, ...) resolve.
#
# The method definitions these functions implement are fixed in the
# pre-registration header of devel/m167-interval-oracle.R; read that first.

# ---- the shipped percentile bootstrap, returning its replicates ----------------

# A copy of cpm_bootstrap() (R/cpm_fit.R) that also returns the replicate
# matrices, so F3 can be formed on the B1 replicates. The RNG use, the
# exclusion rules, the restart and the mirror guard are the shipped ones; the
# checks script asserts that its intervals equal cpm_fit()'s exactly.
m167_boot_reps <- function(engine, sdata, boots) {
  spec <- engine$spec
  par_hat <- engine$par
  N <- nrow(sdata)
  idx <- matrix(sample.int(N, N * boots, replace = TRUE), nrow = boots)
  m167_refit_reps(engine, boots, function(b) {
    suppressWarnings(stats::cor(sdata[idx[b, ], , drop = FALSE]))
  })
}

# F2's replicates: correlation matrices drawn from the fitted Phat at the fit's
# N. The sample correlation of N multivariate-normal rows is cov2cor() of a
# Wishart(N - 1, Phat) draw divided by N - 1, so one rWishart() call per
# replicate gives its exact distribution without drawing N rows.
m167_param_reps <- function(engine, N, boots) {
  P <- engine$P
  W <- stats::rWishart(boots, df = N - 1, Sigma = P)
  m167_refit_reps(engine, boots, function(b) stats::cov2cor(W[, , b]))
}

# The shipped warm-started refit loop (cpm_bootstrap()'s body), fed one
# correlation matrix per replicate by `draw_R(b)`. One departure: a refit
# that throws (for example a singular solve() inside the objective) is
# counted as nonconvergent here, where the shipped loop would abort the fit
# (M167 review); on every resample that does not throw the two are identical.
m167_refit_reps <- function(engine, boots, draw_R) {
  spec <- engine$spec
  par_hat <- engine$par
  p <- spec$p
  ref_rel_hat <- cpm_ref_relative(par_hat, spec)
  theta_reps <- matrix(NA_real_, boots, p)
  zeta_reps <- matrix(NA_real_, boots, p)
  beta_reps <- matrix(NA_real_, boots, spec$m + 1L)
  n_degenerate <- 0L
  n_nonconvergent <- 0L
  for (b in seq_len(boots)) {
    Rb <- draw_R(b)
    if (anyNA(Rb) ||
        min(eigen(Rb, symmetric = TRUE, only.values = TRUE)$values) <= 1e-10) {
      n_degenerate <- n_degenerate + 1L
      next
    }
    Rb <- (Rb + t(Rb)) / 2
    run <- tryCatch({
      r1 <- cpm_optimize_one(par_hat, Rb, spec)
      gnorm <- max(abs(cpm_gradient(r1$par, Rb, spec)))
      if (gnorm > 1e-6 * max(1, abs(r1$F))) {
        r1 <- cpm_optimize_one(r1$par, Rb, spec)
        gnorm <- max(abs(cpm_gradient(r1$par, Rb, spec)))
        if (gnorm > 1e-6 * max(1, abs(r1$F))) r1 <- NULL
      }
      r1
    }, error = function(e) NULL)
    if (is.null(run)) {
      n_nonconvergent <- n_nonconvergent + 1L
      next
    }
    guard <- cpm_mirror_guard(run$par, spec, ref_rel_hat)
    nat <- cpm_unpack(guard$par, spec)
    theta_reps[b, ] <- nat$theta %% (2 * pi)
    zeta_reps[b, ] <- nat$zeta
    beta_reps[b, ] <- nat$beta
  }
  ok <- stats::complete.cases(theta_reps, zeta_reps, beta_reps)
  list(theta = theta_reps[ok, , drop = FALSE],
       zeta = zeta_reps[ok, , drop = FALSE],
       beta = beta_reps[ok, , drop = FALSE],
       n_ok = sum(ok), n_degenerate = n_degenerate,
       n_nonconvergent = n_nonconvergent)
}

# ---- interval forms ------------------------------------------------------------

# Percentile intervals exactly as cpm_bootstrap() forms them: circular quantile
# for angles (degrees, wrapped, lci > uci when the arc straddles 0/360), plain
# quantile for zeta and beta.
m167_percentile <- function(reps, level = 0.95) {
  probs <- c((1 - level) / 2, 1 - (1 - level) / 2)
  p <- ncol(reps$theta)
  out <- m167_empty_ci(p, ncol(reps$beta))
  if (reps$n_ok == 0) return(out)
  for (i in seq_len(p)) {
    q <- quantile.circumplex_radian(new_radian(reps$theta[, i]), probs = probs)
    q <- as.numeric(as_degree(q))
    out$angle[i, ] <- q
    out$zeta[i, ] <- stats::quantile(reps$zeta[, i], probs = probs,
                                     names = FALSE)
  }
  for (k in seq_len(ncol(reps$beta))) {
    out$beta[k, ] <- stats::quantile(reps$beta[, k], probs = probs,
                                     names = FALSE)
  }
  out
}

# Bias-corrected percentile interval (Efron 1981; Efron & Tibshirani 1993,
# sec. 14.3 with acceleration a = 0) for one linear parameter:
#   z0 = qnorm(share of replicates below the estimate, ties counted half),
#   interval = type-7 quantiles at pnorm(2 z0 + qnorm(alpha/2)) and
#   pnorm(2 z0 + qnorm(1 - alpha/2)).
# The share is clamped to [1/(2B), 1 - 1/(2B)] so z0 stays finite when every
# replicate falls on one side. Angles get no BC form (circular parameter).
m167_bc_one <- function(reps, est, level = 0.95) {
  B <- length(reps)
  if (B == 0) return(c(NA_real_, NA_real_))
  share <- (sum(reps < est) + 0.5 * sum(reps == est)) / B
  share <- min(max(share, 1 / (2 * B)), 1 - 1 / (2 * B))
  z0 <- stats::qnorm(share)
  za <- stats::qnorm(c((1 - level) / 2, 1 - (1 - level) / 2))
  stats::quantile(reps, probs = stats::pnorm(2 * z0 + za), names = FALSE)
}

m167_bc <- function(reps, engine, level = 0.95) {
  p <- ncol(reps$zeta)
  out <- m167_empty_ci(p, ncol(reps$beta))
  out$angle[] <- NA_real_                      # not formed for angles
  if (reps$n_ok == 0) return(out)
  for (i in seq_len(p)) {
    out$zeta[i, ] <- m167_bc_one(reps$zeta[, i], engine$zeta[i], level)
  }
  for (k in seq_len(ncol(reps$beta))) {
    out$beta[k, ] <- m167_bc_one(reps$beta[, k], engine$beta[k], level)
  }
  out
}

# B2: the shipped Wald interval (cpm_fit()'s analytic branch).
m167_wald <- function(engine, se, level = 0.95) {
  z <- stats::qnorm(1 - (1 - level) / 2)
  p <- length(engine$zeta)
  out <- m167_empty_ci(p, length(engine$beta))
  out$angle <- cbind(engine$theta - z * se$angle, engine$theta + z * se$angle)
  out$zeta <- cbind(engine$zeta - z * se$zeta, engine$zeta + z * se$zeta)
  out$beta <- cbind(engine$beta - z * se$beta, engine$beta + z * se$beta)
  # a removed weight is the degenerate [0, 0] for every method (header rule),
  # also where a singular information matrix leaves its SE undefined
  out$beta[engine$beta == 0, ] <- 0
  out
}

# F1, the zeta arm: browne1982 eqs. 1.6.30-1.6.32 (p. 95) with h = ln v,
# v = (1 - zeta^2) / zeta^2 (CircE's interval; test-cpm_oracles.R:136).
# SE(ln v) = 2 SE(zeta) / (zeta (1 - zeta^2)) by the delta method. ln v falls
# as zeta rises, so the back-transformed endpoints swap.
m167_lnv_ci <- function(zeta, se_zeta, level = 0.95) {
  z <- stats::qnorm(1 - (1 - level) / 2)
  lv <- log((1 - zeta^2) / zeta^2)
  se_lv <- 2 * se_zeta / (zeta * (1 - zeta^2))
  zeta_of <- function(l) 1 / sqrt(1 + exp(l))
  cbind(zeta_of(lv + z * se_lv), zeta_of(lv - z * se_lv))
}

# F1, the beta arm: the same method with h = logit for each beta_k in (0, 1),
# SE(logit beta) = SE(beta) / (beta (1 - beta)). A removed harmonic
# (beta-hat = 0, SE 0) gets the degenerate interval [0, 0], as B1 and B2 do.
m167_logit_ci <- function(b, se_b, level = 0.95) {
  z <- stats::qnorm(1 - (1 - level) / 2)
  out <- cbind(b, b)
  live <- b > 0 & b < 1
  lb <- stats::qlogis(b[live])
  se_l <- se_b[live] / (b[live] * (1 - b[live]))
  out[live, ] <- cbind(stats::plogis(lb - z * se_l), stats::plogis(lb + z * se_l))
  out
}

# F1: angles stay on the identity scale (an unbounded parameter needs no
# transform), so F1's angle interval equals B2's.
m167_transformed_wald <- function(engine, se, level = 0.95) {
  out <- m167_wald(engine, se, level)
  out$zeta <- m167_lnv_ci(engine$zeta, se$zeta, level)
  out$beta <- m167_logit_ci(engine$beta, se$beta, level)
  out
}

# Basic bootstrap interval (Davison & Hinkley 1997, eq. 5.6) for zeta and beta:
# [2 est - q(.975), 2 est - q(.025)] of the replicates, unclamped, so it can
# leave (0, 1) as the textbook form does. Angles get no basic form.
m167_basic <- function(reps, engine, level = 0.95) {
  p <- ncol(reps$zeta)
  out <- m167_empty_ci(p, ncol(reps$beta))
  if (reps$n_ok == 0) return(out)
  probs <- c((1 - level) / 2, 1 - (1 - level) / 2)
  for (i in seq_len(p)) {
    q <- stats::quantile(reps$zeta[, i], probs = probs, names = FALSE)
    out$zeta[i, ] <- 2 * engine$zeta[i] - rev(q)
  }
  for (k in seq_len(ncol(reps$beta))) {
    q <- stats::quantile(reps$beta[, k], probs = probs, names = FALSE)
    out$beta[k, ] <- 2 * engine$beta[k] - rev(q)
  }
  out
}

# ---- P1: profile-likelihood intervals for zeta and beta ------------------------

# For a component c with value t, minimize F over every other free parameter
# with c held at t, and take {t : (N - 1) (F_prof(t) - F_hat) <= qchisq(.95, 1)}
# (RR26 Q2). Each side is root-found on the logit scale of t from the estimate
# out to a fixed far point; when the statistic stays under the cutoff there, the
# natural bound (0 or 1) is the endpoint. Constructions, on the engine's
# post-polish spec (keep_k lists the kept harmonics; a removed weight gets
# [0, 0] as in every other method):
#   zeta_i: hold u_i = logit(t), free the rest.
#   beta at kept position a >= 2: v_a = logit(t) + log(S), S the sum of exp(v_j)
#     over the other kept positions (v_1 = 0), free the other v.
#   beta at kept position 1 (k = 0): v_j = w_j + c for j >= 2 with
#     c = log((1 - t) / t) - log(sum exp(w)), w_2 = 0, free w_3.. .
# Gradients chain cpm_gradient() through each map. The cutoff is the nominal
# chi-square(1) quantile; at a Heywood wall the statistic is a chi-square
# mixture, so the cutoff is approximate there (stated, not tuned).
#
# m167_profile_setup() builds each component's map once; m167_profile_stat()
# evaluates the statistic at one s. Each evaluation minimizes from the
# estimate's own free parameters and from any extra starts it is given (in
# P1, the solution at the nearest s already evaluated) and keeps the lowest:
# a warm start alone can stall in another basin near a Heywood wall and
# overstate the statistic, which gave false endpoints (M167 review).
m167_profile_setup <- function(engine, R, N) {
  spec <- engine$spec
  par <- engine$par
  p <- spec$p
  ldR <- as.numeric(determinant(R, logarithm = TRUE)$modulus)
  obj <- function(g) cpm_objective(g, R, spec, ldR = ldR)
  grad <- function(g) cpm_gradient(g, R, spec)
  comps <- list()

  # zeta_i
  if (spec$n_zeta == p) {
    for (i in seq_len(p)) {
      comps[[length(comps) + 1L]] <- local({
        idx <- spec$i_zeta[i]
        list(family = "zeta", row = i, s_hat = par[idx], x0 = par[-idx],
             full = function(x, s) append(x, s, after = idx - 1L),
             dgrad = function(x, s, gg) gg[-idx],
             split = function(g) g[-idx], s_of = function(g) g[idx])
      })
    }
  }

  # beta, kept harmonics only
  keep <- spec$keep_k
  L <- length(keep)
  ib <- spec$i_beta                       # gstar positions of v_2..v_L
  n_nb <- length(par) - length(ib)
  v_hat <- c(0, par[ib])
  b_hat <- exp(v_hat) / sum(exp(v_hat))
  if (L >= 2) {
    for (a in seq_len(L)) {
      comps[[length(comps) + 1L]] <- local({
        a <- a
        others <- setdiff(seq_len(L), c(1L, a))   # free v positions (1..L)
        if (a >= 2) {
          full <- function(x, s) {
            v <- numeric(L)
            v[others] <- x[-seq_len(n_nb)]
            v[a] <- s + log(sum(exp(v[-a])))       # v[1] = 0 is included
            append(x[seq_len(n_nb)], v[-1], after = min(ib) - 1L)
          }
          dgrad <- function(x, s, gg) {
            gv <- c(0, gg[ib])
            v <- c(0, full(x, s)[ib])
            S <- sum(exp(v[-a]))
            c(gg[-ib], gv[others] + gv[a] * exp(v[others]) / S)
          }
          x0 <- c(par[-ib], v_hat[others])
          split <- function(g) c(g[-ib], c(0, g[ib])[others])
        } else {
          wfree <- if (L > 2) 3:L else integer(0)
          full <- function(x, s) {
            w <- numeric(L)
            w[wfree] <- x[-seq_len(n_nb)]
            w2 <- w[2:L]
            cc <- log((1 - stats::plogis(s)) / stats::plogis(s)) -
              log(sum(exp(w2)))
            append(x[seq_len(n_nb)], w2 + cc, after = min(ib) - 1L)
          }
          dgrad <- function(x, s, gg) {
            gv <- gg[ib]                           # gradient in v_2..v_L
            v2 <- full(x, s)[ib]
            pi_w <- exp(v2) / sum(exp(v2))
            gw <- gv - sum(gv) * pi_w              # d/dw_l over l = 2..L
            c(gg[-ib], gw[-1])                     # w_2 fixed
          }
          x0 <- c(par[-ib], v_hat[wfree] - v_hat[2])
          split <- function(g) {
            v <- c(0, g[ib])
            c(g[-ib], v[wfree] - v[2])
          }
        }
        s_of <- function(g) {
          v <- c(0, g[ib])
          stats::qlogis(exp(v[a]) / sum(exp(v)))
        }
        list(family = "beta", row = keep[a] + 1L,
             s_hat = stats::qlogis(b_hat[a]), x0 = x0, full = full,
             dgrad = dgrad, split = split, s_of = s_of)
      })
    }
  }
  list(comps = comps, obj = obj, grad = grad, F_hat = obj(par), n = N - 1,
       spec = spec, keep = keep)
}

# The statistic (N - 1) (F_prof(s) - F_hat), floored at 0, minimizing from
# the component's estimate start (unless `include_x0` is FALSE) and each of
# `starts`; returns the statistic, the unfloored statistic `raw` and the
# minimizing free parameters. A start whose minimization throws (a singular
# solve inside the objective) is skipped, not fatal (RR27 rec 7).
m167_profile_stat <- function(setup, comp, s, starts = list(),
                              use_gradient = TRUE, include_x0 = TRUE) {
  best <- NULL
  for (st in c(if (include_x0) list(comp$x0), starts)) {
    fit <- tryCatch(stats::nlminb(
      start = st,
      objective = function(x) setup$obj(comp$full(x, s)),
      gradient = if (use_gradient) {
        function(x) comp$dgrad(x, s, setup$grad(comp$full(x, s)))
      },
      control = list(rel.tol = 1e-10, iter.max = 300L, eval.max = 600L)
    ), error = function(e) NULL)
    if (is.null(fit) || !is.finite(fit$objective)) next
    if (is.null(best) || fit$objective < best$objective) best <- fit
  }
  if (is.null(best)) stop("m167_profile_stat: every start failed")
  raw <- setup$n * (best$objective - setup$F_hat)
  list(stat = max(raw, 0), raw = raw, par = best$par)
}

# The two-start P1 of the first re-run (each evaluation from the estimate and
# the nearest warm start, anchored to the engine's F-hat), which M167's third
# review found too narrow. It is kept only as the planted defect of the
# endpoint check in devel/m167-interval-checks.R; the study uses
# m167_profile_ci() below.
m167_profile_ci_twostart <- function(engine, R, N, level = 0.95) {
  spec <- engine$spec
  p <- spec$p
  crit <- stats::qchisq(level, 1)
  setup <- m167_profile_setup(engine, R, N)
  out <- m167_empty_ci(p, spec$m + 1L)
  out$beta[] <- 0                         # removed weights: [0, 0]
  if (length(setup$keep) == 1L) {         # harmonic 0 alone: beta_0 = 1
    out$beta[setup$keep + 1L, ] <- 1
  }

  # The profile is followed outward from the estimate in growing steps
  # (.05 * 1.5^j on the logit scale) until the statistic crosses the cutoff
  # or the step passes s_far; the crossing is then root-found inside that
  # last bracket. Each evaluation keeps the lower of the estimate start and
  # the solution at the nearest s already evaluated on this side.
  side_root <- function(comp, s_far) {
    s_hat <- comp$s_hat
    cache_s <- s_hat
    cache_x <- list(comp$x0)
    lr <- function(s) {
      near <- cache_x[[which.min(abs(cache_s - s))]]
      ev <- m167_profile_stat(setup, comp, s, starts = list(near))
      cache_s <<- c(cache_s, s)
      cache_x[[length(cache_x) + 1L]] <<- ev$par
      ev$stat - crit
    }
    dir <- sign(s_far - s_hat)
    s_prev <- s_hat
    step <- 0.05
    repeat {
      s_next <- s_prev + dir * step
      if (dir * (s_next - s_far) >= 0) {
        if (lr(s_far) < 0) return(NA_real_)      # no crossing: natural bound
        s_next <- s_far
        break
      }
      if (lr(s_next) >= 0) break
      s_prev <- s_next
      step <- step * 1.5
    }
    stats::uniroot(lr, sort(c(s_prev, s_next)), tol = 1e-4)$root
  }
  for (comp in setup$comps) {
    ci <- tryCatch({
      lo <- side_root(comp, min(comp$s_hat - 1, -14))
      hi <- side_root(comp, max(comp$s_hat + 1, 14))
      c(if (is.na(lo)) 0 else stats::plogis(lo),
        if (is.na(hi)) 1 else stats::plogis(hi))
    }, error = function(e) c(NA_real_, NA_real_))
    out[[comp$family]][comp$row, ] <- ci
  }
  out
}

# P1 as the study runs it (M167-D6, after RR27). The pre-registered statistic
# needs F_hat to be the global minimum of F and F_prof(t) the global minimum
# at each t (RR27 Q2), and no finite search guarantees either (RR27 Q3). So
# P1 searches in four bounded steps and reports a component it cannot verify
# within its caps as having no interval. T units below are (N - 1) F.
#   1. Anchor pass: free refits from the engine optimum, from `n_disp`
#      dispersed starts (sd .7, half around the optimum, half around the
#      battery's theory start g0), from a wall probe per scale (u_j set to
#      logit .99) and from wide angle probes (each free angle +90, -90 and 180
#      degrees). The engine optimum and the lowest other distinct minima
#      within 10 T units of the lowest, at most `max_branches` in all, are the
#      branches; the lowest is F_hat (the anchor).
#   2. Each profile evaluation starts from the nearest solution already found
#      on this side and from every branch with the component set to s, and
#      keeps the lowest.
#   3. Each side starts at the anchor's component value. At a finite crossing
#      the statistic is evaluated again from K dispersed starts at sd .5 and K
#      at sd .8 around the path solution and from the wall probes; a point
#      more than .02 below the path's statistic there, and under the cutoff,
#      joins the branches and the search continues outward from the crossing.
#   4. After a verified crossing, an outward scan (logit +.25, .5, 1, 2, 4, 8,
#      inside the far point) looks for a re-entry under the cutoff; a re-entry
#      is followed to its own crossing, so the interval is the hull of the set
#      found, and the re-entry is counted. For the same reason each side first
#      evaluates its far point, and the natural bound is the endpoint when the
#      statistic is under the cutoff there.
# Steps 3 and 4 share a cap of `max_rounds` continuations per side, and each
# component a cap of `run_cap` minimizations; a component that hits either has
# no interval and is counted as unverified. A profile point more than .01
# below F_hat means the anchor missed a lower minimum: it is refit freely,
# joins the branches, and P1 starts over, at most `max_restarts` times, after
# which every component is unverified. P1 draws its starts from its own `seed`
# and leaves the caller's random stream as it found it.
m167_profile_ci <- function(engine, R, N, level = 0.95, seed = 1L,
                            n_disp = 10L, K = 4L, max_branches = 4L,
                            max_rounds = 3L, run_cap = 300L,
                            max_restarts = 2L, rel_tol = 1e-8) {
  genv <- globalenv()
  if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = genv, inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = genv))
  } else {
    on.exit(rm(".Random.seed", envir = genv))
  }
  set.seed(seed)
  spec <- engine$spec
  p <- spec$p
  crit <- stats::qchisq(level, 1)
  setup <- m167_profile_setup(engine, R, N)
  F_eng <- setup$F_hat
  lib <- m167_p1_anchor(engine, R, setup, n_disp, max_branches)
  runs <- lib$runs
  restarts <- 0L
  repeat {
    setup$F_hat <- min(lib$F)
    res <- m167_p1_pass(setup, lib, crit, K, max_rounds, run_cap, rel_tol)
    runs <- runs + res$runs
    if (is.null(res$undercut) || restarts >= max_restarts) break
    # the anchor missed a lower minimum: refit it freely and start over
    fit <- tryCatch(cpm_optimize_one(res$undercut$g, R, spec),
                    error = function(e) NULL)
    runs <- runs + 1L
    if (!is.null(fit) && is.finite(fit$F) && fit$F < res$undercut$F) {
      lib <- m167_p1_add(lib, fit$par, fit$F, max_branches + restarts + 1L)
    } else {
      lib <- m167_p1_add(lib, res$undercut$g, res$undercut$F,
                         max_branches + restarts + 1L)
    }
    restarts <- restarts + 1L
  }
  failed_anchor <- !is.null(res$undercut)

  out <- m167_empty_ci(p, spec$m + 1L)
  out$beta[] <- 0                         # removed weights: [0, 0]
  if (length(setup$keep) == 1L) {         # harmonic 0 alone: beta_0 = 1
    out$beta[setup$keep + 1L, ] <- 1
  }
  comp_diag <- lapply(seq_along(setup$comps), function(j) {
    comp <- setup$comps[[j]]
    r <- res$comps[[j]]
    unverified <- failed_anchor || is.null(r) || r$unverified
    ci <- if (unverified) c(NA_real_, NA_real_) else r$ci
    est <- stats::plogis(comp$s_hat)
    data.frame(family = comp$family, row = comp$row, lower = ci[1],
               upper = ci[2], unverified = unverified,
               reentry = if (is.null(r)) NA_integer_ else r$reentry,
               rounds = if (is.null(r)) NA_integer_ else r$rounds,
               runs = if (is.null(r)) NA_integer_ else r$runs,
               est_inside = !unverified && ci[1] <= est && est <= ci[2])
  })
  comp_diag <- do.call(rbind, comp_diag)
  for (j in seq_len(nrow(comp_diag))) {
    out[[comp_diag$family[j]]][comp_diag$row[j], ] <-
      c(comp_diag$lower[j], comp_diag$upper[j])
  }
  # the anchor's and each finite endpoint's parameter vectors, so a check can
  # evaluate F where P1 says it is
  end_g <- lapply(seq_along(setup$comps), function(j) {
    r <- res$comps[[j]]
    if (failed_anchor || is.null(r) || r$unverified) NULL else r$end_g
  })
  out$diag <- list(anchor_gap = setup$n * (F_eng - setup$F_hat),
                   n_branches = length(lib$g), restarts = restarts,
                   failed_anchor = failed_anchor, runs = runs,
                   components = comp_diag,
                   anchor_g = lib$g[[which.min(lib$F)]], end_g = end_g)
  out
}

# Step 1: the anchor pass. Returns the branches (full parameter vectors), their
# F, the index of the lowest and the number of free refits run.
m167_p1_anchor <- function(engine, R, setup, n_disp, max_branches) {
  spec <- engine$spec
  q <- length(engine$par)
  theta_theory <- engine$theta_theory * pi / 180
  sv <- cpm_start_values(R, theta_theory, length(engine$beta) - 1L)
  g0 <- cpm_pack(theta_theory, sv$zeta, sv$beta, spec)
  starts <- c(
    lapply(seq_len(ceiling(n_disp / 2)),
           function(k) engine$par + stats::rnorm(q, 0, 0.7)),
    lapply(seq_len(floor(n_disp / 2)),
           function(k) g0 + stats::rnorm(q, 0, 0.7)),
    lapply(spec$i_zeta, function(i) {
      g <- engine$par
      g[i] <- stats::qlogis(0.99)
      g
    }),
    unlist(lapply(spec$i_angle, function(i) {
      lapply(c(90, -90, 180), function(d) {
        g <- engine$par
        g[i] <- g[i] + d * pi / 180
        g
      })
    }), recursive = FALSE)
  )
  lib <- list(g = list(engine$par), F = setup$F_hat, runs = 0L, n = setup$n)
  found <- lapply(starts, function(st) {
    tryCatch(cpm_optimize_one(st, R, spec), error = function(e) NULL)
  })
  lib$runs <- length(starts)
  found <- found[!vapply(found, function(f) is.null(f) || !is.finite(f$F),
                         logical(1))]
  if (length(found)) {
    Fs <- vapply(found, `[[`, numeric(1), "F")
    for (j in order(Fs)) {
      lib <- m167_p1_add(lib, found[[j]]$par, Fs[j], max_branches)
    }
  }
  lib
}

# Adds a minimum to the branches when it is distinct (more than .001 T units
# from every branch), within 10 T units of the lowest, and the cap allows;
# the engine optimum (branch 1) is always kept.
m167_p1_add <- function(lib, g, F, cap) {
  n <- lib$n
  if (any(abs(n * (lib$F - F)) <= 1e-3)) return(lib)
  if (n * (F - min(lib$F, F)) > 10) return(lib)
  lib$g[[length(lib$g) + 1L]] <- g
  lib$F <- c(lib$F, F)
  # drop branches now more than 10 T units above the lowest, then keep the
  # engine optimum and the lowest others up to the cap
  keep <- c(1L, setdiff(order(lib$F), 1L))
  keep <- keep[keep == 1L | n * (lib$F[keep] - min(lib$F)) <= 10]
  keep <- keep[seq_len(min(length(keep), cap))]
  keep <- sort(keep)
  lib$g <- lib$g[keep]
  lib$F <- lib$F[keep]
  lib
}

# Steps 2 to 4 over every component, with F_hat fixed at setup$F_hat. Returns
# per component its interval (natural bounds 0 and 1 where the statistic stays
# under the cutoff), whether it is unverified, its continuation rounds,
# re-entries and minimizations, plus the total minimizations; or, when a
# profile point undercuts F_hat, that point (`undercut`) and no intervals.
m167_p1_pass <- function(setup, lib, crit, K, max_rounds, run_cap, rel_tol) {
  spec <- setup$spec
  ia <- which.min(lib$F)
  total <- 0L
  comps <- vector("list", length(setup$comps))
  for (j in seq_along(setup$comps)) {
    comp <- setup$comps[[j]]
    r <- tryCatch(m167_p1_component(setup, comp, lib, ia, crit, K,
                                    max_rounds, run_cap, rel_tol),
                  m167_undercut = function(e) e)
    if (inherits(r, "m167_undercut")) {
      return(list(comps = NULL, runs = total + r$runs,
                  undercut = list(g = r$g, F = r$F)))
    }
    total <- total + r$runs
    comps[[j]] <- r
  }
  list(comps = comps, runs = total, undercut = NULL)
}

m167_p1_component <- function(setup, comp, lib, ia, crit, K, max_rounds,
                              run_cap, rel_tol) {
  spec <- setup$spec
  branches <- lapply(lib$g, comp$split)
  s_a <- comp$s_of(lib$g[[ia]])
  x_a <- comp$split(lib$g[[ia]])
  runs <- 0L
  signal <- function(class, ...) {
    stop(structure(class = c(class, "error", "condition"),
                   list(message = class, call = NULL, runs = runs, ...)))
  }
  eval_at <- function(s, starts) {
    best <- NULL
    seen <- list()
    for (st in starts) {
      # a start equal to one already run would repeat its minimization
      if (any(vapply(seen, function(u) max(abs(u - st)) < 1e-8, logical(1)))) {
        next
      }
      seen[[length(seen) + 1L]] <- st
      runs <<- runs + 1L
      fit <- tryCatch(stats::nlminb(
        start = st,
        objective = function(x) setup$obj(comp$full(x, s)),
        gradient = function(x) comp$dgrad(x, s, setup$grad(comp$full(x, s))),
        control = list(rel.tol = rel_tol, iter.max = 300L, eval.max = 600L)
      ), error = function(e) NULL)
      if (is.null(fit) || !is.finite(fit$objective)) next
      if (is.null(best) || fit$objective < best$objective) best <- fit
    }
    if (is.null(best)) signal("m167_unverified")
    raw <- setup$n * (best$objective - setup$F_hat)
    if (raw < -0.01) {
      signal("m167_undercut", g = comp$full(best$par, s), F = best$objective)
    }
    if (runs > run_cap) signal("m167_unverified")
    list(stat = max(raw, 0), par = best$par)
  }
  # wall probes around a full parameter vector, for every scale but the
  # component's own
  wall_starts <- function(g) {
    own <- if (comp$family == "zeta") spec$i_zeta[comp$row] else NA
    lapply(setdiff(spec$i_zeta, own), function(i) {
      g[i] <- stats::qlogis(0.99)
      comp$split(g)
    })
  }
  side <- function(dir, s_far) {
    cache_s <- s_a
    cache_f <- -crit
    cache_x <- list(x_a)
    extra <- list()
    nearest <- function(s) cache_x[[order(abs(cache_s - s), cache_f)[1]]]
    lr <- function(s, more = list()) {
      ev <- eval_at(s, c(list(nearest(s)), branches, extra, more))
      f <- ev$stat - crit
      cache_s <<- c(cache_s, s)
      cache_f <<- c(cache_f, f)
      cache_x[[length(cache_x) + 1L]] <<- ev$par
      f
    }
    rounds <- 0L
    reentry <- 0L
    # The interval is the hull of the set found, so a far point under the
    # cutoff makes the natural bound the endpoint, whatever lies between.
    if (lr(s_far) < 0) {
      return(list(s = NA_real_, rounds = rounds, reentry = reentry))
    }
    s_prev <- s_a
    f_prev <- -crit                     # the anchor: statistic 0
    repeat {
      step <- 0.05
      natural <- FALSE
      repeat {
        s_next <- s_prev + dir * step
        if (dir * (s_next - s_far) >= 0) {
          s_next <- s_far
          f_next <- lr(s_far)
          natural <- f_next < 0
          break
        }
        f_next <- lr(s_next)
        if (f_next >= 0) break
        s_prev <- s_next
        f_prev <- f_next
        step <- step * 1.5
      }
      if (natural) {
        return(list(s = NA_real_, rounds = rounds, reentry = reentry))
      }
      lower_first <- s_prev < s_next
      s_star <- stats::uniroot(
        lr, sort(c(s_prev, s_next)),
        f.lower = if (lower_first) f_prev else f_next,
        f.upper = if (lower_first) f_next else f_prev,
        tol = 1e-4
      )$root
      # step 3: verify the crossing against the path's own statistic there
      f_path <- lr(s_star)
      x_star <- nearest(s_star)
      q <- length(x_star)
      vs <- c(lapply(seq_len(K), function(k) x_star + stats::rnorm(q, 0, 0.5)),
              lapply(seq_len(K), function(k) x_star + stats::rnorm(q, 0, 0.8)),
              wall_starts(comp$full(x_star, s_star)))
      f_v <- lr(s_star, more = vs)
      if (f_v < f_path - 0.02 && f_v < 0) {
        rounds <- rounds + 1L
        if (rounds > max_rounds) signal("m167_unverified")
        extra[[length(extra) + 1L]] <- cache_x[[length(cache_x)]]
        s_prev <- s_star
        f_prev <- f_v
        next
      }
      # step 4: scan outward for a re-entry under the cutoff
      re <- NA_real_
      for (d in c(0.25, 0.5, 1, 2, 4, 8)) {
        s <- s_star + dir * d
        if (dir * (s - s_far) > 0) break
        f_s <- lr(s)
        if (f_s < 0) {
          re <- s
          break
        }
      }
      if (is.na(re)) {
        return(list(s = s_star, rounds = rounds, reentry = reentry,
                    g = comp$full(nearest(s_star), s_star)))
      }
      reentry <- reentry + 1L
      rounds <- rounds + 1L
      if (rounds > max_rounds) signal("m167_unverified")
      s_prev <- re
      f_prev <- f_s
    }
  }
  res <- tryCatch({
    lo <- side(-1, min(s_a - 1, -14))
    hi <- side(1, max(s_a + 1, 14))
    list(ci = c(if (is.na(lo$s)) 0 else stats::plogis(lo$s),
                if (is.na(hi$s)) 1 else stats::plogis(hi$s)),
         unverified = FALSE, rounds = lo$rounds + hi$rounds,
         reentry = lo$reentry + hi$reentry, end_g = list(lo$g, hi$g))
  }, m167_unverified = function(e) {
    list(ci = c(NA_real_, NA_real_), unverified = TRUE, rounds = NA_integer_,
         reentry = NA_integer_)
  })
  res$runs <- runs
  res
}

m167_empty_ci <- function(p, nb) {
  list(angle = matrix(NA_real_, p, 2), zeta = matrix(NA_real_, p, 2),
       beta = matrix(NA_real_, nb, 2))
}

# ---- scoring -------------------------------------------------------------------

# One interval per component -> side of the truth: 0 covered, -1 the interval
# lies below the truth, +1 above, NA no interval. For angles "above" means the
# interval lies counterclockwise of the truth: the signed shortest rotation
# from the truth to the arc's midpoint is positive. Angle arcs run
# counterclockwise from lci to uci, which also reads a Wald interval correctly
# (its endpoints are unwrapped around the estimate).
m167_side_linear <- function(ci, truth) {
  out <- ifelse(ci[, 1] <= truth & truth <= ci[, 2], 0,
                ifelse(ci[, 1] > truth, 1, -1))
  out[is.na(ci[, 1]) | is.na(ci[, 2])] <- NA
  out
}

m167_side_angle <- function(ci, truth) {
  lci <- ci[, 1] %% 360
  span <- (ci[, 2] - ci[, 1]) %% 360
  # a Wald interval wider than a full turn covers every direction
  full <- !is.na(ci[, 1]) & (ci[, 2] - ci[, 1]) >= 360
  inside <- ((truth - lci) %% 360) <= span
  mid <- lci + span / 2
  rot <- ((mid - truth + 180) %% 360) - 180
  out <- ifelse(inside | full, 0, ifelse(rot > 0, 1, -1))
  out[is.na(ci[, 1]) | is.na(ci[, 2])] <- NA
  out
}
