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
             dgrad = function(x, s, gg) gg[-idx])
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
        }
        list(family = "beta", row = keep[a] + 1L,
             s_hat = stats::qlogis(b_hat[a]), x0 = x0, full = full,
             dgrad = dgrad)
      })
    }
  }
  list(comps = comps, obj = obj, grad = grad, F_hat = obj(par), n = N - 1,
       spec = spec, keep = keep)
}

# The statistic (N - 1) (F_prof(s) - F_hat), floored at 0, minimizing from
# the component's estimate start and each of `starts`; returns the statistic
# and the minimizing free parameters.
m167_profile_stat <- function(setup, comp, s, starts = list(),
                              use_gradient = TRUE) {
  best <- NULL
  for (st in c(list(comp$x0), starts)) {
    fit <- stats::nlminb(
      start = st,
      objective = function(x) setup$obj(comp$full(x, s)),
      gradient = if (use_gradient) {
        function(x) comp$dgrad(x, s, setup$grad(comp$full(x, s)))
      },
      control = list(rel.tol = 1e-10, iter.max = 300L, eval.max = 600L)
    )
    if (is.null(best) || fit$objective < best$objective) best <- fit
  }
  list(stat = max(setup$n * (best$objective - setup$F_hat), 0),
       par = best$par)
}

m167_profile_ci <- function(engine, R, N, level = 0.95) {
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
