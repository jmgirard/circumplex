# M170: helpers for the engine-estimate study, shared by
# devel/m170-engine-estimate.R and devel/m170-engine-estimate-checks.R.
# Prototypes only: nothing here is exported or called by the package. Run
# under devtools::load_all() so the CPM internals (cpm_engine,
# cpm_optimize_one, cpm_polish_beta, cpm_canonicalize, ...) resolve.
#
# The estimator definitions these functions implement (E, W, H, H_K) are
# fixed in the pre-registration block of devel/m170-engine-estimate.R; read
# that first. Every estimator here fits variant A with unit scaling.

# ---- E: the shipped engine ------------------------------------------------------

# E is cpm_engine() as shipped; an error is returned as NULL (an excluded fit).
m170_engine_e <- function(R, angles, m, reference = 1L) {
  tryCatch(suppressWarnings(cpm_engine(R, angles = angles, m = m,
                                       variant = "A", reference = reference)),
           error = function(e) NULL)
}

# The full (unpolished) spec cpm_engine() builds before its polish, with the
# reference and theory angles filled in as cpm_engine() fills them.
m170_full_spec <- function(eng, m) {
  p <- length(eng$zeta)
  spec <- cpm_spec(p, m, "A", eng$reference, scaling = "unit")
  theta_theory <- eng$theta_theory * pi / 180
  spec$theta_ref_val <- theta_theory[eng$reference]
  spec$theta_fixed <- theta_theory
  spec
}

# E's estimate in the full layout. A weight the polish removed (beta = 0) has
# no finite logit, so every weight is floored at 1e-4 and renormalized. The
# communality logits come from E's own parameter vector (the block is
# position-stable across the polish), clipped to [-30, 30]: the engine can
# drive a logit past 37, where plogis() returns exactly 1 and qlogis() of the
# reported communality is Inf, which would make every start built from this
# point infinite (RR28). The point is a start, not an estimate, so neither
# step moves anything reported.
m170_base_par <- function(eng, spec) {
  b <- pmax(eng$beta, 1e-4)
  g <- cpm_pack(eng$theta_rad_unwrapped, eng$zeta, b / sum(b), spec)
  g[spec$i_zeta] <- pmin(pmax(eng$par[eng$spec$i_zeta], -30), 30)
  g
}

# The battery's theory start g0 (cpm_engine()'s first start).
m170_theory_par <- function(R, eng, spec, m) {
  theta_theory <- eng$theta_theory * pi / 180
  sv <- cpm_start_values(R, theta_theory, m)
  cpm_pack(theta_theory, sv$zeta, sv$beta, spec)
}

# ---- W's deterministic extra starts -----------------------------------------------

# Around E's estimate (full layout): one communality-wall start per scale (that
# scale's logit set to logit .99), one all-communality-level start (every
# scale's logit set to logit .99), one start per free angle and shift (+90,
# -90, 180 degrees), one start per pair of free angles with the two values
# swapped, and each swap again with the wall on one of the two swapped scales.
# Around g0: one communality-wall start per scale. With p = 8: 8 + 1 + 21 +
# 21 + 42 + 8 = 101 starts. No random numbers, so W is the same on every run.
m170_w_starts <- function(base, g0, spec) {
  wall <- stats::qlogis(0.99)
  per_scale <- lapply(spec$i_zeta, function(i) {
    g <- base
    g[i] <- wall
    g
  })
  all_level <- base
  all_level[spec$i_zeta] <- wall
  shifts <- unlist(lapply(spec$i_angle, function(i) {
    lapply(c(90, -90, 180), function(d) {
      g <- base
      g[i] <- g[i] + d * pi / 180
      g
    })
  }), recursive = FALSE)
  pairs <- utils::combn(spec$i_angle, 2, simplify = FALSE)
  swap <- function(g, ij) {
    g[ij] <- g[rev(ij)]
    g
  }
  swaps <- lapply(pairs, function(ij) swap(base, ij))
  # a free angle's position in the angle block is its scale's position among
  # spec$free_pos, which names the scale whose communality goes to the wall
  swap_walls <- unlist(lapply(pairs, function(ij) {
    lapply(ij, function(i) {
      g <- swap(base, ij)
      g[spec$i_zeta[spec$free_pos[i]]] <- wall
      g
    })
  }), recursive = FALSE)
  g0_walls <- lapply(spec$i_zeta, function(i) {
    g <- g0
    g[i] <- wall
    g
  })
  c(per_scale, list(all_level), shifts, swaps, swap_walls, g0_walls)
}

# ---- the saturation rule's independent family -------------------------------------

# Starts drawn with no centre: each free angle uniform on the circle, each
# communality uniform on [.3, .99], and each free weight logit standard
# normal. Built for m170_estimators()'s `extra` argument, so it draws from
# the estimators' own seeded stream after H's starts.
m170_uniform_starts <- function(n) {
  force(n)
  function(base, g0, spec) {
    lapply(seq_len(n), function(k) {
      g <- numeric(length(base))
      g[spec$i_angle] <- stats::runif(length(spec$i_angle), 0, 2 * pi)
      g[spec$i_zeta] <- stats::qlogis(stats::runif(length(spec$i_zeta),
                                                   0.3, 0.99))
      g[spec$i_beta] <- stats::rnorm(length(spec$i_beta))
      g
    })
  }
}

# ---- running a start set ----------------------------------------------------------

# One free refit per start with the engine's own nlminb settings. A start whose
# minimization throws (a singular solve inside the objective) is skipped and
# recorded as F = NA, not fatal (RR27 beyond the brief, item 3). Returns each
# start's F, its parameter vector, and the CPU seconds spent.
m170_run_starts <- function(starts, R, spec) {
  t0 <- proc.time()[["user.self"]] + proc.time()[["sys.self"]]
  runs <- lapply(starts, function(st) {
    tryCatch(cpm_optimize_one(st, R, spec), error = function(e) NULL)
  })
  t1 <- proc.time()[["user.self"]] + proc.time()[["sys.self"]]
  Fs <- vapply(runs, function(r) {
    if (is.null(r) || !is.finite(r$F)) NA_real_ else r$F
  }, numeric(1))
  pars <- lapply(runs, function(r) if (is.null(r)) NULL else r$par)
  list(F = Fs, par = pars, cpu = t1 - t0)
}

# ---- finishing a point the way the engine finishes its own -----------------------

# The engine's post-optimum steps on a full-layout point: the beta boundary
# polish (cpm_polish_beta()), then reflection canonicalization, then the
# report quantities. Returns an engine-like list that cpm_analytic_se() and
# the metrics below accept.
m170_finish <- function(par, F, R, spec, eng) {
  polished <- cpm_polish_beta(list(par = par, F = F), R, spec)
  fit <- polished$fit
  rspec <- polished$spec
  theta_theory <- eng$theta_theory * pi / 180
  canon <- cpm_canonicalize(fit$par, rspec, theta_theory)
  nat <- cpm_unpack(canon$par, rspec)
  gnorm <- max(abs(cpm_gradient(fit$par, R, rspec)))
  theta_deg <- as.numeric(as_degree(as_radian(nat$theta %% (2 * pi))))
  list(theta = theta_deg, theta_rad_unwrapped = nat$theta,
       theta_theory = eng$theta_theory, zeta = nat$zeta, beta = nat$beta,
       F = fit$F, df = rspec$df, m = max(rspec$keep_k),
       reference = eng$reference, gradient_norm = gnorm,
       grad_ok = gnorm <= 1e-6 * max(1, abs(fit$F)),
       removed_harmonics = polished$removed, par = canon$par, spec = rspec)
}

# ---- the three estimators ---------------------------------------------------------

# W and H from one accepted E fit with N - 1 = `n1`. W is the lowest of E and
# W's deterministic starts; H is the lowest of E, W's starts and 2K random
# starts from each family in `random` (a centre, "E" for E's estimate or
# "g0", and an sd; each start is the centre plus independent N(0, sd^2) draws
# on every coordinate of the engine's unconstrained parameters). H_K is the
# same with only the first K random starts of each family. A refit replaces
# E only when it lies more than `thresh` T units below E's F, so the fits
# where an estimator differs from E are exactly the fits where it finds a
# lower discrepancy (RR28). The random starts come from `seed`, and the
# caller's random stream is restored afterwards. `extra` adds named start
# families that are run and recorded but enter neither W nor H (the
# saturation rule's independent family); for each, the result holds its
# lowest F, its hit rate (the share of its runs within `thresh` T units of
# H's lowest F) and its finished best point.
m170_estimators <- function(R, eng, m, K, seed, n1, thresh = 0.01,
                            random = list(list(centre = "g0", sd = 0.7),
                                          list(centre = "g0", sd = 1.2)),
                            extra = list()) {
  genv <- globalenv()
  if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = genv, inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = genv))
  } else {
    on.exit(rm(".Random.seed", envir = genv))
  }
  spec <- m170_full_spec(eng, m)
  base <- m170_base_par(eng, spec)
  g0 <- m170_theory_par(R, eng, spec, m)
  q <- length(base)

  w_starts <- m170_w_starts(base, g0, spec)
  w <- m170_run_starts(w_starts, R, spec)

  set.seed(seed)
  rs <- lapply(random, function(f) {
    centre <- if (f$centre == "E") base else g0
    lapply(seq_len(2L * K), function(k) centre + stats::rnorm(q, 0, f$sd))
  })
  h <- lapply(rs, m170_run_starts, R = R, spec = spec)

  extra_runs <- lapply(extra, function(make) {
    m170_run_starts(make(base = base, g0 = g0, spec = spec), R, spec)
  })

  lowest <- function(sets) {
    Fs <- unlist(lapply(sets, `[[`, "F"))
    pars <- unlist(lapply(sets, `[[`, "par"), recursive = FALSE)
    j <- which.min(Fs)
    if (length(j) == 0) return(list(par = NULL, F = Inf))
    list(par = pars[[j]], F = Fs[j])
  }
  pick <- function(sets) {
    pt <- lowest(sets)
    if (!(n1 * (eng$F - pt$F) > thresh)) return(NULL)
    pt
  }
  first_k <- function(s, k) list(F = s$F[seq_len(k)], par = s$par[seq_len(k)])
  h_sets <- c(list(w), h)
  hk_sets <- c(list(w), lapply(h, first_k, k = K))

  w_pt <- pick(list(w))
  h_pt <- pick(h_sets)
  # the saturation rule's S1 reads both lowest values before any polish
  F_H_raw <- min(eng$F, lowest(h_sets)$F)
  F_HK_raw <- min(eng$F, lowest(hk_sets)$F)


  # A point that fails to finish (an error in the polish or the
  # canonicalization) leaves that estimator at E, and is counted.
  finish <- function(pt) {
    if (is.null(pt)) return(NULL)
    tryCatch(m170_finish(pt$par, pt$F, R, spec, eng), error = function(e) {
      structure(list(), class = "m170_finish_error")
    })
  }
  W <- finish(w_pt)
  H <- finish(h_pt)
  finish_error <- c(W = inherits(W, "m170_finish_error"),
                    H = inherits(H, "m170_finish_error"))
  if (finish_error[["W"]]) W <- NULL
  if (finish_error[["H"]]) H <- NULL
  extra_out <- lapply(extra_runs, function(s) {
    best <- lowest(list(s))
    list(F = best$F,
         hit_rate = mean(n1 * (s$F - F_H_raw) <= thresh, na.rm = TRUE),
         point = if (is.finite(best$F)) {
           tryCatch(m170_finish(best$par, best$F, R, spec, eng),
                    error = function(e) NULL)
         })
  })
  list(
    W = W, H = H, finish_error = finish_error,
    F_E = eng$F,
    # W's and H's discrepancies are read after the polish, as E's is
    F_W = if (is.null(W)) eng$F else W$F,
    F_H = if (is.null(H)) eng$F else H$F,
    F_H_raw = F_H_raw, F_HK_raw = F_HK_raw,
    extra = extra_out,
    n_failed = c(W = sum(is.na(w$F)),
                 H = sum(vapply(h, function(s) sum(is.na(s$F)), numeric(1)))),
    cpu = c(W = w$cpu, H = w$cpu + sum(vapply(h, `[[`, numeric(1), "cpu")),
            extra = sum(vapply(extra_runs, `[[`, numeric(1), "cpu"))),
    w_starts = w_starts
  )
}

# ---- metrics on one estimate ----------------------------------------------------

# Number of scales at the Heywood wall (communality above .995).
m170_n_heywood <- function(est) sum(est$zeta > 0.995)

# The cyclic order of the scales, read counterclockwise from the reference on
# the canonicalized solution: two estimates have the same order exactly when
# these vectors are equal.
m170_cyclic_order <- function(est) {
  rel <- (est$theta - est$theta[est$reference]) %% 360
  order(rel)
}

# The signed circular difference (degrees, in (-180, 180]) of each free angle
# from the truth, and the plain differences of communalities and weights.
m170_errors <- function(est, truth_angle, truth_zeta, truth_beta, free) {
  d <- ((est$theta[free] - truth_angle[free] + 180) %% 360) - 180
  d[d == -180] <- 180
  list(angle = d, zeta = est$zeta - truth_zeta, beta = est$beta - truth_beta)
}
