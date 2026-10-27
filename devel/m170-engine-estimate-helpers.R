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
# no finite logit, so every weight is floored at 1e-4 and renormalized; the
# point is a start, not an estimate, so the floor moves nothing reported.
m170_base_par <- function(eng, spec) {
  b <- pmax(eng$beta, 1e-4)
  cpm_pack(eng$theta_rad_unwrapped, eng$zeta, b / sum(b), spec)
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
# scale's logit set to logit .99), and one start per free angle and shift
# (+90, -90, 180 degrees). No random numbers, so W is the same on every run.
m170_w_starts <- function(base, spec) {
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
  c(per_scale, list(all_level), shifts)
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

# W and H from one accepted E fit. W is the lowest of E and W's deterministic
# starts; H is the lowest of E, W's starts and 2K random starts at sd .7 around
# E's estimate and 2K around g0, all in the engine's unconstrained
# coordinates. H_K is the same with only the first K random starts per centre.
# A start replaces E only when its refit lies more than 1e-8 below E's F (the
# engine's own reproduction tolerance), so a numerically equal point never
# swaps the reported solution. The random starts come from `seed`, and the
# caller's random stream is restored afterwards. `extra` adds named start
# families that are run and recorded but enter neither W nor H (the
# saturation rule's independent family).
m170_estimators <- function(R, eng, m, K, seed, sd = 0.7, extra = list()) {
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

  w_starts <- m170_w_starts(base, spec)
  w <- m170_run_starts(w_starts, R, spec)

  set.seed(seed)
  rs_e <- lapply(seq_len(2L * K), function(k) base + stats::rnorm(q, 0, sd))
  rs_g <- lapply(seq_len(2L * K), function(k) g0 + stats::rnorm(q, 0, sd))
  h_e <- m170_run_starts(rs_e, R, spec)
  h_g <- m170_run_starts(rs_g, R, spec)

  extra_runs <- lapply(extra, function(make) {
    m170_run_starts(make(base = base, g0 = g0, spec = spec), R, spec)
  })

  pick <- function(sets) {
    Fs <- unlist(lapply(sets, `[[`, "F"))
    pars <- unlist(lapply(sets, `[[`, "par"), recursive = FALSE)
    j <- which.min(Fs)
    if (length(j) == 0 || !(Fs[j] < eng$F - 1e-8)) return(NULL)
    list(par = pars[[j]], F = Fs[j])
  }
  first_k <- function(s, k) list(F = s$F[seq_len(k)], par = s$par[seq_len(k)])

  w_pt <- pick(list(w))
  h_pt <- pick(list(w, h_e, h_g))
  hk_pt <- pick(list(w, first_k(h_e, K), first_k(h_g, K)))

  finish <- function(pt) {
    if (is.null(pt)) return(NULL)
    tryCatch(m170_finish(pt$par, pt$F, R, spec, eng), error = function(e) NULL)
  }
  list(
    W = finish(w_pt), H = finish(h_pt),
    F_E = eng$F,
    F_W = if (is.null(w_pt)) eng$F else w_pt$F,
    F_H = if (is.null(h_pt)) eng$F else h_pt$F,
    F_HK = if (is.null(hk_pt)) eng$F else hk_pt$F,
    extra_F = vapply(extra_runs, function(s) min(c(s$F, Inf), na.rm = TRUE),
                     numeric(1)),
    n_failed = c(W = sum(is.na(w$F)), H_E = sum(is.na(h_e$F)),
                 H_G = sum(is.na(h_g$F))),
    cpu = c(W = w$cpu, H = w$cpu + h_e$cpu + h_g$cpu,
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
