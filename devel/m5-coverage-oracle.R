# M5/T3: SEM-based SSM coverage oracle (spec devel/m5-sem-design.md sec. 8.3).
#
# Simulates data from known populations, estimates the latent SSM with
# ssm_sem_parameters() under BOTH CI engines, and measures empirical coverage
# of the nominal-95% intervals per parameter (e, a, d; contrast delta-d in the
# two-measure cell). Answers Brief E Q5.1's blocked-on-M4 question: is cheap
# MVN propagation accurate enough at realistic n versus the refit bootstrap?
# The result decides (confirms or flips) the ci_method = "mvn" default.
#
# Machinery REUSED from ssm_ci_accuracy(), not reinvented (spec sec. 8.3):
# its Bradley-liberal banding + 95% Wilson-interval verdict conventions
# (ssm_ci_bradley_class / ssm_ci_wilson), its PSD repair (ssm_ci_psd_repair),
# and the package's single draw root (mvn_root). The exported ssm_ci_accuracy()
# itself CANNOT assess this estimand (it replays observed-data procedures;
# it refuses circumplex_ssm_sem objects with a pointer here).
#
# Truths (spec sec. 8.1/8.3):
#  * analytic cells: populations built from the sec. 3.1 scaled model itself
#    (phi_g = 0 per the T3 identification amendment), so the latent profile
#    rho*0 is closed-form and the true SSM parameters are its transform;
#  * the g-lean cell: built with a leaning general factor and fitted with the
#    STRICT tier (the only tier where the lean is expressible, sec. 3.1);
#  * the realism cell: cpm_fit()'s P-hat on jz2017's octant scales embedded in
#    the observed joint scales+measure correlation matrix (the
#    ssm_ci_accuracy() population approach), PSD-repaired; its truth is the
#    PSEUDO-TRUE value (White 1982 sense): the tier fitted to the population
#    moments themselves, mapped and transformed. This cell assesses coverage
#    of the model-conditional estimand under realistic misspecification of
#    the fixed-angle model -- never of a "true" circumplex parameter.
#
# Reproducibility: every replicate derives its seed as BASE_SEED + a
# cell/replicate offset and calls set.seed() locally, so results are identical
# for any mc.cores and scheduling.
#
# Cost: the mvn arm is cheap (one lavaan fit + draws per replicate); the boot
# arm refits lavaan BOOTS_BOOT times per replicate and dominates the runtime
# (~1-2 CPU-hours at defaults; minutes in smoke mode). Run via
# /statistical-validation, never R CMD check.
#
# Usage:  Rscript devel/m5-coverage-oracle.R           # full run
#         M5_COV_SMOKE=1 Rscript devel/m5-coverage-oracle.R   # ~3 min smoke
#         M5_COV_CELLS=interior,realism Rscript ...    # named cells only

devtools::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(requireNamespace("lavaan", quietly = TRUE))
# ONE copy of the truth algebra, shared with the unit tests
source(file.path("tests", "testthat", "helper-ssm-sem.R"))

smoke <- nzchar(Sys.getenv("M5_COV_SMOKE"))
REPS_MVN <- if (smoke) 20 else 500 # replications per cell, mvn arm
REPS_BOOT <- if (smoke) 5 else 100 # replications per cell, boot arm
BOOTS_MVN <- if (smoke) 500 else 2000 # draws per replicate
BOOTS_BOOT <- if (smoke) 100 else 250 # lavaan refits per replicate
NS <- if (smoke) 250 else c(250, 1000) # sample sizes (order 100-1000, sec. 8.3)
BASE_SEED <- 20260707
CORES <- max(1, parallel::detectCores() - 1)

p <- 8
angles <- as.numeric(octants())
th <- angles * pi / 180
scales <- paste0("s", 1:p)

# ---- population constructors --------------------------------------------------

# Analytic model population via the shared sem_pop() (helper-ssm-sem.R);
# adds the closed-form true SSM parameters and the fitted model tier.
make_pop <- function(a, cc, theta, sigma_m, v_m, measures, model = "scaled",
                     phi = NULL) {
  pop <- sem_pop(a, cc, theta, angles, sigma_m, v_m, phi = phi,
                 scales = scales, measures = measures)
  pop$truth <- lapply(seq_len(nrow(pop$rho0)), function(k) {
    unlist(suppressWarnings(
      ssm_parameters(as.numeric(pop$rho0[k, ]), angles)
    ))
  })
  pop$model <- model
  pop
}

# Pseudo-true truth for a misspecified cell: fit the tier to the population
# moments themselves (huge nominal n so sampling plays no role), map, transform.
pseudo_truth <- function(sigma, measures, model) {
  syn <- ssm_sem_syntax(scales = scales, angles = angles, measures = measures,
                        model = model)
  fit <- lavaan::cfa(syn, sample.cov = sigma, sample.nobs = 1e6)
  stopifnot(lavaan::lavInspect(fit, "converged"))
  res <- suppressWarnings(ssm_sem_parameters(
    fit, scales = scales, angles = angles, measures = measures, boots = 10
  ))
  lapply(seq_len(nrow(res$results)), function(i) {
    r <- res$results[i, ]
    c(Elev = r$e_est, Xval = r$x_est, Yval = r$y_est, Ampl = r$a_est,
      Disp = as.numeric(r$d_est), Fit = r$fit_est)
  })
}

# ---- cells (spec sec. 8.1) ------------------------------------------------------

dir3 <- function(sg, amp, delta_deg) {
  d <- delta_deg * pi / 180
  c(sg, amp * cos(d), amp * sin(d))
}

cells <- list()

# interior: heterogeneous saturations/residuals, d* mid-quadrant
cells$interior <- make_pop(
  a = seq(0.5, 0.8, length.out = p), cc = seq(0.7, 0.5, length.out = p),
  theta = seq(0.3, 0.6, length.out = p),
  sigma_m = cbind(dir3(0.2, 0.4, 60)), v_m = 1, measures = "m1"
)

# pole: d* exactly at 0/360
cells$pole <- make_pop(
  a = rep(0.55, p), cc = rep(0.6, p), theta = seq(0.3, 0.6, length.out = p),
  sigma_m = cbind(dir3(0.15, 0.45, 0)), v_m = 1, measures = "m1"
)

# low amplitude: d* defined but noisy (the guardrail regime; d coverage is
# measured conditional on d being certified-interpretable is NOT replayed
# here -- raw coverage is reported and read with that regime in mind)
cells$low_a <- make_pop(
  a = rep(0.55, p), cc = rep(0.6, p), theta = seq(0.3, 0.6, length.out = p),
  sigma_m = cbind(dir3(0.3, 0.12, 210)), v_m = 1, measures = "m1"
)

# heterogeneous saturation (sec. 4.3's second-harmonic pattern): supplies the
# demonstrated d* rotation magnitude for the docs; truth is still the
# closed-form transform of rho*0 (the estimand IS the rotated direction)
cells$het_sat <- make_pop(
  a = rep(0.55, p), cc = 0.5 + 0.3 * cos(2 * th),
  theta = seq(0.3, 0.6, length.out = p),
  sigma_m = cbind(dir3(0.2, 0.4, 20)), v_m = 1, measures = "m1"
)

# two-measure contrast near +/-180 (the branch-cut trap, sec. 5.5)
cells$contrast <- make_pop(
  a = rep(0.55, p), cc = rep(0.6, p), theta = seq(0.3, 0.6, length.out = p),
  sigma_m = cbind(dir3(0.15, 0.4, 5), dir3(0.15, 0.4, 186)),
  v_m = c(1, 1), measures = c("m1", "m2")
)

# g-lean under the strict tier (the sec. 4.2 channel, strict-only post-T3)
phi_lean <- rbind(c(1, 0.4, 0.2), c(0.4, 1, 0), c(0.2, 0, 1))
cells$g_lean_strict <- make_pop(
  a = rep(1, p), cc = rep(1, p), theta = seq(0.3, 0.6, length.out = p),
  sigma_m = cbind(dir3(0.3, 0.35, 45)), v_m = 1, measures = "m1",
  model = "strict", phi = phi_lean
)

# realism: cpm_fit() P-hat on jz2017 octants + observed measure block,
# PSD-repaired; pseudo-true target under the scaled tier (misspecification
# cell -- the fixed-cosine structure only approximates real data)
make_realism <- function() {
  data("jz2017", envir = environment())
  oct_names <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  # cormat path: deterministic engine fit, analytic CIs -- no bootstrap needed
  # to obtain P-hat (only the point structure is consumed here)
  cfit <- cpm_fit(cormat = stats::cor(jz2017[oct_names]), scales = oct_names,
                  angles = angles, n = nrow(jz2017), m = 3)
  P <- cpm_implied_cor(
    as.numeric(as_radian(as_degree(cfit$results$Angle))),
    cfit$results$Zeta, cfit$betas$Beta
  )
  R_obs <- stats::cor(jz2017[c(oct_names, "PARPD")])
  joint <- R_obs
  joint[1:p, 1:p] <- P
  rep_out <- ssm_ci_psd_repair(joint)
  sigma <- rep_out$S
  dimnames(sigma) <- list(c(scales, "m1"), c(scales, "m1"))
  list(sigma = sigma, rho0 = NULL,
       truth = pseudo_truth(sigma, "m1", "scaled"),
       measures = "m1", model = "scaled", psd_delta = rep_out$delta)
}
# Apply the cell filter BEFORE building the expensive realism cell (cpm_fit +
# PSD repair + a population-moment lavaan fit), so named-cell iteration runs
# skip its setup cost.
only <- Sys.getenv("M5_COV_CELLS")
selected <- if (nzchar(only)) strsplit(only, ",")[[1]] else
  c(names(cells), "realism")
cells <- cells[intersect(names(cells), selected)]
if ("realism" %in% selected) cells$realism <- make_realism()

# ---- coverage machinery ---------------------------------------------------------

# Circular CI membership via the package's CANONICAL arc rule
# (ssm_ci_d_cover: radians, NA-safe, contrast-aware -- the same rule
# ssm_ci_accuracy() ships), so the harness cannot drift from it. Degrees in;
# contrast endpoints arrive branch-aligned/unwrapped and are passed through.
d_covered <- function(lci_deg, uci_deg, truth_deg, contrast = FALSE) {
  ssm_ci_d_cover(
    truth_deg * pi / 180, lci_deg * pi / 180, uci_deg * pi / 180,
    contrast = contrast
  )$cover
}

root_cache <- lapply(cells, function(cl) mvn_root(cl$sigma))

one_rep <- function(cell_name, cl, N, engine, boots, i) {
  set.seed(BASE_SEED + 1e7 * match(engine, c("mvn", "boot")) +
             1e6 * match(cell_name, names(cells)) + 1e3 * match(N, NS) + i)
  q <- ncol(cl$sigma)
  X <- matrix(stats::rnorm(N * q), N, q) %*% root_cache[[cell_name]]
  colnames(X) <- colnames(cl$sigma)
  dat <- as.data.frame(X)
  contrast <- length(cl$measures) == 2
  # Replay the SHIPPED procedure: ssm_sem() itself (its own lavaan defaults,
  # including the robust sandwich vcov the mvn engine propagates)
  res <- try(silent = TRUE, suppressWarnings(
    ssm_sem(
      dat, scales = scales, angles = angles, measures = cl$measures,
      model = cl$model, ci_method = engine, boots = boots,
      contrast = contrast
    )
  ))
  if (inherits(res, "try-error")) {
    return(list(ok = FALSE, why = attr(res, "condition")$message))
  }
  r <- res$results
  out <- list(ok = TRUE)
  for (k in seq_along(cl$measures)) {
    tr <- cl$truth[[k]]
    out[[paste0("e", k)]] <- r$e_lci[k] <= tr[["Elev"]] &
      tr[["Elev"]] <= r$e_uci[k]
    out[[paste0("a", k)]] <- r$a_lci[k] <= tr[["Ampl"]] &
      tr[["Ampl"]] <= r$a_uci[k]
    out[[paste0("d", k)]] <- d_covered(
      as.numeric(r$d_lci[k]), as.numeric(r$d_uci[k]), tr[["Disp"]] %% 360
    )
  }
  if (contrast) {
    dtruth <- as.numeric(angle_dist(
      as_radian(as_degree(cl$truth[[2]][["Disp"]])),
      as_radian(as_degree(cl$truth[[1]][["Disp"]]))
    )) * 180 / pi
    i3 <- nrow(r)
    out$e_c <- r$e_lci[i3] <= (cl$truth[[2]][["Elev"]] - cl$truth[[1]][["Elev"]]) &
      (cl$truth[[2]][["Elev"]] - cl$truth[[1]][["Elev"]]) <= r$e_uci[i3]
    # contrast interval is branch-aligned/unwrapped; the canonical rule's
    # contrast arm clamps width at a full turn instead of wrapping it to 0
    out$d_c <- d_covered(
      as.numeric(r$d_lci[i3]), as.numeric(r$d_uci[i3]), dtruth,
      contrast = TRUE
    )
  }
  out
}

run_cell <- function(cell_name, cl, N, engine, reps, boots) {
  res <- parallel::mclapply(seq_len(reps), function(i) {
    one_rep(cell_name, cl, N, engine, boots, i)
  }, mc.cores = CORES)
  ok <- vapply(res, function(x) isTRUE(x$ok), logical(1))
  if (!any(ok)) {
    # A whole-cell failure is a reportable result, not a crash that loses
    # every completed cell of a multi-hour run
    return(list(cell = cell_name, N = N, engine = engine, n_ok = 0,
                n_fail = sum(!ok), coverage = NULL))
  }
  keys <- setdiff(names(res[ok][[1]]), c("ok", "why"))
  cov <- sapply(keys, function(k) {
    # NA indicators (e.g., an NA displacement CI from a degenerate replicate)
    # are excluded from the denominator, not counted as misses
    vals <- unlist(lapply(res[ok], function(x) x[[k]]))
    c(k = sum(vals, na.rm = TRUE), n = sum(!is.na(vals)))
  })
  list(cell = cell_name, N = N, engine = engine, n_ok = sum(ok),
       n_fail = sum(!ok), coverage = cov)
}

# ---- run ------------------------------------------------------------------------

t0 <- proc.time()[["elapsed"]]
grid <- expand.grid(cell = names(cells), N = NS, engine = c("mvn", "boot"),
                    stringsAsFactors = FALSE)
results <- vector("list", nrow(grid))
for (g in seq_len(nrow(grid))) {
  cell_name <- grid$cell[g]
  engine <- grid$engine[g]
  reps <- if (engine == "mvn") REPS_MVN else REPS_BOOT
  boots <- if (engine == "mvn") BOOTS_MVN else BOOTS_BOOT
  message(sprintf("[%s] cell %-14s N %4d engine %-4s (%d reps x %d boots)...",
                  format(Sys.time(), "%H:%M:%S"), cell_name, grid$N[g],
                  engine, reps, boots))
  results[[g]] <- run_cell(cell_name, cells[[cell_name]], grid$N[g], engine,
                           reps, boots)
}

# ---- report ---------------------------------------------------------------------

cat("\n== M5 T3 coverage (nominal 95%; Bradley-liberal band via Wilson) ==\n")
rows <- list()
for (r in results) {
  if (is.null(r$coverage)) {
    cat(sprintf("NOTE: cell %s N %d engine %s: all %d replicates failed\n",
                r$cell, r$N, r$engine, r$n_fail))
    next
  }
  for (j in seq_len(ncol(r$coverage))) {
    k <- r$coverage[1, j]
    n <- r$coverage[2, j]
    cls <- ssm_ci_bradley_class(k, n, 0.95)
    w <- ssm_ci_wilson(k, n)
    rows[[length(rows) + 1]] <- data.frame(
      cell = r$cell, N = r$N, engine = r$engine,
      param = colnames(r$coverage)[j],
      coverage = round(k / n, 3), n = n,
      wilson_lo = round(w[1], 3), wilson_hi = round(w[2], 3),
      verdict = cls[1], direction = ifelse(is.na(cls[2]), "", cls[2]),
      fails = r$n_fail
    )
  }
}
tab <- do.call(rbind, rows)
print(tab, row.names = FALSE)
cat(sprintf("\nElapsed: %.1f min\n", (proc.time()[["elapsed"]] - t0) / 60))

saveRDS(list(table = tab, results = results, smoke = smoke,
             settings = list(REPS_MVN = REPS_MVN, REPS_BOOT = REPS_BOOT,
                             BOOTS_MVN = BOOTS_MVN, BOOTS_BOOT = BOOTS_BOOT,
                             NS = NS, BASE_SEED = BASE_SEED),
             date = Sys.time()),
        file.path("devel", "m5-coverage-oracle-results.rds"))
cat("Saved devel/m5-coverage-oracle-results.rds\n")
