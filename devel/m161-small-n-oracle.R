# M161: small-N coverage oracle for ssm_trajectory() intervals -----------------
#
# Measures the coverage of the trajectory intervals that ssm_trajectory()
# builds from a joint REML fit's `coef` and `vcov`, at small numbers of
# persons, under three draw rules that share each replicate's fit and normal
# draws. Replicates come FROM THE SAME MODEL FAMILY the M27 oracle simulates
# (devel/m27-coverage-oracle.R: correlated person random intercepts,
# independent per-dv residuals, linear fixed trajectories), fit with the
# glmmTMB call that ssm_growth_formula("glmmTMB") returns. The derivation the
# rules rest on is RR25 section 1 (cairn/reviews/archive/): on complete
# balanced data REML equals the ANOVA fit, and at the mean design time
# (wave 2) the pivot of every linear combination of e, x and y is exactly t
# on N - 1 df.
#
# GRID (1000 replicates per cell, waves 0:4):
#   truths  -- the M27 `pole` (d drifts 350 -> 10 deg) and `xycor`
#              (rho_u = .9 on x, y; d ~ 135 deg) cells, unchanged.
#   N       -- 10, 20, 30, 50, 200 persons, complete data.
#   missing -- N = 20 and N = 30 per truth, 20% of person-waves deleted at
#              random (all three dvs of a deleted person-wave go).
#
# PRE-REGISTRATION (fixed before the full run; smoke checks machinery only).
#
# Candidate rules (RR25 section 3; M161-D2). Each replicate draws one matrix Z
# of N_DRAWS x 6 standard normals, then L = mvn_root(vcov), then (R1 only) one
# chi-square divisor per draw, in that order:
#   R0  -- shipped normal draws: coef + Z L (what mvn_draws() returns).
#   R1  -- multivariate t, one divisor shared by all coefficients:
#          coef + (Z L) / sqrt(chisq_nu / nu), nu = N - 1.
#   R3a -- normal draws on vcov times N / (N - 1): coef + sqrt(N / (N - 1)) Z L
#          (a plausible-but-wrong control: it undercorrects).
# Every rule's draws go through ssm_trajectory(times = 0:4, draws = .), so all
# three share the shipped summary and certification machinery.
#
# Fit status, disjoint, assigned in this order: "fail" (glmmTMB error),
# "nonpd" (sdr$pdHess FALSE), "nonfinite" (vcov(fit)$cond not all finite),
# else "ok". Coverage is computed for "ok" and for "nonpd" fits with a finite
# vcov (reported only); "fail" and "nonfinite" have none.
#
# Gated waves: all five for e, x, y and d (true a(t) >= .59 at every wave of
# both truths, so d is identified everywhere).
#
# AC2 band (complete-data cells, N in {20, 30, 50}, both truths): coverage
#   over ALL replicates, a fit of any status other than "ok" counted as not
#   covering. A rule passes the band when, in every such cell, the mean 95%
#   coverage over waves lies in [.93, .97] for each of e, x, y and d, and no
#   single wave of these falls below .91.
# AC3 targets (wave 2, complete-data cells, coverage among "ok" fits only):
#   (i)  N in {20, 30, 50, 200}: the coverage of e, x and y under R0 and under
#        R1 lies within 3.5 * sqrt(c (1 - c) / n) of c, n the "ok" count;
#        c = P(|T| < qnorm(.975)), T ~ t on N - 1 df, under R0; c = .95 under
#        R1.
#   (ii) N in {20, 30, 50}: the per-replicate R1 minus R0 coverage difference
#        at wave 2, averaged over e, x and y and pooled over both truths, has
#        mean within 3.5 * sd / sqrt(n) of .95 minus R0's c.
# AC5 control (complete-data N = 200 cells): R0's mean d coverage over waves
#   (all-replicates basis) lies in [.93, .97].
# The N = 10 cells and the missing-wave cells are reported without a gate.
#
# Seeds: BASE_SEED + OFFSET + cell_idx * 1e6 + rep. cell_idx is the LEVEL
# index of the cell in `cells` below (1 to 14), never a raw N value. The
# range 520e6 + [1e6, 15e6] (plus BASE_SEED) is disjoint from M21
# (12e7 + 1e6 * cfg + ...), M27 (15e7 + 1e6 * [1, 5]) and the M29 scripts
# (BASE_SEED + 1e6 * (10 * cell + method), two cells, so below 5e7).
#
# Usage:  Rscript devel/m161-small-n-oracle.R                 # full run
#         M161_SMOKE=1 Rscript devel/m161-small-n-oracle.R    # quick smoke
# Output: devel/m161-small-n-results.rds (+ smoke variant), summary printed.

devtools::load_all(".", quiet = TRUE)
stopifnot(requireNamespace("glmmTMB", quietly = TRUE))

smoke <- nzchar(Sys.getenv("M161_SMOKE"))
REPS <- if (smoke) 25 else 1000
N_DRAWS <- 4000
BASE_SEED <- 20261018
OFFSET <- 52e7
CORES <- max(1, parallel::detectCores() - 1)
WAVES <- 0:4
DVS <- c("e", "x", "y")
RULES <- c("R0", "R1", "R3a")
OUTCOMES <- c("e", "x", "y", "a", "d")
P_MISS <- 0.20

# --- shared machinery (generator and truths as in the M27 oracle) -------------

mvn <- function(R, mu, sigma) {
  eig <- eigen(sigma, symmetric = TRUE)
  root <- eig$vectors %*% (sqrt(pmax(eig$values, 0)) * t(eig$vectors))
  sweep(matrix(rnorm(R * length(mu)), nrow = R) %*% root, 2, mu, "+")
}

traj_coef <- function(start, end, t_max = max(WAVES)) {
  cbind(b0 = start, b1 = (end - start) / t_max)
}

truth_at <- function(coef) {
  e <- coef["e", "b0"] + coef["e", "b1"] * WAVES
  x <- coef["x", "b0"] + coef["x", "b1"] * WAVES
  y <- coef["y", "b0"] + coef["y", "b1"] * WAVES
  list(e = e, x = x, y = y, a = sqrt(x^2 + y^2),
       d = (atan2(y, x) * 180 / pi) %% 360)
}

sim_long <- function(n_person, coef, sigma_u, sd_resid) {
  u <- mvn(n_person, c(0, 0, 0), sigma_u)
  dat <- expand.grid(person = seq_len(n_person), wave = WAVES)
  long <- do.call(rbind, lapply(seq_along(DVS), function(j) {
    dv <- DVS[j]
    data.frame(
      person = dat$person, wave = dat$wave, dv = dv,
      value = coef[dv, "b0"] + coef[dv, "b1"] * dat$wave +
        u[dat$person, j] + rnorm(nrow(dat), 0, sd_resid[j])
    )
  }))
  long$dv <- factor(long$dv, levels = DVS)
  long$person <- factor(long$person)
  long
}

# Delete a random P_MISS share of person-waves (every dv row of each one)
drop_person_waves <- function(long, n_person) {
  n_pw <- n_person * length(WAVES)
  gone <- sample.int(n_pw, round(P_MISS * n_pw))
  key <- (as.integer(long$person) - 1L) * length(WAVES) + long$wave + 1L
  long <- long[!key %in% gone, ]
  long$person <- droplevels(long$person)
  long
}

# The joint REML fit, with the formula ssm_growth_formula("glmmTMB") prints.
# Returns the status (disjoint, in the pre-registered order) with coef/vcov.
fit_joint <- function(long) {
  fit <- tryCatch(
    suppressWarnings(glmmTMB::glmmTMB(
      value ~ 0 + dv + dv:wave + us(0 + dv | person),
      dispformula = ~ 0 + dv, data = long, REML = TRUE
    )),
    error = function(e) NULL
  )
  if (is.null(fit)) return(list(status = "fail"))
  b <- glmmTMB::fixef(fit)$cond
  V <- tryCatch(suppressWarnings(as.matrix(vcov(fit)$cond)),
                error = function(e) NULL)
  finite_v <- !is.null(V) && all(is.finite(V))
  if (!isTRUE(fit$sdr$pdHess)) {
    return(list(status = "nonpd", coef = b, vcov = if (finite_v) V))
  }
  if (!finite_v) return(list(status = "nonfinite"))
  list(status = "ok", coef = b, vcov = V)
}

# Circular inclusion, as in the M27 oracle
d_covered <- function(v, est, lci, uci) {
  rel <- function(a) ((a - est + 180) %% 360) - 180
  !is.na(est) && rel(lci) <= rel(v) && rel(v) <= rel(uci)
}

# The three rules' draw matrices from one fit, sharing Z (and so R0's draws
# are exactly what mvn_draws() would return at the same RNG state)
rule_draws <- function(b, V, n_person) {
  p <- length(b)
  Z <- matrix(rnorm(N_DRAWS * p), nrow = N_DRAWS, ncol = p)
  ZL <- Z %*% mvn_root(V)
  nu <- n_person - 1
  S <- sqrt(rchisq(N_DRAWS, df = nu) / nu)
  out <- list(
    R0 = sweep(ZL, 2, b, "+"),
    R1 = sweep(ZL / S, 2, b, "+"),
    R3a = sweep(sqrt(n_person / (n_person - 1)) * ZL, 2, b, "+")
  )
  lapply(out, function(B) {
    colnames(B) <- names(b)
    B
  })
}

# Coverage, certification and d arc width for one rule's trajectory table
score_rule <- function(B, truth) {
  tr <- suppressWarnings(ssm_trajectory(times = WAVES, draws = B))
  cov <- sapply(OUTCOMES, function(o) {
    est <- tr[[paste0(o, "_est")]]
    lci <- tr[[paste0(o, "_lci")]]
    uci <- tr[[paste0(o, "_uci")]]
    if (o == "d") {
      mapply(d_covered, truth$d, est, lci, uci)
    } else {
      lci <= truth[[o]] & truth[[o]] <= uci
    }
  })
  list(cov = cov, cert = tr$certified, d_width = (tr$d_uci - tr$d_lci) %% 360)
}

# One replicate: arrays [rule, wave, outcome] of coverage, [rule, wave] of
# certification and d width; NA where the fit gives no usable vcov
rep_one <- function(rep_i, cell) {
  set.seed(BASE_SEED + OFFSET + cell$idx * 1e6 + rep_i)
  long <- sim_long(cell$n, cell$coef, cell$sigma_u, cell$sd_resid)
  if (cell$missing) long <- drop_person_waves(long, cell$n)
  fit <- fit_joint(long)
  cov <- array(NA, dim = c(length(RULES), length(WAVES), length(OUTCOMES)),
               dimnames = list(RULES, WAVES, OUTCOMES))
  cert <- d_width <- array(NA, dim = c(length(RULES), length(WAVES)),
                           dimnames = list(RULES, WAVES))
  if (!is.null(fit$vcov)) {
    draws <- rule_draws(fit$coef, fit$vcov, cell$n)
    truth <- truth_at(cell$coef)
    for (r in RULES) {
      s <- score_rule(draws[[r]], truth)
      cov[r, , ] <- s$cov
      cert[r, ] <- s$cert
      d_width[r, ] <- s$d_width
    }
  }
  list(status = fit$status, cov = cov, cert = cert, d_width = d_width)
}

# --- cell definitions -----------------------------------------------------------

truths <- list(
  pole = list(
    coef = rbind(
      e = c(0.50, 0),
      x = traj_coef(0.6 * cos(350 * pi / 180), 0.6 * cos(10 * pi / 180)),
      y = traj_coef(0.6 * sin(350 * pi / 180), 0.6 * sin(10 * pi / 180))
    ),
    sigma_u = {
      sd_u <- c(0.20, 0.20, 0.20)
      R <- rbind(c(1, .3, .2), c(.3, 1, .3), c(.2, .3, 1))
      diag(sd_u) %*% R %*% diag(sd_u)
    },
    sd_resid = c(0.25, 0.25, 0.25)
  ),
  xycor = list(
    coef = rbind(
      e = c(0.50, 0),
      x = traj_coef(0.6 * cos(130 * pi / 180), 0.6 * cos(140 * pi / 180)),
      y = traj_coef(0.6 * sin(130 * pi / 180), 0.6 * sin(140 * pi / 180))
    ),
    sigma_u = {
      sd_u <- c(0.20, 0.35, 0.35)
      R <- rbind(c(1, .2, .2), c(.2, 1, .9), c(.2, .9, 1))
      diag(sd_u) %*% R %*% diag(sd_u)
    },
    sd_resid = c(0.25, 0.15, 0.15)
  )
)
for (nm in names(truths)) {
  dimnames(truths[[nm]]$coef) <- list(DVS, c("b0", "b1"))
}

grid <- rbind(
  expand.grid(n = c(10, 20, 30, 50, 200), truth = names(truths),
              missing = FALSE, stringsAsFactors = FALSE),
  expand.grid(n = c(20, 30), truth = names(truths), missing = TRUE,
              stringsAsFactors = FALSE)
)
grid$idx <- seq_len(nrow(grid))
grid$name <- sprintf("%s_n%d%s", grid$truth, grid$n,
                     ifelse(grid$missing, "_miss", ""))
cells <- lapply(seq_len(nrow(grid)), function(i) {
  c(as.list(grid[i, ]), truths[[grid$truth[i]]])
})
names(cells) <- grid$name

# --- smoke-only machinery checks ------------------------------------------------

if (smoke) {
  # R0's draws equal mvn_draws() at the same RNG state
  V <- diag(6) * 0.01
  V[2, 3] <- V[3, 2] <- 0.004
  b <- setNames(seq(0.1, 0.6, by = 0.1),
                c("dve", "dvx", "dvy", "dve:wave", "dvx:wave", "dvy:wave"))
  set.seed(1)
  ref <- mvn_draws(N_DRAWS, b, V)
  set.seed(1)
  got <- rule_draws(b, V, 20)$R0
  stopifnot(isTRUE(all.equal(unname(ref), unname(got), tolerance = 0)))
  # fit_joint() fits the formula ssm_growth_formula("glmmTMB") prints
  stopifnot(grepl("value ~ 0 + dv + dv:wave + us(0 + dv | person)",
                  paste(capture.output(ssm_growth_formula("glmmTMB")),
                        collapse = " "), fixed = TRUE))
  # The truths keep a(t) >= .59 at every wave (the gated-wave justification)
  stopifnot(all(vapply(truths, function(tt) min(truth_at(tt$coef)$a),
                       numeric(1)) >= 0.59))
  message("Smoke machinery checks passed.")
}

# --- run the grid ---------------------------------------------------------------

message("M161 grid (REPS = ", REPS, ", cores = ", CORES, ") ...")
raw <- list()
for (nm in names(cells)) {
  cell <- cells[[nm]]
  t0 <- Sys.time()
  reps <- parallel::mclapply(seq_len(REPS), rep_one, cell = cell,
                             mc.cores = CORES)
  bad <- vapply(reps, function(z) !is.list(z) || is.null(z$status), logical(1))
  if (any(bad)) stop("Replicate error in ", nm, ": ", reps[[which(bad)[1]]])
  raw[[nm]] <- list(
    status = vapply(reps, `[[`, character(1), "status"),
    cov = simplify2array(lapply(reps, `[[`, "cov")),
    cert = simplify2array(lapply(reps, `[[`, "cert")),
    d_width = simplify2array(lapply(reps, `[[`, "d_width")),
    minutes = as.numeric(difftime(Sys.time(), t0, units = "mins"))
  )
  message(sprintf("  %s: %.1f min, status %s", nm, raw[[nm]]$minutes,
                  paste(names(table(raw[[nm]]$status)),
                        table(raw[[nm]]$status), sep = "=", collapse = " ")))
}

# --- summaries -------------------------------------------------------------------
# cov arrays are [rule, wave, outcome, rep]; cert/d_width are [rule, wave, rep]

summarize_cell <- function(r) {
  ok <- r$status == "ok"
  all_basis <- r$cov
  all_basis[, , , !ok] <- FALSE
  conv <- r$cov[, , , ok, drop = FALSE]
  cov_all <- apply(all_basis, 1:3, mean)
  cov_conv <- apply(conv, 1:3, mean)
  n_ok <- sum(ok)
  pairs <- combn(RULES, 2)
  paired <- lapply(seq_len(ncol(pairs)), function(k) {
    a <- pairs[1, k]
    b <- pairs[2, k]
    one <- function(arr) {
      dd <- arr[b, , , , drop = FALSE] - arr[a, , , , drop = FALSE]
      dd <- array(as.numeric(dd), dim = dim(dd)[-1],
                  dimnames = dimnames(dd)[-1])
      list(diff = apply(dd, 1:2, mean),
           se = apply(dd, 1:2, sd) / sqrt(dim(dd)[3]))
    }
    list(pair = paste0(b, "-", a), all = one(all_basis), conv = one(conv))
  })
  names(paired) <- vapply(paired, `[[`, character(1), "pair")
  list(
    n_reps = length(ok), n_ok = n_ok,
    fail_counts = table(factor(r$status,
                               levels = c("ok", "fail", "nonpd", "nonfinite"))),
    cov_all = cov_all,
    cov_conv = cov_conv,
    mcse_all = sqrt(cov_all * (1 - cov_all) / length(ok)),
    mcse_conv = sqrt(cov_conv * (1 - cov_conv) / n_ok),
    cert_rate = apply(r$cert[, , ok, drop = FALSE], 1:2, mean),
    d_width_median = apply(r$d_width[, , ok, drop = FALSE], 1:2, median),
    paired = paired,
    nonpd_cov = if (any(r$status == "nonpd" & !is.na(r$cov[1, 1, 1, ]))) {
      sel <- r$status == "nonpd" & !is.na(r$cov[1, 1, 1, ])
      list(n = sum(sel), cov = apply(r$cov[, , , sel, drop = FALSE], 1:3, mean))
    }
  )
}

summ <- lapply(raw, summarize_cell)

# --- verdicts against the pre-registered gates ------------------------------------

band_cells <- grid$name[!grid$missing & grid$n %in% c(20, 30, 50)]
band <- sapply(RULES, function(rule) {
  all(vapply(band_cells, function(nm) {
    cv <- summ[[nm]]$cov_all[rule, , c("e", "x", "y", "d")]
    m <- colMeans(cv)
    all(m >= 0.93 & m <= 0.97) && all(cv >= 0.91)
  }, logical(1)))
})

c_target <- function(rule, n) {
  if (rule == "R1") 0.95 else 2 * pt(qnorm(0.975), df = n - 1) - 1
}
ac3_i_cells <- grid$name[!grid$missing & grid$n %in% c(20, 30, 50, 200)]
ac3_i <- do.call(rbind, lapply(ac3_i_cells, function(nm) {
  n <- grid$n[grid$name == nm]
  do.call(rbind, lapply(c("R0", "R1"), function(rule) {
    cc <- c_target(rule, n)
    obs <- summ[[nm]]$cov_conv[rule, "2", c("e", "x", "y")]
    tol <- 3.5 * sqrt(cc * (1 - cc) / summ[[nm]]$n_ok)
    data.frame(cell = nm, rule = rule, outcome = c("e", "x", "y"),
               coverage = obs, target = cc, tol = tol,
               pass = abs(obs - cc) <= tol, row.names = NULL)
  }))
}))

ac3_ii <- do.call(rbind, lapply(c(20, 30, 50), function(n) {
  nms <- grid$name[!grid$missing & grid$n == n]
  d <- unlist(lapply(nms, function(nm) {
    r <- raw[[nm]]
    ok <- r$status == "ok"
    cv <- r$cov[, "2", c("e", "x", "y"), ok, drop = FALSE]
    colMeans(matrix(as.numeric(cv["R1", , , ]) - as.numeric(cv["R0", , , ]),
                    nrow = 3))
  }))
  target <- 0.95 - c_target("R0", n)
  tol <- 3.5 * sd(d) / sqrt(length(d))
  data.frame(n = n, diff = mean(d), target = target, tol = tol, n_reps = length(d),
             pass = abs(mean(d) - target) <= tol)
}))

ac5 <- vapply(grid$name[!grid$missing & grid$n == 200], function(nm) {
  m <- mean(summ[[nm]]$cov_all["R0", , "d"])
  m >= 0.93 && m <= 0.97
}, logical(1))

verdicts <- list(
  AC2_band = band,
  AC3_i = all(ac3_i$pass),
  AC3_ii = all(ac3_ii$pass),
  AC5_n200_R0_d = all(ac5)
)

results <- list(
  raw = raw, summary = summ, grid = grid,
  ac3_i = ac3_i, ac3_ii = ac3_ii,
  meta = list(
    smoke = smoke, reps = REPS, n_draws = N_DRAWS, base_seed = BASE_SEED,
    offset = OFFSET, waves = WAVES, rules = RULES, p_miss = P_MISS,
    date = Sys.Date(), r_version = R.version.string,
    glmmTMB = as.character(packageVersion("glmmTMB")),
    TMB = as.character(packageVersion("TMB")),
    circumplex = as.character(packageVersion("circumplex")),
    verdicts = verdicts
  )
)

out_file <- if (smoke) {
  "devel/m161-small-n-smoke.rds"
} else {
  "devel/m161-small-n-results.rds"
}
saveRDS(results, out_file)

# --- printed summary -------------------------------------------------------------

cat("\n== M161 small-N oracle", if (smoke) "(SMOKE)" else "(FULL)", "==\n")
for (nm in names(summ)) {
  s <- summ[[nm]]
  cat("\n[", nm, "] ok", s$n_ok, "/", s$n_reps, "| status:",
      paste(names(s$fail_counts), s$fail_counts, sep = "=", collapse = " "),
      "\n")
  for (rule in RULES) {
    m_all <- colMeans(s$cov_all[rule, , ])
    m_conv <- colMeans(s$cov_conv[rule, , ])
    cat(sprintf("  %-3s mean cov (all) %s | (conv) %s\n", rule,
                paste(sprintf("%s %.3f", OUTCOMES, m_all), collapse = " "),
                paste(sprintf("%.3f", m_conv), collapse = " ")))
  }
}
cat("\nAC3 (i) wave-2 closed-form checks:\n")
print(ac3_i, digits = 3)
cat("\nAC3 (ii) paired R1 - R0 at wave 2:\n")
print(ac3_ii, digits = 3)
cat("\nVerdicts:\n")
cat("  AC2 band:", paste(names(band), band, sep = "=", collapse = " "), "\n")
for (v in names(verdicts)[-1]) cat("  ", v, ":", verdicts[[v]], "\n")
cat("Saved:", out_file, "\n")
