# CPM CI simulation study -- self-tests for the delicate math (plan sec. 2.5,
# sec. 4.2/4.3, sec. 6.1/6.2). Pure-function fixtures on synthetic vectors: this
# validates the interval/coverage/decision code WITHOUT running any factorial
# cell or fitting the CPM. Run:  Rscript devel/cpm-sim/selftest.R
#
# It exercises: the span-rule circular coverage + miss side, the BCa
# acceleration formula, BCa's reduction to percentile at a = z0 = 0 plus its
# saturation/clamping accounting, the cluster interval, and the Bradley +
# region verdicts.

dir <- if (nzchar(Sys.getenv("CPM_SIM_DIR"))) Sys.getenv("CPM_SIM_DIR") else
  file.path(getwd(), "devel", "cpm-sim")
options(cpm_sim_pkg = Sys.getenv("CPM_SIM_PKG", "."))
for (m in c("common.R", "config.R", "intervals.R", "kernel.R", "summarize.R"))
  source(file.path(dir, m))

.n_ok <- 0L; .n_fail <- 0L
ok <- function(cond, msg) {
  if (isTRUE(cond)) { .n_ok <<- .n_ok + 1L; cat("ok  ", msg, "\n") }
  else { .n_fail <<- .n_fail + 1L; cat("FAIL", msg, "\n") }
}
near <- function(a, b, tol = 1e-8) all(abs(a - b) <= tol)

# ---- span-rule circular coverage (sec. 2.5) ---------------------------------
# Simple interval [10, 50] covers 30, not 60.
ok(angle_covered(10, 50, 30) && !angle_covered(10, 50, 60), "span: simple arc")
# Wrapped interval lci > uci: [350, 20] covers 10 and 355, not 180.
ok(angle_covered(350, 20, 10) && angle_covered(350, 20, 355) &&
     !angle_covered(350, 20, 180), "span: wrapped arc straddling 0/360")
# Truth exactly at the 0/360 pole handled without special-casing.
ok(angle_covered(350, 10, 0) && angle_covered(350, 10, 360), "span: pole truth")
# Miss side + the exact-tie -> upper convention (sec. 5.1).
ok(identical(angle_miss_side(10, 50, 5), "lower") &&
     identical(angle_miss_side(10, 50, 55), "upper"), "miss side: lower/upper")
ok(is.na(angle_miss_side(10, 50, 30)), "miss side: NA when covered")
ok(identical(angle_miss_side(100, 140, 300), "upper"),
   "miss side: equidistant tie -> upper")

# ---- linear coverage --------------------------------------------------------
ok(lin_covered(0.2, 0.8, 0.5) && !lin_covered(0.2, 0.8, 0.9), "lin: covers")
ok(identical(lin_miss_side(0.2, 0.8, 0.1), "lower") &&
     identical(lin_miss_side(0.2, 0.8, 0.9), "upper"), "lin: miss side")

# ---- BCa acceleration formula (sec. 4.3) ------------------------------------
# Hand check on a small skewed vector.
v <- c(1, 2, 2, 3, 10)
d <- mean(v) - v
a_manual <- sum(d^3) / (6 * sum(d^2)^1.5)
ok(near(bca_acceleration(v), a_manual), "accel: matches Efron skewness formula")
ok(near(bca_acceleration(rep(5, 20)), 0), "accel: point-mass -> 0")
# Invariance to common rescaling of deviations (the no-delete-d cancellation).
ok(near(bca_acceleration(v), bca_acceleration(mean(v) + 3 * (v - mean(v)))),
   "accel: invariant to deviation rescaling")

# ---- BCa reduces to percentile at a = 0, z0 = 0 (sec. 4.2) -------------------
set.seed(1)
tstar <- c(-rev(seq(0.001, 1, length.out = 500)), seq(0.001, 1, length.out = 500))
t_hat <- 0                                   # 500 below, 500 above -> z0 = 0
b <- bca_one(t_hat, tstar, a = 0, level = 0.95)
qp <- stats::quantile(tstar, c(0.025, 0.975), names = FALSE)
ok(!b$na && near(b$z0, 0) && near(c(b$lci, b$uci), qp),
   "bca: reduces to percentile when a = z0 = 0")

# ---- BCa z0 saturation + endpoint clamping (sec. 4.2/5.2) --------------------
sat <- bca_one(0, runif_seeded <- {set.seed(2); runif(200, 1, 2)}, a = 0, level = 0.95)
ok(sat$na && sat$saturated, "bca: all-one-side mass -> saturated + NA")
# 195/200 below t_hat -> z0 ~ +1.96 -> upper adjusted prob clamps.
clamp_star <- c(rep(-1, 195), rep(1, 5))
cl <- bca_one(0, clamp_star, a = 0, level = 0.95)
ok(isTRUE(cl$clamped_hi), "bca: extreme z0 -> upper endpoint clamped (counted)")

# ---- cluster interval + Bradley verdict (sec. 6.1/6.2) ----------------------
ci <- cluster_ci(rep(0.95, 400))
ok(near(ci["est"], 0.95) && ci["n"] == 400, "cluster_ci: mean + n")
band <- bradley_band(0.95)
ok(near(unname(band), c(0.925, 0.975)), "bradley band: 95% -> [.925,.975]")
ok(bradley_verdict(c(est = .95, lci = .94, uci = .96, n = 100), band) == "adequate",
   "verdict: inside band -> adequate")
ok(bradley_verdict(c(est = .80, lci = .78, uci = .82, n = 100), band) == "non-nominal",
   "verdict: entirely below -> non-nominal")
ok(bradley_verdict(c(est = .93, lci = .91, uci = .95, n = 100), band) == "borderline",
   "verdict: straddling -> borderline")

# ---- region aggregation (sec. 6.1) ------------------------------------------
rv <- region_verdict(c(rep("adequate", 96), rep("borderline", 4)))
ok(rv$verdict == "adequate", "region: >=95% adequate, none non-nominal -> adequate")
rv2 <- region_verdict(c(rep("adequate", 96), rep("non-nominal", 4)))
ok(rv2$verdict == "mixed", "region: any non-nominal blocks an adequate claim")
rv3 <- region_verdict(rep("non-nominal", 100))
ok(rv3$verdict == "inadequate", "region: >=95% non-nominal -> inadequate")

# ---- review-fix regressions -------------------------------------------------
# M1: the seed offset stays a valid 32-bit integer for a generous cell count.
ok(BASE_SEED + SEED_MULT * 2000L + SEED_MAX_I < .Machine$integer.max,
   "M1: seed offset within integer range for 2000 cells")
# M3: fit_prop returns NA for a method that scored nothing (no phantom fold).
rec_phantom <- list(beta = NULL, beta_removed = list(cover = c(TRUE)))
ok(is.na(fit_prop(rec_phantom, "beta", "percentile")),
   "M3: fit_prop NA when method scored nothing (no removed-only phantom)")
rec_real <- list(beta = list(cover = matrix(c(TRUE, FALSE), 1, 2,
                 dimnames = list("percentile", NULL))),
                 beta_removed = list(cover = c(TRUE)))
ok(near(fit_prop(rec_real, "beta", "percentile"), 2 / 3),
   "M3: fit_prop folds removed score only when method has kept scores")
# M5: Wald-theta miss side names where the TRUTH lies (truth below -> lower).
ok(ang_signed(60, 30) < 0 && identical(angle_miss_side(50, 70, 30), "lower"),
   "M5: truth below estimate -> lower (kernel convention matches shared)")
# S2: a non-saturation NA (B_used < floor) is NOT flagged as saturated.
bfloor <- bca_one(0, seq(-1, 1, length.out = 50), a = 0, level = 0.95)
ok(bfloor$na && !bfloor$saturated, "S2: B_used<floor NA is not saturation")

# ---- coupling tripwire: percentile arm == shipped cpm_bootstrap (fits CPM) --
# Numerical parity fixture (Fable DECIDE-2): asserts the engine's percentile
# constructors reproduce cpm_fit(ci_method="bootstrap") byte-for-byte, so a
# silent drift in the sim_replicates() reconstruction fails loudly. This fits
# the CPM (1 cold fit + 2x200 warm refits, ~2-4 s) -- the pure-math tests above
# run first and this is the only fitting the file does; it is NOT the study run.
cat("\n-- coupling tripwire (fits the CPM) --\n")
P0 <- make_population_matrix(ANGLE_SETS$p8_equal, rep(0.75, 8),
                             BETA_CONFIGS$trail_t005)
# Calibrated data seed (Fable rule): k = 4 is the first offset exercising the
# mirror-guard (reflected >= 1) and the nonconvergent-exclusion branch; degenerate
# resamples are unreachable at N = 60 (documented by the NOTE below), so k keys on
# reflection per the rule's fallback.
set.seed(BASE_SEED + 777L + 4L)
X <- simulate_dataset(P0, 60L)
colnames(X) <- scale_labels(8L)

set.seed(424242L)                   # path A: the shipped default
fit <- suppressWarnings(cpm_fit(data = as.data.frame(X), scales = scale_labels(8L),
                                angles = octants(), m = 3, boots = 200))
eng <- cpm_engine(stats::cor(X), angles = octants(), m = 3, variant = "A",
                  reference = 1)    # deterministic; consumes no RNG
set.seed(424242L)                   # path B: the engine's reconstruction
reps <- sim_replicates(eng, X, 200L)
pth <- ci_percentile_theta(reps$theta_rad, reps$ok, 0.95)
pz  <- ci_percentile_linear(reps$zeta, reps$ok, 0.95)
pb  <- ci_percentile_linear(reps$beta, reps$ok, 0.95)

ok(max(abs(pth$lci - fit$results$Angle_lci)) == 0 &&
   max(abs(pth$uci - fit$results$Angle_uci)) == 0 &&
   max(abs(pz$lci  - fit$results$Zeta_lci))  == 0 &&
   max(abs(pz$uci  - fit$results$Zeta_uci))  == 0 &&
   max(abs(pb$lci  - fit$betas$Beta_lci))    == 0 &&
   max(abs(pb$uci  - fit$betas$Beta_uci))    == 0,
   "tripwire: percentile CIs byte-identical to cpm_fit(bootstrap)")
ok(fit$details$boots_used == reps$boots_used &&
   fit$details$boots_degenerate == reps$boots_degenerate &&
   fit$details$boots_nonconvergent == reps$boots_nonconvergent &&
   fit$details$boots_reflected == sum(reps$reflected[reps$ok]),
   "tripwire: exclusion/reflection accounting identical")
if (reps$boots_degenerate == 0)
  cat("NOTE: tripwire fixture exercised no degenerate resamples\n")
if (sum(reps$reflected) == 0)
  cat("NOTE: tripwire fixture exercised no mirror-guard reflections\n")
# Fable failure message, for a red run:
#   "PARITY FAILURE: sim_replicates()/percentile constructors no longer
#   reproduce cpm_fit(ci_method='bootstrap') byte-for-byte -- the 'shipped
#   default by reconstruction' claim (README #1, plan sec. 4.1) is broken.
#   Re-sync devel/cpm-sim/intervals.R with R/cpm_fit.R::cpm_bootstrap()."

cat(sprintf("\n%d ok, %d FAIL\n", .n_ok, .n_fail))
if (.n_fail > 0) quit(status = 1L)
