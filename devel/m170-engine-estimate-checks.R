# M170 T1: checks on devel/m170-engine-estimate-helpers.R, run before the
# study. Usage: Rscript devel/m170-engine-estimate-checks.R
#
#   1. H reaches RR27's six lower points: on boundary-truth N = 250
#      replicates 11, 21, 26, 28, 30 and 31 of M167's raw-data seeds, H finds
#      a discrepancy below E's by at least 0.325 T units (RR27's smallest
#      reported gap among the six, 0.33, less rounding), and on replicate 26
#      by at least 3.275 (RR27's heavier search there, 3.28). T = (N - 1) F.
#   2. W's starts and W's result are the same on every run: two calls under
#      different random states give identical starts, F and estimates, the
#      caller's random stream is left as it was, and every W start is finite
#      on a fit with a communality at exactly 1 (RR28).
#   3. The finish step matches the engine: refitting E's own estimate from
#      the full layout and finishing it gives E's F (within 1e-8) and
#      estimates (within 1e-4), so W and H are finished the way E is. The
#      fits include boundary N = 250 replicate 6, whose E has a communality
#      at exactly 1 in double precision; the check confirms it does.
#   4. A throwing start is skipped: a start whose refit throws is recorded
#      as F = NA and the other starts still run.
# Each check prints PASS or FAIL; the script stops on any FAIL.

devtools::load_all(".", quiet = TRUE)
source(file.path("devel", "m170-engine-estimate-helpers.R"))

angles <- octants()
p <- 8
scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
P0 <- cpm_implied_cor(as.numeric(as_radian(as_degree(angles))), rep(0.75, p),
                      c(.45, .35, .15, .05))
truth_fit <- cpm_fit(cormat = P0, scales = scales, angles = angles,
                     n = 10000, m = 3)
# M167's raw-data seed for boundary-truth N = 250 replicate i
m167_R <- function(i) {
  set.seed(20260706 + 1e6 * 1 + 1e3 * 1 + i)
  stats::cor(cpm_simulate(truth_fit, 250))
}
K <- 100L
verdicts <- c()
report <- function(name, ok, detail) {
  cat(sprintf("%-4s %s: %s\n", if (ok) "PASS" else "FAIL", name, detail))
  verdicts[[name]] <<- ok
}

# ---- 1 ----
cases <- c(11, 21, 26, 28, 30, 31)
gaps <- unlist(parallel::mclapply(cases, function(i) {
  R <- m167_R(i)
  eng <- m170_engine_e(R, angles, 3)
  if (is.null(eng) || !isTRUE(eng$accepted)) return(NA_real_)
  est <- m170_estimators(R, eng, 3, K = K, seed = 60261027 + i, n1 = 249)
  249 * (est$F_E - est$F_H)
}, mc.cores = min(length(cases), max(1, parallel::detectCores() - 1))))
names(gaps) <- cases
report("1 H reaches RR27's six lower points",
       all(!is.na(gaps)) && all(gaps >= 0.325) && gaps[["26"]] >= 3.275,
       paste(sprintf("rep %s %.3f", names(gaps), gaps), collapse = ", "))

# ---- 2 ----
# replicate 6's E has a communality at exactly 1 (asserted in check 3)
R <- m167_R(6)
eng <- m170_engine_e(R, angles, 3)
spec <- m170_full_spec(eng, 3)
g0 <- m170_theory_par(R, eng, spec, 3)
set.seed(1)
s1 <- m170_w_starts(m170_base_par(eng, spec), g0, spec)
a1 <- m170_estimators(R, eng, 3, K = 2L, seed = 7, n1 = 249)
set.seed(99)
s2 <- m170_w_starts(m170_base_par(eng, spec), g0, spec)
a2 <- m170_estimators(R, eng, 3, K = 2L, seed = 8, n1 = 249)
set.seed(1)
restored <- {
  state_before <- .Random.seed
  invisible(m170_estimators(R, eng, 3, K = 2L, seed = 7, n1 = 249))
  identical(state_before, .Random.seed)
}
finite <- all(vapply(s1, function(g) all(is.finite(g)), logical(1)))
report("2 W is the same on every run",
       identical(s1, s2) && identical(a1$F_W, a2$F_W) &&
         identical(a1$W$par, a2$W$par) && restored && finite &&
         length(s1) == 101,
       sprintf(paste("%d starts, all finite %s, identical starts %s,",
                     "identical W %s, stream restored %s"),
               length(s1), finite, identical(s1, s2),
               identical(a1$F_W, a2$F_W) && identical(a1$W$par, a2$W$par),
               restored))

# ---- 3 ----
diffs <- vapply(c(1:6, cases), function(i) {
  R <- m167_R(i)
  eng <- m170_engine_e(R, angles, 3)
  spec <- m170_full_spec(eng, 3)
  r <- cpm_optimize_one(m170_base_par(eng, spec), R, spec)
  fin <- m170_finish(r$par, r$F, R, spec, eng)
  ang <- abs(((fin$theta - eng$theta + 180) %% 360) - 180)
  c(F = abs(fin$F - eng$F), est = max(ang / 360, abs(fin$zeta - eng$zeta),
                                      abs(fin$beta - eng$beta)),
    exact_wall = any(eng$zeta == 1))
}, numeric(3))
report("3 finish step matches the engine",
       all(diffs["F", ] <= 1e-8) && all(diffs["est", ] <= 1e-4) &&
         any(diffs["exact_wall", ] == 1),
       sprintf(paste("max |dF| %.1e, max estimate difference %.1e over %d",
                     "fits, %d with a communality at exactly 1"),
               max(diffs["F", ]), max(diffs["est", ]), ncol(diffs),
               sum(diffs["exact_wall", ])))

# ---- 4 ----
# a NaN start makes nlminb() throw before its first step; the check confirms
# the throw is that error, then that the run set records it and goes on
throwing <- m170_base_par(eng, spec)
throwing[spec$i_zeta] <- NaN
err <- tryCatch(cpm_optimize_one(throwing, R, spec), error = conditionMessage)
runs <- m170_run_starts(list(throwing, m170_base_par(eng, spec)), R, spec)
report("4 a throwing start is skipped",
       identical(err, "NA/NaN gradient evaluation") && is.na(runs$F[1]) &&
         is.finite(runs$F[2]),
       sprintf("refit error \"%s\"; F = %s", err,
               paste(signif(runs$F, 6), collapse = ", ")))

if (!all(unlist(verdicts))) stop("a check failed")
cat("all checks passed\n")
