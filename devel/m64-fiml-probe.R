# M64 probe: evidence for RB12 (FIML on items for axes_reliability())
# ==============================================================================
# Reproduces the four findings RB12 quotes. Run from the repo root:
#
#   Rscript devel/m64-fiml-probe.R
#
# Provenance: authored at the M64 plan gate (2026-07-26) from three throwaway
# scratchpad probes, consolidated here so RB12's reviewer and any later build
# can re-run them. Seeds are pinned inline; every figure below is printed by
# this script, never quoted from memory.
#
# Reads the DEV tree via devtools::load_all() and prints packageVersion() to
# prove which tree answered (the M21/M34 lesson: a bare library() call reads the
# INSTALLED package).
# ==============================================================================

if (!requireNamespace("lavaan", quietly = TRUE)) {
  stop("This probe needs lavaan (a Suggests dependency).", call. = FALSE)
}
suppressMessages(devtools::load_all(quiet = TRUE))
cat("circumplex tree:", as.character(packageVersion("circumplex")), "\n")
cat("lavaan:", as.character(packageVersion("lavaan")), "\n\n")

rule <- function(title) cat("\n== ", title, " ", strrep("=", 60 - nchar(title)),
                            "\n", sep = "")

# --- Fixture: one population, one seed, used by findings 2-4 -------------------
# 8 octant scales x 3 items = 24 items, drawn from the exact five-component
# population axes_population_cor() defines. Truth: xi1 = .35, xi2 = .10,
# zeta1 = .08 (no blocks).
set.seed(7)
ang <- octants()
n_items <- 3L
truth <- c(xi1 = .35, xi2 = .10, zeta1 = .08)
dat <- axes_simulate(600, ang, n_items, xi1 = truth[["xi1"]],
                     xi2 = truth[["xi2"]], zeta1 = truth[["zeta1"]])
items <- split(colnames(dat), rep(seq_along(ang), each = n_items))
syn <- circumplex:::axes_syntax(items, ang)
mat <- as.matrix(dat)
p <- ncol(mat)

# Drop each cell independently with probability `rate` (MCAR), seed fixed per
# rate so every run of this script reports the same numbers.
mcar <- function(m, rate, seed) {
  set.seed(seed)
  m[matrix(stats::runif(length(m)) < rate, nrow(m))] <- NA
  m
}

# xi1 and its SE from a fitted model, the one quantity every finding compares.
xi1_of <- function(fit) {
  pe <- lavaan::parameterEstimates(fit)
  row <- pe$op == "~~" & pe$lhs == "AX" & pe$rhs == "AX"
  c(est = pe$est[row][[1]], se = pe$se[row][[1]])
}

# ==============================================================================
# FINDING 1 — the cost of listwise deletion at item level
# ==============================================================================
# Analytic, no simulation: under per-item MCAR at rate r, a respondent survives
# listwise deletion with probability (1 - r)^p. The point is that p is the ITEM
# count, not the scale count, so the exponent is large for any real instrument.
rule("F1: expected complete-case share, (1 - rate)^p")
rates <- c(0.01, 0.02, 0.05, 0.10, 0.15)
shares <- t(vapply(c(24L, 64L),
                   function(pp) (1 - rates)^pp, numeric(length(rates))))
dimnames(shares) <- list(sprintf("p = %d items", c(24L, 64L)),
                         sprintf("%.0f%%", rates * 100))
print(round(shares, 3))
cat("\n64 items is a realistic 8 x 8 instrument; 1% per-item MCAR already\n",
    "deletes about half the respondents.\n", sep = "")

# The shipped contract does not degrade here, it REFUSES: complete-case N must
# exceed the item count. 15% per-item MCAR on 600 respondents lands under it.
rule("F1b: the shipped function's behavior at 15% per-item MCAR")
m15 <- mcar(mat, 0.15, seed = 115)
cat("complete cases:", sum(stats::complete.cases(m15)), "of", nrow(m15),
    "| items:", p, "\n")
refusal <- tryCatch(
  axes_reliability(data = as.data.frame(m15), items = items, angles = ang),
  error = function(e) conditionMessage(e)
)
cat("axes_reliability() says:", if (is.character(refusal)) refusal else
  "(no error -- FINDING STALE)", "\n")

# ==============================================================================
# FINDING 2 — lavaan saturates the mean structure itself under missing = "ml"
# ==============================================================================
# The candidate row's worry was that the emitted syntax carries no intercepts
# while missing = "ml" implies a mean structure. lavaan frees every item
# intercept on its own, so the mean structure is SATURATED: npar rises by p and
# df does not move. Nothing to specify, nothing restricted.
#
# Count the intercepts from coef() names, NOT from a `free` column of
# parameterEstimates() -- that column does not exist, so the scratchpad probe's
# `pe$free > 0` filter silently counted zero (the LESSONS (f) family: the
# probe's own syntax lied).
rule("F2: mean structure under FIML is saturated")
n_int <- function(fit) sum(names(lavaan::coef(fit)) %in%
                             paste0(colnames(dat), "~1"))
fit_lw <- lavaan::cfa(syn, data = as.data.frame(scale(mat)), estimator = "ML",
                      se = "standard", missing = "listwise", orthogonal = TRUE)
m05 <- mcar(mat, 0.05, seed = 105)
fit_ml <- suppressWarnings(
  lavaan::cfa(syn, data = as.data.frame(scale(m05)), estimator = "ML",
              se = "standard", missing = "ml", orthogonal = TRUE)
)
for (nm in c("listwise (complete data)", "FIML (5% per-item MCAR)")) {
  f <- if (startsWith(nm, "listwise")) fit_lw else fit_ml
  cat(sprintf("%-26s meanstructure %-5s npar %2.0f  df %3.0f  free intercepts %2d\n",
              nm, lavaan::lavInspect(f, "options")$meanstructure,
              lavaan::fitMeasures(f, "npar"), lavaan::fitMeasures(f, "df"),
              n_int(f)))
}
cat("p =", p, "items, so npar rises by exactly p and df is unchanged.\n")

# ==============================================================================
# FINDING 3 — the unit-diagonal departure is NOT introduced by FIML
# ==============================================================================
# The model's implied per-item variances depart from 1 on COMPLETE z-scored data
# already: free per-item errors do not force exact diagonal reproduction under a
# restricted ML fit (the stationarity condition is on Sigma^-1 (S - Sigma)
# Sigma^-1, not on S - Sigma). So the "unit-variance items" concern is a
# property of the shipped estimator, not a cost of switching to FIML.
rule("F3: implied per-item variance departure from 1")
dep <- function(fit) max(abs(diag(lavaan::lavInspect(fit, "implied")$cov) - 1))
cat(sprintf("complete data, listwise : max |v - 1| = %.4f\n", dep(fit_lw)))
for (r in c(0.02, 0.05, 0.10)) {
  f <- suppressWarnings(lavaan::cfa(
    syn, data = as.data.frame(scale(mcar(mat, r, seed = 100 + r * 100))),
    estimator = "ML", se = "standard", missing = "ml", orthogonal = TRUE
  ))
  cat(sprintf("%3.0f%% per-item MCAR, FIML: max |v - 1| = %.4f\n",
              r * 100, dep(f)))
}
# And the REPORTED components still sum to ~1, because the components table
# carries mean(eps) rather than the per-item errors.
comp <- axes_reliability(data = dat, items = items, angles = ang)$components
cat("\nreported components sum (complete data):",
    sprintf("%.4f\n", sum(comp$Estimate)))

# ==============================================================================
# FINDING 4 — one-stage FIML vs a two-stage FIML-correlation route
# ==============================================================================
# One-stage: standardized rows straight to lavaan with missing = "ml" (the
# sem_fit_cfa pattern RR09 sec. 7.5 names).
# Two-stage: a FIML correlation matrix (lavCor) fed to the M59 cormat path.
# Listwise is carried as the reference arm.
#
# The difference is reported ABSOLUTELY and as a fraction of xi1's own SE. The
# fraction is the honest statement of the claim "the two routes agree": the
# alternative -- that they materially disagree -- would put them about one SE
# apart, so a difference of a few percent of an SE is the discriminating
# comparison and absorbs platform noise on both sides (the M59/M61 lesson).
rule("F4: xi1 by route, and route-to-route agreement")
cat(sprintf("truth xi1 = %.2f\n\n", truth[["xi1"]]))
cat(sprintf("%5s %5s | %-17s | %-17s | %-17s | %s\n", "rate", "cc",
            "one-stage FIML", "two-stage FIML", "listwise",
            "|1st - 2nd| (as % of SE)"))
for (r in c(0.02, 0.05, 0.10)) {
  m <- mcar(mat, r, seed = 100 + r * 100)
  cc <- sum(stats::complete.cases(m))
  z <- as.data.frame(scale(m))

  a <- xi1_of(suppressWarnings(lavaan::cfa(
    syn, data = z, estimator = "ML", se = "standard", missing = "ml",
    orthogonal = TRUE
  )))
  Rf <- lavaan::lavCor(z, missing = "ml", output = "cor")[colnames(dat),
                                                         colnames(dat)]
  b <- xi1_of(suppressWarnings(lavaan::cfa(
    syn, sample.cov = Rf, sample.nobs = nrow(z), estimator = "ML",
    se = "standard", orthogonal = TRUE
  )))
  l <- xi1_of(suppressWarnings(lavaan::cfa(
    syn, sample.cov = stats::cor(m[stats::complete.cases(m), , drop = FALSE]),
    sample.nobs = cc, estimator = "ML", se = "standard", orthogonal = TRUE
  )))
  d <- abs(a[["est"]] - b[["est"]])
  cat(sprintf("%4.0f%% %5d | %.4f (%.4f) | %.4f (%.4f) | %.4f (%.4f) | %.5f (%.1f%%)\n",
              r * 100, cc, a[["est"]], a[["se"]], b[["est"]], b[["se"]],
              l[["est"]], l[["se"]], d, 100 * d / a[["se"]]))
}
cat("\nEstimates are printed as est (SE). Listwise SE inflates as deletion\n",
    "bites while both FIML SEs hold; the two FIML routes track each other to\n",
    "a small fraction of one SE.\n", sep = "")

# ==============================================================================
# FINDING 5 — what base::scale() does to an NA-containing matrix
# ==============================================================================
# Added 2026-07-26 at the M64 review (finding F1). RB12 quotes a figure for this
# ("|mean| <= 6e-17, |SD - 1| <= 9e-16") and asserts this script reproduces
# every figure it quotes, but the check lived only in a plan-gate scratchpad
# probe and did not survive consolidation. RB12 is archived history and cannot
# be edited, so the check is added here instead, which makes the archived claim
# true rather than restating it.
#
# The point: scale() centers and scales COLUMN-WISE with na.rm, so on incomplete
# data each column's AVAILABLE-CASE mean is 0 and SD is 1 to machine precision.
# That is exactly why the construction looks harmless and exactly why RR12 sec. 1
# rejects it -- available-case moments are consistent only under MCAR, and the
# machine-precision agreement below says nothing about which population those
# moments estimate.
rule("F5: scale() on an NA matrix = available-case standardization")
for (r in c(0.02, 0.05, 0.10)) {
  z <- scale(mcar(mat, r, seed = 100 + r * 100))
  cat(sprintf(
    "%3.0f%% per-item MCAR: max |column mean| = %.3g, max |column SD - 1| = %.3g\n",
    r * 100, max(abs(colMeans(z, na.rm = TRUE))),
    max(abs(apply(z, 2, stats::sd, na.rm = TRUE) - 1))
  ))
}
cat("Machine precision on both, at every rate -- the standardization is exact\n",
    "for the available cases and silent about whether those are the right\n",
    "moments (RR12 sec. 1).\n", sep = "")

# ==============================================================================
# RR12 REVIEWER PROBES (V-C, V-D, V-F)
# ==============================================================================
# Added 2026-07-26 at M65 T8, discharging RR12 recommendation B-6: the reviewer
# probes should be reproducible by the same one-command route the brief used.
#
# Six of the nine reviewer probes are already reproducible without this file,
# because M65 turned them into assertions the suite runs on every check --
# V-A (observed information) and V-B (complete-data identities) in the AC4 and
# BC2/BC6/AC3 tests, V-E and V-G in devel/m65-fiml-heavy-cells.R's stored cells,
# V-H in the BC14 test, V-I in the BC15 test. Three are left, and they are left
# for a reason: each one measures something the SHIPPED code deliberately never
# does, so no test of shipped behaviour can carry them.

rule("V-C: stationarity is the WEIGHTED diagonal")
# Why this is not a defect. The classical "ML reproduces the diagonal exactly"
# result of exploratory factor analysis needs the free-loadings stationarity
# equations, and this model's loadings are FIXED. What a free item error e_i
# actually satisfies is dF/de_i ~ [Sinv (S - Sigma) Sinv]_ii = 0 -- the
# weighted diagonal. So off-diagonal sampling misfit has nowhere to go but the
# raw diagonal, and lands there at roughly the sampling SE of a correlation.
{
  z <- scale(mat)
  fit <- suppressWarnings(lavaan::cfa(syn, data = as.data.frame(z),
                                      estimator = "ML", se = "standard",
                                      orthogonal = TRUE))
  S <- lavaan::lavInspect(fit, "sampstat")$cov
  Sig <- lavaan::lavInspect(fit, "implied")$cov
  Sinv <- solve(Sig)
  cat(sprintf("  max |diag(S - Sigma)|                 = %.4f\n",
              max(abs(diag(S - Sig)))))
  cat(sprintf("  max |diag(Sinv (S - Sigma) Sinv)|     = %.2e\n",
              max(abs(diag(Sinv %*% (S - Sig) %*% Sinv)))))
  cat(sprintf("  sampling SE of a correlation at N=%d  = %.4f\n",
              nrow(mat), 1 / sqrt(nrow(mat))))
  cat("  The weighted diagonal is zero to optimizer precision while the raw\n",
      "  one departs at the order of 1/sqrt(N). Expected restricted-ML\n",
      "  behaviour (M64-D3), documented in the roxygen and the vignette.\n",
      sep = "")
}

rule("V-D: the departure shrinks like sampling error")
# The discriminating check. A sampling artefact must shrink like sampling
# error; a misspecification would not. If this row ever stops shrinking, the
# reassuring story above is wrong and the sentence in the docs must go.
for (n in c(600L, 2400L, 9600L)) {
  set.seed(7)
  d <- as.matrix(axes_simulate(n, ang, n_items, xi1 = truth[["xi1"]],
                               xi2 = truth[["xi2"]], zeta1 = truth[["zeta1"]]))
  f <- suppressWarnings(lavaan::cfa(syn, data = as.data.frame(scale(d)),
                                    estimator = "ML", se = "standard",
                                    orthogonal = TRUE))
  cat(sprintf("  N = %5d: max |diag(S - Sigma)| = %.4f   (1/sqrt(N) = %.4f)\n",
              n, max(abs(diag(lavaan::lavInspect(f, "sampstat")$cov -
                                lavaan::lavInspect(f, "implied")$cov))),
              1 / sqrt(n)))
}
{
  pop <- circumplex:::axes_population_cor(ang, n_items, truth[["xi1"]],
                                          truth[["xi2"]], truth[["zeta1"]])$sigma
  dimnames(pop) <- list(colnames(mat), colnames(mat))
  # sample.nobs large enough that lavaan's default (N-1)/N rescaling of
  # sample.cov is itself below the departure being measured -- at N = 10000 it
  # alone would put a 1e-4 floor under this row and hide the answer.
  f <- suppressWarnings(lavaan::cfa(syn, sample.cov = pop, sample.nobs = 1e7,
                                    estimator = "ML", se = "standard",
                                    orthogonal = TRUE))
  # Reordered by name: lavaan returns the implied matrix in its own variable
  # order, which is not the item-map order `pop` is built in.
  imp <- lavaan::lavInspect(f, "implied")$cov[rownames(pop), colnames(pop)]
  cat(sprintf("  population matrix: max |diag(S - Sigma)| = %.3g, xi1 = %.6f\n",
              max(abs(diag(pop - imp))), xi1_of(f)[["est"]]))
  cat("  No sampling error, no departure, and xi1 recovered exactly: the\n",
      "  departure above is sampling misfit leaking through the fixed\n",
      "  loadings, not a wrong model.\n", sep = "")
}

rule("V-F: lavaan fabricates an unobserved moment")
# The justification for BC7 clause (iii), and the reason that clause refuses
# rather than warns. The saturated likelihood is flat in a moment no respondent
# contributed to, so EM returns its start value -- zero -- and reports nothing.
# The shipped code refuses this input, so only a probe can show what it would
# otherwise have returned.
{
  m <- mat
  half <- seq_len(nrow(m) / 2)
  m[half, 1] <- NA
  m[-half, 4] <- NA
  pop <- circumplex:::axes_population_cor(ang, n_items, truth[["xi1"]],
                                          truth[["xi2"]], truth[["zeta1"]])$sigma
  f <- suppressWarnings(lavaan::lavCor(as.data.frame(m), ordered = character(0),
                                       missing = "ml", output = "fit",
                                       meanstructure = TRUE))
  rhat <- stats::cov2cor(lavaan::lavInspect(f, "h1")$cov)
  cat(sprintf("  respondents observing BOTH item 1 and item 4 : %d\n",
              sum(!is.na(m[, 1]) & !is.na(m[, 4]))))
  cat(sprintf("  lavaan's r(1,4)                              : %.4f\n",
              rhat[1, 4]))
  cat(sprintf("  population r(1,4)                            : %.4f\n",
              pop[1, 4]))
  cat("  No warning, no error, no NA -- a fabricated moment inside an\n",
      "  otherwise ordinary correlation matrix. axes_reliability() refuses\n",
      "  this input before the EM stage (BC7 clause iii).\n", sep = "")
}
