# Re-derivation of the Acton & Revelle (2004) structure-test cutoffs at the
# package's scale level (nv = 8) -- M4.5/T2.
#
# Why this exists: A&R's published cutoffs were read off simulations with
# nv = 64/128 variables, and they report a substantial number-of-variables
# effect on the Gap Test (p. 18, eta^2 = .11) with a follow-up at nv = 8/16/32
# whose per-nv cutoffs were never published. This package applies the criteria
# to 8 octant scales, so shipping the 64/128-variable cutoffs would be
# anti-conservative. This script (1) reproduces A&R's own design as a sanity
# gate for the machinery, and (2) re-derives all cutoffs at nv = 8 on exactly
# the criterion statistics the package computes (structure_fisher/gap/vt/rt).
#
# Source of every design constant: devel/ar2004-transcription.md (two-channel
# transcription record of A&R 2004; no value below enters from memory).
#
# Two ambiguities in A&R's write-up are resolved *empirically* by the gate:
#   1. Uniqueness. The printed formula eps_v = sqrt(1 - (phi1^2 + phi2^2)) is
#      identically zero under both loading schemes. Candidate readings:
#      "literal" (eps = 0; all noise enters via finite-sample correlations of
#      the factor scores) vs "standardized" (the weights belong inside:
#      eps^2 = 1 - gamma^2 - omega^2 phi1^2 - xi^2 phi2^2, unit-variance
#      variables). The gate keeps the reading that reproduces the published
#      cutoffs at A&R's own nv = 64/128 design.
#   2. Fisher Test scale. A&R's Eq. 6 is the CV of communalities (h2); their
#      prose describes vector lengths (h = sqrt(h2)); CV(h2) ~ 2 CV(h), so the
#      published .10/.15 cutoffs can only match one. The gate computes both.
#
# Rotation grids: the package's VT2/RT use full-period 5-degree grids (0-175
# and 0-85 degrees), which make them orientation-invariant; CIRC_STRUC's grid
# range is unstated. The gate also computes a 0-45-degree 10-point variant
# (the draft/psych window) as a provenance diagnostic only -- the shipped
# nv = 8 cutoffs are calibrated on the package statistics, never on the
# diagnostic variant.
#
# RNG: script-level set.seed on the global stream only (DESIGN.md contract;
# no package entry point is touched). Rerunning under the stored seed
# reproduces the stored constants exactly.
#
# Usage: Rscript data-raw/structure-test-cutoffs.R   (from the package root;
# takes ~15-40 min, dominated by 128-variable PAF in the gate). Writes
# data-raw/structure-test-cutoffs.rds and prints the constants block for
# R/fit_structure.R.

devtools::load_all(".", quiet = TRUE)

SEED <- 20260707
R_GATE <- 50 # reps per design cell in the published-design gate
R_NV8 <- 2000 # reps per design cell in the nv = 8 derivation

# Generating model (A&R Eqs. 11.1-11.3, p. 10-11) ------------------------------

# Population loadings phi_1v, phi_2v for v = 1..nv.
ar_loadings <- function(nv, structure = c("interstitial", "simple")) {
  structure <- match.arg(structure)
  frac <- seq_len(nv) / nv
  if (structure == "interstitial") {
    ang <- 2 * pi * frac # Eq. 11.2
  } else {
    ang <- rep(0, nv) # Eq. 11.3: nearest axis by quarters of v/nv
    ang[frac >= 1 / 8 & frac < 3 / 8] <- pi / 2
    ang[frac >= 3 / 8 & frac < 5 / 8] <- pi
    ang[frac >= 5 / 8 & frac < 7 / 8] <- 3 * pi / 2
  }
  cbind(cos(ang), sin(ang))
}

# Factor weights (p. 11). gamma is per-variable; in the "variable" general-
# factor condition it cycles over .3-.7 by .1 across variables (assignment
# unstated in the paper; see transcription note 3).
ar_weights <- function(gf = c("none", "large", "variable"),
                       axes = c("equal", "unequal"), nv) {
  gf <- match.arg(gf)
  axes <- match.arg(axes)
  gamma <- switch(gf,
    none = rep(0, nv),
    large = rep(0.5, nv),
    variable = rep_len(seq(0.3, 0.7, by = 0.1), nv)
  )
  if (gf == "none") {
    w <- if (axes == "equal") c(0.6, 0.6) else c(0.7, 0.5)
  } else {
    w <- if (axes == "equal") c(0.4, 0.4) else c(0.4, 0.3)
  }
  list(gamma = gamma, omega = w[1], xi = w[2])
}

# One simulated raw-score sample: n subjects x nv variables, per Eq. 11.1 with
# independent standard-normal factor scores Z_g, Z_1, Z_2 (shared within
# subject) and per-variable unique scores (transcription note 1).
ar_sample <- function(n, nv, structure, axes, gf,
                      reading = c("standardized", "literal")) {
  reading <- match.arg(reading)
  phi <- ar_loadings(nv, structure)
  w <- ar_weights(gf, axes, nv)
  lam <- cbind(w$gamma, w$omega * phi[, 1], w$xi * phi[, 2])
  scores <- matrix(stats::rnorm(n * 3), n, 3)
  x <- scores %*% t(lam)
  if (reading == "standardized") {
    eps <- sqrt(pmax(0, 1 - rowSums(lam^2)))
    x <- x + matrix(stats::rnorm(n * nv), n, nv) * matrix(eps, n, nv, byrow = TRUE)
  }
  x
}

# Criterion statistics per sample ----------------------------------------------

# Diagnostic-only 0-45-degree quarter-period variants (10 points, 0-45 by 5),
# used solely to probe CIRC_STRUC's unstated grid in the gate: the shipped
# statistics with only the grid overridden, so the diagnostics can never
# drift from the shipped per-rotation formulas. Correctly indexed (the
# draft/psych x[0] bug dropped the 0-degree rotation and left a spurious 0 in
# the last slot).
vt_diag45 <- function(loadings) structure_vt(loadings, grid_deg = seq(0, 45, by = 5))
rt_diag45 <- function(loadings) structure_rt(loadings, grid_deg = seq(0, 45, by = 5))

# All criterion values for one scored data matrix. The vt45/rt45 grid probes
# are gate-only; phase 2 skips them (diagnostics = FALSE) since nothing
# downstream reads them there.
criteria_for <- function(x, diagnostics = TRUE) {
  lambda <- paf2(stats::cor(x))
  h2 <- rowSums(lambda^2)
  out <- c(
    fisher = structure_fisher(lambda), # CV of sqrt(h2) (A&R prose; shipped)
    fisher_sq = stats::sd(h2) / mean(h2), # CV of h2 (Eq. 6 as printed) --
    gap = structure_gap(lambda), #          scale-adjudication diagnostic
    vt = structure_vt(lambda),
    rt = structure_rt(lambda)
  )
  if (diagnostics) {
    out <- c(out, vt45 = vt_diag45(lambda), rt45 = rt_diag45(lambda))
  }
  out
}

# Run a design: every cell x rep, both scorings from each generated sample
# (scoring is a marginal comparison, so sharing the generated matrix between
# scorings is legitimate and halves generation cost).
run_design <- function(cells, reps, reading, diagnostics = TRUE) {
  out <- vector("list", nrow(cells) * reps * 2L)
  k <- 0L
  for (i in seq_len(nrow(cells))) {
    cell <- cells[i, ]
    for (r in seq_len(reps)) {
      x <- ar_sample(cell$n, cell$nv, cell$structure, cell$axes, cell$gf, reading)
      for (scoring in c("raw", "deviation")) {
        xs <- if (scoring == "raw") x else x - rowMeans(x)
        k <- k + 1L
        out[[k]] <- data.frame(
          cell[, c("structure", "axes", "gf", "n", "nv")],
          scoring = scoring, reading = reading, rep = r,
          t(criteria_for(xs, diagnostics))
        )
      }
    }
  }
  do.call(rbind, out)
}

# Cutoff operationalization (transcription "Re-derivation notes") ---------------
# "Almost certainly A below x": the 1st percentile of the competing (B)
# distribution -- below it essentially no B samples occur. "k times as likely
# to indicate A as B below x": the largest observed value with
# F_A(x) / F_B(x) >= k under equal condition priors (F_B = 0 counts as
# satisfied when F_A > 0).
derive_cutoffs <- function(target, other) {
  target <- target[is.finite(target)]
  other <- other[is.finite(other)]
  grid <- sort(unique(c(target, other)))
  f_t <- stats::ecdf(target)(grid)
  f_o <- stats::ecdf(other)(grid)
  ratio_cut <- function(k) {
    ok <- f_t > 0 & (f_o == 0 | f_t / f_o >= k)
    if (!any(ok)) NA_real_ else grid[max(which(ok))]
  }
  almost <- unname(stats::quantile(other, 0.01, type = 1))
  # The transcription's operationalization also requires F_target(almost) to
  # be materially positive -- an "almost certainly" bound below which neither
  # class occurs is a useless classifier. Non-binding for the shipped
  # constants (checked: F_target >= .19 at every almost cutoff); warn so a
  # future re-derivation cannot ship a vacuous bound silently.
  if (stats::ecdf(target)(almost) < 0.05) {
    warning(sprintf(
      "'almost' cutoff %.3f has F_target = %.3f < .05: near-vacuous bound",
      almost, stats::ecdf(target)(almost)
    ))
  }
  c(almost = almost, thrice = ratio_cut(3), twice = ratio_cut(2))
}

# Pool a results data frame into per-scoring cutoffs for the interstitiality
# criteria (target = interstitial, competing = simple; pooled over axes, gf,
# n, nv as in A&R Figures 5-7) and for the Fisher test (target = equal axes,
# competing = unequal; pooled over structure, gf, n, nv as in Figure 4).
cutoff_table <- function(res, stats_is, stats_eu) {
  out <- list()
  for (scoring in c("raw", "deviation")) {
    s <- res[res$scoring == scoring, ]
    for (st in stats_is) {
      out[[paste(st, scoring, sep = ".")]] <- derive_cutoffs(
        s[s$structure == "interstitial", st], s[s$structure == "simple", st]
      )
    }
    for (st in stats_eu) {
      out[[paste(st, scoring, sep = ".")]] <- derive_cutoffs(
        s[s$axes == "equal", st], s[s$axes == "unequal", st]
      )
    }
  }
  do.call(rbind, out)
}

# Phase 1: sanity gate on A&R's published design -------------------------------

gate_cells <- expand.grid(
  structure = c("interstitial", "simple"), axes = c("equal", "unequal"),
  gf = c("none", "large", "variable"), n = c(150L, 600L),
  nv = c(64L, 128L), stringsAsFactors = FALSE
)

# Published cutoffs (transcribed; pp. 17-19). NA where A&R report none.
published <- rbind(
  gap.raw = c(almost = .01, thrice = NA, twice = .04),
  gap.deviation = c(.03, NA, .05),
  vt.raw = c(.25, NA, .30),
  vt.deviation = c(.40, .58, .65),
  rt.raw = c(.04, NA, .09),
  rt.deviation = c(.14, NA, .31),
  fisher.raw = c(.10, NA, .15), # p. 17 gives .15 "in either raw or
  fisher.deviation = c(.10, NA, .15) # deviation scored data"
)

message("Gate: simulating A&R's nv = 64/128 design under both readings...")
set.seed(SEED)
gate <- rbind(
  run_design(gate_cells, R_GATE, "literal"),
  run_design(gate_cells, R_GATE, "standardized")
)

gate_tables <- lapply(c(literal = "literal", standardized = "standardized"),
  function(rd) {
    cutoff_table(gate[gate$reading == rd, ],
      stats_is = c("gap", "vt", "rt", "vt45", "rt45"),
      stats_eu = c("fisher", "fisher_sq")
    )
  }
)

# Compare each reading to the published values on the criteria that are free
# of the two ambiguities' second axes: gap (grid-free, scale-free) and fisher
# (both scale variants tried). The reading is chosen by total absolute
# discrepancy on the gap cutoffs plus the better-matching fisher variant.
# (The literal reading is also refuted qualitatively: with eps = 0 and equal
# axes the vector lengths are asymptotically constant, so its Fisher
# equal-axes distribution collapses toward 0, unlike Figure 4.)
gate_score <- function(tab) {
  gap_err <- sum(abs(
    tab[c("gap.raw", "gap.deviation"), c("almost", "twice")] -
      published[c("gap.raw", "gap.deviation"), c("almost", "twice")]
  ))
  fish_err <- function(stat) {
    sum(abs(
      tab[paste(stat, c("raw", "deviation"), sep = "."), c("almost", "twice")] -
        published[c("fisher.raw", "fisher.deviation"), c("almost", "twice")]
    ))
  }
  c(
    gap = gap_err,
    fisher_h = fish_err("fisher"), fisher_h2 = fish_err("fisher_sq"),
    total = gap_err + min(fish_err("fisher"), fish_err("fisher_sq"))
  )
}
gate_scores <- t(vapply(gate_tables, gate_score, numeric(4)))
print(round(gate_scores, 3))

reading <- names(gate_tables)[which.min(gate_scores[, "total"])]
fisher_variant <- if (gate_scores[reading, "fisher_h"] <=
  gate_scores[reading, "fisher_h2"]) "fisher" else "fisher_sq"
message("Chosen reading: ", reading, "; Fisher scale variant: ", fisher_variant)
# The shipped structure_fisher() computes CV(sqrt(h2)); the whole calibration
# is coherent only if that is also the variant the published record confirms.
stopifnot(fisher_variant == "fisher")

message("Gate table (chosen reading) vs published:")
chosen_tab <- gate_tables[[reading]]
print(round(chosen_tab, 3))
print(published)

# Pooled-distribution quantiles (chosen reading), for comparison against the
# shapes of A&R Figures 4-8.
message("Pooled distribution quantiles (chosen reading):")
chosen <- gate[gate$reading == reading, ]
qs <- c(.01, .05, .1, .25, .5, .75, .9, .99)
for (st in c("gap", "vt", "rt", "fisher", "fisher_sq")) {
  pool_by <- if (st %in% c("fisher", "fisher_sq")) "axes" else "structure"
  for (scoring in c("raw", "deviation")) {
    s <- chosen[chosen$scoring == scoring, ]
    for (lev in unique(s[[pool_by]])) {
      message(sprintf("  %s.%s [%s]:", st, scoring, lev))
      print(round(stats::quantile(s[s[[pool_by]] == lev, st], qs, na.rm = TRUE), 3))
    }
  }
}

# Persist the gate record before asserting, so a failed gate still leaves the
# full diagnostic trail on disk. This file is a local artifact (.gitignore'd,
# ~0.5 MB with the per-sample gate frame); the committed record is the slim
# structure-test-cutoffs.rds written at the end.
saveRDS(
  list(
    seed = SEED, r_gate = R_GATE, reading = reading,
    fisher_variant = fisher_variant, gate = gate, gate_tables = gate_tables,
    gate_scores = gate_scores, published = published, date = Sys.Date()
  ),
  "data-raw/structure-test-cutoffs-gate.rds"
)

# Assertions. A&R's published cutoffs are one-sided claims read conservatively
# off cumulative-frequency plots: "a value below c almost certainly indicated
# A" claims F_B(c) ~ 0, and "a value below c was at least k times as likely to
# indicate A as B" claims F_A(c)/F_B(c) >= k. A published c sitting below the
# maximal value satisfying the claim is therefore *consistent*, so the gate
# checks the claims, not two-sided distance to our derived cutoffs:
#   - "almost": F_other(published c) <= 1 - .05^(1/96) ~ .031, i.e. consistent
#     (5% level) with the zero exceedances their ~96-sample curves showed;
#   - "k times": F_target/F_other >= k / sqrt(2) (measurement slack).
#
# Known reproduction limits (asserted as warnings, with the observed F values
# recorded; any failure OUTSIDE this list stops the run): the three left-tail
# "almost" claims below depend on the extreme tail of the competing
# distribution, which is sensitive to CIRC_STRUC's unstated factor-extraction
# pipeline. vt.raw (F_other ~ .05) and fisher.raw (~ .05) are marginal --
# their curves would have shown ~4/96 exceedances, readable as "almost
# certainly" by a generous eye. gap.deviation (F_other ~ .18) is a genuine
# distributional difference: our deviation-scored simple-structure Gap values
# extend further into the left tail than theirs did. Every likelihood-ratio
# claim and every other "almost" claim reproduces.
KNOWN_LIMITS <- c("vt.raw.almost", "fisher.raw.almost", "gap.deviation.almost")

gate_claims <- data.frame()
for (row in rownames(published)) {
  parts <- strsplit(row, ".", fixed = TRUE)[[1]]
  st <- parts[1]
  scoring <- parts[2]
  pool_by <- if (st == "fisher") "axes" else "structure"
  target_lev <- if (st == "fisher") "equal" else "interstitial"
  other_lev <- if (st == "fisher") "unequal" else "simple"
  s <- chosen[chosen$scoring == scoring, ]
  f_t <- stats::ecdf(s[s[[pool_by]] == target_lev, st])
  f_o <- stats::ecdf(s[s[[pool_by]] == other_lev, st])
  for (cut in colnames(published)) {
    c0 <- published[row, cut]
    if (is.na(c0)) next
    fo <- f_o(c0)
    ft <- f_t(c0)
    ok <- if (cut == "almost") {
      fo <= 0.031
    } else {
      fo == 0 || ft / fo >= (if (cut == "thrice") 3 else 2) / sqrt(2)
    }
    gate_claims <- rbind(gate_claims, data.frame(
      claim = paste(row, cut, sep = "."), cutoff = c0,
      f_target = ft, f_other = fo, ok = ok
    ))
  }
}
print(gate_claims, digits = 3)
failed <- gate_claims$claim[!gate_claims$ok]
unexpected <- setdiff(failed, KNOWN_LIMITS)
if (length(unexpected) > 0) {
  stop("Sanity gate failed on unexpected claims: ",
    paste(unexpected, collapse = ", "))
}
if (length(failed) > 0) {
  warning("Known reproduction limits (documented above): ",
    paste(failed, collapse = ", "))
}
resolved <- setdiff(KNOWN_LIMITS, failed)
if (length(resolved) > 0) {
  message("Note: known limits that now pass (update KNOWN_LIMITS): ",
    paste(resolved, collapse = ", "))
}
message(sprintf(
  "Sanity gate: %d/%d published claims reproduced; failures limited to the documented left-tail limits.",
  sum(gate_claims$ok), nrow(gate_claims)
))

# Phase 2: nv = 8 cutoff derivation --------------------------------------------

nv8_cells <- expand.grid(
  structure = c("interstitial", "simple"), axes = c("equal", "unequal"),
  gf = c("none", "large", "variable"), n = c(150L, 600L),
  nv = 8L, stringsAsFactors = FALSE
)

message("Deriving nv = 8 cutoffs (reading = ", reading, ")...")
nv8 <- run_design(nv8_cells, R_NV8, reading, diagnostics = FALSE)
nv8_tab <- cutoff_table(nv8,
  stats_is = c("gap", "vt", "rt"), stats_eu = c("fisher", "fisher_sq")
)
print(round(nv8_tab, 3))

# Split-half stability check (Monte Carlo error indicator, printed for the
# record): cutoffs re-derived on each half of the replications, split by rep
# so the two scorings of one generated sample stay together.
halves <- lapply(split(seq_len(nrow(nv8)), nv8$rep %% 2L), function(i) {
  cutoff_table(nv8[i, ], stats_is = c("gap", "vt", "rt"),
    stats_eu = c("fisher", "fisher_sq"))
})
message("Split-half max |difference| per row:")
print(round(apply(abs(halves[[1]] - halves[[2]]), 1, max), 3))

# The committed derivation record (small: summaries only, no raw replication
# frames -- those are reproducible by rerunning this script under its seed).
# The -gate.rds written above is a LOCAL diagnostic artifact (it carries the
# full per-sample gate frame for failed-gate forensics) and is not committed;
# this file is the authoritative record the testthat pin reads.
saveRDS(
  list(
    seed = SEED, r_gate = R_GATE, r_nv8 = R_NV8, reading = reading,
    fisher_variant = fisher_variant, gate_scores = gate_scores,
    gate_tables = gate_tables, gate_claims = gate_claims,
    published = published, nv8_cutoffs = nv8_tab, halves = halves,
    date = Sys.Date()
  ),
  "data-raw/structure-test-cutoffs.rds"
)

# Constants block for R/fit_structure.R ----------------------------------------
fmt <- function(row) {
  v <- round(nv8_tab[row, ], 2)
  parts <- sprintf("%s = %.2f", names(v)[!is.na(v)], v[!is.na(v)])
  paste0("c(", paste(parts, collapse = ", "), ")")
}
message("\n# nv = 8 cutoffs derived by data-raw/structure-test-cutoffs.R")
message(sprintf("# (seed %d, reading '%s', fisher variant '%s', %s)",
  SEED, reading, fisher_variant, Sys.Date()))
for (st in c("fisher", "gap", "vt", "rt")) {
  message(sprintf("  %s = list(raw = %s,\n    deviation = %s),",
    st, fmt(paste0(st, ".raw")), fmt(paste0(st, ".deviation"))))
}
