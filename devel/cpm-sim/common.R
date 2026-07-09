# CPM CI simulation study -- shared conventions and constants.
#
# Registered plan: devel/cpm-simulation-paper-plan.md (all "sec." refs below).
# This file holds the pins every module depends on so they cannot drift:
# BASE_SEED, angle/coverage conventions (sec. 2/2.5), and the Bradley decision
# bands (sec. 6.1). Sourced first by run.R; nothing here fits or scores.
#
# STATUS: written 2026-07-08, NOT YET RUN. No factorial cell has been executed.

# The package supplies the estimator + the two internal entry points this study
# drives (cpm_engine, cpm_analytic_se) plus the resampling internals reused by
# the replicate generator (cpm_optimize_one, cpm_gradient, cpm_mirror_guard,
# cpm_unpack, cpm_ref_relative, cpm_hessian_fd). load_all exposes them; no
# package code is changed by this study (sec. 10).
if (!exists(".CPM_SIM_LOADED")) {
  suppressMessages(devtools::load_all(
    getOption("cpm_sim_pkg", "."), quiet = TRUE, export_all = TRUE
  ))
  .CPM_SIM_LOADED <- TRUE
}

# ---- pinned reproducibility contract (sec. 6.3, sec. 7.1) -------------------
# Chosen at design time, disjoint from 20260706 (B6) and 20260708 (G). Never
# changed; any re-run under a different seed is reported as such. Per-replicate
# local set.seed(BASE_SEED + offset) makes every result worker-count- and
# schedule-independent (no L'Ecuyer streams needed -- sec. 7.1).
BASE_SEED <- 20260710L

# Per-replicate seed offset multiplier (kernel.R): offset = SEED_MULT*cell_index
# + i must be unique per (cell, replicate) AND keep BASE_SEED + offset inside
# set.seed()'s 32-bit integer range (review M1). 50000 covers any per-cell rep
# count (incl. firing-sized provocation cells + the sec. 6.3.6 top-up block) and
# tops out near BASE_SEED + 50000 * n_cells << 2^31 for the ~800-cell table.
SEED_MULT <- 50000L
SEED_MAX_I <- 49999L                # per-cell replicate/offset ceiling (assert)

# ---- nominal levels (sec. 3.2) ----------------------------------------------
# 95% primary; 90%/99% are extra quantiles over the SAME replicate sets.
LEVELS <- c(0.90, 0.95, 0.99)
PRIMARY_LEVEL <- 0.95

# ---- angle conventions (sec. 2; CLAUDE.md invariants) -----------------------
# API degrees [0, 360), LM = 360. The reference scale is fixed (not estimated)
# and is EXCLUDED from angle coverage everywhere (sec. 2).

# Signed shortest rotation a -> b in degrees, in (-180, 180] (contrast/error
# convention). Used for analytic-Wald angle coverage and angular error.
ang_signed <- function(a, b) -((a - b + 180) %% 360 - 180)

# Circular CI membership by the anchor-free SPAN rule (B6): truth lies on the
# CCW arc lci -> uci. Needs no anchor, so it is correct for wrapped bootstrap
# intervals (lci > uci) and for a truth at the 0/360 pole (sec. 2.5). Vectorized
# over equal-length lci/uci/truth.
angle_covered <- function(lci, uci, truth) {
  ((truth - lci) %% 360) <= ((uci - lci) %% 360)
}

# For a MISS, which side did the interval fall on? The plan scores the shorter
# angular direction from the interval; an exact tie is attributed to the upper
# (CCW, uci) side, consistent with the package's (-180, 180] convention
# including +180 (sec. 5.1). Returns "lower"/"upper"/NA(covered), vectorized.
angle_miss_side <- function(lci, uci, truth) {
  covered <- angle_covered(lci, uci, truth)
  d_lo <- abs(ang_signed(truth, lci))   # angular gap truth->lci
  d_hi <- abs(ang_signed(truth, uci))   # angular gap truth->uci
  side <- ifelse(d_lo < d_hi, "lower", "upper")   # tie -> upper
  side[covered] <- NA_character_
  side
}

# Linear (real-line) coverage + miss side for zeta/beta, vectorized.
lin_covered   <- function(lci, uci, truth) lci <= truth & truth <= uci
lin_miss_side <- function(lci, uci, truth) {
  ifelse(lci <= truth & truth <= uci, NA_character_,
         ifelse(truth < lci, "lower", "upper"))
}

# ---- Bradley (1978) decision bands (sec. 6.1) -------------------------------
# Two-sided liberal band for a nominal level L: L +/- 0.5*(1 - L).
bradley_band <- function(level = PRIMARY_LEVEL) {
  half <- 0.5 * (1 - level)
  c(lower = level - half, upper = level + half)
}
# Per-side band for a one-sided miss rate against its nominal alpha/2:
# [0.5*(alpha/2), 1.5*(alpha/2)] (sec. 6.1).
bradley_side_band <- function(level = PRIMARY_LEVEL) {
  a2 <- (1 - level) / 2
  c(lower = 0.5 * a2, upper = 1.5 * a2)
}

# ---- small shared utilities -------------------------------------------------
# Generic scale labels; the math never depends on them, but cpm_fit wants a
# names vector of length p.
scale_labels <- function(p) sprintf("V%02d", seq_len(p))

# Degrees -> internal radians, wrapped to [0, 2*pi) (the engine's entry
# convention). Accepts a plain numeric degree vector.
deg2rad_wrapped <- function(deg) as.numeric(as_radian(as_degree(deg))) %% (2 * pi)
