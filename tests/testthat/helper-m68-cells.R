# M68 -- the complete-data calibration harness for the scaled global test
# statistic: the three populations, the seed formula, and the one-replicate
# function that produced every row of fixtures/m68-scaled-fit-cells.rds.
#
# Auto-sourced by testthat; also source()d by devel/m68-scaled-fit-cells.R,
# which is the generator (the helper-ssm-sem.R / devel/m5-coverage-oracle.R
# pattern this repo already uses for the same reason).
#
# It lives HERE rather than in devel/ on two grounds, both from the M68 review.
# F7: AC14 asks the smoke cell to run "the generator's replicate function", and
# a cell that re-implements the replicate inline on different seeds cannot catch
# the one thing a smoke cell is for -- the harness and the package drifting
# apart, which is invisible when the harness is a copy. F6: the exact-
# reproduction arm of AC9 has to re-run individual committed replicates from
# their own pinned seeds, which is only meaningful against the very function
# that produced them. A devel/ file would give neither, because devel/ is
# .Rbuildignore'd and absent from the built tarball the suite is checked in.
#
# Nothing here is package code: it is test-side, and the package never calls it.

# The three populations AC7 names.
#
# `strong` is RR13's own probe population, where E[T] = 261.1 against df = 273
# was measured. `weak` is Strack et al. (2013) Table 3's COC Sample 16 Other row
# -- %gen 46.7, %axes 3.2, %item 50.1 over 16 single-item positions
# (strack2013.md, p. 7) -- a real published instrument at the weak-axes,
# strong-general corner, where the metric distortion is largest. `antic` is the
# anti-conservative corner the plan names: weak axes, a dominating general
# factor, and a large item count, so df is big and the eigenvalue dispersion
# that drives the tail behaviour has the most room to show.
m68_pops <- list(
  strong = list(
    label = "strong-axes (RR13 probe: 8 scales x 3 items)",
    angles = octants(), k = 3L, xi1 = .35, xi2 = .10, zeta1 = .08, n = 600L
  ),
  weak = list(
    label = "weak-axes/strong-general (Strack Table 3, COC S16 Other)",
    angles = as_degree(seq(22.5, 360, by = 22.5)), k = 1L,
    xi1 = .032, xi2 = .467, zeta1 = 0, n = 600L
  ),
  antic = list(
    label = "anti-conservative corner (12 scales x 3 items, xi1 = .05)",
    angles = as_degree(seq(30, 360, by = 30)), k = 3L,
    xi1 = .05, xi2 = .60, zeta1 = .05, n = 600L
  )
)

m68_pop_items <- function(p, nm) split(nm, rep(seq_along(p$angles), each = p$k))

# The seed formula, in ONE place. Every replicate seeds itself from its own
# pinned seed, so a result does not depend on the worker count or on scheduling
# order -- and a single stored replicate can be re-run in isolation, which is
# what AC9's exact-reproduction arm needs.
m68_seeds <- function(what, reps) {
  base <- switch(what,
    strong = 10000L, weak = 20000L, antic = 30000L,
    stop("unknown cell: ", what)
  )
  base + seq_len(reps)
}

# The sample-size sweep's seeds are offset by N, so the four sweep cells never
# collide with each other or with the three population cells above.
m68_sweep_seeds <- function(n, reps) 40000L + as.integer(n) + seq_len(reps)

# Does this machine match the environment the fixture was generated under?
#
# Bit-exact replay of a stored replicate is a claim about THIS R and THIS
# lavaan and nothing else. `axes_simulate()` -> `cor()` -> lavaan's optimizer
# amplifies last-bit LAPACK/BLAS differences well past any useful bar (a 1e-15
# relative perturbation of one correlation moves the scaled chi-square by
# ~9e-12), so on a platform with a different numeric library the comparison
# fails for a reason that says nothing about this package. Every exact-replay
# assertion is gated on this; the assertions that survive a changed environment
# are the drift fences and the direction checks, which are gated on nothing.
m68_env_matches <- function(fx) {
  identical(fx$provenance$r_version, R.version.string) &&
    identical(fx$provenance$lavaan_version,
              as.character(utils::packageVersion("lavaan")))
}

# One replicate, reduced to what the criteria consume. Both p-values are stored:
# `p` is what the package now reports and `p_unscaled` is what lavaan reported
# before the scaling, so the "with the unscaled rate recorded alongside" clause
# needs no second run.
m68_one_rep <- function(p, seed) {
  set.seed(seed)
  mat <- as.matrix(axes_simulate(p$n, p$angles, p$k, p$xi1, p$xi2, p$zeta1))
  res <- tryCatch(
    suppressMessages(suppressWarnings(
      axes_reliability(as.data.frame(mat),
                       items = m68_pop_items(p, colnames(mat)),
                       angles = p$angles)
    )),
    error = function(e) NULL
  )
  if (is.null(res) || !is.null(res$details$fit_scaling_failed)) {
    return(c(chisq = NA_real_, chisq_scaled = NA_real_, df = NA_real_,
             p = NA_real_, p_unscaled = NA_real_, cfactor = NA_real_))
  }
  c(
    chisq = res$details$fit_uncorrected$chisq,
    chisq_scaled = res$fit$chisq,
    df = res$fit$df,
    p = res$fit$pvalue,
    p_unscaled = res$details$fit_uncorrected$pvalue,
    cfactor = unname(res$details$scaling_factor[["model"]])
  )
}
