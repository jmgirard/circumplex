# RB26: Design of the CPM interval-coverage study, before its full run (M167)

- **Date:** 2026-10-21
- **Output required:** write findings to `cairn/reviews/RR26-cpm-interval-oracle-design.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained. Do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`circumplex` is an R package on CRAN. Its `cpm_fit()` fits Browne's (1992)
circular stochastic process model to a correlation matrix of p scales. The
parameters are one angle per scale (the reference scale's angle is fixed), one
communality index zeta_i in (0, 1) per scale, and the weights beta_0..beta_m
of the correlation function rho(delta) = sum_k beta_k cos(k delta), with
beta_k >= 0 and sum 1. The engine fits by maximum likelihood on internal
parameters: angles unconstrained, zeta by logit, beta by softmax with v_0 = 0.
A weight whose estimate falls under 1e-2 is removed and the model refit (a
"polish"); its estimate is then exactly 0.

Two interval methods ship. On raw data the default is a percentile bootstrap:
rows resampled, warm-started refits, a circular quantile for angles, plain
quantiles for zeta and beta. On correlation-matrix input only Wald intervals
exist: estimate +/- 1.96 SE, the SE from a finite-difference Hessian with the
delta method to the natural scale.

A 2026-07 coverage study (M4) measured nominal-95% coverage at two truths:
p = 8 octant scales, zeta = .75 for every scale, m = 3, and beta =
.45/.35/.15/.05 ("boundary") or .35/.30/.20/.15 ("interior"), 500 replicates,
N = 250/500/1000, plus a Wald-only ladder to N = 50000. It pooled every
component of a parameter family into one figure. Its results, which the
package's `summary()` cautions rest on: pooled bootstrap beta coverage
.77 to .79 at the boundary truth at every N, .88 to .89 at the interior truth;
bootstrap zeta .76 to .82 at N = 250; Wald angles .70 to .81 at the boundary
truth through N = 20000, in band only at N = 50000. Heywood solutions (some
zeta-hat > .995) occurred in 59 to 91% of fits at N <= 500.

Milestone M167 is a study. It first measures coverage per component and per
miss direction, then scores candidate interval families on the same simulated
data against a band fixed before the full run. It ships no interval method;
a later milestone may ship a family the verdict passes. The design is fixed in
a pre-registration block, and the user asked for an independent review of that
design before the full run. Changes you recommend can still go into the block,
because no full-run output exists yet.

**A finding from the control run, which you have and the planner did not when
the design was drafted.** The study's control mode re-ran the shipped methods
(B1 percentile bootstrap, B2 Wald) at the six M4 cells under the M4 seeds,
per component. It reproduced every pooled M4 figure within .020. Per weight,
B1 at the boundary truth gives beta_0 (truth .45) coverage .74/.67/.66 at
N = 250/500/1000, with 26 to 34% of replicates missing with the interval
below the truth and 0% above; beta_2 (.15) .79/.73/.76 and beta_3 (.05)
.74/.74/.73, missing above the truth (22 to 27%); beta_1 (.35) .93 to .95. At
the interior truth B1 beta_0 covers .84/.81/.81, missing below. B2 (Wald) on
the same fits covers beta_0 at .87 to .92. The B1 communality shortfall at
N = 250 misses above the truth (15 to 26% above, about 1% below), at both
truths. So the record's reading that the beta shortfall is a property of
percentile intervals for a weight near its zero bound looks wrong: the largest
weight carries the largest shortfall, and the misses point to a systematic
shift in the bootstrap distribution, not to pile-up at zero.

## Materials

- `devel/m167-interval-oracle.R` lines 1 to 96: the pre-registration block
  (truths, cells, methods, paths, scoring, band and pass rule, control). The
  rest of the file is the harness that implements it.
- `devel/m167-interval-helpers.R` (210 lines): the interval prototypes
  (`m167_boot_reps`, `m167_param_reps`, `m167_percentile`, `m167_bc_one`,
  `m167_lnv_ci`, `m167_logit_ci`, `m167_side_angle`).
- `devel/m167-interval-checks.R` (137 lines): checks run before the review,
  all passing (F1's zeta arm reproduces CircE's published intervals; the B1
  copy equals `cpm_fit()`'s intervals exactly; Wishart and row-level
  parametric draws agree; the bias-corrected form matches its closed form).
- `devel/m167-interval-control.rds`: the control run. Read it with
  `Rscript -e 'x <- readRDS("devel/m167-interval-control.rds"); str(x$results$boundary_N500$by_method$B1$beta)'`.
  Each cell's `by_method$<method>$<family>` is a data frame of per-component
  coverage, miss shares, conditioning counts and Wilson intervals.
- `R/cpm_fit.R`: `cpm_analytic_se()` from line 1156, `cpm_bootstrap()` from
  line 1271, the polish `cpm_polish_beta()` from line 975, the Heywood flag at
  line 881.
- `cairn/DESIGN.md` lines 324 to 386: the M4 record and how the package reads
  it.
- `cairn/references/browne1982.md`: the transform-and-invert interval method
  (Browne 1982, pp. 95 to 96) that F1 applies.
- `devel/m4-coverage-oracle.R`: the M4 study the control reproduces.

You may run R code in the repo with `devtools::load_all()`. Do not edit any
file other than your report.

## Questions

1. What mechanism best explains the control-run pattern: beta_0 missing
   below, beta_2 and beta_3 missing above, flat in N from 250 to 1000, at both
   truths, with Wald covering beta_0 better than the bootstrap on the same
   fits? Weigh at least weak identification along the alternating mode of an
   equally spaced 8-scale grid (the Heywood rates), bias of the warm-started
   bootstrap refits, the polish, and the softmax constraint. Say what cheap
   diagnostic, if any, the study should add to tell these apart.
2. Given your answer to 1, can the three candidate families plausibly close
   the gap, and are they correctly specified? F1: Wald on ln v for zeta and on
   logit beta_k per weight (a marginal interval under the simplex). F2:
   parametric bootstrap from cov2cor(Wishart(N - 1, Phat)) at the fitted Phat,
   scored as percentile and as bias-corrected percentile. F3: bias-corrected
   percentile (acceleration 0) on the B1 replicates. Name any family the study
   should drop, and any it should add within about 40 CPU-hours in total (the
   control took 33 minutes on 17 cores for B1 and B2 at the six raw cells, and
   the full run as designed should take about 1.6 hours), for example a basic
   bootstrap, a studentized bootstrap, a profile-likelihood interval, or Wald
   on the softmax scale.
3. Is the scoring rule right? It conditions B2 and F1 on a defined SE
   (singular information gives no interval, counted but not a miss), scores a
   removed weight's [0, 0] interval as a miss below the truth, keeps Heywood
   fits, and defines angle miss sides by the arc midpoint. Say whether
   conditioning on a defined interval can make a family pass that a user
   would not get usable intervals from, and how the verdict should report it.
4. Is the pass rule right? Band [.90, .98] per component, a point-estimate
   allowance of .015, every component at every listed cell, no further
   multiplicity correction, 500 replicates. Give the false-fail and
   false-pass behavior you expect, and say whether the replicate count or the
   allowance should change.
5. The raw-data cells reuse the M4 seeds, so the baseline B1 and B2 figures
   at those cells are already known (pooled since M4, per component since the
   control run). The candidate families have not been scored on them. Does
   reusing these replicates for the candidate verdict bias it, given that the
   control finding above shaped this review? Should the verdict use fresh
   seeds at the raw-data cells?
6. The correlation-matrix cells (N = 2000, 5000, 20000) draw the sample
   correlation directly from a Wishart, which is exact for multivariate-normal
   data, and F2 draws from the same family, so F2 is correctly specified by
   construction. What can a pass for F2 license, and what must the verdict
   say about non-normal data?
7. Anything else in the block that would make the verdict an artifact of the
   design rather than a property of the methods.

## Constraints

- D-002 refuses BCa intervals for SSM circular displacement. The study gives
  angles no bias-corrected form. Flag it if you think D-002 reaches CPM zeta
  or beta.
- D-010: the free-scaling family shares the diag family's coverage regime at
  correlation input; it is out of this study.
- IP3: a shipped numeric result needs two or more independent oracle types.
  This study is one oracle type for any family a later milestone ships.
- IP4, the RNG contract, binds package code, not this devel script.
- Out of scope: shipping, the free family, SSM amplitude intervals.

## Output format

In `RR26-cpm-interval-oracle-design.md`: answer each question by number with
your reasoning and evidence; list any additional findings separately under
"Beyond the brief"; end with concrete recommendations, each marked apply /
consider / reject-with-reason. Your report is advisory: emit a
`## Binding criteria` section ONLY if this brief's header slot says
`requested`.
