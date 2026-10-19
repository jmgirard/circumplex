# RB25: Small-sample draw rules for growth-trajectory intervals (M161)

- **Date:** 2026-10-18
- **Output required:** write findings to `cairn/reviews/RR25-small-n-trajectory-draws.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`circumplex` is an R package for the Structural Summary Method (SSM). A
circumplex profile is summarized by elevation `e`, two coordinates `x` and
`y`, amplitude `a = sqrt(x^2 + y^2)` and displacement `d = atan2(y, x)` in
degrees on [0, 360). For repeated measures, the package's growth workflow is:
per-person `(e, x, y)` at each wave, stacked long, fit by the user with one
joint linear mixed model (fixed `0 + dv + dv:wave`, correlated person random
intercepts `us(0 + dv | person)`, a residual variance per `dv`, REML). The
package fits nothing itself (D-060, D-064). The exported helper
`ssm_trajectory(coef, vcov, times)` takes the fixed effects and their
covariance, draws `n_draws` coefficient vectors from `N(coef, vcov)`, forms
`e(t), x(t), y(t)` per draw as intercept plus `t` times slope, and
summarizes each time with `ssm_draws()`: medians and equal-tailed percentile
bounds for `e, x, y, a`, and a circular mean with circular quantile bounds
for `d`. A `certified` flag marks times where the `d` interval is
interpretable (amplitude reliably above zero).

The intervals condition on the estimated variance components. At 200
persons per cell, a coverage oracle (`devel/m27-coverage-oracle.R`, record
`devel/m27-coverage-oracle.md`) measured `d(t)` coverage of .93 to .97 at
every certified wave. Nothing measured small N. A percentile parametric
bootstrap is not a correction here: on complete balanced data the REML fixed
effects equal OLS for any variance components, so bootstrap replicates
reproduce the normal draws (measured in an earlier milestone: bootstrap SD
0.95 to 1.03 of the Wald SE). nlme reports 2095 denominator df for every
coefficient on this design, and glmmTMB offers no Satterthwaite or
Kenward-Roger df.

Milestone M161 builds a seeded small-N coverage oracle and selects, by a
pre-registered gate, a draw rule that milestone M162 then ships as an opt-in
argument on `ssm_trajectory()`. The plan gate chose an opt-in argument over
a new default because the helper cannot know N from `coef` and `vcov`. The
plan names two candidates (the shipped normal draws, and multivariate t
draws with `nu = N - 1`) and asks this review for the rest of the candidate
set and for a check of the design before the oracle runs.

The brief's author computed a rough expectation that may undercut the
premise. It is unverified and you should check it. For `e(t)` (or `x(t)`)
at wave `t` on complete balanced data, the variance of the fitted value is
`s_u^2 / N + s_e^2 * w(t) / N`, with `w(t) = 1/T + (t - tbar)^2 / sum((t -
tbar)^2)`, `T = 5`. The between-person part carries `N - 1` df and the
within part about `N(T - 1) - 2`. A Satterthwaite combination of the two
gives, for the `pole` truth (`s_u = .20`, `s_e = .25`), df of about 59 at
wave 0 and 32 at wave 2 when N = 20; for the `x`, `y` coordinates of the
`xycor` truth (`s_u = .35`, `s_e = .15`), about 23 and 20. Reading a normal
95% interval against those t distributions gives marginal coverage of about
.936 to .945 at N = 20 and .921 to .940 at N = 10. If that holds, the
shipped normal draws pass the planned gate at N = 20 to 50, and a
`nu = N - 1` t rule also passes (overcovering), so the gate may not
discriminate.

## Materials

1. `cairn/milestones/M161-small-n-trajectory-oracle.md` — the whole file:
   Goal, Scope, the five acceptance criteria (AC2 is the gate, AC5 the
   selection rule), and the work log's plan-gate lines.
2. `cairn/milestones/M162-trajectory-small-sample-draws.md` — Goal, Scope
   and AC1 to AC5: what the chosen rule must support when shipped (one
   numeric argument, a closed-form quantile test on `e, x, y` and `x + y`).
3. `devel/m27-coverage-oracle.R` — lines 1 to 63 (header, pre-registration
   shape, seed scheme), 80 to 126 (generator and the joint REML fit), 149
   to 161 (fixed-effect draws), 176 to 182 (circular inclusion), 224 to 282
   (the `pole`, `lowamp` and `xycor` truths). M161 reuses `pole` and
   `xycor`.
4. `devel/m27-coverage-oracle.md` — the 200-person results.
5. `R/ssm_trajectory_helper.R` lines 1 to 165 (roxygen and the draw step)
   and `R/ssm_montecarlo.R` lines 200 to 210 (`mvn_draws()`).
6. `vignettes/growth-ssm-analysis.Rmd.orig` lines 348 to 383 (Section 7,
   the current caution text).
7. `cairn/DECISIONS.md`: D-060 (line 2129) and D-064 (line 2316).

You may run R (`devtools::load_all(".")`; glmmTMB is installed) for small
checks. Do not run the full oracle; it is M161's T3.

## Questions

1. Premise. At the `pole` and `xycor` truths with N in {10, 20, 30, 50},
   what coverage do the shipped normal draws give for `e(t)`, `x(t)`,
   `y(t)` and `d(t)` at each wave? Check or correct the author's rough
   calculation above, including any REML bias in the variance-component
   estimates that the calculation ignores. Is the undercoverage the
   candidate row assumed real at these truths, and at which N?
2. Gate power. Given your answer to 1, does AC2's gate (mean over waves in
   [.93, .97] for each of `e, x, y` and gated `d`, no wave below .91, in
   every complete-data cell with N in {20, 30, 50}, 1000 replicates per
   cell) distinguish the shipped draws from a corrected rule? If not, what
   should change: the gated N range, the truths (for example a
   between-person-dominated truth where df approach N - 1), the band, or
   the milestone pair's goal? State the change in a form a pre-registered
   header can hold.
3. Candidate rules. Name each rule worth gating that is computable from
   `coef`, `vcov`, `times` and one numeric argument with a stated lower
   bound (AC5's form). For each give the exact draw formula, the argument,
   its lower bound, and what it assumes. Consider at least: multivariate t
   with a shared chi-square divisor at `nu = N - 1`; independent t
   marginals; a `vcov` inflation such as `N / (N - k)`; and any df rule
   built from the design that needs only N and `times`. Say which you
   would not gate and why.
4. Shared versus separate divisor. For a multivariate t on the whole
   coefficient vector, what does a shared divisor do to the joint
   distribution of `x(t)` and `y(t)`, and so to `a(t)`, `d(t)` and the
   certification, against per-coefficient divisors? Which is right for the
   between and within parts of this model?
5. Missing waves. With 20% of person-waves deleted at random, the REML
   fixed effects are no longer OLS. Does glmmTMB's `vcov(fit)$cond` under
   `REML = TRUE` already carry a term for variance-component uncertainty
   (TMB's `sdreport` treats the fixed effects as random under REML)? If so,
   on which cells, and does that change how the missing-wave cells should
   be read or whether a correction should apply there?
6. Selection rule. AC5 picks, among passing rules other than the shipped
   draws, the one with the narrowest median `d(t)` arc width pooled over
   gated cells. Is that sound, or does it favor a rule that undercorrects
   at the band's lower edge? Propose a better tie-break if one exists.
7. Gate statistics. With about 160 coverage checks per rule and a Monte
   Carlo SE near .007 at 1000 replicates, what is the chance that a rule
   with true coverage .95 everywhere fails the gate? Is the per-wave floor
   of .91 right?
8. Anything in the design that would let a plausible-but-wrong rule pass,
   or a correct rule fail, that questions 1 to 7 do not cover.

## Constraints

- D-060 and D-064: the package fits no mixed model and calls no engine;
  the rule must work on `coef` and `vcov` alone plus one user-supplied
  number. Flag disagreement explicitly rather than working around it.
- Angles are degrees on [0, 360); `d` intervals use circular quantiles and
  may straddle 0/360 with `d_lci > d_uci`.
- The opt-in argument (not a new default) is the plan gate's choice; it is
  reopened only by a rule whose df are computable from `coef` and `vcov`
  alone. Say so if your answer to 3 finds one.
- Draws supplied as a `draws` matrix (for example brms posteriors) are not
  corrected.
- M161 changes nothing under `R/`, `man/`, the vignettes or NEWS.md.

## Output format

In `RR25-small-n-trajectory-draws.md`: answer each question by number with
your reasoning and evidence; list any additional findings separately under
"Beyond the brief"; end with concrete recommendations, each marked apply /
consider / reject-with-reason. Your report is advisory: emit a
`## Binding criteria` section ONLY if this brief's header slot says
`requested`. It says `not requested`, so emit none.
