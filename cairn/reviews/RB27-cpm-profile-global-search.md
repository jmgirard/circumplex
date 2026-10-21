# RB27: Reaching the global minimum in the CPM profile search and engine (M167)

- **Date:** 2026-10-21
- **Output required:** write findings to `cairn/reviews/RR27-cpm-profile-global-search.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained. Do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`circumplex` is an R package on CRAN. Its `cpm_fit()` fits Browne's (1992)
circular stochastic process model to a p x p correlation matrix by maximum
likelihood (the normal-theory discrepancy F). Parameters: one angle per scale
(the reference angle fixed), a communality index zeta_i in (0, 1) per scale
(logit-parameterized), and weights beta_0..beta_m (softmax with v_0 = 0). The
engine `cpm_engine()` runs a multi-start optimization, keeps the best
solution, and "accepts" it when a gradient criterion holds and the optimum is
reproduced by more than one start.

Milestone M167 is a pre-registered coverage study of CPM interval methods at
two simulated truths (p = 8 octant scales, zeta = .75, m = 3; "boundary"
beta = .45/.35/.15/.05, "interior" beta = .35/.30/.20/.15), at N = 250 to
20000, 500 replicates per cell. At N <= 500, 59 to 91% of accepted fits are
Heywood fits (some zeta-hat > .995). An earlier review (RR26, archived at
`cairn/reviews/archive/RR26-cpm-interval-oracle-design.md`) traced the beta
coverage shortfall to an upward communality bias from weakly identified
angle and communality modes, and recommended adding a profile-likelihood
interval, P1, for zeta and beta.

P1 has now failed review twice, each time by a missed lower minimum:

1. First pass: each profile evaluation was warm-started from the nearest
   previously evaluated point only. Near a Heywood wall this stalled in
   another basin, overstated the statistic, and gave false endpoints. Fix:
   each evaluation now minimizes from the estimate's own free parameters and
   from the nearest warm start and keeps the lower.
2. Third pass (after a full re-run): a reviewer re-profiled P1's finite
   endpoints with 6 to 8 extra starts (the estimate start plus N(0, .5)
   noise) and found 52 of 637 endpoints at boundary N = 250, 34 of 311 at
   boundary N = 1000 and 5 of 314 at interior N = 1000 with a true statistic
   below 3.79 (some near 0). So P1's intervals are sometimes too narrow. The
   reviewer judged that only 2 of 59 low endpoints would turn a miss into a
   cover, so the verdict probably holds, but P1's figures are not
   established.
3. The same reviewer found that the engine itself sometimes accepts a fit
   that is not the global minimum: from a P1 profile point, a free refit
   reached F 3.12/(N - 1) below F-hat, with a different angle order, while
   the engine's `multimodal` flag was FALSE. An independent check then ran
   30 random restarts (the engine's estimate plus N(0, .7) noise on the
   internal parameters) on 37 accepted boundary N = 250 fits and found a
   lower F in 6 of them, by 0.33 to 1.96 in (N - 1) times F.

The user chose this escalation at the review's thrash stop (the third
defect return on the milestone). The study's other results stand: the
strict verdict is that no family passes at every cell; on a "passes from N"
ladder, the shipped percentile bootstrap's angles and P1's communalities
pass at N = 1000 on raw data, and a bias-corrected parametric bootstrap's
weights pass from N = 5000 on correlation-matrix input.

## Materials

- `devel/m167-interval-helpers.R`: `m167_profile_setup()` from line 232,
  `m167_profile_stat()` from line 311, `m167_profile_ci()` from line 329
  (its `side_root()` follows the profile outward in growing steps on the
  logit scale, then calls `uniroot()` in the last bracket).
- `devel/m167-interval-checks.R` check 4d (from line 171): the endpoint check
  that missed defect 2, because it re-profiles from the estimate start only.
- `devel/m167-interval-oracle.R` lines 1 to 200: the pre-registration block,
  including the P1 definition, the pass rule and the "CORRECTIONS AFTER THE
  FIRST FULL RUN" heading. The band, scoring rule and pass rule must not
  change.
- `R/cpm_fit.R`: `cpm_start_set()` from line 490, `cpm_reproduced()` from
  line 528, `cpm_engine()` from line 626 (the multimodality flag near line
  742, acceptance near line 829), `cpm_optimize_one()` near line 535,
  `cpm_canonicalize()` near line 570.
- `devel/m167-interval-results.rds`: the re-run results. Each cell's
  `replicates$endpoints` array holds every method's interval endpoints per
  replicate; `replicates$est` the estimates; `by_method$P1$zeta` and
  `$beta` the per-component summaries. Seeds: raw-data cell replicate i of
  truth t (1 boundary, 2 interior) at N index j (250, 500, 1000) uses
  `set.seed(20260706 + 1e6 * t + 1e3 * j + i)` then `cpm_simulate()` of the
  exact truth fit (see `make_truth()` in the oracle script).
- The reviewer's scripts, outside the repo:
  `/private/tmp/claude-501/-Users-jmgirard-github-circumplex/d44716d9-7772-4dc5-99e7-bef93b5da5d1/scratchpad/rv3_p1n.R`
  and `rv3_case.R` (the multistart re-profile and the rep-8 case), and
  `engine_global.R` (the 30-restart engine check). Their `.log` and `.rds`
  outputs sit beside them.
- `cairn/milestones/M167-cpm-interval-oracle.md`: the milestone's criteria,
  work log and Review section.

You may run R code in the repo with `devtools::load_all()`. Do not edit any
file other than your report.

## Questions

1. Are the lower minima the engine misses legitimate solutions of the same
   model (a different angle configuration with a better likelihood), or
   degenerate ones (a mirror image, an angle order the canonicalization
   should map back, a boundary point such as zeta = 1 where the parameter is
   not identified)? Characterize them on the 6 cases, and say whether
   `cpm_engine()`'s start set or its acceptance rule should have found them.
2. What does this mean for the study? The study measures the shipped
   estimator, whose estimate is the engine's accepted optimum. Say whether
   that is the right object for B1, B2 and the other methods, and whether
   P1's statistic must be anchored to F-hat of the engine's optimum or to the
   global minimum, and what the choice does to P1 at the fits where they
   differ.
3. How can P1's profile evaluation reach the global minimum at each fixed
   value, with confidence and at an acceptable cost? P1 now takes about 2 s
   per fit for 12 components; the full study has about 6000 fits, and a
   budget of about 10 CPU-hours for P1 is acceptable. Weigh, for example, the
   engine's own start set at each evaluation, dispersed random starts, a
   profile followed from both ends, and a final verification pass at each
   endpoint. Say how P1 should treat a profile whose set
   {t : statistic <= 3.84} is not an interval.
4. Design a discriminating endpoint check for `devel/m167-interval-checks.R`
   that the two-start P1 fails and a correct P1 passes, and that runs in a
   few minutes.
5. Should the study fix P1, or drop it from the verdict (keeping its figures
   only as described, or removing it)? Removal is a real option here: this is
   P1's second escalation, and its figures have failed review twice. Weigh
   what each option tells a later milestone that might ship an interval.
6. Is the engine's missed minimum a user-visible bug that `cpm_fit()` users
   meet at field sample sizes, and how should it be handled: a fix through
   the package's hotfix process, a separate milestone, or documentation?
   Say what evidence a fix would need.

## Constraints

- The pre-registered band, scoring rule and pass rule of the M167 block do
  not change. Implementation fixes are recorded under its corrections
  heading.
- D-068 records the current verdict; it will be rewritten from the re-run
  this review leads to.
- D-002 refuses BCa for circular displacement only; the study already reads
  it as not reaching zeta and beta.
- IP1: statistical correctness outranks other concerns. IP4, the RNG
  contract, binds package code, not the devel study.
- Out of scope: shipping any interval method, the free-scaling family.

## Output format

In `RR27-cpm-profile-global-search.md`: answer each question by number with
your reasoning and evidence; list any additional findings separately under
"Beyond the brief"; end with concrete recommendations, each marked apply /
consider / reject-with-reason. Your report is advisory: emit a
`## Binding criteria` section ONLY if this brief's header slot says
`requested`.
