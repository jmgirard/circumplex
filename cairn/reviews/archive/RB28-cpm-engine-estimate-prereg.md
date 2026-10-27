# RB28: The pre-registration and decision rule of the CPM engine-estimate study (M170)

- **Date:** 2026-10-27
- **Output required:** write findings to `cairn/reviews/RR28-cpm-engine-estimate-prereg.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained. Do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`circumplex` is an R package on CRAN. Its `cpm_fit()` fits Browne's (1992)
circular stochastic process model to a p x p correlation matrix by maximum
likelihood (the normal-theory discrepancy F; the fit statistic is
T = (N - 1) F). Parameters: one angle per scale (the reference angle fixed),
a communality index zeta_i in (0, 1) per scale (logit u_i), and weights
beta_0..beta_m (softmax logits v, v_0 = 0). The internal engine
`cpm_engine()` runs a deterministic, theory-centred multi-start battery
(`cpm_start_set()`: the theory start g0, its mirror, and four +/-15 and
+/-30 degree angle jitters), keeps the lowest F, polishes weights below .01
to 0, canonicalizes the reflection, and "accepts" the fit when a gradient
criterion holds and two independent battery starts reproduce the lowest F.

A previous Fable review (RR27, `cairn/reviews/archive/RR27-cpm-profile-global-search.md`)
found that the engine's estimate is often not the global minimum of F: by
an incomplete search, a lower F exists in 24 to 48% of accepted fits at
N = 250 to 1000 and 22 to 37% at N = 2000 to 5000 at the study truths, by
a median 1 to 2 T units. The lower points mostly sit at the Heywood wall
(one or two more scales at zeta = 1) at small N and are interior points at
large N; at the interior truth they often swap two scales' angles. In
RR27's simulation they were farther from the truth than the engine's
estimate. A 150-restart search did not saturate. D-071 (in
`cairn/DECISIONS.md`) then kept the shipped estimator, documented the
contract (M169 shipped the help-page statement), and sent the choice of
estimator to its own milestone with RR27's evidence list. That milestone is
M170 (`cairn/milestones/M170-cpm-engine-estimate-study.md`).

M170 is a bounded, pre-registered simulation study. It compares three
estimators: E (the shipped engine), W (E plus 30 deterministic extra starts
around E's estimate) and H (the lowest over W's starts plus 400 seeded
random starts), checks whether H's search saturates, and applies a decision
rule fixed before the run that picks one of five options: keep E unchanged;
keep E and add a diagnostic for a lower optimum outside the theoretical
configuration; report W; report H; or report E with the lowest discrepancy
found beside it. The decision is recorded as a D-entry; any behavior change
ships in a later milestone. The study code and its pre-registration block
are written and committed, and nothing has run beyond the checks and a
4-replicate smoke run. The plan requires this review of the block before
the timing pilot.

This is the second escalation on the engine-estimate question (RR27 was the
first), so question 8 puts not running the study among the options.

## Materials

Read in full:

- `devel/m170-engine-estimate.R`, lines 1 to 190: the pre-registration
  block. Then lines 339 to 465 (`cell_metrics()`, `favors()`, `decide()`):
  the metrics and decision rule as code, which must match the block.
- `devel/m170-engine-estimate-helpers.R`: E, W, H, H_K, the independent
  family (`m170_uniform_starts()`), and the finish step.
- `devel/m170-engine-estimate-checks.R`: four checks on the helpers. They
  pass at commit 47836901; check 1 printed H gaps of 0.807, 0.650, 3.313,
  3.045, 1.963 and 1.162 T units on RR27's six boundary N = 250 cases
  (replicates 11, 21, 26, 28, 30, 31), where W alone finds 0.755, 0.650,
  0.327, 0, 1.963 and 0. With three seeds each at K = 100, replicate 26 gave
  3.31, 4.51, 4.51 and replicate 28 gave 3.68, 3.68, 3.05: H at K = 100 is
  still seed-dependent on some fits.
- `cairn/milestones/M170-cpm-engine-estimate-study.md`: Goal, Scope,
  Acceptance criteria AC1 to AC7, and the work log.
- `cairn/reviews/archive/RR27-cpm-profile-global-search.md`, sections Q1, Q2
  and Q6: the evidence the study builds on.
- `cairn/DECISIONS.md`: the entry headed `### D-071` (search the heading;
  read that entry only).
- `cairn/DESIGN.md`: the principles block (IP1 to IP6, GP1 to GP7) and the
  section "CPM engine estimate: what it is (D-071)".
- `R/cpm_fit.R`: lines 490 to 560 (start set, single optimization), 626 to
  934 (`cpm_engine()`), 975 to 1009 (`cpm_polish_beta()`) and 1156 to 1210
  (`cpm_analytic_se()`).

You may run, from the repo root:

- `Rscript devel/m170-engine-estimate-checks.R` (about 10 s on 6 cores).
- `M170_SMOKE=1 Rscript devel/m170-engine-estimate.R` (about 15 s; K = 5,
  4 replicates per cell; writes `devel/m170-engine-estimate-smoke.rds`,
  which you must delete afterwards).
- Your own small scripts in a scratch directory outside the repo, sourcing
  the helpers under `devtools::load_all()`. Measured cost: one refit takes
  about 13 ms; H at K = 100 takes about 5.5 CPU-seconds per fit.

Do not run the pilot (`M170_PILOT=1`) or the full run (no mode variable):
both write files the pre-registration counts. Do not commit, and write no
file in the repo except the RR.

## Questions

1. W's starts. Are the 30 deterministic starts (a wall start per scale, one
   all-communality start with every zeta at .99, each free angle shifted by
   +90, -90 and 180 degrees, all around E's estimate) the right
   deterministic set for a probe a package could ship? The plan named "an
   all-communality-level start" without a level; the block sets .99. Is
   that the right reading, or should it be a different level or more than
   one start? Should any W start sit around g0 rather than E's estimate,
   given RR27 Q1's finding that g0-centred restarts found 64 to 88% of the
   lower basins?
2. H's strength. Is K = 100 (200 random starts per centre, sd .7 in the
   engine's coordinates, centres E's estimate and g0) a defensible H given
   the budget (12 CPU-hours over 10 cells) and the seed dependence above?
   Would a different split (more starts, another sd, another centre) buy
   more for the same cost?
3. The saturation rule. S1 passes a cell when H lies more than 0.01 T units
   below H_K on at most 2% of accepted fits. S2 passes when 400 uniform
   starts (no centre) find a point more than 0.01 below H on at most 1 of
   the first 50 accepted fits. Is S1's .02 a defensible bar, and is S2's
   1-of-50 discriminating at that sample size? Is the uniform family
   independent enough of H's two Gaussian centres to be a check, and is
   400 uniform starts strong enough to find what H misses? Given RR27's
   finding that 150 restarts did not saturate, is a fail at N <= 1000 the
   expected outcome, and does the rule still say something useful there?
4. "AC4's figures favor X". `favors(X, c)` requires, per cell, no worse
   family-mean RMSE for angles, communalities and weights, Wald coverage
   deviation from .95 no worse than E's plus .01, standard-error definition
   share no worse than E's minus .02, and |T-exceedance - .05| no worse
   than E's plus .01, with at least one family RMSE at most .95 of E's. Is
   this a sound reading? Should the comparison be restricted to the lower
   fits of X, where X and E differ? Is averaging RMSE over a family's
   parameters the right summary, given that W and H move one or two
   parameters by tens of degrees? Is the T-exceedance figure fair at
   Heywood points, where the reference distribution is a chi-square
   mixture and df is read after the polish?
5. The decision rule's order and thresholds (block lines 127 to 160):
   branches 1 and 2 adopt W or H only when saturation passes and X is
   favored in all ten cells; branch 3 keeps E unchanged when W finds a
   lower point on under 5% of fits in every cell; branch 4 keeps E when W
   costs over 10 CPU-seconds per fit; branch 5 reports the lowest
   discrepancy beside E's when W finds one on 5% or more of fits with a
   median gap of 2 T units or more in some cell; the default (branch 6) is
   the diagnostic. Are .05 and 2.0 defensible, and is "all ten cells" the
   right reach for branches 1 and 2? Is the default right, or should the
   default be "keep E unchanged"? Does the GP4 tie-break (block lines 149
   to 160) read GP4 correctly?
6. Shipping constraints. A shipped diagnostic or report-beside option runs
   only W's deterministic starts (IP4: functions whose output is not
   stochastic leave the RNG stream untouched). Does the rule choose among
   options a package can ship? In particular: can "report H" be shipped at
   all, and if not, should branch 2 exist? Does the rule measure W's
   detection power against H well enough that a W-based diagnostic's
   silence is not misread?
7. The replicate rule (R the largest multiple of 50 in [100, 500] under 12
   CPU-hours with a 1.2 margin) and Monte Carlo precision: at the R the
   pilot is likely to give (about 500), are the cell shares precise enough
   for the .02 and .05 thresholds the rule reads? Should a threshold carry
   an allowance, as M167's band did?
8. Removal (second escalation). Weigh not running the study: close the
   engine-estimate question on the documented contract (M169) and D-071,
   with no measurement, against running it as pre-registered or as you
   amend it. What would each tell a later milestone?
9. Anything in the block or code that would let a result be read two ways
   after the run (an undefined metric, a definition that does not match the
   code, a seed collision, a metric computed on the wrong set of fits).

## Constraints

- D-071 stands until this study's D-entry: the shipped estimator is the ML
  solution in the basin the theory-centred starts reach. Its Reopens clause
  names "a search shown to saturate whose figures change which solution is
  the better estimate".
- The milestone's acceptance criteria fix the decision rule's shape: an
  ordered if/else chain ending in a default branch, selecting one of the
  five options; W or H only where saturation passes in the cells it relies
  on and AC4's figures favor that estimator; a stated GP4 tie-break between
  the diagnostic and the report-beside option. Flag disagreement with this
  shape explicitly rather than working around it.
- No package code changes in this milestone (AC7); any shipped change goes
  to a later milestone, which also re-runs the T calibration and the
  CIRCUM/CircE oracles.
- The plan chose Wald coverage for W and H over bootstrap coverage
  (a bootstrap of W or H needs a full search per resample); bootstrap
  coverage of an adopted alternative is the later milestone's.
- The CPU budget is about 12 CPU-hours for the full run, on a 16-worker
  machine. Data generators are M167's (raw rows from `cpm_simulate()`;
  Wishart draws for correlation-matrix cells).
- IP1 (statistical correctness outranks other concerns), IP3 (shipped
  results validated against two independent oracle types), IP4 (RNG
  contract), GP2 (caution loudly, fail closed), GP4 (printed output and
  return structures are commitments), GP7 (evidence reopens decisions).

## Output format

In `RR28-cpm-engine-estimate-prereg.md`: answer each question by number
with your reasoning and evidence (state which figures you computed and
how); list any additional findings separately under "Beyond the brief";
end with concrete recommendations, each marked apply / consider /
reject-with-reason. Mark each recommendation that changes the
pre-registration block so it can be listed under the block's CORRECTIONS
heading. Your report is advisory: emit no `## Binding criteria` section.
