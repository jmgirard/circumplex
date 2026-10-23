# RR27: Reaching the global minimum in the CPM profile search and engine (M167)

- **Date:** 2026-10-22
- **Brief:** `cairn/reviews/RB27-cpm-profile-global-search.md`
- **Binding criteria:** not requested (none emitted)
- **Reviewer:** Fable, independent; the brief's materials only, plus the
  computations listed under Evidence.

## Evidence

All runs used `devtools::load_all()` on branch `m167-cpm-interval-oracle`
at d1366721, the study's own seeds and truths, and `cpm_engine()` with
variant A, m = 3, octant angles. T units below mean (N - 1) times F, the
scale of the chi-square statistic and of P1's cutoff 3.84.

Read and re-used (code read in full; figures quoted from their logs):

- The brief's reviewer scripts `rv3_p1n.R`, `rv3_case.R`,
  `engine_global.R` and their logs (the 6-of-37 engine cases are in a
  saved `q1_engine.rds` from that reviewer's follow-up, which holds each
  case's parameters and R).
- A survey by the same reviewer (`q6_survey.R`, 100 replicates per cell at
  raw N = 250, 500, 1000 for both truths and correlation-matrix N = 2000,
  5000 for the boundary truth; 40 restarts at sd .7 around the engine
  optimum, 40 around the battery's start g0, 30 structured probes, 2
  zeta-level starts), its companion on estimate quality (`q2_better.R`,
  60 replicates per raw cell), a per-start hit-rate study on the 59 low
  endpoints (`q3_hitrate.R`) and a re-profile of check 4d's own seeds
  (`q4_current.R`).

Run by me:

- Classification of the six engine cases (wall scales, natural-scale
  slope at the wall, barrier along the segment between the two optima).
- `sat.R`: a restart-saturation study, boundary and interior N = 250,
  replicates 1 to 40 (37 and 33 accepted fits), 150 restarts at sd .7 around
  the engine optimum, 150 around g0, and 21 wide angle probes (each free
  angle +90, -90, 180 degrees) per fit.
- `q1_detail.R` (the reviewer's script, completed here): 13 lower-minimum
  cases at interior N = 250 and boundary N = 250, 2000, 5000 in full.
- Restart checks on the two published oracle matrices in
  `tests/testthat/helper-cpm-oracles.R`.
- Timing of one profile evaluation and of the current P1.

Not obtained: the verified-P1 prototype test (`q3_proto_test.R`, K = 6, 40
fits) was stopped by the user after 5 h 40 min of wall time on 15 to 16
workers, about 68 CPU-hours, without writing its results file, and the
check-design run chained after it never started. Every figure those two
runs would have given (the prototype's endpoints, coverage changes,
verification outcomes, per-fit timing, and the pass/fail of the candidate
check on a verified P1) is unmeasured in this report and is not estimated.
My own three jobs ran concurrently on the 18-core machine (44 workers), so
every wall and CPU time I quote below is under 2 to 3 times
oversubscription unless marked otherwise.

## Q1. What the missed minima are, and whether the engine could have found them

**The six cases** (boundary N = 250, replicates 11, 21, 26, 28, 30, 31; the
30-restart check's finds). On every one of them:

- The lower point is a stationary point of F in the engine's own
  coordinates: gradient norm 4e-9 to 2e-7, below the engine's acceptance
  threshold.
- It is not a mirror image. `cpm_canonicalize()` returns it unchanged in
  all six, and a reflection has exactly the same F (rho is even), so a point
  with lower F cannot be one.
- It has the same cyclic order of angles as the engine's solution (order
  1 to 8 relative to PA in all six). The angle shifts are 10 to 66 degrees
  on one to four scales (replicate 28: DE 198 to 264, FG 242 to 303, HI 271
  to 327; replicate 31: LM -13 to 4), and the communalities move by up to
  .40 (replicate 11: HI .55 to .95, JK .77 to .99).
- Five of the six sit at the Heywood wall, with one or two more scales at
  zeta = 1.000 than the engine's solution (11: BC; 21: BC and LM; 26: PA;
  28: DE, from a non-Heywood engine fit; 31: JK and LM). At those scales the
  natural-scale slope (N - 1) dF/dzeta is -0.9 to -20: F is still falling
  into the wall, and the point is stationary only because the logit
  Jacobian zeta (1 - zeta) vanishes. These are infima on the closure of the
  parameter space, the same kind of point the engine itself reports as a
  Heywood fit (59 to 91% of accepted fits at N <= 500 are such points).
  Replicate 30 is the exception: an interior stationary point (every
  zeta < .98, smallest Hessian eigenvalue 2.9e-4, condition number 1.7e4).
- The gap is 0.33 to 1.96 T units; the highest F on the straight segment
  from the engine's point to the lower one is 1.2 to 13.8 T units above the
  engine's F, so they are separate basins, not one flat valley.

So on the six, the lower minima are legitimate points of the same model's
closure with a better likelihood, reached by a different configuration of
the same scales: not mirrors, not an order the canonicalization should map
back, and not points where a parameter is unidentified (every implied
matrix is positive definite; zeta = 1 removes no parameter). "The global
minimum" is, however, not what the 30-restart search found either. The
heavier search in `q1_detail.R` on the same replicate 26 reaches 3.28 T
units below the engine (against 0.33 from 30 restarts), with the order
changed to 1 2 3 6 5 4 7 8 (FG and JK swapped, three scales at the wall).
The six cases are a lower bound on what exists.

**The wider picture** (`q6_survey`, 100 replicates per cell, 80 restarts
plus probes per fit; my saturation study on the N = 250 cells). Among
accepted fits, a lower minimum was found in 43% (boundary N = 250), 48%
(interior 250), 43% (boundary 500), 24% (interior 500), 32% (boundary
1000), 3% (interior 1000), 37% (boundary 2000) and 22% (boundary 5000).
Median gap among the finds 1.1 to 2.1 T units; maxima 4.7 to 12.6. The
lower point is a Heywood point in 98%, 93%, 81%, 86%, 44%, 33%, 19% and 0%
of the finds, in the same cell order. The angle order is the same as the
engine's in 85% of the boundary-truth finds at N = 250 (95%, 100%, 100%,
100% at larger N) but in only 15% at interior N = 250 (23% at 500, 0 of 3
at 1000): there the lower solution swaps two scales, typically across the
circle, with one of them at the wall (interior 250, replicate 5: FG 240 to
294 and JK 308 to 231; replicate 6: BC 104 to 57 and NO 47 to 136;
replicate 7: DE 183 to 275 and HI 280 to 186). At boundary N = 5000 the
finds are interior stationary points (no wall) with gaps as small as 0.08
and angle shifts of 15 to 50 degrees on scales whose communality rises
.08 to .10 (replicate 17: LM -2 to -16 degrees, zeta .78 to .86). The
survey's own search is incomplete (next paragraph), so each share is a
lower bound.

**Could the start set or the acceptance rule have found them?** No, by
construction.

- `cpm_start_set()` is theory-centred and data-blind in the direction that
  matters: angles at theory and at +/-15 and +/-30 degree alternating
  offsets, zeta at sqrt(max |r|) clipped to [.3, .95], beta from a
  least-squares fit, plus the mirror. The lower basins need either a scale
  pushed to the wall or an angle moved 60 to 180 degrees. In all six cases
  all 6 battery starts (5 of 6 in replicate 30) land on the engine's F. In
  the survey, a +/-30 degree probe from the engine's optimum found a lower
  basin in 0 to 3% of the finds; a zeta-wall probe (one u_j set to
  logit .99) found it in 42 to 100% of the boundary-truth finds and 0 to
  14% of the interior-truth ones; random starts at sd .7 around g0 found
  64 to 88%.
- `cpm_reproduced()` asks that the best F be reached by two independent
  battery groups; the battery's agreement among themselves is what it
  certifies. All six cases were reproduced by 5 or 6 of 6 starts. The
  multimodality flag compares battery runs with each other and fires on a
  competitive distinct optimum; it cannot see a basin no battery start
  visits (6 flagged fits among 760 accepted in the survey, none of them the
  lower-minimum cases).
- Random restarts do not saturate at a cost the engine could pay. My
  saturation study (N = 250, replicates 1 to 40): with 10, 30, 60, 100 and
  150 restarts each around the engine optimum and around g0 (sd .7), fits
  with a lower minimum found: boundary 9, 14, 16, 16, 16 of 37; interior
  12, 17, 20, 22, 23 of 33. Between 30 and 150 engine-centred restarts the
  minimum found still fell in 5 boundary and 13 interior fits. The 21 wide
  angle probes alone found 3 of 37 and 13 of 33. The thrash-stop figure "6
  of 37" (30 restarts, one centre) is the 7-of-37 row of this table; the
  same fits give 16 of 37 at 321 starts.

## Q2. What this means for the study, and P1's anchor

**The shipped estimator is the right object for B1, B2, F1, F3, Bb, F2p,
F2bc and F2b.** Those methods are built on the estimate `cpm_fit()`
returns, and the bootstrap replicates are warm-started from it (the shipped
loop, copied in `m167_refit_reps()`), so the study measures the coverage
users get around the estimate users see. The engine finding changes the
description of that estimator, not the measurement: it is the ML solution
in the basin the theory-centred battery reaches, which is not the global
minimum in a quarter to a half of accepted fits at N <= 2000.

It is also not the case that the global minimum would be the better
estimate. On the replicates where a lower minimum was found
(`q2_better`, 60 per raw cell), the lower point is farther from the truth
than the engine's: angle RMSE over the seven free angles 17.7 against 30.0
degrees (boundary 250), 9.9 against 44.5 (interior 250), 13.0 against 18.8,
8.7 against 33.6, 11.1 against 14.8, 4.9 against 44.7 (boundary and
interior 500 and 1000); communality RMSE .129 against .157 at boundary 250
(.111 against .139, .080 against .115 at 500 and 1000); mean beta_0 .440
against .406 (truth .45) at boundary 250; Heywood scales per fit 1.1
against 1.9. The global solution buys 1 to 2 T units of fit by moving a
scale to the wall or across the circle. Switching the study's estimator to
it would worsen the beta_0 bias the study is about and the angle figures,
and would measure an estimator the package does not ship. The verdict for
those families stands as a verdict about the shipped estimator; the record
should say what that estimator is (Q6).

**P1's anchor must be the global minimum, and P1's profile must be the
global profile.** The pre-registered statistic is (N - 1)(F_prof(t) -
F_hat) with F_prof(t) the minimum of F over every other free parameter at
the component held at t. That is a global minimum at each t. It is
consistent with a chi-square(1) cutoff only when F_hat = min_t F_prof(t),
the global minimum of F. The alternatives:

- Engine F_hat with the global profile: stat = (N - 1)(F_prof(t) -
  F_global) - gap, floored at 0. The set is {t : LR_global(t) <= 3.84 +
  gap}: a likelihood-ratio set whose cutoff is raised by the engine's gap.
  In `q2_better` the gap exceeds 3.84 in 4 of 23 found fits at boundary
  250, 7 of 20 at interior 250, 3 of 25, 1 of 11 and 5 of 17 at 500 and
  1000 (2 to 14% of accepted fits, lower bounds), so there the cutoff is
  more than doubled. On check 4d's own seeds (`q4_current`, 24-start
  re-profile), 3 of 10 accepted fits have a gap (.84, 1.54, 2.27), and 5 of
  the 23 endpoints read as too narrow under the engine anchor read as too
  wide (statistic 4.4 to 5.9) once re-anchored.
- Engine F_hat with a local-basin profile (what the two-start P1
  approximates): not a defined object, because basins merge and split as t
  moves; defect 2 is the point where "the estimate's basin" stops meaning
  anything.

**What the global anchor does at the fits where the two differ.** The
interval is narrower than the engine-anchored set by the gap. The engine's
own estimate lies outside its P1 interval whenever the gap exceeds 3.84 (2
to 14% of accepted fits at N <= 1000, from the incomplete search), and the
outward search must start from the global minimizer's component value, not
from the engine's t-hat (from t-hat the step search finds no sign change
and the current code returns NA). The interval is then centred on the
global solution, which at N = 250 has higher communalities (.82 against .77
on the found fits), so P1's communality coverage can fall where the two
differ. This is the honest P1: a likelihood-ratio interval of the model's
likelihood, which is what a later milestone would ship if it ships P1 at
all, and it is independent of the engine's basin problem. The study should
record per fit the anchor gap and whether the engine's estimate lies inside
P1's interval, as descriptive figures beside the coverage.

## Q3. Reaching the global profile minimum at each t, at an acceptable cost

**Cost units.** One profile `nlminb()` with the analytic gradient took
21 ms under my oversubscribed load; the current P1 on the same fit took
7.3 s, which is about 340 profile runs per fit (24 sides, about 7
evaluations per side, 2 starts each). At the brief's 2 s per fit that is
about 6 ms per run unloaded. The budget, 10 CPU-hours over about 6000 fits,
is 6 CPU-seconds per fit, about 1000 runs: three times the current P1's
count. Any design has to be counted in runs per fit against that.

**What the hit-rate evidence says** (`q3_hitrate`, the 59 endpoints the
third-pass review found low, 58 of them confirmed low by the pooled
search): the chance that one dispersed start reaches the lowest basin at
the endpoint is .095 (sd .3), .137 (sd .5), .117 (sd .7), .069 (sd 1.0);
one zeta-wall probe .154, one +/-30 degree angle probe .046. Twelve starts
at sd .5 reach it at 57% of the endpoints; the 28 to 30 structured probes
at 88%. So no fixed small set of starts reaches the lowest basin at every
evaluation, and a guarantee is not available at any finite cost: the free
problem itself does not saturate at 150 restarts (Q1). The standard P1 can
meet is "verified": every finite endpoint re-checked by an independent
search, with the residual failure rate measured by the check in Q4 and by
the study's own diagnostics.

**The options the brief lists.**

- The engine's own start set at each evaluation: reject as the device. It
  is theory-centred; it found none of the basins in Q1 (battery Fs equal
  in all six cases; +/-30 degree probes 0 to 3%). Its only use would be as
  six cheap data-blind starts; they add little.
- Dispersed random starts at every evaluation: not affordable as the
  guarantee. At a .14 per-start hit rate, 20 starts give 95% at one
  evaluation; 20 x 170 evaluations is 3400 runs per fit, over three times
  the budget, and still 5% per evaluation.
- The profile followed from both ends: a cheap supplement (one more pass
  of about 170 evaluations), which tracks the far-end basin inward and
  takes the lower envelope. It catches a basin switch where the far-end
  basin (the wall for a zeta component) is the lower one near the
  crossing, which is the common shape in the Heywood regime; it does not
  catch a basin that appears only mid-path. Unmeasured; consider.
- Verification at each endpoint: necessary. Dispersed starts plus the
  structured probes at the crossing; when a lower basin is found, continue
  outward from the crossing in that basin and verify again. This is the
  core of the design, and it is where the cost sits: 24 crossings per fit.

**The design I recommend** (a "basin library" with verified crossings):

1. Anchor pass, once per fit: the engine's optimum, R dispersed free
   refits around it and around g0 (sd .7), the zeta-wall probes (one u_j at
   logit .99 for each scale) and the wide angle probes. Keep every distinct
   local minimum within about 10 T units of the lowest as a branch; F_hat
   is the lowest. The survey's find rates say the zeta-wall probes and the
   g0-centred restarts are the productive families; R = 20 is a starting
   count, to be set by the pilot below.
2. Each profile evaluation at s starts from the nearest cached solution on
   the path and from each branch with the component set to s, and keeps the
   lowest. A branch is a continuation of a known basin, so the basin
   switch of defect 2 is seen the moment the other branch is lower.
3. The outward search starts at the anchor's component value. At each
   finite crossing, verify with K dispersed starts (sd .5 and .8, around
   the path solution) and the zeta-wall probes; a lower point joins the
   library and the search continues outward from the crossing. Cap the
   verify-and-continue rounds (3) and the runs per component, and report a
   component that hits a cap as unverified rather than running on.
4. After the endpoint, scan outward at logit +.25, .5, 1, 2, 4, 8 for a
   re-entry below the cutoff (Q3's last question).

Projected run count, from the counts alone and not from any measurement:
anchor 60 to 80; evaluations 170 x (1 + B) with B branches, typically 1 to
3; verification 24 x (K x 2 + 8 to 16 probes); scan 24 x up to 6. With K =
4 and B = 2 that is about 60 + 510 + 580 + 100 = 1250 runs per fit, about
1.25 times the budget; with K = 6 and both centres, about 1.8 times. The
pilot sets K and R.

**Finding: the prototype's cost.** The prototype written for this review
(`p1_proto.R`: a 12-start anchor plus probes, two-start evaluations, K = 6
verification at two sds around two centres plus probes at every crossing,
verify-and-continue with no round cap, and an outward scan), tested on 40
fits with an 80-start heavy re-verification of every endpoint, did not
finish in about 68 CPU-hours (5 h 40 min wall on 15 to 16 workers; stopped
by the user; no results file written). The brief's budget is about 10
CPU-hours for P1 over about 6000 fits. The per-fit time, the endpoints, the
coverage changes and the verification outcomes of that run are unmeasured.
The cause is not established; two candidates, neither verified: the
verify-and-continue loop has no cap, so a crossing that keeps finding a
lower basin re-enters the step search without bound, and my three jobs
oversubscribed the machine 2.5 times. Whatever the cause, it is a design
fact that P1 must carry a per-component run budget and a round cap, and a
timing pilot on 10 fits is a gate before any study run.

**A set that is not an interval.** The pre-registered definition is the
set {t : stat <= 3.84}; the scoring needs one interval per component.
Report the hull of the set as found (the outermost crossing before s_far,
from the outward scan), and count per component the fits where the scan
found a re-entry. The hull's coverage is never below the set's, and a
single interval is what a shipped P1 would report. Where the count exceeds
5% at a cell, P1's figure there is reported with that share. The connected
piece containing the anchor is the alternative; it under-reports the set.
Either choice is an implementation reading of "the set", recorded under
the corrections heading; the band, scoring and pass rule are untouched.

## Q4. A discriminating endpoint check for `devel/m167-interval-checks.R`

Replace check 4d with this, on its own seeds (88000 + i, i = 1 to 12; 10
accepted fits at boundary N = 250):

1. The check's own anchor: 36 dispersed free refits around the engine
   optimum (sd .7), the 8 zeta-wall probes and the 14 angle probes; its
   F_min. Record P1's anchor gap against it.
2. At every finite P1 endpoint, re-profile with 12 starts at sd .5 and 12
   at sd .8 around the estimate's free parameters, plus the structured
   probes, under a seed independent of P1's own, and take the statistic
   relative to the check's F_min.
3. Pass when no endpoint has a statistic below 3.79, none above 3.89, and
   P1's anchor is within .05 T units of the check's. The upper bound is
   what catches a P1 whose anchor pass missed the global minimum (every
   endpoint then reads high); the current 4d has no upper bound and
   re-profiles from one of P1's own starts, which is why it passed a P1 the
   third pass found wrong.

Evidence that it discriminates: on these seeds, with 24 random starts and
no probes, the two-start P1 has 23 of 175 finite endpoints below 3.79 on 7
of the 10 fits (`q4_current`), so the FAIL branch is established. The
PASS branch on a verified P1 is unmeasured (that run never started).
Runtime: the two-start half took 7 to 15 s per fit for P1 under 12 workers,
plus 175 endpoints x about 54 runs; about a minute of CPU on an unloaded
core by run count, unmeasured as a whole. The planted-defect convention of
the checks file applies: the two-start P1 is the plant.

## Q5. Fix P1 or drop it

What each option tells a later milestone:

- Remove P1: the ship-step candidate list loses the one likelihood-based
  interval RR26 asked for, with no measurement of it; D-068's ladder loses
  "P1 communalities at N = 1000". A later milestone would start P1 from
  zero, and would meet the same search problem.
- Keep the figures as described, out of the verdict: the figures are known
  to be too narrow at 8 to 14% of finite endpoints at boundary N <= 1000
  and are anchored to a non-global F_hat; describing a defect is of little
  use.
- Fix: the only option that measures the object a ship step needs, an LR
  interval of the likelihood, separated from the engine's basin problem.
  Both of P1's failures were search failures of the same kind, not defects
  of the method, and the fix is now specified (Q3) with a check that would
  have caught both (Q4). Its cost is the risk: the prototype attempt ran
  past the budget without finishing, and the design's run count projects
  at or above the budget.

Recommendation: fix, under a stop rule fixed before the implement pass and
recorded in the milestone: (i) a 10-fit timing pilot of the final P1 must
come in under 6 CPU-seconds per fit at the median on an unloaded core
(scaled to the study's core count), (ii) the Q4 check must pass, (iii) a
fourth defect return on P1's figures removes P1 from the verdict with its
figures reported as not established. If (i) or (ii) fails, P1 is removed
rather than re-tuned. Tier: Fable for the P1 design review and the engine
question (plausible-but-wrong search results are the hazard), Opus for the
implementation, Sonnet for the re-run and record edits.

## Q6. The engine's missed minimum: user-visible, and how to handle it

**Is it user-visible at field sample sizes?** A lower minimum exists, by an
incomplete search, in 24 to 48% of accepted fits at N = 250 to 1000 at the
study's truths and in 22 to 37% at boundary N = 2000 to 5000 (Q1). On
published data: the vocational-interest matrix (p = 7, N = 175, a Heywood
fit, the CircE oracle) has no lower minimum in 207 starts; the verbal
matrix (p = 6, N = 1046, m = 2, df = 2) has one, T 0.84 against the
engine's 3.99 (gap 3.16), found by 1 of 200 restarts, with Grammar moved to
202 and Literature to 112 degrees (order 1 2 5 4 3 6) and beta_2 above
beta_1: a non-circumplex configuration on a simplex-like matrix. What a
user sees is (a) the fit statistic, and with it the p-value, RMSEA and CFI,
overstated by the gap (at df = 10 a gap of 1.5 moves a p-value of .13 to
about .20), and (b) nothing else: `accepted` is TRUE, the "competing
near-tied optima" note does not fire, and the estimates shown are the
theory-basin ones, which in simulation are the better estimates (Q2).

**Is it a bug?** The engine delivers what its design specifies. Design
sec. 3.5 defines the battery as the theory start, its mirror and
deterministic theory-centred jitters, "keep the lowest F-hat"; the help
page names a "convergence acceptance criterion" without defining it, and
DESIGN.md says nothing about global optimality. The package's reference
points were built on this estimator: the CircE oracle reproduces CircE's
published F-hat to 4e-7, which means CircE reports the theory-basin
solution too, and the T calibration was fitted with this engine. A
globalized engine would report T 0.84 for the verbal matrix and swap two
scales, and would move 20 to 48% of small-N estimates to configurations
farther from the truth. So the defect is in the description, not the
estimator: the documentation implies an ML estimate without saying which
one, and the fit statistic is conservative by an unreported amount.

**Handling.**

- Documentation, now, low tier: the `cpm_fit()` help page and DESIGN.md
  state that the estimate is the ML solution in the basin of the
  theoretical angles found by a deterministic theory-centred multi-start;
  that at small N a configuration with a lower discrepancy can exist, with
  a scale at the communality boundary or in a different position; that the
  acceptance criterion certifies agreement among those starts; and that the
  fit statistic is then conservative. Not a hotfix: no user-visible result
  is wrong against the documented contract once the contract is stated.
- A separate milestone, Fable tier at the plan, for the estimator's
  definition. Its options: keep the theory-basin estimator and add a
  bounded, deterministic, data-dependent augmentation to the battery (the 8
  zeta-wall probes from the battery optimum, about 25 ms; the all-zeta-level
  starts) that feeds a new diagnostic, "a lower optimum exists outside the
  theoretical configuration", without adopting it; or adopt the lowest
  found; or report both. The evidence a fix needs: the share and gap per
  cell from a search that is shown to saturate (mine did not at 150
  restarts); bias, RMSE and coverage of the local against the global
  estimator per component at both truths (q2 is a start, 60 replicates per
  cell, incomplete search); the T calibration re-run under any change; the
  CIRCUM/CircE oracles unchanged (a global engine breaks the verbal fit's
  current T and would need the vocational one re-checked at saturation);
  real-data fits (jz2017 and the vignette data) to see whether the lower
  optima are reordered configurations there too; and the bootstrap's
  warm-start interaction (replicates warm-started from a local optimum
  reproduce it, so B1 is self-consistent with the local estimator and would
  change with it).

## Beyond the brief

1. The third-pass record's "6 of 37" understates the engine finding: the
   same 37 fits give 16 of 37 at 321 starts, and the interior truth is
   worse (23 of 33). Any statement of the share should carry the search
   that produced it.
2. At boundary N = 5000, interior lower minima exist with gaps under 0.1 T
   units and angle shifts of 15 to 50 degrees on scales whose communality
   rises by about .1: near-flat directions in the regime the ill-conditioned
   Hessian marker already names. They do not change T materially; they do
   mean two estimates differing by several standard errors have the same
   likelihood to two decimals.
3. `m167_profile_stat()` does not guard `nlminb()`; a start that throws
   (a singular solve inside the objective) aborts the component to NA.
   The prototype wrapped it; the fixed P1 should.
4. The prototype's verify-and-continue loop has no round or run cap; that
   is the first thing a bounded P1 needs (Q3).
5. The survey's 40 not-accepted fits of 800 and 6 multimodal flags among
   760 accepted are consistent with the record's exclusion counts.

## Recommendations

1. **Apply.** P1's F_hat is the global minimum found by its own anchor
   pass; the outward search starts at the anchor's component value; the
   study records per fit the anchor gap and whether the engine's estimate
   lies inside P1's interval. Recorded under the corrections heading as
   the implementation following the definition.
2. **Apply.** Verified crossings: dispersed starts plus zeta-wall probes at
   every finite crossing, continuation outward from a verified lower
   point, a cap of 3 rounds and a per-component run budget, with "unverified"
   reported when a cap is hit.
3. **Apply.** Basin-library continuation: the anchor pass's distinct local
   minima are starts at every evaluation, with the component set to s.
4. **Apply.** A 10-fit timing pilot on an unloaded core gates the study
   run at 6 CPU-seconds per fit median; the Q4 check must pass; a fourth
   defect return removes P1. Failure of the pilot or the check removes P1
   from the verdict with its figures reported as not established.
5. **Apply.** The Q4 check replaces check 4d: independent anchor, 24
   dispersed starts plus probes per endpoint, pass on no endpoint below
   3.79 or above 3.89 and anchor agreement within .05.
6. **Apply.** A non-interval set is reported as its hull, with the
   re-entry count per component; a cell with over 5% carries the share.
7. **Apply.** `nlminb()` in `m167_profile_stat()` wrapped so a throwing
   start is skipped, not fatal.
8. **Apply.** Documentation of the engine's contract in the `cpm_fit()`
   help page and DESIGN.md as in Q6, and a candidate row for the
   estimator-definition milestone with the evidence list in Q6. Tier:
   Sonnet for the documentation, Fable at the plan gate of the milestone.
9. **Consider.** The profile followed from both ends as a supplement to
   the basin library (one extra pass of evaluations; unmeasured).
10. **Consider.** Recording in D-068's rewrite that the B1, B2 and F-family
    figures describe the theory-basin estimator, with the share of accepted
    fits where a lower optimum was found, as descriptive context.
11. **Reject.** The engine's battery as the profile's start set: theory-
    centred, it reached none of the basins in Q1.
12. **Reject.** Dispersed random starts alone as the global guarantee: the
    search does not saturate at 150 restarts on the free problem, and 20
    starts per evaluation is three times the budget.
13. **Reject.** Anchoring P1 to the engine's F_hat: it raises the cutoff by
    the gap, more than doubling it in 2 to 14% of accepted fits at
    N <= 1000, and is not a likelihood-ratio interval.
14. **Reject.** A hotfix of the engine: the design specifies this
    estimator, the global solution is the worse estimate in simulation,
    and the T calibration and the published oracles rest on it. The
    estimator question is a milestone with the evidence in Q6.
15. **Reject.** Re-running the K = 6 prototype as written: unbounded
    rounds, no run budget, and no measurement of why it did not finish.
