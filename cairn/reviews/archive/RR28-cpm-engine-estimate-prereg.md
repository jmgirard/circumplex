# RR28: The pre-registration and decision rule of the CPM engine-estimate study (M170)

- **Date:** 2026-10-27
- **Brief:** `cairn/reviews/RB28-cpm-engine-estimate-prereg.md`
- **Binding criteria:** not requested (none emitted)
- **Reviewer:** Fable, independent; the brief's materials only, plus the
  computations listed under Evidence. Branch `m170-cpm-engine-estimate-study`
  at 76299d92, working tree clean.

## Evidence

Read in full: the pre-registration block and `cell_metrics()`, `favors()`,
`decide()` in `devel/m170-engine-estimate.R`; the helpers and checks; M170;
RR27 Q1, Q2, Q6; D-071; DESIGN.md's principles and the D-071 section; the
named ranges of `R/cpm_fit.R`, plus `cpm_spec()`, `cpm_pack()`,
`cpm_unpack()`, `cpm_start_values()`, `cpm_canonicalize()`,
`cpm_spec_reduce()`; M167's allowance text in `devel/m167-interval-oracle.R`
lines 106 to 120; the `cpm-boundary-fits` vignette's `sim` chunk.

Run by me, from the repo root:

- `Rscript devel/m170-engine-estimate-checks.R`: four PASS, with check 1's
  H gaps 0.807, 0.650, 3.313, 3.045, 1.963, 1.162.
- `M170_SMOKE=1 Rscript devel/m170-engine-estimate.R`: ran end to end;
  its decision was "keep E unchanged (branch 0)" because `cov_dev(W)` and
  `cov_dev(H)` were NA at boundary N = 250 with 4 replicates; the smoke file
  was deleted afterwards. Its AC5 print: jz2017 gaps 0 / 0; voc 0 / 0;
  verbal W 2.541, H 2.541 (T_E 3.994, df 2); demo 0 / 0.
- Scratch scripts, outside the repo, sourcing the helpers under
  `devtools::load_all()` (T units below mean (N - 1) F; "lower" means more
  than 0.01 T units below E):
  - `rr28_probe.R`: N = 250, both truths, M167's seeds (base 20260706,
    replicates 1 to 14; 24 accepted fits). Per fit: E, W, H (K = 100), H_K,
    the block's uniform family (400), and five deterministic families of
    my own around E's estimate in the full layout: 21 pairwise swaps of two
    free angles; the 21 swaps each with the wall (logit .99) on one of the
    two swapped scales (63); 28 two-scale wall starts; the 8 single-wall
    starts placed around g0 instead of E; the all-level start at .5 and
    at .9. Also the uniform family's hit rate: the share of its 400 runs
    ending within 0.01 T units of H's F.
  - `rr28_probe2.R`: on the same 24 fits, 200 random starts each at sd .7,
    1.2 and 2.0 around E's estimate and around g0; and the finished points
    of E, H and the family's best on four fits.
  - `rr28_probe3.R`: how often an accepted E fit has a communality equal to
    1 in double precision (40 replicates per cell at N = 250, 500, 1000,
    M167's seeds), and what the helpers do on such a fit.
  - `rr28_probe4.R` and `rr28_probe5.R`: N = 1000 (raw rows) and N = 2000,
    5000 (Wishart), both truths, 8 fits per cell, my own seeds (base
    99261027): W, H, H_K, uniform family, swap-with-wall and wall-around-g0
    families, hit rates; the family's best point on two interior fits.
  - Binomial and order-statistic arithmetic for Q3 and Q7, in R.

The scratch files are under
`/private/tmp/claude-501/-Users-jmgirard-github-circumplex/3944f3ed-882b-498a-bf3d-9720e85f216b/scratchpad/`.
Nothing was committed; the only file written in the repo is this report.

Probe samples are small (24 fits at N = 250, 8 per cell at larger N). I
use them for direction and for existence claims (a family found a point;
a bug fires), not for rates the study will measure.

## Q1. W's starts

**The 30 starts are a reasonable core but not the right set to ship as a
probe.** On the 24 N = 250 fits, H found a lower point on 16 and W on 10;
W reached H's F (within 0.01) on only 3 of the 16. RR27 Q1 already said
why: the interior-truth lower points swap two scales across the circle,
and a single-angle shift cannot produce a swap. My 21 pairwise swap starts
found a lower point on 8 of 24 fits and reached H's F on 6 of 16; the
swap-with-wall set (63 starts) on 11 of 24 and 7 of 16. On interior fits
1, 5, 6 and 13 the swap start's gap equals H's to three decimals, so H's
point there is exactly a two-scale swap. The two-scale wall set found 7 of
24 (3 of 16 reaching H).

**Starts around g0 add something E-centred starts miss.** The 8 single-wall
starts placed around g0 found a lower point on 6 of 24 fits; on boundary
replicate 2 they reached H's 0.926 where W's own wall starts stopped at
0.330, and on boundary replicate 4 they found 3.746 T units, below both
W's 0.570 and H's 1.740 (the uniform family found the same 3.746). RR27's
"64 to 88% from g0-centred random starts" has a deterministic counterpart.

Union of W, the swap-with-wall set and the 8 g0 wall starts: a lower point
on 14 of the 16 fits where H found one (W alone: 10), and H's F or better
on 10 of 16 (W alone: 3). Cost: 101 refits, about 1.3 s per fit at 13 ms
each, far under the block's 10 s. At N >= 1000 on the boundary truth, W
already equals H on every found fit (probe 4: 4 of 8, 2 of 8, 2 of 8 with
identical gaps), so the extra starts matter at N <= 500 and on the interior
truth.

**The all-communality level.** Setting .99 is the right reading; the start
is there to drop every scale onto the wall and let the optimizer pull back
the ones that belong inside, which no level below the wall does. Levels .5
and .9 found a lower point on 1 and 3 of 24 fits, each already found by
another family. One start at .99 suffices.

## Q2. H's strength

**K = 100 is defensible as a bounded search and is not the global minimum
at N <= 1000 or on the interior truth at any N studied.** On the 24 N = 250
fits, H beat H_K by more than 0.01 on 4 (S1 share .17), and the uniform
family beat H on 7 of 24 by up to 3.5 T units (interior replicate 1: E's
T 13.54, H 8.68, family 5.23). At N = 1000 to 5000 the family beat H on 2
of 7 and 2 of 8 interior fits (gaps 7.98, 0.85, 4.83, 0.98) and 1 of 8
boundary fits at N = 5000. What those family points are matters for the
whole study (see Q3 and Beyond the brief 2).

**Centres and spread.** On the same 24 fits, 200 starts per family: g0 at
sd .7 found a lower point on 16 fits and was the best family on 7; E's
estimate at sd .7 on 13 and 2; g0 at sd 1.2 on 16 and 10 (sum of gaps 49.0
against 41.5 for g0 at .7); E at 1.2 on 11 and 5; sd 2.0 added nothing
over 1.2. Two of the E-centred family's failures are the infinite-start
bug below (Beyond the brief 1), so the E-centred figures are deflated;
even so g0 is at least as productive at every sd. A split that replaces
"2K around E at sd .7" with "2K around g0 at sd 1.2" would likely buy more
for the same cost, but 24 fits is too few to fix it on; I mark it consider,
to be re-checked on the checks script's fits after the bug is fixed.

The seed dependence at K = 100 (replicates 26 and 28) is the expected face
of non-saturation and is not fixable at this budget: RR27's 150-per-centre
search did not saturate either. The block should say that H is a reference
search, not the global minimum, in every cell where S1 or S2 fails, which
on my evidence is every cell except perhaps boundary N = 1000 to 5000.

## Q3. The saturation rule

**S1 at .02.** At n = 475 accepted fits the binomial SE of a .02 share is
.0064; the observed share is at most .02 with probability .52 when the true
rate is .02, .095 at .03 and .008 at .04. So S1 tests "doubling gains on
under about 2.5% of fits", which is a fair reading of "saturates" for a
doubling check. Its known weakness is that a doubling check passes when
both K and 2K miss the same basins; that is S2's job.

**S2's 1 of 50.** P(at most 1 of 50 | per-fit rate r) is .91, .74, .56,
.40, .28, .08 and .03 at r = .01, .02, .03, .04, .05, .08 and .10. It
discriminates r >= .08 from r <= .02 and is a coin flip between .03 and
.04. That is matched to S1's bar; it is not sharp.

**Independence.** The uniform family is independent of the two Gaussian
centres in construction, and empirically: its runs reach H's basin almost
never (median hit rate .002, 1 of 400 starts, at N = 250; .002 to .02 at
larger N), yet it found lower points H missed on 7 of 24 fits. The two
searches see different parts of the surface. That also means a pass is
ambiguous as written: at a hit rate of .002, "the family found nothing
lower" could mean "nothing lower exists" or "the family cannot reach the
region at all". S2 should record the hit rate beside its count so a pass is
read with it.

**Is 400 strong enough?** Strong enough to fail S2 where it should: on my
evidence S2 fails at N = 250 (7 of 24 against 1 of 50 allowed) and in the
interior cells up to N = 2000 (2 of 7, 2 of 8). It is not strong enough to
certify a pass: 400 uniform draws in a 7-torus times 8 logits times 3
weight logits are sparse, and the hit rate says so.

**The expected outcome and what the rule still says.** A fail at N <= 1000
is the expected outcome on RR27's and my evidence, and so is a fail in the
interior cells at N = 2000. The rule still says something useful there: a
fail limits the decision to options that keep E (branches 3 to 6), which
is exactly D-071's Reopens clause, and it labels H's shares as lower
bounds. The block should pre-register this expectation so a run of fails
is not read as a surprise or a defect in the study.

One definitional point: S1 compares H after its polish with H_K before
its polish. The polish gate (F_red <= F + 1e-8) makes the difference at
most 1e-8 in F in one direction and unbounded in the other (a reduced
refit can fall further). It cannot create a false fail; it could in
principle create a false "doubling gain". Define S1 on the pre-polish F of
both, which `m170_estimators()` already holds (`h_pt$F`).

## Q4. "AC4's figures favor X"

**The reading is sound, with two corrections.**

*Restricting to lower fits.* The sign of the family-RMSE difference is the
same whether computed over all accepted fits or over X's lower fits,
because on the other fits X's record is E's (`recs$W <- recs$E` when W
finds nothing lower). RMSE_X^2 - RMSE_E^2 = (1/n) sum over lower fits of
(e_X^2 - e_E^2). So the six "no worse" inequalities need no restriction,
and judging the shipped estimator on every fit is right (a changed
estimator changes every fit). The clause "at least one family RMSE at most
.95 of E's" is different: it is a magnitude, and it must hold in every
cell. In a cell where X differs from E on 3% of fits (RR27: interior N =
1000) a 5% RMSE reduction over all fits is unreachable, so branches 1 and
2 are closed by that clause alone whatever the figures. Require the .95
clause in at least one cell, and "no worse" in every cell.

*The replacement rule breaks the equivalence.* The ESTIMATORS paragraph
lets a refit replace E when its F lies more than 1e-8 below E's, while "a
lower discrepancy" needs 0.01 T units. Between the two (1e-8 to about
4e-5 in F at N = 250) X's point differs from E's but the fit is not a
"lower fit". On a wall, F is flat in u beyond about u = 10, so a refit can
land on a numerically equal point with a different u and a different (or
undefined) analytic SE: X's coverage and se_defined figures then differ
from E's on fits the block calls identical. The smoke run showed the
symptom (cov_dev(W) NA with share_lower(W) = 0). Replace E only when the
gap exceeds the threshold; "the lower fits of X" and "the fits where X
differs from E" then coincide.

*Averaging RMSE over a family.* It is the right summary for a shipped
estimator, because a swap of two scales enters the mean through two large
squared errors and dominates it: RR27 Q2's 17.7 against 30.0 degrees
(boundary 250) is this average. Per-parameter RMSE is reported anyway.

*T-exceedance.* The figure is the calibration of the test the package
runs, applied by the same rule to all three estimators; as a reported
figure it is fair. As a gate it is weakly informative: X's T is lower
than E's on the lower fits by construction, so t_exceed(X) <= t_exceed(E)
mechanically, and whether that moves |t_exceed - .05| down depends only on
which side of .05 E sits. The df difference (read after each estimator's
own polish) is the package's convention and is small (df + 1 at most).
Heywood points are not where the chi-square reference is right for any of
the three, and the gate does not pretend otherwise. Keep it in favors(),
and say in the record that it measures the test, not the estimate.

## Q5. The decision rule's order and thresholds

**.05.** Defensible as "seldom fires". It will not be the deciding read:
RR27's wall probe found 42 to 100% of the boundary-truth lower points,
which exist in about 40% of accepted fits at N = 250, so share_lower(W)
there will be far above .05 and branch 3 cannot fire. Noise at .05 is
irrelevant to branch 3 (every cell must be below).

**2.0.** This is the read the study hinges on, and it sits in the middle of
the expected range: RR27's median gaps among finds were 1.1 to 2.1 T units
over the cells. The median of W's gaps over n_lower fits has a Monte Carlo
SE of about .25 at n_lower = 50, .18 at 100 and .13 at 200 (lognormal with
median 1.8, sdlog .8, simulated); with a true median of 1.8 the observed
median clears 2.0 in 23%, 15% and 7% of runs. Branch 5 against the default
would then be decided by noise in one cell. The rule needs an allowance
(Q7), and the block's own logic supplies the direction: the default
absorbs the ambiguous case, so branch 5 should fire only when the figure
clearly clears 2.0. The threshold itself (a p-value of .13 moving to .22 at
df = 10; the arithmetic checks) is as defensible as any single number; it
is the clearance that is missing.

**"All ten cells" for branches 1 and 2.** Right for "no worse" (every fit
changes). Not right for saturation in branch 1: W is a fixed deterministic
estimator whose properties the study measures directly, and whether a
global minimum exists below it is irrelevant to whether W is closer to the
truth than E. AC1's shape requires sat(c) for W; I flag the disagreement
rather than work around it. In practice it is moot: RR27 Q2 and my probes
say W's lower points are farther from the truth, so favors(W) will fail on
its own.

**The default.** The diagnostic is the right default under GP2 (caution
loudly). It has no false positives: it fires only when a lower F has been
found, which is a fact about F. Its false negatives are the issue (Q6). A
default of "keep E unchanged" would leave users with the help-page
sentence alone in cells where a lower optimum exists in a third of fits,
and would need a positive reason to stay silent that the figures cannot
supply.

**The GP4 tie-break.** The reading is defensible but cites the weaker
principle. GP4 forbids breaking commitments; both options add rather than
break. The real asymmetry is IP3: a second fit statistic is a shipped
numeric result that needs two independent oracles and a T-calibration
re-run, while a flag and a caution line are a diagnostic. Name IP3 beside
GP4. The tie-break's premise ("the gap is too small for its size to carry
the reading") is correct as far as it goes; the stronger point is that W's
"lowest discrepancy found" is not the model's minimum either (probe: the
uniform family went below W by up to 8 T units), so a second T printed
beside E's would be read as the true fit and is not.

## Q6. Shipping constraints

**Can "report H" be shipped?** By the letter of IP4, a search that seeds a
local stream and restores `.Random.seed` is deterministic and leaves the
stream untouched. By its spirit, an estimator that depends on an internal
constant seed is not one to ship: the checks show H at K = 100 differing by
1.2 T units across seeds on replicate 26. The only state in which "report
H" is coherent is the one branch 2 demands: saturation in every cell, where
the seed no longer matters because every seed reaches the same point. Then
a later milestone can ship any search demonstrated to reach it. So branch
2 may exist, but the block's shipping sentence covers only the diagnostic
and report-beside options and leaves "report H" undefined. Add: "report H"
means a later milestone ships a search demonstrated seed-independent in
every cell; H's 430 starts at 5.5 CPU-seconds per fit is its reference, not
its definition. On my evidence branch 2 will not fire (S1 and S2 fail at N
<= 1000 and in the interior cells), so the sentence costs nothing.

**W's detection power.** The rule reads share_lower(W) but defines no
figure for what W misses. Both shares are reported per cell, so a reader
can divide, but the diagnostic's silence is the one thing a user will
over-read, and the D-entry needs the number in front of it. Add to M3 a
recall figure, n_lower(W) / n_lower(H), and the share of accepted fits
where H or the uniform family is lower but W is not. On my 24 N = 250
fits, W's recall against H is 10 of 16 (14 of 16 with the Q1 starts), and
against "H or the family" lower; at boundary N >= 1000 it is 8 of 8.

## Q7. The replicate rule and Monte Carlo precision

**The pilot's R.** At 13 ms per refit, one replicate costs about 6.0 CPU-s
(E 0.1, W 0.4, H 5.2, SEs small) and one family run 5.2 s. Projected cost
at R = 500: 10 x 500 x 6.0 + 10 x 80 x 5.2 = 34,160 s, times 1.2 = 41,000
s = 11.4 CPU-hours, so R = 500 fits only if the per-replicate median stays
under about 6.1 s; otherwise 450. Either is adequate.

**Precision at n ~ 475.** Binomial SE .0064 at a share of .02, .010 at .05,
.018 at .20. S1's .02 bar behaves as computed in Q3. Branch 3's .05 is not
the deciding read. The median gap is (Q5): SE .13 to .25.

**Allowance.** Yes, for the median in branch 5. M167's band carried a
Monte Carlo allowance scaled to n; the analogue for a median is
distribution-free: the one-sided 95% lower confidence bound on the median
is the order statistic at rank floor(n/2 - 0.8224 sqrt(n)) (binomial
normal approximation; exact ranks from qbinom(.05, n, .5) are as easy).
Branch 5 fires when that bound is at least 2.0 in some cell with
share_lower(W) >= .05. For the shares, use the point estimates: branch 3's
read is far from its threshold, and S1's .02 is a bar, not a boundary. If
one allowance rule is wanted for consistency, the Wilson one-sided bound in
the direction away from the default does the same job.

## Q8. Removal (second escalation)

**Not running the study** closes the question on RR27's evidence: shares
and gaps from one reviewer's uncommitted survey with an incomplete search;
estimate quality from 60 replicates per raw cell; no measurement of a
shippable deterministic probe; no real-data fits beyond the two oracle
matrices. A D-entry on that basis could say "keep E; the help page covers
it" and would be defensible under D-071. It could not specify a diagnostic
(no detection rate per cell for any deterministic start set), could not
say whether the published fits carry reordered lower points (the smoke run
already shows the verbal matrix does, gap 2.54 from W's 30 starts), and
would leave GP7's reopening evidence undefined. A later milestone that
wanted the diagnostic would have to run this study anyway.

**Running as pre-registered** produces a decision with four known defects:
W is silently disabled on the Heywood fits with a communality at exactly 1
(Beyond the brief 1; about 5 to 8% of N = 250 fits, the fits the diagnostic
is for); branch 0 can return "keep E unchanged" on an NA that only branch
2 reads; the AC5 `demo` fit is not the vignette's sample; and branch 5
against the default is a noise read.

**Running as amended** costs the same 12 CPU-hours and yields what a
diagnostic needs: the detection table of a shippable start set per cell,
its recall against H and against the uniform family, the gap distribution,
the share of lower points that are reorderings, the T-exceedance and
Wald figures that would let anyone argue for W later, and four real fits.
The saturation verdicts will mostly fail, and that is itself a result the
D-entry can state with figures: the global minimum of F on these data is
often a reordered configuration (Beyond the brief 2), which is the
strongest support D-071's estimator has had.

Recommendation: run it, amended. Model tier for the amendments: Opus for
the helper fix, the start-set extension and the block corrections; the
corrections are mechanical once agreed, and the risk is in the block's
wording, which this report fixes.

## Q9. Readings the block or code leave open

1. **Infinite starts on exact-wall fits** (Beyond the brief 1). Not a
   two-way reading but a silent disabling of W on the fits that matter.
2. **Replacement rule against the threshold** (Q4): X's point can differ
   from E's on fits the block calls identical, changing coverage and
   se_defined figures on "non-lower" fits.
3. **Branch 0's reach.** `decide()` collects every quality figure of E, W
   and H in every cell into `read` and fails closed on any NA. An NA in
   H's cov_dev (one parameter's SE undefined on every accepted fit at H's
   point in one cell, which the wall makes possible) then returns "keep E
   unchanged" at branch 0, although only branch 2 reads H's quality, and
   branches 3 to 6 read only W's share, gap and CPU. The smoke run hit this
   path. Scope it: an NA in a favors() input makes favors(X, c) FALSE
   (adoption fails closed); an NA in share_lower(W), gap_median(W) where
   read, or the pilot's CPU figures returns "keep E unchanged".
4. **AC5's `demo` seed.** The vignette sets `set.seed(2026)` immediately
   before `cpm_simulate(cpm, n = 2500)` (vignette line 154); the block and
   `real_cases()` continue the stream from the bootstrap instead. The
   study's `demo` is therefore not the vignette's `sim`. Add
   `set.seed(2026)` before the simulate call. The bootstrap does not change
   the point estimate `cpm_simulate()` draws from, so `boots = 500` is
   spent for nothing, but keeping it is harmless and matches the chunk.
5. **S1 across the polish** (Q3): define on pre-polish F for both.
6. **Non-stationary lower points.** `grad_ok` is recorded for W and H but
   the block does not say whether a lower refit that fails the gradient
   criterion counts as "a lower discrepancy". It should count (F is F,
   and a non-stationary point still proves a lower value exists) and the
   block should say so, with grad_fail reported beside the shares.
7. **Replicates that error after E.** A `try-error` from anything after
   `m170_engine_e()` drops the replicate from `reps` entirely; it is
   counted in `n_error` but is neither "excluded" (the block's term for E
   errors) nor accepted, and it is a selection on the outcome if the error
   correlates with the fit. The helpers catch refit, finish and SE errors,
   so it should be rare; the block should say that n_error > 0 in a cell
   is reported in the record and in the D-entry with the messages.
8. **Seeds.** No collision: data seeds 41,262,028 to 42,266,527; H seeds
   541,262,028 to 542,266,527; AC5 seeds 50,261,028 to 50,261,031; the
   checks' 60,261,028 and up; M4 and M167's bases below 3.7e7. The H and
   family draws of replicates 1 to 80 are identical in the pilot and the
   full run, by construction. The pilot's data for replicates 1 to 10 are
   the full run's first 10 replicates; the block says the pilot's other
   output is not reported, which is the right rule, and the pilot does not
   compute the decision.
9. **cpu_ok(H)** at 10 s per fit is generous for a function that now takes
   0.1 s; it is not a reading problem, only a note for the later milestone.
10. **"Every cell" and the family window.** S2 runs on replicates 1 to 80
    and needs 50 accepted; at RR27's 95% acceptance that gives about 76.
    Fine. The code takes the first 50 accepted in replicate order from
    `mclapply()`'s input-ordered output; the order is preserved.
11. **The nested subassignment** `covered[length(e) - 3:0][est$beta == 0] <-
    FALSE` works as intended (checked: it marks the removed weight only).

## Beyond the brief

1. **`m170_base_par()` produces infinite starts on exact-wall fits.** When
   the engine drives a communality logit past about 37, `plogis()` returns
   exactly 1, and `qlogis(eng$zeta)` is `Inf`. On such a fit every one of
   W's 30 starts and every E-centred random start contains `Inf`. `nlminb()`
   does not throw (n_failed stays 0); it returns a point far above E's F
   (on boundary N = 250 replicate 6, with u up to 148, the best of 200
   E-centred starts was 17 T units above E; interior replicate 8, 30
   units). W then equals E and the fit is counted as "W found nothing".
   Frequency among accepted fits, M167's seeds, 40 replicates per cell:
   boundary N = 250 3 of 37, interior N = 250 2 of 33, interior N = 1000 1
   of 40, none at N = 500. Check 3 passed because none of its 11 fits has
   an exact-1 communality. Fix: build the angle and u blocks of the base
   from `eng$par` (position-stable across the polish) and clip u to
   [-30, 30] (plogis(30) = 1 - 9e-14, an F change far below 1e-8), or clip
   `qlogis(pmin(eng$zeta, 1 - 1e-13))`; extend check 3 to boundary N = 250
   replicate 6, and have check 2 assert every W start is finite.
2. **The global minimum of F on these data is often a non-circumplex
   configuration.** The uniform family's lower points: interior N = 250
   replicate 1, T 5.23 against E's 13.54, order 1 2 5 6 3 4 7 8 with two
   wall scales; interior N = 1000 replicate 1, T 9.86 against 17.84, order
   1 6 3 8 5 2 7 4, no wall scale, beta_3 = .29 above beta_1 = .14;
   interior N = 2000 replicate 2, T 15.06 against 16.51, order 1 2 3 8 7 6
   5 4 (a half-reflection). These are stationary points (gradient norm
   2e-9 to 2e-7) of the same model with a much better likelihood and no
   resemblance to the hypothesis. Two consequences. First, "the global
   minimum" is not a candidate estimator at any N the study covers, which
   the D-entry can now say with points, not shares. Second, "a lower
   optimum" conflates two things a user needs told apart: a nearby basin in
   the same cyclic order (a wall or a shift; the fit statistic is
   overstated) and a reordering (the data prefer a different arrangement of
   the scales). order_changed(X) is recorded for W and H; S2's family point
   is not finished or recorded. Finish the family's best point and record
   its order and Heywood count, and report share_lower split by
   order_changed, so the diagnostic's later wording can distinguish them.
3. **W's reordered finds on published data.** The smoke run's verbal
   matrix: W's 30 deterministic starts reach a point 2.54 T units below E
   (RR27 found 3.16 with 1 of 200 random restarts). AC5 will report its
   order; the block's "any change in angle order" covers it.
4. **The E-centred random family is wasted on exact-wall fits** beyond the
   bug: with u at 148, N(0, .7) perturbations stay on the wall, so 200
   starts explore nothing in that coordinate. After the clip to 30 the
   same holds (plogis(29.3) = 1). If the E-centred family is kept, draw its
   u perturbations from the clipped base; better, let the g0 family carry
   the wall exploration (Q2).
5. **The budget arithmetic is tight at R = 500** (Q7); the rule handles it.
   Running the pilot "on one worker, nothing else running" and the full
   run on 17 workers means the full run's per-replicate CPU will likely
   exceed the pilot's (memory bandwidth, thermal), so the 1.2 margin is
   doing real work. If the full run overruns, that is a wall-clock fact,
   not a pre-registration event.

## Recommendations

Each marked apply / consider / reject-with-reason. **[block]** marks a
change to the pre-registration block, for the CORRECTIONS heading.

1. **Apply.** Fix `m170_base_par()`'s infinite starts (Beyond the brief 1);
   extend check 3 to an exact-wall fit and check 2 to assert finite starts.
   **[block]**: the ESTIMATORS paragraph says how E's estimate is placed in
   the full layout; add "a communality at 1 in double precision is placed
   at logit 30".
2. **Apply. [block]** Extend W's deterministic set: the 21 pairwise swaps
   of two free angles, the 42 swap-with-wall starts, and the 8 single-wall
   starts around g0 (101 starts). Evidence: Q1. The all-level start stays
   one start at .99.
3. **Apply. [block]** A refit replaces E in W or H only when
   (N - 1)(F_E - F_refit) > 0.01, the threshold, so "the lower fits of X"
   and "the fits where X differs from E" coincide (Q4, Q9.2).
4. **Apply. [block]** Scope branch 0: an NA in a favors() input makes
   favors(X, c) FALSE; an NA in share_lower(W), in gap_median(W) where it
   is read, or in the pilot's CPU figures returns "keep E unchanged"
   (Q9.3). Change `decide()` to match.
5. **Apply. [block]** favors(X, c): the six "no worse" inequalities in every
   cell; the "at least one family RMSE at most .95 of E's" clause in at
   least one cell (Q4).
6. **Apply. [block]** Branch 5 fires when the one-sided 95% lower
   confidence bound on gap_median(W) (order statistic at rank
   qbinom(.05, n_lower, .5)) is at least 2.0 in some cell with
   share_lower(W) >= .05 (Q5, Q7). Shares keep point estimates.
7. **Apply. [block]** AC5's `demo`: `set.seed(2026)` immediately before
   `cpm_simulate(cpm, n = 2500)`, as the vignette does (Q9.4). Fix
   `real_cases()`.
8. **Apply. [block]** Add to M3: recall(W) = n_lower(W) / n_lower(H) and
   the share of accepted fits on which H or the uniform family is lower
   but W is not; the D-entry carries them beside any diagnostic (Q6). Add
   to S2's record the family's hit rate (share of its runs within the
   threshold of H's F) and the finished best family point's order and
   Heywood count (Q3, Beyond the brief 2). Report share_lower(X) split by
   order_changed.
9. **Apply. [block]** Pre-register the expectation: on RR27's and this
   report's evidence the saturation verdict is expected to fail at N <=
   1000 and in the interior cells at N = 2000, so branches 1 and 2 are
   expected to be unreachable and the decision is expected among branches
   3 to 6; a fail is a result, not a defect (Q3).
10. **Apply. [block]** State what "report H" means for shipping: a later
    milestone ships a search demonstrated seed-independent in every cell;
    the study's H is that search's reference, not its definition (Q6).
11. **Apply. [block]** S1 compares the pre-polish F of H and H_K (Q3); a
    lower refit counts whether or not it meets the gradient criterion, with
    grad_fail reported (Q9.6); n_error > 0 in a cell is reported with its
    messages (Q9.7).
12. **Apply. [block]** Name IP3 beside GP4 in the tie-break (Q5).
13. **Consider. [block]** Replace H's "2K around E's estimate at sd .7" with
    "2K around g0 at sd 1.2", after re-checking on the checks script's six
    fits once recommendation 1 is in (Q2). If kept, draw the E-centred u
    perturbations from the clipped base (Beyond the brief 4).
14. **Consider.** Drop sat(c) from branch 1 (W needs no saturation to be a
    well-defined estimator). This disagrees with AC1's shape, so it is the
    user's call; the practical effect is nil because favors(W) is expected
    to fail on its own (Q5).
15. **Consider.** Use one allowance rule (a Wilson one-sided bound away
    from the default) for every threshold if consistency is preferred over
    the point estimates for shares (Q7).
16. **Reject.** A default of "keep E unchanged": the diagnostic has no false
    positives, GP2 asks for the caution, and silence would need a reason
    the figures cannot give (Q5).
17. **Reject.** Removing branch 2: with the sat guard and recommendation 10
    it is coherent and costs nothing; without it the rule could not record
    a saturating result if one appeared at large N (Q6).
18. **Reject.** More all-communality levels: .5 and .9 found nothing the
    other families missed on 24 fits (Q1).
19. **Reject.** Not running the study: RR27's evidence cannot specify a
    diagnostic or state W's detection rate, and the amended study does both
    at the same cost (Q8).
20. **Reject.** Raising K or the family size to chase saturation: the
    uniform family's finds at N = 250 to 2000 are reorderings no shippable
    estimator should reach, so saturation in those cells would certify the
    wrong object (Beyond the brief 2).
