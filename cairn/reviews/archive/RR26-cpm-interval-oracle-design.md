# RR26: Design of the CPM interval-coverage study, before its full run (M167)

- **Date:** 2026-10-21
- **Brief:** `cairn/reviews/RB26-cpm-interval-oracle-design.md`
- **Reviewer:** Fable 5.1, independent of the planning session
- **Status:** advisory (the brief's header says binding criteria were not requested)

## What this review read and ran

Read: the pre-registration block and harness of `devel/m167-interval-oracle.R`,
`devel/m167-interval-helpers.R`, `devel/m167-interval-checks.R`, the control
run `devel/m167-interval-control.rds` (every per-component table), `R/cpm_fit.R`
(engine acceptance, polish, Heywood flag, `cpm_analytic_se()`,
`cpm_bootstrap()`), `cairn/DESIGN.md` lines 324 to 386, `cairn/references/browne1982.md`,
`devel/m4-coverage-oracle.R`, the M167 milestone file, D-002, D-010, IP3, IP4.

Ran (reviewer scripts in the session scratchpad, not in the repo; no repo file
other than this report was changed):

- D1: 300 M4-seeded fits per cell at N = 500 and 1000, both truths, point
  estimates only (no bootstrap). These are the control's own fits.
- D2: 55 to 60 datasets per cell on fresh seeds (77000 + 1e4 truth + 10 N + i;
  not the M4 seeds) at boundary N = 500, boundary N = 1000, interior N = 500,
  with 400 resamples each of B1 and F2, scoring B1, F3, F2p, F2bc, the basic
  bootstrap on both replicate sets, B2 and F1. At this size a coverage near .8
  carries a Monte Carlo SE of about .05, so differences under .10 between
  families are not resolved; the signs and the orderings are.
- D3: a profile-likelihood interval prototype for zeta and beta on the same
  D2 datasets.
- D4: variant B (angles fixed at theory) and variant C (one common zeta) fits
  on 200 fresh datasets per truth at N = 500; and cold (full multi-start
  battery) versus warm-started bootstrap refits, 6 datasets x 60 resamples.
- Population-level: the Hessian of F at each truth matrix, its eigen-structure,
  and profiles of F over a common zeta shift and over a single zeta_i, with
  everything else re-optimized.

## Answers

### 1. Mechanism behind the control-run pattern

**Short answer.** The beta pattern is the shadow of an upward finite-sample
bias in the per-scale communalities, and that bias comes from the weakly
identified joint angle/communality modes of variant A on these truths (the
Heywood solutions are the same mode hitting its wall at zeta = 1). The
softmax constraint only routes the loss in beta_0 into beta_2 and beta_3; the
polish and the warm start are not the cause. The bootstrap is worse than Wald
for two reasons that have nothing to do with pile-up at zero: it reproduces
only part of the bias, and the Wald figure is conditional on a subset of fits
that excludes most of the biased ones.

**Evidence, in order.**

(a) *The estimator itself is biased, and the bias does not shrink relative
to its SD between N = 500 and 1000.* D1, boundary truth (beta =
.45/.35/.15/.05): mean beta-hat .420/.346/.166/.068 at N = 500 (SD
.030/.022/.019/.023) and .427/.348/.162/.063 at N = 1000 (SD
.022/.015/.013/.016). The beta_0 bias is -.030 and -.023, i.e. 1.0 and 1.05
SD; P(beta_0-hat < .45) is .85 and .86. Interior truth: beta_0 .330 at N = 500
(bias -.020, SD .024) and .339 at N = 1000 (-.011, SD .018). Mean zeta-hat is
.78 to .80 at N = 500 and .775 at N = 1000 for a truth of .75;
cor(beta_0-hat, mean zeta-hat) = -.59 to -.69 across the four cells. A bias
that stays near one SD while N quadruples is the signature of a boundary
effect, not of a smooth O(1/N) bias; it is why coverage is flat from 250 to
1000.

(b) *A single communality can run to the wall almost for free, and when it
does the re-optimized beta is exactly the observed mean.* At the boundary
population matrix, fixing zeta_1 = .995 with all other 17 parameters
re-optimized costs N·F = 1.42 at N = 500 (2.85 at N = 1000), below the 3.84
cutoff, and the re-optimized beta is .422/.349/.165/.064, which matches the
observed mean beta-hat .420/.346/.166/.068 to within .004 per weight. The
neighbors of the Heywood scale move to zeta = .82 and the rest to .72 to .73.
Interior truth: cost 2.91, beta re-optimizes to .336/.304/.207/.153 (observed
.330/.301/.208/.161). The common-zeta direction, by contrast, is strongly
identified (a shift to .80 costs N·F = 10.3), so this is a per-scale effect.
The Hessian at the boundary truth has its two smallest eigenvalues (.0021,
.0033 against a top of 1.64) on angle-stretch modes with communality
compensation and zero beta loading; the implied angle SEs at N = 500 are 15
to 34 degrees (boundary) versus 6 to 10 (interior). At the interior truth the
smallest mode is the alternating (+ - + -) communality mode (.0051), the
Nyquist reading in DESIGN.md.

(c) *Removing the angle freedom removes the bias and the Heywood rate on the
same data.* D4a, boundary N = 500, 200 fresh datasets: variant A gives
beta-hat .422/.347/.167/.065, mean zeta .788, Heywood 61%; variant B (angles
fixed at theory) gives .452/.348/.150/.050, mean zeta .751, Heywood 0%;
variant C (one common zeta) .447/.347/.153/.053, Heywood 0%. Interior N = 500:
A .327/.300/.209/.164 with 68% Heywood; B .347/.301/.203/.149 with 14%
Heywood; C .345/.300/.203/.151 with 0%. So at the boundary truth the whole
effect is the angle/communality mode; at the interior truth most of it is, and
the residual 14% under fixed angles is the alternating communality mode.

(d) *The polish is not it.* Removed-harmonic rates in the control are 0 to
.041 (boundary N = 250, beta_3 only) and the shortfall is largest on beta_0,
which is never removed.

(e) *The warm start adds little.* D4b: over 6 datasets x 60 resamples the
warm-started mean beta_0* differs from the cold multi-start mean by at most
.015 (cold is higher in 5 of 6, by .001 to .015); the Heywood share of
replicates is .67 to 1.00 warm against .65 to .98 cold. A warm start keeps a
replicate on the wall slightly more often; it is a second-order contributor.

(f) *The softmax constraint transmits, it does not cause.* Sum-to-one forces
the beta_0 deficit into the other weights; the profile in (b) shows it lands
on beta_2 and beta_3 (not beta_1), which is exactly the control's miss
pattern (beta_2, beta_3 miss above; beta_1 .93 to .95).

(g) *Why the percentile bootstrap is worse than Wald.* D2, boundary N = 500:
the realized bias of beta_0-hat is -.028; the bootstrap's own bias estimate
(mean beta_0* minus beta_0-hat) is -.011 for B1 and -.009 for F2. The
bootstrap world starts from a fit that already sits nearer the wall (zeta-hat
.78 to .79, 70% Heywood), so its replicates are more Heywood still (93% of B1
replicates, 90% of F2) and zeta* averages .80; the wall truncates the
excursion and the bootstrap reproduces about 40% of the bias. At the interior
truth it reproduces 15% (-.003 against -.023). The percentile interval is
therefore centered about 1.4 biases below the truth, and every beta_0 miss is
below (control: 26 to 34% below, 0% above). The Wald interval is centered on
a 1-SD-biased estimate; a symmetric 1.96-SE interval with a 1-SD bias covers
Phi(.96) - Phi(-2.96) = .83, which is what B2 shows on the fits where it
exists (.84 to .89 in D2; .87 to .92 in the control). The rest of the Wald
"advantage" is conditioning: the SE is undefined on 66 to 76% of Heywood
fits and 0% of non-Heywood fits at N = 500 (D1, interior and boundary), so
B2's figure is taken on the better half. B1 restricted to the same SE-defined fits covers beta_0 at .82 at
boundary N = 500 (all fits: .65); B1 on Heywood fits covers .55, on
non-Heywood fits .88. The brief's phrase "on the same fits" does not hold for
the control table.

**Cheap diagnostics to add.** All fit-level, no extra bootstrap:

1. Store per-replicate records in the results file: the point estimates
   (angles, zeta, beta), the Heywood flag and the count of zeta_i > .995,
   the SE-defined flag, the removed-harmonic flags, each method's interval
   endpoints, and for each bootstrap the replicate mean and SD per component
   and the BC z0. With these the record can report, per cell and component,
   the realized bias of the estimator, the bootstrap's bias estimate, the
   Heywood share within replicates against within fits, coverage split by
   Heywood flag, and every method's coverage on the SE-defined subset. This
   is what separates "the bootstrap misses the bias" from "the bootstrap is
   too narrow" from "the Wald is conditioned on easy fits".
2. A variant-B refit (angles fixed at the theory angles) of every raw-data
   replicate, recording beta-hat, zeta-hat and the Heywood flag (about 0.3 s
   per fit). If the beta_0 bias and the Heywood rate fall to near zero under
   B on the same data, the angle/communality mode is confirmed in-sample; if
   they persist, the mechanism is elsewhere. This is the test that
   distinguishes the mechanism in (c) from a generic small-sample ML bias.
3. The cold-restart comparison on a 1% subsample of replicates is optional;
   D4b already bounds the warm-start contribution at about .01.

### 2. Can the candidate families close the gap; are they correctly specified

Specification first: all three are implemented as their sources define them.
F1's zeta arm reproduces CircE (checks script); F1's beta arm is the marginal
logit interval the block states; F2's Wishart draw is the exact distribution
of cor(X) under normality; the BC form matches its closed form. Two
specification notes: (i) the block's claim that "ln v stays finite because
zeta-hat < 1 under the logit parameterization" is false in double precision:
`plogis(u)` returns exactly 1 for u > 36.7, and 2 to 3% of accepted fits at
N = 500 have a zeta of exactly 1 (D1: 8 of 294 boundary, 6 of 273 interior),
where `m167_lnv_ci()` returns a NaN lower limit; the scoring then counts the
component as NA (conditional), which is the right outcome, but the block
should say so and the NA count should be reported. (ii) For a Heywood scale
with a defined SE, F1's zeta interval is effectively [0, 1] (the ln v SE
blows up as zeta -> 1): it covers, but it is vacuous; see Q3.

Can they close the gap: on the pilot (D2/D3, Monte Carlo SE about .05):

| beta_0 coverage | boundary N=500 | boundary N=1000 | interior N=500 |
|---|---|---|---|
| B1 percentile | .65 | .65 | .71 |
| F3 (BC on B1) | .81 | .82 | .71 |
| F2p | .61 | .67 | .71 |
| F2bc | .79 | .82 | .78 |
| basic on B1 reps | .74 | .88 | .71 |
| basic on F2 reps | .77 | .82 | .69 |
| B2 (SE-defined only; n = 28, 49, 31) | .89 | .84 | 1.00 |
| F1 (same subset) | .93 | .86 | 1.00 |
| profile likelihood (all fits) | .79 | .83 | .85 |

Zeta coverage, mean over the 8 scales: B1 .87/.91/.86; F3 .86/.92/.85; F2p
.87/.90/.86; B2 (conditional) .88/.92/.91; F1 (conditional) .91/.94/.96 with
zero misses above; basic .64/.78/.56; profile .93/.94/.93 with balanced miss
sides.

Reading per family:

- **F1.** Keep. The zeta arm is the one candidate that fixes the zeta
  shortfall's direction (B1 misses above the truth at N <= 500; F1's
  back-transform puts the upper limit at 1 for any high zeta-hat). Its beta
  arm is a reshaping of B2 and cannot remove a 1-SD bias; expect it to track
  B2 within .03. Both arms inherit B2's conditioning (SE undefined on 40 to
  73% of fits at N <= 500), so a pass is a conditional pass; see Q3.
- **F2p.** Keep. It agrees with B1 in bias, SD and coverage (D2: bootstrap
  bias -.009 vs -.011, SD .027 vs .028), which is itself a finding worth
  recording (parametric and nonparametric bootstraps fail the same way under
  normality). It will not pass beta_0 at N <= 1000. At N >= 2000 the wall is
  4 or more SEs away (SE(zeta) about .06 at N = 2000, .037 at 5000, .019 at
  20000) and the bias should decay as O(1/N); expect a pass from N = 5000 at
  both truths, with N = 2000 uncertain.
- **F3 and F2bc.** Keep, they are free given the replicates, but expect them
  to fail beta on the raw path: BC corrects a median shift that the bootstrap
  sees, and the bootstrap sees 40% of the shift at the boundary truth and 15%
  at the interior truth (where F3 gives no improvement at all: .71 vs .71).
  Mean z0 for beta_0 is .39 (N = 500) and .58 (N = 1000); at z0 = .58 the
  upper endpoint is the pnorm(3.1) = .999 quantile, the replicate maximum,
  so the interval is unstable in exactly the regime it is meant to fix. D-002
  does not reach them: its reason is circularity, and zeta and beta are
  linear. But the record should say that BC is the wrong tool for a
  wall-induced bias the bootstrap does not reproduce, not that D-002 forbids
  it.
- **Add: basic bootstrap** on both replicate sets (zero cost). It is the
  interval that subtracts the bootstrap's bias estimate and reflects, which
  is the textbook response to a bias the bootstrap reproduces; it is the best
  beta_0 figure among the unconditional families at N = 1000 (.88) and the
  worst for zeta (.56 to .78, because reflecting a right-skewed zeta*
  distribution overshoots the wall). Score it for beta and zeta both; the
  verdict will likely pass it on nothing, but its beta row is the comparison
  that shows how much of the shortfall is reproduced bias.
- **Add: profile-likelihood intervals** (call it P1) for zeta and beta, and
  consider angles. For each component c and value t, minimize F with c fixed
  at t (zeta_i: drop u_i; beta_0: parameterize the other three logits as
  w + const with the constant solving beta_0 = t; beta_k, k >= 1: v_k =
  logit(t) + log(1 + sum of the other exp(v_j))) and take the interval
  {t : (N - 1)(F_prof(t) - F_hat) <= 3.84}, root-found on each side from the
  estimate, with the natural bound as the endpoint when no root exists. It
  needs no standard error, so it exists for every accepted fit including
  Heywood fits; it is parameterization-invariant; it follows the ridge toward
  the truth (the cost of moving a Heywood zeta back to .75 is 1.4 units,
  which is why it covers zeta at .93 with balanced misses where B1 gives
  .86 to .91). In the pilot it is the best unconditional beta_0 family at
  N = 500 and ties the best at N = 1000; still short of .90 at the boundary
  truth, because no interval centered on a 1-SD-biased estimator reaches .95
  without being wide. Cost: 1.3 to 1.7 s per fit for 12 components with an
  unoptimized `uniroot` (20 to 30 constrained refits per component); 6000
  fits x 2 s = 3 to 4 CPU-hours, 6 to 7 with angles. Pre-register its
  calibration caveat: at a Heywood wall the LR statistic is a chi-square
  mixture, so the nominal 3.84 is approximate there. Polished fits need the
  reduced spec (the prototype skipped them; they are <= 4% of fits).
  Independent-oracle status: it is a different interval construction, not a
  different oracle in the IP3 sense; the coverage study remains one oracle
  type.
- **Reject: studentized bootstrap.** It needs a per-replicate SE; the SE is
  undefined on 60 to 93% of replicates at N <= 1000 (the replicate Heywood
  shares above), and a bootstrap-of-bootstrap variance costs 1000x.
- **Reject: Wald on the softmax scale.** A Wald interval on v_k =
  log(beta_k / beta_0) is a reparameterization of B2 that cannot be
  back-transformed to a single beta_k without the other v's, and the problem
  is a biased point estimate, not the shape of the interval around it.
- **Drop nothing.** The expected failures of F3 and F2bc on beta are
  themselves the record the docs need (the current DESIGN.md sentence
  attributes the shortfall to percentile intervals near zero; the study
  should be able to say "and bias-corrected percentile intervals do not fix
  it, because the bootstrap does not see the bias").

The honest expectation: no family will pass beta_0 at the boundary truth at
N <= 1000 inside [.885, .995], because the gap is an estimator bias, and the
verdict should be designed so that this outcome is informative (Q7, the
ladder).

### 3. Scoring rule

**Conditioning on a defined SE.** Yes, it can make B2 or F1 pass on intervals
most users would not get. The control's B2 figures at N = 250 rest on 183 of
460 fits (boundary) and 111 of 406 (interior); at N = 500 on 272 of 488 and
267 of 448. The excluded fits are the Heywood ones (D1: SE undefined on 76%
of Heywood fits, 0% of the rest), which carry the largest bias (B1 beta_0
coverage .55 on Heywood fits versus .88 on the rest). A conditional pass
therefore describes the method on the half of fits that did not need
rescuing. The verdict should:

- report, beside every B2 and F1 figure, the undefined share at that cell
  (the code records `n_na`; the verdict table does not use it);
- pre-register a conditioning limit: a (method, path, family) passes
  *unconditionally* at a cell only if its undefined share there is at most
  5% (choose the number now); above it the pass is labeled "conditional on a
  defined standard error, x% undefined", and the D-entry says a user whose
  fit prints the Heywood or ill-conditioning marker is in the excluded set;
- report B1 (and every bootstrap family) on the SE-defined subset as well, so
  the like-for-like comparison is in the record and nobody re-derives "Wald
  beats the bootstrap on the same fits" from it;
- note that Heywood fits *with* a defined SE (24% of Heywood fits at N =
  500) get Wald zeta intervals that cover at about .8 in the pilot; the
  Heywood split in the per-replicate record covers this.

**Removed weight scored as [0, 0], a miss below.** Right for a user-facing
verdict (the user sees a degenerate interval), and small: <= 4.1% of fits,
one cell, one weight. Report the removed count beside the figure (the code
has `removed_rate`) and, since 4 points can decide a band, also the coverage
conditional on the weight being kept, labeled descriptive.

**Heywood kept.** Right. Add the split by Heywood flag to the record; it is
the one figure a user with a flagged fit can use, because `summary()` prints
that marker.

**Angle side by arc midpoint.** Fine; the checks exercise the straddling and
over-full cases. Two gaps: a Wald arc of 360 or more counts as covering (a
vacuous interval), and nothing records width. Record the median width and
the share of vacuous intervals per component (angle arc > 180 degrees, zeta
interval wider than .5, beta interval wider than .5) and pre-register that a
component with more than, say, 5% vacuous intervals cannot pass regardless
of coverage. F1's zeta arm on Heywood fits and B2's angle arm at the boundary
truth are the cases this catches.

**Exclusion of non-accepted fits.** 94 of 500 at interior N = 250 (19%), 40
at boundary N = 250 (8%). This is the convention, but it is a large
conditioning event at N = 250 and the D-entry should carry it next to the
verdict.

### 4. Pass rule

The arithmetic: at n = 500 the SE of a coverage estimate is .0114 at .93,
.0128 at .91, .0098 at .95. The allowance .015 is 1.3 to 1.5 SE.

False fail (a method that truly covers everywhere): per component-cell,
P(estimate < .885) is about 0 at true .95, 5e-5 at .93, .026 at .91, .13 at
.90. A method at .93 or better on every component passes with near
certainty, since 8 x 6 = 48 zeta component-cells at .93 give under 1% chance
of any false fail. A method sitting at .91 everywhere fails with probability
.72 over 48 tests, and one at .90 almost surely. That is the right direction for a
shipping decision, and it should be stated: the rule is calibrated to pass
methods comfortably inside the band, not methods on its edge.

False pass: per component-cell, P(estimate >= .885) is .16 at true .87, .37
at .88, .05 at .86. A method bad everywhere passes with probability ~0 (the
all-cells conjunction); the realistic false pass is a single component at
.87 to .88 at one cell slipping through at 16 to 37%. The upper edge (.995)
binds only for near-vacuous intervals (B2 beta_0 reached 1.00 on 31 fits at
interior N = 500 in the pilot) and is doing useful work.

Two changes, both cheap:

- The allowance should scale with the realized n, which the conditional
  families and the N = 250 exclusions reduce to 111 to 272. At n = 111 the
  SE at .93 is .024 and the fixed .015 is 0.6 SE, so a conditional pass at
  N = 250 is close to a coin flip around the edge. Pre-register either
  allowance = 1.4 * sqrt(.93 * .07 / n) (which equals .016 at n = 500, so
  nothing changes for the unconditional families) or "a component-cell with
  n < 400 defined intervals is not evaluable" and report it as such.
- Keep 500 replicates. The dominant error here is systematic (a 1-SD bias),
  not Monte Carlo; doubling to 1000 halves an SE that is already below the
  allowance and doubles a run that will grow with the added families.

No multiplicity correction is right under the conjunction rule, and the
Wilson intervals beside each figure are the honest companion. Say in the
D-entry that the rule is one-sided in spirit: it protects against shipping a
method near the edge, at the cost of failing methods at .90 to .91.

### 5. Reusing the M4 seeds

Reuse does not bias the candidate verdict. The seeds were fixed in 2026-07
for a different purpose; no candidate family was built or tuned on fits from
them; the families are deterministic functions of the same replicate data
and of RNG draws that come after the data draw. What the control did shape
is this review, and through it possibly the pre-registration block: the two
families this review recommends adding were piloted on fresh seeds (77000
series), not on the M4 seeds, and are recommended on the mechanism, not on
their performance at the M4 cells. The benefits of keeping the M4 seeds are
real: AC2 depends on them, and every candidate is scored on the same
replicates as B1 and B2, so family differences are paired and carry a
smaller Monte Carlo error than the coverages themselves.

Recommended instead of switching seeds: for any (family, path, parameter
family) that passes at the raw-data cells, run a fresh-seed confirmation at
the cells it passes (one family, its passing cells, 500 replicates), and
record both figures. That meets "no family passes on replicates its
designers had seen" at a fraction of the cost of fresh seeds everywhere, and
leaves AC2 intact. The D-entry should list the families added after the
control run (this report's recommendations) with the date, which AC3 already
requires.

### 6. The correlation-matrix cells and what an F2 pass licenses

The Wishart draw is exact for the sample correlation of N multivariate-normal
rows, and F2 draws from the same family at Phat, so F2 is correctly specified
under the study's data-generating process. A pass licenses: "for
multivariate-normal data at p = 8 octant scales, equal zeta = .75, m = 3 and
these two beta truths, the parametric bootstrap from the fitted Phat covers
every component inside the band from N = N*". It does not license any
statement about non-normal data, unequal communalities, non-octant angles,
other p or m, or the free-scaling family (D-010 places it out).

The verdict must say three things about non-normal data: (i) the cormat path
cannot see it, because a user who supplies a correlation matrix supplies no
information about the distribution that produced it, so no interval method on
that path can be robust to kurtosis in principle, and the help page should
say that the cormat-path intervals assume normality; (ii) the raw-data cells
are also normal (`cpm_simulate()` draws normal factors and uniquenesses), so
the study never exercises the nonparametric bootstrap's one advantage over
F2, and a B1 pass does not license a non-normality claim either; (iii) under
elliptical non-normality the variance of every correlation scales by a
kurtosis factor, so the direction of the bias mechanism in Q1 is unchanged
but its size is not, and the N* found here is a normal-data N*.

Consider one descriptive sensitivity cell outside the verdict: boundary truth,
N = 1000, 200 replicates, rows drawn from a multivariate t with 5 df (or a
skewed transform of the normal draw) scaled to the same Phat, scored for B1,
F2p and P1. It costs about 1/12 of the run and gives the help page one
measured sentence about non-normal data instead of none.

### 7. Other ways the verdict could be an artifact of the design

1. **The cor-path pass set includes N = 250 to 1000.** `PATHS$cor` lists all
   six N, so F2p, F2bc, B2 and F1 on the cor path are judged at N = 250 with
   78 to 91% Heywood fits. Given Q1, every family will fail beta_0 there and
   the binary verdict will read "none passed" on every path, which is true
   and useless for the ship step, whose caution is already N-gated. Promote
   the ladder to the verdict: pre-register "a (method, path, family) passes
   from N* when every component passes at every listed N >= N*", report N*
   or "never", and keep the all-cells pass as the strict verdict beside it.
   AC3's wording ("whether every component's coverage lies inside the band
   at every cell") can stay if the D-entry reports both.
2. **The fixed allowance at variable n** (Q4).
3. **Vacuous intervals count as coverage** (Q3): an F1 zeta interval of
   [0, 1] or a Wald angle arc of 300 degrees passes the band.
4. **The ln v finiteness claim** (Q2): fix the wording; the code already
   scores it as NA.
5. **One truth family.** Equal communalities on an equally spaced grid with
   free angles are the configuration that makes the angle/communality mode
   weak (Q1). The verdict is a statement about this configuration; the
   D-entry must say so, and the help page should not generalize it to
   unequal communalities or non-octant instruments. A single extra cell
   (heterogeneous zeta .60 to .90 on the boundary beta, N = 500, 200
   replicates, descriptive) would show whether the Heywood rate and the beta_0
   bias are specific to equal zeta; consider it if budget allows after the
   additions above.
6. **Acceptance exclusions of 8 to 19% at N = 250** are a conditioning event
   on every method; report beside the verdict.
7. **The P1 family's chi-square calibration at the wall** is approximate;
   pre-register it as the 3.84 rule and say so, rather than tuning the
   cutoff after seeing results.
8. Minor, no action: the raw-data seed arithmetic (1e6 truth + 1e3 N index +
   i with i <= 500) cannot collide; B1's resample draw is the first RNG use
   after the data, so the M4 replicates are reproduced exactly (the control
   shows this); the BC z0 clamp at 1/(2B) bounds |z0| at 3.3 for B = 1000.

## Beyond the brief

- **The DESIGN.md Heywood sentence needs correcting.** It attributes the
  Heywood rate to "the weakly identified alternating/Nyquist mode of equally
  spaced grids interacting with beta_0". The alternating communality mode is
  the weakest direction at the interior truth, but at the boundary truth the
  weakest directions are angle-stretch modes with communality compensation,
  and fixing the angles removes the Heywood rate entirely there (61% to 0%)
  and most of it at the interior truth (68% to 14%). The interaction is with
  the free angles, and beta_0 is downstream.
- **The `summary()` caution thresholds were calibrated on conditional Wald
  coverage.** M4's analytic figures were taken on fits with a defined SE
  (its `ana_*` fields are NA otherwise), so the N = 2000 and 50000
  thresholds describe the Wald interval on the subset where it exists. That
  does not change the thresholds, since the caution fires on the markers that
  select the excluded fits, but the help page sentence should not imply the
  analytic coverage was measured on all fits.
- **Variant B and C intervals may be the practical answer for users who fix
  angles.** Under variants B and C the estimator bias and the Heywood rate
  are near zero at N = 500 (Q1c), so the shipped intervals probably already
  cover there. Out of this study's scope, but worth one line in the candidate
  row: the boundary-regime problem is a variant-A problem.
- **A bias-corrected point estimate is the other half of the story.** The
  bootstrap sees 40% of the beta_0 bias and the profile sees the ridge, but
  the estimator stays 1 SD low at N = 1000. If a later milestone wants beta_0
  intervals that pass at N <= 1000, it will need either a bias-reduced
  estimator (e.g. the variant-B fit as a reference, or a penalized
  communality) or a wider interval; no re-centering of the present estimator's
  replicates gets there. The study should record this expectation now so the
  verdict is read as "as designed" rather than as a surprise.

## Recommendations

Each marked apply / consider / reject-with-reason. "Apply" items go into the
pre-registration block before the first full run, with a work-log line each.

1. **Apply.** Store per-replicate records in the results file: point
   estimates, Heywood flag and count of zeta_i > .995, SE-defined flag,
   removed-harmonic flags, every method's interval endpoints, each
   bootstrap's replicate mean and SD per component, and the BC z0. Derive
   from them, per cell and component: realized estimator bias, bootstrap
   bias estimate, replicate Heywood share, coverage split by Heywood flag,
   every method's coverage on the SE-defined subset, median width and
   vacuous-interval share. Cost: storage only.
2. **Apply.** Add a variant-B refit (angles fixed at theory) of every raw-data
   replicate as a diagnostic arm, recording beta-hat, zeta-hat and the
   Heywood flag. About 0.3 s per fit. Pre-register the reading: if the
   variant-B beta_0 bias is under 0.25 SD and its Heywood rate under 5%, the
   mechanism is the angle/communality mode.
3. **Apply.** Add the basic bootstrap on the B1 and the F2 replicates (zero
   cost), scored for zeta and beta on the same paths as F3 and F2bc.
4. **Apply.** Add a profile-likelihood family P1 for zeta and beta on both
   paths, with the construction and the 3.84 rule stated in the block and its
   wall-calibration caveat recorded; handle polished fits with the reduced
   spec. 3 to 4 CPU-hours. Expect zeta to pass unconditionally from N = 500
   and beta_0 not to pass at N <= 1000.
5. **Consider.** Angles in P1 (fix theta_i, root-find the same statistic),
   another 2 to 3 CPU-hours; it is the only candidate that addresses the
   Wald angle shortfall at the boundary truth, which M4 found persists to
   N = 20000.
6. **Apply.** Promote "passes from N*" to the verdict object beside the
   all-cells pass (Q7.1), pre-registered now.
7. **Apply.** Conditioning rule for B2 and F1: report the undefined share
   beside every figure; a pass is unconditional only when that share is at
   most 5% at the cell, else labeled conditional; report every bootstrap
   family on the SE-defined subset too.
8. **Apply.** Allowance scaled to the realized n (1.4 SE at .93), or
   "n < 400 defined intervals: not evaluable"; pre-register which.
9. **Apply.** Record width and vacuous-interval share per component (angle
   arc > 180 degrees; zeta or beta width > .5) and pre-register that a
   component with more than 5% vacuous intervals cannot pass.
10. **Apply.** Fix the ln v wording: zeta-hat reaches exactly 1 in double
    precision in 2 to 3% of fits at N = 500; F1's interval is then NA and
    counted as undefined.
11. **Apply.** Write the expected outcome into the milestone file before the
    run: the beta_0 shortfall is an estimator bias of about one SD at N <=
    1000 that re-centering cannot remove, so a verdict of "no family passes
    beta on the raw path" is the designed reading, and the ladder (6) is
    where the ship step reads N*.
12. **Consider.** A fresh-seed confirmation run of any passing (family, path,
    parameter family) at its passing raw-data cells, recorded beside the M4-seed
    figure (Q5).
13. **Consider.** One descriptive non-normal cell (multivariate t, 5 df,
    boundary N = 1000, 200 replicates) for B1, F2p and P1 (Q6).
14. **Consider.** One descriptive heterogeneous-zeta cell (Q7.5).
15. **Consider.** Correct the DESIGN.md Heywood mechanism sentence in T5 with
    the variant-B figures from (2).
16. **Reject: fresh seeds at all raw-data cells.** The M4 seeds were fixed
    before any candidate existed, AC2 depends on them, and paired scoring on
    identical replicates is a benefit; (12) covers the residual concern.
17. **Reject: 1000 replicates.** The error that matters is systematic; the
    pass rule already passes any method at .93 or better with near
    certainty at 500.
18. **Reject: studentized bootstrap.** The per-replicate SE is undefined on
    60 to 93% of replicates at N <= 1000.
19. **Reject: Wald on the softmax scale.** A reparameterization of B2 that
    does not map back to a single weight, against a bias problem.
20. **Reject: dropping F3 or F2bc.** They are free and their expected failure
    is the sentence the docs need.
21. **Reject: reading D-002 as reaching CPM zeta or beta.** Its ground is
    circularity; these parameters are linear. The reason BC does not help
    here is the one in Q2, and the D-entry should give that reason, not
    D-002.
