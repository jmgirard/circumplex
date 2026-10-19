# RR25: Small-sample draw rules for growth-trajectory intervals (M161)

- **Date:** 2026-10-18
- **Brief:** `cairn/reviews/RB25-small-n-trajectory-draws.md`
- **Status:** advisory (binding criteria not requested)
- **Reviewer:** Fable (independent; read only the brief's materials)

Checks were run under `devtools::load_all(".")` with glmmTMB 1.1.15.2. The
scripts live in the session scratchpad and are not part of the repository;
every number below states how it was obtained so M161's T2 can reproduce
it. Nothing here ran the full oracle.

## Summary

The premise is real but smaller than the candidate row assumed, and the
brief's rough calculation is wrong in a way that matters for the design.
On complete balanced data the REML fit is exactly the ANOVA fit, and at the
centre of the design (wave 2, the mean of `times`) the pivot for `e`, `x`,
`y` and every linear combination of them is exactly `t` with `N - 1`
degrees of freedom, whatever the truth. The shipped normal draws therefore
cover `.918` at `N = 10`, `.935` at `N = 20`, `.940` at `N = 30` and `.944`
at `N = 50` at that wave, and a little more at the outer waves. The
multivariate `t` rule with a shared divisor and `nu = N - 1` is exact there
and conservative elsewhere, by at most `.015` at `N = 10`. AC2's gate as
written cannot see any of this: the shipped draws sit `.006` to `.010`
above the band's lower edge at `N = 20`, which is one to two Monte Carlo
standard errors, so whether they pass is a coin flip, and a `vcov`
inflation of `N / (N - 1)`, which undercorrects, passes the band and is
narrower than the `t` rule, so AC5 as written would ship it. Separately,
at `N = 10` the glmmTMB fit lands on the boundary of the random-effect
covariance (non-positive-definite Hessian) in 14% to 34% of replicates, so
the `N = 10` cells are uninterpretable under the rule that a failed fit is
a miss. The selection should rest on the closed-form result and on
calibration against `.95`, with the oracle confirming it, rather than on
the pass/fail band.

## 1. Premise

### The pivot on complete balanced data

Per `dv`, the model is `y_ij = b0 + b1 t_j + u_i + eps_ij`, `T = 5` waves
at `t = 0..4`, `tbar = 2`, `Sxx = sum (t - tbar)^2 = 10`. Under balance the
column space of the design is invariant under the marginal covariance for
any variance components, so GLS equals OLS for every `theta`, jointly
across the three `dv`s as well. The fitted value at wave `t` is
`f(t) = ybar + b1 (t - tbar)`, where `ybar` is the grand mean and `b1` the
within-person slope. Its variance is

```
Var f(t) = (s_u^2 + s_e^2 / T) / N  +  s_e^2 (t - tbar)^2 / (N Sxx)
         =        A'              +           B'(t)
```

which equals the brief's `s_u^2 / N + s_e^2 w(t) / N` as a total, but
splits differently. The between-person mean square `MSB` (person means,
`N - 1` df) estimates `s_u^2 + s_e^2 / T`, not `s_u^2`; the within mean
square `MSW` (`N(T - 1) - 1` df, one slope per `dv`) estimates `s_e^2`.
Under balance REML equals these ANOVA estimators, the REML `vcov` gives
`Var-hat f(t) = MSB / N + MSW (t - tbar)^2 / (N Sxx)`, and `ybar`, `b1`,
`MSB`, `MSW` are mutually independent. So the pivot
`(f-hat(t) - f(t)) / sqrt(Var-hat f(t))` is a Behrens-Fisher statistic
whose two pieces have `N - 1` and `4N - 1` df. At `t = tbar` the second
piece vanishes and the pivot is exactly `t_{N-1}`. This holds for every
linear combination of `e`, `x`, `y` at `tbar` (the between part is one
Wishart, so `c' MSB c` is `chi^2_{N-1}`-scaled for every `c`), hence for
the tangential component that drives the `d(t)` interval.

Verified on a `pole` replicate at `N = 20`: REML fixed effects equal
per-`dv` OLS to `2e-16`; REML residual variances equal `MSW` to `1e-7`;
REML `Sigma_u` equals `MSB - diag(MSW) / T` to five decimals; the `vcov`
fitted-value variance equals `MSB / N + MSW (t - tbar)^2 / (N Sxx)` to
`1e-7` at every wave; `-v01 / v11 = 2 = tbar` exactly for each `dv`, and
`Cov(f(tbar), b1) = 0` in `vcov`.

### Corrected Satterthwaite df and coverage

Satterthwaite df `(A' + B')^2 / (A'^2 / (N - 1) + B'^2 / (4N - 1))` and
the coverage of the normal 95% interval (exact, by 2e6 draws of the
Behrens-Fisher pivot; the Satterthwaite approximation agrees to `.001`):

| truth, dv | N | wave 0/4 df | wave 1/3 df | wave 2 df | cov 0/4 | cov 1/3 | cov 2 | mean |
|---|---|---|---|---|---|---|---|---|
| pole x,y; e in both | 10 | 18.6 | 11.2 | 9 | .936 | .926 | .918 | .928 |
| | 20 | 39.3 | 23.7 | 19 | .943 | .938 | .935 | .940 |
| | 30 | 59.9 | 36.2 | 29 | .945 | .942 | .940 | .943 |
| | 50 | 101.1 | 61.1 | 49 | .947 | .945 | .944 | .946 |
| xycor x,y | 10 | 10.3 | 9.3 | 9 | .923 | .920 | .918 | .921 |
| | 20 | 21.8 | 19.7 | 19 | .937 | .936 | .935 | .936 |
| | 30 | 33.2 | 30.0 | 29 | .942 | .941 | .940 | .941 |
| | 50 | 56.1 | 50.7 | 49 | .945 | .945 | .944 | .945 |

The brief's figures (59 and 32 at `pole`, `N = 20`; 23 and 20 at `xycor`)
come from assigning `s_u^2` alone to the `N - 1` piece. The correct wave-0
df are 39 and 22 and the wave-2 df are 19 in both truths. The brief's
marginal coverage range (`.936` to `.945` at `N = 20`) becomes `.935` to
`.943`, so the conclusion it drew survives: the shipped draws pass the
band at `N = 20` to `50`, and fail it only at `N = 10`, by `.002` (`pole`)
and `.009` (`xycor`) on the mean over waves. `e` has the `pole` variance
components in both truths. The `d(t)` interval follows the tangential
linear combination to first order, so its figures are the `x, y` rows,
less whatever the circular-quantile summary loses (M27 measured `d`
coverage `.932` to `.944` at `N = 200` in `xycor`, where the linear theory
says `.949`; 500 replicates, so that is within noise but worth watching).

### REML bias

None under balance. The REML variance components are the unbiased ANOVA
estimators, except that REML truncates `Sigma_u` at the boundary of the
positive-definite cone. The undercoverage comes entirely from the sampling
variability of `MSB` (and, away from `tbar`, of `MSW`), which is the
`t`-versus-normal effect. Maximum likelihood would add a bias of order
`(N - 1) / N`; REML does not.

### Empirical check (250 replicates per cell, not the oracle)

Closed-form intervals on the REML fitted-value SE, and the package's own
draw-based `d` interval (`ssm_trajectory(draws = ...)`, 2000 draws, normal
draws and shared-divisor `t_{N-1}` draws from the same normals). Coverage
among converged positive-definite fits, wave 2, Monte Carlo SE about
`.015`:

| cell | fits used | `x` normal | `x` t | `d` normal | `d` t |
|---|---|---|---|---|---|
| pole N = 20 | 247/250 | .931 | .951 | .936 | .955 |
| xycor N = 20 | 250/250 | .948 | .952 | .936 | .952 |
| pole N = 10 | 112/150 | .923 (e,x,y pooled) | .946 | — | — |
| xycor N = 10 | 129/150 | .904 (e,x,y pooled) | .943 | — | — |

All consistent with the closed form (`.935` at `N = 20`, `.918` at
`N = 10` for the normal; `.95` for `t`). Over all replicates with failed
fits counted as misses the `N = 10` figures are `.59` to `.75` for every
rule, see question 8.

### Answer

The undercoverage is real at every `N` in the grid and is largest at the
centre wave: `.918` at `N = 10`, `.935` at `N = 20`, `.940` at `N = 30`,
`.944` at `N = 50`, for `e`, `x`, `y` and (to first order) `d`. It is a
one- to three-point deficit in the gated range and three points at
`N = 10`. It is not large enough to fail AC2's band at `N >= 20`.

## 2. Gate power

No. At `N = 20` the shipped draws' expected mean-over-waves coverage is
`.936` (`xycor x, y`) to `.940` (`pole`; `e` everywhere), against a lower
edge of `.93`. The Monte Carlo SE of a mean over five waves at 1000
replicates lies between `.0069 / sqrt(5) = .003` and `.0069`; the waves
share `MSB`, and the fitted values at waves 0 and 2 correlate at
`sqrt(A' / (A' + B'))`, which is `.82` for `pole` and `.97` for `xycor`,
so about `.005` to `.006`. The margin is one to two SE per check, and there
are eight mean checks at `N = 20` (two truths, `e, x, y, d`). Treating them
as independent, the chance that the shipped rule passes all eight is about
`.5`; dependence raises it somewhat. The gate's verdict on the shipped rule
at `N = 20` is therefore noise, and at `N = 30` and `50` the shipped rule
passes almost surely. Meanwhile the `t_{N-1}` rule passes everywhere
(`.950` to `.958` means, maximum single-wave `.965` at `pole N = 10`
wave 0, inside the band). So the gate separates nothing in `{20, 30, 50}`.

What to change. Not the truths: the centre wave already has df `N - 1`
under every truth, and `xycor`'s `x, y` are already between-dominated
(df 19 to 22 at `N = 20`). Not the band: with 24 mean checks and SE
`.005`, a band narrower than about `.95 +/- .015` would fail a correct
rule by chance (see 7), and `+/- .015` still admits the shipped draws at
`N >= 30`. The power lives at `N = 10` and in the closed form. Three
changes, each in pre-registrable form:

1. Closed-form fixture (new, the discriminating element). "At wave 2, for
   each of `e, x, y`, in every complete-data cell, the coverage of each
   rule among converged positive-definite fits lies within `.02` of its
   closed-form value: `P(|t_{N-1}| < 1.960)` for the shipped draws
   (`.918, .935, .940, .944, .949` at `N = 10, 20, 30, 50, 200`); `.95` for
   the shared-divisor `t` rule with `nu = N - 1`;
   `P(|t_{N-1}| < 1.960 sqrt(k))` for a `vcov` inflation by `k`." This is
   a second oracle type (closed form beside simulation coverage) and it
   tests that the oracle machinery sees the effect the correction targets.
2. Selection by calibration, not by the band (see 6). "Among rules whose
   mean coverage over waves lies in `[.93, .97]` with no wave below `.91`
   in every complete-data cell with `N` in `{10, 20, 30, 50}`, coverage
   taken among converged positive-definite fits, the chosen rule minimises
   the maximum over those cells, waves and `e, x, y` of
   `|coverage - .95|`; median `d(t)` arc width breaks a tie only when two
   rules' maxima differ by less than `.007`." Under the closed form the
   maxima are `.032` (shipped), `.015` (`t_{N-1}`), `.019` (inflation
   `N / (N - 1)`), so the `t` rule wins with a margin the oracle can see
   only at `N = 10` and only among converged fits. That is why `N = 10`
   must enter the selection domain, and why the gate must not count
   boundary fits as misses (see 8).
3. The pair's goal. The rule is selected by derivation: exact at the
   design centre, conservative elsewhere, for this model on complete data.
   The oracle's job is to confirm the derivation (the fixture), measure the
   over-coverage at the outer waves and at `N = 10`, measure the
   missing-wave cells, and record the boundary-fit rate. M161's AC5 can
   keep its wording "chosen among the passing rules" with the calibration
   criterion in place of width.

## 3. Candidate rules

Notation: `L = mvn_root(vcov)`, `Z` a row of `p` standard normals,
`B = coef + Z L` the shipped draw (`mvn_draws()`), `S_nu = sqrt(chi^2_nu
/ nu)`.

**R0, shipped normal.** `B = coef + Z L`. No argument. The control.

**R1, multivariate `t`, shared divisor.** `B = coef + (Z L) / S_nu`, one
`S_nu` per draw shared by all `p` coefficients. Argument: `df = nu`, lower
bound `1` (a proper `t`; every summary the helper reports is a quantile or
a circular mean, so no finite moment is needed; the help text states the
validated range from the record). For the vignette's model on complete
data, `df = N - 1`. Every linear contrast `c'B` is `c'coef + sqrt(c' vcov
c) t_nu`, so M162's AC2 quantiles are `c'coef +/- t_{nu, q} sqrt(c' vcov
c)` in closed form, for `x(t) + y(t)` as well. Assumes one df for every
contrast: exact at `tbar` under balance, conservative away from it (the
slope part has `4N - 1` df). Gate it; it is the rule I expect to ship.

**R2, independent `t` marginals.** `B_j = coef_j + (Z L)_j / S_{nu, j}`
with an independent divisor per coefficient. Do not gate. Linear
combinations of its draws have no closed form (M162 AC2 cannot be written
for it), it cannot reproduce the exact `t_{N-1}` pivot for any contrast,
and it attenuates the implied correlation between coefficients by
`E[1/S]^2 / E[1/S^2]`, which is `.931` at `nu = 9`, `.971` at `19`, `.982`
at `29`, `.989` at `49`. In `xycor` the tangential direction at 135
degrees rides the correlated `(1, 1)` axis, so attenuation narrows the
`d(t)` interval: the univariate-shortcut failure M27's A3 cell was built to
catch, in miniature. Its one role would be as a plausible-but-wrong
control; M162's `x + y` test already excludes it, so I would not spend an
arm on it.

**R3, `vcov` inflation.** `B = coef + sqrt(k) Z L`.
(a) `k = N / (N - 1)` (or `N / (N - p)`): no derivation ties it to this
model. Centre-wave coverage `.931` (`N = 10`), `.941` (`20`), `.944`
(`30`), `.947` (`50`): it undercorrects, passes the band at `N >= 20`, and
is narrower than R1 (sd factor `1.026` against `1.068` at `N = 20`). Gate
it only as the control that shows AC5's width criterion would ship it;
reject it as a candidate to ship.
(b) `k = (t_{nu, .975} / 1.960)^2`: gives R1's 95% interval exactly for
every linear contrast, with normal rather than `t` shape. It differs from
R1 only through the nonlinear `a` and `d` summaries and at any
`interval` other than `.95`, where it is wrong by construction. Reject as
a shipping rule (level-specific); as an oracle arm it isolates the
tail-shape effect on `d` and certification if that is wanted.
(c) `k = nu / (nu - 2)` (match the `t` variance): sd factor `1.134` at
`nu = 9` against the quantile ratio `1.154`; undercorrects at small `nu`,
and needs `nu > 2`. Reject.

**R4, design-based df from `N` and `times`.** Under balance and a linear
trend, `vcov` itself carries the between/within split: per `dv`,
`tbar = -v01 / v11`, the between variance at `tbar` is `v00 - v01^2 /
v11`, and the within part at `t` is `v11 (t - tbar)^2`. Two forms follow.
(a) Two-part draw: scale the `f(tbar)` block of the draw by `1 / S_{N-1}`
and the slope block by `1 / S_{N(T-1)-1}` (or leave it normal). This
reproduces the Behrens-Fisher structure of the true pivot, so it is the
most faithful rule. (b) Per-time single divisor with Satterthwaite
`nu(t) = min over dv of (A' + B'(t))^2 / (A'^2 / (N - 1) + B'(t)^2 /
(N(T - 1) - 1))`. Both need `T` (the number of design waves) as well as
`N`, so neither is "one numeric argument"; (a) has no closed-form
quantile for M162's AC2 (Behrens-Fisher); both assume the balanced
structure of `vcov` (with missing waves the recovered `tbar` differs by
`dv` and `v00 - v01^2 / v11` can lose its meaning); and the gain over R1
is at most the ratio `t_{19, .975} / t_{39, .975} = 2.093 / 2.023`, 3.4%
of width at the outer waves at `N = 20`, below what 1000 replicates
resolve. Do not gate as shipping candidates. (b) is worth one reference
arm if the record wants to show how much R1's conservatism costs.

**R5, `nu = N - 2` or `N - 3`.** "One df per fixed effect" or the
Jeffreys-posterior df for a 3-vector mean. Both over-correct: `t_7` at
`N = 10` covers `.967` where `t_9` is exact. Under balance the slope is
estimated from the within part and costs no between df, and the
Jeffreys posterior's marginals are known to be wider than the exact
per-contrast pivot. Reject; no derivation supports them here.

**A rule computable from `coef` and `vcov` alone.** None. `vcov` is
`Psi / N` plus the slope part with `Psi` unknown, so `N` is not
identifiable from it; the between/within split is (R4), the df are not.
The plan gate's opt-in choice stands.

**Argument form for M162.** `df`, not `n_persons`. The df are the
engine-agnostic primitive; a user with lmerTest or pbkrtest Satterthwaite
or Kenward-Roger df, or nlme's df, can pass them; and the mapping
`df = N - 1` is specific to this model on complete data with no
person-level covariate (one covariate makes it `N - 2`). The help page
states the mapping and its conditions. The refusal set in M162 AC3
(`NA`, below the bound, together with `draws`) fits `df` unchanged.

## 4. Shared versus separate divisor

With one divisor for the whole vector, `(x(t), y(t))` is bivariate `t_nu`
with scale `C(t) vcov C(t)'`: elliptical, correlation preserved, every
direction's marginal `t_nu`. The tangential component is `t_nu`, so the
`d(t)` half-width scales by `t_{nu, .975} / 1.960` to first order (`1.068`
at `nu = 19`, `1.154` at `nu = 9`), the `a(t)` interval widens the same
way, and the certification ratio `a_lci / (a_uci - a_lci)` falls, so
fewer times certify. At `a = .6` and `N = 10` the ratio is still about
`.65` against the `.35` threshold, so certification survives in these
truths (observed `.80` to `1.00` certified among converged fits at
`N = 10` and `20`, identical under both rules). That is the safe
direction.

With independent per-coefficient divisors the joint law is not
elliptical: linear combinations have lighter tails than `t_nu` (a sum of
independent `t`s is closer to normal) and the correlation is attenuated
by the factors in 3 (R2). Both effects narrow the tangential interval.

Which is right. The true structure splits by between and within, not by
coefficient: the between part (`f(tbar)` for all three `dv`s) shares one
Wishart with `N - 1` df, so a single divisor across `e, x, y` is right
there; the within part (the three slopes) has `4N - 1` df each and is
independent across `dv`s because the residual is diagonal. R1 applies the
between df to the within part too, which is conservative; R4(a) is the
exact split. Per-coefficient divisors match neither part.

## 5. Missing waves

Yes in form, no in magnitude. `vcov.glmmTMB` under REML takes
`sdreport(getJointPrecision = TRUE)`, forms the marginal precision of the
non-random parameters (`beta`, `theta`, `betadisp`) by `GMRFmarginal`,
inverts it, and returns the `beta` block. That block is the Schur
complement `(H_bb - H_bt H_tt^{-1} H_tb)^{-1}`, which to first order is
the conditional `(X' V-hat^{-1} X)^{-1}` plus the Kackar-Harville term
for the variability of `beta-hat(theta)`. On complete balanced data the
cross-derivative `H_bt = X' (dV^{-1}/dtheta) r` is identically zero
because `V^{-1} X` lies in the column space of `X` for every `theta`, so
`vcov(fit)$cond` is exactly the conditional matrix; measured equality at
every wave to `1e-7`. With 20% of person-waves deleted at `N = 20`, the
fitted-value SE from `vcov(fit)$cond` exceeds the conditional SE (computed
by hand from the REML `VarCorr` and the observed design) by a factor of
`1.0001` to `1.0042` across waves and `dv`s on two replicates; nlme's
`vcov` equals the conditional one to `3e-4`, with fixed effects agreeing
to `1e-5`. So the term is present only on the missing-wave cells, is
worth under half a percent of SE there, and does not cover the
`t`-versus-normal effect, which is about the sampling variability of
`V-hat`, not of `beta-hat(theta)`. The missing-wave cells should be read
as the complete-data cells are, with a between df still about `N - 1`
(persons) and a within df of about `0.8 N (T - 1) - 1`, so R1 with
`df = N - 1` remains a sound lower bound there. The record should say
the oracle validated glmmTMB's REML `vcov`; nlme's is the same on complete
data and `0.4%` narrower on the missing cells.

One of three unbalanced `N = 20` fits in that check failed (non-PD
Hessian). The boundary rate on the missing-wave cells may be material;
AC3 reports it, and the reading in 8 applies.

## 6. Selection rule

Not sound. "Narrowest width among passers" is efficiency subject to
validity, which is right only when the validity test has power. Here the
band's lower edge (`.93`) sits below the shipped draws at `N >= 20`
(`.936` to `.946`), so every mild correction passes, and the narrowest
mild correction wins. Concretely, inflation by `N / (N - 1)` has
centre-wave coverage `.941` at `N = 20` and `.931` at `N = 10`, passes
the band at every gated `N`, and is narrower than `t_{N-1}` by
`1.068 / 1.026`; AC5 as written ships it, and it undercorrects at exactly
the samples the argument is for.

Better: calibration first. Over the selection domain (complete-data
cells, `N` in `{10, 20, 30, 50}`, all waves, `e, x, y`, coverage among
converged positive-definite fits), minimise the maximum of
`|coverage - .95|`; break ties within one Monte Carlo SE (`.007`) by the
pooled median `d(t)` arc width. `d` is reported, not used for
selection, because its summary carries its own small deficit (1). Under
the closed form this picks R1 at `.015` over inflation at `.019` and the
shipped draws at `.032`; the margins are visible only at `N = 10`, and
only if boundary fits are excluded from the coverage denominator. Add
the derivation as the stated reason for the choice: the record should
say R1 is exact at the design centre, since a tie within noise should
not be broken by width when one rule has a proof and the other does not.

## 7. Gate statistics

Per-wave coverage at 1000 replicates has SE `.0069` at `.95`. The floor
`.91` is `5.8` SE below `.95`: a rule at `.95` never trips it (about
`3e-9` per check, `4e-7` over 120 checks), and a rule at a true `.93`
trips it with probability `.006`. It catches only rules below about
`.925` and is a sanity floor, not a discriminator; it is right as that.
The band on means has SE about `.005` to `.006` (2), so `+/- .02` is
`3.3` to `4` SE: a rule at `.95` fails one mean check with probability
about `5e-4` and the gate with about `1%` over 24 checks. The gate's
type-I error is small. Its power against the shipped draws is the
problem: they fail a given `N = 20` mean check with probability `.04` to
`.14`, so the gate's verdict on them is a coin flip (2), and at `N >= 30`
they pass. Two further points. Rules share replicates, so the paired
difference in coverage between two rules has SE `sqrt(p_discordant /
1000)`, about `.004` when the rules disagree on 1.5% of replicates; the
record should report paired differences, not only marginal coverages.
And the mean over `e, x, y` is not three independent checks: they share
`MSB`'s Wishart and correlate through `rho_u`.

## 8. Other design risks

**Boundary fits at `N = 10` make those cells uninterpretable under
AC2's convention.** With `us(0 + dv | person)` the REML `Sigma_u` is a 3
by 3 unstructured matrix estimated from 10 persons. Its smallest true
eigenvalue is `.027` (`pole`) and `.012` (`xycor`) against `D / T` of
`.0125` and `.0045`, so the sample estimate `MSB - D-hat / T` is often not
positive definite, REML stops at the boundary, and glmmTMB reports a
non-positive-definite Hessian. Measured: 38 of 150 (`pole`) and 21 of 150
(`xycor`) at `N = 10`, every one with `min eigenvalue(Sigma_u-hat)` below
`1e-8`; a second run of 250 gave 86 and 49. At `N = 20`: 3 of 250 and 0
of 250. With failures counted as misses, `N = 10` coverage over all
replicates is `.59` to `.75` for every rule, which says nothing about any
rule. Their `vcov` is finite and usable; the centre-wave coverage among
them was lower (`.87` normal, `.90` `t`, 38 fits) in `pole` and higher in
`xycor` (21 fits), too few to read. Three consequences. (i) The gate and
the selection should use coverage among converged positive-definite fits
and pre-register a boundary-rate ceiling for gated cells (for example
`5%`), with the all-replicates figure reported beside as AC3 already
asks. (ii) The vignette and M162's help text should say that at about ten
persons the pasted glmmTMB call reaches the boundary of the random-effect
covariance in a sizeable fraction of samples, and that no draw rule
repairs that fit. (iii) `vcov.glmmTMB` can return an all-`NA` matrix
(when the marginal-precision solve fails) on a fit whose `pdHess` is
`TRUE`; my first run died on it inside `mvn_root()`. The oracle's
`fit_joint()` should treat a non-finite `vcov` as a failed fit and count
it separately.

**A plausible-but-wrong rule passing.** Any inflation between `1.026` and
the `t` ratio passes the band at `N >= 20` (3, 6). The calibration
criterion and the closed-form fixture close this.

**A correct rule failing.** Only through boundary fits counted as misses
(above), or through the `d` summary's own deficit if `d` stays in the
selection set. The `t` rule's over-coverage at the outer waves (`.965` at
`pole N = 10`, wave 0; `.957` at `N = 20`) is inside the band.

**Draw-count noise.** With 4000 draws the `.025` quantile has SE about
`.04` in `z` units; the induced coverage loss is about `.0002`. Ignore.

**Level consistency.** R1 is correct at every `interval`; the inflation
rules are correct at one. M162's tests run at `.95` only; one assertion at
another level would pin this.

**RNG discipline (M162 IP4).** Draw the normals first, exactly as now,
then the `chi^2` divisor only when `df` is given, so the default path's
stream is unchanged and AC1's `identical()` holds.

## Beyond the brief

- The exactness at `tbar` gives M162 a derivation to cite in the help
  page and Section 7: "exact at the mean of the design times for the
  joint model on complete data; conservative at other times; `df = N - 1`
  with no person-level covariate." That is a stronger statement than a
  simulation band and is testable in M162's own unit tests (the
  `x(t) + y(t)` quantile at `t = tbar` against `t_{N-1}`).
- `vcov` under balance encodes `tbar` as `-v01 / v11` per `dv`. The
  helper could check that the three `dv`s agree and warn when they do
  not (an unbalanced fit), but that is a candidate row, not M162.
- `devel/m27-coverage-oracle.R`'s `fit_joint()` does not check `vcov`
  for finiteness; the M27 cells at `N = 200` never hit the case, but the
  M161 reuse will.
- The Section 7 text says a parametric bootstrap "is not" a correction on
  balanced data; the reason is right, and the same fact is what makes the
  `t_{N-1}` rule exact there, which Section 7 could say once M162 lands.

## Recommendations

1. **apply** — Correct the premise in M161's header and work log: between
   piece `(s_u^2 + s_e^2 / T) / N` with `N - 1` df, within piece
   `s_e^2 (t - tbar)^2 / (N Sxx)` with `N(T - 1) - 1` df; the centre-wave
   pivot is exactly `t_{N-1}`; shipped coverage `.918 / .935 / .940 /
   .944` at `N = 10 / 20 / 30 / 50`.
2. **apply** — Add the closed-form fixture (2, item 1) to AC2 and AC3 as
   a second oracle type; it is the only check with power at `N >= 20`.
3. **apply** — Replace AC5's width criterion with the calibration
   criterion (6), domain `N` in `{10, 20, 30, 50}`, `e, x, y` only, `d`
   reported; width as a within-SE tie-break; the derivation named as the
   reason.
4. **apply** — Gate and select on coverage among converged
   positive-definite fits with a pre-registered boundary-rate ceiling per
   gated cell; keep the all-replicates figure as a reported column. Treat
   a non-finite `vcov` as a failed fit in `fit_joint()`.
5. **apply** — Candidate set: R0 (control), R1 shared-divisor `t` with
   `nu = N - 1`, R3(a) inflation `N / (N - 1)` as the plausible-but-wrong
   control. Do not gate R2, R3(c), R5. Optional reference arm: R4(b) or
   R3(b).
6. **apply** — M162's argument is `df` (lower bound `1`, validated range
   from the record), with the `df = N - 1` mapping and its conditions in
   the help text; draw the divisor after the normals (IP4).
7. **apply** — Record in the M161 record and M162's Section 7 that at
   `N = 10` the fit reaches the random-effect boundary in a sizeable
   fraction of samples, with the measured rate.
8. **consider** — Report paired coverage differences between rules
   (shared replicates), which have SE about `.004`.
9. **consider** — One M162 test at `interval = .90` or `.99` to pin level
   consistency.
10. **reject, with reason** — A new between-dominated truth: the centre
    wave has df `N - 1` under every truth, and `xycor` is already
    between-dominated at the outer waves.
11. **reject, with reason** — Tightening the band to create power: 24
    mean checks at SE `.005` cannot support a band inside `+/- .015`
    without failing correct rules by chance, and `+/- .015` still admits
    the shipped draws at `N >= 30`.
12. **reject, with reason** — Reopening the opt-in default: `N` is not
    identifiable from `coef` and `vcov`; no rule in 3 computes its df
    from them alone.
