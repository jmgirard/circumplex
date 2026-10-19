# M161 small-N trajectory coverage oracle: results (2026-10-18)

This record goes with `devel/m161-small-n-oracle.R`, the seeded regeneration
script. Its header holds the pre-registered gates. The committed results are in
`devel/m161-small-n-results.rds`. Every figure below comes from that file:
the `summary`, `ac3_i` and `ac3_ii` elements and `meta$verdicts`. The oracle
measures the coverage of the `ssm_trajectory()` intervals at 10 to 200
persons. The intervals come from a joint glmmTMB REML fit's `coef` and `vcov`,
under three draw rules. Each replicate fits once, and all three rules use that
fit and one shared matrix of normal draws. There are 1000 replicates in each
of the 14 cells and 4000 draws for each replicate. The model and the two
truths (`pole` and `xycor`) are those of `devel/m27-coverage-oracle.R`.

R0 is the shipped normal draws. R1 is multivariate t draws with one
chi-square divisor shared by all coefficients and nu = N - 1. R3a is normal
draws on `vcov` times N / (N - 1), a control that undercorrects. The script's
header gives their formulas. The derivation behind R1 is in
`cairn/reviews/archive/RR25-small-n-trajectory-draws.md`, section 1. On
complete balanced data, the pivot at the mean design time (wave 2) is exactly
t on N - 1 df. This holds for every linear combination of e, x and y.

## Verdicts

| Gate | Result |
|---|---|
| AC2 band: complete data, N in {20, 30, 50}, all replicates, non-PD fits count as misses | **Every rule fails.** R1 fails only in `pole` N = 20, with means .919 to .923 for e, x, y and d. Its other five cells pass. R0 fails in `pole` and `xycor` at N = 20, and R3a fails in `pole` at N = 20. |
| AC3 (i): wave 2, among converged fits, R0 and R1 within 3.5 SE of the closed form, N in {20, 30, 50, 200} | Pass, all 48 checks. The largest miss is 0.52 of the tolerance. |
| AC3 (ii): paired R1 minus R0 at wave 2, pooled over truths and e, x, y | Pass. Observed against target: .0148 and .0148 at N = 20, .0077 and .0097 at N = 30, .0068 and .0057 at N = 50. The tolerance is .0038 to .0056. |
| AC5: R0 mean d(t) coverage at N = 200 | Pass, .953 (`pole`) and .946 (`xycor`). |

No fit returned an error, and no fit returned a non-finite `vcov`.

## Coverage by cell

The table gives mean coverage over the five waves and over e, x, y and d.
"All" counts every replicate, with a non-PD fit as a miss. "Conv" counts
only converged fits that have a positive-definite Hessian. The `.rds` holds
the per-wave and per-outcome values and their Monte Carlo standard errors. It
also holds the certification rates, the a(t) coverage and the paired
differences for each pair of rules.

| Cell | ok / non-PD | R0 all | R1 all | R3a all | R0 conv | R1 conv | R3a conv |
|---|---|---|---|---|---|---|---|
| pole_n10 | 726 / 274 | 0.682 | 0.702 | 0.691 | 0.939 | 0.967 | 0.951 |
| pole_n20 | 968 / 32 | 0.908 | 0.921 | 0.914 | 0.938 | 0.952 | 0.944 |
| pole_n30 | 999 / 1 | 0.942 | 0.951 | 0.945 | 0.943 | 0.952 | 0.946 |
| pole_n50 | 1000 / 0 | 0.944 | 0.949 | 0.946 | 0.944 | 0.949 | 0.946 |
| pole_n200 | 1000 / 0 | 0.952 | 0.953 | 0.952 | 0.952 | 0.953 | 0.952 |
| xycor_n10 | 823 / 177 | 0.757 | 0.781 | 0.768 | 0.920 | 0.949 | 0.933 |
| xycor_n20 | 990 / 10 | 0.932 | 0.947 | 0.938 | 0.941 | 0.956 | 0.948 |
| xycor_n30 | 999 / 1 | 0.943 | 0.951 | 0.946 | 0.944 | 0.952 | 0.947 |
| xycor_n50 | 1000 / 0 | 0.941 | 0.946 | 0.943 | 0.941 | 0.946 | 0.943 |
| xycor_n200 | 1000 / 0 | 0.947 | 0.948 | 0.948 | 0.947 | 0.948 | 0.948 |
| pole_n20_miss | 915 / 85 | 0.868 | 0.881 | 0.873 | 0.949 | 0.963 | 0.954 |
| pole_n30_miss | 984 / 16 | 0.928 | 0.937 | 0.932 | 0.943 | 0.952 | 0.947 |
| xycor_n20_miss | 961 / 39 | 0.906 | 0.919 | 0.911 | 0.942 | 0.956 | 0.948 |
| xycor_n30_miss | 998 / 2 | 0.935 | 0.945 | 0.940 | 0.937 | 0.947 | 0.942 |

The N = 10 and missing-wave cells are reported without a gate.

## Reading

The draw rules behave as the derivation predicts. AC3 (i) checks the design
centre at every gated N. There, R0 sits at P(|t_{N-1}| < 1.96) and R1 at .95. AC3 (ii) shows that the oracle sees the gap between them. At N = 20 the
paired difference is .0148, with a standard error under .002. Among converged
fits, R1's mean coverage is .946 to .956 in every complete-data cell from
N = 20 to 200. At N = 10 it is .967 (`pole`) and .949 (`xycor`). It
overcovers at the outer waves, which RR25 section 1 predicts. R3a lies
between R0 and R1, as RR25 predicts.

The pre-registered band still fails R1, and the cause is the fit, not the
draw rule. At 20 persons, the REML estimate of the 3 by 3 random-effect
covariance stops on its boundary in 32 of 1000 `pole` replicates. glmmTMB
then reports a non-positive-definite Hessian. The band counts that replicate
as a miss under every rule. That costs about three points of coverage, which
is more than the band's lower margin. RR25 section 8 measured a 0% to 1.2%
boundary rate at N = 20 on 250 replicates. Its rec 4 asked for a gate among
converged fits with a ceiling on the boundary rate. Because of that
measurement, the ingest did not apply it. The full run measured 3.2%. Those
non-PD fits still return a finite `vcov`. Their intervals cover about as well
as the others. Over the 32 `pole` N = 20 fits, R1 covers .925 to .956 on e,
x, y and d. That count is too small to read closely. The band counts a non-PD fit as a
miss. So it measures the fit's boundary rate as well as the interval's
coverage.

At N = 10 the boundary rate is 27.4% (`pole`) and 17.7% (`xycor`). No draw
rule repairs those fits. Coverage over all replicates is .68 to .78 under
every rule. On that basis the 10-person cells say nothing about the draw
rules. In the missing-wave cells, the boundary rate at N = 20 is 8.5%
(`pole`) and 3.9% (`xycor`). At N = 30 it is 1.6% and 0.2%. Among converged
fits, R1 covers .947 to .963 there.

Certification held in every cell. Under each rule and at each wave, the
certification rate among converged fits was at least .995. The wider R1
intervals did not lower certification in these truths.

In `pole`, the median d(t) arc width under R1 exceeds R0's by about 7% at
N = 20 (22.5 against 21.1 degrees). At N = 10 it exceeds R0's by about 16%
(34.7 against 30.0).

## Design iterations

None. The script ran once at full size. Its header is unchanged from the
commit before the results: `git diff 2e9bd0e2` on the script is empty. A move
of the band to the converged-fit basis after the results is an unregistered
gate. That choice belongs to the plan that follows this record. It needs a
fresh pre-registration with new seeds.

## Oracle types

There are two types. The first is simulation coverage: the AC2 band, the
per-cell tables and the N = 200 control. The second is closed form. It gives
the AC3 targets from the exact t pivot at the design centre (RR25 section 1)
and the paired difference they imply.
