# RR21: mechanism for the a-posteriori per-fit accuracy certificate (M108)

- **Date:** 2026-08-24
- **Brief:** `cairn/reviews/RB21-axes-degeneracy-per-fit-certificate.md`
- **Reviewer:** Fable (independent review; advisory — binding criteria not
  requested)
- **Materials read:** `R/axes_corrected_se.R` (full), `R/axes_scaled_fit.R`
  (full), `devel/degeneracy-oracle/exact_oracle.{R,py}` (full),
  `tests/testthat/helper-m106-degeneracy.R`, DESIGN.md IP/GP blocks and the
  oracle-records row, D-044/D-048/D-049/D-050, RR19 (archived), RR20
  (archived), `cairn/milestones/M108-per-fit-certificate.md`.
- **Measurements performed for this review** (constructions stated so M108 can
  rebuild them without this session):
  (i) the committed exact-rational oracle re-run from the repo root — ANCHORS
  PASS, SWEEP PASS, REACHABLE PASS; per-case SE errors 5.9e-14…1.1e-11, cval
  errors 2.1e-14…1.1e-8, fixture B at 3.41e-2 SE / sign-flipped cval,
  reproducing every figure the brief quotes.
  (ii) a same-precision second-route pricing (every inverse via symmetric
  eigendecomposition, sums reassociated) compared against the shipped
  solve()-route on the five reachable cases, fixture B, and two
  beyond-the-floor near-duplicate cases, each against the exact oracle.
  (iii) a complete double-double (compensated-arithmetic) recomputation of the
  pricing pipeline — Knuth two-sum, Dekker-split two-product, normalized
  add/mul, three-step division, Newton square root, tree summation, pivoted
  Gauss–Jordan — run over the same cases and against the exact oracle, plus a
  cost measurement at p = 24, q = 27.
  All three used `tests/testthat/fixtures/rb18-counterexample-b.rds` and the
  helper-file family constructions at their committed parameters; N = 600.

---

## 1. Which mechanism should the certificate use?

**Recommendation: recompute the whole pricing pipeline once per fit in
compensated double-double arithmetic (~31 significant digits, base R, no
dependency), and read the certificate off the relative disagreement between
the shipped double-precision values and the recomputed ones, times a stated
safety factor.** This is candidate (a) — two routes compared — built with the
one route pair whose errors cannot correlate: the second route runs at
ε_dd ≈ 6e-32 against the working route's ε ≈ 2.2e-16, so the disagreement *is*
the double route's committed error rather than an uncorrelated-error proxy for
it. Measured on the five reachable cases, fixture B, and a case two decades
past the current floor, the disagreement equals the exact-rational oracle's
measured error to within 2 % — every case, both quantities (§3's table).

**(a) taken literally — two independent same-precision factorizations —
fails, measurably, in both directions.** I built the natural second route
(inverses via `eigen()`, accumulation reassociated) and priced all cases:

| case | SE err (true) | SE route-disagreement | ratio | cval err (true) | cval disagreement | ratio |
|---|---|---|---|---|---|---|
| A p=8 κ=1e4 | 5.89e-14 | 6.21e-10 | 1.1e4 | 2.10e-14 | 8.47e-13 | 40 |
| A p=8 κ=1e5 | 3.00e-12 | 1.00e-08 | 3.3e3 | 2.20e-13 | 7.21e-13 | 3.3 |
| C p=4 κ=1e5 | 6.46e-13 | 1.24e-08 | 1.9e4 | 1.12e-08 | 9.22e-09 | **0.82** |
| ND r=.9999 | 6.30e-13 | 1.44e-09 | 2.3e3 | 7.24e-14 | 4.16e-11 | 574 |
| ND r=.99999 | 1.13e-11 | 5.71e-08 | 5.1e3 | 1.49e-12 | 1.62e-09 | 1088 |
| B (fixture) | 3.41e-02 | 5.01e-01 | 15 | 4.89 | 1.38 | **0.28** |

The disagreement is dominated by whichever route is *less* accurate — here the
eigendecomposition route — so it overstates the shipped route's SE error by
2.3e3 to 1.9e4 on every reachable case, failing AC2's 1e3 ceiling five times
out of five; and it **under-reports** the cval error at family C (0.82×) and
at fixture B (0.28×), which is the licensing failure the certificate exists to
prevent. A same-precision route pair prices the wrong route's error and has no
mechanism to know which route it priced. Reject as the literal mechanism; the
precision-separated pair below is what makes the two-route idea sound.

**(b) on-demand exact-rational recomputation is not achievable in base R at
acceptable cost — stated plainly, per the brief's instruction.** Base R has no
arbitrary-precision integers. A doubles-as-limbs bignum with GCD-reduced
fractions is possible in principle, but (i) it is several hundred lines of
novel arithmetic whose own correctness becomes the load-bearing question —
a larger correctness surface than the certificate it serves; (ii) cost: the
C-integer Python oracle takes ~0.5–0.7 s per case at p ≤ 9, interpreted-R
bignum is plausibly two to three decades slower, and exact elimination at
q = 27 grows fraction bit-lengths into the thousands, putting p = 24 at
minutes to hours per fit. Routing through `gmp`/`Rmpfr` in Suggests is also
rejected: a refusal gate cannot gracefully degrade — what `axes_reliability()`
refuses must not depend on which optional packages the user has installed.
(Neither package is even present on this machine; the certificate must not
assume them.)

**Third options weighed and not chosen.**
- *A-posteriori residual / backward-error bounds* and *condition estimates of
  the specific functional*: both convert a computed residual or gradient into
  a forward-error claim through a backward-error model — the assumption that
  the committed rounding is equivalent to some input perturbation of size
  O(ε). That assumption is unproven for this composite pipeline (two nested
  inversions plus a cancellation), and this subsystem's history is exactly a
  story of plausible error models wrong by decades in both directions (the
  κ-bound: 5–8 decades pessimistic in reachable geometry, attained at B).
  They also return bound-shaped numbers, which is the overstatement problem
  this milestone exists to remove.
- *One step of iterative refinement*, correction size as the estimate: prices
  the two solves but misses the `tr_vg − proj` cancellation (B's sign flip)
  unless the residuals are computed in extended precision — at which point
  most of the double-double route has been built anyway. Subsumed by the
  recommendation.

**What the recommended mechanism costs, detects, and how it fails.** Cost:
§7 (measured 0.31 s at p = 24 in an unoptimized prototype; far less at
p ≤ 9). Detects: every error class the double pipeline commits — both
inversions, the sandwich accumulation, and the cval cancellation, since the
identical arithmetic is replayed at 1e16 times finer precision; at B the
certificate reads the sign flip as a 4.9e0 relative error. Fails: only
through a defect in the double-double implementation itself. That residual
channel is what the validation battery is for (§4: a closed-form second
oracle pins the reference route to independently derived truth, planted
perturbations pin the comparison's sensitivity, and the exact-rational oracle
ties the whole certificate at six anchor geometries), plus one cheap runtime
known-answer self-test of the error-free transforms (two_sum/two_prod on
committed operand pairs) so that an exotic floating-point mode (FMA
contraction, x87 double rounding — theoretical under R's C99/SSE2 semantics)
degrades to the shipped a-priori criterion rather than to a silently wrong
certificate.

## 2. The estimated quantity, exactly

Let P be the priced matrix — the realigned `cov2cor(Σ̂)`, the same object
`axes_se_pricing()` and `axes_scaling_factor()` consume — and let se_r(P),
cval(P) be the exact-real-arithmetic values of the shipped formulas at P.
The certificate estimates:

- **E_SE = max over fitted components r of |ŝe_r − se_r(P)| / se_r(P)** — the
  committed relative error of the corrected component SE vector, aggregated
  by max. Max, not a componentwise vector, because the reported vector
  refuses as a unit (the M62/M66 contract): the gate must protect the worst
  component, and a per-component certificate would license nothing the max
  does not.
- **E_cval = |ĉval − cval(P)| / |cval(P)|.**

Both are estimates of the **committed error, not proven upper bounds** — no
theorem converts the disagreement into a bound. The stated safety factor
F = 10 makes the estimate behave as an upper estimate, and that behavior is
enforced empirically: AC2/AC3 pin it at six anchor geometries against exact
truth, and the planted-perturbation invariants (§4) pin its sensitivity
everywhere else. This is the honest shape: the a-priori alternative is a
proven-style bound that misinforms by five to eight decades.

**n-freeness — confirmed, and stronger than the brief asks.** The corrected
SE is sqrt(v_r / n) with v_r = 2 tr(W_c P W_c P). The relative SE error is
|sqrt(v̂_r/n) / sqrt(v_r/n) − 1| = |sqrt(v̂_r / v_r) − 1|: **n cancels
exactly, not merely to first order.** The certificate should therefore be
computed from the v_r themselves — quantities in which n never appears — as

  Ĉ_SE = F · max( δ_v / 2, 2ε ),  δ_v = max_r |v̂_r − v_r^{dd}| / |v_r^{dd}|
  Ĉ_cval = F · max( δ_c, 2ε ),   δ_c = |ĉval − cval^{dd}| / |cval^{dd}|

with F = 10 and ε the machine epsilon. The certificate function should not
even accept an n argument, making AC1's two-n identity structural rather than
tested-into-existence. The only n-touching arithmetic in the reported number —
the final division by n and the square root — contributes at most ~2 ulp
regardless of n, which is what the n-free 2ε additive floor covers. The
δ_v/2 first-order conversion differs from the true SE error by O(δ²): measured
raw ratios 0.997 (case A κ=1e4) and 0.983 (B) — *below 1*, which is exactly
why F exists and why raw disagreement alone would fail AC2's "at least the
oracle's error" clause (§3).

**Scope, stated so it cannot be overclaimed:** the certificate prices the
arithmetic from P to the reported numbers. The optimizer's error in Σ̂ and
cov2cor()'s own ~1-ulp rounding are upstream of P and common to both routes —
out of scope, exactly as they were for the a-priori bound; the indefiniteness
band already prices optimizer error separately (M90's sqrt(p·ε) argument).

## 3. Is the validation window achievable? Yes — measured

The full candidate certificate (double-double comparison, F = 10, 2ε floor,
v-based and n-free) against the exact-rational oracle:

| case | SE true | SE cert | ratio | cval true | cval cert | ratio | in [1, 1e3]? |
|---|---|---|---|---|---|---|---|
| A p=8 κ=1e4 | 5.889e-14 | 5.871e-13 | 9.97 | 2.096e-14 | 2.096e-13 | 10 | yes |
| A p=8 κ=1e5 | 3.004e-12 | 3.004e-11 | 10 | 2.205e-13 | 2.205e-12 | 10 | yes |
| C p=4 κ=1e5 | 6.459e-13 | 6.459e-12 | 10 | 1.124e-08 | 1.124e-07 | 10 | yes |
| ND r=.9999 | 6.302e-13 | 6.302e-12 | 10 | 7.245e-14 | 7.245e-13 | 10 | yes |
| ND r=.99999 | 1.126e-11 | 1.126e-10 | 10 | 1.488e-12 | 1.488e-11 | 10 | yes |
| B (fixture) | 3.413e-02 | 3.355e-01 | 9.83 | 4.890 | 48.90 | 10 | yes |

**AC2 is met on all five reachable cases for both quantities, with the ratio
pinned at ≈ 10 — two decades inside the pre-registered 1e3 ceiling.** AC3 is
met: every reachable estimate is below 1e-4 (largest: 1.1e-7, family C's
cval), and at counterexample B both estimates exceed 1e-4 by three decades or
more (0.34 and 48.9). The underlying double-double reference agreed with the
exact-rational oracle to ≤ 6.2e-16 at every case — the resolution limit of the
oracle's `%.17g` output, with the route's analytic error at
(amplification)·ε_dd ≈ 1.8e-18 even at B. Two cautions for the implementing
tests: (i) with F = 1 the certificate *fails* AC2's floor at two of the six
anchors (raw ratios 0.997, 0.983) — the safety factor is load-bearing, not
decorative; (ii) the measured ratios sit so uniformly at F that the AC2
assertion effectively pins F — a future change to F is a visible, gated event,
which is the right property.

## 4. The second oracle type

The exact-rational Python oracle is the first type. The **second type should
be a closed-form oracle**: one small configuration, driven through the
internal seam (the pricing functions take arbitrary derivative matrices in
`d$mats`), with S and every derivative matrix chosen dyadic-rational so that
v_r and cval are exact rationals derivable by hand once and committed as
literal fractions in the test. What it asserts: **the certificate's reference
route lands on independently derived truth** — the double-double v_r and cval
reproduce the committed fractions to reference-route precision (hi word equal
to the correctly rounded double, lo word bounded), and the certificate at that
configuration therefore equals the known committed error of the double route.
This type shares nothing with the Python oracle — not the code, not the
`fractions` module, not the mirrored pipeline — and nothing with the double
route under test.

A third, supporting layer — not one of the two counted types but worth
shipping under AC4's umbrella — is a **planted-perturbation sensitivity
invariant**: hand the certificate deliberately corrupted double values
(v̂ · (1+δ), ĉval · (1+δ) for δ spanning 1e-10…1e-2, on criterion-accepted and
near-floor matrices) and assert the estimate reads at least F·δ/2·(1 − small).
It asserts the comparison wiring and monotone response with no external truth
at all.

On the brief's flagged hazard: it applies with full force to the
same-precision route-pair mechanism, where an invariant over the two routes
tests the thing by itself. It does not bite here, because the two validation
layers test different properties through different doors: the closed-form
oracle pins the *level* of the reference route against outside truth, the
planted invariant pins the *sensitivity* of the comparison, and the
exact-rational oracle ties the assembled certificate at six geometries. None
of the three is an agreement check between the certificate's own two routes.

## 5. Behavior across the whole admitted domain

The admitted domain — `"singular"` and `"indefinite"` limbs pass — includes
matrices with λ_min anywhere in (−λ_max·sqrt(p·ε), ~0], machine-singular
near-duplicates, and unbounded κ. Measured behavior along that degradation
path (near-duplicate family, p = 9):

- κ = 2.87e5 (just past the current floor): both routes compute; certificate
  reads the true error (1.13e-11) exactly.
- κ = 2.87e6 (two decades past): both compute; true SE error 2.15e-10,
  certificate ratio 1.00. Still six decades inside δ\*.
- κ = 2.87e8: the double route's own `solve(info)` throws (reciprocal
  condition 5.6e-17); the shipped pricing returns `"unidentified"` — there is
  no reported number to certify. The double-double route still computes.
- pair_eps = 0 (machine-singular: r = 1 − 2.2e-16, λ_min = 2.7e-16): same —
  shipped pricing refuses `"unidentified"` on its own before any certificate
  matters.

So the certificate's contract should be: **compute the comparison; if either
route fails to produce finite values, or any v is nonpositive, or a
denominator is zero, return the sentinel estimate 1** — "no digits certified",
finite and non-negative by construction, and above δ\* so M111's gate fails
closed (GP2). Everywhere both routes compute, finiteness and non-negativity
are automatic (absolute ratios of finite numbers with guarded denominators),
and pivoted Gauss–Jordan handles the roundoff-indefinite corner of the domain
without a positive-definiteness assumption.

Degradation is conservative in the operative sense: the estimate *is* the
committed error times ten, so it grows continuously as the error grows and
saturates at the fail-closed sentinel; it cannot sit still while the number
rots. Structural under-reporting would require the reference route to commit
an error equal to and aligned with the double route's, which the sixteen-decade
precision separation rules out — same algorithm, same data, error envelopes
1e16 apart. The one genuine under-report channel is a defect in the
double-double arithmetic itself; that is exactly what §4's closed-form oracle,
the planted-defect battery (AC4), and the runtime known-answer self-test are
for, and the self-test's failure mode must be "fall back to the a-priori
criterion", never "certify anyway".

## 6. Removal, weighed a fourth time

**Verdict: (i) — keep the limb and shrink it to what the certificate cannot
certify; concretely, M111 should re-key the `"ill_conditioned"` refusal from
the a-priori κ floor to the certificate.** Refuse when the estimate exceeds
δ\* or is the sentinel; compute otherwise; `"indefinite"` and `"singular"`
stay as they are.

The reasoning, not a restatement: the κ floor and the certificate are two
instruments for one IP3 obligation. Once a per-fit instrument exists that
measures the actual committed error to within 2 % at every point measured —
including the single known point where the a-priori bound is attained — the
a-priori floor retains no independent evidentiary value. Every refusal it
would add beyond the certificate's is a refusal of a number the better
instrument measured accurate (the M106 exemplar: refused at κ = 1e5 with true
error 3.0e-12); every case it would admit that the certificate refuses is the
silent-wrong-number class. Keeping both gates preserves the five-to-eight
decade overstatement in the band between them, which is the defect this
milestone exists to remove. And the replacement resolves RR19 §5's decisive
objection to a caution-based alternative — "the caution has no truthful
number to carry" — because the refusal warning can now carry the fit's own
measured estimate, and a computed fit can carry its certificate.

Option (ii), read literally as "no ill-conditioning refusal at all once the
certificate ships", is wrong at exactly one measured point and that point is
committed: at B the certificate reads 0.34 — the fit must still be refused,
and the certificate is the thing that refuses it. The shrunk limb *is* the
mechanism of (ii)'s intent. Option (iii) — remove now — is rejected: mid-M108
the certifying means exists as a prototype in a scratch directory, not as
shipped, validated code; removal today recreates B's silent 3.4 % with
nothing standing in front of it. The sole ground all three prior escalations
gave for keeping the limb is still true this week and stops being true only
when M108's validation gates pass.

## 7. Cost

Measured at the largest reachable design (p = 24, q = 27, df = 273): the
double pricing (both SE sandwiches plus cval) is 5.2 ms; the double-double
recomputation is **0.31 s in an unoptimized, elementwise-vectorized pure-R
prototype — 60×**. At p ≤ 9 it is well under 0.1 s. A naive scalar-loop
implementation is 6× worse (1.87 s at p = 24); the difference is tree
summation and elementwise vectorization of the two-word arithmetic, so the
implementation should be vectorized from the start. An acceptable per-fit
envelope: **up to ~100× the double pricing, ≤ ~0.5 s absolute at p = 24** —
the certificate runs once per fit beside a lavaan ML fit that dominates wall
time, and IP1 prices a tenth of a second at zero against a certified number.
The recommendation fits inside that envelope as measured, with obvious
headroom left (the prototype recomputes the SE sandwich per component
serially and inverts with two-word division inside the elimination loop; both
are compressible). No flag needed.

## Beyond the brief

- **B1.** RR20 §5 named "two independent factorization routes compared" as a
  candidate without fixing the routes' precisions. Measured here at equal
  precision it fails M108's own AC2 window five times out of five on SE
  (over-reporting 2.3e3–1.9e4×) and under-reports cval at two of six anchors
  (0.82×, 0.28×). Record it as rejected-by-measurement in the T2 D-entry so a
  later session does not half-adopt the cheap variant.
- **B2.** The two fixture copies (`cairn/reviews/` and `tests/testthat/
  fixtures/`) are currently byte-identical (same SHA-1), and the oracle's
  `FIXTURE` still reads the cairn copy; T4's repoint-and-delete is safe as
  planned.
- **B3.** The Python oracle prints `EXACT_*` at `%.17g`, which caps the
  measurable accuracy of any reference route at ~1e-16 relative. Sufficient
  for AC2/AC3, but if M108 wants to *pin* the double-double reference below
  double resolution (a stronger closed-form-adjacent assertion), have the
  Python side additionally emit the exact value rounded to a hi/lo double
  pair via `%a` — a two-line change.
- **B4.** `fiml_ratio` is the third user-consumed vector priced at
  cov2cor(Σ̂) (the FIML path multiplies lavaan's SE by it). Extending the
  certificate's max over v-errors to include the naive-at-cov2cor v vector —
  both are computed by the same `axes_se_pricing()` call — covers it at
  negligible cost (its relative error is bounded by roughly the sum of the
  two arms'). The baseline factor `cb` needs no certificate: a closed-form
  sum with error ~p²·ε and no inversion.
- **B5.** AC1's "finite, non-negative … for every matrix" is satisfiable only
  with the sentinel contract of §5 stated beside the function: on this domain
  the double pipeline can legitimately fail to produce a number at all, and
  the certificate's value there is the fail-closed 1, whose meaning ("no
  digits certified") belongs in the function's header.

## Recommendations

Tier notes advisory, per the repo's model-tier doctrine. No disagreement with
any constraint in the brief: the constants do not move, D-044's metric is
untouched, no dependency is proposed, and nothing here changes what
`axes_reliability()` returns in M108.

1. **Apply — the double-double comparison certificate** (§1, §2): recompute
   v_r and cval at the priced matrix in compensated double-double arithmetic
   (vectorized, base R); certificate = F·max(δ_v/2, 2ε) and F·max(δ_c, 2ε)
   with F = 10; computed from v_r so n never enters (no n argument at all);
   sentinel 1 on any route failure, nonpositive v, or zero denominator; a
   runtime known-answer self-test of two_sum/two_prod whose failure falls
   back to the a-priori criterion. Tier: Opus for implementation against this
   RR's numbers; the estimator design is settled here.
2. **Apply — the validation battery** (§3, §4): AC2/AC3 at the six anchors
   against the exact-rational oracle (expect ratios ≈ 10; assert the window,
   not the point value); a hand-derived dyadic-rational closed-form oracle as
   the second IP3 type, recorded at the asserting test per the Oracle-records
   convention; planted-perturbation sensitivity invariants. For AC4, defects
   that vary form and location: truncate the lo words before comparing
   (certificate collapses toward 0 — reddens AC2's floor at B), drop the W_c
   diagonal fold in the reference route only (reddens AC2's ceiling on all
   five reachable cases), drop F (reddens AC2's floor at the 0.997/0.983
   anchors), skip an element in the tree sum (reddens the closed-form
   oracle). Tier: Opus, with the oracle run gating.
3. **Apply — record in T2's D-entry** the chosen mechanism, the two rejected
   ones with §1's measured grounds (same-precision routes: fails the window
   both directions; exact-rational in R: correctness surface and decades of
   cost), and the falsifying evidence class: any matrix on which the
   certificate's estimate is below the exact oracle's measured error (an
   under-report), which reopens the mechanism, not the constants. Tier:
   Sonnet (tracking edit).
4. **Apply (routed to M111) — re-key the refusal** (§6): `"ill_conditioned"`
   fires iff the certificate cannot certify (estimate > δ\* or sentinel);
   the warning carries the fit's own estimate; `"indefinite"`/`"singular"`
   unchanged. Until M111 ships, the a-priori limb stands as-is. Tier: the
   rewiring is mechanical against M111's existing criteria (Opus), but its
   review should re-run the exact oracle.
5. **Consider (M111's design call)** — surface the certificate's estimate on
   computed fits (e.g., in `details`), not only inside refusal warnings: it
   is the first per-fit accuracy statement the package can make, and IP3's
   spirit favors showing it. Tier: Opus.
6. **Consider** — B3's hex-precision oracle output and B4's fiml_ratio
   coverage; both are small and neither blocks the milestone. Tier: Sonnet.
7. **Reject — same-precision two-route comparison** as the mechanism: §1's
   table; it fails AC2's ceiling on every reachable case and under-reports
   cval at two of six anchors, including at the committed counterexample.
8. **Reject — runtime exact-rational recomputation**, in base R (cost and
   correctness surface, §1b) or via Suggests (`gmp`/`Rmpfr`): a refusal gate
   must not change behavior with the user's installed-package set.
9. **Reject — removing the limb now (option iii)**: the certifying means is
   not yet shipped; removal today recreates the silent 3.4 % B exemplar with
   nothing in front of it. Removal-by-replacement arrives with M111 on
   recommendation 4's terms.
