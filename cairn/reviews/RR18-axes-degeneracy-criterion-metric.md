# RR18: Which matrix should the fitted-matrix degeneracy criterion price?

- **Date:** 2026-08-15
- **Brief:** `cairn/reviews/RB18-axes-degeneracy-criterion-metric.md`
- **Reviewed at:** branch `m89-fitted-matrix-degeneracy`, files as cited in the
  brief (commit `2695f54f` line numbers).

## Method note (read first — it changes two of the brief's premises)

This review built an **exact rational-arithmetic oracle** for both consumers.
Every quantity from Σ̂ to `cval` and to the corrected variances is a rational
function of the matrix entries: the only irrationals are the `cos()` values in
Δ (which enter as exactly-representable doubles) and the final `sqrt()` (taken
after the comparison). For a unit-diagonal Σ̂ — which the committed
counterexample B is, exactly — `cov2cor()` is the identity, so the entire
pipeline was recomputed in Python `fractions` (offline dev tooling, no package
dependency) from the bit-exact doubles. All "EXACT" numbers below are from
that oracle; all "double" numbers are from `axes_se_pricing()` /
`axes_scaling_factor()` on this machine (R 4.x, reproduced from the committed
`.rds`).

Two measured facts reframe the brief:

**(i) Counterexample B is not a metric counterexample.** The committed
exemplar's diagonal is exactly `c(1, 1, 1)`, so `cov2cor(S) == S` and the raw
and correlation metrics have *identical* eigenvalues: λmin = 4.166e-07,
λmax = 2.7719, κ = 6.654e6 in **both** metrics. The brief's and RO2's framing
— "well conditioned raw but degenerate *after* `cov2cor()`" — is false for
this exemplar. B is evidence about the **cutoff value and the missing
cancellation guard**, not about which matrix to price. (The ROADMAP row and
the milestone's RO2 record should be corrected; see Beyond the brief, F1.)

**(ii) B is a silent wrong number, not merely a surface disagreement.** The
finite corrected SEs returned with `reason = NULL` at B are **wrong by 3.4%**:

|             | double (reported)  | EXACT (oracle)     | rel. error |
|-------------|--------------------|--------------------|-----------|
| SE comp 1   | 0.142594879        | 0.147633963        | 3.41e-2   |
| SE comp 2   | 0.139478432        | 0.144373995        | 3.39e-2   |
| `cval`      | −0.216059 (refused "indefinite") | **+0.055547879** | sign flipped; abs. error 0.272 |

The true `cval` is *positive*. The `"indefinite"` refusal at B is an
arithmetic accident (catastrophic cancellation: tr_vg = 873.854, proj =
873.799, measured amplification (|tr_vg|+|proj|)/|tr_vg−proj| ≈ 8090 at the
double values), not a statement about the model — and it fires only by sign
luck. A draw whose computed `cval` landed at +0.2 instead of −0.216 would have
silently scaled `chisq` by ~3.6× the wrong factor. So B contains, in one
matrix, both halves of M89's own Goal failure mode: a plausible finite wrong
number in a reported field (`components$SE`, error in the third digit, reason
NULL), and a mis-labeled refusal that could as easily have been a
mis-scaling.

---

## Answers

### 1. Which matrix should the criterion price?

**`cov2cor(Σ̂)` for every user-reported quantity, on both surfaces. The raw Σ̂
should be priced only where it is actually inverted — the internal `naive`
arm of `axes_corrected_se()`, which is test-only.**

The justification is in terms of the error that actually propagates, as the
brief demands:

- **Every number `axes_reliability()` reports is a function of `cov2cor(Σ̂)`
  alone.** Verified by reading the consumers: `axes_reliability()` consumes
  only `corrected$corrected`, `corrected$fiml_ratio`, and `corrected$reason`
  from the SE helper (`R/axes_reliability.R:1746-1749`), and
  `axes_scaling_factor()` normalizes before computing anything
  (`R/axes_scaled_fit.R:152`). `naive` is consumed nowhere in `R/` outside
  its own file — only in `tests/testthat/test-axes-corrected-se.R`, as the
  lavaan tie D-037 describes. `details$se_uncorrected` comes from lavaan's
  own `comp_se()`, not from `naive`.
- **The numerical error in the reported quantities is governed by
  κ(`cov2cor(Σ̂)`), not κ(Σ̂), and the two can be made arbitrarily different
  only by a transformation the reported quantities are exactly invariant
  under.** Measured on counterexample A (probe-octant fitted Σ̂, D =
  diag(1e4, 1, …, 1)): κ(raw) = 2.128e8, κ(cov2cor) = 10.447; the
  `corrected` vector changes by ≤ 5.96e-16 relative, `fiml_ratio` by
  ≤ 6.40e-16, and the scaling factor is 0.956334562741 with reason NULL on
  the base matrix — while HEAD refuses the inflated one as
  `"ill_conditioned"`. `cov2cor()` itself is entrywise
  r_ij = s_ij/√(s_ii·s_jj), a few-ε operation regardless of κ(raw); no raw
  conditioning enters it.
- **Nothing the criterion should detect about the matrix *as a model
  statement* is lost by moving metrics.** `cov2cor()` is the congruence
  D^(−1/2) Σ̂ D^(−1/2) (positive diagonal is already guarded upstream at both
  call sites), and by Sylvester's law of inertia congruence preserves the
  signs of the eigenvalues exactly: Σ̂ is indefinite iff `cov2cor(Σ̂)` is
  indefinite; exactly singular iff exactly singular. Only the *quantitative*
  κ differs across the two metrics — and the difference is precisely the
  diagonal-scaling component that no reported quantity depends on. The
  shipped rationale's sentence "when it is not numerically a covariance
  matrix, the correlation-metric quantities downstream are transforms of a
  matrix that never was one" (`R/axes_corrected_se.R:305-308`) conflates
  conditioning with validity: D S D is exactly as positive-definite as S.
- **The raw metric still governs one computation — `naive` — and only
  there.** At counterexample A the raw arm fails (`"unidentified"`, measured)
  while both reported vectors are exact. The raw gate is correct *for that
  arm*, and D-037's tie to lavaan is preserved by keeping it there.

Not a conjunction as a single shared predicate, and not "some other derived
quantity": the answer is **per-matrix-actually-inverted**, which happens to
mean cov2cor-only at the scaling surface and both at the SE helper (because
of its all-three-vectors contract; see Q3).

### 2. What catches the diagonal-inflation regime?

**For every reported statistic: nothing should, because the regime is benign
— and this half of M89's premise was wrong.** Evidence, as the brief requires
for this claim:

- The scaling factor is a function of `cov2cor(Σ̂)` alone and
  `cov2cor(D Σ̂ D) = cov2cor(Σ̂)` to machine precision at every inflation;
  measured, the pre-M89 factor on the inflated matrix equals the uninflated
  one (0.9563346, reason NULL), and this review adds that `corrected` and
  `fiml_ratio` are likewise invariant to ≤ 6.4e-16. The inflated matrix is
  priceable, and the pre-M89 divergence at k = 7 was an honest, loud
  asymmetry: the SE surface refused (wrong literal aside) a quantity whose
  raw arm genuinely dies, while the scaling surface computed a correct
  number. **No scaled statistic was ever silently unscaled or corrupted in
  that regime.** The Goal's failure mode ("a scaled statistic that is really
  unscaled looks entirely normal") never occurred there.
- The one quantity inflation does corrupt is `naive` — internal, test-only,
  and refused by the raw arm (measured `"unidentified"` at κ(raw) = 2.1e8).
  So the raw-metric gate survives exactly as far as `naive` is computed, and
  the inflation regime is "caught" there and nowhere else.

**M89's broader premise — that a stated criterion is needed — was right, but
in the other metric.** Counterexample B (unit diagonal, so visible to a
correlation-metric criterion) is measured silent corruption: 3.4% SE error at
reason NULL, and a `cval` that is 100% wrong. The regime that needs catching
is correlation-metric degeneracy at a tighter cutoff (Q4), not raw-metric
inflation.

### 3. Should the two surfaces share one criterion?

**One criterion *function* — one cutoff form, one τ, one vocabulary — yes.
One *matrix* — no. Each quantity is gated at the matrix it is computed
from:**

- `axes_scaling_factor()`: gate at `cov2cor(Σ̂)` (after its diagonal guards,
  which make `cov2cor()` well-defined).
- `axes_corrected_se()`: gate at **both** the raw realigned Σ̂ (for `naive`)
  and `cov2cor(Σ̂)` (for `corrected`/`fiml_ratio`), refusing wholesale if
  either trips — which preserves the helper's own three-vectors-NA-together
  contract verbatim, at the cost of refusing A-type helper inputs whose
  reported quantities are computable. That cost is a conservative NA with an
  honest literal on inputs unreachable through the exported API, not a wrong
  number; the alternative that removes it is below.

**The cross-surface "NA together" contract must be restated, and I am arguing
for that explicitly as the brief permits.** Under the recommended design the
degeneracy-refusal sets are *nested*: every matrix the scaling surface
refuses, the SE surface refuses with the same literal (the cov2cor arm is
common), and on every matrix whose two metrics coincide — any near-unit
diagonal, which includes every fitted matrix reachable through
`axes_reliability()` (measured diag 0.943–1.072, κ(R) 3.6–21.4) — the
biconditional holds exactly. It is broken only on non-unit-diagonal helper
inputs, where the SE surface may refuse alone; and precisely there the fit
statistics are *provably unaffected* (the ≤ 6.4e-16 invariance above), so the
hazard the biconditional exists to prevent — a user trusting a scaled
statistic computed from the same degenerate input that NA'd the SEs — cannot
occur: the two surfaces' inputs are *not* the same matrix there, and the
refusal literal can say which metric died. A strict biconditional can only be
bought by NA-ing an exactly-computable correct statistic (the O1/RS5 defect,
which I confirm), or by the `naive` decoupling below.

**Alternative preserving the biconditional everywhere (consider):** drop
`naive` from the all-NA-together set — return it NA-alone (own reason field)
when only the raw arm trips, keeping `corrected`/`fiml_ratio` gated at
cov2cor. Then both surfaces gate at exactly one matrix, `cov2cor(Σ̂)`, and
the biconditional is exact on the full helper domain. The anti-fallback
rationale for NA-together is directionless here: the dangerous direction is
"corrected NA, naive finite, naive gets reported", and `naive` is never
reported; the safe direction ("naive NA, corrected finite") is the only one
this creates. Costs a return-contract change and test rework, which is why it
is consider rather than apply.

**D-037 is engaged, not superseded — in fact it is completed.** D-037 already
settled that the user-visible corrected quantities live in the correlation
metric with `naive` deliberately raw. Pricing degeneracy at
`cov2cor(Σ̂)` for the user-visible quantities and at raw for `naive` is the
criterion catching up to D-037's own metric assignments. No part of D-037 is
replaced.

The `:262`/`:264` two-matrix pricing inside `axes_corrected_se()` is
therefore not a complication to eliminate but the design to make explicit:
the helper computes at two matrices, so it gates at two matrices.

### 4. Is the cutoff's form and dimension factor right?

**The dimension p = `nrow(Σ̂)` is right; the κ-floor form is right as the
a-priori input gate; the threshold is wrong by orders of magnitude; and the
κ floor alone cannot see `cval`'s cancellation, which needs its own
a-posteriori guard.**

- **p = nrow is the correct dimension.** The only matrices ever inverted are
  p×p (Σ̂ or R, twice in the sandwich) and the q×q information matrix. No
  p\*-dimensional matrix is ever formed, let alone inverted — the closed
  forms exist precisely to avoid that — so p\* has no business in the bound.
  q enters through the info solve, whose conditioning is empirically
  κ(info) ≈ κ(R)² (measured pairs on the sweep below: κ(R) = 6.65e6 →
  κ(info) = 3.01e15; 1.09e5 → 8.06e11; 1.11e4 → 8.31e9; 1107 → 8.45e7;
  109 → 9.63e5), so flooring κ(R) bounds it, and genuinely rank-deficient Δ
  stays with the `"unidentified"` backstop. Note in passing that at B the
  info solve *succeeded* at κ(info) = 3.0e15 and silently injected most of
  the 3.4% SE error — `solve()` succeeding is no evidence of accuracy, which
  is the strongest argument for a stated a-priori gate over emergent
  refusals, i.e. for M89's core idea.
- **The form is validated, the threshold is not.** Exact-oracle error curve
  (p = 3, the family S_t = t·S_B + (1−t)·I, unit diagonal exact):

  | κ(R)     | SE rel. error (double vs exact) | `cval` rel. error | shipped bound p·κ²·ε |
  |----------|------------------|-------------------|-----------------------|
  | 109      | 6e-11            | 9.5e-10           | 8e-12                 |
  | 1.11e3   | 1.0e-9           | 9.7e-8            | 8e-10                 |
  | 1.11e4   | 2.0e-7           | 1.1e-5            | 8.2e-8                |
  | 1.09e5   | 2.6e-5           | 1.7e-3            | 7.9e-6                |
  | 6.65e6   | 3.4e-2           | ~4.9 (sign flip)  | 2.9e-2                |

  The SE error tracks p·κ²·ε within a factor of ~3 across five decades — the
  bound in the shipped rationale is *empirically right as a predictor*. The
  defect is the threshold: refusing where the bound reaches **1** certifies
  nothing better than "some sign bit may survive". It admits B's 3.4% error
  (bound 2.9e-2 < 1) exactly as designed. **The floor should carry a stated
  accuracy target τ: refuse when p·κ(R)²·ε ≥ τ, i.e. λmin ≤
  λmax·sqrt(p·ε/τ).** Recommended τ = 1e-6 (reported SEs good to ~ppm):
  the floor becomes the shipped inequality × 1000, κ_max ≈ 3.9e4 at p = 3
  and 1.4e4 at p = 24. Headroom: reachable fitted matrices measure
  κ(R) ≤ 21.4, three orders below; exemplar B (κ = 6.65e6) is refused. The
  brief's observation that both counterexamples "sit at p = 3 where the p
  factor makes the cutoff loosest" is answered not by changing p but by τ:
  at any p the shipped floor tolerates O(1) error.
- **The κ floor is constitutionally blind to `cval`'s cancellation, so a
  direct check is needed there too.** `cval = (tr_vg − proj)/df` is a small
  difference of large numbers with measured amplification
  (|tr_vg|+|proj|)/|tr_vg−proj| between 680 and 1.6e4 on the sweep, growing
  with κ (tr_vg grows with ‖R⁻¹‖ while the true `cval` stays ~0.056). That
  is why the `cval` error column runs ~100× the SE column at every κ. Within
  the τ = 1e-6 floor the residual `cval` error is bounded by roughly
  κ²ε·amplification ≈ 7e-5 on the worst sweep point, so the tightened floor
  alone closes the measured cases; a cheap a-posteriori guard — refuse when
  κ(R)²·ε·(|tr_vg| + |proj|) ≥ τ·|tr_vg − proj|, from quantities already in
  hand — is the instrument that prices the actual computation and should be
  added as the backstop for extreme-amplification corners (df small, tr_vg
  large) the input gate cannot see. This is the brief's "direct residual or
  backward-error check on the quantity being computed", scoped to the one
  quantity that measurably needs it.

### 5. The df = 0 divide

**Yes — an explicit `df == 0` guard with its own literal, placed immediately
after the two df-consistency checks (`R/axes_scaled_fit.R:99-101`), before
any matrix arithmetic.** Saturation is a structural fact about the model
(p\* = q; no overidentifying restriction exists, so there is no statistic to
scale), not an arithmetic failure, and it is known the moment `df` is
validated. Reproduced: the brief's deterministic p = 3 construction returns
`"indefinite"` via `cval = Inf` — a false statement on both counts (nothing
is indefinite; nothing was even ill-conditioned; the quantity is undefined).
Suggested literal: `"saturated"`. Reachability: df = 0 requires p = 3 (at
p ≥ 4, q ≤ p + 4 < p(p+1)/2), so it is helper-boundary only behind the
≥ 4-scales gate — but the reason enumerations document the helper contract,
and the helper should not lie at its own boundary.

### 6. Is there an oracle?

**Yes — two usable kinds, one of which this review built and ran.**

- **Exact rational arithmetic** (the Method note): the pipeline is a rational
  function of the doubles, so Python `fractions` — or any exact-rational
  system; none of this touches the package's dependencies, it is offline
  validation tooling like the entombed battery — computes true values
  bit-exactly for unit-diagonal inputs, and near-unit-diagonal inputs can be
  priced at their exactly-represented `cov2cor` output. It settled B
  decisively (true `cval` +0.0555479; true SEs 0.1476340/0.1443740) and
  produced the τ-calibration curve above. This is a genuinely independent
  oracle *type* (exact arithmetic vs analytic bound), satisfying the
  two-oracle doctrine for the criterion itself, and should be adopted into
  the validation battery.
- **The analytic forward-error framework** the shipped rationale already
  uses is the standard one (Higham, *Accuracy and Stability of Numerical
  Algorithms*; Golub & Van Loan §2.6/§3.5) and is now empirically validated
  by the first oracle rather than self-referential.
- **Published treatments of conditioning for Satorra–Bentler scaling factors
  or correlation-structure-corrected SEs:** none known to me; the SB
  literature (satorra1994 and successors) treats the statistic, not its
  floating-point computation. **lavaan's internals** are a practice
  reference only — it guards sample-moment matrices with eigenvalue
  tolerances and falls back to generalized inverses in its SB machinery —
  but its Γ is not this model's Γ_R, so it cannot serve as a numeric oracle
  for these quantities. No second *external implementation* oracle exists;
  the exact-arithmetic oracle fills that slot.

### 7. The reason vocabulary

**The conflation should be partially undone — and the current vocabulary is
exactly backwards at its worst point.** Two changes:

- **Genuine indefiniteness gets its own literal.** λmin < 0 beyond roundoff
  is a statement about the *model* (a Heywood-adjacent solution whose implied
  matrix is not a covariance matrix at all), it is invariant across both
  metrics (inertia), and it is the single most diagnostic fact a user
  debugging their model could be given. M89's T9 measurements show an
  indefinite Σ̂ at λmin = −0.382 now printing `"ill_conditioned"` — an
  arithmetic-sounding label for a model property. Rule: within the refusal
  region, print `"indefinite"` iff λmin < −λmax·sqrt(p·ε) (clearly beyond
  symmetric-eigensolver roundoff; λmin = −9.32e-16 stays
  `"ill_conditioned"`), else `"ill_conditioned"`.
- **The `cval ≤ 0` a-posteriori refusal should stop printing
  `"indefinite"`.** Exact tr(UΓ) ≥ 0 always (both operators PSD), and the
  oracle confirms it at B: the true value is +0.0555 while the double is
  −0.216. A computed `cval ≤ 0` behind passing gates is *always* arithmetic,
  never a model statement — the one place the current vocabulary uses
  "indefinite" is the one place indefiniteness is provably not the cause.
  Relabel to the conditioning literal (`"ill_conditioned"`), with the
  tr(UΓ) ≥ 0 rationale recorded beside it.

The remaining fold — exactly singular (λmin = 0) into `"ill_conditioned"` —
is acceptable: an exactly singular and a barely-nonsingular matrix are the
same object to double arithmetic, and no user action differs between them.

---

## Beyond the brief

- **F1 — the RO2/ROADMAP record mischaracterizes counterexample B.** The
  committed exemplar has an exactly unit diagonal; `cov2cor(S) == S`;
  κ identical (6.654e6) in both metrics; `axes_sigma_degenerate()` evaluated
  at `cov2cor(S)` accepts it just as the raw evaluation does. The stored
  claim "well conditioned raw but degenerate after `cov2cor()`" is false for
  it, and B is not evidence on the metric question. The RO2 note on the
  milestone and ROADMAP row should be corrected when this RR is ingested.
- **F2 — B is the package's first measured silent wrong reported number in
  this subsystem**: corrected SEs off by 3.4% at `reason = NULL`
  (`components$SE` is a documented user-facing field). This upgrades the
  severity of the cutoff question independently of the metric question.
- **F3 — the Decisions entry's calibration is confounded.** "Every measured
  accurately-computing point sits below κ ≤ 8.6e6" reads raw-κ on
  inflated-diagonal grids whose actual computations ran at κ(R) = 10.45;
  accuracy there certifies the correlation metric at κ ≈ 10, not the raw
  metric at 8.6e6. In the metric that governs the reported numbers, M89's
  evidence contains no accurate point above κ(R) ≈ 21, while this review
  measures 2.6e-5 SE error at κ(R) = 1.09e5 and 3.4e-2 at 6.65e6.
- **F4 — `solve()` succeeding is not accuracy**: at B the q×q info solve
  succeeded at κ(info) = 3.0e15. Any future temptation to re-relax the stated
  gate back to emergent `solve()` failures should be resisted with this
  number.
- **F5 — consequences for the AC2 grid.** Under the recommended design the
  inflation grid's expected pins change (scaling computes at every k;
  the SE surface refuses from where the raw arm's floor trips), and the
  grid's "both surfaces agree" assertion narrows to the nested-refusal
  contract of Q3. This is a legal deviation to route through the milestone's
  "Deviations from RR18" table only if the milestone chooses to keep any of
  the old pins; the BCs below assume the new contract.
- **F6 — `eigen(symmetric = TRUE)` reads one triangle** (round-2 RO6/O9).
  With the criterion becoming the primary instrument, symmetrize the input
  (`(S + t(S))/2`) or assert symmetry at the criterion, so the gate and the
  downstream arithmetic see the same matrix.

## Recommendations

1. **Apply** — Move the scaling surface's degeneracy gate to
   `cov2cor(Σ̂)`; keep the SE helper gated at both matrices (raw arm for
   `naive`, cov2cor arm for `corrected`/`fiml_ratio`); restate the
   cross-surface contract as nested refusals with exact agreement on
   unit-diagonal inputs (Q1, Q3). Supersedes the strict biconditional as
   argued; D-037 untouched.
2. **Apply** — Re-express the floor with a stated accuracy target τ = 1e-6:
   refuse when λmin ≤ λmax·sqrt(p·ε/τ) (Q4). Record τ and the exact-oracle
   error table beside the criterion.
3. **Apply** — `df == 0` guard with literal `"saturated"` ahead of any
   arithmetic in `axes_scaling_factor()` (Q5).
4. **Apply** — Vocabulary split: eigenvalue-refusals print `"indefinite"`
   iff λmin < −λmax·sqrt(p·ε), else `"ill_conditioned"`; the `cval ≤ 0`
   refusal relabels from `"indefinite"` to `"ill_conditioned"` with the
   tr(UΓ) ≥ 0 rationale (Q7).
5. **Apply** — Adopt the exact-rational oracle into the validation battery
   and correct the B mischaracterization in the tracking records (Q6, F1).
6. **Consider** — The a-posteriori cancellation guard on `cval`
   (κ(R)²·ε·(|tr_vg|+|proj|) ≥ τ·|tr_vg−proj|): the tightened floor already
   closes every measured case, so this guards corners not yet constructed;
   calibrate its constant against the oracle before wiring it in (Q4).
7. **Consider** — Decouple `naive` (NA-alone with its own reason field) to
   restore the exact cross-surface biconditional on the full helper domain
   at the price of a helper return-contract change (Q3).
8. **Reject** — Pricing the raw matrix for the scaling surface or for the
   corrected/fiml quantities in any form (as shipped, or as a conjunction
   folded into one shared predicate at both surfaces): the reported
   quantities are exactly invariant under the transformation that moves
   κ(raw) (measured ≤ 6.4e-16 across eight decades of inflation), so a
   raw-metric refusal there is a false NA on a computable correct
   statistic, and inertia-invariance means no model-statement content is
   lost by leaving raw unpriced where it is not inverted.
9. **Reject** — Replacing p = nrow with p\* or q in the floor: no
   p\*-dimensional matrix is ever inverted, and q's entry point (the info
   solve) is empirically bounded by κ(R)² once the floor holds, with
   `"unidentified"` as the structural backstop (Q4).

## Binding criteria

- **BC1** — `axes_scaling_factor()` evaluates the shared degeneracy
  criterion on `cov2cor(Σ̂)` (the realigned fitted matrix, after its
  existing diagonal guards). On the counterexample-A construction — the
  probe-octant fitted Σ̂ congruence-scaled by D = diag(1e4, 1, …, 1) — it
  returns `reason = NULL` and a `scale` equal to the unscaled matrix's
  `scale` to within 1e-9 relative.
- **BC2** — `axes_corrected_se()` evaluates the same criterion on both the
  raw realigned Σ̂ and `cov2cor(Σ̂)` and refuses (all three vectors NA, one
  reason) when either trips. On the BC1 counterexample-A construction it
  refuses with the criterion's conditioning literal. Nestedness: over the
  probe grid the revised AC2 test enumerates, plus the committed exemplar
  B, every matrix `axes_scaling_factor()` refuses for degeneracy is also
  refused by `axes_corrected_se()` with the same literal.
- **BC3** — The criterion's floor is λmin ≤ λmax·sqrt(p·ε/τ) with
  τ = 1e-6 recorded as a named constant beside the criterion (equivalently:
  the shipped floor × 1000). At p = 3 it refuses the committed exemplar
  `cairn/reviews/rb18-counterexample-b.rds` (λmin/λmax = 1.503e-7 ≤
  2.581e-5); it accepts all three probe-map fitted matrices
  (`probe_octant()`, `probe_six()`, `probe_single()` fits at p = 24/12/8,
  whose κ(cov2cor(Σ̂)) measure 10.45/4.85/4.08 — this review — against
  floors at κ ≈ 1.37e4/1.94e4/2.37e4).
- **BC4** — `axes_scaling_factor()` refuses `df == 0` with the literal
  `"saturated"`, checked after the two df-consistency guards and before any
  matrix computation; the brief's deterministic p = 3 saturated construction
  (S = {1,.5,.3;.5,1,.4;.3,.4,1}, scales A/A/B, `fit_zeta1 = TRUE`, df = 0)
  returns `"saturated"`, and no path reaches `cval = Inf`.
- **BC5** — Within the refusal region the eigenvalue criterion returns
  `"indefinite"` iff λmin < −λmax·sqrt(p·ε), else `"ill_conditioned"`;
  measured anchors: the M89 T9 indefinite probe (λmin = −0.382) returns
  `"indefinite"` on both surfaces, and the exactly/near-singular probe
  (λmin = −9.32e-16) returns `"ill_conditioned"` on both.
- **BC6** — The `cval ≤ 0` (or non-finite) refusal at the end of
  `axes_scaling_factor()` no longer returns `"indefinite"`; it returns
  `"ill_conditioned"`, with the tr(UΓ) ≥ 0 rationale recorded in a comment
  beside it.
- **BC7** — An offline exact-rational oracle script lives with the
  validation materials (no package dependency) and reproduces, from
  `rb18-counterexample-b.rds`: true `cval` = 0.05554788 ± 1e-7 and true
  corrected SEs 0.1476340 ± 1e-6 and 0.1443740 ± 1e-6; and reproduces the
  Q4 sweep (S_t = t·S_B + (1−t)·I, t ∈ {1−2.5e-5, 1−2.5e-4, 1−2.5e-3})
  showing double-precision SE relative error within a factor of 10 of
  p·κ(R)²·ε at each t.
- **BC8** — The documented reason enumerations, the criterion's in-code
  rationale, and NEWS state the revised contract: the criterion prices
  `cov2cor(Σ̂)` (plus raw for the SE helper's `naive` arm), the two
  surfaces' degeneracy refusals are nested with exact agreement on
  unit-diagonal inputs, and the raw-metric rationale sentence at
  `R/axes_corrected_se.R:299-308` ("...transforms of a matrix that never
  was one") is corrected to the inertia-invariance argument. The tracking
  record correction of F1 (exemplar B is not a metric counterexample) is
  made wherever RO2's claim is recorded.
