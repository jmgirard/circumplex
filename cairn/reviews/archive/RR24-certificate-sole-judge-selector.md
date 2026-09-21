# RR24: The certificate as the sole conditioning judge — licensing below eps, the selector's home and threshold, the exact duplicate-pair check (M147)

- **Date:** 2026-09-21
- **Brief:** `cairn/reviews/RB24-certificate-sole-judge-selector.md`
- **Binding criteria:** not requested; none emitted
- **Read:** the materials the brief lists (`R/axes_corrected_se.R` core, floor,
  refusal, shared-refusal seam and both surfaces' consultation;
  `R/axes_scaled_fit.R`; `R/axes_certificate.R` claim, sentinel, `dd_solve()`
  and the comparison; `R/axes_reliability.R` call site, the four-scale and
  equal-spacing doors, `axes_design()`/`axes_fits_zeta1()`/`axes_fits_zeta2()`;
  `tests/testthat/test-axes-corrected-se.R` M89 T10; the sweep script, its
  committed `.rds` and summary; D-051, D-055, D-061; RR22's reframing
  section, section 2 and recommendation 9; the M147 milestone file).
- **Own measurements:** macOS/arm64, reference BLAS, R 4.6.1, at HEAD
  `17bfe1df`, script in the session scratchpad (not committed). Every number
  below labelled "measured here" is from that run; every other number is
  read off the committed sweep results.

## Summary

The decision is licensed where the sweep looked, and the mechanism argument
says why: the two routes run the same algorithm on the same stored input with
rounding at eps and at eps^2, so their errors scale together and the ratio of
estimate to true error is pinned at the safety factor (measured 9.9996 to
10.005 at the 55 graded region matrices with true error below 1e-2). A
decision-relevant under-report would need the dd route to be about sixteen
decades unluckier than the double route at the same matrix, which is not a
mechanism. Two things must change in D-061 before it ships.

First, **the selector threshold cannot be eps.** The selector exists for one
class only — a design whose information matrix is singular in exact
arithmetic but whose stored double matrix is a rounded version that LU under
`tol = 0` inverts — and for that class `rcond(info)` is itself order-eps
noise. Measured here: ten structurally singular designs (seven one-item-per-scale, three antipodal-block) gave `rcond(info)`
between 2.4e-18 and 5.3e-17 (within a factor 4 to 90 of eps), and generic
rank-deficient 27 x 27 matrices with entries spanning six decades exceeded eps
in 3 of 400 draws (max 2.9e-16). A selector keyed at eps puts the same
coin-flip RR22 diagnosed for the refusal into the one place whose miss is a
silent wrong number (certificate readings of 1.7 to 159 at those designs
with reason NULL). Put it at `sqrt(eps)`. Nothing reachable refuses newly:
the five floor-admitted reachable matrices in the sweep that sit below
`sqrt(eps)` all certify at or below 1.1e-7.

Second, **the sweep does not cover the zeta2 (blockwise) designs** the
exported API reaches, nor p above 24. Measure a crossed-block family before
T5 closes.

The exact pairwise check is right in kind and should be extended by one
tolerance-free clause (a component matrix identical to the identity), which
closes the rank-deficient design D-061 names without any arithmetic. No QR
rank test.

## 1. Licensing below eps

**Mechanism.** The certificate's estimate is 10 x |d - r| / |r| (pre-root, then
halved for the SE fields) where d is the shipped double value and r the
double-double replay; the oracle's true error is |d - e| / |e| with e the
exact-rational value of the same pipeline on the same stored `sigma` and
`d$mats`. Both routes execute the same sequence of operations on bit-identical
inputs; the only difference is the unit roundoff, eps = 2.2e-16 against about
1.2e-32 for double-double. To first order each route's committed error is its
own roundoff times a matrix-dependent amplification K (condition of the
sandwich, pivot growth, q): |d - e| ~ K eps, |r - e| ~ K eps^2. So
|d - r| = |d - e| (1 + O(eps)), and the estimate-to-truth ratio is the safety
factor times (1 + O(eps)) — until the errors are O(1) and the first-order
variance-to-SE conversion stops being linear.

The sweep is exactly what this predicts. Measured here from the committed
`.rds`: at the 55 graded region matrices whose largest true error is below
1e-2 the ratio (all three fields) lies in [9.9996, 10.005]; at the 28 whose
true error is at or above 1e-2 the SE-field ratio runs 9.8 to 37 (the sqrt
nonlinearity), and every one of those refuses. The brief's 9.9996 to 10.0005
is a slightly narrower statement of the same thing.

**Can both routes be wrong together within the target?** For the certificate
to pass (estimate <= 1e-4, so |d - r|/|r| <= 1e-5) while the truth fails
(|d - e|/|e| > 1e-4), the dd route must carry more than 9e-5 of the error and
the double route must land within 1e-5 of it. Under the scaling above that
needs K eps^2 > 9e-5, i.e. K > 7e27, at which point the double route's own
error is K eps ~ 1e12 — and the double route would have to be sixteen decades
inside its own bound while the dd route sits on its bound, at the same
matrix. There is no shared rounding that could correlate them: `dd_of(sigma)`
is exact, and no intermediate is computed once and consumed by both. The
inputs both share — the optimizer's Sigma-hat, `cov2cor()`'s rounding, the
cosines in `d$mats` — are upstream of the certificate's claim by D-051's
stated scope and are equally upstream of the oracle's definition of truth, so
they cannot produce a discrepancy between the two.

What can defeat the model, and what stands in front of each:

- Double-double arithmetic not delivering eps^2 (FMA contraction of the
  two-product, extended precision, fast-math). `axes_dd_selftest()` runs on
  every call and returns the sentinel, which refuses.
- The dd route's own elimination hitting a zero pivot or a nonpositive
  quadratic form. Sentinel, refuses. The region contained 0 sentinels; the two
  region matrices whose double pricing refused after inverting (`"indefinite"`,
  rcond(info) 1.8e-24 and 1.8e-25) refuse `"uncertified"` in the tol0 world,
  which is the fail-closed direction.
- Pivot sequences diverging between the double LU (LAPACK, full-value partial
  pivoting) and `dd_solve()` (pivot on `hi`). Each route's error is still its
  own roundoff times its own growth; the ratio argument survives, with K
  differing between routes by at most the pivot-growth factor, which is small
  at q = 27 and does not approach the sixteen decades needed.

**Licensed for the domain `axes_reliability()` reaches?** Yes for the shapes
the four families span: model-implied correlation matrices of the four-component
model at k in {4, 8} scales, p in {4, 8, 9, 16, 24}, with and without zeta1,
including mixed instruments, item error down to 1e-12, and rcond(info) down
to 1.8e-25. Within that span the answer is not merely "measured": the ratio is
pinned at 10.00 at every graded matrix, so a departure anywhere else in the
same class would need a mechanism the class does not contain.

**Classes the four families do not cover, and must be measured before T5
closes:**

1. **zeta2 designs.** No matrix in the sweep fits zeta2 (`fit_zeta2 = FALSE`
   at `tol0_sweep.R:271`; no block id in any family). The exported API reaches
   them through `blocks`, the derivative set gains a fifth component matrix,
   and the information matrix gains a row whose structure (same-block
   indicator, crossed with the scales) is unlike anything the sweep priced.
   Add a family: `axes_crossed_blocks()` layouts at k = 4 and 8 scales, n = 2
   and 3 items, zeta2 in (0, 0.3), on the same item-error grid.
2. **p above 24.** Real instruments reach 64 items (eight scales, eight items).
   K grows with p and q; the certificate's cost grows as roughly p^3 (about
   3 s at p = 64 by scaling the 0.17 s measured at p = 24). One p = 64 row in
   the m106 family, plus a timing, is enough.
3. **Boundary and negative component estimates.** The random family draws
   xi2, zeta1 >= 0. A converged lavaan fit can return a negative zeta1 or xi2
   (the package's own `axes_is_boundary()` names the case); the model-implied
   matrix is then still PD and priceable but not in the sampled class. A few
   rows with zeta1 = -0.05 and xi2 = -0.02 close it.
4. **Non-octant scale counts** (six, twelve, sixteen), reachable and cheap.

None of these is a class where I expect the ratio to move; they are the
classes where the claim "the ratio is pinned throughout the reachable domain"
is currently unmeasured.

## 2. The selector

### 2(a) Threshold: not eps; `sqrt(eps)`

The selector's job in the sweep domain is nil: at every one of the 85
matrices the default tolerance refused, the floor had already fired
(`floor x default_outcome` table in the `.rds`: `NULL x refused` = 0), and no
floor-admitted matrix in any family has rcond(info) below eps. The selector's
only work is the structural class D-061 names — designs whose information
matrix is singular in exact arithmetic but whose stored doubles LU inverts.
The threshold therefore has to be chosen for that class, and eps is the
worst possible choice for it.

Measured here (every design floor-admitted, every one inverted under
`tol = 0`, every one computing with reason NULL in the tol0 world with no
selector):

| design | rcond(info) | certificate worst |
|---|---|---|
| one item per scale, zeta1 forced, p = 8, item error 0.5 / 0.3 / 0.1 / 0.03 / 0.01 / 1e-3 / 1e-4 | 1.7e-17, 2.8e-18, 1.5e-17, 5.9e-18, 1.1e-17, 2.4e-18, 4.5e-18 | 5.0, 83, 4.5, 1.7, 4.8, 1 (sentinel), 159 |
| four cardinal scales x 2, antipodal blocks, zeta2 forced (C = 2B - Z in exact arithmetic; max entry gap 1.8e-16 from cos(90 deg)) at three error levels | 3.2e-17, 5.3e-17, 3.9e-17 | 5.0, 9.7, 5.0 |
| M89 T10 one scale (zeta1 identical to xi2) | exactly 0 | zero pivot on every tolerance |
| blocks = scales, zeta2 forced (zeta2 identical to zeta1) | exactly 0 | zero pivot on every tolerance |

Generic evidence for the band: 400 rank-deficient 27 x 27 Gram matrices with
one column the sum of two others (rounded in the product, as the information
matrix's dependent row is) gave rcond between 8e-33 and 1.8e-16, 0 of 400
above eps; with column scales spanning six decades, which is what
`Sigma^-1 M_s` products give the information matrix, 3 of 400 exceeded eps
(max 2.9e-16). The condition estimate of a rounded singular matrix is
growth x eps with a growth factor that is sometimes above 1. Keying the
selector at eps reproduces RR22's finding at counterexample B, moved from the
refusal (where a miss is a spurious `"unidentified"`) to the selector (where a
miss is a computed SE wrong by a factor of 5 with no warning).

`sqrt(eps)` = 1.49e-8 is the right threshold, for three reasons. It is a
machine-derived constant with a stated meaning — no pivot growth polynomial in
q reaches it — rather than a calibration to this sweep. Its low-side margin to
the structural band is 7.7 decades from the largest structural rcond measured
(5.3e-17). And its high-side cost is bounded and measured: eight of the 91
floor-admitted sweep matrices sit below it, three at the unreachable p = 3
shape and five reachable (B9 eps = 3.2e-5 at 1.15e-9, random p9 #03 at
2.05e-9, cert b9a at 5.6e-9, cert c4 at 6.4e-9, B9 eps = 1e-4 at 1.15e-8),
every reachable one certifying at or below 1.12e-7 (c4's cval) — three
decades inside the target. The price is 0.17 s at fits already within a
decade of the floor. The brief's "6.4e-9 at four or more scales" undercounts:
the mixed instruments (zeta1 fitted, a single-item scale present), which
`axes_fits_zeta1()` admits, reach 1.15e-9.

`sqrt(eps)` also does something the eps selector and the floor both fail to
do at the p = 3 Q4 perturbation t = 0.999975: that floor-admitted matrix has
rcond(info) 9.4e-13 and a certificate cval of 1.67e-2 — a fit the floor's
bound (7.9e-6 at p = 3, kappa 1.09e5) admits while the true cval error is
about 1.7e-3. The source already knows the bound is optimistic in that regime
and relies on the four-scale door to keep users out of it. A selector at
`sqrt(eps)` would refuse it `"uncertified"`; at eps or at 1e4 x eps it
computes. This is the one measured floor-admitted matrix anywhere in the
sweep with a certificate above the target, and it is the exact shape (info far
worse conditioned than kappa(sigma)^2 predicts) that a floor on sigma cannot
see. The selector is a real second net there, which is a better reason to
have it than the structural corner alone.

A threshold of 1e4 x eps = 2.2e-12 (the AC2 (b) quantity) is the minimum I
would accept: 4.6 decades above the structural band's measured top, 2.7
decades below the reachable floor-admitted minimum. It misses the t = 0.99975
row (rcond 9.1e-11, cval estimate 1.06e-4, just over the target) and has no
derivation beyond "four decades". `sqrt(eps)` is preferred.

### 2(b) Home: `axes_degeneracy_refusal()`, with the core returning rcond as data

The selector cannot live in `axes_pricing_core()` as a third literal. The
core is called by the certificate's own replay (`axes_v_pricing()` and
`axes_u_pricing()` inside `axes_accuracy_certificate()`), so a refusing string
from the core at rcond below the threshold would sentinel the certificate at
exactly the fits the selector sends to it, and the certificate could never
grade a selected fit. The core must invert under `tol = 0`, return
`rcond(info)` beside `si`, `sim`, `acov`, and refuse only on non-finite output
or LAPACK's exact-singular condition. The decision — floor fired OR
rcond(info) < sqrt(eps) — belongs where the floor's decision already is,
`axes_degeneracy_refusal()`, and the core result should travel on the refusal
object through the M117 seam so the surfaces price once. That is what the
implement log's question gate already chose; it is right.

Two details for T4:

- `axes_degeneracy_refusal()`'s early return
  `if (!identical(reason, "ill_conditioned"))` must not return on NULL any
  more: NULL now prices the core and reads rcond. `"indefinite"` and
  `"singular"` still return before any pricing.
- `rcond()` in R runs its own `dgetrf` + `dgecon`; the core's `solve(info,
  tol = 0)` runs `dgesv`. Two factorizations of a 27 x 27 matrix cost nothing,
  but note the selector reads the estimate from a separate LU, which is fine
  only because the threshold is decades from the band. Do not read
  `rcond(info)` and then reuse the estimate as a refusal ground anywhere.

### 2(c) Every fit: reject

Cost is the smaller reason: 0.17 s at p = 24 measured here against 0.0012 s
for the pricing (a 140x ratio), scaling as about p^3 to roughly 3 s at
p = 64, paid inside every bootstrap or simulation loop a user writes. The
larger reason is exposure: the certificate is a large arithmetic surface
whose sentinel refuses. Consulting it on every fit makes every fit's outcome
depend on that surface's health, so a certificate defect (a spurious
condition in `dd_solve()`, a self-test that fails under some future compiler
flag) refuses healthy fits everywhere rather than at the floor's edge. And it
buys nothing measurable: above the floor the a-priori bound p kappa^2 eps is
below tau = 1e-5 by the floor's own definition, and the sweep's floor-admitted
reachable maximum certificate is 1.12e-7. The selector at `sqrt(eps)` covers
the one floor-admitted regime where the bound is optimistic (2(a)).

## 3. The exact duplicate-pair check

**Keep it, extend it by one exact clause, and add no rank test.**

What is structurally reachable matters more than what is structurally
possible. The derivative set is {C = cos(theta_i - theta_j), J = all-ones,
[B = same-scale], [Z = same-block], E_1..E_p}. A linear dependence among them
is either integer-exact in the stored doubles or it involves C's cosines and is
therefore not exact in the stored doubles at all. Enumerating:

| dependence | when | exact in doubles? | caught by |
|---|---|---|---|
| B = J | one scale | yes | pairwise check (M89 T10) |
| Z = J | one block | yes | pairwise |
| Z = B | blocks are the scales | yes | pairwise |
| C = J | all angles bit-equal | yes (cos(0) = 1 exactly) | pairwise |
| B = sum E_i = I | one item per scale, zeta1 fitted | yes | **not pairwise**; `identical(B, diag(p))` |
| Z = sum E_i = I | one item per block, zeta2 fitted | yes | **not pairwise**; `identical(Z, diag(p))` |
| C = 2B - J, C = 1.5A - 0.5J, C = 2B - Z, ... | two antipodal scales; three equally spaced scales; antipodal blocks | **no** — off by cos() rounding (1.8e-16 measured) | selector + certificate (rcond 3e-17 to 5e-17, certificate 5 to 10, refuses `"uncertified"`) |

Every row is unreachable through the exported API: the four-scale and
equal-spacing doors (`angles_spacing_status()`, tol 1e-8) exclude the
one-scale, two-scale, three-scale and equal-angle rows; `axes_fits_zeta1()`
excludes B = I; `axes_fits_zeta2()`'s rank test on the off-diagonal design
excludes every Z row (it already returns FALSE at the antipodal-block layout,
measured here). So the check is a helper-contract guard, and the question is
what shape of guard is honest at the helper.

The exact pairwise check is honest: it fires on a property of the design that
holds on every platform, and it fires ahead of any arithmetic. Its two blind
spots are the identity rows, which are equally exact and equally
tolerance-free: `any(vapply(d$mats[seq_len(d$n_comp)], identical, TRUE,
diag(p)))`, or its combinatorial equivalent (every scale a singleton with
zeta1 fitted; every block a singleton with zeta2 fitted). Adding that clause
means the rank-deficient design D-061 uses to motivate the selector refuses
`"unidentified"` on every platform, and the selector is left with only the
cosine rows — which are not structurally singular in the stored matrix, so
`"uncertified"` is the literally correct word for them.

Do not add the pairwise check's `identical()` to the E_i pairs' comparison
budget without noticing q^2/2 = 351 comparisons of 24 x 24 matrices; it is
still microseconds, but comparing only the `n_comp` component matrices
against each other and against I is enough, because two E_i are never
identical.

**Reject the QR rank test on `vec(d$mats)`.** It is complete, but its only
additional catches over the two exact clauses are the cosine rows, which are
not exact singularities; it introduces a tolerance (and the package's
existing `axes_design()` rank test is only stable because its tolerance
1e-7 sits nine decades above the cosine fuzz); and it would make
`"unidentified"` say "singular to within 1e-7" at the pricing surface while
the same word means "bit-identical" at the design. The selector plus
certificate already gives the cosine rows a graded, platform-stable answer.

**Reject dropping the check.** LU on bit-identical rows produces an exact
zero pivot only if every kernel path applies the same rounding to both rows;
blocked and vectorised LU can route two identical rows through different
micro-kernels with different accumulation orders. The milestone log's own
record — an 8 x 8 duplicate-row toy inverting on one run and hitting
U[8,8] = 0 on another — is that platform dependence. The M89 T10 literal
should not depend on it.

## 4. Removal: (i) amended

**(iii), keep the default tolerance and the D-055 two-route admission:**
reject. It leaves in the shipped package a refusal decided by LU roundoff at
every matrix whose rcond(info) straddles eps — the sweep shows 37 of the 85
matrices the default gate refuses here compute with true error at most
9.9e-6 in the tol0 world, i.e. correct numbers withheld on some platforms and
delivered on others under a literal (`"unidentified"`) that claims a property
of the model. The two-route test admission is a truthful description of that
behaviour, not a fix for it.

**(ii), pure `tol = 0` with no selector and no structural check:** reject on
the measurement in 2(a): every structurally singular design measured here
inverts under `tol = 0`, passes the floor, and computes with reason NULL
while its certificate reads 1.7 to 159. The rank-deficient design is
unreachable today, but "the helper reports a number wrong by 5x with no
warning whenever a future caller forces zeta1 on a singleton design" is not a
contract the helper should carry.

**(i), amended:** `tol = 0` on the certified path; the selector in
`axes_degeneracy_refusal()` at `sqrt(eps)`, not eps; the exact structural
check as the pairwise clause plus the identity clause; `"unidentified"` on
exactly three grounds (bit-identical pair, component matrix identical to I,
non-finite or exact-singular inversion); the raw arm at the default
tolerance. The amendment to the threshold and the added clause are
departures from D-061's text and need a superseding D-entry.

## 5. AC2 (b)

The four-decade margin is the wrong quantity to pre-register, because it is a
margin above eps, and eps is the threshold this report recommends abandoning.
What (b) was trying to say is "the selector never fires at a fit the floor
admits in the reachable domain, with margin"; once the threshold is
`sqrt(eps)` that is false by design at floor-edge fits and is not what needs
to hold. Register instead the three quantities the design actually depends
on:

- (b1) The selector threshold theta = `sqrt(.Machine$double.eps)`, by name.
- (b2) Low side: at every committed structurally singular probe (the four
  designs of the table in 2(a), or the subset T3 commits), `rcond(info)` is
  below theta / 1e4, i.e. below 1.5e-12. Measured here: max 5.3e-17.
- (b3) High side: every floor-admitted matrix of a design the exported API
  admits (k >= 4 equally spaced scales, zeta1 per `axes_fits_zeta1()`, zeta2
  per `axes_fits_zeta2()`) that the selector sends to the certificate passes
  it, with its estimate listed. Committed run: five matrices, max 1.12e-7.

On the narrowing itself: "four or more scales" is the right direction but the
wrong predicate. The domain is "what the exported API admits", which is the
conjunction above, and the (b) subset as drafted (zeta1 only with two or more
items on every scale) also excludes the mixed instruments the API admits and
which hold the reachable minimum (1.15e-9). Do not delete the p = 3 rows
from the sweep: they are the only floor-admitted matrices anywhere with a
certificate above the target (t = 0.999975: cval 1.67e-2; t = 0.99975:
1.06e-4), which makes them the best regression probe the sweep has for the
selector's high-side behaviour. Keep them measured and outside (b3), stated as
out of domain.

## 6. The raw arm

The asymmetry may stand. `naive` never reaches the user (`axes_reliability()`
forwards only `naive_reason`), the certificate never prices raw Sigma-hat
(D-037), and `tol = 0` on that arm would put a value wrong by 30 to 130 % (RR22's
forced-pricing row) into a vector whose one purpose is to reproduce lavaan's
SE to 1e-7. A default-tolerance refusal into `naive_reason` is the honest
outcome for that arm.

Two consequences to write down rather than defects:

- With the structural check in the core, the raw arm inherits it: a
  duplicate pair now refuses `"unidentified"` on both arms, which is what the
  M89 T10 test already asserts (`naive` NA too). Nothing to change.
- `naive_reason` becomes the one place a platform-dependent literal survives
  in the package, and it survives exactly at counterexample B, whose raw
  matrix IS its cov2cor matrix (unit diagonal): on 97 of 300 one-ulp
  neighbours the raw arm refuses `"unidentified"` while `reason` is
  `"uncertified"` everywhere. Any test that asserts `naive_reason` at B or in
  the band must take the same two-route shape D-055 prescribes, and AC4's
  prose sweep should say that `details$naive_reason` can differ across
  platforms at a matrix the reported fields treat identically.

## Beyond the brief

- **B1. The brief's "6.4e-9 at four or more scales" is the (b)-subset
  minimum, not the reachable minimum.** Mixed instruments (zeta1 fitted, some
  single-item scale) are floor-admitted at rcond(info) 1.15e-9 (m106 B9
  eps = 3.2e-5) and 2.05e-9 (random p9 #03). Both are reachable. Used in 2(a).
- **B2. One floor-admitted matrix carries a certificate above the target.**
  q4 t = 0.999975 (p = 3): cert cval 1.67e-2 with reason NULL under the
  shipped predicate and in the tol0 world. Unreachable (three scales), and
  the source's own comment anticipates it ("the bound optimistic beyond the
  fixture's own 3.3x ... the four-scale minimum is what keeps the reachable
  set out of that regime"), but it is the first floor-admitted matrix the
  package has measured with a certificate past delta_star, and it should be
  recorded as such in the sweep summary rather than left inside a
  pass/fail line. A `sqrt(eps)` selector refuses it.
- **B3. The certificate's "ratio 10" is a first-order identity, not a
  calibration.** Section 1 states why; it is worth one sentence at the
  certificate's header, because the current text ("pinned empirically rather
  than assumed") undersells what the sweep shows and a reader could take the
  55 matrices as the whole ground.
- **B4. `axes_degeneracy_note()`'s hint precondition still holds** for
  selector-routed refusals (lambda_min > 0 there, so the hint prints a
  condition number and, where a near-collinear pair exists, names it), but
  the hint's conditioning clause will now sometimes name a kappa the floor
  admits. Restate the scope note at `axes_degeneracy_hint()` so it does not
  claim `"uncertified"` is reached only from `"ill_conditioned"`; the roxygen
  at `R/axes_reliability.R:730` and the vocabulary comment at :2044 need the
  same restatement (AC4 already schedules the grep).
- **B5. The sweep's bit-identity claim (c) is a property of `solve()`'s
  code path, not of the tolerance.** `La_solve` factorises once and checks
  rcond afterwards, so `tol` cannot change the factorisation; (c) is worth
  keeping as a guard against a future `solve()` implementation, but its
  rationale in the header should say that it is expected to hold by
  construction.

## Recommendations

1. **Apply — selector threshold `sqrt(.Machine$double.eps)`, not eps** (2(a)).
   Supersede D-061's "a condition estimate below eps selects the fit" with
   the named constant and its two margins. Tier: Fable at the D-entry (it
   moves where the shipped package emits a number), Opus for the edit.
2. **Apply — extend the exact structural check by the identity clause** (3):
   `"unidentified"` on a bit-identical pair among the component matrices, or
   a component matrix identical to `diag(p)`, or a non-finite or
   exact-singular inversion. Supersede D-061's "two grounds" with three. Add
   the one-item-per-scale zeta1 design to the duplicate-ground tests as a
   platform-stable `"unidentified"`, and move the antipodal-block zeta2
   design (or another cosine-row design) into the routed-ground test as the
   case that refuses `"uncertified"`. Tier: Opus.
3. **Apply — add a zeta2 family to the sweep before T5, plus one p = 64 row,
   a few negative-component rows, and k in {6, 12}** (1). Extend AC2 (d) so
   the zeta2 family is one of the families that must be non-empty. Tier:
   Sonnet for the script, Fable to read the T5 numbers if any ratio departs
   from 10.00 by more than the sqrt nonlinearity explains.
4. **Apply — restate AC2 (b) as (b1)–(b3)** (5): the threshold by name, the
   low-side gap at committed structural probes, the high-side list of
   floor-admitted reachable matrices the selector routes and their certificate
   values. Define "reachable" as the API's own conjunction, not "four or more
   scales". Keep the p = 3 rows measured and out of domain. Tier: Sonnet.
5. **Apply — keep the selector in `axes_degeneracy_refusal()`, the core
   returning `rcond(info)` as data and refusing only on non-finite or
   exact-singular** (2(b)). The NULL branch of the refusal helper now prices;
   `"indefinite"` and `"singular"` still return first. Tier: Opus.
6. **Apply — record B2 in the sweep summary and in the D-entry's Reopens**
   (a floor-admitted matrix with a certificate above the target is now a
   measured fact at p = 3, and the reopening clause should say what changes
   if one appears at k >= 4: the floor's tau, not the selector). Tier: Sonnet.
7. **Consider — the AC4 prose for `naive_reason`** (6): one sentence that it
   can differ across platforms at a matrix whose reported fields do not, and
   the two-route shape for any test that asserts it in the band. Tier: Sonnet.
8. **Consider — B3, B4, B5 comment restatements.** Tier: Sonnet.
9. **Reject-with-reason — the certificate on every fit** (2(c)): 140x the
   pricing's cost at p = 24 and about 3 s at p = 64 inside every user loop,
   and it makes every fit's outcome depend on the certificate surface's
   health, for no measured gain above the floor.
10. **Reject-with-reason — a QR rank test on `vec(d$mats)`** (3): its only
    additional catches are cosine dependences that are not exact
    singularities in the stored matrix, it introduces a tolerance into a
    literal that elsewhere means bit-identity, and the selector plus
    certificate already answers those designs.
11. **Reject-with-reason — pure `tol = 0` (RR22 rec 9 as written)** (4):
    ten structurally singular designs measured here invert under
    `tol = 0`, pass the floor, and compute with reason NULL at certificate
    readings of 1.7 to 159.
12. **Reject-with-reason — keeping the default tolerance and the D-055
    admission as the shipped state** (4): 37 of 85 region matrices in the
    sweep compute correctly (true error at most 9.9e-6) where the default
    gate refuses them on this platform and not on others.
13. **Reject-with-reason — `tol = 0` on the raw arm** (6): it would place an
    uncertified value wrong by 30 to 130 % at B into the one vector whose
    purpose is the lavaan tie.
