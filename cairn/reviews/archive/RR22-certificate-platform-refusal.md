# RR22: A platform-dependent refusal at the certificate's counterexample B (M121/M122)

- **Date:** 2026-09-05
- **Brief:** `cairn/reviews/RB22-certificate-platform-refusal.md`
- **Reviewer:** Fable (independent review; advisory — binding criteria not
  requested)
- **Materials read:** `tests/testthat/test-axes-certificate.R` (full);
  `R/axes_corrected_se.R` 120–260 and 660–935 (`axes_pricing_core()`,
  `axes_sigma_degenerate()`, `axes_degeneracy_refusal()`,
  `axes_shared_refusal()`, `axes_certificate_worst()`,
  `axes_degeneracy_note()`); `R/axes_certificate.R` 400–end;
  `R/axes_scaled_fit.R` 40–110 (`axes_u_pricing()`); the six further fixture
  read sites named in the brief with their assertions; `cairn/DESIGN.md`
  principles and Known fragilities; RR21 (archived) in full; D-048–D-054;
  the ROADMAP's v2.0.1, arm64-harness and all-skip-detector rows;
  `cairn/LESSONS.md` line 46 (the M108-family lesson); the M115 and M118
  archives' Outcome/Review lines; `helper-m106-degeneracy.R`'s builders;
  `cran-comments.md`, `NEWS.md`, `tests/testthat.R`.
- **Measurements performed for this review** (macOS aarch64, R 4.6.1,
  R's reference BLAS/LAPACK; constructions stated so M122 can rerun them):
  (i) `rcond(info)` at the committed fixture and at 300 one-ulp neighbours of
  it (each upper-triangle entry multiplied by 1 + {−1, 0, +1}·ε, seed 2);
  (ii) the shipped pricing, the certificate and the refusal predicate at each
  neighbour; (iii) `solve(info, tol = …)` at ε, 1e-16, 5e-17, 1e-17 and 0
  across the same neighbours; (iv) forced pricing (`tol = 0`) at the
  refusing neighbours, measured against the dd replay; (v) the dd reference
  route at B against the committed exact hi/lo pairs.

---

## The measurement that reframes the brief

The brief presents the arm64 refusal as two platforms straddling a
threshold. It is narrower than that: **the refusal at counterexample B is a
coin flip on every platform, decided by rounding noise in the fixture's own
last bits.**

| quantity at B (this machine) | value |
|---|---|
| `rcond(info)` at the fixture | 2.6008e-16 (LAPACK estimate = exact 1-norm value here) |
| `rcond(info)` over 300 one-ulp neighbours | min 1.667e-16, median 2.41e-16, max 3.35e-16 |
| neighbours the shipped pricing refuses (`"unidentified"`, `"unidentified"`) | **97 of 300** |
| criterion `axes_sigma_degenerate()` at every neighbour | `"ill_conditioned"` (300/300) |
| refusal predicate at every neighbour | `"uncertified"` (300/300) |
| certificate `se` on the 203 priced neighbours | 0.074 … 2.18 |
| certificate `cval` on the 203 priced neighbours | 2.1 … 128 |
| dd reference route at every refusing neighbour | computes |
| forced `tol = 0` pricing at refusing neighbours, `v` vs dd replay | relative error 0.32 … 1.28 |
| dd route at B vs committed exact values | `v` 1.4e-18, `v_naive` 1.4e-18, `u` 6.3e-16 (hi words of `v`, `v_naive` bit-identical) |

Three consequences follow, and the rest of this report rests on them.

1. `.Machine$double.eps` sits inside the band `rcond(info)` occupies under
   ulp perturbation of B (1.67e-16 to 3.35e-16). Which side a given machine
   lands on is decided by its LAPACK's LU roundoff, not by any property the
   fixture has that the suite could commit. macOS aarch64 and OpenBLAS aarch64
   are two draws from that band; ubuntu-latest and windows are two more that
   happened to land above ε.
2. On the platforms that price B, the double route's answer is itself noise:
   an SE wrong by anywhere between 7 % and 220 % depending on which ulp the
   entries carry, `cval` wrong by a factor of 2 to 128. The `0.0341` (macOS)
   and `0.124` (ubuntu) figures already in the file's comments are two draws
   from this distribution.
3. The user-visible answer is identical on every platform: the criterion says
   `"ill_conditioned"`, the certificate is consulted, and the fit is refused
   `"uncertified"` — with a graded estimate on one route, the sentinel on the
   other. The only observable difference is the two digits in the warning
   ("estimated relative error 49" against "estimated relative error 1").

The repo already knew the first point. `test-axes-scaled-fit.R:1740`'s
"WHY NOT EXEMPLAR B" comment records that nudging one fixture entry by a
single ulp, thirteen times, "made `solve()` refuse outright twice". The
refusal branch's premise — an admitted geometry cannot refuse, so a refusal is
a regression — was contradicted by a measurement in the same test suite.
That is the defect: a frozen platform fact ("`cxb` always prices") of exactly
the kind the M108-family lesson warns against, at a site the M115 sweep did
not reach.

## 1. What must remain asserted at counterexample B where the pricing refuses

Specific assertions, all of which run on both routes unless marked:

**Outside any branch (already there, keep):**
- `expect_equal(m106_kappa(fx$S), 6654372.506, tolerance = 1e-6)` — the
  fixture is still the matrix the exact values describe (line 542).
- `expect_identical(axes_sigma_degenerate(fx$S), "ill_conditioned")` — add
  here; today this is asserted only in the skip-on-CRAN refusal suite. It pins
  that B still reaches the certificate's limb and not `"singular"` or
  `"indefinite"`.

**The matrix check, hardened for B.** `cert_true_error()`'s first exit skips
when `sigma[upper.tri(sigma)]` is not bit-identical to `fz$sig`. For the five
`cos()`-built anchors that is the right disposition. For `cxb` it is not: the
matrix is read from committed bytes, so a mismatch means the fixture or the
frozen block changed, and the honest verdict is `fail()`, not `skip()`. The
`cxb` case must never be able to record `"skipped"`.

**On the refusing route, in place of the priced brackets:**
- The refusal's identity, exactly: `expect_identical(v, "unidentified")` and
  `expect_identical(u, "unidentified")`. Not `"singular"` — `rcond(sigma)` is
  1.39e-7 at B, five decades inside double range, so a `solve(sigma)` failure
  would be a genuine regression in `axes_pricing_core()`. Not `"indefinite"` —
  that literal lives in `axes_se_pricing()`, downstream of the two functions
  the certificate suite calls, so its appearance here would mean a wiring
  change. Any other string, or a refusal from only one of `v`/`u`, fails.
- The certificate's answer, exactly:
  `expect_identical(axes_accuracy_certificate(fx$S, d), axes_certificate_sentinel())`.
  This is D-051's contract — a refusal from the shipped route yields the
  all-ones sentinel — asserted for the first time at a matrix that reaches it
  naturally rather than through a planted duplicate derivative (line 724's
  test).
- The refusal predicate's answer: `axes_degeneracy_refusal(fx$S, d)$reason`
  is `"uncertified"`. This is the claim users depend on and it holds on both
  routes; asserting it here makes the CRAN-live cxb test say what the
  skip-on-CRAN refusal suite says.

**What survives of the bracket claim without a priced value.** The bracket
has two halves: the shipped route's error against exact truth, and the
certificate's estimate against that error. Without a shipped value the first
half is empty. But the certificate's *reference* route — the dd replay — is
pure R arithmetic, platform-independent, and computes at B on every route
(measured: computes at all 97 refusing neighbours). Its agreement with the
committed exact hi/lo pairs is the yardstick the certificate *would* have
used, and it is checkable with no shipped value at all:
`|dd − exact| / |exact|` ≤ 1e-14 for `v`, `v_naive` and `u` (measured 1.4e-18,
1.4e-18, 6.3e-16; the `u` figure is larger because `u` is a difference of two
~0.9 quantities and the dd route's own error is amplified by the same
cancellation the double route loses to). The file's comment at lines 116–121
says the dd route is "deliberately NOT pinned" — but the concern recorded
there is about using the dd route as a *precondition* (a gate that skips),
not about *asserting* it against independent truth, which the closed-form
oracle tests already do. This assertion should run on both routes at cxb; it
is the one part of the exact-rational oracle's reach at B that the refusing
platform does not lose. Say so in the comment (IP3 obligation from the
brief's constraints).

**The two `expect_gt(true_rel$…, delta_star)` assertions.** These claim "no
machine gets within the target on this matrix". A refusal satisfies that
claim by construction — a refused fit reports no number, so it is not within
the target — and the certificate says so numerically: assert
`axes_certificate_worst(cert) > axes_degeneracy_delta_star` outside the
branch. On the priced route the two existing `expect_gt` calls still run and
still carry their margin (measured ≥ 3 decades at every one of the 203 priced
neighbours: minimum `se` error 0.074 against 1e-4).

**Disposition.** Record `"refused -- unidentified"` for cxb, distinct from
`"priced"` and from `"skipped -- …"`, so the detector and the log can see
which route ran (Question 5b).

## 2. Telling a platform-dependent refusal from a regression

**Recommend (A), scoped as follows; reject (B); (C) noted as a later gated
option.**

**(A), scoped so it cannot widen.** The admission is not "counterexample B
may refuse". It is a conjunction, every clause of which must hold or the
case fails:

1. `id == "cxb"` — the *only* case with an admission. The five anchors keep
   the unconditional `fail()`. Their `rcond(info)` values are decades above
   ε (they price on every platform measured, arm64 included — the arm64 run
   passed them), so a refusal there stays a regression.
2. The refusal literal is exactly `"unidentified"` from both `v` and `u`.
3. The criterion says `"ill_conditioned"` and `kappa` is pinned (already).
4. The matrix is bit-identical to `fz$sig` (a mismatch is `fail()` for cxb,
   never `skip()`).
5. The certificate returns exactly the sentinel and the predicate says
   `"uncertified"`.
6. The dd route agrees with the exact values to ≤ 1e-14.

Ground the admission in a committed *property*, not a case name: add to the
`cxb` entry of `cert_frozen` (or beside it) the measured `rcond(info)` band —
`c(1.67e-16, 3.35e-16)` under one-ulp perturbation, seed and construction
stated — and the sentence "this band straddles `.Machine$double.eps`, so
`solve(info)`'s outcome is a property of the platform's LU roundoff, not of
the matrix". Then the admission reads: a case may refuse iff its committed
band contains ε. Today only cxb has such a band; adding one to an anchor
would be a visible, reviewable edit rather than a widened `if`.

The all-skip detector then requires: cxb ∈ {`"priced"`, `"refused --
unidentified"`} (never skipped — it is bytes), and (Question 5a) at least one
anchor `"priced"`. Every case refusing is impossible under this scoping
(anchors cannot record a refusal without failing), so the brief's "redden
when every case refuses" is subsumed.

**What falsifies (A).** (i) A platform on which cxb *prices* and the
certificate's `se` or `cval` comes in *below* `delta_star` — that would mean
the double route landed within the target on a matrix whose true error is
0.07–2.2 under ulp noise, which is an under-report and reopens D-051's
mechanism, not this test. (ii) A refusal at cxb with any literal other than
`"unidentified"`. (iii) A platform on which cxb's dd route fails to compute
(dd_solve's sentinel) — the certificate would still refuse, but the yardstick
assertion would fail and the file would rightly go red, because then the
reference route has a defect at the one matrix that matters. (iv) An anchor
refusing anywhere.

**(B), rejected — measured.** Every explicit tolerance is either a coin flip
or a removal of the gate:

| `tol` | refusing neighbours / 300 |
|---|---|
| ε = 2.22e-16 (default) | 97 |
| 1e-16 | 0 |
| 5e-17 | 0 |
| 1e-17 | 0 |
| 0 | 0 |

No `tol` inside the band [1.67e-16, 3.35e-16] gives a platform-stable
answer, because the band is what the platforms sample from. Any `tol` at or
below 1e-16 makes B price everywhere — and what it prices is wrong by 32 % to
128 % (the forced-pricing row above), which the certificate then refuses
`"uncertified"` anyway. So (B) buys platform stability only by turning the
LAPACK conditioning gate off, at which point it is not a tolerance but a
design change: "the shipped pricing never refuses on conditioning; the
certificate is the sole judge". That is a coherent position (below, as C′) but
it is a change to when the *shipped* package emits `"unidentified"`, it
touches a literal asserted across the refusal suite, and its oracle would be
a sweep over the admitted domain showing the certificate catches every matrix
the default gate used to refuse. Not the 2.0.1 fix, and not something the
brief's constraints permit as an implementation detail. The brief's own
warning — a tolerance calibrated on one machine is not a tolerance — is
exactly what the table shows: there is no machine-independent value inside
the band, and every value outside it is a removal.

**(C′), for a later gate.** Since M111 the refusal decision is the
certificate's; `axes_pricing_core()`'s `solve(info)` gate predates it and now
duplicates it badly (a binary refusal keyed to an rcond estimate against a
graded estimate keyed to the actual error). Passing `tol = 0` there — refuse
only on exact singularity or non-finite output, let the certificate judge —
would remove this whole class of platform-dependent refusal from the shipped
package. Cost: `"unidentified"` becomes rare; the certificate must be shown
to refuse everything the gate used to (sweep over the M89/M106 near-threshold
families and the machine-singular corner; RR21 §5 already measured the
certificate refusing the pair_eps = 0 case on its own). Weigh at a plan gate
with its own D-entry; the exact-singular path (`dgesv` INFO > 0) still errors
with `tol = 0`, so `"unidentified"` keeps a meaning.

## 3. Is refusing this matrix correct behavior?

**Yes, on both platforms, and the package is reporting honestly in both.**
The user-facing contract at B is "refused, `uncertified`, with an estimate in
the warning" on every platform measured or perturbed (300/300 neighbours).
What differs is the estimate printed: a graded number where the double route
produced values (49 on this machine; 0.42 on ubuntu; between 2 and 128 across
neighbours), or 1 where it produced none.

Is the sentinel honest at B? It means "no digits certified" and prints as 1.
Where the shipped route produced *nothing*, there is no reported number to
have a relative error, so the sentinel is the only truthful value; a graded
figure would be a fabrication. Where the route produced a number, the graded
estimate is the truth about that number. Both statements are true of the
machine that made them. What neither states — and what the brief's question
reaches for — is that at B *the double route's output is noise on every
machine*: its error under ulp perturbation spans two decades (0.07–2.2), so
the graded "49" is no more a property of the matrix than the sentinel is.
The warning does not overclaim (it says "estimated", two significant digits),
and the fit is refused either way, so users are not misled. If anything the
sentinel is the more honest reading of B.

Should the package refuse B *everywhere* by a stronger rule? No new rule is
needed: the certificate already does, on every route, and it does so on the
right ground (measured error against target). Forcing `"unidentified"`
everywhere would need a tolerance, which Question 2 shows does not exist.
The test's job is therefore not "assert B prices and is wrong by 3.4 %" but
"assert B is refused `uncertified` on every route, that the route taken is
one of the two admitted ones, and where a value exists, that the certificate
brackets it". That is a change to the test's stated job, and it should be
written into the test's header.

One honesty gap does exist and is worth a sentence in the warning's
documentation, not a code change: at a matrix this ill-conditioned the
estimate's digits are machine-specific, so "estimated relative error 49" on
one laptop and "1" on another are both the same verdict. The `?axes_reliability`
prose about the certificate should say the estimate is a property of the fit
*on this machine*. Consider, not apply — it is prose, and the brief's scope
is the test.

## 4. Retire, or keep and repair

**Keep the certificate and its exact-rational bracket; retire nothing; but
change what the suite is allowed to assert, so this is the last escalation of
this kind.** Reasoning:

**What the certificate is load-bearing for.** Since M111 (`axes_degeneracy_refusal()`)
a fit whose matrix the a-priori floor calls `"ill_conditioned"` computes *only
on the certificate's word*. RR19 §3 measured that band as containing fits
whose corrected SEs are exact to 1e-13; those users get numbers today because
the certificate certified them. If the certificate under-reports, a wrong SE
ships with no warning — the licensing failure D-051 names. IP3 therefore
binds the certificate as a shipped numeric result, and the anchor brackets are
the only place its *level* (ratio ≈ 10 to true error) is checked against
truth. RR21 §3 caution (i) is the concrete stake: with F = 1 the certificate
fails the floor at two of six anchors; F = 10 is what makes it an upper
estimate, and only the brackets know that.

**What each retirement would lose.**
- *Wholly*: the certificate's level is validated by nothing. The closed-form
  oracles pin the dd route at two 2×2 configurations (one where the shipped
  error is zero, one where it is ~1e-12 on one arm); planted perturbations
  pin sensitivity with no truth. Neither reaches the κ 1e4–1e5 geometries
  users actually fit, nor any geometry with an error above the target. Reject.
- *The counterexample-B layer*: B is the only anchor where the certificate
  faces an error *above* `delta_star` — the direction where an under-report
  licenses a wrong number. Every other anchor has true error ≤ 1.1e-8, four
  decades below the target, so a certificate that always said "1e-6" would
  pass all five brackets and refuse nothing. Discrimination is tested
  elsewhere only under `skip_on_cran()` (line 601; the refusal suite). The
  layer is worth keeping, but *reshaped* per Question 1: its CRAN-live job is
  the refusal on every route plus the dd yardstick; the graded bracket is what
  it adds where a value exists.
- *The exact-rational apparatus, keeping a coarser check*: a "coarser check"
  here can only be the dd route against the double route — which is the
  certificate checking itself. The 106-bit hi/lo pairs are what make the dd
  route's own correctness observable (1.4e-18 agreement at B). Reject.

**Why five escalations, then.** None of RB18–RB22 found the certificate's
numbers wrong. Every return was a harness defect of one shape: a fact about
*one machine* frozen into an assertion (a measured error, a bit pattern, a
decade window, and now "always prices"). The lesson at `LESSONS.md:46`
already names the cure; this brief is its last un-swept site. The repair that
ends the series is a rule for the file, stated in its header and enforced at
review: **every assertion is one of (a) a property of a committed matrix or
of the exact oracle, (b) this machine's own measurement bracketed by a
machine-independent bound, or (c) an exhaustive disposition — every
outcome the shipped route can take at that matrix is enumerated and each
branch asserts.** "The shipped route does X here" with no branch for not-X is
a frozen measurement and is disallowed. The cxb block today violates (c);
the M111 graded-route test at `test-axes-certificate-refusal.R:180` violates
it too (Beyond the brief, B1).

**On cost.** Six latent defects, ten milestones and two CRAN rejections is a
real bill, and IP1 says release timing is not an argument — but it cuts both
ways: retiring the only truth-anchored validation of a shipped gate to stop
paying test-maintenance cost is a release-convenience argument, and IP1
refuses it. The honest cost reduction is smaller CRAN exposure (Question 6
and B3), not a smaller oracle.

## 5. Which recorded weaknesses travel with the fix

**(a) The "cxb always prices" premise and the one-anchor-priced repair —
travel, with one guard.** The premise is the failure under review, and the
detector is being rewritten anyway to admit the new disposition. The
detector should require: cxb ∈ {priced, refused-unidentified}, and ≥ 1
anchor priced. The guard: the anchor builders are `cos(u − v)` at octant
differences (`helper-m106-degeneracy.R:30, 57, 71`), so a libm that rounds
`cos(225°)` one ulp differently — CRAN's macOS x86_64, per PR #152 — builds a
different matrix and all five anchors *skip* there. The per-case tests carry
no `skip_on_cran()` (lines 506–524; the ROADMAP row's "the built anchors are
CRAN-skipped" is not what the file says — see B2), so a CRAN-live
"≥ 1 anchor priced" clause could go red on macOS x86_64 and hand CRAN a third
rejection. Before making that clause CRAN-live, read CRAN's 2.0.0 macOS
x86_64 check log for the testthat skip summary: if it lists "does not build
the anchor matrix" five times, the anchor clause must sit under
`skip_on_cran()` (CI covers it on three platforms every push) while the cxb
clause stays CRAN-live. If the log shows the anchors priced there, both
clauses can be CRAN-live. The roadmap's "accepting a red on any math library
that rounds `cos(225°)` differently" is acceptable in CI; on CRAN it is not a
cost the maintainer chooses once but one CRAN re-imposes every submission.

**(b) The unpinned disposition vocabulary — travel.** The fix adds a third
disposition, so the string contract is being edited regardless, and this is
also the brief's own evidence problem ("nothing prints which cases were
actually checked"). Minimal form: a named constant per disposition
(`cert_disp <- c(priced = "priced", refused = "refused -- unidentified", skipped = "skipped")`)
used at every `cert_record()` and read at the detector, plus one assertion in
the detector that every recorded disposition is in that set (a typo becomes
red-with-a-reason, not green-on-nothing). And have the detector *always*
emit the disposition table — `testthat::expect_true(TRUE, info = …)` or a
`cat()` under `testthat::is_testing()` — so the arm64 log shows
`cxb = refused -- unidentified; a4 = priced; …` on a green run. The
`cert_record(id, "priced")`-after-the-`cert_rel()`-calls ordering can be
pinned by moving the record into the `list(...)` construction's last
evaluated position or by asserting inside the detector that every `"priced"`
case has a non-NULL `true_rel` recorded beside it; either is a few lines.

**(c) The six Known-fragilities defects — defer, except two.** Four of the
six are about regeneration hygiene (component-count pinning at three
`cert_rel()` sites, the unaggregated dyadic `cval` bracket, the oracle
driver's silent NULL, the missing planted-perturbation on the quotient's
denominator) and are unreachable by this fix's paths; folding them in widens
a hotfix-class change into a milestone and they are already routed to
"whichever milestone next opens the certificate". Two touch code this fix
edits and should travel: `cert_bracket()`'s at-the-floor branch selected by
`identical(est, cert_floor)` — the fix adds a branch structure to the cxb
block and should not leave a value-coincidence branch beside it (replace with
an explicit `at_floor` argument or a `<= cert_floor` test with the reason
stated); and `cert_rel()` dividing by `hi + lo` where the exact quantity is
zero — the fix adds a dd-vs-exact assertion at cxb that uses the same
division, so guard it once (`stopifnot(hi + lo != 0)` or an explicit
absolute-error branch). `cert_root_rel()` returning NaN at e ≤ −1 is one
line and can ride along if the maintainer prefers; it is not on this fix's
path.

## 6. How firmly to wire in the arm64 harness

**Base image: pin to a digest, and record the tag it was taken from.** The
argument for rolling is that CRAN's runners upgrade; the argument against is
the one the maintainer already hit (a Debian-unstable index drift broke the
first build). A harness that fails to build on the day of a resubmission is
worse than one that lags CRAN by a month. Pin the digest in the harness file,
carry a one-line `refresh` recipe (`docker pull r-base:latest`, record the
new digest, rebuild, re-run the check, commit the digest bump if green), and
make refreshing it a step in `/cairn-release`'s walk rather than an
automatic behaviour. The thing that must track CRAN is OpenBLAS's presence
and R's minor version, not the day's Debian snapshot; the digest note should
record both (`R 4.6.1`, `OpenBLAS 0.3.33`) so a refresh that changes either
is visible.

**Required or advisory: required, for the arm64 flavor specifically, and the
2399-vs-2410 gap does not undermine it.** The gap is eleven assertions in
tests that need `brms`, `OpenMx`, `glmmTMB` or `vdiffr`; all four are
Suggests, all are skipped by `skip_if_not_installed()`, and none of the three
CRAN rejections touched them. What the harness exists to catch is
BLAS/LAPACK- and libm-dependent arithmetic in the package's own numerics,
and every one of those 2399 assertions runs. Record the eleven as the
harness's stated blind spot in its header (which tests, which package), so a
future failure in one of them is not mistaken for harness coverage. Required
means: `/cairn-release` refuses to reach the submission step without a green
arm64 check log on the release tarball, dated, in `cran-comments.md`'s test
environments list — which is also the honest thing to tell CRAN, since the
current text lists five environments and the failing one is not among them.

Two further points. First, CRAN's other exact-arithmetic flavor, macOS
x86_64 (the `cos(225°)` libm difference), is not covered by this harness or
by CI (GitHub's `macos-latest` is arm64); the harness should be recorded as
covering one of the two platform-exact rejection sources, not both.
Second, GitHub-hosted `ubuntu-24.04-arm` runners exist and are free for
public repositories; if `r-lib/actions/setup-r` and Posit's package manager
serve arm64 Linux binaries (verify — I have not), an arm64 CI job would run
this check on every push rather than only at release, and with Debian/Ubuntu's
`libopenblas` it would sample the same BLAS family CRAN does. Consider as
M121's stretch goal or a follow-on; the Docker harness remains the
reproduction of record either way, since CI cannot pin CRAN's exact image.

## Beyond the brief

- **B1. A second site is exposed to the same refusal, under `NOT_CRAN`.**
  `test-axes-certificate-refusal.R:180` ("M111 AC3 (graded route)") asserts
  at B that the warning does *not* carry `"estimated relative error 1;"` —
  i.e. that the graded route, not the sentinel, was taken. On the refusing
  route the sentinel prints exactly that and the test fails. It is
  `skip_on_cran()`, so CRAN and the Docker `R CMD check` both skip it; the
  Docker harness run as `devtools::test()`, or any future arm64 CI job, will
  redden it. It is the same frozen-platform-fact defect as the one under
  review and should be repaired in M122 with the same exhaustive-disposition
  shape (graded → not "1;"; refused → exactly "1;"). The other five fixture
  sites assert `"uncertified"`, `"ill_conditioned"`, or estimates derived
  from the same certificate call, and hold on both routes (verified by
  reading: lines 661 and 734 of the refusal suite; scaled-fit 1674, 1740,
  2197).
- **B2. The ROADMAP's all-skip row says "the built anchors are
  CRAN-skipped".** The five per-case tests at `test-axes-certificate.R:506–524`
  carry no `skip_on_cran()`, and the arm64 CRAN log (FAIL 1, the cxb line
  only) is consistent with all five having run and passed there. Either the
  row is wrong or a mechanism I did not find skips them; the maintainer
  should settle this from a CRAN log before acting on Question 5(a), because
  the two readings imply opposite CRAN exposures for the anchor clause.
- **B3. CRAN-live exposure of this file is a choice worth making
  explicitly.** M120 moved 416 blocks off CRAN's check on time grounds. This
  file keeps the six brackets and the detector CRAN-live, and each is an
  assertion about the machine's arithmetic against a bound. The (A)-shaped
  cxb test is exhaustive over the two routes and so should be safe to keep
  CRAN-live; the anchors are safe wherever they build the matrix bit-for-bit
  and skip where they do not. That is a defensible CRAN posture, but it
  should be recorded as decided (in the file header or a D-entry), because
  the alternative — run the whole bracket suite in CI only and keep on CRAN
  just the refusal-on-every-route claim — is the lower-risk reading of the
  same IP3 obligation and has not been weighed on the record.
- **B4. The dd route's agreement with exact truth is not asserted at any
  anchor.** Lines 116–121 record a deliberate decision not to *gate* on the
  dd route. Nothing in that reasoning forbids *asserting* it, and the two
  closed-form tests already do. Asserting `|dd − exact|/|exact| ≤ 1e-14` at all
  six cases (it holds at 1e-18 to 6e-16 here) is cheap, platform-independent
  (pure R arithmetic), and is the one exact-oracle assertion that survives a
  refusal or a matrix skip. M122 should add it at cxb (Question 1) and can
  add it at the anchors in the same pass — outside the matrix check for
  anchors it cannot run (the exact values describe a matrix this machine did
  not build), so inside it, after the check, before the pricing.
- **B5. `cran-comments.md` should name the arm64 failure honestly.** The
  current text says both 2.0.0 failures are fixed and describes them as
  last-place comparison differences. The third failure is a platform-dependent
  *refusal* in a test, not a comparison, and the resubmission note should say
  what it was and that the test now admits the refusal on stated grounds.
  CRAN's reviewers read these; a second "fixed, resubmitting" note after a
  second rejection is what draws a manual look.

## Recommendations

Tier notes are advisory per the repo's model-tier doctrine. No disagreement
with any constraint: D-048/D-049's constants and D-051/D-053's estimand and
sentinel contract are untouched; no dependency is proposed; the all-skip
lesson is upheld and extended rather than superseded; IP1 is served by
recommending the slower path where it is right (rec 6, rec 9).

1. **Apply — repair the cxb block to the exhaustive-disposition shape
   (Question 1, Question 2(A)).** Matrix mismatch at cxb fails, never skips.
   Two admitted routes: priced (three brackets plus the two `expect_gt`
   against `delta_star`, unchanged) or refused with exactly `"unidentified"`
   from both `v` and `u` (sentinel identical, predicate `"uncertified"`).
   Outside both: kappa, criterion `"ill_conditioned"`,
   `axes_certificate_worst(cert) > delta_star`, and the dd route within 1e-14
   of the committed exact values. Record a distinct disposition. Commit the
   measured `rcond(info)` band beside the case as the stated ground for the
   admission. Tier: Opus for the edit against this report's numbers; Fable
   review, because the test decides what a shipped gate is allowed to do at
   its one committed worst case.
2. **Apply — repair `test-axes-certificate-refusal.R:180` in the same
   change (B1).** Same shape: graded route asserts the warning is not
   `"… 1;"`; refused route asserts it is. Tier: Opus.
3. **Apply — pin the disposition vocabulary and print it (Question 5b).**
   Named constants, a known-set assertion in the detector, the disposition
   table emitted on every run. Tier: Sonnet.
4. **Apply — the detector requires cxb ∈ {priced, refused} always, and
   ≥ 1 anchor priced under a CRAN-exposure decision made from a CRAN log
   (Question 5a, B2).** If CRAN macOS x86_64 skips the anchors, the anchor
   clause sits under `skip_on_cran()`; otherwise both clauses are CRAN-live.
   Tier: Sonnet for the edit; the log read is the maintainer's.
5. **Apply — fold in the two Known-fragilities items on this fix's path
   (Question 5c):** `cert_bracket()`'s value-coincidence floor branch, and
   `cert_rel()`'s zero-denominator. Defer the other four to the milestone
   the ROADMAP already routes them to. Tier: Opus.
6. **Apply — verify the fix in the arm64 container before resubmission, and
   list the arm64 environment in `cran-comments.md` with an honest account
   of the third failure (B5, Question 6).** Tier: Sonnet.
7. **Apply — M121 pins the image digest, records R/OpenBLAS versions and
   the eleven-assertion blind spot in its header, and `/cairn-release`
   requires a dated green arm64 log (Question 6).** Tier: Sonnet.
8. **Apply — add the file-header rule (Question 4):** every assertion is a
   matrix/oracle property, a bracketed own-measurement, or an exhaustive
   disposition; and extend `LESSONS.md:46` with one clause — "a refusal is a
   platform fact too: enumerate every outcome the shipped route can take at
   the committed matrix". Tier: Sonnet.
9. **Consider — C′: `tol = 0` in `axes_pricing_core()`'s `solve(info)`,
   making the certificate the sole conditioning judge (Question 2).** Its own
   plan gate and D-entry; oracle is a sweep over the admitted domain and the
   M89/M106 near-threshold families showing the certificate refuses every
   matrix the default gate did. Not for 2.0.1. Tier: Fable at design, because
   it changes when the shipped package emits a number.
10. **Consider — an `ubuntu-24.04-arm` CI job (Question 6)** after verifying
    `setup-r` and binary availability on arm64 Linux. Tier: Sonnet.
11. **Consider — dd-vs-exact assertions at the five anchors (B4)** and a
    documentation sentence that the certificate's estimate is a property of
    the fit on this machine (Question 3). Tier: Sonnet.
12. **Consider — record the CRAN-live posture of this file as a decision
    (B3).** Tier: Sonnet.
13. **Reject — (B), an explicit `solve()` tolerance chosen to stabilize B.**
    Measured: no value inside the band [1.67e-16, 3.35e-16] is
    platform-stable, and every value below it removes the gate rather than
    tuning it (0 of 300 refusals at 1e-16 and below).
14. **Reject — retiring the certificate's validation layer, wholly or the
    exact-rational apparatus (Question 4).** It is the only truth-anchored
    check on a gate that decides, since M111, whether users receive corrected
    SEs on ill-conditioned fits; F = 10 is unvalidated without it.
15. **Reject — removing counterexample B from the bracket suite.** It is the
    one anchor where the certificate faces an error above the target, i.e.
    the under-report direction; the fix reshapes its job rather than dropping
    it.
