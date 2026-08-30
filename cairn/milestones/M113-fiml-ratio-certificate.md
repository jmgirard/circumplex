# M113: Certify the ratio the reported FIML standard error is multiplied by

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3, GP2
- **Branch/PR:** `m113-fiml-ratio-certificate` / https://github.com/jmgirard/circumplex/pull/144

## Goal

Extend the per-fit accuracy certificate to `fiml_ratio`, so every quantity a
reported standard error is composed from is priced by the check whose estimate
the refusal warning names.

## Scope

Surface tier: **user-facing** — the certificate gates a refusal the user sees,
and its estimate is printed in that refusal's warning.

**In:** a third certificate field estimating the committed relative error of
`fiml_ratio = corrected / naive` at `cov2cor(Σ̂)` (both arms are already
computed by the `axes_v_pricing()` call the certificate makes and one is
discarded, `R/axes_certificate.R:412`); `axes_degeneracy_refusal()` taking its
max over three fields; the exact-rational oracle extended to bracket the new
field; a second independent oracle type for it (IP3); the false
"never user-reported" claim at `R/axes_certificate.R:343` corrected without
disturbing the true raw-arm claim at `R/axes_corrected_se.R:757`; a `tryCatch`
at the bare certificate call in `axes_degeneracy_refusal()`; and a recorded
characterisation of the naive-only non-finiteness divergence between the two
surfaces (M111 review F3).

**Out:** the shared refusal predicate's straddle fence and the nestedness
literal set → M114. The packaged bracket's platform reach → M115. Surfacing
the estimate on computed fits (RR21 rec 5), the certificate's double
evaluation per checked fit, and the M108/M111 cosmetic residue → the ROADMAP
degeneracy candidate row.

## Acceptance criteria

- [x] AC1 `axes_accuracy_certificate()` returns a third element estimating the
      committed relative error of `fiml_ratio`, computed from the same
      double-double replay as the other two and finite and non-negative on
      every input — its sentinel gaining a third element, so no route failure
      leaves the field `NaN` for `axes_degeneracy_refusal()`'s comparison to
      raise on. `axes_degeneracy_refusal()` refuses `"uncertified"` on the max
      over all three fields against `axes_degeneracy_delta_star`, and
      `axes_degeneracy_note()` prints that same max as the warning's estimated
      relative error, so no fit is refused against a target its warning reports
      it inside.
- [x] AC2 At every geometry `devel/degeneracy-oracle/exact_oracle.R`'s
      certificate case list holds, the new field is at least the oracle's
      measured relative error of `fiml_ratio` there and at most a ceiling of
      its own, measured once on a clean tree and thereafter frozen as a
      constant no plant run re-measures; `CERT_CEILING`, which bounds the `se`
      and `cval` lines, is unchanged. The script emits eighteen certificate
      lines — six geometries by three fields — and exits non-zero if it emits
      fewer, if any ratio falls below 1, or if any exceeds its own field's
      ceiling.
- [x] AC3 The new field is validated against a second independent oracle type,
      recorded at the asserting test, sharing no code, no library and no
      pipeline with `exact_oracle.py` or with the route under test (IP3) — the
      independence M108 already records at
      `tests/testthat/test-axes-certificate.R:29-30`. That type pins the
      replayed ratio against a hand-derived exact value at a configuration
      whose shipped double route carries a NONZERO committed error, so the
      field is validated where there is an error to catch and not only at its
      floor. `(RB tripwire: ip-touching)`
- [x] AC4 Three planted defects, run one at a time against the frozen
      ceilings, vary magnitude, metric and route rather than one axis three
      times, and each is caught by a check present on the clean tree before
      the plant is applied. The new field's safety factor dropped from 10 to 1
      (an under-report against the floor), and the quotient's denominator
      replayed one non-identity diagonal congruence away from the priced
      matrix (the D-044 metric split, expressed as a congruence because the
      certificate is handed only `cov2cor(Σ̂)` and never the raw `Σ̂`), each
      redden the new field's own ratio line at a named geometry in
      `exact_oracle.R`. The double-double replay of the naive arm collapsed
      onto the shipped double route — the maximal form of that defect, the
      only form this criterion claims — reddens
      `expect_gte(cert$fiml_ratio, true_rel)` in the closed-form oracle test
      at `tests/testthat/test-axes-certificate.R`, while every
      `exact_oracle.R` ratio line stays inside its window at all six
      geometries.
- [x] AC5 The comment sites `R/axes_certificate.R:343`,
      `R/axes_corrected_se.R:311`, `:359`, `:757` and
      `R/axes_reliability.R:1989` each state the arm relationship correctly:
      none claims the certificate omits an arm because that arm is never
      user-reported, except where the arm it names is the raw-`Σ̂` one.
- [x] AC6 Two conditions raised inside `axes_accuracy_certificate()`, run one
      at a time — a `stop()` and a non-error route failure — each reach
      `axes_corrected_se()` and `axes_scaling_factor()` as
      `reason = "uncertified"` with exactly one warning and no error.
- [x] AC7 `devtools::test()` clean; `devtools::document()` no diff and no
      unresolved-link warning at pinned `cli.width`;
      `devtools::check(args = "--no-manual")` 0 errors / 0 warnings / 0 notes.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T3
- AC3 → T4
- AC4 → T5
- AC5 → T6
- AC6 → T7
- AC7 → T9

## Tasks

- [x] T1 Measure the cancellation: replay `corrected`, `naive` and their ratio
      in double-double at each of the oracle's certificate cases and record the
      ratio's relative error against each arm's. Settles whether the ratio needs
      its own replay or is bounded by the arms.
- [x] T2 Add the `fiml_ratio` field to `axes_dd_pricing()` and
      `axes_accuracy_certificate()` (`R/axes_certificate.R`), with its own
      denominator guard and a third sentinel element; extend
      `axes_degeneracy_refusal()`'s max to three fields
      (`R/axes_corrected_se.R:765`) and `axes_degeneracy_note()`'s printed
      estimate to the same max (`R/axes_corrected_se.R:786-793`).
- [x] T3 Extend `exact_oracle.py` / `exact_oracle.R` to emit and bracket the
      ratio's true relative error; measure the new field's ceiling once on a
      clean tree and freeze it as its own constant beside `CERT_CEILING`.
- [x] T4 Construct a dyadic-rational configuration ill-conditioned enough
      that the shipped double route commits measurable error on `fiml_ratio`
      while the exact ratio stays hand-derivable; derive it, commit it as
      literal fractions at the asserting test, and record the oracle type
      there. `(RB tripwire: ip-touching)`
- [x] T5 Plant the three AC4 defects one at a time against the frozen
      ceilings; record per-defect results naming the check that reddened and
      its site, revert each and verify the tree clean.
- [x] T6 Correct the five AC5 comment sites; sweep `man/`, `NEWS.md`,
      `vignettes/` and `tests/` for paraphrases of the same claim.
- [x] T7 `tryCatch` the certificate call at `R/axes_corrected_se.R:764`,
      returning the sentinel; test both condition routes.
- [x] T8 Characterise the naive-only non-finiteness divergence (M111 review
      F3): exhibit an admitted matrix whose naive row alone is non-finite at
      `R/axes_corrected_se.R:239`, or record that none was found. No behaviour
      change here — a change to which literal a user sees goes to a D-entry.
- [x] T9 NEWS entry for the widened refusal basis; profile verify and
      consistency-gate slot.

## Work log

- 2026-08-30: created by /milestone-plan.
- 2026-08-30: criteria audit ran in FULL mode (declared user-facing tier), one fresh-context [O] reader that authored none of the criteria, jointly over M113, M114 and M115; it returned eleven findings. Nine with one clear right answer were fixed before writing and reported in chat: an unbounded "anywhere it is false" grep promise narrowed to five enumerated comment sites (AC5); an ambiguous ratio count pinned to `CERT_EXPECTED` (AC2); the `[1, 1e3]` window made a measured output rather than imported before measurement (AC2); recording, reversion and tree-clean clauses moved out of criteria into task procedure; a magnitude plant added beside the location plant (AC4) and a non-`stop()` route added beside the `stop()` plant (AC6); and the gate command pinned to `check(args = "--no-manual")`. Two findings became gate questions.
- 2026-08-30: plan gate chose certifying `fiml_ratio` in its own field over RR21 B4's max over the two arms' standalone errors, because the arms' errors partially cancel in the ratio and B4's bound would refuse fits whose reported SE is accurate — the false-refusal failure D-048/D-049's history is about; falsified by a geometry at which the ratio's measured relative error exceeds both arms'.
- 2026-08-30: plan gate chose demoting M111 review F3 to a recorded characterisation (T8) over an acceptance criterion requiring the two surfaces to agree, because agreement is reachable only by the SE surface reporting a non-finite `fiml_ratio` on the FIML path, against GP2's fail-closed clause; falsified by an admitted matrix whose naive row alone is non-finite and whose reported FIML SE is nonetheless finite.
- 2026-08-30: plan gate chose three milestones over folding M114's predicate fences into this one, because the straddle fixture may not be constructible and that uncertainty should not ride in the same PR as the estimand change; falsified by the fixture proving trivial to construct.
- 2026-08-30: the criteria the gate changed (AC1, AC2, AC3, AC4) went back through the audit's questions in FULL mode, to a second fresh-context [O] reader that authored none of them; it returned nine findings. Eight were fixed here: the AC4 location plant could not redden, because under the plan gate's own cancellation hypothesis substituting an arm's error OVER-estimates and AC2 let the ceiling be re-measured — the ceiling is now frozen and the plant set replaced by three varying magnitude, metric and route; `axes_degeneracy_note()` still printed `max(se, cval)`, so a fit refused on the ratio field would have warned a number below the target it was refused against; AC1 permitted a non-finite third field, whose `NaN` would raise in `axes_degeneracy_refusal()`'s comparison OUTSIDE T7's tryCatch, against GP2; AC3's "no code and no pipeline" was unsatisfiable as worded and is narrowed to M108's own phrasing; AC2's count was pinned to `CERT_EXPECTED`, a constant the implementer sets after the fact, and to `cert_n`, which counts lines rather than ratios — now eighteen lines stated in the criterion; the single shared `CERT_CEILING` could have been loosened for the two already-audited fields and is now per-field; and AC4's redden is tied to the new field's own line at a named geometry. The ninth (F8 — AC3 was satisfiable by extending the zero-committed-error dyadic oracle, which would have validated the new field only where there is no error to catch) went to the user gate and was closed by requiring a nonzero committed error; see the gate line below.

- 2026-08-30: plan gate chose requiring AC3's second oracle type to pin the ratio at a configuration carrying a NONZERO committed error over extending the existing dyadic closed-form oracle, whose configuration is priced exactly and where the certificate is only asserted to report its floor, because a second type that never meets an error validates IP3 in letter and not in substance for a number printed to users; falsified by no dyadic-rational configuration existing that is ill-conditioned enough to commit measurable error and still hand-derivable, which would force the weaker claim at a mini gate.
- 2026-08-30: T1 measured, ahead of the question gate because the gate rests on it: at the six geometries `exact_oracle.R` prices, the quotient's committed relative error exceeds BOTH arms' own at four of them (worst factor 1.85), and falls 76x below their sum only at counterexample B. D-053's first reopening condition is therefore met; the disposition is M113-D1 below.
- 2026-08-30: T2 — the certificate returns a third field, `fiml_ratio`, priced pre-square-root on the quotient of the shipped arms against the quotient of the replayed arms (n cancels, as for `se`); `axes_dd_pricing()` now replays the naive arm at the same matrix; the refusal predicate and the warning's printed estimate both read one new helper, `axes_certificate_worst()`, so they cannot drift apart again. Suite: 0 failures, 8789 passing.
- 2026-08-30: T3 — the exact-rational oracle emits the quotient per component and the driver brackets the new field at all six geometries; the run prints eighteen certificate lines and passes, the quotient's ratios measuring 9.97 to 10.00, so its own ceiling is frozen at 1e3 (`CERT_CEILING_RATIO`), the window the other two already carry. The six measured errors are frozen at the anchors in `tests/testthat/test-axes-certificate.R`.
- 2026-08-30: T4 — the construction the plan gate's falsifier was aimed at exists. At `S = [[1, 3/8], [3/8, 147469/2^20]]` with the single derivative matrix `[[0,0],[0,1]]` (kappa 1.0e5, admitted), the hand-derived exact arm variances are `65784995034489/2^51` and `169/2^39`, both exact doubles; the replay lands on both, while the shipped route is wrong by 1.1e-12 on the quotient and by less than a floor's worth on the corrected arm and `cval` — so the new field alone carries the estimate there. Derivation and independence recorded at the asserting test in `tests/testthat/test-axes-certificate.R`. No weaker claim and no mini gate needed.
- 2026-08-30: T5 — the three plants run one at a time, each reverted and the tree verified clean after it. (i) The new field's safety factor 10 → 1: `exact_oracle.R` exits 1, the counterexample-B ratio line falling to 0.9995 while every SE and cval line stays at 9.83–10. (ii) The quotient's denominator replayed at `diag(sqrt(1..p)) %*% sigma %*% diag(sqrt(1..p))`: exits 1, all six ratio lines above the 1e3 ceiling (5.3e4 to 1.5e15) with the SE and cval lines untouched. (iii) The naive arm's replay collapsed onto the shipped double route: `exact_oracle.R` exits 0 with all six ratio lines inside their window — measured, in three forms (low word dropped, last matrix product in plain double, full collapse) — while the collapse reddens three assertions in `tests/testthat/test-axes-certificate.R`, `expect_gte(cert$fiml_ratio, true_rel)` among them.
- 2026-08-30: amendment (substantive, mini gate): AC4 rewritten. Two of its three plants do not redden the check it named — the raw-`Σ̂` replay is not writable inside a certificate handed only `cov2cor(Σ̂)`, and the route collapse is invisible to all six `exact_oracle.R` geometries because the corrected arm's error sits within an order of magnitude of the quotient's there. The amended wording pins each plant to the check that does catch it, requires that check to exist on the clean tree before the plant, excludes the identity congruence, and claims only the maximal form of the route defect. It went to a fresh-context [O] reader that authored none of it, running the criteria audit's questions in FULL mode (the milestone's declared tier); the reader returned six findings and the amended text is its recommended narrower repair, which fixed all six — the unpinned detector, the prose-satisfiable escape clause, the recording clause the first audit had already moved to T5, the unbounded causal claim, the identity congruence, and the unclaimed partial-precision forms. The gate adopted it.
- 2026-08-30: T7 — the certificate call in `axes_degeneracy_refusal()` is fenced on `error` only (a warning raised inside it must still reach the user), returning the sentinel. Both condition routes tested at both surfaces: a `stop()` and a self-test failure each give `reason = "uncertified"` with exactly one warning and no error at `axes_corrected_se()` and at `axes_scaling_factor()`. The fence is shown load-bearing: removed, the `stop()` route propagates out as `Error: planted condition from inside the certificate`.
- 2026-08-30: T6 — the five comment sites now each name the arm they mean. The two the change made stale were fixed with the code (the certificate's replay scope, and the refusal helper's raw-arm note); the three remaining were narrowed to say raw-metric where the distinction is now load-bearing, and the site inside `axes_v_pricing()` gained the clause that the cov2cor call's own naive arm is what `fiml_ratio` divides by. The sweep of `man/`, `NEWS.md`, `vignettes/` and `tests/` for paraphrases found two, both in tests, both already naming the raw arm correctly. The comment growth pushed the Wc fold out of the range `R/axes_scaled_fit.R` cites, which reddened the M69 citation guard; the citation is repointed to 202-214 (span 12, under its 15-line cap).
- 2026-08-30: T8 — no matrix whose naive arm alone is non-finite was found in 12,127 candidates; recorded as M113-D2 below, with what M113 changes about the hazard's reach.
- 2026-08-30: T9 — the NEWS entry for the check now names all three quantities it prices and says the refusal reads the worst of them, so a fit can be refused on the FIML ratio while its standard errors alone would have passed; the entry is amended rather than added, the check itself being unreleased. Gate slot: `devtools::document()` no diff and zero `resolve link` lines at `cli.width = 500`; `devtools::test()` 0 failures / 8822 passing / 1 skip (a fixture-version skip) / 5 warnings (all lavaan's own, pre-existing); `devtools::check(args = "--no-manual")` Status OK, 0 errors / 0 warnings / 0 notes.
- 2026-08-30: all tasks complete; status → review.
- 2026-08-30: the T1 finding is recorded at project level as D-054, annotating D-053's Rejected clause; the gate chose that over leaving the correction milestone-local.
- 2026-08-30: review opened; branch pushed and draft PR #144 created. Evidence gathering in progress.
- 2026-08-30: all seven criteria executed with fresh evidence and ticked; consistency gate clean; three fresh-context lenses ran, the [O] diff-bug lens returning ten ranked findings.
- 2026-08-30: gate triage — findings 1, 2 rejected as defects and filed; 4, 5, 7, 9 fixed on the branch; 3, 6, 8, 10 filed as follow-ups on the ROADMAP degeneracy candidate row. Suite, check and oracle re-run green after the fixes.
- 2026-08-30: first CI run red on windows-latest — the AC3 closed-form test's shipped-error half had nothing to bracket there (true_rel 0 against 5.6e-13 locally). Fixed at the gate on the maintainer's choice: the shipped-error half now skips with a stated reason where the shipped route is exact, the replay half staying unconditional.

## Decisions

### M113-D1 (2026-08-30): the quotient's committed error is bounded by neither arm's own, which settles that it needs its own replay and removes the "conservative" reading of the rejected alternative

**Measurement (T1).** `v_corrected`, `v_naive` and their quotient replayed in
double-double at the six geometries `devel/degeneracy-oracle/exact_oracle.R`
prices, each read against the shipped double route. Maxima over fitted
components, relative errors of the pre-square-root quantities:

| case | e(v_corrected) | e(v_naive) | e(quotient) | e(v_corrected)+e(v_naive) |
|---|---|---|---|---|
| a4 (family A, kappa 1e4) | 1.174e-13 | 8.378e-14 | 1.194e-13 | 2.012e-13 |
| a5 (family A, kappa 1e5) | 6.008e-12 | 2.131e-12 | 3.877e-12 | 8.139e-12 |
| c4 (family C, p = 4) | 1.292e-12 | 1.388e-12 | 1.136e-12 | 2.680e-12 |
| b9a (near-duplicate .9999) | 1.260e-12 | 8.785e-13 | 2.139e-12 | 2.139e-12 |
| b9b (near-duplicate .99999) | 2.253e-11 | 2.090e-11 | 1.336e-11 | 4.343e-11 |
| cxb (counterexample B) | 6.710e-02 | 6.547e-02 | 1.742e-03 | 1.326e-01 |

Per fitted component, the quotient's error exceeds both arms' own at a4, c4,
b9a and b9b -- worst factor 1.85 (b9b, the `zeta1` component: 1.336e-11 against
7.216e-12 and 6.148e-12). Reproducible from the tree once T2 lands: the replay is
`axes_dd_pricing()`'s and the shipped values are `axes_v_pricing()`'s, read at
the case builders in `tests/testthat/helper-m106-degeneracy.R`.

**What it settles.** The quotient is not bounded by either arm, so it cannot be
certified by reading an arm's field -- the plan's T1 question is answered in
favour of its own replay, which is what AC1 requires.

**What it corrects.** D-053 rejected RR21 B4 (the max over the two arms'
standalone errors) as an over-price that would refuse accurate fits, and named
"a geometry at which the measured relative error of `fiml_ratio` exceeds both
arms' own" as the finding that would put B4's max back in contention. Four of
six geometries are that finding, and they point the other way: a max over the
arms UNDER-reports the quotient's error by up to 1.85x, so B4 is not the
conservative option D-053 called it. The two-sided cancellation claim holds
only at counterexample B, where the arms' errors are four decades larger than
anywhere reachable. D-053's decision -- price the quotient itself -- is
unchanged and now rests on the stronger ground that no arm-wise bound is safe.

### M113-D2 (2026-08-30): the naive-only non-finiteness divergence is left uncorrected, and no matrix exhibiting it was found

**What was looked for (T8).** `axes_se_pricing()` returns `"indefinite"` when
either arm's quadratic form is nonpositive, so a matrix whose NAIVE row alone
is nonpositive at `cov2cor(Σ̂)` would refuse the SE surface while the scaling
surface, which never prices that arm, computed — the split the M111 review's
F3 names.

**Search.** 12,127 matrices: the three `m106_*` builders swept across nine
decades of their conditioning parameter; planted spectra at
`lambda_min` in {0, ±0.5, ±1, ±2} × `sqrt(p*eps)` at p = 4, 8, 12, 24; and
12,000 random correlation matrices at p = 4, 8, 12 with eigenvalues
log-uniform over nine decades. The predicate: the naive forms nonpositive or
non-finite while every corrected form is finite and positive. **None was
found**, at any conditioning, admitted or refused. That is consistent with the
standing record at `R/axes_corrected_se.R` — the guard has never fired, and an
earlier 3,822-matrix indefinite sweep did not reach it either.

**Disposition.** No behaviour change, as T8 states. What M113 changes is the
hazard's reach rather than its remedy: the certificate now returns its
sentinel on a nonpositive or non-finite naive arm at the priced matrix, and
the sentinel refuses at both surfaces together — so inside the criterion's
ill-conditioned band, where the certificate is consulted, the two surfaces can
no longer split on this. Outside that band the divergence stays reachable in
principle and unexhibited in fact.

## Review

PR: https://github.com/jmgirard/circumplex/pull/144 (draft opened 2026-08-30).
Branch 9 ahead / 0 behind `origin/master` at review start; no merge needed.

### Acceptance-criteria evidence (fresh, 2026-08-30)

- **AC1** — `axes_accuracy_certificate()` returns `fiml_ratio` from the same
  `axes_dd_pricing()` replay as the other two (`ref$v / ref$v_naive`,
  `R/axes_certificate.R`), and its sentinel carries three elements at every
  early return. The finiteness/non-negativity assertion runs over the admitted
  domain in "AC1: the estimate is finite and non-negative across the admitted
  domain". `axes_certificate_worst()` is the single definition read by both
  `axes_degeneracy_refusal()` and `axes_degeneracy_note()`; the test "AC1: the
  refusal predicate and its warning both read the quotient" drives a
  certificate whose first two fields sit inside the target and whose third sits
  outside, gets `reason = "uncertified"`, and matches the warning text
  `estimated relative error 0.01`. Suite green (below).
- **AC2** — `Rscript devel/degeneracy-oracle/exact_oracle.R` re-run on a clean
  tree: exit 0, `CERTIFICATE (18 of 18 lines checked ...): PASS`, ratio lines
  9.83 to 10.00 at the six geometries. `CERT_CEILING` is unchanged at 1e3;
  `CERT_CEILING_RATIO` is its own constant, frozen, and no plant run
  re-measures it. The non-zero exits are exercised, not asserted: plant (i)
  drove a ratio line below 1 and plant (ii) drove six above the ceiling, each
  exiting 1 (AC4 below).
- **AC3** — second oracle type at `tests/testthat/test-axes-certificate.R`,
  test "the quotient's replay lands on hand-derived exact values where the
  shipped route is WRONG (closed-form oracle)": the configuration
  `S = [[1, 3/8], [3/8, 147469/2^20]]` with `M = [[0,0],[0,1]]`, derived by
  hand and committed as literal fractions, sharing no code, library or pipeline
  with `exact_oracle.py` or the route under test — the independence M108
  records at that file's header. The committed error is nonzero and asserted so
  (`expect_gt(true_rel, 2 * .Machine$double.eps)`; measured ~5.6e-13), and
  `se` and `cval` both sit at their floor there, so the estimate for that fit
  comes from the new field alone. Passed in the green run.
- **AC4** — the three plants re-run one at a time on this tree, each reverted
  and the tree verified clean after it (`git status` empty, certificate tests
  147 passing again at the end). (i) Safety factor 10 -> 1 on the new field:
  `exact_oracle.R` exit 1, the counterexample-B ratio line falling under 1
  while every SE and cval line stayed at 9.83-10. (ii) The quotient's
  denominator replayed at the non-identity congruence
  `diag(sqrt(1..p)) %*% sigma %*% diag(sqrt(1..p))`: exit 1, all six ratio
  lines above the 1e3 ceiling (5.3e4 to 1.5e15), SE and cval lines untouched.
  (iii) The naive arm's replay collapsed onto the shipped double route in its
  maximal form (`ref$v_naive <- dd_of(vn_hat)`): `exact_oracle.R` exit 0 with
  all six ratio lines inside their window, while
  `expect_gte(cert$fiml_ratio, true_rel)` at
  `tests/testthat/test-axes-certificate.R:671` reddened (4e-15 against a true
  5.6e-13), as did the discrimination assertion at `:681`. All three checks
  existed on the clean tree and were seen green there before each plant.
- **AC5** — the five sites read directly: `R/axes_certificate.R` (the replay
  scope) names the RAW Sigma-hat arm; `R/axes_corrected_se.R:311` names
  cov2cor as the matrix every user-reported number depends on;
  `R/axes_corrected_se.R:362` and `:763` and `R/axes_reliability.R:1990` each
  name the raw-metric arm explicitly. None claims the certificate omits an arm
  because that arm is never user-reported except where the arm named is the
  raw one. A `grep` over `R/`, `man/`, `NEWS.md`, `vignettes/` and `tests/`
  for the claim found two further paraphrases, both in tests, both already
  naming the raw arm.
- **AC6** — test "AC6: a condition inside the certificate refuses at both
  surfaces, never errors" runs a `stop()` and a non-error route failure one at
  a time, each asserting `reason = "uncertified"`, `expect_length(w, 1L)` and
  no error at both `axes_corrected_se()` and `axes_scaling_factor()`. Passed in
  the green run.
- **AC7** — `devtools::test()`: FAIL 0 / WARN 5 / SKIP 1 / PASS 8822 (the five
  warnings are lavaan's own and pre-existing; the skip is the fixture-version
  guard). `options(cli.width = 500); devtools::document()`: exit 0, no working-
  tree diff, zero lines matching `resolve link`.
  `devtools::check(args = "--no-manual")`: Status OK, 0 errors / 0 warnings /
  0 notes (11m 38s).

### Consistency gate

Universal: `cairn_validate.py` exit 0, all checks pass (47 advisory work-log
warnings, all pre-existing in M7). `cairn_impact.py` skipped — no DESIGN.md
principle changed. Toolchain (`r-package` `consistency-gate` slot):
`document()` no diff and no unresolved-link warning; no generated file
hand-edited; README.md not stale against README.Rmd (neither touched);
`pkgdown::check_pkgdown()` clean; NEWS.md entry present (amended, the check
being unreleased); no new top-level files; `check()` clean; master watches
`R-CMD-check.yaml` and `test-coverage.yaml` both `success` on the newest push
run of `master`; `tools/check-master-red-alert.R`,
`tools/master-red-alert-dryrun.R` and `tools/check-branch-protection.R` each
exit 0.

### Independent review — three fresh-context lenses

Executable surface touched, so the full three-lens fan-out ran.

**[S] blame-history** — no findings. Verified that M89's nestedness contract,
M91's decoupled-refusal contract, M108/D-051's scope claim and M111's shared
predicate are extended rather than undone, and that D-054 annotates D-053
rather than editing it. One cosmetic note: a stray line-wrap in `NEWS.md`
leaves an orphaned one-word line.

**[S] prior-PR-comments** — no findings. The
`gh api repos/jmgirard/circumplex/pulls/comments` probe returned empty, so the
PR-thread walk was skipped and archived `## Review` sections were the surface
(M89, M90, M91, M106, M108, M110, M111, plus RR21). Nothing in the diff walks
back a recorded disposition.

**[O] diff-bug** — ten findings, ranked; listed with disposition below. The
reviewer independently re-ran `exact_oracle.R` (18/18, exit 0) and the
certificate tests (all pass, no skips) before reporting.

#### [O] diff-bug findings, as reported, with the gate's disposition

1. **The widened predicate refuses fits on a field inert for three of the four
   reported-quantity paths.** `fiml_ratio` reaches a user only at
   `R/axes_reliability.R:1827`, i.e. only when `missing == "fiml"`; on listwise
   and cormat fits, and for all four scaled statistics on every path, it is
   computed and discarded. An ill-conditioned listwise fit with `se = 4e-5`,
   `cval = 2e-5`, `fiml_ratio = 1.5e-4` now returns every corrected SE and all
   four scaled statistics as `NA`/`"uncertified"` although every number it
   would have reported carries at most 4e-5. Reachable: the reviewer measured
   `fiml_ratio/se` from 0.46 to 2.1 over the `m106` families, and this branch's
   own closed-form test exhibits `se` at its floor with `fiml_ratio` 2500x
   higher.
2. **The refusal warning attributes the printed estimate to the corrected
   standard errors.** `R/axes_corrected_se.R:270-276` composes "The corrected
   component standard errors could not be computed (uncertified: estimated
   relative error ...)", filled from the three-field max, so a user can read
   the FIML ratio's number as the SEs' accuracy. Same text is reached from
   `axes_scaling_factor()`, where the ratio is not a term in anything computed.
3. **`axes_certificate_worst()` drops a missing field silently and fails OPEN
   on an empty certificate.** `axes_certificate_worst(list(se, cval))` returns
   the two-field max with no error; `axes_certificate_worst(NULL)` returns
   `-Inf` with a warning, which certifies the fit (against GP2) and emits a
   second warning. Unreachable from shipped code today -- both producers return
   three fields -- so this is a latent gap in the helper that exists to make
   the field set impossible to get wrong.
4. **The `tryCatch` handler duplicates the sentinel literal.**
   `R/axes_corrected_se.R:788` writes `list(se = 1, cval = 1, fiml_ratio = 1)`
   rather than calling the certificate's own sentinel, so a fourth field would
   drift between the two -- the drift `axes_certificate_worst()` was added
   thirteen lines below to prevent.
5. **The bit-identity precondition does not pin the shipped naive arm the new
   anchors depend on.** `tests/testthat/test-axes-certificate.R:236` compares
   `c(cert_hex(v$corrected), cert_hex(u))` against the frozen hex; the frozen
   `ratio` anchors are a function of `axes_v_pricing()$naive` too. A platform
   reproducing `corrected` and `u` bit for bit but not `naive` does not skip,
   and `expect_gte(cert$fiml_ratio, cs$ratio)` is then evaluated against
   another machine's yardstick.
6. **The planted-perturbation sensitivity layer was not extended to the third
   field.** `tests/testthat/test-axes-certificate.R:472-511` perturbs only
   `out$corrected` and `u` and asserts only `cert$se` and `cert$cval`; nothing
   plants `out$naive` -- the new field's denominator -- and nothing asserts
   `cert$fiml_ratio` responds.
7. **The "this is a real error" guard uses a threshold that is not the floor it
   names.** `tests/testthat/test-axes-certificate.R:665` tests
   `true_rel > 2 * eps`, while the floor that would swallow it bites until
   `delta_q > 4 * eps`. Harmless as committed (`true_rel` ~5.6e-13) and the
   neighbouring `expect_gt(cert$fiml_ratio, floor_est)` does pin it correctly.
8. **`CERT_CEILING_RATIO` was set to the inherited 1e3 rather than to the
   measurement.** Every measured ratio line is 9.97-10.00, so two decades of
   slack means a planted defect inflating the field up to 50x still passes
   every ratio line; T5's plant (ii) only reddened because it overshot by 5e4x.
9. **The exported help still describes a single estimate.**
   `R/axes_reliability.R:722-729` says the check "estimates the relative error
   the numbers it produced actually carry"; `NEWS.md` was widened to name all
   three quantities and to say a fit can be refused on the ratio alone, and the
   roxygen was not -- so no user-facing help tells a non-FIML user their fit
   can be refused on a ratio their fit never reports (finding 1).
10. **The oracle's `ratio_rel()` measures an n-dependent quantity against an
    n-free exact value.** `devel/degeneracy-oracle/exact_oracle.R:174-181`
    forms `pr$corrected / pr$naive` at `n = 600` (as the shipped route does),
    while `exact_oracle.py` computes `sqrt(v_c/v_n)` n-free and the certificate
    is n-free by construction; a future geometry whose true error lands near
    1 ulp would have its anchor set by the choice of `n`.

One further cosmetic note from the [S] blame-history lens: `NEWS.md` carries a
stray line-wrap leaving an orphaned one-word line ("reasons").

#### Dispositions (maintainer, at the gate, 2026-08-30)

- **1 and 2 — rejected as defects, filed as a follow-up.** The max over three
  fields is what AC1 requires and what the M111 gate chose; the direction is
  the fail-closed one (GP2). In reachable geometry the widening is bounded:
  the reviewer measured `fiml_ratio/se` between 0.46 and 2.1 over the `m106`
  families, so few additional fits are refused, and the 2500x separation cited
  is the contrived dyadic test configuration. Both are handed to the ROADMAP
  degeneracy candidate row, whose promotion condition names M114 (already
  planned, `Depends on: M113`, and scoped to this predicate).
- **4, 5, 7, 9 — fixed on this branch** (see below).
- **3, 6, 8, 10 — filed as follow-ups** on the same candidate row: 3 is
  unreachable from shipped code today and belongs with M114's predicate work;
  6 is real test work; 8 wants the ratio ceiling tightened toward the measured
  9.97-10.00 and belongs with M115, which opens the bracket; 10's direction is
  conservative (the oracle measures the n-dependent quantity the user actually
  gets, which the n-free certificate must cover) and it is sub-ulp at every
  measured geometry.
- The **[S] cosmetic NEWS line-wrap** was fixed with the F9 documentation pass,
  the two being the same user-facing text.

#### Fix-now work, and its re-verification

- **F4** — `axes_certificate_sentinel()` added in `R/axes_certificate.R` as the
  one definition; `axes_accuracy_certificate()` and the `tryCatch` handler in
  `axes_degeneracy_refusal()` both call it, so the field set cannot drift
  across the fence.
- **F5** — `cert_frozen` gained a `naive` element at all six cases, and
  `cert_skip_unless_reproduced()` pins `cert_hex(v$naive)` against it. Measured
  on this machine, which reproduces the existing `dbl` anchors bit for bit and
  is the machine the `ratio` anchors were measured on. No case skips as a
  result: SKIP is still 1 (the unrelated fixture-version guard).
- **F7** — the emptiness guard's threshold moved from `2 * eps` to `4 * eps`,
  which is where the certificate's floor `fac * max(delta_q / 2, 2 * eps)`
  actually stops biting.
- **F9** — `R/axes_reliability.R`'s roxygen now names the three quantities, says
  the worst governs, and states that a fit on any path can be refused on the
  FIML ratio's estimate even where that ratio is not part of what it reports.
  `man/axes_reliability.Rd` regenerated.

Re-verified after the fixes: `devtools::test()` FAIL 0 / WARN 5 / SKIP 1 /
PASS 8822; `devtools::check(args = "--no-manual")` Status OK, 0/0/0 (14m 25s);
`options(cli.width = 500); devtools::document()` no diff beyond the regenerated
`.Rd` and zero `resolve link` lines; `exact_oracle.R` exit 0.

#### Windows CI red, and its repair (2026-08-30)

The first CI run on PR #144 (run 33329301066) was red on `windows-latest`; the
two other platforms and both non-matrix checks passed. Cause: the AC3
closed-form test asserts the shipped double route is WRONG at the hand-derived
configuration, and windows-latest prices that matrix EXACTLY -- `true_rel` 0
there, against 5.6e-13 on macOS and ubuntu. Three assertions reddened for a
platform being more accurate than the one the case was derived on
(`test-axes-certificate.R:684`, `:691`, `:700`). Not caused by the F7 threshold
change: `true_rel == 0` fails the original `2 * eps` form identically. The five
anchor-case skips on windows in the same run are pre-existing and by design
(M108's bit-identity precondition).

Repaired at the gate on the maintainer's choice among three options. The
platform-independent half -- the double-double replay landing on the
hand-derived exact values, R-level arithmetic throughout -- stays
unconditional and passed on windows already. The shipped-error half now sits
behind a precondition: where `true_rel <= 4 * eps` (the certificate's own floor)
the test skips naming that reason rather than asserting a floor against a floor.
`expect_gt(true_rel, 4 * eps)` is removed, the skip predicate replacing it --
the predicate cannot pass vacuously as the expectation could.

Discrimination shown both ways: on this tree the case runs every assertion and
skips nothing (certificate file 146 pass / 0 skip); with `axes_v_pricing()`
mocked to return the hand-derived exact values at that configuration -- the
state windows-latest is actually in -- the case reports skipped, not failed.

This is the platform-reach problem M115 already exists to widen, reached at a
second surface; it is noted on the ROADMAP row beside finding 8.
