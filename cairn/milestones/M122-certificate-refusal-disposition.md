# M122: Make the certificate suite exhaustive over the routes the shipped pricing can take

- **Status:** review
- **Priority:** high
- **Depends on:** M121
- **Driving RR:** —
- **Principles touched:** IP3
- **Resolves:** —
- **Surface tier:** internal — test-suite behavior; no exported behavior changes
- **Branch/PR:** `m122-certificate-refusal-disposition` / https://github.com/jmgirard/circumplex/pull/155

## Goal

At counterexample B the shipped pricing may price or may refuse, decided by
the platform's LU roundoff; make the suite assert both routes exhaustively
instead of freezing one machine's outcome into a regression check.

## Scope

**In:** the counterexample-B block and the refusal branch of
`cert_true_error()`; the same defect at `test-axes-certificate-refusal.R:180`;
the disposition vocabulary and the all-skip detector; the two Known-fragilities
items on this fix's path (`cert_bracket()`'s value-coincidence floor branch,
`cert_rel()`'s zero denominator); the file-header rule and the LESSONS clause.

**Out:** `tol = 0` in `axes_pricing_core()` → candidate row (RR22 rec 9; it
changes when the shipped package emits a number, and wants its own gate and
oracle). The other four Known-fragilities items → the row already routing
them. dd-vs-exact assertions at the five anchors, and the `?axes_reliability`
sentence that the estimate is a property of the fit on this machine → candidate
row (RR22 rec 11). Recording this file's CRAN-live posture as a decision →
candidate row (RR22 rec 12).

## Acceptance criteria

- [x] AC1: `R CMD check` of a branch tarball on M121's arm64 harness reports 0
      ERRORs, and `Rscript -e 'devtools::test()'` on macOS reports 0 failures —
      the two routes, both green.
- [x] AC2: The refusing route asserts which refusal it is. With
      `axes_pricing_core()` edited to return `"singular"` where it returns
      `"unidentified"` today, a run of `test-axes-certificate.R` on the arm64
      harness fails with a message naming both `cxb` and `singular`; with it
      edited to return `"indefinite"` there, likewise.
- [x] AC3: A refusal at any anchor is still a regression. With
      `axes_pricing_core()` edited to return `"unidentified"` unconditionally,
      a run of `test-axes-certificate.R` fails with messages naming each of
      `a4`, `a5`, `c4`, `b9a` and `b9b`; and with the `cxb` matrix check made
      to mismatch, that case fails rather than skipping.
- [x] AC4: The exact-rational oracle keeps a live assertion at `cxb` on both
      routes: the double-double reference route agrees with the committed exact
      values to ≤ 1e-14, and perturbing one committed exact literal by one ulp
      makes that assertion fail on macOS and on the arm64 harness alike.
- [x] AC5: Dispositions are a pinned set, printed on every run. A
      `cert_record()` call with a value outside the set fails; the run's output
      names each of the six cases and its disposition; and the detector reddens
      under a planted all-skip run and under a planted all-refused run.
- [x] AC6: `test-axes-certificate-refusal.R:180` is exhaustive over the two
      routes: with `NOT_CRAN=true` it passes on macOS and on the arm64 harness,
      and inverting its expected warning text makes it fail on both.
- [x] AC7: `Rscript -e 'devtools::check(manual = TRUE)'` reports 0 errors and 0
      warnings; `cert_bracket()` selects its floor branch by a stated argument
      rather than by `identical(est, cert_floor)` and `cert_rel()` refuses a
      zero denominator, each with a planted probe that reddens; and NEWS.md
      carries an entry naming the platform and stating that no exported
      behavior changed.

## Coverage

- AC1 → T1, T2, T7
- AC2 → T1, T6
- AC3 → T1, T6
- AC4 → T3, T6
- AC5 → T4, T6
- AC6 → T2, T6
- AC7 → T5, T7

## Tasks

- [x] T1: Rewrite the counterexample-B block to the exhaustive-disposition
      shape: matrix mismatch fails (never skips, the matrix is committed
      bytes); the priced route keeps its three brackets and both `delta_star`
      assertions; the refusing route asserts `"unidentified"` from both `v` and
      `u`, the sentinel identically, and the predicate `"uncertified"`; kappa,
      the criterion `"ill_conditioned"` and `axes_certificate_worst(cert) >
      delta_star` sit outside both. Commit the measured `rcond(info)` band
      beside the case as the stated ground for the admission
      (RB tripwire: ip-touching).
- [x] T2: Repair `test-axes-certificate-refusal.R:180` to the same shape —
      graded route asserts the warning is not `"… 1;"`, refused route asserts
      it is.
- [x] T3: Add the double-double-versus-exact assertion at `cxb`, running on
      both routes, with the reason stated against the file's existing
      "deliberately NOT pinned" comment (that comment refuses a *gate*, not an
      assertion).
- [x] T4: Pin the disposition vocabulary to named constants, assert membership
      in the detector, emit the disposition table on every run, and require
      `cxb ∈ {priced, refused}` plus at least one anchor priced.
- [x] T5: Fold in the two Known-fragilities items: `cert_bracket()`'s floor
      branch and `cert_rel()`'s zero denominator.
- [x] T6: Run the planted-defect probes for AC2, AC3, AC4, AC5 and AC6 on both
      macOS and the arm64 harness.
- [x] T7: Add the file-header assertion rule and the LESSONS clause; run
      `devtools::check(manual = TRUE)` and the arm64 harness; write NEWS.

## Work log

- 2026-09-05: planned from RR22. Criteria audit (full mode): returned three
  findings on this milestone's drafts — AC4 was satisfied by any failure
  anywhere in the file, AC3 and AC5 each stood one exemplar in for a family
  (neither varied which case nor which refusal literal; the all-skip probe
  tested the pre-existing skip route rather than the all-refused state this
  fix creates), and no task ran the procedure AC2 named. All three repaired
  before writing; cleared AC1, AC7.
- 2026-09-05: approach — RR22 rejected an explicit `solve()` tolerance by
  measurement (no value inside the band [1.67e-16, 3.35e-16] is
  platform-stable; every value at or below 1e-16 gives 0 refusals in 300, so
  it removes the conditioning gate rather than tuning it). Falsified by: a
  machine-independent derivation of a tolerance inside that band.
- 2026-09-05: approach — the detector's "at least one anchor priced" clause
  is planned CRAN-live. CRAN's r-release-macos-x86_64 log for 2.0.0 lists no
  "anchor matrix" skip reason, so the five anchors price on the flavor whose
  `cos(225°)` rounds differently, and the per-case tests carry no
  `skip_on_cran()`. Weighed against that: the ROADMAP records the anchors as
  0.04 ulp from a bit flip at that cosine, and this release has been rejected
  three times. Implement should confirm the margin before leaving the clause
  CRAN-live, and put it under `skip_on_cran()` if it cannot. Falsified by: a
  CRAN flavor reporting an anchor-matrix skip.
- 2026-09-05: checkpoint, tasks not yet ticked. T1-T5 and T7 are written and
  both edited files pass on both platforms (macOS 217/222 passes,
  aarch64-linux/OpenBLAS 213/222 -- the gap is the refusing route's smaller
  assertion count); the full `devtools::test()` run and T6's planted probes
  have not reported yet, so no checkbox is ticked.
- 2026-09-05: baseline reproduced before any edit -- `tools/arm64/testfile.sh`
  fails at `test-axes-certificate.R:544` with the CRAN message ("the shipped
  pricing REFUSES at case 'cxb' (unidentified, unidentified)"), and all five
  anchors priced there.
- 2026-09-05: measured, for the gate -- of the eight distinct cosines the
  anchor builders use, one sits 0.0396 ulp from a rounding boundary
  (cos(3.9269908169872414)) and moves four anchors; c4 moves only on cos(0)
  and cos(pi), both 0.5 ulp clear. Gate answer: the detector's "at least one
  anchor priced" clause stays CRAN-live, and B's admission is built as
  reviewed rather than re-escalated.
- 2026-09-05: measured -- the dd reference route is bit-identical on
  aarch64-apple-darwin23 and aarch64-unknown-linux-gnu/OpenBLAS at every cxb
  quantity, 0.17/0.42 ulp for v/v_naive and 3.8e-17 absolute for u, which is
  what the two dd-vs-exact bounds are set from. `rcond(info)` is 2.6008e-16
  on the first and 2.0494e-16 on the second, straddling eps.
- 2026-09-05: minor amendment -- fixed two defects in `tools/arm64/testfile.sh`
  found while running T6's loop: it copied macOS `src/*.o` into the container
  (make skipped the aarch64 compile and the install died on "invalid ELF
  header"), and it ran `library(circumplex)`, so the suite's helpers could not
  see internals and the run died in `source_test_helpers()`. Both are
  discovered sub-tasks of T6, not new scope.
- 2026-09-05: LESSONS.md byte budget was tripped by the M122 clause (20,398 of
  20,000); the M108-family line was compressed in the same pass to 19,991.
  `cairn/test-craft.md` is at 8,924 of its own 9,000, so the ownership exit
  was not available for the remedy.
- 2026-09-05: full `devtools::test()` returned FAIL 1 at the AC7 helper probe:
  `expect_success()` asserts exactly one expectation and the two-sided bracket
  branch fires two, so the helper reported the branch's arity rather than its
  verdict. Probed through the `expectation_failure` condition instead; both
  probe helpers were then shown to redden on the case they are meant to catch.
  Suite now 9276 passes, 0 failures.
- 2026-09-05: T1-T6 done. Planted-defect probes, each run against the file it
  targets, restoring the tree between runs. AC2 (arm64, where the shipped
  pricing refuses): the refusal literal edited to `"singular"` and then to
  `"indefinite"` fails naming `cxb` and that literal, from both `v` and `u`.
  AC3: `axes_pricing_core()` refusing unconditionally fails naming each of
  `a4`, `a5`, `c4`, `b9a`, `b9b` on both platforms; the `cxb` matrix moved one
  ulp fails at three sites and records `matrix mismatch`, never `skipped`.
  AC4: each of the four committed `v_hi`/`vn_hi` literals moved one ulp
  reddens the dd-vs-exact assertion (1.39, 1.34, 0.67, 1.42 ulp against the
  0.5 bound) on macOS, and `v_hi[1]` gives the identical 1.39 on arm64.
  AC5: `cert_record()` with a value outside the set errors; the disposition
  table prints on every run; the detector reddens on the planted all-skip run
  (five anchors moved) and on the planted all-refused run. AC6: the inverted
  expectation fails on both platforms, and on opposite branches - macOS
  "expected 2, got 0" on the graded route, arm64 "expected 0, got 2" on the
  refusing one.
- 2026-09-05: the first AC6 patch was invalid and its arm64 run reported a
  false green - a string replace matched both branches and left the file
  semantically unchanged there. Rewritten to swap the two branches by line
  index and re-run on both platforms.
- 2026-09-05: T7 done. `devtools::check(manual = TRUE)` on macOS: Status OK,
  0 errors / 0 warnings / 0 notes, 8m 4s, PDF manual built. The linux-arm64
  flavor check on the branch tarball (`tools/arm64/check.sh`, R 4.6.1,
  aarch64-unknown-linux-gnu, OpenBLAS 0.3.33) reports `Status: OK` where the
  submitted 2.0.1 ERRORed; its own test log carries the disposition line
  "a4 = priced; a5 = priced; c4 = priced; b9a = priced; b9b = priced;
  cxb = refused -- unidentified" and no anchor-matrix skip, which is the
  CRAN-flavor evidence for keeping the anchor clause live. Green log and
  platform record at `~/tmp-m122/` for the release walk's cran-comments step.
- 2026-09-05: `tools/arm64/check.sh` refused the tarball on its first run
  because the scratchpad is outside Docker Desktop's shared paths - M121's
  empty-bind-mount guard firing as designed. Re-run from a shared path.
- 2026-09-05: review opened. master had not moved (branch 0 behind); branch
  pushed and draft PR #155 opened. Evidence gathering in progress: AC1's arm64
  half is green (`check.sh` on a fresh branch tarball, `Status: OK`, FAIL 0 /
  PASS 2427, disposition line `cxb = refused -- unidentified`); the macOS
  `devtools::test()` run and the planted-defect probe battery have not reported
  yet, so no criterion checkbox is ticked. `cairn_validate` exits 0.

## Review

Fresh evidence, gathered 2026-09-05 at the review gate. Every figure below is
from a run made in this phase, not carried from implementation.

- **AC1 — met.** macOS `Rscript -e 'devtools::test()'` (aarch64-apple-darwin23):
  `FAIL 0 | WARN 9 | SKIP 1 | PASS 9278`, disposition line `a4 = priced;
  a5 = priced; c4 = priced; b9a = priced; b9b = priced; cxb = priced`. arm64
  `tools/arm64/check.sh` on a branch tarball built this phase
  (`circumplex_2.0.1.tar.gz`, plain `R CMD build`): `Status: OK`, 0 ERRORs, its
  test log `FAIL 0 | WARN 4 | SKIP 540 | PASS 2427`, disposition line
  `... cxb = refused -- unidentified`, no anchor-matrix skip. Platform record:
  R 4.6.1, `aarch64-unknown-linux-gnu`, LAPACK
  `openblas-pthread/libopenblasp-r0.3.33.so`. The two platforms take opposite
  routes at counterexample B and both are green, which is the criterion's
  "two routes, both green".
- **AC2 — met.** Probes run on the arm64 harness, where the shipped pricing
  refuses. `axes_pricing_core()`'s `return("unidentified")` (R/axes_corrected_se.R:179)
  edited to `"singular"`: three failures, two of them
  `Expected cxb v pricing to be identical to "unidentified"` /
  `cxb u pricing`, `actual: "singular"`. Edited to `"indefinite"`: the same two
  failures with `actual: "indefinite"`. Both name `cxb` and the planted literal,
  and both surfaces (`v` and `u`) fail separately.
- **AC3 — met.** `axes_pricing_core()` made to refuse unconditionally: on macOS
  and on arm64 alike the detector fails with the table
  `a4 = refused -- unidentified; a5 = ...; c4 = ...; b9a = ...; b9b = ...`,
  each of the five anchors named, plus five per-case bracket failures.
  Separately, the committed `cxb` matrix moved one ulp
  (`sig[1]` `-0x1.ac70f5bf320e9p-1` -> `...eap-1`): 3 failures on both
  platforms, the case recorded `matrix mismatch` (never `skipped`), the
  message reading "the matrix at case 'cxb' is read from committed bytes and
  no longer matches the exact values committed beside it".
- **AC4 — met.** The dd-vs-exact assertion is live at `cxb` on both routes
  (macOS priced, arm64 refused) and passes in both green runs. Perturbing the
  committed `v_hi[1]` by one ulp (`0x1.a27aa6fa81289p+3` -> `...8ap+3`) fails
  identically on both platforms:
  `Expected cxb dd-vs-exact v (ulp) < 0.5. Actual comparison: 1.39 >= 0.50`.
  The criterion's stated bound is 1e-14; the implementation asserts about
  5.5e-17 relative, so it is stronger than the criterion asks (see finding 9).
- **AC5 — met.** `cert_record()` with an out-of-set value errors and the value
  never reaches the environment (in-suite test "AC5: the disposition vocabulary
  is closed", green on both platforms). The table prints on every run, green
  ones included, naming all six cases — observed in every log gathered here,
  including the arm64 `R CMD check` test log. The detector reddens on a planted
  all-refused run (AC3's probe: `Expected anchors priced (...) > 0L`) and on a
  planted all-skip run (the five anchor `sig[1]` literals each moved one ulp):
  `Expected anchors priced (a4 = skipped; a5 = skipped; c4 = skipped;
  b9a = skipped; b9b = skipped; cxb = priced) > 0L`. That last run is also the
  discrimination evidence for the M122 rewrite: the old "at least one case
  priced" clause would have passed it, since `cxb` priced.
- **AC6 — met.** With `NOT_CRAN=true` the repaired block passes on both
  platforms (macOS: inside the green `devtools::test()` run; arm64:
  `tools/arm64/testfile.sh`, `FAIL 0 | PASS 222`). Swapping the two branches by
  line index fails on both, and on opposite branches — macOS at
  `test-axes-certificate-refusal.R:252` (the graded route it takes), arm64 at
  `:246` (the refusing route it takes) — which is the exhaustiveness the
  criterion asks for.
- 2026-09-05: review evidence gathered for AC1-AC6, all six ticked against the
  Review section's lines; AC7's `devtools::check(manual = TRUE)` still running.
  Three fresh-context lenses reported; six findings stand for gate triage, none
  of them showing a criterion failing, so no return under the floor.
- **AC7 — met.** `Rscript -e 'devtools::check(manual = TRUE)'` on macOS:
  `Status: OK`, 0 errors / 0 warnings / 0 notes, 7m 40.1s, PDF and HTML manuals
  both built. `cert_bracket()` now selects its floor branch by the `at_floor`
  argument (defaulting to `est <= cert_floor`) rather than by
  `identical(est, cert_floor)`, and `cert_rel()` stops with "the exact value is
  zero" on a zero denominator; the in-suite probes for both
  ("AC7: the two harness helpers select their branches on a stated condition")
  are green on both platforms, which is the helpers reddening on the cases they
  target. NEWS.md carries an entry naming Linux arm64 and stating that
  `axes_reliability()` refuses that matrix on every platform as before and no
  result it reports has changed. See finding 3 on the discrimination of one of
  those two probes.

### Consistency gate

Universal cairn-file checks: `cairn_validate.py` exits 0, every check PASS or
OK; 75 advisory warnings, all of them the wrapped work-log lines this repo has
always written. No `release window` advisory. No `DESIGN.md` principle changed
by the diff, so `cairn_impact.py --changed` does not apply.

Toolchain checks, from the `r-package` profile's `consistency-gate` slot:
`devtools::document()` produces no diff and zero `resolve link` warnings at
`cli.width = 500`; generated files unedited; README.Rmd untouched by the diff,
so no re-knit is owed; `pkgdown::check_pkgdown()` reports no problems; NEWS.md
carries this milestone's entry; no new top-level files, so no `.Rbuildignore`
entry owed; `devtools::check()` clean (the AC7 run above). Master watches: the
newest push run on master reaching a verdict is `success` for both
`R-CMD-check.yaml` and `test-coverage.yaml` (`bb54f6ff`, 2026-09-05).
`tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R` and
`tools/check-branch-protection.R` all exit clean.

Gate result: pass.
### Independent review

Three fresh-context lenses, none having seen the implementation: [O] diff-bug,
[S] blame-history, [S] prior-review-record. The blame lens reported no defects.
The prior-review lens found one (finding 2 below) and recorded that
`gh api .../pulls/comments` returns `[]`, so there is no GitHub inline-review
surface in this repo and the archived `## Review` sections are the whole
prior-review record. The diff lens reported twelve. Consolidated and deduped,
with each claim re-verified against the implementation at this gate:

1. `dd_ulp()` divides a relative error by `2^-53`, which equals an ulp count
   only at mantissa exactly 2. Measured from the committed literals, one code
   unit is 1.0850-1.2794 true ulp, so the `< 0.5` bound documented as "half a
   unit in the last place" actually demands 0.391-0.461 ulp. The error is in
   the safe direction (it overstates the measured error), but this is a new
   CRAN-live assertion whose tightest measured margin is 0.419 of 0.5, in a
   package that has taken three platform-exact rejections.
2. `cairn/DESIGN.md:83-95` still lists `cert_bracket()` selecting its branch
   "by `identical(est, cert_floor)`, a value coincidence" and "`cert_rel()`
   divides by `hi + lo`, which is zero exactly when the exact quantity is" as
   open latent defects. T5 fixed both, so that record is now false, and its
   "six latent defects" count is stale at four.
3. The `cert_bracket()` below-floor probe does not discriminate its own repair:
   under the pre-M122 `identical(est, cert_floor)`, `est = cert_floor / 2`
   takes the two-sided branch and `expect_gte(cert_floor/2, cert_floor*4)`
   fails, so the probe reddens either way. The reviewer's stronger claim -- that
   the two branches have identical failure sets below the floor -- is wrong:
   the old branch fails iff `true_rel > est`, the new one iff
   `true_rel > cert_floor`, so below the floor the new code is strictly more
   lenient. Which makes the probe's comment ("under the old test it did not")
   false in the opposite direction from the one reported.
4. The new file header and the LESSONS clause both say counterexample B cost
   2.0.1 its "third CRAN rejection"; D-055 says its "second pre-test
   rejection". Reading the ROADMAP's sequence, D-055 is right -- "third" is the
   third platform-exact failure site, not the third rejection.
5. `test-axes-certificate-refusal.R:238` reads the route by pricing raw `S`,
   while the surface under test prices `cov2cor(sigma)`. Safe today only
   because the fixture has an exact unit diagonal, which nothing asserts or
   records; a fixture regenerated without one would silently put the probe on a
   different matrix than the branch it selects.
6. `axes_accuracy_certificate()` returns its sentinel by six routes besides
   "the pricing refused" (`!axes_dd_selftest()`, a NULL `axes_dd_pricing()`,
   non-finite `v_hat`/`vn_hat`/`u_hat`, nonpositive quadratic forms, a vanished
   cval numerator). If one fires while the pricing succeeds, the priced branch
   brackets a sentinel `cert` and passes, since `expect_lte(1, max(cert_ceiling
   * true_rel, cert_floor))` holds. The two-branch split is exhaustive over the
   shipped pricing's routes, which is what the criteria ask, but not over the
   certificate's.
7. `cert_refusal_admitted()` compares the committed band to
   `.Machine$double.eps`, which is `2^-52` on every platform R supports, so the
   predicate is a constant: true at `cxb`, false at every anchor. It is
   therefore equivalent to naming the case, and admits any `unidentified`
   refusal at `cxb` including one from a genuine regression. The refusal's
   identity is asserted, so `singular` and `indefinite` still fail; what is
   unasserted is that this machine's own `rcond(info)` falls inside the band.
8. The disposition table prints the pinned string `refused -- unidentified`
   whatever literal was recorded, so an anchor refusing as `singular` would be
   reported in the table as `unidentified`; only `detail` carries the truth.
   The `fail()` message is correct, so nothing goes green wrongly.
9. `bracket_passes(1e-3, 1e-5, at_floor = FALSE)` passes on exact bit-equality
   (`100 * 1e-5` and `1e-3` are the same double), so the probe has zero margin.
10. The NEWS entry describes the failure as a check requiring the accuracy
    check "to report a number", which is accurate for the certificate suite but
    not for the second file repaired, whose assertion turned on warning text.
11. AC4 states agreement "to <= 1e-14"; the implementation asserts about
    5.5e-17 relative. The implementation is stronger, so the criterion is met
    as written and nothing is under-tested.

None of these demonstrates an acceptance criterion failing, and none is a
defect in what the shipped package does for its users, so none meets the
return floor. Dispositions are recorded below at the gate.
### Triage at the gate (2026-09-05)

Maintainer's decision: fix findings 1-5 on the branch, route 6 and 7 to a
candidate row, merge on green CI.

- Finding 1 -- **fixed now.** `dd_ulp()` now divides the absolute error by
  `2^(floor(log2(abs(hi))) - 52)`, one unit in the last place of the exact
  value, and refuses a zero `hi`. Re-measured after the change: 0.135 and
  0.045 ulp for `v`, 0.387 and 0.234 for `v_naive`, `u` 3.76e-17 against
  1.11e-16. The comment's figures were replaced with these. AC4's planted
  probe re-run against the corrected bound fails at `1.14 >= 0.50` on macOS
  and on arm64 alike -- 1.39/1.2235, exactly the rescaling.
- Finding 2 -- **fixed now.** `cairn/DESIGN.md`'s Known-fragilities entry is
  corrected in place and marked `corrected 2026-09-05`: the two M122 fixed are
  moved to a "Fixed by M122" clause naming what replaced each, and the count
  reads two fixed, four remaining.
- Finding 3 -- **fixed now**, by correcting the claim rather than the probe.
  The comment now states that the probe shows the floor branch reddening but
  does not discriminate the repair, that no probe can (the new failure set is
  a strict subset of the old below the floor), and that what the repair buys
  is a truthful report rather than a wider one.
- Finding 4 -- **fixed now.** The file header and the LESSONS clause now read
  "second pre-test rejection", agreeing with D-055; the header adds that this
  is the third platform-exact failure site, which is what "third" meant.
- Finding 5 -- **fixed now.** `expect_identical(stats::cov2cor(S), S)` is
  asserted before the route probe, with the reason stated.
- Findings 6 and 7 -- **follow-up**, one candidate row: the certificate's six
  other sentinel routes are not covered by the two-branch split, and
  `cert_refusal_admitted()` never measures this machine's own `rcond(info)`.
- Findings 8, 9, 10 -- **rejected at the gate** as too small to carry: the
  disposition table's literal, the zero-margin argument probe, and the NEWS
  wording. None can turn a red run green.
- Finding 11 -- **no action.** AC4 is met as written; the implementation is
  stronger than the criterion asks.

Re-verification after the fixes: macOS `test_local` over both edited files
`FAIL 0` with `cxb = priced`; arm64 `tools/arm64/testfile.sh`
`test-axes-certificate.R` `FAIL 0 | PASS 221` with
`cxb = refused -- unidentified`, `test-axes-certificate-refusal.R`
`FAIL 0 | PASS 223` (up one, the new `cov2cor` assertion). AC1's full-suite and
AC7's `check(manual = TRUE)` re-runs on the fixed tree are recorded below.

- 2026-09-05: step-7 approval: PR #155 approved for merge, with findings 1-5
  fixed on the branch and 6-7 routed to a candidate row.
