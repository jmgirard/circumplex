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

- [ ] AC1: `R CMD check` of a branch tarball on M121's arm64 harness reports 0
      ERRORs, and `Rscript -e 'devtools::test()'` on macOS reports 0 failures —
      the two routes, both green.
- [ ] AC2: The refusing route asserts which refusal it is. With
      `axes_pricing_core()` edited to return `"singular"` where it returns
      `"unidentified"` today, a run of `test-axes-certificate.R` on the arm64
      harness fails with a message naming both `cxb` and `singular`; with it
      edited to return `"indefinite"` there, likewise.
- [ ] AC3: A refusal at any anchor is still a regression. With
      `axes_pricing_core()` edited to return `"unidentified"` unconditionally,
      a run of `test-axes-certificate.R` fails with messages naming each of
      `a4`, `a5`, `c4`, `b9a` and `b9b`; and with the `cxb` matrix check made
      to mismatch, that case fails rather than skipping.
- [ ] AC4: The exact-rational oracle keeps a live assertion at `cxb` on both
      routes: the double-double reference route agrees with the committed exact
      values to ≤ 1e-14, and perturbing one committed exact literal by one ulp
      makes that assertion fail on macOS and on the arm64 harness alike.
- [ ] AC5: Dispositions are a pinned set, printed on every run. A
      `cert_record()` call with a value outside the set fails; the run's output
      names each of the six cases and its disposition; and the detector reddens
      under a planted all-skip run and under a planted all-refused run.
- [ ] AC6: `test-axes-certificate-refusal.R:180` is exhaustive over the two
      routes: with `NOT_CRAN=true` it passes on macOS and on the arm64 harness,
      and inverting its expected warning text makes it fail on both.
- [ ] AC7: `Rscript -e 'devtools::check(manual = TRUE)'` reports 0 errors and 0
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
