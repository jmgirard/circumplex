# M122: Make the certificate suite exhaustive over the routes the shipped pricing can take

- **Status:** planned
- **Priority:** high
- **Depends on:** M121
- **Driving RR:** —
- **Principles touched:** IP3
- **Resolves:** —
- **Surface tier:** internal — test-suite behavior; no exported behavior changes
- **Branch/PR:** —

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

- [ ] T1: Rewrite the counterexample-B block to the exhaustive-disposition
      shape: matrix mismatch fails (never skips, the matrix is committed
      bytes); the priced route keeps its three brackets and both `delta_star`
      assertions; the refusing route asserts `"unidentified"` from both `v` and
      `u`, the sentinel identically, and the predicate `"uncertified"`; kappa,
      the criterion `"ill_conditioned"` and `axes_certificate_worst(cert) >
      delta_star` sit outside both. Commit the measured `rcond(info)` band
      beside the case as the stated ground for the admission
      (RB tripwire: ip-touching).
- [ ] T2: Repair `test-axes-certificate-refusal.R:180` to the same shape —
      graded route asserts the warning is not `"… 1;"`, refused route asserts
      it is.
- [ ] T3: Add the double-double-versus-exact assertion at `cxb`, running on
      both routes, with the reason stated against the file's existing
      "deliberately NOT pinned" comment (that comment refuses a *gate*, not an
      assertion).
- [ ] T4: Pin the disposition vocabulary to named constants, assert membership
      in the detector, emit the disposition table on every run, and require
      `cxb ∈ {priced, refused}` plus at least one anchor priced.
- [ ] T5: Fold in the two Known-fragilities items: `cert_bracket()`'s floor
      branch and `cert_rel()`'s zero denominator.
- [ ] T6: Run the planted-defect probes for AC2, AC3, AC4, AC5 and AC6 on both
      macOS and the arm64 harness.
- [ ] T7: Add the file-header assertion rule and the LESSONS clause; run
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
