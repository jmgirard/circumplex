<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M116: Make three passing-without-checking assertions in the certificate suite redden

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP3, IP6
- **Branch/PR:** `m116-certificate-suite-vacuities`

## Goal

Three assertions in `tests/testthat/test-axes-certificate.R` that pass without
checking what they name are replaced by assertions that redden on the failure
each was written to catch.

## Scope

Surface tier: **internal** — the deliverable is the package's own test suite,
which no external consumer of the repo relies on. No shipped code changes.

**In:** the bracket ceiling at `tests/testthat/test-axes-certificate.R:345-352`,
three decades above anything the certificate delivers (measured ratios 9.83 to
10.00 against a ceiling of 1e3); the unconditional `expect_identical(cert$…,
floor_est)` assertions at :728-730 and :816-817, which state that the running
machine's shipped pricing commits zero error at those configurations; and the
dimension half of `cert_true_error()`'s precondition at :288, which skips where
it should redden. Each repair is proved able to fail by a planted defect.

**Out:**
- the certificate's double evaluation per fit → M117
- the `axes_v_pricing()` exactness identities at :723-725, the same claim shape
  one level down → ROADMAP degeneracy row (declined at this plan's gate)
- the remaining M115-deferred items — `cert_hex()` dead code,
  `cert_root_rel()` at e ≤ -1, `cert_rel()`'s zero denominator,
  `exact_oracle.R:70-79`'s silent NULL, the `identical(est, cert_floor)` branch
  selector, asserted-vs-skipped indistinguishability → ROADMAP degeneracy row
- surfacing the certificate on computed fits → ROADMAP degeneracy row; it adds
  a field to an exported return, so it wants its own gate

## Acceptance criteria

- [ ] AC1: The bracket ceiling in `cert_bracket()`
      (`tests/testthat/test-axes-certificate.R:345-352`) is 100 — ten times the
      safety factor `10` already written down at :268 — and is not read from
      `axes_certificate_safety_factor`.
- [ ] AC2: With `axes_accuracy_certificate()`'s three returned fields each
      multiplied by 50 and nothing else in the tree changed,
      `Rscript -e 'options(testthat.progress.max_fails = 200); devtools::test()'`
      reports a failure whose label is `"b9b se: estimate"`. (Depends on AC1
      having landed: at the old 1e3 ceiling this plant reddens nothing. The
      raised display cap is what the label needs to be visible: the plant
      reddens 32 assertions in this file alone, and testthat's default stops
      printing after 10.)
- [ ] AC3: `expect_identical(cert$se, floor_est)`,
      `expect_identical(cert$cval, floor_est)` and
      `expect_identical(cert$fiml_ratio, floor_est)` at :728-730 are replaced by
      `cert_bracket()` calls against errors the running machine measures against
      that test's own committed exact values 97/128, 2 and 5/8;
      `expect_identical(cert$se, floor_est)` at :816 is replaced by a
      `cert_bracket()` call against the error the running machine measures
      against that test's committed `v_exact`; and
      `expect_identical(cert$cval, floor_est)` at :817 is deleted, no exact `u`
      being committed for that configuration. `grep -n 'expect_identical(cert\$'
      tests/testthat/test-axes-certificate.R` then returns no line between :700
      and :849.
- [ ] AC4: The shape test at :355 asserts each case's built matrix `nrow(cs$r)`
      against the `p` already written down in its `cert_shape` table. With one
      anchor's builder call edited to return a matrix of a different dimension
      and nothing else in the tree changed, `Rscript -e 'devtools::test()'`
      reports a failure from that shape test naming that case.
- [ ] AC5: On the unplanted tree, `Rscript -e 'devtools::test()'` is clean and
      `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors and
      0 warnings.

## Coverage

- AC1 → T1
- AC2 → T1, T2
- AC3 → T3
- AC4 → T4
- AC5 → T5

## Tasks

- [x] T1: Measure this machine's certificate estimate and true error for every
      case × field, noting which sit at `cert_floor`. Record the figures with
      the date and the command that produced them in a comment beside the
      ceiling, and set the ceiling to ten times the safety factor already
      written down at :268.
- [x] T2: Apply the ×50 plant to `axes_accuracy_certificate()`'s three returned
      fields, run the suite with the display cap raised, confirm the
      `"b9b se: estimate"` failure, revert. Summarize the run in the work log.
- [ ] T3: Rewrite the :728-730 and :816-817 assertions per AC3, deleting the
      `cval` one with a comment recording that the test's hand derivation covers
      `v` and `v_naive` only. Run the grep.
- [ ] T4: Add the dimension assertion to the shape test at :355. Plant a
      dimension change on one anchor's builder call, confirm the shape test
      fails naming that case, revert. Summarize in the work log.
- [ ] T5: `devtools::test()` and `devtools::check(args = "--no-manual")` clean.

## Work log

- 2026-08-30: created by /milestone-plan.
- 2026-08-30: plan gate chose replacing the second closed-form test's `cval` floor identity with deletion over hand-deriving an exact `u` for that configuration, because the test commits `v` and `v_naive` only and a fresh hand derivation is its own correctness surface; falsified by a later need to price `cval` at that configuration, which would make the derivation owed anyway.
- 2026-08-30: plan gate chose a ceiling of ten times the safety factor over three times it, because the measured ratios reach the factor exactly (9.83–10.00 against a factor of 10) and three times it leaves no room for a machine rounding the other way; falsified by a measured ratio above 100 on any machine, which would mean the certificate overstates by more than the factor alone.
- 2026-08-30: plan gate declined sweeping the `axes_v_pricing()` exactness identities at :723-725 in the same pass, keeping the diff to the `cert$` assertions the roadmap disposition named; falsified by either identity reddening on a platform, which is the same class as the M108 trap.
- 2026-08-30: criteria audit ran in **reduced** mode (internal tier), fresh-context [O] reader, two passes. First pass returned findings on five of six drafted criteria — three wrong line references, an unsatisfiable planted defect, five recording-act clauses binding instruments rather than the deliverable, and a demonstration arm spanning two revisions of the tree. Second pass over the post-gate wording returned four more — an inert exemption registry naming lines the criterion's own grep does not match, a plant whose multiplier only bites once the ceiling has landed, a shape-test line reference off by three, and a zero-skips clause the plant itself contradicts. All disposed before this file was written.

- 2026-08-30: T1 — measured estimate/true-error on aarch64-apple-darwin23, R 4.6.1, reference BLAS: eighteen ratios across six priced cases x three fields, 9.829 (`cxb se`) to 10.000 (every anchor); the two closed-form configurations sit at the floor bar `cf2 fiml_ratio` (10.000), and `cf2`'s true SE error is 1.19e-16 rather than zero. Ceiling set to a named `cert_ceiling <- 100` beside `cert_floor`, comment recording range, extremes, date, machine and procedure. Gate chose the named constant over an inline literal, and the range-plus-extremes comment over a per-case table.
- 2026-08-30: gate chose pinning all six cases' matrix dimensions in T4, the saved counterexample fixture included, over the five built anchors alone: the fixture takes the same skip-on-size-mismatch path and its size is already written down in the table the check reads.
- 2026-08-31: AC2 amended at a mini gate — its command gains `options(testthat.progress.max_fails = 200)`, and a sentence saying why. As written the criterion was unsatisfiable: the ×50 plant reddens 32 assertions in this file and testthat prints labels for only the first 10, so `"b9b se: estimate"` was counted and never named. Nothing in the tree changes; the promise is what it was.
- 2026-08-31: criteria audit re-ran on the amended AC2 in **reduced** mode (internal tier), fresh-context [O] reader that did not author the wording — no finding on any of the three questions.
- 2026-08-31: T2 — with the ×50 plant on `axes_accuracy_certificate()`'s three fields, `devtools::test(filter = "axes-certificate")` reported FAIL 32 / PASS 350, `"b9b se: estimate"` among them (`Expected b9b se: estimate <= cert_ceiling * true_rel`); `b9b cval` and `b9b fiml_ratio` too. Plant reverted, the file back to FAIL 0 / PASS 382.

## Decisions

## Review
