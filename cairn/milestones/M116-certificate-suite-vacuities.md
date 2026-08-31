<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M116: Make three passing-without-checking assertions in the certificate suite redden

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP3, IP6
- **Branch/PR:** `m116-certificate-suite-vacuities` — https://github.com/jmgirard/circumplex/pull/147

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

- [x] AC1: The bracket ceiling in `cert_bracket()`
      (`tests/testthat/test-axes-certificate.R:345-352`) is 100 — ten times the
      safety factor `10` already written down at :268 — and is not read from
      `axes_certificate_safety_factor`.
- [x] AC2: With `axes_accuracy_certificate()`'s three returned fields each
      multiplied by 50 and nothing else in the tree changed,
      `Rscript -e 'options(testthat.progress.max_fails = 200); devtools::test()'`
      reports a failure whose label is `"b9b se: estimate"`. (Depends on AC1
      having landed: at the old 1e3 ceiling this plant reddens nothing. The
      raised display cap is what the label needs to be visible: the plant
      reddens 32 assertions in this file alone, and testthat's default stops
      printing after 10.)
- [x] AC3: `expect_identical(cert$se, floor_est)`,
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
- [x] AC4: The shape test at :355 asserts each case's built matrix `nrow(cs$r)`
      against the `p` already written down in its `cert_shape` table. With one
      anchor's builder call edited to return a matrix of a different dimension
      and nothing else in the tree changed, `Rscript -e 'devtools::test()'`
      reports a failure from that shape test naming that case.
- [x] AC5: On the unplanted tree, `Rscript -e 'devtools::test()'` is clean and
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
- [x] T3: Rewrite the :728-730 and :816-817 assertions per AC3, deleting the
      `cval` one with a comment recording that the test's hand derivation covers
      `v` and `v_naive` only. Run the grep.
- [x] T4: Add the dimension assertion to the shape test at :355. Plant a
      dimension change on one anchor's builder call, confirm the shape test
      fails naming that case, revert. Summarize in the work log.
- [x] T5: `devtools::test()` and `devtools::check(args = "--no-manual")` clean.

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
- 2026-08-31: T3 — the three floor identities in the dyadic closed-form test are now `cert_bracket()` calls fed by errors measured against 97/128, 2 and 5/8; the `se` identity in the quotient closed-form test is a `cert_bracket()` call against `v_exact`, and the `cval` identity is gone with a comment saying the hand derivation there covers `v` and `v_naive` only. `grep -n 'expect_identical(cert\$' tests/testthat/test-axes-certificate.R` returns nothing anywhere in the file. Full suite FAIL 0 / PASS 9155.
- 2026-08-31: T4 — the shape test now asserts each case's matrix `dim()` against the `p` in `cert_shape`, for the five built anchors and for counterexample B's saved matrix. Plant: `c4`'s builder call sliced to `[1:3, 1:3]`. The shape test failed at `"c4 matrix dim"`, and the `c4` per-case test SKIPPED under the same plant — the behavior this assertion exists to replace. Plant reverted; full suite FAIL 0 / PASS 9161.
- 2026-08-31: T5 — `devtools::test()` FAIL 0 / WARN 5 / SKIP 1 / PASS 9161 (all five warnings and the one skip pre-existing, in the lavaan and scaled-fit suites); `devtools::check(args = "--no-manual")` Status: OK, no errors, warnings or notes. No shipped code changed, so no NEWS entry is owed. Status → review.

## Decisions

## Review

Reviewed 2026-08-31 on branch `m116-certificate-suite-vacuities`, PR
https://github.com/jmgirard/circumplex/pull/147, against origin/master at
`2df0a15b` (master in sync with origin, branch 5 ahead / 0 behind, so no merge
was owed). Machine: aarch64-apple-darwin23, R 4.6.1.

### Acceptance criteria

- **AC1 — met.** `cert_ceiling <- 100` at `tests/testthat/test-axes-certificate.R:288`,
  read by the only ceiling half of `cert_bracket()` at :369
  (`expect_lte(est, cert_ceiling * true_rel, ...)`). `grep -n
  axes_certificate_safety_factor` over the file returns two lines, both prose
  comments (:263, :272) — no expectation reads the package constant. No `1e3`
  remains in any ceiling position (`grep -n '1e3'` returns :133 and :279, both
  prose, and :648/:650, an unrelated overflow test).
- **AC2 — met.** With `50 *` applied to each of the three fields
  `axes_accuracy_certificate()` returns (`R/axes_certificate.R:494-496`) and
  nothing else changed, `Rscript -e 'options(testthat.progress.max_fails = 200);
  devtools::test()'` reported FAIL 31 / WARN 5 / SKIP 1 / PASS 9134, and printed
  `Expected b9b se: estimate <= cert_ceiling * true_rel` at
  `test-axes-certificate.R:443:5` — the labelled failure the criterion names.
  Plant reverted; `git status` clean of it before the next run.
- **AC3 — met.** The three floor identities in the dyadic closed-form test are
  `cert_bracket()` calls at :783-785 fed by `cert_rel()` errors measured against
  the committed 97/128, 2 and 5/8 (:779-781); the quotient test's `se` identity
  is a `cert_bracket()` call against `v_exact` at :877-878, and its `cval`
  identity is deleted, replaced by a comment recording that the hand derivation
  there covers `v` and `v_naive` only. `grep -n 'expect_identical(cert\$'
  tests/testthat/test-axes-certificate.R` exits 1 with no output — no line
  anywhere in the file, so none between :700 and :849.
- **AC4 — met.** The shape test asserts `dim(cs$r)` against
  `rep(cert_shape[[cs$id]][[1L]], 2L)` for all five anchors (:413-417) and
  `dim()` of counterexample B's saved matrix against `cert_shape$cxb` (:419-421);
  `dim() == rep(p, 2L)` asserts the criterion's `nrow(cs$r) == p` and the column
  count besides. Plant: `c4`'s builder call sliced to `[1:3, 1:3]`. `Rscript -e
  'devtools::test()'` reported FAIL 3 / SKIP 2 / PASS 9142 with
  `Expected c4 matrix dim to be identical to rep(cert_shape[[cs$id]][[1L]], 2L)`
  at :415:5 — a failure from the shape test naming that case — while the `c4`
  per-case test SKIPPED at :439:5 on the bit-for-bit precondition, the silent
  behavior this assertion replaces. Plant reverted.
- **AC5 — met.** On the unplanted tree `Rscript -e 'devtools::test()'` reported
  FAIL 0 / WARN 5 / SKIP 1 / PASS 9161 (the five warnings and the one skip are
  pre-existing, in the lavaan and scaled-fit suites), and `Rscript -e
  'devtools::check(args = "--no-manual")'` finished `Status: OK` — 0 errors,
  0 warnings, 0 notes, 10m50s.

No Driving RR, so no projection-vs-outcome pairs are owed.

### Consistency gate

- `cairn_validate.py` exit 0 — all checks passed, 47 advisory warnings, every
  one a pre-existing M7 work-log wrapping line, none from this milestone. The
  `release window` advisory did not fire.
- `cairn_impact.py` skipped: the diff touches no `DESIGN.md` principle (only
  the test file, the milestone file and one ROADMAP row).
- `devtools::document()` at `cli.width = 500` left no diff and emitted zero
  lines matching `resolve link`.
- Generated files untouched; no new top-level files, so no `.Rbuildignore`
  entry is owed. README.md is newer than README.Rmd.
- `pkgdown::check_pkgdown()` — no problems found.
- NEWS.md: nothing owed. No shipped code changed.
- `devtools::check(args = "--no-manual")` clean (recorded under AC5).
- Master watches: newest push run on `master` reaching a verdict is `M115: …`
  (2026-08-31T03:02:32Z) — `R-CMD-check.yaml` success, `test-coverage.yaml`
  success.
- `Rscript tools/check-master-red-alert.R` and
  `Rscript tools/master-red-alert-dryrun.R` both exit 0 (all five dry-run
  scenarios ok); `Rscript tools/check-branch-protection.R` exits 0, both
  rulesets matching `tools/branch-protection.json`.

Gate passes.

### Independent review

The declared surface tier is internal, but the diff touches an executable
surface (`tests/testthat/test-axes-certificate.R`), so the full three-lens
fan-out ran, each reviewer fresh-context and none having authored the change.
[S] blame-history and [S] prior-review both returned zero findings — the first
tracing every replaced assertion to M108/M113 and judging the replacements a
strengthening rather than an undoing, the second finding the diff implements
rather than contradicts the ROADMAP's carried-forward M108/M113 items (the
GitHub inline-comment probe returned `[]`, so the per-PR walk was correctly
skipped). [O] diff-bug returned five, ranked below as it ranked them. Findings
1, 2 and 5 were re-verified against the implementation rather than against the
reviewer's account of it.

#### Findings reported, ranked as the reviewer ranked them

- **F1 — the three replacement brackets at the dyadic configuration are
  themselves vacuous, with the same failure set as the assertions they
  replaced** (`test-axes-certificate.R:783-786`). The `true_rel` fed to all
  three is structurally zero, not measured: :760-762 retain
  `expect_identical(axes_v_pricing(s, d)$corrected, 97/128)` and its two
  siblings, so on any otherwise-green run `dv`, `dn` and `du` are exactly `0`.
  **Verified independently:** `dv = dn = du = 0`, and `cert$se`, `cert$cval`,
  `cert$fiml_ratio` are each `identical()` to `cert_floor`, so each bracket
  takes the floor branch and runs `expect_lte(0, 4.44e-15)` — unfailable. The
  two branches together reduce to `identical(est, cert_floor)`, the same
  predicate as the deleted `expect_identical(cert$se, cert_floor)`; the AC2
  ×50 plant reddens :783-785 exactly as it would have reddened the old lines.
  AC3's letter is met; the milestone's Goal gains nothing at this site.
- **F2 — at the same three sites the ceiling half degenerates to `est <= 0`, an
  unsatisfiable bound** (`:783-786`). With `true_rel ≡ 0`, the non-floor branch
  asserts `expect_lte(est, cert_ceiling * 0)`. **Verified:** the arithmetic
  follows from F1's measured zeros. A platform on which `axes_dd_selftest()`
  fails returns the sentinel `se = 1` and the dyadic test then reports
  `closed-form dyadic se: estimate` as "1 is not less than or equal to 0" —
  naming a comparison against zero rather than the refusal that happened; the
  same holds for any platform whose estimate sits just above the floor while
  the pricing stays exact.
- **F3 — deleting the quotient test's `cval` identity leaves an uncovered drift
  window** (deletion site, `:880-888`). `cval` is still weakly constrained by
  `expect_identical(axes_certificate_worst(cert), cert$fiml_ratio)` at :919,
  which catches only `cval > fiml_ratio`. On this machine `cert$fiml_ratio =
  5.60e-12` and `cert$cval = 4.44e-15`, so a `cval` regression landing in
  `(4.44e-15, 5.60e-12]` is now green where the deleted line was red. AC3
  mandates the deletion, and the plan gate weighed it; the gap is real and is
  not the one the work log records.
- **F4 — the orientation block still cites the removed 1e3 ceiling** (`:133`).
  "certificate-over-true-error ratios ran 9.83 to 10.00 against a ceiling of
  1e3" — the file no longer contains that constant. Unmodified by the diff,
  invalidated by it.
- **F5 — the new ceiling comment misstates the measurement it exists to
  record** (`:279-286`). "all between 9.829 (`cxb se`) and 10.000 (every
  anchor, all three fields)" — **verified false**: replaying all eighteen
  ratios through the file's own `cert_true_error()` and
  `axes_accuracy_certificate()` gives a range of 9.829339 to 10.0025192, with
  `a4 fiml_ratio` 10.002519, `c4 fiml_ratio` 10.000362, `a5 fiml_ratio`
  10.000170 all above the stated upper end and `b9a fiml_ratio` 9.999994 below
  10.000. Separately, "three decades under" 1e3 is two decades for a ratio of
  10. The same "three decades" phrasing appears in the milestone's Scope.

Found sound by the same reviewer and not disputed here: the quotient test's
`se` replacement at :877 is non-vacuous (`true_rel = 1.19e-16` against a floor
of 4.44e-15, reddening if this machine's corrected-arm error rose ~37x); the
error arithmetic (`cert_root_rel`, the quotient form `(dv - dn)/(1 + dn)`)
matches `cert_true_error()`; `cert_ceiling` carries ~10x headroom over every
measured ratio; the dimension pin covers all six cases that reach the
skip-on-size-mismatch precondition; `floor_est` at :876 is still live (:917).
