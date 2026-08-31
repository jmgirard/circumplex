# M115: Make the packaged accuracy bracket assert where the shipped pricing differs

- **Status:** review
- **Priority:** normal
- **Depends on:** M113
- **Driving RR:** —
- **Principles touched:** IP3
- **Branch/PR:** `m115-certificate-bracket-reach` / https://github.com/jmgirard/circumplex/pull/146

## Goal

Replace the frozen macOS pricing yardstick with a committed exact value, so
the packaged bracket measures the running machine's own error instead of
skipping wherever that machine is not the one the figures were frozen on.

## Scope

Surface tier: **internal** — the deliverable is the validation apparatus, a
test file and a `devel/` script, on which no external consumer of the package
relies.

**In:** RR21 B3 — `exact_oracle.py` emitting each case's exact `v` and `u` as
hi/lo double pairs, committed in `tests/testthat/test-axes-certificate.R` — so
the bracket's bit-identity precondition
(`tests/testthat/test-axes-certificate.R:202-234`) can drop its
shipped-pricing half and keep only the anchor-matrix half, which reproduces
everywhere; a non-empty-domain assertion in the packaged file; a
safety-factor sensitivity plant; `cert_n` counting ratios rather than
`cert_line()` calls; and `sweep_ok` and `reach_ok` simplified so neither can
report PASS having checked nothing.

**Out:** extending the bracket to M113's third certificate field → M113. The
shared-predicate and nestedness fences → M114. The recorded cost figure, the
`dd_*` namespace prefixing, the certificate's double evaluation per fit and
the rest of the M108/M111 cosmetic residue → the ROADMAP degeneracy candidate
row.

## Acceptance criteria

- [x] AC1 `exact_oracle.py` emits, for each certificate case, the exact `v` and
      `u` as hi/lo double pairs via `%a`, and those pairs are committed in
      `tests/testthat/test-axes-certificate.R`.
- [x] AC2 The bracket's precondition no longer reads the shipped double
      pricing: on this machine, with that pricing perturbed so it no longer
      matches the figures the frozen values were measured on,
      `devtools::test(filter = "axes-certificate")` reports zero skips and the
      bracket assertions still run.
- [x] AC3 `tests/testthat/test-axes-certificate.R` reddens, rather than passing
      green, when its certificate case list is emptied.
- [x] AC4 Raising `axes_certificate_safety_factor` from 10 to 100 reddens at
      least one assertion in the packaged suite.
- [x] AC5 `exact_oracle.R` reports the number of ratios it formed rather than
      the number of `cert_line()` calls it made, and `sweep_ok` and `reach_ok`
      are each replaced by a form derived from the values their own loop
      collects, so neither can report PASS on an empty domain; each of the two
      is shown to fail with its domain emptied.
- [x] AC6 `devtools::test()` clean; `devtools::document()` no diff and no
      unresolved-link warning at pinned `cli.width`;
      `devtools::check(args = "--no-manual")` 0 errors / 0 warnings / 0 notes.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6, T7, T8, T9

## Tasks

- [x] T1 Extend `exact_oracle.py` to emit each case's exact `v` and `u` as
      hi/lo double pairs; commit them in the test file beside the existing
      frozen figures.
- [x] T2 Rewrite `cert_skip_unless_reproduced()` to compute each case's true
      relative error on the running machine from the committed exact pair, and
      drop the shipped-pricing half of the precondition; run the AC2
      perturbation and record the skip count.
- [x] T3 Add the non-empty-domain assertion; prove it by emptying the case
      list, then restore.
- [x] T4 Run the safety-factor plant against the packaged suite; record which
      assertions redden, revert and verify the tree clean.
- [x] T5 Fix `cert_n` to count formed ratios; replace `sweep_ok` and
      `reach_ok` with forms derived from their loops' collected values; empty
      each domain in turn and record the failure.
- [x] T6 Run the profile's verify and consistency-gate slot. Then, at the
      review gate — where the PR and therefore the CI run exist — read that run
      and record which platforms the bracket asserted on: a gate observation,
      not a criterion.
- [x] T7 Repair the counterexample-B decade windows that reddened on
      ubuntu-latest: assert the machine's own measured error against the
      package's stated accuracy target rather than against a window fitted to
      one machine's figures; prove the new form can still fail.
- [x] T8 The four carried review findings, all in the same test file: compare
      the anchor matrix numerically rather than through `%a` text; pass
      `cert_bracket()`'s label into its expectations; check the matrix before
      reporting a pricing refusal as a regression; assert each committed case's
      value-array lengths.
- [x] T9 Re-run the profile's verify slot after the repair.

## Work log

- 2026-08-30: created by /milestone-plan.
- 2026-08-30: criteria audit ran in REDUCED mode (declared internal tier), one fresh-context [O] reader that authored none of the criteria, as part of the joint M113/M114/M115 run. Two findings fixed before writing: AC4's "packaged suite **or** the oracle run" disjunction let the devel script carry the promise this milestone exists to restore, and was narrowed to the packaged bracket; AC3's recording and restoration clauses moved to T3 as instrument bookkeeping. Its note that no criterion verifies the title's off-macOS claim is deliberate and recorded below.
- 2026-08-30: the internal-tier criteria standard is why AC2 is scoped to this machine rather than to the three CI platforms: a demonstration family spanning environment boundaries is itself a finding at that tier. The cross-platform observation is T6's gate step. Residual risk recorded: a green local AC2 with the CI run unread would leave the milestone's own goal unverified.
- 2026-08-30: plan gate chose retiring the precondition's shipped-pricing half via RR21 B3's exact emission over widening the frozen floor by a platform tolerance, because M108's mini gate measured a 100x slack as exceeding the 10x the dropped-safety-factor plant moves, so the tolerance would retire that plant's coverage; falsified by the exact hi/lo pair proving insufficient to recover a case's true error.
- 2026-08-30: plan gate chose simplifying `sweep_ok` and `reach_ok` to forms derived from their loops' collected values over adding a `cert_ok`-style accumulator count, on the user's answer at the checker-regress question, which took the subtractive option; falsified by the derived form proving unable to express either flag's condition.
- 2026-08-30: T1 — `exact_oracle.py` emits each case's exact `v`, `v_naive` and `u` as hi/lo hex double pairs; `exact_oracle.R` parses them and, under `CERT_EMIT=1`, prints a paste-ready `cert_frozen` block carrying matrix and exact values from one construction, so the two cannot describe different matrices. `v_naive` travels beside `v` because it is the `fiml_ratio` field's denominator and AC2 brackets all three fields.
- 2026-08-30: T2 — the precondition's shipped-pricing half is gone; `cert_true_error()` measures the running machine's own relative error against the committed exact pairs, and `cert_bracket()` asserts on both branches (at the floor: the machine's error is below what the floor certifies; above it: the [1, 1e3] bracket), so no case can report PASS having asserted nothing. The M113 closed-form case's `fiml_ratio` skip is replaced by the same two-branch form, closing the second surface of the platform-reach gap.
- 2026-08-30: AC2 demonstration — with `axes_v_pricing()`'s corrected arm perturbed by a relative 3e-13, the pre-M115 test file skipped all six cases naming the shipped pricing, while the new file reported zero skips and every bracket assertion ran (its four failures are the two dyadic closed-form cases, which the plant makes non-exact by construction). Plant reverted, `devtools::test()` clean at FAIL 0 / SKIP 1, that skip pre-existing in `test-axes-scaled-fit.R`.
- 2026-08-30: T3 — the non-empty-domain assertion (`AC3: the anchor case list is not empty`) shipped in the T1-T2 commit, since it lives in the same helper block; proved here by replacing `cert_anchors()`'s body with `list()`: it reddens with two failures naming the expected length and the expected ids, where before the change an emptied list generated no per-case tests at all and the file reported PASS. Two further errors surfaced in the n-invariance and planted-perturbation tests, which index the list directly. List restored, file green.
- 2026-08-30: T4 — the safety-factor plant. Before this milestone nothing reddened: every assertion mentioning the factor computed its expectation from `axes_certificate_safety_factor` itself, so estimate and expectation moved together. Writing 10 down at the three sites (`cert_floor`, and `f` in the planted-perturbation test) makes `axes_certificate_safety_factor <- 100` fail 13 assertions in 3 tests, all in `test-axes-certificate.R`: 8 in the planted-perturbation test (the `se` and `cval` upper bounds at each of the four deltas), 3 in the dyadic closed-form test and 2 in the quotient closed-form test (each field asserted at the floor). Plant reverted, tree clean against HEAD, file green.
- 2026-08-30: T5 — `cert_n` now increments where the ratio is formed, not on entry to `cert_line()`, so a case going exact would report 17 of 18 and redden instead of printing eighteen comparisons it had not made; `sweep_ok` and `reach_ok` are derived from the ratio vectors their own loops collect, checked against written-down counts (3 and 5) rather than against the domain's own length, which would go on matching with both emptied. The two verdict lines now name how many ratios they compared. Measured: with both domains emptied the pre-M115 accumulator form printed SWEEP PASS and REACHABLE PASS having compared nothing, while the new form prints `0 of 3` FAIL and `0 of 5` FAIL and the script exits 1. Emptying the reachable domain alone also drops the certificate count to 3 of 18, FAIL. Domains restored, script exits 0.
- 2026-08-30: minor amendment to T6 — the CI read is moved to the review gate. `R-CMD-check.yaml` triggers only on push to the default branch or on `pull_request`, and `/milestone-review` is what opens the PR, so no CI run exists while implement is running. Nothing in AC6 or any other criterion moves; the CI read was already recorded as a gate observation rather than a criterion.
- 2026-08-30: T6 — profile verify clean on this machine: `devtools::document()` no diff and zero `resolve link` lines at `cli.width = 500`; `devtools::test()` FAIL 0 / WARN 5 / SKIP 1 / PASS 9115, the one skip pre-existing in `test-axes-scaled-fit.R:921` (fixture generated under a different R or lavaan version) and outside this diff; `devtools::check(args = "--no-manual")` Status OK, 0 errors / 0 warnings / 0 notes in 30m 30s. No NEWS.md entry: the milestone changes a test file and a `devel/` script only, with no user-visible surface. The CI read stays owed at the review gate.
- 2026-08-30: `Depends on: M113` because M113 extends the oracle's certificate case list, which T1 and T2 both read.
- 2026-08-30: repair gate — the counterexample-B repair asserts the machine's own error against `axes_degeneracy_delta_star` rather than a widened window or nothing at all, on the user's "decide for me" at that question; the other two answers took all four cheap findings into this repair and sent the all-skip blindness and the at-the-floor branch selection to the ROADMAP row.
- 2026-08-30: T7 — the three decade windows at counterexample B are replaced by two assertions against `axes_degeneracy_delta_star` (1e-4), the package's own accuracy target: at an ill-conditioned matrix the size of the rounding error is a property of the machine, and what is a property of the matrix is that no machine gets within the target on it. Margin is three decades or more on both machines seen (macOS 0.0341 / 4.890, ubuntu-latest 0.124 / 0.42). Shown able to fail: with `cxb`'s committed exact words overwritten by this machine's own shipped doubles — the case made exact by construction — both new assertions redden at `:419` and `:420` reading 0.00000 against 0.00010, plus three bracket failures. Plant reverted, file green at FAIL 0 / SKIP 0 / PASS 340. Two `cert_rel` comment figures that named only the authoring machine now name both machines with their date.
- 2026-08-30: T8 — the four carried findings, all in `test-axes-certificate.R`. The anchor-matrix precondition compares doubles (`identical(sigma[upper.tri(sigma)], as.numeric(fz$sig))`) instead of `%a` text, which `?sprintf` says is not uniquely defined across platforms; it now runs BEFORE the shipped pricing is called, so a machine building a different matrix skips rather than being told its refusal is a regression; `cert_bracket()`'s label reaches every expectation, carrying case and field; and each committed case's array lengths are asserted against written-down shapes. Shown able to fail, three plants, each reverted: truncating `a4`'s `v_hi` to one word reddens only the new length check (`Expected a4 v_hi length to be identical`, 1 vs 2) — the bracket recycled and passed, which is the finding; flipping one bit of `c4`'s committed matrix skips that case alone naming the reason; and a bracket failure now reads `Expected cxb se: estimate >= true_rel` rather than two unnamed numbers. File green at FAIL 0 / SKIP 0 / PASS 382, up 42 from the six cases' seven new length assertions.
- 2026-08-30: the two findings the repair gate sent on rather than fixing — nothing distinguishing six cases asserted from six skipped, and the at-the-floor branch selected by a value coincidence — are recorded on the ROADMAP degeneracy candidate row with their promotion condition. That row's file is still over its byte budget (26,567 against 24,000, up 647 from this addition); the standing remedy is a graduate-or-prune pass needing maintainer decisions, unchanged by this milestone.
- 2026-08-30: T9 — verify re-run on the repaired tree at `0d875497`: `options(cli.width = 500); devtools::document()` zero `resolve link` lines and no diff; `devtools::test()` FAIL 0 / WARN 5 / SKIP 1 / PASS 9156, the one skip the pre-existing `test-axes-scaled-fit.R:921` fixture skip; `devtools::check(args = "--no-manual")` Status OK, 0 errors / 0 warnings / 0 notes in 8m 1.9s. AC6's CI half is re-read at the review gate, where the pushed branch has a run.
- 2026-08-31: CI read on the repaired branch, `R-CMD-check` run 33347293145 on PR #146 — `ubuntu-latest` pass (FAIL 0 | WARN 4 | SKIP 78 | PASS 8594), `macos-latest` pass (FAIL 0 | WARN 5 | SKIP 78 | PASS 8594), `windows-latest` pass (FAIL 0 | WARN 4 | SKIP 79 | PASS 8588), `matrix` and `pkgdown` pass. No job log carries an anchor-matrix skip line, so the six certificate cases asserted on all three platforms, the ubuntu error that returned this milestone is gone, and AC6's procedure is green everywhere it was read. Recorded here as an implement observation; the review gate re-reads it as its own evidence.
- 2026-08-30: review gate FAILED, defect return 1. AC6 fails: `R CMD check` errors on ubuntu-latest (PR #146, run 33343808365), `FAIL 2 | WARN 4 | SKIP 78 | PASS 8551`, both failures this milestone's own `test-axes-certificate.R:408-409` — the counterexample-B decade windows (`true_rel$se` in (1e-2, 1e-1), `true_rel$cval > 1`) are the authoring machine's figures, and ubuntu measures 0.124 and 0.42. macos-latest and windows-latest pass with zero certificate skips. AC1-AC5 verified; five further [O] findings recorded in the Review section for triage in the repair.
- 2026-08-30: correcting the entry above it — the CI-read line reading `2026-08-31` was written by commit `de5ef9fb`, timestamped 2026-08-30, and was inserted above the first gate's failure line rather than appended; both stand as history (IP4) and are superseded by this line. Chronological order of the log's tail is: the first gate's failure, then the repair tasks T7-T9, then that CI read, then this.

## Decisions

## Review

_PR: https://github.com/jmgirard/circumplex/pull/146 (draft while the review ran). Branch synced: `origin/master` at e0da58bd had not moved since the branch was cut, so no merge was needed._

### First pass — acceptance-criterion evidence (2026-08-30)

- **AC1 — verified 2026-08-30.** Regenerated the whole block from committed material with `CERT_EMIT=1 Rscript devel/degeneracy-oracle/exact_oracle.R` and compared its `cert_frozen` against the one committed in `tests/testthat/test-axes-certificate.R` by sourcing both and comparing after name-sorting: names identical, values identical, 6 cases, 68 hi/lo hex double words. The only textual difference is list order (`cxb` emitted first, committed last), which no lookup reads — access is by name. `exact_oracle.py` prints `HEX_V_HI/LO`, `HEX_VNAIVE_HI/LO`, `HEX_U_HI/LO` via `dd_hex()`, which rounds the exact `Fraction` to a double and the remainder to a second, each printed with `%a`.
- **AC5 — verified 2026-08-30.** Three fresh plants against `devel/degeneracy-oracle/exact_oracle.R`, each run to completion and reverted, the tree verified clean after each. (i) `SWEEP_T <- numeric(0)`: `SWEEP (0 of 3 ratios ...): FAIL`, script exits 1, the other two verdicts unaffected. (ii) `reach_cases <- list()`: `REACHABLE (0 of 5 ...): FAIL` and `CERTIFICATE (3 of 18 ratios formed ...): FAIL`, exit 1. (iii) `cert_line("SE", cert_b$se, 0)` — one case forced onto the exact/floor-only branch, which forms no ratio: `CERTIFICATE (17 of 18 ratios formed ...): FAIL`, exit 1, where a count of `cert_line()` calls would have read 18 and passed. `cert_n <<- cert_n + 1L` sits after the `true_rel == 0` early return, on the ratio-forming path only; `sweep_ok` and `reach_ok` are each `length(<the loop's own collected vector>) == <a written-down count>` conjoined with the per-ratio window, so neither can hold on an emptied domain. Baseline (unplanted) run exits 0 with all four verdicts PASS.
- **AC2 — verified 2026-08-30.** Perturbed `axes_v_pricing()`'s corrected arm by a relative `3e-13`, so the shipped double pricing no longer matches the figures the pre-M115 frozen errors were measured on, and ran `devtools::test(filter = "axes-certificate")` twice under that one plant. With the M115 file: `FAIL 4 | WARN 0 | SKIP 0 | PASS 337` — zero skips, every bracket assertion ran; the four failures are the two closed-form cases (`test-axes-certificate.R:680,685,687,773`), whose assertions the plant makes non-exact by construction. With `git show master:tests/testthat/test-axes-certificate.R` in place under the same plant: `FAIL 4 | WARN 0 | SKIP 6 | PASS 295`, the six skips each naming "does not reproduce the shipped double pricing bit for bit" at `a4`, `a5`, `c4`, `b9a`, `b9b`, `cxb`. Plant and file both reverted; `git status` clean apart from this milestone file.
- **AC3 — verified 2026-08-30.** Replaced `cert_anchors()`'s body with `list()` and ran `devtools::test(filter = "axes-certificate")`: `FAIL 4 | WARN 0 | SKIP 0 | PASS 271`. Two failures are the non-empty-domain test itself (the written-down length 5 and the written-down id vector, at `:322` and `:323` of the planted file), and two are errors from the n-invariance and planted-perturbation tests indexing the list directly (`cert_anchors()[[5L]]` and `[[1L]]`, subscript out of bounds). Without that test the emptied list generates no per-case tests at all and the file reports PASS. List restored, tree clean.
- **AC4 — verified 2026-08-30.** Set `axes_certificate_safety_factor <- 100` in `R/axes_certificate.R` and ran the packaged suite. `devtools::test()` hit testthat's default 10-failure cap, so the count was re-measured under `testthat::set_max_fails(Inf); devtools::test(filter = "axes-certificate")`: `FAIL 13 | WARN 0 | SKIP 0 | PASS 328`, all 13 in `test-axes-certificate.R` and in 3 tests — 8 in the planted-perturbation test (`:614` and `:616`, the `se` and `cval` bounds at each of four deltas), 3 in the dyadic closed-form test (`:685`, `:686`, `:687`) and 2 in the quotient closed-form test (`:773`, `:774`), each of the last five an `expect_identical(cert$<field>, cert_floor)` reading `4.44e-14` against the written-down `4.44e-15`. Constant reverted, tree clean.
- **AC6 — verified 2026-08-30.** Fresh runs on a clean tree at `db50cb4a`: `options(cli.width = 500); devtools::document()` exits 0, leaves `git status` clean (no diff) and emits zero lines matching `resolve link`; `devtools::test()` reports `FAIL 0 | WARN 5 | SKIP 1 | PASS 9115`, the one skip pre-existing at `test-axes-scaled-fit.R:921` ("fixture was generated under a different R or lavaan version") and outside this diff; `devtools::check(args = "--no-manual")` reports `Status: OK`, 0 errors / 0 warnings / 0 notes in 28m 11.6s. **NOT TICKED — the criterion names a procedure and no machine, and that procedure errors on ubuntu-latest.** PR #146's `R-CMD-check` run 33343808365: `macos-latest` pass, `windows-latest` pass, `ubuntu-latest` **fail** — `Status: 1 ERROR`, `FAIL 2 | WARN 4 | SKIP 78 | PASS 8551`, both failures in this milestone's own new code. See the T6 CI read and F0 below.

### First pass — T6 gate observation, the CI read (2026-08-30)

`R-CMD-check` run 33343808365 on PR #146, read 2026-08-30. The bracket's reach is restored where the milestone aimed: **every one of the six cases asserted on all three platforms — zero certificate skips anywhere**, including `windows-latest`, which before M115 reported nothing at all (M108 run 32752082137, M113 run 33329301066). `macos-latest` pass (19m12s), `windows-latest` pass (21m58s), `matrix` pass, `pkgdown` pass. `ubuntu-latest` **fail** (24m46s).

### First pass — findings (2026-08-30)

Three fresh-context reviewers, distinct evidence bases. The [S] blame-history lens and the [S] prior-review-record lens each reported no findings (the latter after a `gh api .../pulls/comments` probe came back empty, so the PR-thread walk was not paid for). The [O] diff-bug lens reported seven, ranked; F0 below is the review's own CI read, ranked above all of them.

**F0 (gate failure; from the T6 CI read). The two decade-window assertions this milestone added at counterexample B are a frozen one-machine figure asserted as if it described the matrix — the same defect the milestone exists to remove, reintroduced at a new site.** `test-axes-certificate.R:407-409` asserts `true_rel$se` in `(1e-2, 1e-1)` and `true_rel$cval > 1`. Those are the authoring machine's numbers (macOS measures 3.41e-02 and 4.890). On `ubuntu-latest` the same fixture prices to `true_rel$se` = 0.124 (`Expected true_rel$se < 0.1. Actual comparison: 0.124 >= 0.100`) and `true_rel$cval` = 0.42 (`Expected true_rel$cval > 1. Actual comparison: 0.42 <= 1.00`), a different BLAS giving counterexample B — the deliberately ill-conditioned case — a materially different rounding error. `cert_bracket()` itself passed on all three platforms, so the milestone's mechanism works; what fails is the descriptive window written beside it. Net effect on CI: before M115 the certificate cases skipped off-macOS, after M115 they redden there.

**F1 ([O], rank 1). The surviving precondition compares `%a`-formatted strings, and `?sprintf` states `%a` is not uniquely defined across platforms.** `cert_true_error()` gates on `identical(cert_hex(sigma[upper.tri(sigma)]), fz$sig)`. A platform agreeing on every bit but formatting differently (unsuppressed trailing zeros, a non-normalized leading digit — both `0x1.b4d8379580e2p-1` and `0x0p+0` appear in the committed `sig`) would skip all six cases, which is the vacuity M115 exists to remove. Verified at review: `identical(sigma[upper.tri(sigma)], as.numeric(fz$sig))` holds for all six committed cases and routes through R's own `R_strtod` rather than the platform's formatter; R's hex parse round-trips exactly for every value in play (it underflows only at `0x1p-1074`, a denormal none of these are).

**F2 ([O], rank 2). Nothing in the packaged file distinguishes "six cases asserted" from "six cases skipped", so an all-skip run is green.** Recorded as residual risk in the work log and deferred to T6's CI read, which this review ran — but the criterion defining the milestone is not self-enforcing in the artifact.

**F3 ([O], rank 3). `cert_bracket()` takes an `lbl` argument at all seven call sites and never uses it.** The three expectations inside carry no `label =`, so a CI failure reports only `est`/`true_rel` and names neither the field nor the case.

**F4 ([O], rank 4). `cert_true_error()` runs the refusal check before the matrix check.** A refusal is reported via `testthat::fail()` saying "an admitted geometry, so this is a regression, not a platform difference" — a conclusion only warranted once `sig` has confirmed this machine builds the admitted geometry.

**F5 ([O], rank 5). The at-the-floor branch is selected by `identical(est, cert_floor)`, a value coincidence rather than the estimator's state.** Raise `axes_certificate_safety_factor` and no case is recognized at-floor; all six take the ratio branch, where a 100x ratio still sits inside the 1e3 ceiling.

**F6 ([O], rank 6). `cert_frozen`'s per-case vector lengths are not asserted against `n_comp`, so a truncated regeneration paste recycles.** `cert_rel()` is elementwise; the AC3 test checks the case ids but not each case's array lengths.

**F7 ([O], rank 7; noted as not a defect). `cert_line()`'s exact-case branch asserts only `cert > 0 && is.finite(cert)`, applying no ceiling** — now covered by `cert_n` failing the 18-count, which is AC5's design. Flagged so it reads as a choice rather than an oversight.

### First gate outcome (2026-08-30)

**Returned to `in-progress`.** AC6 names `devtools::check(args = "--no-manual")` at 0 errors and names no machine; that procedure reports `Status: 1 ERROR` on `ubuntu-latest`. Under the never-reinterpret rule the criterion is not narrowed to the authoring machine at review, so this is a defect return, not an amendment return: F0 falsifies AC6 inside the domain the criterion's own wording quantifies over. First defect return on this milestone. AC1–AC5 stand verified on the evidence recorded above; F1 and F3–F6 are carried into the repair for triage there.


### Second pass — acceptance-criterion evidence (2026-08-30)

Re-run in full on the repaired tree at `de5ef9fb`; the first pass's evidence was
taken before T7-T9 rewrote the same test file, so none of it is reused. Branch
synced: `origin/master` is still at `e0da58bd`, unmoved since the branch was cut,
so no merge was needed. PR #146 (draft while the review ran).

- **AC1 — verified 2026-08-30.** `CERT_EMIT=1 Rscript devel/degeneracy-oracle/exact_oracle.R` regenerated the whole `cert_frozen` block from committed material; sourcing the emitted block and the one committed in `tests/testthat/test-axes-certificate.R` and comparing after name-sorting gives identical names and identical values — 6 cases, 68 hi/lo hex words across `v`, `v_naive` and `u`, 137 `sig` words. The only textual difference is list order (`cxb` emitted first, committed last), which no lookup reads: access is by name and the AC3 test sorts. The same run reported ANCHORS/SWEEP/REACHABLE/CERTIFICATE all PASS and exited 0.
- **AC2 — verified 2026-08-30.** `axes_v_pricing()`'s corrected arm perturbed by a relative `3e-13`, so the shipped double pricing no longer matches the figures the pre-M115 frozen errors were measured on, and `devtools::test(filter = "axes-certificate")` run twice under that one plant. With the M115 file: `FAIL 4 | WARN 0 | SKIP 0 | PASS 378` — zero skips, every bracket assertion ran; the four failures are the two closed-form cases, which the plant makes non-exact by construction. With `git show master:tests/testthat/test-axes-certificate.R` in place under the same plant: `FAIL 4 | WARN 0 | SKIP 6 | PASS 295`, the six skips each naming "does not reproduce the shipped double pricing bit for bit" at `a4`, `a5`, `c4`, `b9a`, `b9b`, `cxb`. Both plants reverted, `git status` clean.
- **AC3 — verified 2026-08-30.** `cert_anchors()`'s body replaced by `list()`: `FAIL 4 | WARN 0 | SKIP 0 | PASS 312`. Two failures are the non-empty-domain test itself (`:342` expecting length 5 against 0, `:343` expecting the id vector against an empty one) and two are subscript-out-of-bounds errors from the n-invariance and planted-perturbation tests indexing the list directly (`cert_anchors()[[5L]]`, `[[1L]]`). Without that test an emptied list generates no per-case tests at all and the file reports PASS. Restored, tree clean.
- **AC4 — verified 2026-08-30.** `axes_certificate_safety_factor` raised from 10 to 100 in `R/axes_certificate.R`, measured under `testthat::set_max_fails(Inf)`: `FAIL 13 | WARN 0 | SKIP 0 | PASS 369`, all 13 in `test-axes-certificate.R` and in 3 tests — 8 in the planted-perturbation test (`:657` and `:659`, the `se` and `cval` bounds at each of four deltas), 3 in the dyadic closed-form test (`:728`-`:730`) and 2 in the quotient closed-form test (`:816`, `:817`). Constant reverted, tree clean.
- **AC5 — verified 2026-08-30.** Three fresh plants against `devel/degeneracy-oracle/exact_oracle.R`, each run to completion and reverted, the tree verified clean after each. (i) `SWEEP_T <- numeric(0)`: `SWEEP (0 of 3 ratios ...): FAIL`, exit 1, the other verdicts unaffected. (ii) `reach_cases <- list()`: `REACHABLE (0 of 5 ...): FAIL` and `CERTIFICATE (3 of 18 ratios formed ...): FAIL`, exit 1. (iii) `cert_line("SE", cert_b$se, 0)` — one case forced onto the exact/floor-only branch, which forms no ratio: `CERTIFICATE (17 of 18 ratios formed ...): FAIL`, exit 1, where a count of `cert_line()` calls would have read 18 and passed. Unplanted baseline exits 0 with all four verdicts PASS.
- **AC6 — verified 2026-08-30.** Local, on a clean tree at `de5ef9fb`: `options(cli.width = 500); devtools::document()` exits 0, leaves `git status` clean and emits zero lines matching `resolve link`; `devtools::test()` reports `FAIL 0 | WARN 5 | SKIP 1 | PASS 9156`, the one skip pre-existing at `test-axes-scaled-fit.R:921` and outside this diff; `devtools::check(args = "--no-manual")` reports `Status: OK`, 0 errors / 0 warnings / 0 notes in 11m 9.1s. The procedure names no machine, so it is also read on the other two: `R-CMD-check` run 33348851037 on PR #146 reports `Status: OK` on all three — `ubuntu-latest` `FAIL 0 | WARN 4 | SKIP 78 | PASS 8594`, `macos-latest` `FAIL 0 | WARN 5 | SKIP 78 | PASS 8594`, `windows-latest` `FAIL 0 | WARN 4 | SKIP 79 | PASS 8588` — with `matrix` and `pkgdown` also passing. The ubuntu error that returned this milestone at the first gate is gone.

### Second pass — T6 gate observation, the CI read (2026-08-30)

`R-CMD-check` run 33348851037 on PR #146, at `de5ef9fb`. The milestone's own goal, read from the job logs rather than assumed: **not one of the three platform logs carries an anchor-matrix skip line, and none carries a `test-axes-certificate` skip of any kind** — so all six certificate cases asserted on `ubuntu-latest`, `macos-latest` and `windows-latest` alike. Before M115 `windows-latest` reported nothing at all (M108 run 32752082137, M113 run 33329301066).

### Second pass — consistency gate (2026-08-30)

`cairn_validate.py` exits 0, every check PASS; its 47 advisory WARNs are all pre-existing `work-log format` lines in M7's log and outside this diff. No DESIGN.md principle changed in this diff, so `cairn_impact.py` does not apply. Profile `consistency-gate` slot: `document()` no diff and zero `resolve link` lines (above); no generated file hand-edited (the no-diff `document()` covers it); `README.Rmd` unmodified by this diff; `pkgdown::check_pkgdown()` "No problems found"; no NEWS entry owed — the diff touches a test file, a `devel/` script and tracking files, with no user-visible surface; no new top-level file, and `check()` reports 0 notes; full `check()` clean. Master watches: the five newest push runs on `master` reaching a verdict are `success` for both `R-CMD-check.yaml` and `test-coverage.yaml`. `Rscript tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R` (5 scenarios ok) and `tools/check-branch-protection.R` (both rulesets match `tools/branch-protection.json`) each exit clean.

### Second pass — findings (2026-08-30)

Three fresh-context reviewers, distinct evidence bases. The **[S] prior-review-record lens reported no findings**: the archived `## Review` sections touching these files carry no file:line findings checkable against this diff, and a `gh api .../pulls/comments?per_page=1` probe returned empty, so the PR-thread walk was not paid for; it confirmed instead that F0, F1, F3, F4 and F6 from the first gate are each fixed at the sites named, and that F2 and F5 are recorded debt rather than reintroduced defects. The **[S] blame-history lens reported one finding** — the dead `cert_hex()` helper, which the [O] lens also found and which is carried below as F3' — and explicitly cleared the rest: nothing in the diff undoes a deliberate past addition, resurrects a fixed bug or contradicts a D-entry, and the counterexample-B rewrite is in-scope and evidenced. The **[O] diff-bug lens reported eleven**, ranked; it also recorded what it checked and found clean (the `cert_n` increment sitting after the exact-case early return, `sweep_ok`/`reach_ok` reading written-down counts rather than their domains' own lengths, the three `*_EXPECTED` literals being independent, only two safety-factor sites and neither reading the package constant, `dd_hex()`'s ~106-bit split, and the emitted-vs-committed list order being harmless).

**F1' ([O], rank 1). The `se` and `cval` floor identities at the two closed-form cases assert that every machine prices them exactly, which is the F0 defect class at the one site the milestone most nearly touched.** `test-axes-certificate.R:816-817` and `:728-730` assert `expect_identical(cert$<field>, floor_est)` unconditionally; the comment beside them concedes the grounds are "on every platform measured". The third field at the same matrix (`fiml_ratio`) was measured platform-divergent at M113 and M115 converted it from a `skip()` to a two-branch `cert_bracket()`, while these two kept exact-identity assertions. A fourth platform, or a BLAS/R update, committing one more ulp on the corrected arm makes `cert$se` leave the floor and `R CMD check` error — the ubuntu red that returned this milestone. Pre-existing lines, edited in this diff only to read `cert_floor`.

**F2' ([O], rank 2). Nothing pins the *measured* side's component count against the committed pair's length, so `cert_rel()` can still recycle.** `test-axes-certificate.R:312-314` vs the shape test at `:373-383`. The F6 repair pins `length(fz$v_hi)` against a written-down count but never checks `length(v$corrected)` against it, and R recycles a length-2 `hi` against a length-4 `hat` silently. A change to `axes_se_derivs()`/`axes_fits_zeta1()` making `a4` fit four components would price components 3 and 4 against components 1 and 2's exact values. It reddens rather than passing, so this is a diagnosability defect, not a vacuity.

**F3' ([O] rank 3; also the [S] blame lens's only finding). `cert_hex()` is dead code that still reads as the live precondition.** `test-axes-certificate.R:243`. Its one caller was replaced by the F1 numeric comparison. Leaving it defined beside a comment explaining why `%a` text comparison was abandoned invites a later case to reach for it and re-plant the all-skip vacuity.

**F4' ([O], rank 4). `cert_root_rel()` returns NaN rather than failing for a relative variance error at or below −100%.** `test-axes-certificate.R:259`. Errors past −1 are demonstrably reachable in this file's domain (`du` at counterexample B is ≈ −4.89 on macOS); `cval` escapes only by taking `abs(du)` with no square root. A sign-flipped `v_hat` would give `true_rel$se = NaN` and a comparison failure naming neither the sign flip nor the field.

**F5' ([O], rank 5). `cert_rel()` divides by `hi + lo`, which is zero when the exact quantity is zero.** `test-axes-certificate.R:253`. `axes_accuracy_certificate()` guards this case explicitly (`R/axes_certificate.R:462`); `cert_true_error()` has no counterpart, so a case with exact `u = 0` would give `cval = Inf` and `expect_lte(est, 1e3 * Inf)` would pass vacuously. Not reachable with the six committed cases — every `u_hi` was checked — so latent.

**F6' ([O], rank 6). The oracle driver's switch from a named numeric vector to a list turns a missing oracle key from an error into a silent `NULL`.** `devel/degeneracy-oracle/exact_oracle.R:70-79`. A vector's `[[` on an absent key raised "subscript out of bounds"; a list's returns `NULL`. If `exact_oracle.py` stopped emitting one `HEX_*` line, `cert_emit_block()` would deparse `v_hi = NULL` into a paste-ready block. The packaged AC3 length test catches it downstream, so the harm is bounded, but the generator no longer fails at the point of loss.

**F7' ([O], rank 7; reported as not a defect). `expect_gt(true_rel$cval, axes_degeneracy_delta_star)` is an empirical generalization from two machines dressed as a property of the matrix.** `test-axes-certificate.R:452`. The reviewer's own judgment is that it is sound — margins of 3.6 and 4.7 decades, and a catastrophic-cancellation mechanism scaling as `kappa^2 * eps ≈ 1e-2` at kappa 6.7e6 — and the comment states the reasoning honestly. Recorded as the residual risk the T7 repair chose to accept.

**F8' ([O], rank 8). A builder that changes a case's matrix *dimension* skips rather than reddens.** `test-axes-certificate.R:290-296`. A length mismatch makes `identical()` FALSE, which routes to `skip()`, so `m106_family_b()` returning an 8x8 instead of 9x9 silently disarms that case. The `expect_equal(m106_kappa(...))` guard sits outside the precondition and would probably catch it, but it asserts a scalar to 1e-3 relative and nothing forces a resized matrix's kappa off the pinned value.

**F9' ([O], rank 9). The `sig` precondition pins only the strict upper triangle, not the diagonal.** `test-axes-certificate.R:296`. The exact `v` and `u` are functions of the whole matrix; every case here is a correlation matrix and the oracle asserts `all.equal(cov2cor(S), S)` for the fixture, but the packaged file never asserts a unit diagonal, so `sig` under-determines the matrix the committed values describe. Nothing in the builders touches the diagonal.

**F10' ([O], rank 10). The newest work-log entry was inserted above the last line rather than appended, and carries tomorrow's date.** `cairn/milestones/M115-certificate-bracket-reach.md:122`. The CI-read entry sits above the first gate's "review gate FAILED" line, so a reader taking the log's tail as current state reads the milestone as still failing AC6; and it is dated 2026-08-31 where the commit that wrote it (`de5ef9fb`) is timestamped 2026-08-30. Append-only and absolute-dates are both tracking rules, and history is superseded rather than edited.

**F11' ([O], rank 11). The Review section's first-pass AC6 bullet is stale against the recorded green run.** Not a code defect — the finding is that the current gate has to reconcile it, which this second pass's own AC6 evidence and this relabelling do.
