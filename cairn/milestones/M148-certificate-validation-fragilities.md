<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M148: Close the five remaining accuracy-certificate validation-layer fragilities

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M147
- **Driving RR:** —
- **Principles touched:** IP3
- **Resolves:** —
- **Surface tier:** internal — repairs to the certificate test suite and the devel oracle driver, no shipped behavior
- **Branch/PR:** `m148-certificate-validation-fragilities`

## Goal

The five latent defects DESIGN.md's Known fragilities records in the accuracy certificate's validation layers can no longer turn a real regression green.

## Scope

**In:** the five items the Known-fragilities paragraph will list once M147 retires the constant admission predicate. In `tests/testthat/test-axes-certificate.R`: the measured side's length unpinned at the three priced-loop `cert_rel()` sites; `cert_root_rel()` returning NaN below a relative variance error of -100%; the planted-perturbation layer not reaching the quotient's denominator; the counterexample-B block bracketing a sentinel as if priced. In `devel/degeneracy-oracle/exact_oracle.R`: a missing oracle key becoming a silent NULL. DESIGN.md's paragraph is corrected in place at the end.

**Out:** naming which sentinel route fired inside `axes_accuracy_certificate()`, because that needs a route tag in shipped code and is not internal-tier; it stays a Known-fragilities note if the block's single failure proves too coarse. A cross-process demonstration of the oracle-driver guard, for the same tier reason; the guard is asserted in process on a stubbed key list. The `cval` bracket's aggregation, dropped at the audit: `u_hi` and `u_lo` are pinned to length 1 (test-axes-certificate.R:653-654), so the bracket is already a scalar.

## Acceptance criteria

- [x] AC1: At the three priced-loop `cert_rel()` sites in `tests/testthat/test-axes-certificate.R` (the calls at lines 553-555 at `dcb98f45`, re-enumerated by `grep -n "cert_rel(" tests/testthat/test-axes-certificate.R` at implement time), the measured vector's length is asserted equal to the committed exact pair's length before the comparison. A measured vector one element short reddens each of the three sites.
- [x] AC2: `cert_root_rel()` fails with a message naming the input at a relative variance error below -100%, where it returned NaN. A test plants that input and asserts the failure's message.
- [x] AC3: At the anchor the planted-perturbation layer drives (`cert_anchors()[[1L]]`), a perturbation planted in the naive arm's quadratic forms reddens the `fiml_ratio` bracket, as the layer's existing plants redden the `se` and `cval` brackets.
- [x] AC4: In `devel/degeneracy-oracle/exact_oracle.R`, the function that parses the Python driver's output stops with a message naming the missing key when a key it reads is absent. The guard is asserted in process against a stubbed output with one key removed.
- [x] AC5: In the counterexample-B block of `tests/testthat/test-axes-certificate.R`, a certificate sentinel returned while the shipped pricing succeeded fails the block with a message saying the certificate degraded on a priced route. A planted `axes_dd_selftest()` failure reddens it.
- [ ] AC6: `devtools::test(reporter = "check")` reports zero failures, and no warning reported against `tests/testthat/test-axes-certificate.R` (the only test file this milestone changes), including warnings raised by package code or installed libraries while that file's tests run. Skips are permitted, and so are warnings reported against other test files.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6

## Tasks

- [x] T1: The three length pins with the one-short plant proved red first.
- [x] T2: `cert_root_rel()` failure below -100% and its test.
- [x] T3: The naive-arm plant at the driven anchor.
- [x] T4: The oracle-driver key guard and its in-process assertion.
- [x] T5: The sentinel-on-priced-route failure in the counterexample-B block, with the `axes_dd_selftest()` plant.
- [ ] T6: DESIGN.md Known fragilities corrected in place (`corrected M148`); `devtools::test()`.

## Work log

- 2026-09-21: created by /milestone-plan at the M147 gate, from the ROADMAP degeneracy row's six-fragility remainder (DESIGN.md Known fragilities, M118 and M122 lineage).
- 2026-09-21: criteria audit ran in reduced mode ([O] fresh reader). Seven findings, all disposed with the tier-preserving answer: the NaN threshold is strictly below -100%; the sentinel-route promise drops per-route naming (a route tag is shipped code); the oracle-driver guard is asserted in process, not across the Python boundary; AC1 names the measured side at the three priced-loop sites, since the committed side is already pinned by `cert_shape`; the `cval` aggregation clause was vacuous and is dropped; AC3 names the one anchor the layer drives; AC6 defines clean as zero failures and warnings with skips permitted.
- 2026-09-21: plan gate chose a separate milestone over folding these into M147 because the sizing tripwire fires past seven criteria; falsified by nothing (a packaging choice).
- 2026-09-21: /milestone-implement started; branch `m148-certificate-validation-fragilities` cut from synced master. Question gate skipped: the plan left no choice open that changes the work.
- 2026-09-21: T1 done. `cert_pin_length()` added and called at the three priced-loop `cert_rel()` sites (now lines 577-579). Plant `[-1L]` on each measured vector: 18 failures, three per priced case, each naming its site ("a4 corrected measured length" and so on). Plant reverted; the file runs clean, all six cases priced on this machine.
- 2026-09-21: T2 done. `cert_root_rel()` stops below -1 with the offending values in the message; -1 itself is admitted. Probes added to the AC7 harness-helpers test (`-1.5`, `c(0.5, -2)`, the -1 boundary), plus a `cert_pin_length()` probe pair (one short reddens naming the site, equal passes). File clean.
- 2026-09-21: T3 done. New test at `cert_anchors()[[1L]]`: the naive arm multiplied by (1 + delta) at four deltas; `fiml_ratio` must land in [(f*delta/2 - base)/(1 + delta) - slack, (f*delta/2 + base)/(1 + delta) + slack], and `se`, `cval` must stay bit-identical to the unperturbed certificate. Discrimination: a denominator-blind estimate (the base value, 5.97e-13) sits below the lower bound at every delta (4.99e-10 at the smallest). File clean.
- 2026-09-21: T4 done. `exact()` split into `exact_parse(out, n_comp)` plus the process call; `exact_keys(n_comp)` lists every key the script reads (16 at two components). A missing key stops naming it; a non-numeric `EXACT_*` value stops naming its key (added beside the key guard, same silent-NA class). The in-process assertion runs at load on a stubbed output: full set parses, each of three dropped keys names itself, one corrupted value names its key. `Rscript devel/degeneracy-oracle/exact_oracle.R` runs end to end, all four verdicts PASS.
- 2026-09-21: T5 done. `cert_priced_not_degraded(cert, id)` fails on the sentinel with "the certificate degraded to its sentinel on a priced route at case 'cxb'", called first in the block's priced branch; probe pair in the AC7 test. Plant (`axes_dd_selftest` mocked FALSE before the block's certificate call): the new failure fired, and on this machine the `cval` and `fiml_ratio` brackets also failed the sentinel while the `se` bracket passed it, so which brackets catch it is a platform fact and the comment says so. Plant removed; file clean.
- 2026-09-21: T6 checkpoint (partial). DESIGN.md's two Known-fragilities paragraphs corrected in place (`corrected M148`): all five fixed, the vacuous `cval` clause dropped, the sentinel-route naming kept as a note. `cairn_validate` passes. `devtools::test()` with the summary reporter exited 0 with no failure section; the WARN and SKIP counts are being re-read with the check reporter before T6 is ticked. Claim audit: not owed — internal tier.
- 2026-09-22: /milestone-review started on the user's direct invocation with status `in-progress` and T6 partial (override logged): T6's remaining item was the `devtools::test()` WARN/SKIP re-read, which lands here as AC6 evidence. Status moved to `review`. Master had not moved since the branch was cut (six commits ahead, origin/master an ancestor of HEAD).

- 2026-09-22: amendment return: AC6 — "`devtools::test()` reports zero failures, and no warning raised from a file this milestone touches; skips and warnings from untouched files are permitted." Reason: the suite reports twelve warnings, every one from an untouched file or an installed library, so the criterion as written cannot pass on any branch cut from this master and never could; the work itself is verified (AC1-AC5 PASS). Amendment-return count for M148: 1 (AC6). Status back to `in-progress` for the gated amendment via /milestone-implement step 6, then re-review. Review stops here.

- 2026-09-22: /milestone-implement resumed on the review's amendment return (AC6). Mini gate: the user chose "Amend AC6 as proposed" over keeping it as written or dropping it.
- 2026-09-22: re-audit: AC6 (reduced) — two definitions of scope ("a file this milestone touches" is a changing diff set, the parenthetical names one fixed file; name the file only); "raised from" is ambiguous between the file the reporter attributes a warning to and the file that calls `warning()` (use "reported against"); the check reporter that attributes warnings is not named; no proportionality or instrument-binding finding. Wording fixed at the gate to the reader's text.
- 2026-09-22: re-audit: AC6 (reduced) — nothing. Second line on AC6: the stop; no further reader is spawned for it.
- 2026-09-22: amendment return: AC6 — "`devtools::test(reporter = \"check\")` reports zero failures, and no warning reported against `tests/testthat/test-axes-certificate.R` (the only test file this milestone changes), including warnings raised by package code or installed libraries while that file's tests run. Skips are permitted, and so are warnings reported against other test files." Narrows the original whole-suite zero-warning promise to the file the milestone changes; zero failures stays suite-wide.

## Decisions

## Review

Evidence gathered 2026-09-22 on macOS arm64, branch `m148-certificate-validation-fragilities` at `df050584`, master `89989cba` (an ancestor of HEAD, no merge needed).

- AC1: `grep -n "cert_pin_length(" tests/testthat/test-axes-certificate.R` shows the helper at 317 and the three priced-loop calls at 609-611, each before its `cert_rel()` comparison. Plant: `[-1L]` on the measured argument at all three sites, file run via `testthat::test_file()` with all six cases priced: 18 failures, three per case, each naming its site ("a4 corrected measured length" through "cxb u measured length"). Plant reverted; the tree is clean. PASS.
- AC2: `cert_root_rel()` at 303-311 stops below -1 with the offending values in the message. The AC7 harness test asserts `expect_error(cert_root_rel(-1.5), "below -100%.*-1\\.5")`, the vector case `c(0.5, -2)` naming `-2`, and admits -1 (value 1); the AC7 test ran 19 expectations, 0 failures, under `NOT_CRAN=true`. PASS.
- AC3: the new test "the quotient's estimate tracks a planted perturbation of the naive arm" multiplies the naive arm at `cert_anchors()[[1L]]` by (1 + delta) at four deltas and brackets `fiml_ratio` while `se` and `cval` stay bit-identical; 16 expectations, 0 failures, under `NOT_CRAN=true` (the file skips this test without it). Discrimination probe run separately: the unperturbed estimate 5.97e-13 sits below the bracket's lower bound at every delta (4.99e-10 at delta 1e-10), so a denominator-blind certificate reddens it. PASS.
- AC4: `exact_parse()` stops with "the oracle output is missing key(s) <names>"; `exact_keys()` lists 16 keys at two components. The in-process assertion at load parses the full stub, and each of three dropped keys and one corrupted value names itself. Evidence: `Rscript devel/degeneracy-oracle/exact_oracle.R` ran end to end, exit 0, all four verdicts PASS; the guard block sourced alone evaluated clean and a one-key output stopped naming the 15 missing keys. PASS.
- AC5: `cert_priced_not_degraded(cert, "cxb")` runs first in the priced branch (894). Plant: `axes_dd_selftest` mocked to FALSE before the block's certificate call; 3 failures, the first "the certificate degraded to its sentinel on a priced route at case 'cxb' ..." and, on this machine, the `cval` and `fiml_ratio` brackets beside it, the `se` bracket passing the sentinel as the comment says. Plant reverted. The AC7 probe pair (sentinel reddens, graded estimate passes) also ran clean. PASS.
- AC6: `devtools::test(reporter = "check")` exit 0: FAIL 0, WARN 12, SKIP 1, PASS 13942. The twelve warnings all rise from files the branch does not touch (`git diff master..HEAD --name-only` lists only the certificate test file, the oracle driver, and tracking): four "the standard deviation is zero" from `test-pole-values.R:56` (a zero-variance profile by design), a glmmTMB/TMB version-mismatch warning and a lavaan version warning from the installed libraries, two "CPM Hessian is ill-conditioned" from `test-ci_accuracy.R`, and the rest of the same environmental kind. Zero failures holds; zero warnings does not, and did not before the branch was cut. FAIL as written; see the amendment return below.

Consistency gate (2026-09-22): `cairn_validate.py` all checks passed, exit 0. No IP/GP principle line changed in DESIGN.md, so `cairn_impact.py` was skipped. `devtools::document()` produced no diff and zero `resolve link` lines. README.md is current and untouched. NEWS.md untouched: internal tier, no user-visible change. No new top-level files. `pkgdown::check_pkgdown()`: no problems. Master watches: newest push runs of `R-CMD-check.yaml` and `test-coverage.yaml` on master both `success` at `d51b3b73`; the three later master commits are `cairn/`-only (paths-ignore), no run expected. `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R`, `tools/check-branch-protection.R` all exit 0. `devtools::check(args = "--no-manual")`: Status OK, 0 errors, 0 warnings, 0 notes (rerun after a session crash lost the first log).

Independent review (three lenses, fresh context; findings ranked as reported, filtered nothing). Dispositions below are the reviewer's proposals for the gate, which this pass did not reach:
- O1 (diff-bug, most severe): the comment on `cert_root_rel()` said NaN "passes an `expect_lt()` as NA rather than reddening"; verified false at review, `expect_lt(NaN, 1)` fails with "Expected NaN < 1". The stop is still the right fix (it names the input); the rationale was wrong. **Fix now**: comment rewritten to say testthat reddens on NaN and the old failure named neither the helper nor the input. DESIGN.md's clause ("returned NaN ... now stops naming the input") makes no passing claim and stands.
- O2: AC1 and AC5 have no committed test planting the defect at a real site; the plants were run by hand and logged. Proposed **reject**: the criteria say the plant reddens, and the plant and its result are recorded here and in the work log; the helpers' own probes are committed in the AC7 test.
- O3: `exact_parse()`'s non-numeric guard also stops on a stray non-`HEX_` stdout line. Proposed **reject**: a stray line in oracle output is a reason to stop, and the Python script emits exactly the key lines today.
- O4: `exact_parse()` does not check a `HEX_*` value's shape; an empty value reaches `cert_record()` as an empty literal. Proposed **follow-up** (candidate row, search-first) or a one-line fix at the gate: outside AC4 as written.
- O5: duplicate keys are not detected. Proposed **reject**: the script prints each key once.
- O6: an NA input to `cert_root_rel()` stops with R's own "missing value where TRUE/FALSE needed". Proposed **reject**: red either way; no committed quantity is NA.
- O7: the quotient test's `slack = f * eps` covers the rounding it must with little room. **Noted**, no change.
- S-blame: no conflicts with M115, M118, M122, M147 or D-048 through D-062; the dropped `cval` clause verified vacuous against `cert_shape`.
- S-prior: probe `pulls/comments?per_page=1` returned `[]`, no PR walk. One finding: `cert_priced_not_degraded()` decides by `identical(cert, axes_certificate_sentinel())`, the value-coincidence shape M122 retired in `cert_bracket()`. Proposed **reject with reason**: the sentinel is the certificate's own declared constant, not two independently produced numbers, a coincidence here yields a false failure rather than a silent pass, and a route tag is out of scope by the plan; the M122 lineage is worth a sentence in the helper's comment at the gate.

Return floor: no actioned finding demonstrates an acceptance criterion failing or a load-bearing defect; none returns status.
