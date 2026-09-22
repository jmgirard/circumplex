# M149: Reference-route checks at the anchors, and the certificate file's CRAN posture

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3
- **Resolves:** —
- **Surface tier:** user-facing, because a `?axes_reliability` sentence ships beside the internal test and record changes
- **Branch/PR:** m149-dd-anchors-cran-posture

## Goal

The test file asserts the certificate's double-double reference route against the committed exact values at all six committed cases. The help page states that the estimate depends on the machine. A decision records the CRAN posture of the certificate test file.

## Scope

**In:** RR22 recommendations 11 and 12. These are items (ii) and (iii) of the ROADMAP's RR22 follow-ons row. The work adds half-ulp dd-vs-exact assertions at the five anchors in `tests/testthat/test-axes-certificate.R`. A test over committed values backs the bound. One `?axes_reliability` sentence says that the estimate is a property of the fit on the running machine. Counterexample B's priced branch gains an `"uncertified"` assertion. A D-entry and a file-header block keep the file's current CRAN posture. The anchor-list test becomes CRAN-live.

**Out:** Three items stay on the RR22 follow-ons row. They are (i) the `ubuntu-24.04-arm` CI job, (iv) the rcond sampling in [eps, 1e-10] and the once-priced core, and (v) the selector-route warning clause. Showing the estimate on computed fits stays on the degeneracy row as (iii). CRAN-dark invariants in other test files stay on the M120 remainders row as (i). The milestone changes no shipped R code and no number the package reports.

## Acceptance criteria

- [ ] AC1: A test in `tests/testthat/test-axes-certificate.R` checks each of the five anchor cases. It converts `axes_dd_pricing()`'s `v`, `v_naive` and `u` with `dd_to_double()`. It asserts that each lies strictly within half a unit in the last place of the committed exact value (`cert_frozen`'s hi/lo pair). If `axes_dd_pricing()` returns anything other than its list, the test fails and names the case. The assertion runs on the priced route and on the refusing route. It runs whenever this machine builds the case's matrix and the derivative set's `xi1` matrix bit for bit as committed. Otherwise it records the case as `skipped` through `cert_record()` and skips, naming which input differed.
- [ ] AC2: A test reads only committed values. It checks every component of `v`, `v_naive` and `u` at the five anchors. For each, it asserts that the exact value lies farther from the rounding midpoint than an upper bound on the reference route's error. The test's comment derives that bound from the anchor's conditioning. Both figures are in ulps of `hi`. Where `lo` is zero, the distance is half an ulp.
- [ ] AC3: `?axes_reliability` (roxygen in `R/axes_reliability.R`, `man/` regenerated) states a fact about a severely ill-conditioned fit. There, the certificate's estimate is a property of the fit as computed on the running machine. The same fitted matrix can print a graded estimate on one machine and 1 on another, and both refuse the fit as `"uncertified"`. The counterexample-B test asserts `"uncertified"` on both of its admitted routes, which backs the refusal half. The differing-estimates half rests on RR22 section 3's cross-machine measurements. The milestone record cites them, and the help page does not.
- [ ] AC4: A grep of `R/`, `vignettes/` and the development-version section of `NEWS.md` for `relative error` and `uncertified` finds hits. No hit presents the certificate's estimate as the same on every machine. The review evidence lists each hit with its disposition.
- [ ] AC5: `cairn/DECISIONS.md` gains a D-entry that keeps the file's current CRAN posture. It states which classes of tests run under `R CMD check` on CRAN, which skip there, and the ground for each class. It names RR22's CI-only alternative as rejected, with the evidence that falsifies the choice. The one change is that the anchor-list test stops skipping on CRAN. The file's header states the posture and cites the D-entry. The review evidence lists every `test_that()` in the file as CRAN-live or CRAN-skipped, with its class. Take a machine that builds all five anchors and their `xi1` bit for bit. On it, a run of the file with `NOT_CRAN=false` skips exactly the tests listed as CRAN-skipped.
- [ ] AC6: `devtools::test()` reports no failures, and `devtools::check(manual = TRUE)` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T2
- AC3 → T4, T5
- AC4 → T5
- AC5 → T6
- AC6 → T7

## Tasks

- [x] T1: Commit each anchor's `xi1` upper triangle as hex beside `sig` in `cert_frozen`. Regenerate it through `devel/degeneracy-oracle/exact_oracle.R`, or assert it equal to the oracle's inputs. Add the per-anchor dd-vs-exact test. It has the matrix and `xi1` gate and the `skipped` record before `skip()`. It fails by name on a non-list return and applies the half-ulp bound to `v`, `v_naive` and `u`. Move `dd_ulp()` from the counterexample-B test to file scope and reuse it. Reconcile the header's "deliberately NOT pinned" block (lines ~128-133) and the counterexample-B comment (~816-821) with the new assertions.
- [x] T2: Add the committed-values margin test (AC2) and derive the reference route's error bound in its comment. If the derived bound exceeds the margin at any anchor, stop and raise an amendment. Do not loosen the bound.
- [x] T3: Prove that the AC1 assertions can fail. Apply each plant alone, revert it, and summarize the result in the work log. Plant (a): `dd_two_sum()` and `dd_two_prod()` return a zero low word, so the route loses its error-free transforms. Plant (b): for each of `v`, `v_naive` and `u`, move the first component's returned high word up by one ulp of that high word. Then do the same to the last component. Also plant a mismatched `xi1`. Show that the case records `skipped` and that the detector stays green for that reason.
- [x] T4: Add `expect_identical(axes_degeneracy_refusal(fx$S, d)$reason, "uncertified")` to counterexample B's priced branch.
- [x] T5: Write the `?axes_reliability` sentence next to the sentence saying that the warning names the worst estimate (`R/axes_reliability.R` ~730). Run `devtools::document()`. Run the AC4 grep and record the disposition of each hit in the work log.
- [x] T6: Classify every `test_that()` in the file as CRAN-live or CRAN-skipped, with its ground. Draft and append the D-entry. Write the header posture block. Remove `skip_on_cran()` from the anchor-list test (~689). Run the file with `NOT_CRAN=false` and record its skip list.
- [ ] T7: Run `devtools::test()` and `devtools::check(manual = TRUE)`.

## Work log

- 2026-09-22: created by /milestone-plan. Promoted from RR22 follow-ons (ii) and (iii). Inbox sweep found 1 open issue (#180) and 0 open PRs, with no overlap.
- 2026-09-22: criteria audit, full mode, by a fresh [O] reader. Round one gave nine findings: seven fixed directly, two posed at the gate. Round two gave four findings, all fixed directly. The plant matrix moved from the criteria to T3 because it is an instrument property.
- 2026-09-22: plan gate chose to keep the file's current CRAN posture over RR22's CI-only alternative. CRAN runs platforms that CI does not, and since M147 the only platform-dependent refusal left is an exact zero pivot. A CRAN check failure in this file that is a platform fact and not an under-report falsifies the choice.
- 2026-09-22: plan gate chose a half-ulp bound over RR22's 1e-14 relative bound. The zero-low-word plant moves `u` at a5 by only 87 ulp (about 1e-14), and the relative bound can miss that. An anchor whose committed margin falls below the derived route error falsifies the choice.
- 2026-09-22: T1 done. `exact_oracle.R` now emits each anchor's `xi1` upper triangle, and a baseline run reproduced all six committed cases bit for bit before the change. The regenerated block went into `cert_frozen` through a script paste, not the Edit tool. `cert_dd_vs_exact()` runs inside `cert_true_error()` after the `sig` check. The header's dd-route block and the counterexample-B comment are reconciled. The file passes, and all six cases are priced on macOS arm64.
- 2026-09-22: T2 done. The derived bound is 10 * p * kappa^2 * 2^-51 ulp: the floor's p * kappa^2 * eps bound with its factor-10 allowance and the dd unit roundoff 2^-104 in place of eps. It ranges from 3.6e-6 (a4) to 3.3e-3 (b9b). The tightest margin is 0.0072 ulp at b9b `vn_hi`. The measured dd errors are 1.3e-15 to 2.5e-9 ulp, all below the bound. No committed `hi` is a power of two. A 3x-bound plant failed at b9b `vn_hi` only.
- 2026-09-22: T3 done, each plant applied alone in `R/axes_certificate.R` and reverted with `git checkout`. Plant (a), zero low words: all 15 anchor dd-vs-exact assertions failed, plus the three at counterexample B (42 failures in the file). Plant (b), a one-ulp move up of the returned high word: `v` first and last, `v_naive` first and last, and `u` each failed exactly that field's assertion at all five anchors. The same move fails at counterexample B for `v` and `v_naive` but not `u`, because its bound is an absolute 2^-53 and one ulp of 0.0555 is below that. A first plant (b) pass is void: a failed shell loop left a `v_hi[[1]]` plant in the source, so every variant also failed `v`; the file was restored and all five variants rerun. A mismatched `xi1` entry at a4 recorded `a4 = skipped` with the `xi1` reason, with no failure, and the detector stayed green on the four priced anchors.
- 2026-09-22: T4 done. Counterexample B's priced branch now asserts the refusal reason `"uncertified"`. It passes on macOS arm64, where B prices.
- 2026-09-22: T5 done. The `?axes_reliability` sentence follows the sentence saying the warning names the worst estimate, and `man/axes_reliability.Rd` is regenerated. No NEWS entry, because no behavior changes. AC4 grep: 42 hits in `R/`, 4 in the development section of `NEWS.md`, and 4 in `vignettes/`. The hits in `ssm_draws.R`, `ssm_trajectory.R`, NEWS line 279 and the growth vignette use "uncertified" for displacement certification, a different subject. The rest describe the certificate or the refusal literal and make no claim about the estimate across machines. None needed a change.
- 2026-09-22: T6 done. D-063 appended, the header posture block written, and `skip_on_cran()` removed from the anchor-list test. Timed under `NOT_CRAN=true`, the whole file runs in 1.3 s, and each CRAN-skipped test takes 0.1 s or less, so D-063 grounds the skipped class on what those tests can detect and not on cost. With `NOT_CRAN=false`, the file skips exactly the five tests in the skipped class, and all six cases are priced.
- 2026-09-22: claim audit round one (fresh [O] reader): 34 claims read, 5 wrong and 1 unverifiable, all corrected. The CRAN block, D-063's ground and its heading now draw the line at "checks the estimate against exact truth", because the closed-form oracle tests also bracket against exact values and three skipped tests catch a low estimate against a planted or known error. The derivative-set count is now 10 to 12 matrices. The oracle comment now says B's `xi1` is cos() but not pinned. B's bounds are described per field. The margin bound is stated as an analogy and carries a factor of 2 for the squared SE (20 * p * kappa^2 * 2^-51 ulp; b9b bound 0.0066 against a 0.0072 margin). D-063 was corrected in place because it has not left this branch. Re-read pending; T7 to rerun after these edits.
- 2026-09-22: claim audit: 34 claims read, 6 corrected — R/axes_reliability.R, man/axes_reliability.Rd, tests/testthat/test-axes-certificate.R, devel/degeneracy-oracle/exact_oracle.R. The re-read confirmed four of the six corrections. It found that the detector brackets nothing and that c4's set has 6 matrices. Both were fixed in the test header and in D-063 with no further pass, per the stopping rule.

## Decisions

## Review
