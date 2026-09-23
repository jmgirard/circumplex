# M150: Price every certificate case from its committed xi1

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3
- **Resolves:** —
- **Surface tier:** user-facing, because a `?axes_reliability` sentence ships beside the internal test changes
- **Branch/PR:** m150-pin-xi1-certificate-cases

## Goal

Every committed case in the certificate test file is priced from its committed `xi1`. A machine whose `cos()` builds a different `xi1` then still runs every reference-route check and bracket against exact values that describe its input.

## Scope

**In:** RR22 follow-ons row (vi), from the M149 review. O3 and S3: a machine whose `xi1` differs at all five anchors runs no anchor reference-route check. S2: on such a machine the brackets measure against exact values priced from another `xi1`. O4: counterexample B's `xi1` is not pinned. O9: B's `hi` values have no power-of-two check, which identity with `hi` makes unnecessary. O5: the help sentence rests on one matrix, measured before M147, and is re-measured and rewritten to match.

**Out:** O2 (the factor 10 exceeded for `u` at B) is dropped at the plan gate. The margin test's comment discloses it, and a wrong bound can only give a false red. The `exact_parse()` two-word check stays on the degeneracy row as (iv). The rest of the RR22 follow-ons row, (i), (iv) and (v), stays there. The shipped route still builds `xi1` with this machine's `cos()`, and M150 changes no shipped R code.

## Acceptance criteria

- [x] AC1: In `tests/testthat/test-axes-certificate.R`, eleven tests compare against `cert_frozen`'s exact values: the five bracket tests, the five reference-route tests and counterexample B's test. Each passes the shipped pricing (`axes_v_pricing()`, `axes_u_pricing()`, `axes_accuracy_certificate()`) and the reference route (`axes_dd_pricing()`) a derivative set whose `xi1` is rebuilt from that case's committed upper triangle in `cert_frozen`. No `skip()` in the file is conditioned on `xi1`.
- [x] AC2: For each of the six cases, a test asserts that the `xi1` this machine builds has the committed length and is within `4 * .Machine$double.eps` of the committed one in every entry, and it fails by the case's name. The test skips on CRAN and nowhere else.
- [x] AC3: `cert_frozen$cxb` carries counterexample B's `xi1` upper triangle, diagonal included. It is emitted by `devel/degeneracy-oracle/exact_oracle.R` in a run whose output reproduces every other committed field of all six cases bit for bit. The anchor-list test asserts that its length is p(p + 1)/2 for B's three variables.
- [x] AC4: At counterexample B, the reference-route check for `v` and `v_naive` asserts that `dd_to_double()` of the route's value is identical to the committed `hi` in every component. `u` keeps its absolute bound of `2^-53`. The test's comment states that this identity rests on committed inputs and R-level IEEE arithmetic, not on a stated margin.
- [x] AC5: B's certificate estimate is observed by calling `axes_accuracy_certificate()` on B's fixture matrix with `axes_se_derivs()` as the package builds it, not with the test's pinned `xi1`. It is observed on macOS arm64 and on linux-arm64 (the `tools/arm64` image). If the two machines print different estimates, the `?axes_reliability` sentence on machine-dependent estimates says so and names one ill-conditioned test matrix as its evidence. If they print the same estimate, the page makes no claim that the estimate depends on the machine. The roxygen is in `R/axes_reliability.R`, and `man/` is regenerated.
- [x] AC6: A grep of `tests/testthat/test-axes-certificate.R` and `devel/degeneracy-oracle/exact_oracle.R` for `xi1`, with each hit read with five lines of context on each side, finds no comment stating that an `xi1` mismatch skips a test or that counterexample B's `xi1` is not pinned.
- [ ] AC7: On the PR's windows-latest R-CMD-check job, the five anchor reference-route tests, counterexample B's test and the `xi1` guard test run and do not skip. The evidence is that none of their names appears in the skip list of the job's printed testthat output.
- [x] AC8: `devtools::test()` reports no failures, and `devtools::check(manual = TRUE)` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T2
- AC2 → T2, T4, T8
- AC3 → T1
- AC4 → T3, T4
- AC5 → T5, T9
- AC6 → T6
- AC7 → T7
- AC8 → T7

## Tasks

- [x] T1: In `exact_oracle.R` (~149-161, ~210), pass `d` to `cert_record("cxb", ...)` so that the driver emits B's `xi1`. A baseline regeneration must first reproduce the committed block bit for bit. Paste B's `xi1` into `cert_frozen`. In the anchor-list test (~910), replace `expect_null(cert_frozen$cxb$xi1)` with the length check.
- [x] T2: Write one helper that rebuilds the full symmetric `xi1` from a committed upper triangle and returns `d` with it in place. Use it in `cert_derivs()` callers for the bracket tests, the five reference-route tests and B's test. Remove the `xi1` skip from `cert_dd_vs_exact()` (~671-679). Add the AC2 guard as its own `test_that()` with `skip_on_cran()`, and give it no other skip. The pricing reads `xi1` only through `d$mats` (the plan audit read `R/axes_corrected_se.R:197,207` and `R/axes_certificate.R:352`). Confirm that again before relying on it.
- [x] T3: Change B's `v` and `v_naive` checks (~1129-1134) to identity with `hi`, and write the AC4 comment.
- [x] T4: Apply each plant alone, locate it by its text, and assert that `git diff --stat` is non-empty. (a) Change one large entry (about 0.707) of the builder's `xi1` output by one ulp at all six cases. Then none skips, all are priced, all pass, and the guard passes. (b) Change the builder's formula, for example `cos(2 * delta)`. The guard then fails, naming each case. (c) Return zero low words from `dd_two_sum()` and `dd_two_prod()`. The reference route then fails at all six cases. (d) Move B's returned `v` by one ulp, then `v_naive`. Identity then fails for that field. Summarize the results in the work log.
- [x] T5: Measure B's estimate as AC5 states, on macOS arm64 and in the `tools/arm64` image. Record each machine and value in the work log. Rewrite the sentence at `R/axes_reliability.R` ~731-736 to match, and run `devtools::document()`.
- [x] T6: Sweep the comments for `xi1` in both files: the header (~145-174), the `cert_dd_vs_exact()` block (~626-662), B's test and the oracle comment (~149-156). Record each hit's disposition in the work log.
- [x] T7: Run `devtools::test()` and `devtools::check(manual = TRUE)`. At review, read the PR's windows-latest log for AC7.
- [x] T8: Give the two `xi1` length checks a label that names the case: the guard test's check and the anchor-list test's check. Plant a truncated `xi1` and make sure that each failure names the case. Run T7's two commands again.
- [x] T9: Name the committed counterexample and its fixture file in the `?axes_reliability` sentence, and run `devtools::document()`. Add a NEWS.md bullet under "Minor improvements and fixes" for the changed sentence. Run the claim audit over the new lines, then T7's two commands again.

## Work log

- 2026-09-22: created by /milestone-plan. Promoted from the RR22 follow-ons row (vi). Inbox sweep: 1 open issue (#180) and 0 open PRs, with no overlap.
- 2026-09-22: criteria audit, full mode, by a fresh [O] reader. Round one found 10 findings. Seven were fixed directly, and three were posed at the gate (O2, O5 and the O5 staleness). Round two, after the gate, found 4 findings, and all were fixed directly (the AC5 call and machines, the AC5 conflict, the guard in AC7, and the AC7 evidence).
- 2026-09-22: plan gate chose to price from the committed `xi1` over M149's skip on an `xi1` mismatch. The skip ran the check nowhere on Windows, and M149 chose it under the thrash rule, not on merit. A supported platform whose built `xi1` falls outside the AC2 tolerance falsifies this choice.
- 2026-09-22: plan gate chose identity with `hi` at B over adding B to the power-of-two assertion. Identity needs no power-of-two premise. A committed B `hi` that `dd_to_double()` cannot return on some IEEE platform falsifies this choice.
- 2026-09-22: plan gate chose to skip the `xi1` guard on CRAN over running it there. The guard has no exact yardstick (D-063 lets such a check skip), and CI catches edits to the builder. A builder change that reaches CRAN without failing on CI falsifies this choice.
- 2026-09-22: plan gate chose to re-measure the help sentence over only rewording it, because its measurements predate M147. It dropped O2 and kept the `exact_parse()` check on its row.
- 2026-09-22: T1 done. A baseline `CERT_EMIT=1` run of `exact_oracle.R` reproduced the committed `cert_frozen` block byte for byte. With `d` passed at cxb, the only diff was B's new six-entry `xi1`, now pasted in. `cert_record()` now requires `d`. The anchor-list test checks the `xi1` length at all six cases. The certificate file passes, and all six cases are priced on macOS arm64.
- 2026-09-22: T2 done. `cert_pinned_derivs()` writes the committed `xi1` into the built matrix with `[<-`, and `cert_cxb_derivs()` builds B's set. The bracket tests, the reference-route tests and B's test price from the pinned set. The `xi1` skip left `cert_dd_vs_exact()`. The new guard test skips on CRAN only. The pricing reads `xi1` only through `d$mats` (grep of `d$` in `R/axes_corrected_se.R`, `R/axes_certificate.R`, `R/axes_scaled_fit.R`). On macOS arm64 the pinned `xi1` is `identical()` to the built one at all six cases. `devtools::test()`: 0 failures.
- 2026-09-22: T3 code in (checkpoint, not ticked until the full suite runs after T4). B's `v` and `v_naive` checks assert identity with the committed `hi`. The comment gives B's margin (0.113 ulp at `v_naive[1]`) and stated bound (1.18 ulp), both recomputed from `cert_frozen` this session. The certificate file passes.
- 2026-09-22: T4 plants, each alone in `R/`, located by text, with a non-empty `git diff --stat` before each run and `git checkout -- R/` after. The file was run with `NOT_CRAN=true`. The control had 29 tests, 0 failed and 0 skipped. (a) Moving the largest off-diagonal `xi1` entry one ulp toward zero (1 at the anchors and 0.976 at B, so built and committed differ by 2^-53 at all six) gave 29 tests, 0 failed and 0 skipped, with all six priced. An earlier plant on `xi1[1,2]` was void, because that entry is 6e-17 at c4. (b) `cos(2 * delta)` failed only the guard, at all six cases by name. (c) Zero low words failed the reference route at all six, including all 15 anchor fields and B's three. (d) A one-ulp move of `v_hi[1]` failed B's `v` identity, and the same move of `vn_hi[1]` failed B's `v_naive` identity.
- 2026-09-22: T6 sweep. A grep for `xi1` with five lines of context in the test file and `exact_oracle.R` was scanned for skip, precondition and unpinned wording. The header's derivative-set block, "where this machine builds their inputs" (now "their matrix") and the posture block's CRAN-skipped list (the guard added) were rewritten. The hits at :141 and :206 say that no test skips on `xi1`. The hit at :695 is past-tense history ("Until M150 this test skipped") and was kept. The oracle driver had no hit.
- 2026-09-22: T5 in progress (checkpoint). B's estimate was measured with `axes_se_derivs()` as built, through `tools/measure`-style scripts in the scratchpad. macOS arm64 with reference LAPACK gave se 0.335, cval 48.9 and fiml_ratio 0.0087. linux-arm64 in the `tools/arm64` image, with OpenBLAS 0.3.33, gave se 2.53, cval 5.63 and fiml_ratio 0.0062. Both refuse `"uncertified"`, and both build the same `xi1`. Neither printed the sentinel 1, so the old "and 1 on another" was stale. The roxygen sentence now names the two machines and one test matrix, with no figures. `man/` is not regenerated yet, because it waits for the running full suite.
- 2026-09-22: T3-T6 done. The full suite at the T3 code (`f9c8f373`) gave 0 failures, so T3 and T4 are ticked. `devtools::document()` changed only `man/axes_reliability.Rd`, which finishes T5. Correction to the T5 line above: the measurement scripts were scratchpad files, not `tools/` scripts.
- 2026-09-22: claim audit: 31 claims read, 5 corrected — R/axes_reliability.R, man/axes_reliability.Rd, tests/testthat/test-axes-certificate.R. The help sentence now says the check's worst estimate (the figure the warning prints) differed, not that a fit printed it. The guard comment names only builder edits, because an angle edit already reddens `kappa`, and it no longer claims the Windows difference was one ulp. The header says "matches the committed bytes", because B's matrix is read, not built. B's comment says "is given the same input". The same reader's re-read is pending.
- 2026-09-22: claim-audit re-read: the same reader confirmed all five corrections. One short header line was rewrapped. T7 is running (`devtools::test()`, then `devtools::check(manual = TRUE)`).
- 2026-09-22: T7 done at `dc627e0e`. `devtools::test()` gave 0 failures. `devtools::check(manual = TRUE)` gave 0 errors, 0 warnings and 0 notes, "checking PDF version of manual ... OK", and `Status: OK`. The Windows reading for AC7 waits for the PR at review. Status set to review.
- 2026-09-22: review return 1 (a defect return, count 1): AC2 fails. The guard's length check (`expect_length(cert_frozen[[id]]$xi1, ...)`, test file :1110) has no label, so a length mismatch fails as "Expected `cert_frozen[[id]]$xi1` to have length 6" without the case's name (reproduced with testthat; the [O] reviewer found it too). The same unlabelled check in the anchor-list test (:940) contradicts its comment "fails here by name". Status set to in-progress. Pass-1 evidence and the unresolved reviewer findings are in the Review section.
- 2026-09-22: resumed by /milestone-implement. T8 added as a minor amendment (a discovered sub-task, and Coverage AC2 → T2, T4, T8). No question gate, because nothing was open.
- 2026-09-22: T8 checkpoint (not ticked): both `xi1` length checks are now `expect_identical(length(...), (p * (p + 1L)) %/% 2L, label = "<id> xi1 length")`. A first draft without the outer parentheses gave 32 at p = 8, because `%/%` binds tighter than `*`, and the plant exposed it. With one entry cut from B's committed `xi1`, both checks failed as "Expected cxb xi1 length ..." and named no other case. After the plant was removed, the file with `NOT_CRAN=true` gave 29 tests, 0 failed, 0 skipped. The full suite and `check(manual = TRUE)` are running.
- 2026-09-22: T8 done at `51043edd`. `devtools::test()` gave FAIL 0, WARN 12, SKIP 1 and PASS 14022. `devtools::check(manual = TRUE)` gave 0 errors, 0 warnings and 0 notes, "checking PDF version of manual ... OK", and `Status: OK`. The claim audit was not re-run, because T8 adds two test labels and no prose claim. Status set to review.
- 2026-09-22: review return 2 (a defect return, count 2): AC5 fails as written, because the `?axes_reliability` sentence does not name the ill-conditioned test matrix it cites. The consistency gate also fails, because NEWS.md has no entry for that sentence. Pass-2 evidence ticks AC1-AC4, AC6 and AC8. The implement gate can amend AC5 instead of changing the text. Then the amendment goes on the amendment track, and this line counts as a gate return for NEWS alone. Status set to in-progress.
- 2026-09-22: resumed by /milestone-implement. At the question gate the user chose to name the matrix, not amend AC5, and to add the drafted NEWS bullet. T9 was added as a minor amendment (Coverage AC5 → T5, T9).
- claim audit: 7 claims read, 2 corrected — R/axes_reliability.R, man/axes_reliability.Rd, NEWS.md
- 2026-09-22: T9 checkpoint (not ticked). The help sentence now names the three-variable matrix at `tests/testthat/fixtures/rb18-counterexample-b.rds`, which `.Rbuildignore` does not exclude. NEWS.md has a new bullet. The audit's corrections: "The three" became "The three estimates", and the macOS machine is described as having R's reference BLAS and LAPACK. The same reader confirmed both on its re-read. The Linux side of the sentence rests on the pass-2 measurement in the Review section. The full suite and `check(manual = TRUE)` are running.
- 2026-09-22: T9 done at `ebff2c5a`. `devtools::test()` gave FAIL 0, WARN 12, SKIP 1 and PASS 14022. `devtools::check(manual = TRUE)` built the final text and gave 0 errors, 0 warnings, 0 notes, "checking PDF version of manual ... OK" and `Status: OK`. `document()` gave 0 `resolve link` lines. Status set to review.

## Decisions

## Review

Pass 1, 2026-09-22, at `2be40dfe`. Stopped at AC2, so no box is ticked. Pass 2 re-runs every criterion.

- AC1 (not ticked, pass returned): 11 tests price from `cert_pinned_derivs()`: the bracket loop (:1051), the reference-route loop (:1077) and B's test (:1147), 5 + 5 + 1. The two remaining `skip()` calls (:707, :777) test `sig`, not `xi1`.
- AC2 FAILED: the value check fails by name (label at :1114), but the length check at :1110 does not name the case.
- AC3 (not ticked, pass returned): `CERT_EMIT=1 Rscript devel/degeneracy-oracle/exact_oracle.R` printed a `cert_frozen` block equal to the committed one in every field, B's `xi1` included. The only difference was one trailing space in the printed output. The anchor-list test checks the length for all six cases (:938-941).
- AC4 (not ticked, pass returned): identity checks at :1215-1219, `u` at `2^-53` (:1220-1222), and the comment at :1194-1203 rests identity on committed inputs and R-level arithmetic.
- AC5 (not ticked, pass returned): measured again with `axes_se_derivs()` as built. macOS arm64, reference LAPACK: se 0.3355, cval 48.9, fiml_ratio 0.008708. `tools/arm64` image, OpenBLAS 0.3.33: se 2.534, cval 5.635, fiml_ratio 0.006238. Both machines refuse the fit as `"uncertified"` and build the same `xi1`. The estimates differ, and the help sentence says so.
- AC6 (not ticked, pass returned): 36 `xi1` hit lines in the test file and 9 in the oracle driver, read with 5 lines of context. No hit says that an `xi1` mismatch skips or that B is not pinned. :695 is history ("Until M150").
- AC7: not read. The PR opens only after the approval gate (step 8), so pass 2 reads the windows-latest log during the step-8 CI wait, before the merge.
- AC8: the suite was stopped when the pass returned.

Reviewer findings, pass 1. The three lenses ran. The prior-review lens found no GitHub threads. The blame-history lens found no conflict with a past decision. Each finding below waits for triage at the pass-2 gate, except the first:
- [O] 1: the guard's length check does not name the case (AC2). Its disposition is this return.
- [O] 2: the anchor-list length check (:940) has no label, which contradicts its comment "fails here by name".
- [O] 3: pinning removes the check of the machine's own `xi1` from the CRAN-live tests. Only the guard checks it now, and CRAN skips the guard. The header (:77-86) does not say so.
- [O] 4: the comment on B's identity (:1195-1203) assumes no FMA contraction and no extended precision. The test calls `axes_dd_pricing()` directly, without `axes_dd_selftest()`, and a failure there is a false red.
- [O] 5: B's derivative set is still built inline at :1471, :1561 and :1587, not with `cert_cxb_derivs()`.
- [O] 6: B's comment at :1282-1285 describes the help page as a general claim. The page now reports one measured difference.
- [O] 7: the help sentence does not say that the matrix is counterexample B, and it gives no size or direction for the difference.
- [O] 8: the header clause at :217-219 says "where this machine's matrix matches the committed bytes" and includes B. B's matrix is committed bytes, and a mismatch there fails.
- [O] 9: the guard's 4-eps tolerance cannot see builder edits smaller than that. The plan gate accepted this.
- [O] 10: the vacuity paths at :1058 and :707 are older than M150 and are backed by the detector test.
- [O] 11: the ROADMAP hygiene stamp is dated 2026-09-23, one day after today. That line is older than this branch.
- [S-prior] 1: M150 reverses the choice M149's review made (skip over pin). The M150 plan gate records this reversal.
- [S-prior] 2: B's identity check is stricter than the anchors' half-ulp bound, and the asymmetry is intentional (AC4).
- [S-prior] 3: the new guard never calls `cert_record()`, which follows the LESSONS line about overwrites.

Pass 2, 2026-09-22, at `5fdd7139`. Master has not moved since the branch was cut.

- AC1: 11 tests price from `cert_pinned_derivs()`: the bracket loop (:1052), the reference-route loop (:1078) and B's test (:1149), 5 + 5 + 1. The two `skip()` calls (:707, :777) test `sig`, not `xi1`.
- AC3: a new `CERT_EMIT=1 Rscript devel/degeneracy-oracle/exact_oracle.R` run printed a 157-line `cert_frozen` block. With trailing spaces removed, it is identical to the committed block, B's six-entry `xi1` included. The anchor-list test checks the `xi1` length of all six cases (:938-942).
- AC5: measured again with `axes_se_derivs()` as built. macOS arm64, reference LAPACK: se 0.3355, cval 48.9, fiml_ratio 0.008708. `tools/arm64` image, OpenBLAS 0.3.33: se 2.534, cval 5.635, fiml_ratio 0.006238. Both refuse the fit as `"uncertified"`. The estimates differ, and `R/axes_reliability.R:734` and `man/axes_reliability.Rd:246` say so and cite one ill-conditioned matrix from the package's tests.
- AC5 correction: the box is unticked again. The criterion says the sentence "names one ill-conditioned test matrix". The sentence says "one ill-conditioned matrix from the package's tests" and does not name it. The tick read "names" as "cites", and a review does not reinterpret a criterion. The pass-2 [O] reviewer raised this as finding 3. The measurements above stand.
- The warning's figure: `axes_degeneracy_note()` on B's refusal, with `xi1` as built on macOS, printed "estimated relative error 49", and `refusal$cert` is `identical()` to `axes_accuracy_certificate()` on the same input. So "the figure the warning prints" is the worst estimate, rounded to two digits.
- AC8: at `87d0be94`, `devtools::test()` gave FAIL 0, WARN 12, SKIP 1 and PASS 14022, with all six cases priced. `devtools::check(manual = TRUE)` gave 0 errors, 0 warnings, 0 notes, "checking PDF version of manual ... OK" and `Status: OK`.
- AC2: the guard test (:1097) has `skip_on_cran()` and no other skip, and it passed in the AC8 suite. Two plants were applied together, and the file was then restored with `git checkout` (empty `git diff`). One plant cut one entry from B's committed `xi1`. The other moved a4's committed 0.7071 entry by 16 ulp (8 eps). The guard failed with "cxb xi1 length" (:1111) and "a4 xi1: largest distance from the committed copy" (:1115), and the anchor-list test failed with "cxb xi1 length" (:940).
- AC4: B's test (:1215-1222) asserts that `dd_to_double()` of `v` and of `v_naive` is `identical()` to the committed `hi`, and that `u` is within `2^-53`. The comment (:1194-1203) rests identity on committed inputs and R-level IEEE arithmetic, and it says that no margin covers B. The test passed in the AC8 suite.
- AC7: not read. The PR opens only after the approval gate, so pass 3 reads the windows-latest skip list during the step-8 CI wait, before the merge.

Consistency gate, pass 2. `cairn_validate` passed (one advisory: 8 criteria, over the 7-criterion split tripwire). `document()` left no diff, with 0 `resolve link` lines. `pkgdown::check_pkgdown()` found no problems. The newest verdict on master is success for both `R-CMD-check.yaml` and `test-coverage.yaml` (2026-09-23T01:11Z). `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R` and `tools/check-branch-protection.R` all exit 0. README.md is not touched. FAILED: NEWS.md has no entry for the changed `?axes_reliability` sentence. The entry at NEWS.md:48 is M147's.

Reviewer findings, pass 2. The three lenses ran again. Each finding waits for triage at the next gate, unless its disposition is stated:
- [O2] 1 (= pass-1 O3): CRAN no longer checks this machine's own `xi1`, and the header does not say so.
- [O2] 2 (= O4): B's identity check assumes no FMA contraction and no extended precision.
- [O2] 3 (= O7, made stronger): AC5 says "names" the matrix, and the sentence does not name it. Its disposition is this return.
- [O2] 4 (new): the sentence names the OS (macOS, Linux), but the difference that matters is most likely the LAPACK/BLAS library.
- [O2] 5 (new): "the figure the warning prints" was not observed. Refuted: the note prints "estimated relative error 49", which is the worst estimate (see above).
- [O2] 6 (= O8), [O2] 7 (= O6), [O2] 9 (= O5), [O2] 10 (= O9): confirmed as in pass 1.
- [O2] 8 (new): the guard joins the CRAN-skipped group, but D-063's list of that group does not name it.
- [O2] 11: the T8 fix is correct (integer types, precedence, both labels, the guard's `next`). O2 is closed, O10 is older than M150, and O11 is confirmed.
- [O2] 12: AC7 has no evidence until the PR exists.
- [S-blame2] 1-3 repeat O3, O4 and O8. 4: T8 keeps M116's intent (the checks came from M149, `165af9d9`). 5: the M149 reversal is recorded. 6 repeats O7.
- [S-prior2] 1: `expect_identical()` treats `0` and `-0` as equal (LESSONS, M147). Proposed rejection: B's committed `v_hi` and `vn_hi` are about 13 and 59, so no zero can occur.

Pass 3, 2026-09-22, at `aec5b81b`. Master has not moved. Since pass 2 (`87d0be94`), only NEWS.md, the roxygen in `R/axes_reliability.R` and `man/axes_reliability.Rd` changed outside `cairn/`, and no R code line changed. The evidence for AC1, AC3 and AC6 therefore stands on unchanged files, and so does AC2's plant evidence.
- AC5: the sentence (`R/axes_reliability.R:734-739`, `man/axes_reliability.Rd` to match) now names the matrix: "That committed counterexample is the three-variable matrix saved in the package sources as `tests/testthat/fixtures/rb18-counterexample-b.rds`." It says that the worst estimate differed between macOS (R's reference BLAS and LAPACK) and Linux (OpenBLAS), and that both refused the fit. The pass-2 measurements (worst 48.9 against 5.635, both `"uncertified"`) were taken at the same R code. `.Rbuildignore` does not exclude the fixture.
- AC8: T9's run at `ebff2c5a`, whose tree outside `cairn/` is identical to HEAD: `devtools::test()` FAIL 0, WARN 12, SKIP 1, PASS 14022. `devtools::check(manual = TRUE)`: 0 errors, 0 warnings, 0 notes, PDF manual OK, `Status: OK`.
- AC4: B's test is unchanged since pass 2 and passed in the AC8 run.
- AC7: not read yet. It is read from the PR's windows-latest job during the step-8 CI wait, before any merge.

Consistency gate, pass 3: `cairn_validate` passed (the same one advisory). `document()` left no diff, with 0 `resolve link` lines. `check_pkgdown()` found no problems. NEWS.md has the entry (NEWS.md:68). The newest master verdicts are success for `R-CMD-check.yaml` and `test-coverage.yaml` (2026-09-23T01:11Z). The three audit scripts exit 0.
- AC6: 38 `xi1` hit lines in the test file and 9 in the oracle driver, read with 5 lines of context. No hit says that an `xi1` mismatch skips a test or that B is not pinned. :695 is history ("Until M150"), and :1094 is about the guard's own CRAN skip.
