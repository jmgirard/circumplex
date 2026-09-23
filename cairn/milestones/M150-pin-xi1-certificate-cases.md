# M150: Price every certificate case from its committed xi1

- **Status:** in-progress
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

- [ ] AC1: In `tests/testthat/test-axes-certificate.R`, eleven tests compare against `cert_frozen`'s exact values: the five bracket tests, the five reference-route tests and counterexample B's test. Each passes the shipped pricing (`axes_v_pricing()`, `axes_u_pricing()`, `axes_accuracy_certificate()`) and the reference route (`axes_dd_pricing()`) a derivative set whose `xi1` is rebuilt from that case's committed upper triangle in `cert_frozen`. No `skip()` in the file is conditioned on `xi1`.
- [ ] AC2: For each of the six cases, a test asserts that the `xi1` this machine builds has the committed length and is within `4 * .Machine$double.eps` of the committed one in every entry, and it fails by the case's name. The test skips on CRAN and nowhere else.
- [ ] AC3: `cert_frozen$cxb` carries counterexample B's `xi1` upper triangle, diagonal included. It is emitted by `devel/degeneracy-oracle/exact_oracle.R` in a run whose output reproduces every other committed field of all six cases bit for bit. The anchor-list test asserts that its length is p(p + 1)/2 for B's three variables.
- [ ] AC4: At counterexample B, the reference-route check for `v` and `v_naive` asserts that `dd_to_double()` of the route's value is identical to the committed `hi` in every component. `u` keeps its absolute bound of `2^-53`. The test's comment states that this identity rests on committed inputs and R-level IEEE arithmetic, not on a stated margin.
- [ ] AC5: B's certificate estimate is observed by calling `axes_accuracy_certificate()` on B's fixture matrix with `axes_se_derivs()` as the package builds it, not with the test's pinned `xi1`. It is observed on macOS arm64 and on linux-arm64 (the `tools/arm64` image). If the two machines print different estimates, the `?axes_reliability` sentence on machine-dependent estimates says so and names one ill-conditioned test matrix as its evidence. If they print the same estimate, the page makes no claim that the estimate depends on the machine. The roxygen is in `R/axes_reliability.R`, and `man/` is regenerated.
- [ ] AC6: A grep of `tests/testthat/test-axes-certificate.R` and `devel/degeneracy-oracle/exact_oracle.R` for `xi1`, with each hit read with five lines of context on each side, finds no comment stating that an `xi1` mismatch skips a test or that counterexample B's `xi1` is not pinned.
- [ ] AC7: On the PR's windows-latest R-CMD-check job, the five anchor reference-route tests, counterexample B's test and the `xi1` guard test run and do not skip. The evidence is that none of their names appears in the skip list of the job's printed testthat output.
- [ ] AC8: `devtools::test()` reports no failures, and `devtools::check(manual = TRUE)` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T2
- AC2 → T2, T4
- AC3 → T1
- AC4 → T3, T4
- AC5 → T5
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
- [ ] T7: Run `devtools::test()` and `devtools::check(manual = TRUE)`. At review, read the PR's windows-latest log for AC7.

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

## Decisions

## Review
