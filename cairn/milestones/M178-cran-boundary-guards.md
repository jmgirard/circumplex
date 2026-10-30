# M178: Run the boundary bug guards on CRAN

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, IP6
- **Resolves:** —
- **Surface tier:** user-facing — the tests ship in the tarball and run in CRAN's own check, where a failure blocks a release
- **Branch/PR:** m178-cran-boundary-guards

## Goal

Five guards against past boundary and order bugs run under CRAN's own check, where `skip_on_cran()` hides them today.

## Scope

**In:** Remove `skip_on_cran()` in place from the 13 `test_that()` blocks of the guard set, named here exactly. Add no CRAN-only copies.

- G1, `tests/testthat/test-ssm_analyze_long.R`: "occasion order follows first appearance, not alphabetical (T10/T2)" and "a factor occasion column uses its levels for order".
- G2, `tests/testthat/test-ssm_sem_groups.R`: "latent group contrast near +/-180 stays on the estimate's branch (sec. 6.4)" and "the reference group follows FACTOR LEVELS, not row order: relabeling flips the contrast (CLAUDE.md grouping contract)".
- G3, `tests/testthat/test-ssm_trajectory.R`: "the fixture really does straddle the 0/360 seam", "the displacement branch is continuous across the seam", "each CI bound lands on its own estimate's branch", "an interval wider than a half-turn stays upright", "the seam guard has teeth against a linear implementation" and "the plotted displacement path is continuous across the seam".
- G4, `tests/testthat/test-cpm_boundary.R`: "free scaling, class A: an in-family angle at the 0/360 pole recovers" and "free scaling, class A: a genuine sigma != 1 pattern recovers at the pole".
- G5, `tests/testthat/test-utils.R`: "angle_dist reports an exact half-turn as +pi, not -pi (F3)".

A bound changes only where AC2 requires it, and a fixture's cost only where AC4 requires it. Also correct the comment above the M168 test in `tests/testthat/test-lavaan-cfi-helper.R` (lines 69-74). It says that the R CMD check job on pull requests does not run `skip_on_cran()` blocks. That is false, because `r-lib/actions/setup-r@v2` exports `NOT_CRAN=true`.

**Out:** The other boundary-matrix cells whose cited tests all skip on CRAN go to the candidate row "Run more boundary cells on CRAN". No change to package code under `R/` or `src/`. No NEWS entry, because only tests and comments change. Refreshing the check times in `cran-comments.md` belongs to `/cairn-release`.

## Acceptance criteria

- [x] AC1: Every block of the guard set runs and passes in CRAN mode. For each of the five files, a non-interactive `Rscript` run of `testthat::test_file()` with `NOT_CRAN=false` and lavaan installed reports each guard-set block of that file by name. Each such block has at least one expectation, no skip and no failure.
- [x] AC2: In each guard-set block, an `expect_*()` call is in scope when it puts an upper bound, a numeric literal or `tolerance =`, on an error whose ideal value is 0. The error is `max(abs(x - y))` for an absolute bound, and the mean relative difference that `all.equal()` reports for `tolerance =`. A bound on numerical noise from arithmetic or an optimizer is at least 10 times its error measured on the review machine. A bound on sampling error keeps its literal, unless the measured error exceeds half of it. A widened bound stays below the error that the block's guarded defect gives.
- [x] AC3: The plant for a block is the defect that the block's comments or source milestone name. Each guard-set block whose expectations or fixture this milestone changes beyond removing `skip_on_cran()` fails under its plant in the package code, run as in AC1. At least one changed expectation fails under the plant. Blocks whose subject is the fixture or a rule written inside the test are exempt ("the fixture really does straddle the 0/360 seam" and "the seam guard has teeth against a linear implementation"). If such a block changes, a fixture that lacks the property it checks makes it fail.
- [x] AC4: In a fresh R session, the sum of the `real` times that testthat's `ListReporter` records for the 13 guard-set blocks in AC1's run is under 5 seconds on the review machine. At planning it was 1.9 s on 2026-10-30 at 97954765.
- [x] AC5: This AC covers each comment block that holds a match of `grep -rn NOT_CRAN tests .github/workflows tools` or of `grep -rni "pull request" tests`. No such block says that the R CMD check jobs on pull requests skip `skip_on_cran()` blocks.
- [ ] AC6: `devtools::test()` reports 0 failures, and `devtools::check(args = "--no-manual")` reports 0 errors, 0 warnings and 0 notes. `R CMD check --as-cran` runs on the built tarball with `NOT_CRAN` unset and lavaan in the check library. It reports `FAIL 0` in `testthat.Rout`, no ERROR or WARNING, and no NOTE that the same command does not report on master.

## Coverage

- AC1 → T3, T7
- AC2 → T2
- AC3 → T4
- AC4 → T3, T7
- AC5 → T5
- AC6 → T1, T7

## Tasks

- [x] T1: Take a baseline on master. Run the five files under `NOT_CRAN=false` and record the 13 blocks that report a skip. Run `R CMD check --as-cran` on the master tarball and record its NOTEs and its SKIP count.
- [x] T2: List the AC2 calls in the 13 blocks by line, and sort each into noise or sampling error. Measure each error on this machine. The audit measured 14.2° against the step bounds of 90, 3.3° against `deg_gap < 12` and 8.9e-15 on the arc span. Widen only the bounds that AC2 requires. Record each call's bound, its measured error and, for a widened bound, the defect's error in the work log.
- [x] T3: Remove `skip_on_cran()` from the 13 blocks. Cut boots or fixture size only if AC4 requires it. Update any comment in the five files that says these blocks skip on CRAN.
- [x] T4: For each block that AC3 covers, plant its defect in a committed copy of the package code, run the block as in AC1 and see the changed expectation fail. Under `devtools::load_all()`, assign the plant in both the namespace and the attached environment. Restore by copy and confirm the file hash. Record each plant, its block and its result in the work log. If no block changed, record that AC3 has no block to cover.
- [x] T5: Correct the comment above the M168 test in `test-lavaan-cfi-helper.R`. Read every hit of AC5's two greps and correct any other comment that makes the same claim.
- [x] T6: In `cairn/boundary-coverage.md`, re-cite the cells that point into the five edited files. Add an M178 audit note that names the cells whose tests now run on CRAN.
- [x] T7: Run AC6's three checks. Then run AC1 and AC4's timed CRAN-mode run in a fresh `Rscript` session and record the per-block times.

## Work log

- 2026-10-30: created by /milestone-plan from the candidate row "CRAN-dark boundary invariants".
- 2026-10-30: tier check: milestone — removing CRAN skips from 13 test blocks across five files is more than a typo and leaves bound and posture choices open.
- 2026-10-30: idea vet: gap, in-place skip removal or CRAN-only copies; took in-place removal, which the row's words allow and which keeps one copy of each check. Gap, the row's M168 note says PR CI skips these blocks; the CI log shows `NOT_CRAN: true` from setup-r and SKIP 104 against 523 skip sites, so took the reading that the note is false and corrected the M168 comment. No flaw.
- 2026-10-30: lessons used: 2026-07-19 (M7 family; CRAN-mode skips show only with `NOT_CRAN` unset), 2026-08-24→2026-09-05 (M108 family; a skip is green), 2026-10-28 (M171; plant in both environments under load_all), the test-craft pointer (M13 and M59 families: prove teeth by plant, derive a bound from the needed discrimination).
- 2026-10-30: collision check: the source row is absorbed in full. M120 (skips added), M149 (D-063, certificate file only) and M168 (the false NOT_CRAN note) are done, with no planned or in-progress overlap. Inbox sweep: 0 open issues, 0 open PRs.
- 2026-10-30: plan gate chose in-place removal of `skip_on_cran()` over CRAN-only slim copies (the M120 `test-pole-values.R` pattern) because copies duplicate each check on CI and must be kept in step; falsified by a block whose CRAN-mode cost or platform noise cannot be cut without losing its CI strength.
- 2026-10-30: question set: whether the two lavaan SEM guards run on CRAN — all five, after the user asked about speed (slowest 2.0.1 flavor 470 of 600 s, guards about 12 s there, other lavaan fits already live on CRAN); posture recorded as D-076.
- 2026-10-30: question set: whether to widen to the other CRAN-dark matrix cells — keep to five; the cells went to the new candidate row "Run more boundary cells on CRAN".
- 2026-10-30: criteria audit (full mode, fresh Opus reader): 12 findings. Fixed: AC2 limited to upper bounds on a zero-ideal error, split into noise (10x) and sampling (keep unless over half), with a defect ceiling and named error metrics; AC3 plant fixed to the block's named defect, failing a changed expectation, with fixture and in-test-rule blocks exempt; AC1 non-interactive Rscript; AC4 names ListReporter `real` in a fresh session; AC5 adds a "pull request" grep; AC6 requires lavaan in the check library. No finding on principles or on instrument wording.
- 2026-10-30: changelog: none, because the change is tests and comments only.
- 2026-10-30: implement started on branch m178-cran-boundary-guards. The untracked `CRAN-SUBMISSION` file at the repo root is unrelated and stays unstaged.
- 2026-10-30: T1 baseline at d56942a8: under non-interactive `Rscript` with `NOT_CRAN=false` and lavaan installed, all 13 guard blocks report a skip. `R CMD check --as-cran --no-manual` on the master tarball with `NOT_CRAN` unset: Status OK, no NOTE, tests 55 s, `[ FAIL 0 | WARN 6 | SKIP 619 | PASS 5792 ]`.
- 2026-10-30: T2 bounds measured on this machine (macOS arm64). Noise, kept: relabel `abs(e2 + e1) < 1e-4` measured 2.85e-7; arc span `tolerance = 1e-9` measured 6.5e-16 and 0 (half-turn block); CPM `F <= 1e-8` measured -1.8e-15 and 0; CPM sigma `tolerance = 1e-6` measured 2.5e-10 and `1e-5` measured 1.4e-10; CPM pole distance `< 1e-3` measured 4.2e-8 both; `angle_dist` near-atom `< 1e-9` measured 1e-12 both. Sampling, kept: `deg_gap < 12` measured 3.34. Widened: relabel `wrap(d2 + d1) < 1e-3` measured 1.14e-4 (8.8x), now 0.01; the guarded defect gives 141°. Out of AC2's scope (ideal value not 0): the step bounds (3.2, 9.1, 14.2 against 90), fixture-shape checks and range checks.
- 2026-10-30: T3 removed `skip_on_cran()` from the 13 blocks; no boots or fixture change was needed. No comment in the five files said these blocks skip on CRAN (the one at `test-cpm_boundary.R:143` belongs to the lower-discrepancy block, which keeps its skip). CRAN-mode run: all 13 report no skip and no failure, 2.84 s summed `real` while a check ran in the background.
- 2026-10-30: T4 the relabel block is the only block changed beyond the skip removal. Plant: deleted `args$group.label <- levels(dat[[grouping]])` in `sem_fit_cfa()` (`R/ssm_sem.R:754`). In CRAN mode the widened `wrap(d2 + d1) < 0.01` failed, as did the `"A - B"` label match and the elevation bound. Restored by copy, same hash 01f38273.
- 2026-10-30: T5 corrected the comment above the M168 test in `test-lavaan-cfi-helper.R`. Read every hit of AC5's two greps: the comment blocks at `test-axes-fiml.R:1170`, `test-axes-certificate.R:138`, `:192`, `:539`, `:1092`, `test-coverage.yaml:64`, the three `tools/m120-*` hits and `tools/arm64/testfile.sh:38` make no false claim.
- 2026-10-30: T6 re-cited the SEM and free-scaling CPM rows of `cairn/boundary-coverage.md` (`:442`→`:456`, `:182`→`:185`, `:466`→`:481`, `:22`→`:26`, `:40`→`:44`, the dropped `:463` was inside the `:456` block) and added an M178 audit note. The CPM class D cell's cites (`:60`, `:72`, `:86`) also moved (`:64`, `:76`, `:90`).
- 2026-10-30: checkpoint: T1-T6 work committed with boxes unticked; the full `devtools::test()` run and the branch tarball check are still running.
- 2026-10-30: verify slot clean at c26eafd1: `devtools::test()` gives `FAIL 0 | SKIP 1 | PASS 15478`. T1-T6 ticked.
- 2026-10-30: T7 at b87f484f (test files identical to the checked tarball by hash). `R CMD check --as-cran --no-manual` on the branch tarball with `NOT_CRAN` unset and lavaan 0.7.2 installed: Status OK, no NOTE, tests 49 s, `[ FAIL 0 | WARN 6 | SKIP 606 | PASS 5847 ]`, 13 fewer skips than master. `devtools::check(args = "--no-manual")`: 0 errors, 0 warnings, 0 notes. Timed CRAN-mode run in a fresh `Rscript` session: all 13 blocks run with no skip and no failure, summed `real` 2.82 s (the planning figure of 1.9 s was taken with `NOT_CRAN=true` in one session with lavaan already loaded).
- 2026-10-30: claim audit: 4 claims read, 1 corrected — tests/testthat/test-lavaan-cfi-helper.R ("every CI job" narrowed to the R-CMD-check and test-coverage jobs, the two that run the suite). The edit is comment-only, after the T7 checks; the file's tests pass.
- 2026-10-30: all tasks done; status set to review.

## Decisions

## Review

Review pass 1, 2026-10-30, at 72e9517b on macOS arm64.

- AC1 evidence: a fresh non-interactive `Rscript` run of `testthat::test_file()` on the five files with `NOT_CRAN=false` (`testthat:::on_cran()` TRUE) and lavaan installed reports all 13 guard blocks by name, each with no skip, no failure and no error, and with 3, 1, 5, 4, 2, 2, 3, 4, 1, 4, 3, 3 and 20 expectations. On master (T1) all 13 reported a skip.
- AC2 evidence: the in-scope calls were re-measured at HEAD with the same fixtures and seeds. Noise bounds: relabel elevation 1e-4 over 2.85e-7 (351x), relabel displacement now 0.01 (`test-ssm_sem_groups.R:366`) over 1.14e-4 (88x), arc span `tolerance = 1e-9` over 6.5e-16 and 0, CPM `F <= 1e-8` over -1.8e-15 and 0, CPM sigma 1e-6 over 2.5e-10 and 1e-5 over 1.4e-10, CPM pole distance 1e-3 over 4.2e-8 and 4.2e-8, `angle_dist` near-atom 1e-9 over 1e-12 and 1e-12. The smallest ratio is 88x. Sampling bound: `deg_gap < 12` over 3.34, under half, unchanged. The one widened bound, 0.01°, is below the 141° that the guarded relabel defect gives. Calls outside scope (ideal value not 0) are listed in the T2 work-log line.
- AC3 evidence: the diff changes one guard block beyond skip removal, the relabel block (one bound). Its plant, deleting `args$group.label <- levels(dat[[grouping]])` from `sem_fit_cfa()`, makes the changed expectation `expect_lt(abs(((d2v + d1v + 180) %% 360) - 180), 0.01)` fail in CRAN mode, along with the `"A - B"` label match and the elevation bound. `R/ssm_sem.R` was restored by copy to the same hash 01f38273, and `git status` shows `R/` clean.
- AC4 evidence: the same fresh-session run gives a summed ListReporter `real` of 2.831 s over the 13 blocks, under 5 s.
- AC5 evidence: `grep -rn NOT_CRAN tests .github/workflows tools` gives 12 hits and `grep -rni "pull request" tests` gives 5 hits. Every comment block holding a hit was read. The corrected block at `test-lavaan-cfi-helper.R:72-75` now says the R-CMD-check and test-coverage jobs run the test. No block says the pull-request check jobs skip `skip_on_cran()` blocks.
- Gate: `cairn_validate.py` all checks passed. No principle text changed, so `cairn_impact` is skipped. Master watches: the newest `R-CMD-check.yaml` and `test-coverage.yaml` push runs with a verdict are success at 0d4b5177 (later master commits are `cairn/`-only). `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R` and `tools/check-branch-protection.R` exit 0. `pkgdown::check_pkgdown()`: no problems. README.Rmd and roxygen untouched, so no `build_readme()` or `document()` diff is owed (document no-diff run below). NEWS: no entry, because no user-visible behavior changes.
