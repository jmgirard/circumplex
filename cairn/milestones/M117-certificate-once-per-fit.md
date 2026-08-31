<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M117: Price the accuracy certificate once per checked fit

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP2
- **Branch/PR:** m117-certificate-once-per-fit / PR #148

## Goal

`axes_reliability()` prices the per-fit accuracy certificate once per checked
fit instead of the twice recorded at `R/axes_corrected_se.R:737-740`, with no
change to any refusal, warning or reported number.

## Scope

Surface tier: **user-facing** — it changes shipped R code on
`axes_reliability()`'s call path, which external consumers run, even though the
change is behavior-preserving.

**In:** `axes_reliability()` calls `axes_corrected_se()`
(`R/axes_reliability.R:1813`) and `axes_scaling_factor()` (:1922) with the same
matrix and the same derivative set, and each independently calls
`axes_degeneracy_refusal()` → `axes_accuracy_certificate()`. This milestone
gives both surfaces an optional pre-computed-refusal argument that
`axes_reliability()` fills once, keeps both standalone-callable with the
argument absent, and proves the two surfaces still agree on both sides of the
seam.

**Out:**
- memoizing across fits or across sessions → not planned; a per-call seam is
  what the duplication needs
- the three certificate-suite test weaknesses → M116
- surfacing the certificate on computed fits → ROADMAP degeneracy row

## Acceptance criteria

- [x] AC1: For `axes_reliability()` calls whose realigned `cov2cor` matrix
      `axes_sigma_degenerate()` answers `"ill_conditioned"` for — injected at
      the `axes_fitted_cov` binding the way
      `tests/testthat/test-axes-reliability.R:3105` already does —
      `axes_accuracy_certificate()` is entered exactly once per call, counted by
      a trace, on both a listwise-default call and a `missing = "fiml"` call.
- [x] AC2: `axes_corrected_se()` and `axes_scaling_factor()` called WITHOUT the
      new pre-computed-refusal argument each compute the certificate themselves
      and return the `"uncertified"` literal with a warning matching
      `"estimated relative error "` whose estimate is derived from that
      certificate, asserted by a test firing each surface standalone on an
      uncertifiable matrix.
- [x] AC3: On one matrix refused as `"uncertified"`, the warning
      `axes_corrected_se()` emits and the warning `axes_scaling_factor()` emits
      report the same estimated relative error — asserted both where each
      surface computes its own certificate and where both receive the
      pre-computed refusal.
- [x] AC4: `Rscript -e 'devtools::test()'` is clean and
      `git status --short tests/testthat/_snaps/` reports nothing.
- [x] AC5: `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors
      and 0 warnings.

## Coverage

- AC1 → T2, T3
- AC2 → T2, T3
- AC3 → T2, T3
- AC4 → T5
- AC5 → T5

## Tasks

- [x] T1: Measure the wall-clock cost of one `axes_reliability()` call on an
      ill-conditioned p = 24 fit before the change. Record the command, date,
      machine and figure in the work log.
- [x] T2: Add the optional pre-computed-refusal argument to
      `axes_corrected_se()` and `axes_scaling_factor()`, defaulting to computing
      it; `axes_reliability()` computes it once and passes it to both. The
      finiteness / `"singular"` / `"infinite_diagonal"` precedence and the
      `naive_reason` decoupling stay ahead of the seam.
- [x] T3: Write the trace test (AC1, both `missing` paths), the two standalone
      tests (AC2), and the agreement test on both sides of the seam (AC3).
- [x] T4: Re-measure T1's fit after the change; record beside T1's figure.
- [x] T5: `devtools::test()` clean with no snapshot diff;
      `devtools::check(args = "--no-manual")` clean.

## Work log

- 2026-08-30: created by /milestone-plan.
- 2026-08-30: plan gate chose an optional per-call pre-computed-refusal argument over memoizing inside `axes_degeneracy_refusal()` on a cache keyed by the matrix, because a cache would have to decide key identity for a floating-point matrix and would outlive the call; falsified by a second duplication appearing on a path that cannot thread an argument through.
- 2026-08-30: criteria audit ran in **full** mode (user-facing tier), fresh-context [O] reader, two passes. First pass returned three findings — a goal sentence claiming the duplication departs from D-051's decision text when D-051 states a cost model rather than a call-count contract, a criterion binding a before-measurement that cannot be reproduced from the merged tree, and a criterion already green on the pre-change tree. Second pass over the post-gate wording returned three more — a wrong injection-site line reference, one probe standing for a family free in the `missing` path, and an unbounded promise over a warning text whose estimate is machine-dependent by design. All disposed before this file was written.

- 2026-08-31: implement started; branch m117-certificate-once-per-fit. Question gate skipped — both seam surfaces are internal (`NAMESPACE` exports only `axes_reliability`), the plan gate settled the design, no tripwire tags; the two minor choices made here: the argument is named `refusal` (the `axes_degeneracy_refusal()` return, default `NULL` = compute it), and `axes_reliability()` builds it once inline at the seam.
- 2026-08-31: T1 done — scratchpad script (M89 AC6 injection shape: p = 24 octant matrix, item 2 duplicated + 1e-9 ridge, mocked at `axes_fitted_cov`), 5 timed reps after warm-up via `system.time`, Apple M5 Pro / R 4.6.1, 2026-08-31: median 0.033 s per `axes_reliability()` call; one `axes_accuracy_certificate()` evaluation on that matrix: median 0.002 s.
- 2026-08-31: T2 done — `refusal = NULL` argument on both surfaces, consulted only at the `axes_degeneracy_refusal()` seam (every door guard stays ahead); new `axes_shared_refusal()` beside the refusal helper mirrors the door guards and returns NULL when a door would refuse; `axes_reliability()` computes `axes_fitted_cov(fit)` and the refusal once and passes both to the two call sites. test-axes-corrected-se.R, test-axes-scaled-fit.R and test-axes-certificate-refusal.R pass under load_all.
- 2026-08-31: T3 done — two tests appended to test-axes-certificate-refusal.R: an AC1 trace (counting wrapper around `axes_accuracy_certificate`, M89 AC6-style injection at `axes_fitted_cov`, exactly 1 evaluation per call on listwise and fiml, both surfaces still warning "uncertified") and a combined AC2+AC3 test on the committed p = 3 counterexample (each standalone surface moves the trace by one and warns the estimate re-derived independently from the certificate; with the shared refusal passed the trace does not move; one extracted estimate across all four warnings). Discrimination proven: planting the old double evaluation reddened AC1 on both paths (count 2), and a callee ignoring `refusal` reddened the seam-side count (4 vs 3); both plants reverted.
- 2026-08-31: T4 done — same script, machine and date as T1: median 0.029 s per call after the change (was 0.033 s; the saved evaluation is the certificate's own ~0.002 s plus its setup on this p = 24 fit). Both "uncertified" warnings still emitted.
- 2026-08-31: T5 done — `devtools::test()` 0 failed / 9185 passed, `git status --short tests/testthat/_snaps/` empty; `devtools::check(args = "--no-manual")` 0 errors / 0 warnings / 0 notes. Status → review.

- 2026-08-31: review — all five criteria verified with fresh evidence; consistency gate clean; three-lens fan-out returned six findings, none demonstrating a criterion failing, so no floor return.

## Decisions

## Review

### Acceptance-criterion evidence (2026-08-31, branch m117-certificate-once-per-fit @ eb5ea92b, PR #148)

- AC1 — `Rscript -e 'devtools::test()'` and a targeted `test_file()` run of
  `tests/testthat/test-axes-certificate-refusal.R`: the M117 AC1 test passes on
  both the listwise-default and the `missing = "fiml"` call, asserting the
  traced `axes_accuracy_certificate` count is exactly 1 per call while both
  surfaces still warn `"uncertified"`. Discrimination re-proven at review, not
  taken from the work log: with `refusal = NULL` planted at both
  `axes_reliability()` call sites the same test reddens twice (one failure per
  `missing` path) at `test-axes-certificate-refusal.R:647`; the plant was
  reverted and `git status R/` is clean.
- AC2 — same targeted run: the M117 AC2+AC3 test fires `axes_corrected_se()`
  and `axes_scaling_factor()` standalone on the committed p = 3
  `rb18-counterexample-b` matrix without the new argument. Each moves the
  certificate trace by exactly one, returns `reason == "uncertified"`, and
  warns an estimate matched against `axes_certificate_worst()` recomputed
  independently from `axes_accuracy_certificate(cov2cor(S), d)` in the test.
- AC3 — same test: the estimate substring is extracted from all four warnings
  (each surface with its own certificate, each surface given the shared
  refusal) and `unique()` over the four is length one.
- AC4 — `Rscript -e 'devtools::test()'`: 0 failed / 9185 passed / 5 warnings /
  1 skip (the pre-existing lavaan-version fixture skip and the pre-existing
  CPM/lavaan warnings, all on master too). `git status --short
  tests/testthat/_snaps/` printed nothing.
- AC5 — `Rscript -e 'devtools::check(args = "--no-manual")'`: Status OK,
  0 errors / 0 warnings / 0 notes, 8m 14s.

### Consistency gate

- `cairn_validate.py` exit 0, all checks pass; 47 advisory work-log-format
  warnings, all on M7, all pre-existing. `release window` did not fire.
- No `DESIGN.md` principle changed, so `cairn_impact.py` was skipped.
- Toolchain slot (`r-package`): `document()` produced no diff and zero
  `resolve link` lines at `cli.width = 500`; `NAMESPACE`, `man/` and the
  RcppExports pair unchanged. `pkgdown::check_pkgdown()` — no problems.
  README.md is newer than README.Rmd and neither is touched. No NEWS entry is
  owed: the change is behavior-preserving and both seam surfaces are internal
  (`NAMESPACE` exports only `axes_reliability`). No new top-level files.
  Master watches: newest push run reaching a verdict is `success` on both
  `R-CMD-check.yaml` and `test-coverage.yaml` (head `bb0be478`).
  `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R` (5/5
  synthetic payloads ok) and `tools/check-branch-protection.R` all exit clean.

### Independent review (three fresh-context lenses, user-facing tier)

[O] diff-bug, [S] blame-history, [S] prior-review, none having seen the
implementation. The [O] lens verified behavior-preservation by execution, not
by reading: a 13-matrix battery (clean, rescaled, ill-conditioned, negative /
zero / infinite / NA / NaN diagonal, NA and Inf off-diagonal, exactly singular,
indefinite) fired each surface with `refusal = NULL` and with the shared
refusal, and return value and warning vector were `identical()` across all 26
comparisons; end-to-end `axes_reliability()` on a clean p = 24 fit and on the
M89-style injection was `identical()` with the seam live and with
`axes_shared_refusal` mocked to NULL. The [S] blame lens cleared the M66 / M89
/ M108 / M111 / M113 / M114 intent behind every touched line and the
D-044 / D-048 / D-049 / D-051 / D-053 / D-054 family, and confirmed the raw
arm's `naive_reason` decoupling and the guard-order precedence are untouched.
The [S] prior-review lens found the diff implements M111 review finding F14
rather than regressing anything; `gh api .../pulls/comments` returned `[]`, so
no PR-thread surface was walked.

Findings, ranked as reported, with disposition:

- F1 [O] `refusal` is consumed with no check that it belongs to this matrix,
  and the failure is silent and fail-open (`R/axes_corrected_se.R:352-356`,
  `R/axes_scaled_fit.R:247-251`). Demonstrated: handing a clean matrix's
  refusal to the degenerate `rb18-counterexample-b` matrix returns populated
  corrected SEs and emits no warning at all; the reverse direction warns an
  estimate from one matrix beside a condition number from the other, because
  `axes_degeneracy_note()` mixes the passed refusal with the local matrix.
  Unreachable through the exported API today — `axes_reliability()` is the only
  caller and passes a matched pair. Disposition: FIXED at the gate. The
  decision now carries the realigned cov2cor matrix it was priced for in
  `$priced`, and one shared `axes_check_shared_refusal()` aborts at both
  consumption sites when it does not `identical()` the matrix in hand.
  Discrimination proven: deleting both guard calls reddens the new test's two
  `expect_error()` assertions; the plant was reverted.
- F2 [O], echoed by [S] blame: `axes_shared_refusal()`'s header claims it
  returns NULL whenever *either* surface's pre-seam doors would refuse and that
  the guards mirror the callees "verbatim", but `axes_scaling_factor()`'s
  `df_mismatch`, `baseline_df_mismatch` and `saturated` doors are not mirrored
  (`R/axes_corrected_se.R:817-824` vs `R/axes_scaled_fit.R:174-185`). No
  behavior consequence — those doors stay ahead of the seam — but the stated
  contract is false as written. Disposition: FIXED at the gate. The header now
  scopes its NULL promise to the matrix doors the two surfaces share, names the
  three scaling-only doors it does not mirror, and says a non-NULL return does
  not promise that surface reaches the seam.
- F3 [O] the derivative set is built after the matrix guards in the helper and
  before them in both callees, so a malformed derivative spec now aborts from
  `axes_shared_refusal()` rather than from the callee: same condition,
  different call context, and a second way the "verbatim mirror" claim fails.
  Disposition: FIXED at the gate by reordering rather than by documenting — the
  helper now builds the derivative set immediately after realignment, where
  both callees build theirs, so the abort surfaces from the same position.
- F4 [O] a third `axes_se_derivs()` build is constructed and discarded on every
  call, including the common non-degenerate one. Measured at review on the
  clean p = 24 fit (same machine and date as T1/T4): branch 0.042 s and 0.049 s
  across two runs of 7 reps, master's code in the same tree 0.046 s — the
  branch-vs-master gap sits inside this measurement's own run-to-run spread, so
  no clean-fit regression is demonstrated. Disposition: REJECTED at the gate on
  that measurement — the cost is real but below what this fit can resolve, and
  threading the derivative set through both surfaces would widen the seam the
  milestone deliberately kept to one argument.
- F5 [O] `item_block` carries no `= NULL` default in the helper, unlike both
  functions it mirrors, so the natural named-argument spelling errors and the
  new test has to pass NULL positionally (`R/axes_corrected_se.R:826`).
  Disposition: FIXED at the gate — `item_block = NULL`, asserted by the new
  test calling the helper with the named-argument spelling.
- F6 [S] prior-review, surfaced as low-confidence: `axes_shared_refusal()` is a
  third hand-copied instance of the door-guard sequence already duplicated
  between the two surfaces, the divergence class M69 A1 and M71 F1 both hit.
  The lens judged it an acknowledged tradeoff, not a reintroduced defect.
  Disposition: REJECTED at the gate — the duplication is what keeps each
  surface's doors authoritative over its own refusal literals, which is the
  property M69 and M71 were about; the header now says so.

### Re-verification after the gate fixes

`Rscript -e 'devtools::test()'`: 0 failed / 9192 passed (was 9185 before the
fixes; the seven are the new F1 guard test), 5 warnings / 1 skip, all
pre-existing; `git status --short tests/testthat/_snaps/` printed nothing.
`Rscript -e 'devtools::check(args = "--no-manual")'`: Status OK, 0 errors /
0 warnings / 0 notes, 8m 22s. AC1-AC5 therefore stand on the merged tree, not
only on the pre-fix one.

Return floor: no finding demonstrates an acceptance criterion failing, and F1
— the only one with a wrong-number scenario — is unreachable through the
exported API, so none returns the milestone to `in-progress`.
