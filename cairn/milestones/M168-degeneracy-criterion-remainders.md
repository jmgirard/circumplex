# M168: Close the degeneracy criterion remainders

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP3, GP4
- **Resolves:** —
- **Surface tier:** internal — test fixtures, a test-suite detector and a devel-only oracle script; no exported behavior changes
- **Branch/PR:** m168-degeneracy-criterion-remainders

## Goal

The test fixtures name their origin, a lavaan rename can no longer silence the CFI corroboration unnoticed, and the exact oracle's parser refuses malformed hex output.

## Scope

**In:** a `provenance` element on the two fixtures in `tests/testthat/fixtures/` that lack one (`rb18-counterexample-b.rds`, `mc-seeded-pins.rds`), under D-070, and the profile rule's pointer to D-070; a test that fails when `lav_cfi_ref()` (`tests/testthat/helper-lavaan-cfi.R`) gets no CFI from the installed lavaan; a value check in `exact_parse()` (`devel/degeneracy-oracle/exact_oracle.R`) for every `HEX_*` key.

**Out:** showing the certificate's estimate on computed fits (row part iii), declined by D-069 at this plan gate. Regenerating any fixture's values: D-070 keeps the bytes. A generator script for either fixture: rejected in D-070. Running the corroboration detector on CRAN: it skips there, so a lavaan release cannot fail the package's CRAN check.

## Acceptance criteria

- [x] AC1: Every `.rds` file that `list.files("tests/testthat/fixtures", pattern = "\\.rds$")` returns carries a `provenance` element naming its source, its generator, and its seed, or stating which of these was not kept; in `rb18-counterexample-b.rds` and `mc-seeded-pins.rds`, every element present at the plan commit stays `identical()` to its plan-commit value with `num.eq = FALSE`.
- [x] AC2: When lavaan is installed and `skip_on_cran()` does not skip, the test suite fails if `lav_cfi_ref()` gets no CFI from the installed lavaan's `lav_fit_cfi()` under both argument spellings (neither yields a finite number). It passes on the lavaan installed at review, and fails when `lav_fit_cfi_fn()` is replaced by one returning a function that accepts neither spelling, and by one returning `NULL`.
- [x] AC3: `exact_parse()` in `devel/degeneracy-oracle/exact_oracle.R` stops, naming the key, when a `HEX_*` value does not split into the word count its key carries (`n_comp` for `HEX_V*` and `HEX_VNAIVE*`, 1 for `HEX_U*`), or when a word does not match `^-?0x[0-9a-f](\.[0-9a-f]+)?p[+-][0-9]+$`. The script's load-time guard asserts this on an empty value, a wrong word count for an `n_comp = 2` stub, and a non-hex word, and parses a valid stub written in Python's `float.hex()` form; every hex word in `cert_frozen` in `tests/testthat/test-axes-certificate.R` matches the same pattern.
- [x] AC4: The profile's `verify` slot is clean: `devtools::test()` passes.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4, T5, T6

## Tasks

- [x] T1: Add the `provenance` element to `rb18-counterexample-b.rds` (source: the RB18 random search over near-collinear 3×3 correlation matrices, `cairn/reviews/archive/RB18-axes-degeneracy-criterion-metric.md`; generator and seed not kept) and to `mc-seeded-pins.rds` (source: the M4 refactor's pre-refactor capture, legacy archive; seed 2026, calls as in `tests/testthat/test-ssm_montecarlo.R`; read its capture commit from `git log` before writing). Rewrite each file once from its plan-commit object plus the element, then confirm every old element `identical(num.eq = FALSE)` against `git show <plan-commit>:<path>` read into R. Point `cairn/PROFILE.md`'s fixture-provenance bullet at D-070. Grep the tests and `devel/` for any read of these fixtures' element names or length that the new element could change.
- [x] T2: In `tests/testthat/test-lavaan-cfi-helper.R`, add a test gated by `skip_on_cran()` and `skip_if_not_installed("lavaan")` only (not by `lav_fit_cfi_fn()` being non-NULL), asserting `lav_cfi_ref()` returns the closed-form CFI at one input. Plant both AC2 defects through `local_mocked_bindings()` on `lav_fit_cfi_fn` and see each fail before trusting the green; record the plants in the work log.
- [x] T3: In `exact_parse()`, check each `HEX_*` value's word count against its key and each word against the AC3 pattern, stopping with the key named. Extend the `local()` guard with the AC3 stubs. Check the pattern against every hex string in `cert_frozen` with a one-off read of the test file, and record the count in the work log.
- [x] T4: Run `devtools::test()`; review's consistency gate runs the check.
- [x] T5: Review return 1: bring `cairn/PROFILE.md` under its 120-line cap by folding the D-070 pointer into the fixture-provenance bullet's first sentence (ripple #2).
- [x] T6: Land review pass 1's fix-now findings (Review section): the detector's comments (diff-bug #1, #6); both fixtures' provenance text (diff-bug #2, #3); the oracle guard's header, message assertions, `inf` and two-word `HEX_U_LO` stubs (diff-bug #4, #7, #8, prior-review #2); the fixture comment in test-axes-scaled-fit.R (ripple #1).

## Work log

- 2026-10-22: created by /milestone-plan.
- 2026-10-22: tier check: milestone — the row holds a design decision (fixture provenance) and an API question (part iii).
- 2026-10-22: idea vet: (iii) concern that the certificate runs only on selector-routed fits (D-062), so a shown estimate would be empty on almost every fit and binds GP4 — answered at the question set: do not build (D-069); (iv) the row's premise "two hex words" is wrong (`HEX_V*` carry `n_comp` words, `HEX_U*` one, in Python's `float.hex()` form) — reframed in AC3 by the agent; (i) and (ii) none.
- 2026-10-22: collision check: the source candidate row is absorbed whole ((i), (ii), (iv) here, (iii) to D-069); M107 and M148 archives are the origin of (i), (ii) and (iv); D-062 and D-067 bear on (iii) and are cited in D-069; no planned or in-progress milestone overlaps (M167 is blocked on CPM intervals).
- 2026-10-22: inbox sweep: 0 open issues, 0 open PRs on jmgirard/circumplex.
- 2026-10-22: lessons harvested: M65 family (a failing probe skips invisibly; lavaan renames) shapes T2; M108 family and M116 (plant in the code under test) shape T2's plants; M147 (`identical()` needs `num.eq = FALSE`) shapes AC1.
- 2026-10-22: scope widened from the row's (i) to the fixtures directory: a sweep of the 8 `.rds` files found `mc-seeded-pins.rds` also without provenance, so AC1 quantifies over the directory listing.
- 2026-10-22: criteria audit (reduced mode, internal tier, fresh Opus reader): AC1 note that "or capture commit" was weaker than the profile rule — fixed by "stating which of these was not kept" plus D-070's exception; AC2 "under either spelling" read as always failing and "not on CRAN" undefined — fixed to "under both" and `skip_on_cran()`; AC3 unsatisfiable (per-key word counts, Python hex form, platform subnormals) — fixed to per-key counts and a fixed pattern.
- 2026-10-22: question set: show the certificate's estimate on computed fits — not built, recorded as D-069; fixture provenance — record origin and keep bytes, recorded as D-070.
- 2026-10-22: plan gate chose recording provenance over a literal-copy generator because the generator would copy known bytes without rerunning the search; falsified by a fixture whose missing generator hides a stale value a test trusts.
- 2026-10-22: plan chose a CRAN-skipped detector over one that runs everywhere because a lavaan internal rename would otherwise fail the package's CRAN check; falsified by a rename that reaches a release without CI having run on the new lavaan.
- 2026-10-22: no changelog entry: internal tier, no user-visible change.
- 2026-10-22: status in-progress; branch m168-degeneracy-criterion-remainders cut from master at bfdc649f. Untracked `devel/m167-interval-smoke.rds` (M167's) left unstaged.
- 2026-10-22: T1 done: both fixtures gained a `provenance` list (source, generator, seed) via a one-off scratch script; every old element `identical(num.eq = FALSE)` to the bfdc649f copy read through `git show` (5 of 5 and 3 of 3); all 8 fixtures now carry provenance (`growth-fixef.rds` as one string naming all three); mc-seeded-pins' capture read from 6c7e5fc6 and its test comment; PROFILE.md bullet points at D-070; no test reads either fixture's element names or length; certificate, scaled-fit and Monte Carlo test files green. The simple-english hook flagged PROFILE.md's pre-existing text, left as is.
- 2026-10-22: T2 done: "M168: the corroboration reaches the installed lavaan" added, gated by `skip_on_cran()` and `skip_if_not_installed("lavaan")`; green on lavaan 0.7.2 (CFI 0.6). Plants were made in `lav_fit_cfi_fn()` in helper-lavaan-cfi.R, not by mocking, and each was reverted to zero diff. The first plant returned a function accepting neither spelling (it would return 0.6 if reached), and the second returned `NULL`. Each failed the M168 test at line 77 with `actual` NULL. Under `NOT_CRAN=false` the test skips "On CRAN". Implementation choice: planting the code under test over `local_mocked_bindings()` (lesson M116).
- 2026-10-22: T3 done: `exact_parse()` checks each `HEX_*` word count (`exact_hex_words()`: `n_comp`, 1 for `HEX_U_*`) and form (`exact_hex_pattern`), stopping with the keys named. The load-time guard stub now uses float.hex() words at per-key counts and adds empty, wrong-count and non-hex stubs. The guard, run alone, passes, and 383 of 383 hex strings in `cert_frozen` (test-axes-certificate.R lines 237-393) match the pattern. Plants in the parser each fail the guard (count check removed; form check removed), and the unplanted parser passes. A full `Rscript devel/degeneracy-oracle/exact_oracle.R` exits 0 with ANCHORS, SWEEP, REACHABLE and CERTIFICATE all PASS. The header's "carries only `S` and `ia`" claim was reworded, because the fixture also holds `a`, `b`, `kappa` and now `provenance`.
- 2026-10-22: T4 done: `devtools::test()` gave 15274 passed, 0 failed, 0 errors, and 1 skip (test-axes-scaled-fit.R:930, gated on the R and lavaan version, untouched). The 11 warnings all come from untouched files (ci_accuracy, pole-values, print-width, ssm_sem).
- 2026-10-22: claim audit: not owed — internal tier.
- 2026-10-22: status review; no deviations from the plan; no amendments.
- 2026-10-22: review pass 1 checkpoint: AC1-AC3 evidence recorded; the suite, the full check and four reviewers are still running; `cairn_validate` fails weight caps (PROFILE.md 121 lines, cap <120), a gate failure to route once those report.
- 2026-10-22: review return 1: consistency gate FAIL, `cairn_validate` weight caps (`cairn/PROFILE.md` 121 lines, cap <120). Pass 1's eleven fix-now findings ride the same return as T6, and no finding met the return floor. T5 and T6 added (minor amendment), and Coverage AC4 → T4, T5, T6. Status in-progress.
- 2026-10-22: T5 done: the PROFILE.md fixture bullet was rewritten from 7 lines to 4, with the D-070 pointer in its sentences, and the file is 118 lines; `cairn_validate` passes all checks. The simple-english hook's remaining 42 hits are all in pre-existing PROFILE.md text, left as is.
- 2026-10-22: T6 done: The detector comment now says local `devtools::test()` and the master covr job run it, and the R CMD check job does not; it was trimmed to what the code cannot show. The provenance `generator` text was corrected in both fixtures by a scratch script. rb18 now says "copied here in ac7fd860" with the original deleted in 096b7cda. mc-seeded-pins now names the parent of 6c7e5fc6 as the likely code, marked not rerun, and lists each call's arguments, which were checked against test-ssm_montecarlo.R. Every other element is unchanged. The oracle guard header was rewritten. `stops_naming()` now also asserts the `stop()` message, with two new stubs: a two-word `HEX_U_LO` and an `inf` `HEX_U_HI`. A plant that rewords the hex stop as the missing-key stop fails the guard at the message check. The test-axes-scaled-fit.R fixture comment now names the `provenance` field. The guard, the full oracle run (all PASS), and the helper, scaled-fit, certificate and Monte Carlo test files are green.
- 2026-10-22: verify after return 1: `devtools::test()` gave 15274 passed, 0 failed, 0 errors, and 1 skip (the same version-gated skip). Status review.

## Decisions

## Review

- AC1 (2026-10-22, branch at 82ee9c53): `list.files("tests/testthat/fixtures", pattern = "[.]rds$")` returns 8 files, and each one's `provenance` names a source, a generator and a seed (`growth-fixef.rds` does so in one string). Read against the copies at the plan commit bfdc649f through `git show`: `rb18-counterexample-b.rds` keeps 5 of 5 elements and `mc-seeded-pins.rds` keeps 3 of 3, each `identical(num.eq = FALSE)`, and neither had `provenance` at bfdc649f.
- AC2 (2026-10-22, lavaan 0.7.2): the "M168: the corroboration reaches the installed lavaan" test passes, and the file is green. Two plants were made in `lav_fit_cfi_fn()` in helper-lavaan-cfi.R. The first returned `function(a, b, c, d) 0.6`, which accepts neither spelling, and the second returned `NULL`. Each failed that test at test-lavaan-cfi-helper.R:77 with `actual` NULL. The helper was restored with a zero-line diff. Under `NOT_CRAN=false` the test skips with reason "On CRAN", and its gates are `skip_on_cran()` and `skip_if_not_installed("lavaan")` only.
- AC3 (2026-10-22): the parser and its load-time guard, run apart from the oracle, pass. A valid stub in float.hex() form, at the right per-key counts, parses. An empty `HEX_V_LO`, one word under `HEX_VNAIVE_HI` at `n_comp = 2`, and `1.5` under `HEX_U_HI` each stop with "wrong word count or a non-hex word under key(s)" naming that key. Two plants were made in `exact_parse()`, one removing the count test and one removing the form test, and each failed the guard. In `cert_frozen` (test-axes-certificate.R:237-393, cases cxb, a4, a5, c4, b9a, b9b), 383 of 383 hex strings match `exact_hex_pattern`. The full `Rscript devel/degeneracy-oracle/exact_oracle.R` exits 0, with ANCHORS, SWEEP, REACHABLE and CERTIFICATE all PASS on real Python output.
- AC4 (2026-10-22, branch at 145cf6d6): `devtools::test()` gave 15274 passed, 0 failed, 0 errors, and 1 skip (test-axes-scaled-fit.R:930, a version-gated fixture check).
- Gate pass 1, toolchain (2026-10-22): `devtools::check()` gave Status OK with 0 errors, 0 warnings and 0 notes. `document()` emitted 0 `resolve link` lines, and its only diff was the `Config/roxygen2/version` stamp (8.1.0 to 8.1.1), reverted per lesson M85. `pkgdown::check_pkgdown()` found no problems. NEWS has no entry, since the milestone is internal. The diff adds no new top-level file.
- Gate pass 1 (2026-10-22): `cairn_validate` FAIL weight caps, `cairn/PROFILE.md` 121 lines against a cap of under 120, from T1's two added lines. All other checks PASS. The master watches are green: R-CMD-check.yaml and test-coverage.yaml both `success` at c215fe31. `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R` and `tools/check-branch-protection.R` all exit 0.
- spawned: diff-bug, blame-history, prior-review, ripple
- diff-bug #1: the new test's comment says "CI runs it", but R-CMD-check.yaml sets no `NOT_CRAN`, and only the master-push coverage job (covr) runs `skip_on_cran()` tests — fix now, fixed 91bdbfa2.
- diff-bug #2: the mc-seeded-pins `provenance$generator` omits the pre-refactor code's commit and the per-call arguments — fix now; name the parent of 6c7e5fc6 as the likely code, marked not rerun, and list the arguments, fixed 91bdbfa2.
- diff-bug #3: the rb18 provenance says "moved here in ac7fd860", but that commit added the copy and 096b7cda (M108) deleted the original — fix now, fixed 91bdbfa2.
- diff-bug #4: the guard header in exact_oracle.R still says the stubs test "the key check alone and not about any number" — fix now, fixed 91bdbfa2.
- diff-bug #5: the new test checks dependency behavior or a guard on untouched code, which "What gets a test" lists under Never — reject, planned change: AC2 names this test as the deliverable.
- diff-bug #6: the new test's comments carry bug history, a count that can go stale ("all three"), and a restatement of the code — fix now; trim to what the code cannot show, fixed 91bdbfa2.
- diff-bug #7: `stops_naming()` asserts only the key name, not which `stop()` fired — fix now; also assert each stop's message fragment, fixed 91bdbfa2.
- diff-bug #8: the comment says `inf` and `nan` stop, but no stub runs that case — fix now; add an `inf` stub, fixed 91bdbfa2.
- diff-bug #9: exact_oracle.R:67 says "hi/lo hex double PAIRS", which can be misread as two words per key — reject, false: the pairs are split across the `_HI` and `_LO` keys as written, and the new comment at the count check states the counts.
- diff-bug #10: `exact_hex_words()` assigns `n_comp` words to an unrecognized `HEX_` key — reject, edge: only a key the Python script does not print reaches this branch, and its effect would be a stop.
- blame-history #1: the CRAN skip reopens M107's silent skip on CRAN — reject, planned change: Scope `Out:` keeps the detector off CRAN so that a lavaan release cannot fail the package's CRAN check.
- blame-history #2: no byte fence enforces D-070's "committed bytes are its definition" — reject, false: D-070 promises no fence, and the certificate and refusal tests assert the matrix's load-bearing properties.
- blame-history #3: the parser contract matches exact_oracle.py, and no D-entry conflicts — noted.
- prior-review #1: the CRAN skip is the same failure class as M107 — reject, planned change, as for blame-history #1.
- prior-review #2: the guard has no stub for an over-long word list or a two-word `HEX_U_*` value — fix now; add a two-word `HEX_U_LO` stub, fixed 91bdbfa2.
- prior-review #3: the provenance cites `cairn/reviews/archive/…`, which is not in the tarball — reject, false: the text is a pointer into the repo record, as the m66 fixtures' `devel/` generator paths are, and no code reads it.
- prior-review #4-#7: the M148 item (iv) is closed, M107's helper findings are untouched, the fixtures' old elements are identical, and the comments hold — noted.
- ripple #1: test-axes-scaled-fit.R:1656-1668 lists the fixture's elements without `provenance` and calls the missing seed "the whole of the provenance gap" — fix now, fixed 91bdbfa2.
- ripple #2: PROFILE.md's fixture sentence does not point at D-070 from its first line — fix now, inside the rewrite that the weight-cap failure needs, fixed 2232808e.
- ripple #3, #4: the skip comments in test-axes-scaled-fit.R and helper-lavaan-cfi.R stay accurate — reject, false: both still describe what a NULL does at those call sites.
- ripple #5: the help text and comments that describe the matrix stay accurate — noted.
- Pass 2 (after return 1, branch at 03c6a5ee; master unmoved): fresh evidence follows.
- AC1 pass 2: all 8 fixtures carry `provenance` naming source, generator and seed. Against bfdc649f, rb18 keeps 5 of 5 elements and mc-seeded-pins 3 of 3, each `identical(num.eq = FALSE)`. Both `generator` texts read as corrected in T6.
- AC3 pass 2: the guard passes; the count-check and form-check plants each fail it; in `cert_frozen` (lines 237-393), 383 of 383 hex strings match; the guard's five hex stubs are an empty value, one word under `HEX_VNAIVE_HI`, two words under `HEX_U_LO`, `1.5`, and `inf`; and the full oracle run prints its 4 PASS lines.
- AC2 pass 2 (lavaan 0.7.2): the detector passes. The two plants in `lav_fit_cfi_fn()` (a function accepting neither spelling, and `NULL`) each fail it at test-lavaan-cfi-helper.R:76 with `actual` NULL. The helper was restored with a zero-line diff, and under `NOT_CRAN=false` the test skips "On CRAN".
- spawned: diff-bug, blame-history, prior-review, ripple
- Measured for pass 2's provenance items (2026-10-22, R 4.6.1, macOS): `git archive a1593dd3` (6c7e5fc6's parent) was installed into a scratch library and the three pinned calls rerun. The stored `num()` vectors differ from mc1-mc3 by at most 5.6e-17, 3.5e-14 and 7.1e-14, never bit for bit. The current code gives the same three figures, so the capture code is not identified, and the bytes are the pins' only exact form.
- diff-bug #1: the mc-seeded-pins provenance omits the `num()` step that makes each stored vector — fix now, fixed 31ad8e98.
- diff-bug #2: D-070's carve-out ("an input matrix found by search is not one") does not cover mc-seeded-pins, which pins output — reject, false: the test reruns the recorded calls on every run and compares within 1e-10, and they reproduce within 7.1e-14, so the reproducibility rule's purpose (a stale value fails a test) holds; D-070 governs only the bytes, which no rerun gives exactly.
- diff-bug #3: "most likely the parent of 6c7e5fc6 (not rerun)" is unverified — fix now with the measured rerun above, fixed 31ad8e98.
- diff-bug #4: the detector comment's list of runners omits local `devtools::check()`, which sets `NOT_CRAN` — fix now, fixed 31ad8e98.
- diff-bug #5: the test-axes-scaled-fit.R fixture comment restates the source and seed now held in `provenance`, and line 1669 is 86 characters — fix now: point at the field and rewrap, fixed 31ad8e98.
- diff-bug #6: asserting the exact CFI also fails if lavaan changes its formula — reject, planned change: AC2 names this test, and a changed formula is something the corroboration should also flag.
- blame-history #1, #2: the CRAN skip leaves the old CFI comparison silent on CRAN, and the closed-form check is the class D-063 keeps live there — reject, planned change: Scope `Out:` and the plan's work-log line record the trade-off (a lavaan internal rename must not fail this package's CRAN check), and D-063 binds only the certificate file.
- blame-history #3: mc-seeded-pins has a rerunnable origin at 6c7e5fc6's parent, so D-070 should not apply — reject, false: the rerun above reproduces no value bit for bit from that commit, so no rerun gives the bytes D-070 governs.
- prior-review #1: the CRAN skip is the M107 silent-skip shape — reject, planned change, as for blame-history #1.
- ripple #1, #2: test-axes-fiml.R:1169-1171 says every CI run, pull requests included, sets NOT_CRAN, and test-axes-certificate.R:1097 says "CI runs it", but R-CMD-check.yaml and `check-r-package@v2` set no `NOT_CRAN`, so pull requests skip those tests — follow-up: the lines predate this diff and are untouched, absorbed into the "CRAN-dark boundary invariants" candidate row.
- ripple #3: exact_oracle.R:67-68 "hi/lo hex double PAIRS" — reject, false, as for pass-1 diff-bug #9.
- ripple #4: DESIGN.md:98 describes M148's guard without M168's stops — reject, false: it is the record of what M148 fixed, and it stays accurate.
- ripple #5: helper-lavaan-cfi.R:20-22 and test-axes-scaled-fit.R:1254-1256 say a NULL makes the caller skip — reject, false, as for pass-1 ripple #3 and #4.
