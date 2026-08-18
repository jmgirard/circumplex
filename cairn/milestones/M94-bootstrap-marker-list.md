# M94: Print the fired-marker list on the bootstrap path

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP5
- **Branch/PR:** `m94-bootstrap-marker-list` / [PR #123](https://github.com/jmgirard/circumplex/pull/123)

## Goal

Make `summary()` on a bootstrap-CI `circumplex_cpm` fit print the fired
boundary/weak-identification marker list with its own validation limit stated,
where today only the analytic caution ever names the marker vocabulary.

## Scope

Surface tier: **user-facing** — the deliverable is printed console output, a
help page, and vignette prose, all reaching package users.

**In:**
- A descriptive marker line in `summary()`'s `# Diagnostics` section on
  `ci_method = "bootstrap"` fits, at every N, naming the fired
  `cpm_marker_labels()` labels and stating that the markers were validated as
  interval predictors on the analytic path only (`R/cpm_oop.R:218-222` is the
  insertion region).
- Byte-identity fences on the analytic path and `print()` (the D-010
  coverage-validated caution at `R/cpm_oop.R:234-267` is untouched).
- The `summary.circumplex_cpm` roxygen and the vignette locus paragraph
  (`vignettes/evaluating-circumplex-structure.Rmd:169-172`) updated, with the
  guard test extended.

**Out:**
- Any analytic-path or `print()` output change → none; the analytic caution's
  thresholds and wording stay under D-010.
- Reworking the vignette's boundary demo around the jz2017 bootstrap fit →
  stays as shipped by M92; revisit only on teaching evidence (gate choice
  2026-08-18).
- Marker-conditioned coverage measurement for bootstrap intervals → the
  boundary-regime interval improvement candidate row.
- NEWS.md's historical M92 note → historical record, never updated.

## Acceptance criteria

- [x] AC1: On a `circumplex_cpm` fit with `ci_method = "bootstrap"`,
      `summary()` prints in its `# Diagnostics` section one note whose
      opening sentence — a single sentence, wrapped only at whole-label
      boundaries — names every fired marker by its `cpm_marker_labels()`
      label, at every sample size (no N gate); the caveat sentences that
      follow it are AC2's, not part of this count. Tested on two bootstrap
      fits running unskipped on CI and CRAN (small `boots`): one with
      N < 2000 whose asserted fired set has ≥2 markers (exercising the join)
      and one with N ≥ 2000 firing exactly one; each test asserts the fired
      set via `cpm_boundary_markers()` first, then asserts the marker
      sentence (not the whole section) with its fixed label prefix, naming
      each fired label and no unfired one, and asserts each fired label
      intact in the raw un-normalized output (the whole-label-wrap check);
      asserted phrases are code-composed (the prefix and
      `cpm_marker_labels()` values, never data-borne strings such as scale
      names).
- [x] AC2 (amended 2026-08-18, review round 1 F1): The marker note block
      (the label sentence plus its caveat, as distinct from the whole
      `summary()` output) states its own validation limit: its fixed caveat
      text — pinned exactly by test — says that what has been measured about
      the markers covers analytic intervals only and not every marker was
      measured, and that they are not validated as predictors of the
      bootstrap intervals shown, without asserting any individual marker to
      be a validated predictor; the note block contains none of "mis-cover",
      "near a parameter boundary", "no effect", "does not affect". On a
      bootstrap fit with no fired markers (fired set asserted via
      `cpm_boundary_markers()` first), `summary()` prints no marker note.
- [x] AC3: The analytic path and `print()` are unchanged: analytic-path
      `summary()` output is byte-identical to the merge-base commit's output
      for four fits — clean N ≥ 2000, marker-firing N ≥ 2000, N < 2000, and
      free-scaling N ≥ 2000 — with the capture regenerable at review from the
      merge-base commit (procedure recorded in the test file); on an analytic
      marker-firing fit the fired-label set appears exactly once in
      `summary()` output; `print()` output on a bootstrap marker-firing fit
      is byte-identical to merge-base.
- [x] AC4: The three surfaces this milestone updates agree with the new
      behavior: (a) the `summary.circumplex_cpm` roxygen describes the
      bootstrap marker line and `devtools::document()` runs warning-free;
      (b) the vignette locus paragraph (anchor "`summary()` prints that list
      when", kept in the rewrite) states the new behavior; (c) the guard test
      `tests/testthat/test-cpm_boundary_vignette.R` gains an assertion on
      that paragraph pinning the corrected claim and failing on the retired
      "rather than the list" wording, skipping only where the package
      installs without vignettes. A supplementary `grep -rni "marker"` sweep
      over `R/`, `man/`, `vignettes/`, `tests/testthat/` is reviewed for
      further printing-locus claims and its result recorded in the work log
      (non-load-bearing).
- [x] AC5: The r-package profile's verify commands are clean:
      `devtools::test()` all passing and
      `devtools::check(args = "--no-manual")` at 0 errors / 0 warnings /
      0 notes.

## Coverage

- AC1 → T2, T3
- AC2 → T2, T3
- AC3 → T1, T3
- AC4 → T4
- AC5 → T5

## Tasks

- [x] **T1** — Capture the merge-base baselines and write the byte-identity
      fences (four analytic `summary()` fits + bootstrap `print()`),
      recording the regeneration procedure in the test file.
- [x] **T2** — Write the bootstrap marker-line tests (red first): the two
      CI-runnable fits with fired-set assertions, marker-line prefix/label
      assertions, the exact-text pin, the negative-phrase assertions, and the
      empty-set absence case.
- [x] **T3** — Implement the marker line in `summary.circumplex_cpm`
      (bootstrap branch, after the diagnostics block at
      `R/cpm_oop.R:218-222`); T1/T2 green.
- [x] **T4** — Update the roxygen (`R/cpm_oop.R:138-152`) and `document()`;
      rewrite the vignette locus paragraph keeping its anchor; add the
      guard-test assertion; run the supplementary grep sweep and log its
      result.
- [x] **T5** — Full profile verify: `devtools::test()` and
      `devtools::check(args = "--no-manual")` clean.

## Work log

- 2026-08-18: created by /milestone-plan from the M92-residue candidate row; its promotion condition ("evidence users miss the markers there") had not fired — Jeff overrode the parking at the plan gate by selecting the candidate.
- 2026-08-18: criteria audit ran in full mode ([O] fresh-context reader, two passes): round 1 found AC4's enumerating grep provably missed the vignette sentence being fixed and AC2/AC3 instrument-bound; round 2 found AC2's ban on the token "coverage" colliding with the limit statement it requires, plus missing N-axis and free-scaling probes; all repairs applied to the committed wording.
- 2026-08-18: plan gate chose a caveat-carrying marker line over a bare list because the record validates the markers on the analytic path only (GP2/GP5); falsified by a bootstrap-path marker-coverage measurement showing the caveat wrong or misleading.
- 2026-08-18: plan gate chose the bootstrap-summary()-only locus over all-paths or print() printing because it matches the candidate exactly and leaves D-010's coverage-validated caution untouched; falsified by evidence that analytic sub-2000 or print() readers miss the marker vocabulary.
- 2026-08-18: plan gate chose a minimal vignette correction over reworking the demo around the jz2017 bootstrap fit because the candidate scopes this as a printed-output change, not teaching; falsified by evidence readers misread the boundary section after the change.
- 2026-08-18: T1 done — tests/testthat/test-cpm_summary_markers.R fences the four analytic summary() regimes and bootstrap print() as merge-base snapshots (_snaps/cpm_summary_markers.md; regeneration procedure in the file header), plus the once-per-label assertion on the analytic marker-firing fit; fixtures probed empirically (jz2017 boots=25 fires 3 markers at N=1166; the n=2500 cpm_simulate refit fires exactly small-beta; the clean n=800 draw at 0.15 smallest-beta truth fires none, margins 0.14 vs 0.10 and 7.7e3 vs 1e8); full suite 0 fail / 8349 pass. Implementation gate chose the Note-style line wording (shown verbatim in chat) over the terse "Boundary markers:" prefix for consistency with the neighboring Diagnostics notes; falsified by user feedback the note reads as clutter.
- 2026-08-18: T2+T3 done — four marker-note tests written and run red first (every failure verified to be the note-absent assertion, `is.na(block)` TRUE at test-cpm_summary_markers.R:143 and downstream matches, never another cause), then the note implemented in summary.circumplex_cpm's bootstrap branch (R/cpm_oop.R, after the diagnostics block; strwrap on the variable sentence, literal caveat lines). All 37 file tests green including the untouched AC3 fences; full suite 0 fail / 8375 pass; the one legitimate snapshot change is the new note appended to test-cpm_api.R:636's local-only bootstrap summary render (print() snapshot byte-untouched), accepted and diff-verified.
- 2026-08-18: T4 done — roxygen gains the bootstrap-note sentence (document() warning-free, only man/summary.circumplex_cpm.Rd changed); the vignette locus paragraph rewritten keeping the "`summary()` prints that list when" anchor; the guard test gains the locus-paragraph assertion (fails on the retired "rather than the list" wording; whitespace-normalized matching per the M67 lesson) — 42 guard passes. Supplementary sweep result: `grep -rni marker` over R/, man/, vignettes/, tests/testthat/ reviewed; printing-locus claims existed only on the three updated surfaces — R/cpm_fit.R's catalog comments and R/ssm_ci_oop.R's SSM-side "Boundary markers:" printer state no cpm locus claim, and the "What a fired marker does and does not tell you" measurement-scope prose stays true. Full suite 0 fail / 8380 pass.
- 2026-08-18: minor amendment — discovered sub-task: NEWS.md's unreleased 2.0.0 cpm_fit block describes the analytic marker caution, so one sentence describing the bootstrap note is added beside it (backed by the AC1/AC2 tests per "What gets a test"); the plan's Out item about NEWS referred to the historical M92 note, which stays untouched.
- 2026-08-18: T5 done — `devtools::check(args = "--no-manual")` clean, 0 errors / 0 warnings / 0 notes (32m47s); suite 0 fail / 8380 pass (5 pre-existing lavaan warnings from untouched SEM tests). Mutation probes per the M13-family lesson: restoring the retired locus wording reddened the new guard on all three assertions, and forcing the note onto the analytic path reddened the once-per-label fence with each fired label counted twice; both files restored byte-identical (git diff empty).
- 2026-08-18: all tasks done; status review.
- 2026-08-18: review round 1 returned the milestone (defect return 1 of this milestone): AC1 fails — the note prints outside `# Diagnostics` when no diagnostic line fires (diff-bug F2, reproduced on the N=2500 fixture) and the AC1 tests cannot see it (F3); AC2 needs a gated wording amendment — its mandated "validated as interval predictors" overclaims the record for the removed and multimodal markers (F1). Also queued: F4 vignette topic sentence, F6 NEWS wrap/flow, F7 label-splitting wrap, F9 "reminder" direction; rejected F5 (deliberate local-only print fence) and F8 (repo RNG convention). Status back to in-progress.
- 2026-08-18: amendment return: AC2 — "The marker note block (the label sentence plus its caveat, as distinct from the whole `summary()` output) states its own validation limit: its fixed caveat text — pinned exactly by test — says that what has been measured about the markers covers analytic intervals only and not every marker was measured, and that they are not validated as predictors of the bootstrap intervals shown, without asserting any individual marker to be a validated predictor; the note block contains none of "mis-cover", "near a parameter boundary", "no effect", "does not affect". On a bootstrap fit with no fired markers (fired set asserted via `cpm_boundary_markers()` first), `summary()` prints no marker note." Audited by a fresh [O] reader (one repair round: note-block definition, fit-property empty case, "not every marker was measured"; confirm pass no blocking findings); adopted at the mini gate.
- 2026-08-18: return-round fixes — F2: the note now opens `# Diagnostics` when no diagnostic line fires (condition includes the note); F3: new `m94_expect_note_in_diagnostics()` asserts header presence, order, and no intervening section header on both AC1 fixtures (the N≥2000 fixture additionally asserts 0 diagnostic lines, making it the F2 case by construction); F1: the amended caveat landed on all five surfaces (code, test pin, roxygen/Rd, vignette, NEWS) with a guard assertion banning the retired "validated as interval predictors" phrase from the vignette paragraph; F4: locus topic sentence now scopes the analytic window; F6: NEWS sentence rewrapped and moved after the percentile sentence; F7: label-boundary line packing replaces strwrap (verified in the regenerated snapshot: the label renders unbroken); F9: "reminder" dropped. Mutation probe: reverting the header condition reddened the N≥2000 test on the header assertions. All three touched test files green (44/43/160); document() 0 resolve-link lines.
- 2026-08-18: process slip, recorded honestly — the header-condition mutation probe ran against UNCOMMITTED code and its `git checkout --` restore discarded the return-round edits to R/cpm_oop.R (the M82 lesson exactly); re-applied from the session record and re-verified green. Also: `snapshot_accept()` after a desc-filtered `test_file()` run pruned other tests' snapshots from _snaps/cpm_api.md — restored and re-accepted from a full-file run.

- 2026-08-18: amendment return: AC1 — "one note whose opening sentence — a single sentence, wrapped only at whole-label boundaries — names every fired marker by its `cpm_marker_labels()` label, at every sample size (no N gate); the caveat sentences that follow it are AC2's, not part of this count […] asserts each fired label intact in the raw un-normalized output (the whole-label-wrap check)". Round-2 finding 6: the round-1 F7 label-safe wrap made AC1's "one line" stale. Adopted at the mini gate; the fresh-reader audit passed it with two repairs, both applied (the raw-label assertion added to both AC1 tests so the wrap promise is verified, and the AC1/AC2 sentence partition stated). First amendment return on AC1.
- 2026-08-18: round-2 fix batch — the doubled blank line when the note opens Diagnostics (finding 1; conditional separator), a byte pin of the header seam plus a no-triple-newline assertion (finding 3), packer width harmonized at 70 content chars (finding 4), NEWS caveat completed to all three clauses (finding 2), vignette forward pointer named instead of "next passage" (finding 5). Rejected: finding 7 (cross-surface phrasing drift — content verified identical, registers differ by design) and finding 8 (apostrophe convention — style nitpick).

## Decisions

## Review

Round 1 — 2026-08-18, PR [#123](https://github.com/jmgirard/circumplex/pull/123). Three fresh-context lenses.

Consistency gate (all by command): `cairn_validate` all checks passed; `document()` no diff, 0 `resolve link` lines (cli.width 500); `pkgdown::check_pkgdown()` no problems; README predates the branch (last touched 2026-05-24); NEWS entry present, no milestone numbers; no new top-level files; `check(args = "--no-manual")` 0 errors / 0 warnings / 0 notes (this session, 32m47s); master matrix watch: latest master push run = M93 merge, success (run 32100586209, 2026-08-18).

Lens results: [S] blame-history — 0 findings (D-010 block byte-untouched; marker machinery unmodified; the one behavioral reversal is the milestone's purpose, guarded). [S] prior-PR-comments — no prior-review evidence contradicted (M92's archived findings all stand; PR-thread probe empty). [O] diff-bug — 9 ranked findings:

- **F2 — defect return, AC1 fails as written**: the note prints outside `# Diagnostics` whenever no diagnostic line fires — the header at R/cpm_oop.R sits inside `if (length(diag_lines) > 0)` and the new block outside it. Independently reproduced on the milestone's own N = 2500 fixture: 0 diagnostic lines, no header, note present. Disposition: fix in the return round (header condition includes the note).
- **F3 — fix with F2**: `m94_marker_block()` slices the note out of the capture, so no assertion pins its section; the AC1 tests pass while AC1 is unmet. Disposition: add header-presence and position assertions.
- **F1 — amendment return on AC2**: the mandated sentence "Markers are validated as interval predictors on the analytic path only" over-states the record — the removed-harmonic marker showed no predictive evidence at all, and multimodality was not separately measured; the pre-M94 prose was deliberately negative-only. The overclaim is embedded in AC2's own wording, so the repair routes through the gated criterion-amendment protocol; all five surfaces (code, test pin, roxygen/Rd, vignette, NEWS) re-word together.
- **F4 — fix now**: the vignette topic sentence "prints that list when any marker fires" is false in two analytic regimes the same paragraph then describes.
- **F6 — fix now**: the NEWS insertion leaves a 93-char line and splits the prior sentence mid-flow.
- **F7 — fix now**: `strwrap(width = 74)` splits marker labels mid-phrase ("small / correlation-function weight"); repack at label boundaries.
- **F9 — fix now**: "a reminder" points at material that appears after the paragraph.
- **F5 — rejected**: the `print()` fence running only locally is the plan's deliberate BLAS accommodation (AC3 names no CI requirement; AC1 does and its tests are CI-run).
- **F8 — rejected**: the `.Random.seed` rm-on-exit pattern matches the repo's existing convention (test-cpm_api.R:630); suite-wide RNG-hygiene reform is out of this milestone's scope.

Return: defect return 1 of this milestone (floor: F2 demonstrates AC1 failing inside its domain) plus a pending amendment return on AC2 (F1). No criterion ticked this round. A fresh full-suite run was in flight when the return fired; its greenness is not in dispute — F3 is why it cannot arbitrate AC1.

Round 2 — 2026-08-18, after the return-round fixes (06c19932) and the in-round fix batch (efc42a1c). Lenses: [S] blame-history — no findings, no assertions weakened; [S] prior-PR-comments — no prior-review evidence contradicted, all round-1 fixes independently verified including byte-diffing the amended caveat across the five surfaces; [O] diff-bug — all seven round-1 actioned findings VERIFIED-FIXED (F1 initially partial on NEWS), 8 new candidates: findings 1–5 fixed in-round (seam spacing conditional + byte pin + no-triple-newline assertion, mutation-verified reddening on the reverted condition; NEWS caveat completed; packer width 70; named forward pointer), finding 6 became the AC1 amendment return (adopted at the mini gate, fresh-reader audit passed with two repairs applied — raw-label wrap assertions and the AC1/AC2 sentence partition), findings 7–8 rejected (phrasing-register drift with verified-identical content; apostrophe style).

Acceptance-criterion evidence (all fresh this round):
- AC1: test-cpm_summary_markers.R 51/0 under NOT_CRAN — both fixtures assert fired sets (`{Heywood, small-beta, illcond}` at N=1166; `{small-beta}` at N=2500 with 0 diagnostic lines), the marker sentence with prefix, no unfired label, each fired label intact in raw output, and the Diagnostics-section placement with the seam byte pin. Ticked.
- AC2: exact caveat pin (`m94_caveat_raw`), banned-phrase absences on the note block, empty-set fixture asserts `character(0)` then absence — in the same 51/0. Ticked.
- AC3: fence snapshots byte-unchanged since capture commit 46647e68 (`git diff` empty, reviewer-confirmed); four analytic regimes + bootstrap print() snapshots pass; once-per-label test green; `_snaps/cpm_api.md` additions-only. Ticked.
- AC4: `document()` fresh — no diff, 0 `resolve link` lines; vignette locus paragraph + guard test 43/0 (fails on retired wording, mutation-verified in the T5 probe); grep sweep recorded in the T4 work-log line. Ticked.
- AC5: `devtools::test()` 0 fail / 8395 pass (5 pre-existing lavaan warnings, 3 standard skips); `devtools::check(--no-manual)` 0 errors / 0 warnings / 0 notes (11m49s, post-fix round). Ticked.

Consistency gate round 2: `cairn_validate` all checks passed; `pkgdown::check_pkgdown()` no problems (after the Rd rewording); README untouched; NEWS entry complete; master matrix latest push run success (M93 merge). No Driving RR — projection-vs-outcome no-ops.
