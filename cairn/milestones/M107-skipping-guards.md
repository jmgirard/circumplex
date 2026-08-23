# M107: Make the exemplar-B guards run in the gate that ships a release

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3
- **Branch/PR:** `m107-skipping-guards`

## Goal

Relocate the exemplar-B fixture so the four assertions that today skip under
`R CMD check` execute there, and restore the lavaan cross-check that has
silently stopped comparing.

## Scope

Internal tier: the deliverable is test-suite coverage, and no external
consumer of the package relies on it. Exported behaviour is unchanged.

**In:** move `rb18-counterexample-b.rds` (246 bytes) into
`tests/testthat/fixtures/`, the convention this file already uses at
`test-axes-scaled-fit.R:752`, and repoint the four sites that read it via
`test_path("..", "..", "cairn", ...)` (lines 1601, 1661, 1680, 2085); add a
drift guard tying the packaged copy to the `cairn/` record; replace the three
`lav_fit_cfi()` calls with a helper that probes both argument spellings.

**Out:** the ~50 other sites across 15 test files reading the source tree via
`test_path("..", "..", ...)` — most check `data-raw/` scripts, where skipping
under `R CMD check` may be correct → ROADMAP candidate row, whose promotion
carries the grep that enumerates them rather than a recalled list. Any change
to the degeneracy criterion or its accuracy target → D-048/D-049/D-050 stand
untouched. Keeping mutation-test leftovers out of the tarball → already
landed 2026-08-23 as `d285f7f8`, before this milestone was planned.

## Acceptance criteria

- [ ] In one `R CMD check` run of the built tarball, that run's own skip
      listing contains no skip whose reason names an absent `cairn/` fixture.
- [ ] `grep -n 'cairn' tests/testthat/test-axes-scaled-fit.R` returns no line
      also containing `test_path`.
- [ ] Each of the four relocated assertions is proved to execute under
      `R CMD check` by mutation: with the asserted value altered, that run
      reddens naming that test; restored, it passes. The four are those the
      criterion-2 grep reported before the move.
- [ ] In one `devtools::test()` run, that run's own skip listing contains no
      skip whose reason names `lav_fit_cfi` being uncallable, and the three
      comparisons execute.
- [ ] The helper's older-spelling fallback is exercised by a test that fails
      when the fallback arm is deleted.
- [ ] `PROFILE.md`'s verify slot clean, plus `devtools::check(manual = TRUE)`
      per CLAUDE.md's release check.

## Coverage

- AC1 → T1, T3
- AC2 → T1
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6

## Tasks

- [x] **T1** — Copy the fixture to `tests/testthat/fixtures/`; repoint the
      four sites to `test_path("fixtures", ...)`; drop their
      `skip_if_not(file.exists(...))` guards, which no longer have anything
      to guard.
- [x] **T2** — Drift guard: when `cairn/reviews/rb18-counterexample-b.rds` is
      present, assert the packaged copy is byte-identical to it. This guard
      legitimately skips under `R CMD check` — it fences the `cairn/` record,
      not shipped behaviour — and says so in place.
- [ ] **T3** — Mutation-prove all four run under `R CMD check`, one mutant per
      invocation against committed files (test-craft M82 family); record which
      test each mutant reddened and that it reddened for its own reason.
- [x] **T4** — Helper probing `lav_fit_cfi()` by call, new spelling
      (`x2, df, x2_null, df_null`, lavaan 0.7.2) then old
      (`X2, df, X2.null, df.null`), skipping only if neither returns a finite
      value; repoint lines 536, 922 and 1241. The names stay non-contractual:
      the helper probes, it does not assume.
- [x] **T5** — Mutation-prove the fallback arm: delete it and confirm a test
      reddens on a forced old-spelling call.
- [ ] **T6** — Full verify + `check(manual = TRUE)`; record evidence.

## Work log

- 2026-08-23: created by /milestone-plan.
- 2026-08-23: criteria audit ran in **reduced mode** (internal tier) and was
  **self-run by the planning session, not by a fresh-context reader** — this
  session's tool policy withholds subagents absent an explicit request, so the
  independence the instrument exists for is absent. Recorded rather than
  smoothed over: treat these criteria as unaudited by an independent reader.
  Returned two findings, both fixed before writing: AC1 and AC4 originally
  promised "no skips" over the whole suite, a domain neither run enumerates
  for that claim, and were narrowed to each run's own skip listing; AC3
  originally said "the four sites", a recalled list, and now names the
  criterion-2 grep as what enumerates them.
- 2026-08-23: plan gate chose reviving the lavaan cross-check over deleting it
  because a second opinion against the reference implementation serves IP3's
  two-independent-oracles bar, and the repo's own doctrine (M65/M68 family)
  prescribes run-time feature detection rather than treating an unexported
  function's argument names as a contract; falsified by evidence that probing
  both spellings is itself unstable across lavaan releases, at which point
  deleting the arm beats maintaining it.
- 2026-08-23: plan gate chose fixing the four behaviour-asserting sites now and
  surveying the other ~50 later because the four assert shipped behaviour while
  most of the rest check `data-raw/` scripts whose skipping may be correct;
  falsified by evidence that a skipped source-tree read in the norms-audit or
  vignette families hides a shipped-behaviour defect, which would make the
  survey urgent rather than deferred.
- 2026-08-23: T1 — fixture copied to `tests/testthat/fixtures/`, four sites repointed to `test_path("fixtures", ...)`, their absence guards dropped; suite FAIL 0 / SKIP 3 / PASS 8509, none of the three skips an absent fixture.
- 2026-08-23: T2 — drift guard added as `tests/testthat/test-fixture-drift.R`, comparing raw bytes and skipping when the tracking record is absent; kept out of `test-axes-scaled-fit.R` so that file names `cairn` nowhere (AC2 passes for its own reason, not by splitting a line). Mutation-proved: flipping the packaged copy's last byte reddens the byte-identity assertion at line 30 naming bytes 243-246; restored, it passes. Suite FAIL 0 / SKIP 3 / PASS 8511.
- 2026-08-23: minor amendment — T4/T5 taken before T3, and committed together. T3 needs `R CMD check` runs against committed files, so doing it once after the code settles avoids re-running a ~10-minute check per intermediate state; T4 and T5 are one code change (helper, its tests, three call sites) and split into two commits only artificially.
- 2026-08-23: T4/T5 — `lav_cfi_ref()` in `tests/testthat/helper-lavaan-cfi.R` probes both spellings by call and takes the function as an argument, so a stand-in can force the older arm; three call sites repointed. Both `lav_fit_cfi` skips gone and the three comparisons now execute against lavaan 0.7.2 (whose signature is the newer `x2, df, x2_null, df_null` — the older spelling errors here, which is why they had been skipping). Fallback mutation-proved: deleting the older-spelling arm reddens `test-lavaan-cfi-helper.R:14` alone, returning NULL where 0.6 was expected; restored, six pass. Suite FAIL 0 / SKIP 1 / PASS 8520.
