# M107: Make the exemplar-B guards run in the gate that ships a release

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3
- **Branch/PR:** `m107-skipping-guards` / https://github.com/jmgirard/circumplex/pull/136

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

- [x] In one `R CMD check` run of the built tarball, none of the four sites
      the mutation criterion names skips, per that run's own skip listing,
      and no skip in that listing names `rb18-counterexample-b.rds`. Other
      skips in that listing are out of this criterion's reach.
- [x] `grep -n 'cairn' tests/testthat/test-axes-scaled-fit.R` returns no line
      also containing `test_path`.
- [x] Each of the four relocated assertions is proved to execute under
      `R CMD check` by mutation: with the asserted value altered, that run
      reddens naming that test; restored, it passes. The four are the
      fixture read sites reported by `git show
      68ada0f8:tests/testthat/test-axes-scaled-fit.R | grep -n
      'test_path("\.\.", "\.\.", "cairn"'`, each with the assertion in the
      `test_that` block containing it.
- [x] In one `devtools::test()` run, that run's own skip listing contains no
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
- [x] **T3** — Mutation-prove all four run under `R CMD check`, one mutant per
      invocation against committed files (test-craft M82 family); record which
      test each mutant reddened and that it reddened for its own reason.
- [x] **T4** — Helper probing `lav_fit_cfi()` by call, new spelling
      (`x2, df, x2_null, df_null`, lavaan 0.7.2) then old
      (`X2, df, X2.null, df.null`), skipping only if neither returns a finite
      value; repoint lines 536, 922 and 1241. The names stay non-contractual:
      the helper probes, it does not assume.
- [x] **T5** — Mutation-prove the fallback arm: delete it and confirm a test
      reddens on a forced old-spelling call.
- [x] **T6** — Full verify + `check(manual = TRUE)`; record evidence.

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
- 2026-08-23: amendment (substantive, gated) — AC1 rewritten and AC3's enumerator repinned. AC1 as written could not pass: the check run's skip listing names an absent `cairn/` directory for `test-norms-provenance.R:581,703`, two of the ~50 sites the Out section defers, and the same wording also caught the T2 drift guard the plan declares a correct skip. AC3 named "the criterion-2 grep", which AC2 requires to return nothing once T1 lands, so it enumerated the empty set; it now pins the pre-move revision `68ada0f8`, where the grep reports exactly the four Scope lines 1601/1661/1680/2085. Neither promise widened: AC1's fixture clause narrows to one filename, AC3's promise is unchanged with a resolvable enumerator.
- 2026-08-23: criteria audit on the amended AC1/AC3 ran in **reduced mode** (internal tier) with a **fresh-context [O] reader** that authored neither text, at the user's instruction — the independence the planning session recorded as absent. Two passes, both returning findings on the bounded-promise question, none on proportionality or instrument: pass 1 found AC3's enumerator unresolvable and AC1 asserting an unenumerated identity; pass 2 found AC1's replacement sentence a universal claim over every other skip that is false (the drift guard and the lavaan-absent skips are neither deferred sites nor source-tree reads). Both fixed at the gate, the second by the user's call since AC1 had re-entered once. Reader also verified the AC3 command resolves to exactly four lines.
- 2026-08-23: T3 — all four mutation-proved under `R CMD check` on the built tarball, one mutant per invocation against committed files, each reddening alone (FAIL 1) for its own reason (the altered literal): line 1628 → "AC2/AC3: the committed exemplar B is refused by both surfaces at p = 3"; 1668 → "M90 AC5: the one recorded negative-cval matrix is refused by the criterion, not the backstop"; 1696 → "M90 AC5: the backstop's own literal is 'ill_conditioned' (branch WIRING only...)"; 2090 → "M106 AC2: the near-duplicate geometry computes and the RR18 fixture still refuses". Restored after each; tree clean.
- 2026-08-23: T6 — verify slot clean (`devtools::test()`: FAIL 0 / WARN 5 / SKIP 1 / PASS 8520; the one skip is the pre-existing fixture-environment gate at `test-axes-scaled-fit.R:918`). Release check `devtools::check(manual = TRUE)` clean: 0 errors, 0 warnings, 1 NOTE, 8m38s. The NOTE is this machine's check tooling, not the package — `/usr/bin/tidy` is Apple's 2006 build, whose `--version` carries no version triple for R's `.find_tidy_cmd` pattern, and the `V8` package is not installed, so R skips HTML validation and math rendering. Both are present on CRAN's machines; remedy on this Mac is `brew install tidy-html5`, `R_TIDYCMD=/opt/homebrew/bin/tidy` in `~/.Renviron` (needed because `/usr/bin` precedes `/opt/homebrew/bin` on PATH), and `install.packages("V8")`.
- 2026-08-23: AC1 evidence — a separate clean `R CMD check` of the built tarball, kept so its skip listing is readable (`devtools::check` prints none on a passing run): FAIL 0 / SKIP 153 / PASS 7462. No skip in that listing names `rb18-counterexample-b.rds`, and none of the four sites appears in it; the file's only skips are `:918` (fixture environment), `:966` (vignette source) and `:1140` (R/ sources absent), all outside the four. Two skips still name an absent `cairn/` directory (`test-norms-provenance.R:581,703`) — the deferred sites the amended criterion no longer reaches — and one is the T2 drift guard skipping as designed.
- 2026-08-23: no NEWS entry — internal tier, exported behaviour unchanged; the deliverable is test coverage under `R CMD check`.

## Review

PR: https://github.com/jmgirard/circumplex/pull/136 (draft; opened at review start).
Base `master` at `68ada0f8` had not moved since the branch was cut, so no merge
was needed before gathering evidence.

### Acceptance-criteria evidence

- **AC2 — pass.** `grep -n 'cairn' tests/testthat/test-axes-scaled-fit.R`
  returns one line, 1605, a prose comment; piping it through `grep test_path`
  exits 1 (no line). The criterion's promise holds for its own reason: the
  drift guard that does read `cairn/` lives in a separate file.
- **AC4 — pass.** A fresh `devtools::test()` run at `52b251be`:
  FAIL 0 / WARN 5 / SKIP 1 / PASS 8520. The run's skip listing has exactly one
  entry, `test-axes-scaled-fit.R:918` ("fixture was generated under a different
  R or lavaan version") — the pre-existing M68 environment gate. No skip names
  `lav_fit_cfi`. The two `skip_if()` calls guarding the comparisons (lines 532
  and 1235) therefore did not fire, so all three comparisons — `got$fit$cfi`
  vs `r1`, `got2$fit$cfi` vs `r2`, and `res$fit$cfi` vs `ref` — executed and
  passed against lavaan 0.7.2.
- **AC1 — pass.** A fresh `R CMD build` + `R CMD check --no-manual` of
  `circumplex_2.0.0.tar.gz` at `1f1f66a7`: FAIL 0 / WARN 4 / SKIP 146 /
  PASS 7507. The tarball ships the fixture
  (`circumplex/tests/testthat/fixtures/rb18-counterexample-b.rds`, listed by
  `tar tzf`). That run's skip listing names exactly three entries in
  `test-axes-scaled-fit.R` — `:918` (M68 fixture environment), `:966`
  (vignette source), `:1140` (package R/ sources absent). The four sites are
  the `test_that` blocks opening at 1592, 1661, 1672 and 2069, none of which
  appears. `grep -c rb18-counterexample-b.rds` over the whole check test log
  returns 0, so no skip in the listing names the fixture. The remaining skip
  groups (On CRAN, `cairn/` not present, `data-raw/` not present, the drift
  guard's "repo tracking record absent") are out of this criterion's reach.
- **AC3 — pass.** The criterion's enumerator, re-run fresh, reports exactly
  four lines on `68ada0f8`: 1601, 1661, 1680, 2085. Their enclosing
  `test_that` blocks are, in order, "AC2/AC3: the committed exemplar B is
  refused by both surfaces at p = 3", "M90 AC5: the one recorded negative-cval
  matrix is refused by the criterion, not the backstop", "M90 AC5: the
  backstop's own literal is 'ill_conditioned'", and "M106 AC2: the
  near-duplicate geometry computes and the RR18 fixture still refuses". Four
  fresh `R CMD check` runs of the built tarball, one mutant each, altering only
  the asserted literal at the post-move line (1628, 1668, 1696, 2090
  respectively). Every run: FAIL 1 — the mutated test alone, named in the
  failure header, failing for its own reason (`actual "ill_conditioned"` vs
  `expected "ill_conditionedXX"`). Restored between runs; the tree is clean.
  The mutant runs used `--no-vignettes --ignore-vignettes` for speed, which
  raises their skip count to 153 against the baseline's 146; the four sites
  execute in both.
