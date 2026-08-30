# M114: Pin the shared refusal predicate, and assert the dead literal rather than dropping it

- **Status:** review
- **Priority:** normal
- **Depends on:** M113
- **Driving RR:** —
- **Principles touched:** GP2
- **Branch/PR:** `m114-refusal-predicate-fences` / https://github.com/jmgirard/circumplex/pull/145

## Goal

Fence the one-predicate-both-surfaces refusal design against the per-surface
alternative it was chosen over, and assert the deadness claim that motivated
dropping `"ill_conditioned"` from the nestedness set without paying the
backstop coverage dropping it would cost.

## Scope

Surface tier: **user-facing** — both criteria fence which refusal literal a
user is shown.

**In:** a committed input whose certificate estimates straddle
`axes_degeneracy_delta_star`, asserted to refuse at both surfaces and shown to
redden when the shared `max()` at `R/axes_corrected_se.R:765` is replaced by
each surface reading only its own field (M111 review F4); and an added
assertion on the AC8 grid in `tests/testthat/test-axes-scaled-fit.R` that no
`reason` field there returns `"ill_conditioned"`, with the literal kept in
`check_nested()`'s set (M111 review F12, as repaired by the criteria audit).

**Out:** the certificate's estimand and the FIML denominator → M113. The
packaged bracket's platform reach → M115. The M108/M111 cosmetic and
performance residue → the ROADMAP degeneracy candidate row.

## Acceptance criteria

- [x] AC1 A committed input on which the certificate's fields straddle
      `axes_degeneracy_delta_star` — at least one at or below it, at least one
      above — is asserted to refuse `"uncertified"` at both surfaces; with the
      shared max replaced by each surface reading only its own field, that
      assertion reddens at one surface. Where no matrix producing a straddling
      set is found, the same assertion is driven by a stubbed
      `axes_accuracy_certificate()` return instead.
- [x] AC2 `check_nested()`'s literal set in
      `tests/testthat/test-axes-scaled-fit.R:1409` still contains
      `"ill_conditioned"`, and the AC8 test additionally asserts that on every
      matrix its own grid drives, neither surface's `reason` field returns that
      literal.
- [x] AC3 `devtools::test()` clean; `devtools::document()` no diff and no
      unresolved-link warning at pinned `cli.width`;
      `devtools::check(args = "--no-manual")` 0 errors / 0 warnings / 0 notes.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4

## Tasks

- [x] T1 Search the anchor families and the committed counterexample for an
      input whose certificate fields straddle `axes_degeneracy_delta_star`;
      record the search and its outcome either way.
- [x] T2 Commit the straddling input (or the stub), assert `"uncertified"` at
      both surfaces, mutate the predicate to per-surface fields, record the
      redden, revert and verify the tree clean.
- [x] T3 Add the AC8 grid assertion, keeping `"ill_conditioned"` in the guard
      set; prove it able to fail by planting that return.
- [x] T4 NEWS entry if any user-visible behaviour moved; profile verify and
      consistency-gate slot.

## Work log

- 2026-08-30: created by /milestone-plan.
- 2026-08-30: criteria audit ran in FULL mode (declared user-facing tier) as part of the joint M113/M114/M115 run; its finding on AC2 is disposed in the gate line below, and its instrument-property finding on AC1's "the search that failed is recorded" clause was fixed by moving that clause to T1.
- 2026-08-30: plan gate chose keeping `"ill_conditioned"` in `check_nested()`'s set and asserting its deadness on the AC8 grid over dropping it as M111 review F12 proposed, because `axes_scaling_factor()`'s `cval <= 0` backstop still emits that literal (`R/axes_scaled_fit.R:302`, asserted at `test-axes-scaled-fit.R:1764`), so dropping it would silently retire nestedness coverage there; falsified by that backstop being removed or proved unreachable.
- 2026-08-30: `Depends on: M113` because M113 adds a third certificate field, which changes what a straddling set means for AC1.
- 2026-08-30: pre-implementation gate: the per-surface alternative AC1 must redden against is `max(se, fiml_ratio)` at the SE helper and `cval` at the scaling surface — the partition each function's own return defines; the M113 review's F3 fail-open on a short certificate was declined for this milestone and stays on the ROADMAP degeneracy row.
- 2026-08-30: T3 run ahead of T1/T2 (minor reorder — the AC2 assertion is independent of AC1's input search, which runs long).
- 2026-08-30: T3 done. `check_nested()` now asserts `reason` is not `"ill_conditioned"` at both surfaces on every matrix the AC8 grid drives, with the literal kept in the guard set; planting `list(reason = "ill_conditioned", ...)` in `axes_degeneracy_refusal()` reddened both new assertions (`actual TRUE` at the near-singular matrix, all three p), reverted and green at 348 passing.
- 2026-08-30: T1 done. Searched the three anchor families' stated parameter space and a neighbourhood of the committed counterexample for a straddling certificate: 9,588 candidates, 6,666 of which reached the certificate, 388 straddling. Families A and B produce none at any parameter tried -- their three fields climb together to ~1e-7 and then both pricing routes fail at once, so all three become the sentinel with nothing between; the committed counterexample does not straddle either (se 3.4e-2, cval 4.9, fiml_ratio 8.7e-4, all above the target). Family C does, at p = 4: across item-error variances 2.5e-9 to 8e-9 with xi1 = 0.1, xi2 = 0.3 the cval estimate runs 400-5000x the target while the SE vector's and the quotient's sit about 1000x below it. Search scripts in the session scratchpad, not committed.
- 2026-08-30: T2 done. The straddle is committed as a closed-form call on the existing `m106_family_c()` builder at three band members (3e-9, 5e-9, 8e-9) -- deterministic, no fixture, no seed -- and both surfaces are asserted to refuse `"uncertified"` on each, the SE helper being the one whose own two fields are inside the target. Mutating `axes_degeneracy_refusal()` to read `max(se, fiml_ratio)` at the SE helper and `cval` at the scaling surface reddened the SE helper at all three members (`reason` NULL rather than `"uncertified"`, with the raw arm's `"ill_conditioned"` surfacing in `naive_reason`); reverted, `git status` clean under R/. A second stubbed-certificate test covers the other two fields, which the search found on the far side only in company.

- 2026-08-30: T4 done. No NEWS entry: the branch changes tests and tracking only, and no user-visible behaviour moved. `devtools::test()` 0 failures / 9,100 passing, with the suite's 5 warnings and 1 skip all in files this branch does not touch (`test-ci_accuracy.R`, `test-ssm_sem.R`, and the pre-existing fixture-version skip at `test-axes-scaled-fit.R:921`); `devtools::document()` no diff and no unresolved-link warning at pinned `cli.width`; `devtools::check(args = "--no-manual")` Status OK, 0 errors / 0 warnings / 0 notes in 8m 48.5s; `cairn_validate` all checks pass.

## Decisions

## Review

PR: https://github.com/jmgirard/circumplex/pull/145. Master had not moved since
the branch was cut (`origin/master..HEAD` two commits, `HEAD..origin/master`
empty), so no merge was needed before gathering evidence.

### Acceptance-criterion evidence (fresh, this session)

- **AC1 — met.** The committed straddle is `m106_family_c(eps, xi1 = 0.1,
  xi2 = 0.3)` at eps 3e-9 / 5e-9 / 8e-9, p = 4. On this machine all three
  members straddle: `cval` 0.35 / 0.50 / 0.040 against a target of 1e-4, with
  `se` and `fiml_ratio` at 3.5e-8 to 1.2e-7 — three to four decades inside it.
  Both surfaces refuse `"uncertified"` on each, and the test asserts the
  straddle facts before the refusals and refuses to pass on an empty band.
  Mutation, run fresh here: `axes_degeneracy_refusal()` rewritten to read
  `max(se, fiml_ratio)` under the SE helper and `cval` under the scaling
  surface reddened the SE helper at every band member — `reason` NULL rather
  than `"uncertified"`, `corrected` finite, no warning emitted (failures at
  `test-axes-certificate-refusal.R:415/417/420/427/429`, hitting testthat's
  failure cap). Reverted; `git status` clean, and the two touched files back to
  0 failures / 2,279 passing. The second AC1 test drives the other two fields
  from a stubbed certificate, with a both-directions liveness check on the stub.
- **AC2 — met.** `check_nested()`'s guard set at
  `test-axes-scaled-fit.R:1425-1426` still lists `"ill_conditioned"`, unchanged.
  The two added `expect_false()` assertions sit unconditionally in
  `check_nested()`, which the AC8 grid calls on all 34 inflation matrices per
  map plus the indefinite and near-singular cases, across three maps — no
  branch skips them and no loop is empty. Non-vacuity proved fresh here by
  planting `reason = "ill_conditioned"` on the certificate branch of
  `axes_degeneracy_refusal()`: five of the new assertions reddened at both
  surfaces before the failure cap. Reverted; tree clean.
- **AC3 — met.** `devtools::test()` 0 failures / 9,100 passing (5 warnings and
  1 skip, all in files this branch does not touch);
  `Rscript -e 'options(cli.width = 500); devtools::document()'` exit 0, zero
  lines matching `resolve link`, and `git status` empty afterwards;
  `devtools::check(args = "--no-manual")` Status OK — 0 errors, 0 warnings,
  0 notes, 8m 7.6s.

### Consistency gate

`cairn_validate.py` exit 0, all checks pass (47 advisory work-log-format warnings,
all pre-existing in M7). No `DESIGN.md` principle changed, so `cairn_impact.py`
does not apply. Toolchain slot: `document()` no diff and no unresolved-link
warning (above); no generated file hand-edited; README.md in sync;
`pkgdown::check_pkgdown()` "No problems found"; no NEWS entry owed (tests and
tracking only, no user-visible behaviour moved); no new top-level files;
`devtools::check()` clean (above). Master watches: newest push run on master
reaching a verdict is `success` for both `R-CMD-check.yaml` and
`test-coverage.yaml` (2026-08-30T20:11:30Z). `tools/check-master-red-alert.R`,
`tools/master-red-alert-dryrun.R` (5/5 synthetic payloads ok) and
`tools/check-branch-protection.R` all exit clean.

### Independent review — three lenses, fresh context

Declared tier is user-facing, so the full fan-out ran.

**[S] blame-history — no findings.** The pre-existing pointwise nestedness
assertion is untouched (same guard set, same `expect_identical`); the two new
`expect_false()` calls are strictly additive and strictly stronger. The literal
retention matches M111 F12's recorded disposition; F4 and M113's F3 are named as
deferred rather than silently dropped. No `R/` or `src/` change anywhere in the
diff.

**[S] prior-PR-comments — no findings.** The `gh api .../pulls/comments` probe
returned `[]` (no inline review comments at all), so the PR-thread walk was
skipped. On the archived `## Review` record the diff complies rather than
regresses: it is the fix M111 F4 asked for, it follows the M108-era lesson to
fence a refusal warning's *route* and not its digits, and it carries the
non-empty-domain guard that lesson requires.

**[O] diff-bug — ten findings, ranked.** Verdict: both criteria met in
substance, tests mutation-sensitive, every load-bearing factual claim in the new
comments verified true except as noted. Findings and dispositions:

1. `test-axes-certificate-refusal.R:378,387` — `straddling` is built with
   `format(eps)` on scalars but filtered with `format(m114_straddle_eps)` on the
   vector, and `format()` pads a numeric *vector* to a common mantissa width.
   Confirmed here: `format(c(1e-9, 1.25e-9, 3e-9))` gives `"1.00e-09"` where
   `format(1e-9)` gives `"1e-09"`. Harmless on today's band, but widen it and
   the non-vacuity guard can pass while the assertion loop runs zero times.
   **Fix now.**
2. `:481-511` — the stubbed certificates are hand-written three-field lists, so
   a fourth certificate field (as M113 added `fiml_ratio`) would be pinned by
   nothing while all six straddle assertions stayed green. This is the field-set
   drift `axes_certificate_sentinel()` exists to prevent, reintroduced test-side.
   **Fix now.**
3. `:354,504` — neither committed domain asserts its own length, against this
   file's own convention at `:51`. A dropped case would lose coverage silently.
   **Fix now.**
4. `:384` — `straddling` is checked non-empty but never against an expected set,
   so a partial band collapse is invisible. **Reject:** the tolerance is
   deliberate, and bounding it re-imports the M113 windows problem in weaker form.
5. `:526-527` — the literal `"0.01"` in the warning grep is derived from
   `axes_degeneracy_delta_star` but hard-coded, so a change to that constant
   fails these greps for an unrelated reason. **Fix now.**
6. Milestone file `:31` cites `R/axes_corrected_se.R:765` for the shared `max()`,
   which is at `:789` (comparison) and `:800` (definition); AC2's own
   `test-axes-scaled-fit.R:1409` citation is now self-stale at `:1425`.
   **Reject:** pre-existing on master, and both live in plan-owned text review
   must not edit.
7. `:447-450` — "AC2 above" inside a block headed "M114 AC1" means *M111* AC2,
   and that test can `skip_if_not_installed("lavaan")`. The liveness argument
   does not depend on it, so this is comment precision. **Fix now.**
8. `:407-408` — df hard-coded where `m111_dfs()` exists twenty lines up for
   exactly this reason. Correct today and a mismatch would redden rather than
   pass. **Fix now.**
9. `:460-473` — the stub test does not assert one warning per refusal, unlike its
   sibling at `:427-428`. **Fix now.**
10. Observation, not a defect: the straddle rests on the three fields disagreeing
    by six decades on one matrix, so a later correction to the `cval` estimate
    may collapse it — loudly, thanks to the `:384` guard. **No action.**

Return floor: none of the ten demonstrates an acceptance criterion failing, and
the diff changes no shipped behaviour, so none is a load-bearing defect in what
the package does for users. No status return.

### Fix-now work at the gate

The maintainer chose "fix six, then merge" at the approval chip, so findings 1,
2, 3, 5, 7, 8 and 9 were repaired on the branch (seven changes; 3 and 9 are one
sitting each at two sites). Findings 4, 6 and 10 stand rejected as recorded
above. Changes, all in `tests/testthat/test-axes-certificate-refusal.R`:

- F1: the band-membership set is now a logical mask indexed positionally, so no
  `format()` round-trip stands between the straddle test and the loop it feeds;
  the non-emptiness guard is `expect_true(any(straddling))`.
- F2: `expect_named(axes_certificate_sentinel(), c("se", "cval", "fiml_ratio"))`
  now precedes the stubs, so adding a fourth certificate field reddens here
  rather than leaving the hand-written stubs silently short.
- F3: `expect_length(m114_straddle_eps, 3L)` and `expect_length(straddles, 3L)`.
- F5: the expected note text is built with `sprintf("estimated relative error
  %.2g", hi)` rather than the literal `"0.01"`.
- F7: the comment now names the M111 AC2 test and says outright that the
  liveness argument rests on the all-fields-inside baseline run in this test,
  not on a case that can skip without lavaan.
- F8: the AC1 df pair comes from `m111_dfs(4L, ang, scl)`.
- F9: one warning per refusal asserted at both surfaces in the stub test.

`expect_length()` takes no `label`, so the two count assertions are
`expect_identical(length(...), 1L, label = ...)`.

Re-verified after the fixes: the two touched files 0 failures / 2,288 passing
(up 9 from the pre-fix 2,279); the AC1 per-surface mutation still reddens, at
`test-axes-certificate-refusal.R:428/430/433/440/442` across the band, reverted
with `R/` clean; `devtools::check(args = "--no-manual")` Status OK — 0 errors,
0 warnings, 0 notes, 7m 44.2s.
