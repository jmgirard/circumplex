# M86: Name every roster shape the norms-audit builder cannot honestly audit

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP5
- **Branch/PR:** m86-norms-audit-roster-refusals / https://github.com/jmgirard/circumplex/pull/114

## Goal

Every roster and object list `audit_norms()` cannot honestly audit against is
refused by a message naming the instrument, column, or pair at fault.

## Scope

**In:** the roster path of `data-raw/audit-norms.R` — `validate_roster()`,
`roster_from_objects()`, and `audit_norms()`'s argument list and the order in
which it validates them — plus the roster tests in
`tests/testthat/test-norms-audit-roster.R`, the `roster =` call sites in
`test-norms-audit-coverage.R`, `test-norms-audit-markers.R` and
`test-norms-audit-sample-key.R` that the fixture-world exemption's `FALSE`
default requires updating, and the abort-site registration those refusals
require. Absorbs the six M84 review findings scored 20–65 (F2–F7) carried by
the norms-audit roster-builder candidate row.

**Out:** the abort machinery's own matcher and sweep in
`tests/testthat/helper-norms-audit-script.R` — the same-binding-twin defect and
the denylist's field-access-as-value hole stay on their own candidate rows,
which name that helper rather than this script. No shipped `data/` object, no
`R/` surface, and no user-facing behavior changes; this is developer machinery
serving IP5.

## Acceptance criteria

- [x] AC1: `validate_roster()` refuses a roster missing `instrument` with a
      message naming `instrument`, and one missing `sample` with a message
      naming `sample`; the two messages differ. The single
      `%in% names(roster)` condition at `data-raw/audit-norms.R:150`, the
      assertion at `tests/testthat/test-norms-audit-roster.R:206-211`, and the
      registry entry at `tests/testthat/test-norms-audit-markers.R:414-419` are
      replaced by their two-site successors, not left standing. A test asserts
      each message against a roster missing only that column.
- [x] AC2: `roster_from_objects()` refuses an `objects` list carrying one name
      twice, naming the repeated name, rather than rostering the first entry
      twice — measured 2026-08-15, `list(fx = <Sample 1>, fx = <Sample 2>)`
      returns two rows both reading `fx 1`. A test asserts message and refusal.
- [x] AC3: `roster_from_objects()` evaluates `objects[[nm]]$Norms[[1]]` only
      after asserting the entry is a list and its `Norms` a non-empty list,
      each refusal naming the instrument; a `NULL` `Norms` is still skipped and
      `tests/testthat/test-norms-audit-roster.R:100-101` still passes. Measured
      2026-08-15 before the guard: a non-list entry raises `$ operator is
      invalid for atomic vectors`, `Norms = list()` raises `subscript out of
      bounds`, naming neither instrument nor fault. A test asserts each message.
- [x] AC4: `audit_norms()` calls `validate_batch(batch)` before resolving the
      `NULL` roster default. A test binds `shipped_roster` in the sourced
      script environment to a function that aborts distinctively, calls
      `audit_norms()` with a malformed batch and a defaulted roster, and
      asserts the batch's own message surfaces — an assertion that fails
      against the order at `data-raw/audit-norms.R:725-728`. M84's "the default
      roster is resolved before it is validated" test still passes.
- [x] AC5: `tests/testthat/test-norms-audit-roster.R` asserts (a)
      `roster_from_objects()` over the shipped objects `expect_identical()`-equals
      a 24-row (instrument, sample) literal authored as character in the test
      file, the builder's frame compared uncoerced so a type change reddens;
      and (b) M79's gap-equivalence regression, auditing the batch slice with
      that literal as `roster` against the defaulted run. Verified by mutating
      the builder's returned frame 26 times — one per pair dropped, one
      spurious pair added, one `sample` returned numeric — each reddening (a).
      The self-comparing assertion at `:334-335` is replaced by both.
- [ ] AC6: `validate_roster(roster, fixture_world = FALSE)` refuses every
      roster that does not cover every (instrument, sample) pair
      `shipped_roster()` returns, naming the omitted pairs. `fixture_world =
      TRUE` is the only exemption, and it is asked for at the call site rather
      than inferred from the roster: `audit_norms()` passes its own
      `fixture_world` argument through unchanged, and that argument defaults
      to `FALSE`. Nothing in the EXEMPTION decision compares an instrument
      name against a list of known names, so no spelling of an instrument can
      obtain the exemption. Measured 2026-08-15 before the amendment:
      `data.frame(instrument = "CSIE", sample = "1")`, the same with
      `"csie "`, and the same with `NA` each pass `validate_roster()` and
      audit the csie batch slice at 1 non-exempt shipped-sample gap where the
      shipped roster reports 23. Tests assert each of those three shapes is
      now refused, and all four cells of (exemption asked | not asked) x
      (real instrument | fake instrument `fx`) — the exempted-real cell
      included, that cell being the declared lie the exemption deliberately
      does not police.
- [ ] AC7: every `stop()`/`stopifnot()` site this milestone adds to or removes
      from `data-raw/audit-norms.R` is reflected in the abort-site registry —
      the registry/walk set-equality assertion in
      `tests/testthat/test-norms-audit-markers.R` passes, each new site carries
      a fixture that raises at it, and the cross-discrimination matrix shows
      each new message distinguishable from every other registered site's. The
      denylist sweep in `tests/testthat/test-norms-audit-denylist.R` covers the
      aliased and namespaced abort spellings the parse-tree walk cannot see.
- [ ] AC8: `devtools::test()` clean and `devtools::check(args = "--no-manual")`
      at 0 errors / 0 warnings / 0 notes.

## Coverage

- AC1 → T1, T7
- AC2 → T2, T7
- AC3 → T3, T7
- AC4 → T4
- AC5 → T5
- AC6 → T6, T7, T9
- AC7 → T7, T9
- AC8 → T8

## Tasks

- [x] T1: split `validate_roster()`'s `%in% names(roster)` condition
      (`data-raw/audit-norms.R:149-150`) into two named refusals; migrate the
      superseded assertion and add the per-column tests.
- [x] T2: refuse a duplicate-named `objects` entry in `roster_from_objects()`
      (`data-raw/audit-norms.R:546-557`); test.
- [x] T3: guard the `$Norms[[1]]` access — `is.list(entry)`, then the `NULL`
      skip, then non-empty `Norms` (`data-raw/audit-norms.R:559-563`); test.
- [x] T4: move `validate_batch(batch)` ahead of the default-roster resolution
      (`data-raw/audit-norms.R:725-728`); add the stubbed-`shipped_roster`
      ordering test.
- [x] T5: author the 24-pair literal, replace the self-comparing assertion at
      `tests/testthat/test-norms-audit-roster.R:334-335` with the equality and
      gap-equivalence pair, and run the 26 mutations.
- [x] T6: add the shipped-superset refusal to `validate_roster()`; test the
      measured csie shape and confirm the fake-instrument fixtures are untouched.
- [x] T7: register every added and removed abort site; re-run the registry
      set-equality assertion and the cross-discrimination matrix.
- [ ] T8: run the profile's verify slot and the full check (re-runs after T9).
- [ ] T9: replace the instrument-name trigger with the call-site
      `fixture_world` exemption in `validate_roster()` and `audit_norms()`;
      update the 13 fixture `roster =` call sites, attribute a failed
      `shipped_roster()` build to its own message (F2), test and register the
      `NULL` entry refusal and correct its stale comment (F3), and re-derive
      the abort registry and cross-discrimination matrix.

## Work log

- 2026-08-15: created by /milestone-plan.
- 2026-08-15: plan-gate criteria audit ([O], fresh context) ran twice — round 1 returned findings on AC1, AC3, AC4, AC5, AC6 plus three cross-criterion conflicts (AC4's probe could not discriminate the two orders; AC5's mutation was blind at the very pair its test was built around); round 2 over the revised set returned AC5's coercion ambiguity, AC5's dropped gap-equivalence regression, and AC7's universal outrunning its procedure. All fixed before writing; AC6's ambiguity went to the user.
- 2026-08-15: plan gate chose refusing any roster that names a shipped instrument without covering every shipped pair, over per-instrument completeness, because csie ships one sample so the measured 0-gap roster is already per-instrument complete; falsified by a legitimate use for a narrow audit over real data.
- 2026-08-15: plan gate chose a hand-authored 24-pair literal over asserting counts and instrument names, because only the literal reddens on a mistyped sample or a swapped pair; falsified by the literal's maintenance cost exceeding the losses it catches as `data/` grows.
- 2026-08-15: plan gate chose taking the argument-ordering fix with a stubbed-`shipped_roster` test over leaving it out, the shape being unreachable from shipped data today; falsified by the stub proving unbindable in the sourced script environment.

- 2026-08-15: T1 done. The two column guards are written out rather than looped: a loop is one `stop()` call carrying the column as an argument, keying `"`roster` has no `{}` column"`, whose matcher accepts both messages — the matrix would then certify as distinguishable two refusals it cannot tell apart. Deleting the `sample` guard reddens 1 assertion in test-norms-audit-roster.R and 8 in test-norms-audit-markers.R; restore verified by blob hash.

- 2026-08-15: T2 done. `anyDuplicated(nms)` refuses a repeated name, reporting each repeated name once however many times it recurs; measured before the guard, `list(fx = <Sample 1>, fx = <Sample 2>)` returned two rows both reading `fx 1`. Guard sits after the naming check, so an unnamed list still reports as unnamed.

- 2026-08-15: T3 done. Guard order is `is.list(entry)`, then the `NULL` skip, then the non-empty-`Norms` refusal — NULL and `list()` are both length 0 and only the second is a defect. One departure from the plan's two shapes: an ATOMIC `Norms` now reaches the new guard rather than the `is.data.frame()` refusal it fell to through M85, where `(1:3)[[1]]` being 1 made it correct by luck and only for atomics of length >= 1; the message states the actual class and length rather than calling it empty. All 11 norms-audit test files green.

- 2026-08-15: T4 done. `validate_batch()` now runs before the default roster is built. The probe stubs `shipped_roster` in the sourced script environment, which `sys.source()` makes the enclosure of `audit_norms`; measured both ways — green on the new order, and under the old order the call reports `STUB: the default roster was built` rather than the batch's message, so the assertion separates the two orders the plan's first draft could not.

- 2026-08-15: T5 done. The 24-pair literal's origin is a direct `load()` read of `data/*.rda`, not the builder and not the package namespace — the replaced assertion compared the defaulted run against `roster = shipped_roster()` while the default IS `shipped_roster()`, one nullary call on both sides. All 26 mutations of the builder's returned frame redden the equality (24 drops, one spurious pair, one numeric `sample`); the comparison is uncoerced, which is what makes the type mutation reachable.

- 2026-08-15: T6 done. The all-or-nothing rule: a roster naming any instrument in `circumplex:::instrument_names()` must cover every shipped pair, and one naming none is a fixture's own world and is not consulted against `data/`. All 11 norms-audit test files stay green, so no fixture roster in the suite touches a shipped instrument. Cost noted: `validate_roster()` now builds `shipped_roster()` on any real-instrument roster, so the default path derives it twice per run.

- 2026-08-15: T7 done. The walk now collects 31 abort sites, 12 on the roster path; this milestone added 5 (two column refusals, duplicate name, non-list entry, non-indexable `Norms`, narrow roster) and removed 1 (the shared `%in% names(roster)` condition). Registry/walk set-equality and the cross-discrimination matrix both pass, and the denylist sweep stays green at 80. Teeth checked by planting an unregistered `stop()` inside a function no fixture calls — 3 assertions redden; restore verified by blob hash.

- 2026-08-15: T8 done. `devtools::test()` FAIL 0 | WARN 6 | SKIP 3 | PASS 7252; the 6 warnings are lavaan convergence notices in `test-ssm_sem.R` and occasions messages, in files this branch does not touch (`git diff --name-only master..HEAD` is 5 files, none under `R/`, `src/` or `man/`). `devtools::check(args = "--no-manual")` Status: OK, 0/0/0. Stated rather than assumed: the PDF-manual step did not run (grep count 0 for `checking PDF version of manual`), which is what `--no-manual` means and is not coverage; no roxygen or `man/` file is in the diff. Status -> review.

- 2026-08-15: review returned AC6 for a gated criterion amendment. The fresh-context diff lens found the guard evadable by any misspelling or NA instrument name (`"CSIE"`, `"csie "`, `NA` each audit the csie slice at 1 gap against 23), scored 85/85 — but AC6 holds as written, its second clause exempting a roster that names no shipped instrument, so this is evidence about the promise and not the work. F2 (88, the builder's message surfacing from a validator whose subject is the caller's argument) and F3 (80, a NULL entry now aborting where master skipped) ride into the same round. Blame-history and prior-review lenses returned zero findings; ten further findings logged below the action bar.

- 2026-08-15: amendment return: AC6 — "`validate_roster(roster, fixture_world = FALSE)` refuses every roster that does not cover every (instrument, sample) pair `shipped_roster()` returns, naming the omitted pairs. `fixture_world = TRUE` is the only exemption, and it is asked for at the call site rather than inferred from the roster: `audit_norms()` passes its own `fixture_world` argument through unchanged, and that argument defaults to `FALSE`. Nothing in the EXEMPTION decision compares an instrument name against a list of known names, so no spelling of an instrument can obtain the exemption."
- 2026-08-15: the amendment's repair is the narrowing one, not a wider match: the evasion was measured this session (`"CSIE"`, `"csie "`, `NA` each pass and audit the csie slice at 1 non-exempt gap against 23), and matching more spellings would fix membership by author recall. Rejected alternative: keep the name trigger and narrow AC6 to what exact matching settles, logging the residual as a candidate — it loses because the audits that run today keep the hole. Falsified by a fixture world that cannot be declared at its call site.
- 2026-08-15: amendment gate also widened Scope (three test files and `audit_norms()`'s argument list, which the 13 fixture call sites and the new parameter reach) and kept the Goal unamended — an exemption the caller asks for out loud is not a shape that slips through silently, which is what the Goal's "cannot honestly audit" names.
- 2026-08-15: AC7 un-ticked and T8 un-checked: the amendment changes the narrow-roster abort message and adds sites, so the registry, the cross-discrimination matrix and the full check are all re-derived rather than carried over. AC1–AC5 stay verified; their evidence is untouched by the exemption, which sits after the shape guards.
- 2026-08-15: amended-criterion audit ([O], fresh context) ran twice on wording no session author wrote. Round 1 killed the first design outright — marking rosters built by `roster_from_objects()` marks by provenance, and `roster_from_objects(list(csie = <real object>))` is a marked narrow roster over real `data/`, so the exemption would have relocated rather than closed. Round 2 over the call-site design returned the bounded-promise defect repaired, plus three clear fixes taken before writing (qualify the name-comparison sentence to the exemption decision; assert all three measured shapes rather than one exemplar; un-tick AC7) and three judgments routed to the gate.
- 2026-08-15: the T6 work-log line above and the comment at `data-raw/audit-norms.R:184-186` describe the superseded all-or-nothing rule keyed on `instrument_names()`; T9 replaces the comment, and this line supersedes that log entry rather than editing it.

## Decisions

## Review

Reviewed 2026-08-15 on `m86-norms-audit-roster-refusals` at `cf0b19da`, PR #114.

### Acceptance-criteria evidence

Every line below was executed at review, not recalled.

- AC1: `validate_roster(df(sample = "1"))` raises ``roster` has no `instrument`
  column; it has: sample`; `validate_roster(df(instrument = "fx"))` raises
  ``roster` has no `sample` column; it has: instrument`. The two differ and each
  names its own column. Replacement verified rather than assumed: the combined
  `all(c("instrument", "sample") %in% names(roster))` condition occurs nowhere
  outside comments in `data-raw/` or `tests/`, and the walked registry shows 5
  `validate_roster` sites with the two per-column keys and no combined one.
- AC2: `roster_from_objects(list(fx = one, fx = one))` raises ``objects` carries
  the name fx more than once, and only the first entry of a repeated name is
  ever read`. A name repeated three times is reported once.
- AC3: a non-list entry raises `instrument object for fx is not a list but a
  integer`; `Norms = list()` raises ``Norms` for fx must be a non-empty list to
  hold a norms table; it is a list of length 0`. `Norms = NULL` still returns a
  0-row roster rather than aborting, so the M79 skip stands.
- AC4: with `shipped_roster` stubbed to abort in a freshly sourced script
  environment, a malformed batch raises `AUDIT_BATCH$divisor must be numeric,
  not character` — the batch's own message, not the stub's. Measured against the
  pre-M86 order at T4, the same call reported the stub instead.
- AC5: `shipped_roster()` is `expect_identical()`-equal to the 24-pair literal
  read from `data/*.rda` by `load()`. All 26 mutations of the builder's returned
  frame redden the equality — 24 one-pair drops, one spurious pair, one numeric
  `sample`; the last is reachable only because the comparison is uncoerced.
- AC6: `validate_roster(df(instrument = "csie", sample = "1"))` raises ``roster`
  names shipped instruments but omits 23 shipped (instrument, sample) pair(s)`
  and lists all 23. A roster over `fx`/`fy` returns `TRUE`, so a fixture's own
  world is not consulted against `data/`.
- AC7: the walk collects 31 abort sites, 12 on the roster path; registry/walk
  set-equality and the cross-discrimination matrix pass, and the denylist sweep
  is green. Teeth checked by planting an unregistered `stop()` inside a function
  no fixture calls — 3 assertions redden; restore confirmed by blob hash.
- AC8: recorded below with the consistency gate.

### Consistency gate

- `cairn_validate.py` exit 0, all checks passed; 48 advisories, none a gate
  failure. Two concern this milestone: `sizing` flags 8 criteria against the >7
  tripwire (AC8 is the template-mandated verify criterion, so the substantive
  count is 7 — kept as one milestone deliberately, logged at plan time), and
  `work-log format` counts 47 wrapped lines, all pre-existing M7 entries.
- `cairn_impact.py` not run: this milestone changes no `DESIGN.md` principle.
  `Principles touched: IP5` records the principle it works under, not one it
  alters.

### Independent review (three lenses + scorer)

Blame-history lens: zero findings — every deleted condition, assertion and
registry entry is a documented 1:1 or strengthened replacement, and it
confirmed independently that the M79 gap-equivalence fence survives and the
`Norms = NULL` skip is preserved. Prior-review lens: zero findings; its
`gh api .../pulls/comments` probe returned `[]`, so the archived `## Review`
sections were the primary evidence, and it verified each of F2–F7 is closed in
the direction its original finding named. Diff-bug lens: 14 findings, scored by
a fresh agent that did not generate them.

Actioned (>= 80), four:

- F2 (88) — the narrow-roster guard makes `validate_roster()` depend on
  `shipped_roster()` succeeding, so a caller passing a well-formed explicit
  roster meets the BUILDER's message when a shipped norms table is malformed.
  Reproduced by stubbing `shipped_roster` to abort. This is the
  message-precedence inversion T4 removed, reappearing inside T6's guard.
- F3 (80) — `roster_from_objects(list(fx = NULL))` now aborts where master
  silently skipped. A third shape AC3 does not name, with no test and no
  registry fixture, and it makes the M84 comment at `data-raw/audit-norms.R`
  `:590-595` stale.
- F4 (85) — AC6's rule is evadable by any misspelling: `%in% known` is exact
  match, so `instrument = "CSIE"` or `"csie "` audits the csie slice at 1 gap
  where the shipped roster reports 23.
- F5 (85) — an all-`NA` roster clears the same guard, `NA %in% known` being
  FALSE; measured at 1 gap versus 23.

Logged below the bar, ten: F1 (30) duplicate roster pairs double-report gaps
(25 vs 23), sweep unmodified by this diff; F6 (28) `paste()` join key unfenced
against separator collision, no collision constructible against shipped data;
F7 (20) shipped roster derived twice per default run, logged at T6 as accepted;
F8 (12) `validate_batch()` keeps a combined five-column condition, Scope-excluded;
F9 (45) the literal's "no install can shadow it" comment overstates, since
`instrument_names()` reads the installed data index while the objects resolve
from the `load_all()` namespace; F10 (35) `shipped_roster_literal()` recycles
silently on an odd-length literal; F11 (22) the equality pins row order, no
consumer depends on it, passes under `LC_ALL=C`; F12 (12) `roster_from_objects(NULL)`
returns a 0-row roster, pre-existing M84; F13 (12) `obj$Norms` partial-matches
and is re-evaluated, pre-existing; F14 (15) article agreement.

### Disposition: amendment return on AC6

F4 and F5 do not falsify AC6 — they falsify the Goal through it. AC6's second
clause blesses the evasion in so many words ("a roster naming no such
instrument is not consulted against `data/`"), and `"CSIE"` names no instrument
in `instrument_names()`, so the criterion holds exactly as written while 23
shipped samples are reported as covered. That is evidence about the promise
rather than the work, so it routes to the gated criterion-amendment protocol
rather than being fixed under the criterion as it stands. F2 and F3 are carried
into that amendment round for ordinary triage; neither requires a criterion
change. AC1–AC5 and AC7 stay verified on the evidence above and are not
re-opened.
