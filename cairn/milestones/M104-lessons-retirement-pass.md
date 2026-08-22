# M104: Graduate the matured verification-craft families out of LESSONS.md

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m104-lessons-retirement / https://github.com/jmgirard/circumplex/pull/133

## Goal

Free durable byte headroom in `cairn/LESSONS.md` by graduating six matured
verification-craft families into a repo-local doctrine module, landing the two
lessons M102 could not.

## Scope

Surface tier: **internal** — the deliverable is `cairn/` tracking records, read
only by the cairn skills; no external consumer of the repo relies on them.

**In:** author `cairn/test-craft.md` holding the six graduated families
verbatim; trim `cairn/LESSONS.md` to a pointer line; fold M102's two orphaned
lessons into the graduated M95-family entry; record the D-entry superseding
M44's holding; retire the parked-lessons ROADMAP row.

The six families exit under the **maturation** exit (tracking-rules "Retiring a
lesson"): each teaches transferable craft, each has been extended or
consolidated at least twice, and neither the enforcement nor the ownership exit
applies — no test fails on these mistakes, and `PROFILE.md` at 119 of 120 lines
cannot own them. Measured: M13 1,456 B · M82 714 B · M59 1,078 B · M60 877 B ·
M75 887 B · M95 728 B = 5,740 B over six lines.

**Out:** the M56 stranded-prose family stays in `LESSONS.md` → it is records
craft rather than verification craft and is the file's most actively recurring
line. Compressing the remaining lines → not attempted; the caps rule reaches
for compression last. Any pointer in `PROFILE.md` → declined at the gate on its
one line of headroom; reachability rides the `LESSONS.md` pointer instead.
Retiring lessons under the enforcement exit (shipping guards) → a later
milestone that opens the guarded code.

## Acceptance criteria

- [x] AC1: `wc -l -c cairn/LESSONS.md` reports fewer than 50 lines and fewer
      than 16,000 bytes.
- [x] AC2: no lesson text is lost in the move — every `- ` line that
      `git diff master..<branch> -- cairn/LESSONS.md` reports as removed has its
      bolded family headline and each of its parenthesized milestone-tag clauses
      found by `grep -F` in `cairn/test-craft.md`. The diff's removed-line set
      is the domain swept.
- [x] AC3: `cairn/LESSONS.md` carries one pointer line naming
      `cairn/test-craft.md` and stating what it holds.
- [x] AC4: `cairn/test-craft.md`'s M95-family entry carries both lessons M102
      could not land — the greedy-span-extraction lesson and the
      YAML-plain-scalar lesson — in the wording the ROADMAP's parked-lessons row
      preserved.
- [x] AC5: `cairn/DECISIONS.md` carries a new D-entry superseding M44's holding
      that the retirement pass happens at a shipping milestone's post-merge
      hygiene, citing the M100/M101/M102 hygiene stamps as the evidence.
- [x] AC6: six greps are run — one per graduated family tag (`M13 family`,
      `M82 family`, `M59 family`, `M60 family`, `M75 family`, `M95 family`)
      across `cairn/` and `CLAUDE.md` — and every hit each returns either sits
      inside `cairn/test-craft.md` or states a claim still true after the move.
- [x] AC7: the ROADMAP's parked-lessons candidate row is retired.

## Coverage

- AC1 → T2
- AC2 → T1, T2, T3
- AC3 → T2
- AC4 → T1
- AC5 → T5
- AC6 → T4
- AC7 → T6

## Tasks

- [x] T1: author `cairn/test-craft.md` — a scope header declaring what the file
      holds and that `/milestone-plan` reads it when the scope touches
      verification, then the six family entries moved byte-for-byte from
      `cairn/LESSONS.md`, with M102's two lessons folded into the M95-family
      entry from the ROADMAP parked row's preserved wording.
- [x] T2: trim `cairn/LESSONS.md` — remove the six graduated lines, add the
      pointer line, and check the header note (`:3`) for anything the move makes
      false. Measure with `wc -l -c`.
- [x] T3: run the loss sweep — take the removed `- ` lines from
      `git diff master..<branch> -- cairn/LESSONS.md`, `grep -F` each headline
      and milestone-tag clause in `cairn/test-craft.md`, record the counts.
- [x] T4: run the six citation greps across `cairn/` and `CLAUDE.md`; the known
      sites are `cairn/DECISIONS.md:846`, `:1091`, `:1524`. Fix what the move
      makes stale, in both directions (the M56-family lesson).
- [x] T5: append the D-entry superseding M44's holding — quote M44's rationale,
      state what changed (three hygiene passes at 4 bytes, M102's lost lessons),
      and record what would reopen it.
- [x] T6: retire the ROADMAP parked-lessons candidate row; restate the hygiene
      stamp with the measured counts.
- [x] T7: gate checks — `python3 ~/.claude/skills/cairn/scripts/cairn_validate.py`
      clean, and `Rscript -e 'devtools::test()'` clean as a no-regression check
      (no code is touched, so this carries no acceptance criterion).

## Work log

- 2026-08-22: created by /milestone-plan.
- 2026-08-22: plan gate criteria audit ran in REDUCED mode (internal tier, no RB tripwires), fresh-context [O] reader, two passes. Pass 1 returned one instrument finding (AC5 bound the ROADMAP hygiene stamp, a record that counts were verified, where AC1 already binds the counts) — fixed, the stamp moved to T6. Pass 2 over the gate-revised set returned one bounded-promise finding (AC6's universal quantified over "references to graduated material" while its procedure swept a hand-list of six tag spellings — the M102 shape) — fixed by narrowing the promise to what the six named greps sweep.
- 2026-08-22: plan gate chose a standalone retirement pass over folding it into the next shipping milestone's post-merge hygiene (M44's standing holding) because that mechanism has now failed three consecutive times — M100, M101 and M102 each reported the same 4 bytes of headroom and M102 lost two lessons to it; falsified by a shipping milestone's hygiene pass actually retiring a lesson and freeing bytes without a dedicated milestone.
- 2026-08-22: plan gate chose the six verification-craft families over adding the M56 stranded-prose family (7 lines, 6,692 B) or graduating only the three heaviest (3,486 B) because the six are one genus and the M56 line is the file's most actively recurring; falsified by LESSONS.md returning to its byte cap within a few milestones, or by a plan-time read missing a graduated family it needed.
- 2026-08-22: plan gate chose a pointer line inside `LESSONS.md` over a pointer in `PROFILE.md`'s test-doctrine slot because PROFILE.md is at 119 of 120 lines and `LESSONS.md` is already read whole twice per planning run; falsified by a planning run reaching the pointer and not the module.
- 2026-08-22: plan gate chose folding M102's two lessons into the M95-family entry over landing them as standalone lines because the parked row already assigns them to that family; falsified by either lesson recurring in a shape the M95 headline does not cover.

- 2026-08-22: T1+T2 committed together — LESSONS.md trimmed without the module in place would be data loss, so the move is one commit. `cairn/test-craft.md` 23 lines / 7,065 B holds the six families; `cairn/LESSONS.md` 45→40 lines, 19,996→14,738 B (AC1: under 50 lines and under 16,000 B).

- 2026-08-22: T3 loss sweep — `git diff master..HEAD -- cairn/LESSONS.md` reported 6 removed `- ` lines (the domain); all 6 bolded family headlines and all 48 parenthesized milestone-tag clauses found by `grep -F` in `cairn/test-craft.md`; 0 failures (AC2).
- 2026-08-22: T4 citation sweep — the six family-tag greps over `cairn/` and `CLAUDE.md` returned 13 hits: 6 in `cairn/test-craft.md` (the graduated content), 6 in M104's own AC6 wording (names the tags to grep — still true), 1 in `cairn/milestones/archive/M96-master-red-alert.md` ("Lesson graduated into the M95 family line" — still true, names no file, IP4 history). No hit directs a reader to `cairn/LESSONS.md` for graduated content (AC6).
- 2026-08-22: T4 also checked the three sites the greps cannot reach (they cite lessons by other spellings): `cairn/DECISIONS.md:846` and `:1524` cite the M7 family, which did not graduate; `:1091` cites "the M59/M61 lesson" and names no file. All three still true, and all three IP4 history that is superseded rather than edited.

- 2026-08-22: T5 — D-046 appended, superseding M44's holding that retirement runs at a shipping milestone's post-merge hygiene. Its narrow claim: the maturation exit cannot be reached from a shipping milestone's scope, so post-merge hygiene stays the ordinary route for the enforcement and ownership exits (AC5).
- 2026-08-22: T6 — parked-lessons candidate row struck as a tombstone pointer; ROADMAP 59 lines, 23,668→23,190 bytes (AC7). Minor task deviation: T6's "restate the hygiene stamp" is NOT done here — the stamp records the last hygiene check, which is still the M103 merge; writing an M104 stamp before the merge would claim a check that has not run. It belongs to review's post-merge hygiene.

- 2026-08-22: T7 — `cairn_validate` all checks pass, 47 advisories (the M103-merge baseline, M7's pre-M28 multi-line work-log WARNs). `devtools::test()`: FAIL 0 | WARN 5 | SKIP 3 | PASS 8395, exit 0. The branch touches only `cairn/*.md`, so the suite is a no-regression check by construction; `git status` clean after the run (M86).

## Decisions

## Review

**Evidence (fresh, post-fix, 2026-08-22).**

- AC1 — `wc -l -c cairn/LESSONS.md` → **40 lines / 14,861 bytes**; under 50 and under 16,000. Was 45 / 19,996 on master.
- AC2 — `git diff master..HEAD -- cairn/LESSONS.md` reports **6** removed `- ` lines (the domain). All 6 bolded family headlines and all **48** parenthesized milestone-tag clauses found by `grep -F` in `cairn/test-craft.md`; **0 failures**. The [O] reviewer independently confirmed five of the six lines byte-identical old vs. new, the M95 line diverging only at its date range plus the appended M102 text.
- AC3 — one pointer line in `cairn/LESSONS.md` names `cairn/test-craft.md` and lists all six graduated headlines.
- AC4 — the M95-family entry in `cairn/test-craft.md` carries both M102 lessons; `grep` finds "GREEDY span extraction overruns once added text repeats its terminator" and "YAML plain scalar cannot hold" in that entry, matching the ROADMAP row's preserved wording apart from one sentence-initial `a`→`A`.
- AC5 — `cairn/DECISIONS.md:1560` carries D-046.
- AC6 — the six family greps over `cairn/` and `CLAUDE.md` return 10 sites: 6 in `cairn/test-craft.md` (the graduated content), 3 in M104's own file (AC6's wording and T4's log line), 1 in `cairn/milestones/archive/M96-master-red-alert.md` ("Lesson graduated into the M95 family line" — still true, names no file, IP4 history). **No site directs a reader to `cairn/LESSONS.md` for graduated content.**
- AC7 — the parked-lessons row is struck as a tombstone; zero live parked rows remain.

**Consistency gate.** `cairn_validate`: all 16 checks PASS, exit 0, 47 advisories (the M103-merge baseline). No principle change (`Principles touched: —`), so `cairn_impact` skipped. Toolchain slot (`PROFILE.md` consistency-gate): `devtools::check(args="--no-manual")` **0 errors, 0 warnings, 0 notes**, 8m03s; master watches both green on the newest push run reaching a verdict (`R-CMD-check.yaml` and `test-coverage.yaml`, M103 merge); both master-red-alert audits exit 0; `.Rbuildignore:22` `^cairn$` already covers the new file; no NEWS entry owed (zero non-`cairn/` files changed). Byte budgets by hand: ROADMAP 23,342 < 24,000; LESSONS 14,861 < 20,000.

**Independent review.** Internal tier + docs-only diff → single [O] diff-bug lens (12 findings, ranked). Every factual claim was verified against the record before triage.

*Fixed at this gate:*

- **F1** — D-046's "verbatim" quote elided "When the next milestone captures a lesson it will hit 50 lines and FAIL `weight caps`;", leaving "that milestone's post-merge hygiene" without its antecedent and inverting M44's holding. Confirmed against `archive/M44-lessons-consolidation.md:10-11`. Sentence restored to the quote.
- **F2** — D-046 claimed forced-time hygiene "freed nothing". Confirmed false: the M100 stamp (`7b0e6dc4`) records that pass retiring the M95-family line's two prescriptions under the **ownership** exit, spending the bytes on two new recurrences. D-046 rewritten to say what is true — the *maturation* exit is what four passes never reached — and to say explicitly that hygiene is not inert.
- **F9** — the evidence was under-counted at three passes; M103's stamp (`dcc5034a`) reports the same 4 bytes. Corrected to four throughout D-046.
- **F4** — the new tombstone said the row "had stood since the M100 stamp". `git log -S` puts its creation at `be08ec28` (M102 merge). Corrected: the row dates to M102, the 4-byte condition to M100.
- **F3** — the ROADMAP stamp still asserted "unchanged at 45 lines / 19,996 bytes … so the parked-lessons row stands" while this branch struck it. Past-tensed and annotated with what M104 changed, without claiming a hygiene check that has not run.
- **F5** — `cairn/test-craft.md` asserted the `LESSONS.md` caps "exist to force retirement of lessons that have not matured", a rationale no record holds and the sentence licensing the file's uncapped status. Rewritten as M104's own choice, citing where the caps are actually stated.
- **F7** — the pointer line broke the format `LESSONS.md:3` declares (no date, no milestone tag), the one thing T2 was told to check. Header amended to admit a pointer line explicitly.

*Recorded, not fixed:*

- **F8** — T6's work-log figure "23,668→23,190 bytes" is wrong at both ends (branch was 23,672 when T6 ran; HEAD is 23,342 after this gate's fixes). Confirmed. The work log is IP4 history, so it is corrected here rather than edited: this is the M87 clause of the M56-family line — a hand-pinned count of a moving artifact cannot stay true — recurring inside the very milestone that moved that lesson.
- **F10** — the maturation exit requires the archive summary to name what graduated. Not yet due; discharged at step 9 hygiene.
- **F12** — no defect; the M95 date-range restyle to `2026-08-18→2026-08-21` is correct (M102 merged 2026-08-21, `6ec6816a`).

*To the maintainer at the approval gate:* **F6** and **F11** are real and not fixed here — F6 because Scope is plan-owned and amendment-gated, F11 because the fix lies outside this repo's diff. Both appear verbatim in the gate presentation.

**Return floor.** No finding demonstrates an acceptance criterion failing inside its named procedure's domain; AC1–AC7 all hold as written. No defect return, no amendment return. Defect-return count for M104: 0.

