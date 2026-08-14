# M84: Validate the norms-audit roster at its boundary

**Status:** done (2026-08-14, PR #112 https://github.com/jmgirard/circumplex/pull/112)

**Goal:** `audit_norms()` refuses a roster it cannot audit against and `shipped_roster()`
refuses a degenerate norms table by name, so no audit reports a clean run over unread data.

**Outcome:** `validate_roster()` refuses a non-data-frame roster, one missing `instrument`/`sample`,
and a zero-row one, running after `validate_batch()` and after the `NULL` default resolves.
`roster_from_objects()` takes over the derivation and refuses a non-data-frame `Norms[[1]]`, a
rows-but-no-`Sample`-column table, an `NA` sample, and an unnamed or `NA`-named object list; a
zero-row `Norms[[1]]` stays skipped. `shipped_roster()` is its no-argument wrapper, so
`roster = shipped_roster(objects)` — M79's return-2 narrowing — is unspellable; 13 call sites
migrated, 7 abort sites registered. The csie slice reports 23 gaps defaulted and 23 passed
explicitly, against 0 for a capitalised-column or empty roster before the guard.

**Decisions:** AC2 and AC3 amended at the implement gate to name the builder, both cleared by a
fresh-context criteria audit; a fourth refusal, the unnamed object list, added there under AC5.
The same-binding-twin candidate declined on the merits — all seven new sites carry distinct messages.

**Review:** Blame-history and prior-review lenses returned zero findings. Diff-bug lens returned
nine; F1 (87) — an `NA` name cleared the unnamed-objects guard, `nzchar(NA_character_)` being
`TRUE` — was fixed on the branch. Eight scored 15–65 are logged; F2–F7 graduated to a candidate row.
The validity-predicate lesson family was extended rather than a new `LESSONS.md` line added.
