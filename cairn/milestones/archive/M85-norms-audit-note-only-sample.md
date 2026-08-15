# M85: Carry the sample through the audit's note-only coverage rows

**Status:** done (2026-08-14, PR #113 https://github.com/jmgirard/circumplex/pull/113)

**Goal:** A note-only coverage row carries the sample its dedupe key already
distinguishes it by, so two such rows stop emitting identically.

**Outcome:** The `note-only-sample` emitter in `data-raw/audit-norms.R` passes
`note_only$sample[fresh]` instead of inheriting `coverage_rows()`'s `NA`
default, and a new `blank_to_na()` beside `tag_or_na()` spells a blank note cell
`NA` rather than `""` in a key column. The four-axis note-only test gains a
`sample`-cell assertion and a per-axis `distinct` count — 1 on the `anchor` axis,
where the key runs one cell wider than the report by design. Coverage CSV
regenerated: 14 cells, `NA` → `NO_SAMPLE`; gap count 0 before and after.

**Decisions:** M85-D1 — the regenerated `norms-audit-ledger.csv` is not
committed; only its three stamp cells move, and committing them would date a
provenance re-verification this milestone did not perform.

**Review:** Three lenses, 11 findings, 1 actioned — F1 (82), a blank sample cell
reaching the report as `""`, introduced here and fixed on the branch. 10 logged,
highest F4 (48) and F2 (45); M80's own review had logged this as its F1 (72).
