# M57: ΔCFI secondary invariance criterion for `ssm_sem()`

**Status:** done (2026-07-25, PR #83 https://github.com/jmgirard/circumplex/pull/83)

**Goal:** Add the Cheung–Rensvold (2002) ΔCFI as a labeled, reported-only
secondary criterion in `ssm_sem()`'s ladder, scope-gated; Δχ² stays sole gate.

**Outcome:** `sem_fit_ladder()` gains `dcfi` (a rung's CFI minus the previous
*fitted* rung's; NA for configural and the strict tier's vacuous metric rung),
a `cr` retain/reject column via `sem_dcfi_cutoff`/`sem_dcfi_flag`, and a
`dcfi_scope` record carried into `invariance`. The label prints only for two
groups AND ML estimation AND a plain non-robust CFI, `sem_dcfi_note()` naming
which condition withholds it; `print()` shows `dcfi` at `max(digits, 4)`.
Source note `cheung2002.md` authored, values read from the PDF.

**Decisions:** D-027 (reported-only; direction from Table 5's 1% lower tails,
not the article's contradicted p. 251 sentence), D-028 (the ML conjunction,
narrowing D-027 part 3). Milestone-local: print surface; `cr` stored in table.

**Review:** Two passes; the first FAILED on AC2 (F1, 85: the gate keyed on a
plain-CFI proxy that GLS/WLS/ULS/DWLS defeat). Actioned F1, F2 85, F5 80, F8
80, an anchor fix; logged F3 75 (folded in), F6 70, F7 52, F4 40 (rejected).
