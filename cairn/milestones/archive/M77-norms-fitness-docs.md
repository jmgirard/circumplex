# M77: Say precisely what the shipped reference statistics are

- **Status:** done (2026-08-08, PR #105)

## Goal
Stop the docs implying a population referent the reference samples do not have.

## Outcome
The using-instruments vignette characterizes the shipped reference samples from the instrument objects themselves — a visible chunk computes 24 samples across 15 instruments, 11 college/undergraduate, 7 under 300, 6 national standardization, 2 with no identified source — and resolves sample choice on which group the reader's participants resemble rather than on size.
A false claim that some instruments offer nationality-matched samples is gone; the matched sets are gender (igicr, iip32, iip64) and age (cais).
`?norms` adds that `Population` names the group a sample was drawn from, not a frame it was drawn to represent, and warns that the unsourced tables are unverified — the residual-hazard closure D-041 assigned here.
Docs only; no runtime surface. NEWS entry under Documentation.

## Decisions
Rewritten passages say "reference samples", never "comparison sample", which connotes a study-internal control group (RR16 R4/B3).
M74's choice-vs-sampling-error figures stayed qualitative: the vignette ships and `norms-audit.md` does not, so printing them would leave them uncitable by the reader.

## Review
Two rounds. Round 1 returned the milestone — the new prose claimed every non-standardization sample had an identified authoring study, false for iis32 and ipipipc and contradicting their own help pages (92). Round 2 caught that the fix's grammar exempted those two from the caution rather than strengthening it (82), fixed in review.
29 sub-threshold findings logged. All five criteria verified with fresh evidence; `check()` 0/0/0 with the vignette rebuilt.
