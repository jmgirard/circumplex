# M173: Shorten NEWS and fix two pre-release details

**Status:** done (2026-10-28, PR #207 https://github.com/jmgirard/circumplex/pull/207)

**Goal:** Ready the development version for its CRAN release walk: a testthat floor the tests actually need, a correct vignette sentence on the analytic caution, and a development NEWS section cut to about 150 lines that loses no change a v2.0.1 user would notice.

**Outcome:** NEWS.md's development section went from 506 lines in 51 bullets to 170 lines in 21, Breaking changes first with every refusal and affected function kept, detail left to the help page or vignette each bullet names; released sections unchanged. DESCRIPTION's testthat floor in Suggests rose from 3.0.0 to 3.1.7 (D-073); the suite passed under testthat 3.1.7 (a scratch build: `with_mock()`'s C entry stubbed, a libc++ define for vendored Catch, since 3.1.7 cannot build on R 4.6) and 3.3.2 with the same 1353 tests and one skip. "Evaluating Circumplex Structure" (`.Rmd.orig` and `.Rmd`) now says the analytic caution with a boundary marker prints for every N below 50000.

**Decisions:** D-073 (testthat floor 3.1.7). Milestone-local: the user chose the aggressive NEWS cut at plan; raised the length figure 150 → 170 at a stop after review fixes; at the thrash stop (three AC3 returns) chose to drop the 77-row change-ledger criterion, fix all known findings and merge.

**Review:** four passes, four-lens fan-out on passes 2 and 3, a claim audit at implement (85 claims, 5 corrected). Returns: three defect returns, all on the ledger criterion (compression slips: dropped qualifiers, a wrong vignette attribution, a false reference-scale claim, a false ledger correction); two substantive amendments. Findings across passes 2-3: 30 fixed, 1 follow-up, 15 rejected. Graduated: "Raise the testthat floor to 3.1.7" (removed); "Vignette prose follow-ons" narrowed to (i)-(iii). Filed: "Check NEWS against v2.0.1 before release" (split from M173), "Tie documented caution limits to code" (parked). Lessons: two captured (old testthat on new R; NEWS compression), M120 and M140 evicted by rank, the M7-family lesson marked used.
