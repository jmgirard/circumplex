# M162: Small-sample draws in ssm_trajectory() (dropped)

**Status:** dropped (2026-10-19, at the user's choice at the M163 plan gate)

**Goal:** `ssm_trajectory()` draws by the small-sample rule that M161 selected when the user asks for it, and its help page, the growth vignette and NEWS state the rule's validated range.

feature series: M161 -> M162.

**Why dropped:** M161 selected no rule (D-066), so the goal could not be met as planned. The user chose to drop the milestone over a replan with a fresh pre-registered oracle run. Among converged fits, the shipped intervals cover .938 to .944 at 20 to 50 persons, against .95 (`devel/m161-small-n-oracle.md`). A t rule can only be an opt-in argument, because the helper cannot see the number of persons. At 10 persons the problem is boundary fits, which no draw rule repairs.

**Where the work went:** M163 states the measured small-sample coverage in the growth vignette's Section 7, the `ssm_trajectory()` help page and the NEWS entry. The t rule (R1, nu = N - 1) is the parked candidate row "Small-sample correction for trajectory draws", reopened only by D-066's condition. The RR25 recs routed here (6, 7, 9) go with that row, except rec 7's boundary-fit rate, which M163 states.

No branch, no PR, no code touched. The plan-time work log is in git history (`git log -- cairn/milestones/M162-trajectory-small-sample-draws.md`).
