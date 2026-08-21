# M100: Align the review gate's master watch with the alert's verdict set

**Status:** done (2026-08-21, PR #129 https://github.com/jmgirard/circumplex/pull/129)

**Goal:** Make `cairn/PROFILE.md`'s consistency-gate master-watch classify a push run's conclusion
the same way `.github/workflows/master-red-alert.yaml` does, so a run concluding `timed_out` or
`startup_failure` cannot open a "master is red" issue and be walked past by the next review gate.

**Outcome:** the master-watch bullet states one three-way partition — no conclusion yet is
skipped (as the alert's `types: [completed]` does); a conclusion in the alert's job `if:` benign
list minus `success` is no verdict; every other conclusion is a verdict, only `success` green,
the rest red by exclusion. That list is carried BY REFERENCE, so `PROFILE.md` holds no second
copy of the conclusion vocabulary. `R-CMD-check.yaml` fails the gate on red or NO RUN AT ALL
(M93); `test-coverage.yaml` on red, an absent run being no verdict there alone (M95); the M96
cross-check binds every red. `test-coverage.yaml`'s stale third copy became a pointer, and
`master-red-alert.yaml` gained a header comment naming its list's second reader, the direction-independence
of any edit, and `EXPECTED_IF` in `tools/check-master-red-alert.R` as the co-edit sibling. Comment-only.

**Decisions:** none milestone-local; the three plan-gate choices (mirror by exclusion, pointer over restatement, no checker) are in the work log.

**Review:** two passes. Pass 1 returned the milestone (defect return 1) on F1 — the rule made a
still-running run a gate failure — plus F2's stale third copy and F3's overstated sweep; nine
findings dispositioned. Pass 2: [S] prior-PR-comments and [S] blame-history clean, [O] diff-bug
seven, G1–G6 fixed at the gate, G7 closed by this stamp. Retired at hygiene: the M95-family
lesson's two prescriptions, now owned by `PROFILE.md`'s slot.
