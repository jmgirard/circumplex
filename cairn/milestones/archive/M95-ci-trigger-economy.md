# M95: Stop running the suite twice on every pull request

**Status:** done (2026-08-18, PR #124 https://github.com/jmgirard/circumplex/pull/124)

**Goal:** Retire the dead `pr-commands.yaml` workflow and take `test-coverage.yaml` off the pull-request trigger, moving its master-side result under the review gate's watch.

**Outcome:** `pr-commands.yaml` deleted — its `/document` and `/style` triggers
never matched (9 runs since 2025-07-21, all `skipped`) and its
`install_deps(dependencies = TRUE)` would have installed the brms/Stan stack
D-015 keeps out of CI. `test-coverage.yaml` drops `pull_request`, its `push`
block byte-unchanged (51dce082…) and the dead `fail_ci_if_error` conditional
collapsed to `true`: ~27 min less runner time per PR but no wall clock, the two
jobs having run in parallel (runs 32184165512 / 32184165497). No PR reporting is
lost — `codecov.yml` has set `comment: false` with informational statuses since
before cairn. `PROFILE.md`'s consistency-gate gains a master coverage watch
reading the newest run to conclude success OR failure, since `cancel-in-progress`
makes `cancelled` a completed non-verdict (run 32187677266); it treats an absent
run as no verdict, names `/hotfix` as the non-deadlocking way to clear a red one,
and records that it reads one milestone late. Its test-doctrine slot now separates
the coverage NUMBER (never a gate) from the JOB's pass/fail (which gates), and drops
two stale claims: that CI is the stock usethis pair, and that Codecov annotates the PR.

**Decisions:** none milestone-local; no D-entry owed — the gate model's record is the PROFILE.md slot, D-015/D-029 untouched.

**Review:** three lenses; blame-history and prior-PR-comments no-op (the latter clean on both surfaces, per M33). [O] diff-bug returned 14 ranked: ten actioned, three rejected (criterion/deliverable conflation, a false M96 premise, one self-caught), one already handled; four verified against artifacts first. AC5 took an amendment return — it claimed its three greps were "the complete set of files carrying a claim", which `.github/CONTRIBUTING.md:25` falsifies — narrowed per the widening test, its new wording audited by a fresh reader that caught a false clause in the proposed fix. F13 rescoped (nothing stranded) and F14 (plaintext Codecov token, pre-existing since M12) became candidate rows. One lesson captured, one family extended; none retired.
