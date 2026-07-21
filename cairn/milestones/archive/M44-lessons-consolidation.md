# M44: LESSONS.md consolidation and retirement pass (dropped)

**Goal:** bring `cairn/LESSONS.md` back under both weight axes with durable
headroom, without losing a live lesson.

**Dropped 2026-07-20 (Jeff's request).** Not because the premise was wrong —
`LESSONS.md` really is at the `weight caps` edge, 49/50 lines — but because Jeff
chose to defer the compression rather than spend a dedicated milestone on it now.
The file passes both live checks today (`weight caps` PASS, no `record density`
WARN). When the next milestone captures a lesson it will hit 50 lines and FAIL
`weight caps`; the consolidation/retirement then happens at that milestone's
post-merge hygiene, scoped to what it ships (D-051/D-015), instead of the
pre-emptive standalone pass M44 planned. (The whole-file char cap M44 also
targeted, `<17,000`/`20,500` chars, was retired plugin-side by D-058 regardless.)

**Deliberately abandoned:** the pre-planned family consolidations, the `:33`
split, and the standalone D-051 retirement/mutation audit — all deferred to
forced-time, not lost.

**Folded into the drop commit:** LESSONS.md's header (`:7-8`) cited the retired
`<17,000 chars` cap — a false statement in a current-knowledge file — corrected
to point at `cairn_validate`'s two checks (M44's old AC4).

No branch, no PR, no code touched.
