# Lessons

Durable repo lessons — build quirks, testing tricks, gotchas worth
remembering next time — captured at milestone end and surfaced at plan time.
Not status, not decisions.

Append-only; one line per lesson: `- YYYY-MM-DD (M<NN>): <lesson>`. Capped at
50 lines — when full, prune the stalest lessons; git history keeps the record.

<!-- lessons appended below by /milestone-review post-merge hygiene -->

- 2026-07-12 (migration): pre-cairn build/CI lessons live in the entombed `cairn/legacy/` and in `cairn/DESIGN.md` (e.g. Linux-only netlib-BLAS reproduction via a `rocker/r-ver` container for the `cpm_pack` β=0 boundary; `skip_on_ci()` for BLAS-sensitive snapshot/vdiffr tests). Not re-transcribed here — cited from legacy.
