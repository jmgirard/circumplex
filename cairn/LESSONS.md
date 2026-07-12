# Lessons

Durable repo lessons — build quirks, testing tricks, gotchas worth
remembering next time — captured at milestone end and surfaced at plan time.
Not status, not decisions.

Append-only; one line per lesson: `- YYYY-MM-DD (M<NN>): <lesson>`. Capped at
50 lines — when full, prune the stalest lessons; git history keeps the record.

<!-- lessons appended below by /milestone-review post-merge hygiene -->

- 2026-07-12 (migration): pre-cairn build/CI lessons live in the entombed `cairn/legacy/` and in `cairn/DESIGN.md` (e.g. Linux-only netlib-BLAS reproduction via a `rocker/r-ver` container for the `cpm_pack` β=0 boundary; `skip_on_ci()` for BLAS-sensitive snapshot/vdiffr tests). Not re-transcribed here — cited from legacy.
- 2026-07-12 (M8): behaviour-preserving SEM refactors are fenced by two byte-pinned guardrails — the `exp_strict_*` syntax snapshots (`test-ssm_sem_syntax.R`) and the `summary()` label strings (tabs/casing) — so DRY extraction there is safe to verify by "existing suite stays green". The single/multi-group emitters in `ssm_sem_syntax.R` are NOT a shared extraction (plain vs `c()`-vector cross-group labels); don't try to unify them.
- 2026-07-12 (M9): vectorizing `sem_estimate()`'s per-draw transform loop needed NO re-pins — keep the tested scalar `sem_ssm_transform()` as the reference and pin the matrix pass to it row-by-row (incl. the §5.5 flat/zero-amplitude NA semantics); `rowMeans`/matmul FP reorder stays within existing tolerances. `sem_pop()`-composing `make_pop_2g()` was bit-identical (same op sequence), so re-recording the coverage rds is a deterministic no-op — verify by `identical()` instead of an expensive stochastic rerun.
