# M23: Longitudinal & intraindividual SSM — Fable-reviewed design + build-ready spec (done)

- **Status:** done · **PR:** #47 (merged 2026-07-16) · design-gate milestone, docs/design only (no package code; mirrors M17).
- **Goal:** turn Brief E's longitudinal directions (Q6.1–Q6.3, M4 deps discharged) into one Fable-reviewed, build-ready spec so build milestones implement without re-opening design.

**Outcome:** `devel/longitudinal-ssm-spec.md` pinned as the binding build contract (**D-013**). Independent Fable review RB06→RR06 (archived): verdict "needs change (targeted)", architecture confirmed; 12 recommendations applied (§9 revision log), 3 rejections accepted. Load-bearing holdings:
- Wide person-rows make the paired occasion analysis ride the existing row resampler (case bootstrap = `boot::boot` over rows); stacked-occasions MC covariance (sample cov of stacked person vectors / n) verified correct.
- **Paired-efficiency claim is conditional** — exact for Δe iff within-person elevation ρ > 0; for Δa/Δd narrower iff ∇g₂ᵀC∇g₁ > 0 (∝ cos Δd isotropic), **reversing for |Δd| > 90°**; unconditional claim may never be printed (the draft's version was false — RR06's headline catch).
- Growth recipes must fit (x, y) **jointly** (independent LMMs zero Cov(x̂,ŷ) → wrong d(t) intervals); per-t D-007 amplitude certification caution; low-amplitude cell is the real coverage danger, pole-crossing the boundary headline.
- One `ssm_draws()` adapter serves Bayesian + growth pipelines (explicit type required in the ncol=3/no-angles cell); cross-occasion stem-alignment validation closes the rotation channel; listwise-only occasions (estimand grounds); `ssm_ci_accuracy()` errors on occasions objects.

**Plan-gate decisions:** unified spec; Bayesian = draws adapter + brms vignette only; builds NOT merge-gated behind M7 (**D-012**, supersedes D-001's exclusion insofar as it gates merges); Fable review via RB06.

**Review:** all 4 ACs fresh-evidenced; gate green (`cairn_validate`, `document()` no-diff, pkgdown, `check()` 0 errors / 0 warnings / 0 notes; CI 7/7). 3-lens review → scorer: F1 (85) stale `m6-*` artifact prefix fixed pre-merge; F2 (25) logged → Build A's gate (CLAUDE.md occasion-order clause).

**Carries:** build candidates per spec §7 — Build A (occasions core + output surface), Build B (per-person layer + adapter + vignette), Build C (growth support, dep B); pairwise semantics, `ssm_ci_accuracy()` occasions extension, `ssm_analyze_long()` sugar, Stan companion (§5.4 criteria) all deferred in the ROADMAP row. DESIGN.md oracle-registry pointer line owed by the first build adding an oracle.
