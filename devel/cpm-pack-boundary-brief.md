# Fable brief — `cpm_pack` β-boundary error on Linux (CI portability)

**Status:** handoff brief for a Fable session (estimator/canonicalization work;
plausible-but-wrong statistics are possible here). **Tier: Fable.** Surfaced
2026-07-07 by the M4.5 PR #28 CI; recorded as a v2.0.0 release blocker in
ROADMAP (`## CRAN release strategy` → v2.0.0 pre-release items, class 1).

## Symptom

**Linux only.** After the class-2/3 skips landed (PR #29), macOS and
**Windows R-CMD-check pass**; the error persists on **ubuntu (release, oldrel-1,
devel) and the covr `test-coverage` job**. So this is specific to the ubuntu
runners' BLAS/LAPACK, which narrows the reproduction target: reproduce under a
`rocker/r-ver` (Debian/OpenBLAS) container, not just "a non-mac platform."
`R-CMD-check` on those Linux jobs fails with:

```
Error ('test-cpm_fit.R:195'): exact recovery: an angle exactly at the 0/360 pole
Error ('test-cpm_fit.R:313'): mirror starts converge to equal F and identical canonical output
Error ('test-cpm_fit.R:418'): a clean in-family fit is not flagged multimodal
Error ('test-cpm_fit.R:654'): free-angle acceptance still holds when a jitter confirms g0
  Error in cpm_pack(theta_theory, sv$zeta, sv$beta, spec):
    all(b_keep > 0) is not TRUE
```

**macOS `R-CMD-check` passes** — the failure does **not reproduce on the dev
machine**. It is present on master too (master's CI has been red on all
platforms since M4; M4 landed without a green multi-platform gate), i.e. it is
pre-existing M4 debt, not an M4.5 regression. The other CI failures (bootstrap
snapshots, vdiffr) are already handled by `skip_on_ci()`/`skip_on_cran()` on the
`ci-cross-platform` branch; **this β-boundary error is the remaining blocker.**

## Mechanism (confirmed by reading the code)

`cpm_pack()` (`R/cpm_fit.R:157-174`) maps the harmonic correlation-function
weights β into the optimizer's unconstrained coordinates via a **softmax
inverse** over the kept harmonics (`spec$keep_k = 0:m`, line 144):

```r
b_keep <- beta[spec$keep_k + 1L]
stopifnot(all(b_keep > 0))            # line 170
v <- log(b_keep) - log(b_keep[1])     # v_0 = 0; v_k = log(beta_k) - log(beta_0)
```

The log-parameterization requires **every kept β strictly positive** (a zero has
no finite preimage — the comment at 166-168 says so and fails loudly rather than
emit `-Inf`). On the ubuntu-runner BLAS the estimator lands on (or the test's
start/fitted values `sv$beta` carry) a kept β **at or below the β = 0 boundary**;
on macOS and Windows the same fit stays just positive. A vanishing harmonic
weight is a
*documented CPM boundary* (see `cpm_boundary_markers()` and the "harmonic weight
at 0" language in the CI-trustworthiness vignette), so this is a real boundary
the estimator must survive, not a data error.

Note the engine already has a **harmonic-removal ("polishing") path**
(`polished$removed`, `R/cpm_fit.R:588`, `removed_harmonics` at 689) that zeroes
out top harmonics; `keep_k` is `0:m` unless polishing dropped one. The bug is
that a β reaches ≤ 0 on the kept set **without** being polished out (or a caller
re-packs such a solution), so `cpm_pack` is asked to invert an unrepresentable
point.

## What to figure out (do not pre-commit to a fix)

1. **Capture the offending values on Linux first** (the failure is invisible on
   macOS). Options: a temporary CI step / a `browser()`-free diagnostic that
   prints `b_keep`, `spec$keep_k`, `spec$polished`/`removed`, and the fit path on
   the four failing tests; or reproduce under a `rocker/r-ver` container / Linux
   box with a reference BLAS. Know the actual boundary case before designing.
2. **Locate where the ≤ 0 β enters `cpm_pack`.** Is it (a) a *converged* boundary
   solution being re-packed (then the fix is in the engine/canonicalization —
   detect the vanishing harmonic and reduce `m`/polish it before re-packing,
   consistent with the existing `removed` machinery), or (b) an *intermediate*
   optimizer iterate (then the parameterization/optimizer needs to keep β in the
   interior, e.g. reflect/clamp with an ε that does **not** perturb the reported
   estimate), or (c) a *starting value* the test constructs at the boundary
   (then the fix may be in the test's `sv$beta`, but confirm the estimator is
   genuinely robust, not just the test)?
3. **Is `stopifnot(all(b_keep > 0))` the right invariant?** It is correct *given*
   the softmax parameterization. The question is whether the boundary should be
   handled upstream (polish the zero harmonic → smaller `keep_k`) so `cpm_pack`
   never sees a non-interior point — likely the cleanest fix and consistent with
   the design's harmonic-removal story.

## Constraints (CLAUDE.md / DESIGN.md)

- Statistical correctness outranks everything; **no fix may change point
  estimates on valid interior fits.** Verify byte-identical `cpm_fit()` output
  (seeded engine fits + `jz2017`) before/after, as the M4 review-#1 fix did.
- Run `/statistical-validation` (this touches `ssm_*`/estimation code).
- Boundary suite (0°/360° peaks, ±180° contrasts, flat/zero-variance, and now
  the **β = 0 vanishing-harmonic** boundary explicitly).
- RNG contract: the four failing tests include mirror-start and free-angle
  acceptance — keep the start-group counting and `.Random.seed` behavior intact.
- The seeded regression pins in `test-cpm_fit.R`/`test-ssm_analysis.R` are
  intentional; if any changes, justify it as a deliberate statistical change.

## Acceptance

- The four `test-cpm_fit.R` tests pass on the ubuntu `R-CMD-check` jobs (macOS
  and Windows already pass) and under `covr` in `test-coverage`.
- A regression test exercising the β = 0 (vanishing-harmonic) boundary directly,
  so the case is pinned platform-independently rather than only caught by the
  cross-platform CI matrix.
- Point estimates on interior fits unchanged (parity test).
- Then CI is green: classes 2–3 already skipped on the `ci-cross-platform`
  branch, so this closes the last red.
