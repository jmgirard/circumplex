# M59: Correlation-matrix input path for `axes_reliability()`

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M54
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m59-axes-reliability-corr-input`

## Goal

Let `axes_reliability()` estimate from a published item correlation matrix plus
its sample size, so a reanalysis needs no raw data.

## Scope

**In:** a secondary input path on the existing exported function —
`cormat` + `n` take an item correlation matrix instead of raw items, fit
through `lavaan`'s `sample.cov`/`sample.nobs` surface; its own refuse contract;
Nunnally–Bernstein reported `NA`-with-reason there (RR09 §7.4); the
oracle battery (raw-path round-trip, population-matrix recovery, cross-engine);
docs, vignette, NEWS. Plus the outstanding RR09 §7.8 blockwise-ζ2 doc note
(M54 review F3, deferred sub-threshold).

**Out:** non-octant types b–f, quasi-circumplex weights, blockwise ζ2
estimation, FIML on items → they stay on the ROADMAP candidate row
"Axes-reliability deferred-in-spec extensions", each still gated on a concrete
use case (D-026). Covariance-matrix (non-unit-diagonal) input → not planned;
the model assumes unit-variance items. Gating the release → out: M7 gains **no**
`Depends on: M59` and never waits for it (M59 plan gate). Amended at the
implement gate: the milestone *does* enter v2.0.0's contents, because master
ships as v2.0.0 and M59 merges first, so D-030 records the narrow D-001
supersession the plan wrongly thought unnecessary.

## Acceptance criteria

- [ ] AC1 (surface). `axes_reliability()` gains `cormat = NULL` and `n = NULL`,
      following `cpm_fit()`'s house pattern (`R/cpm_fit.R:1559`): exactly one of
      `data` or `cormat`; `n` is required with `cormat` and refused with `data`.
      `devtools::document()` produces no diff. *(RB tripwire: irreversible-api)*
- [ ] AC2 (round-trip oracle). On ≥2 datasets, `axes_reliability(cormat =
      cor(x), n = nrow(x), …)` equals `axes_reliability(x, …)` on ξ1/ξ2/ζ1, both
      reliabilities, SEm at `sd = "std"`, `df` and χ² within 1e-6 — with
      lavaan's `(N−1)/N` likelihood rescaling explicitly handled, not absorbed
      into tolerance (RR09 BC5's trap).
- [ ] AC3 (two independent oracle types on the corr path). **(a)
      Deterministic population matrix** — the exact `axes_population_cor()`
      matrix fed through the public `cormat` path recovers (ξ1, ξ2, ζ1) within
      1e-4 with χ² < 1e-6. **(b) Cross-engine** — lavaan and OpenMx fits of the
      identical model on the identical correlation matrix agree on every free
      component within 1e-3; that test **skips**, never passes, when OpenMx is
      absent; no new Imports (D-006/D-014). Both halves need their own evidence.
- [ ] AC4 (N–B and SEm). `nb_reliability` is `NA` on the `cormat` path and
      `print`/`summary` state why — RR09 §7.4: "N–B must be `NA`-with-reason
      there, not dropped silently". `sd = "raw"` errors informatively (no raw
      scores exist); `"std"` and numeric `sd` work.
- [ ] AC5 (refuse contract). Each errors informatively, with a regression test:
      `data` and `cormat` both supplied, or neither; `cormat` non-square,
      asymmetric, non-unit-diagonal, non-finite, or non-PD; `cormat` dimnames
      absent or mismatched with `items`; `n` absent with `cormat`, supplied
      with `data`, non-numeric, non-finite, or ≤ number of items.
- [ ] AC6 (docs). Roxygen and `vignettes/axes-reliability.Rmd` document the
      corr path (including the Cudeck SE approximation already stated for the
      raw path) and carry the RR09 §7.8 note that a blockwise-administered
      instrument analyzed without ζ2 folds block variance into the general and
      scale components; NEWS entry added.
- [ ] AC7 (profile verify). `devtools::test()` clean and
      `devtools::check()` OK — with the PDF-manual step confirmed to have
      **run** by grepping the log for `checking PDF version of manual`, since
      this milestone touches roxygen (M7/M57 lesson).

## Coverage

- AC1 → T2, T8
- AC2 → T1, T2, T3
- AC3 → T5 (a), T6 (b)
- AC4 → T4
- AC5 → T2, T7
- AC6 → T8, T9
- AC7 → T9, T10

## Tasks

- [x] T1. Test-first: write the AC2 round-trip test against the not-yet-added
      `cormat`/`n` arguments and watch it fail. Prove each new guard by
      mutation, not by eye, and scope every probe to the surface it claims to
      check (M57).
- [x] T2. Add `cormat`/`n` and the AC5 refuse contract in `axes_reliability()`
      (`R/axes_reliability.R:366`), mirroring `cpm_fit()`'s validation block
      (`R/cpm_fit.R:1583`), reusing the existing PD/eigenvalue guard at `:456`
      and bypassing the listwise block at `:427`.
- [x] T3. Route the fit to `sample.cov`/`sample.nobs`. `sem_fit_cfa()`
      (`R/ssm_sem.R:745`) is the single `lavaan::cfa` chokepoint and takes
      `data`; extend it or add a sibling seam without disturbing its SEM
      callers. `axes_ols_shadow()` (`:137`) already takes `R` directly.
- [x] T4. N–B → `NA` with a stated reason; refuse `sd = "raw"`; update
      `print`/`summary`. Grep every consumer of `nb_reliability` and the
      `details` list before changing their contract (M18).
- [x] T5. AC3(a) population-matrix oracle through the public `cormat` path.
- [x] T6. AC3(b) cross-engine OpenMx oracle on the correlation matrix,
      patterned on M54's existing BC7 test.
- [x] T7. AC5 refuse-contract regression tests.
- [x] T8. Roxygen: `@param cormat`, `@param n`, the corr-path `@details`
      paragraph, the blockwise-ζ2 note; `devtools::document()`.
- [x] T9. Vignette section + NEWS. Check the tail bytes of any wholesale-written
      file (M34) and confirm no echoed chunk depends on a hidden one (M50).
- [ ] T10. Full `devtools::test()` + `devtools::check()`; verify the PDF-manual
      step actually ran (AC7); fix fallout.

## Work log

- 2026-07-25: created by /milestone-plan.
- 2026-07-25: start — status in-progress, branch `m59-axes-reliability-corr-input` cut from master.
- 2026-07-25: T1 — AC2 round-trip test written first; fails with `unused arguments (cormat, n)`, the intended pre-implementation failure.
- 2026-07-25: T9 — vignette section 4 ("Starting from a published correlation matrix") + a fourth caveat carrying the RR09 §7.8 blockwise note; NEWS folded into the existing unreleased `axes_reliability()` bullet. Vignette knits; no echoed chunk depends on a hidden one (M50 check clear).
- 2026-07-25: amended Scope at the implement gate — the plan's "v2.0.0 not entered" was wrong on mechanics (master ships as v2.0.0; `DESCRIPTION` already reads 2.0.0), so D-030 records the narrow D-001 supersession. M7 still gains no dependency. Jeff's call at the gate.
- 2026-07-25: T8 — roxygen: `@param cormat`/`@param n`, a "Supplying a correlation matrix" section, a "Blockwise instruments" section discharging RR09 §7.8 (M54 F3), and a cormat example. `document()` idempotent; only `man/axes_reliability.Rd` changed. The `cpm_gradient` link warning is pre-existing (present on the unmodified tree).
- 2026-07-25: T5–T7 — AC3(a) population oracle pins the convention exactly (recovered = truth × (n−1)/n to 1e-8 relative at n = 500/5e3/5e4, and a permuted cormat gives an identical answer); AC3(b) cross-engine OpenMx agrees to ~2e-5 against a 1e-3 bar; AC4/AC5 regressions. Both novel guards proven by mutation: dropping `is.finite(n)` and dropping the cormat reordering each turn the suite red. Suite FAIL 0 / PASS 3309.
- 2026-07-25: T2–T4 — `cormat`/`n` path, refuse contract, `axes_fit_cormat()` seam, N–B `NA`-with-reason, `sd="raw"` refusal, print/summary. Round-trip agrees to 1e-15 (not merely inside the 1e-6 bar); the wishart/normal ratio measured exactly 499/500, confirming the `(N−1)/N` mechanism is matched rather than tolerated. Suite FAIL 0 / PASS 3263 (baseline 3247 + the 16 new).
- 2026-07-25: amended AC1/AC2/AC3a/AC4/AC5 + Scope + T1/T2/T8 at the implement gate — the planned `nobs`-switches-`data` surface is replaced by `cormat` + `n`, matching `cpm_fit()`'s existing correlation-matrix path (`R/cpm_fit.R:1559`), a house precedent the plan's collision sweep missed. Jeff's call at the gate.

## Decisions

## Review
