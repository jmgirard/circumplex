# M59: Correlation-matrix input path for `axes_reliability()`

- **Status:** planned
- **Priority:** normal
- **Depends on:** M54
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Let `axes_reliability()` estimate from a published item correlation matrix plus
its sample size, so a reanalysis needs no raw data.

## Scope

**In:** a secondary input path on the existing exported function — `nobs`
supplied switches `data` from raw items to an item correlation matrix, fit
through `lavaan`'s `sample.cov`/`sample.nobs` surface; its own refuse contract;
Nunnally–Bernstein reported `NA`-with-reason there (RR09 §7.4); the
oracle battery (raw-path round-trip, population-matrix recovery, cross-engine);
docs, vignette, NEWS. Plus the outstanding RR09 §7.8 blockwise-ζ2 doc note
(M54 review F3, deferred sub-threshold).

**Out:** non-octant types b–f, quasi-circumplex weights, blockwise ζ2
estimation, FIML on items → they stay on the ROADMAP candidate row
"Axes-reliability deferred-in-spec extensions", each still gated on a concrete
use case (D-026). Covariance-matrix (non-unit-diagonal) input → not planned;
the model assumes unit-variance items. v2.0.0 scope → **not** entered: M7 gains
no dependency and D-001 is not superseded (M59 plan gate).

## Acceptance criteria

- [ ] AC1 (surface). `axes_reliability()` gains `nobs = NULL`; a non-`NULL`
      `nobs` treats `data` as an item correlation matrix and every other
      argument keeps its raw-path meaning. `devtools::document()` produces no
      diff. *(RB tripwire: irreversible-api)*
- [ ] AC2 (round-trip oracle). On ≥2 datasets, `axes_reliability(cor(x), …,
      nobs = nrow(x))` equals `axes_reliability(x, …)` on ξ1/ξ2/ζ1, both
      reliabilities, SEm at `sd = "std"`, `df` and χ² within 1e-6 — with
      lavaan's `(N−1)/N` likelihood rescaling explicitly handled, not absorbed
      into tolerance (RR09 BC5's trap).
- [ ] AC3 (two independent oracle types on the corr path). **(a)
      Deterministic population matrix** — the exact `axes_population_cor()`
      matrix fed through the public `nobs` path recovers (ξ1, ξ2, ζ1) within
      1e-4 with χ² < 1e-6. **(b) Cross-engine** — lavaan and OpenMx fits of the
      identical model on the identical correlation matrix agree on every free
      component within 1e-3; that test **skips**, never passes, when OpenMx is
      absent; no new Imports (D-006/D-014). Both halves need their own evidence.
- [ ] AC4 (N–B and SEm). `nb_reliability` is `NA` on the corr path and
      `print`/`summary` state why — RR09 §7.4: "N–B must be `NA`-with-reason
      there, not dropped silently". `sd = "raw"` errors informatively (no raw
      scores exist); `"std"` and numeric `sd` work.
- [ ] AC5 (refuse contract). Each errors informatively, with a regression test:
      non-square, asymmetric, non-unit-diagonal, or non-finite `data`; absent
      or `items`-mismatched dimnames; non-PD matrix; `nobs` non-numeric,
      non-finite, or ≤ number of items.
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

- [ ] T1. Test-first: write the AC2 round-trip test against the not-yet-added
      `nobs` argument and watch it fail. Prove each new guard by mutation, not
      by eye, and scope every probe to the surface it claims to check (M57).
- [ ] T2. Add `nobs` + correlation-matrix detection and the AC6 refuse contract
      in `axes_reliability()` (`R/axes_reliability.R:366`), reusing the existing
      PD/eigenvalue guard at `:456` and bypassing the listwise block at `:427`.
- [ ] T3. Route the fit to `sample.cov`/`sample.nobs`. `sem_fit_cfa()`
      (`R/ssm_sem.R:745`) is the single `lavaan::cfa` chokepoint and takes
      `data`; extend it or add a sibling seam without disturbing its SEM
      callers. `axes_ols_shadow()` (`:137`) already takes `R` directly.
- [ ] T4. N–B → `NA` with a stated reason; refuse `sd = "raw"`; update
      `print`/`summary`. Grep every consumer of `nb_reliability` and the
      `details` list before changing their contract (M18).
- [ ] T5. AC3 population-matrix oracle through the public path.
- [ ] T6. AC4 cross-engine OpenMx oracle on the correlation matrix, patterned
      on M54's existing BC7 test.
- [ ] T7. AC6 refuse-contract regression tests.
- [ ] T8. Roxygen: `@param nobs`, the corr-path `@details` paragraph, the
      blockwise-ζ2 note; `devtools::document()`.
- [ ] T9. Vignette section + NEWS. Check the tail bytes of any wholesale-written
      file (M34) and confirm no echoed chunk depends on a hidden one (M50).
- [ ] T10. Full `devtools::test()` + `devtools::check()`; verify the PDF-manual
      step actually ran (AC7); fix fallout.

## Work log

- 2026-07-25: created by /milestone-plan.

## Decisions

## Review
