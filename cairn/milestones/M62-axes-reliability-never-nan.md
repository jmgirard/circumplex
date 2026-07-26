# M62: Close `axes_reliability()`'s two never-NaN gaps — ξ1 ≥ 1 and an unvalidated `sd`

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m62-axes-reliability-never-nan`

## Goal

Make `axes_reliability()` incapable of reporting a NaN, negative, or infinite
standard error of measurement, by closing the two paths that still can.

## Scope

**In:**
- Fold `xi1 >= 1` into the boundary test at `R/axes_reliability.R:843`, so a
  fit whose axes variance implies a reliability at or above 1 gets the shipped
  boundary treatment (NA reliability and SEm, the boundary warning, the
  `boundary` flag) instead of `sqrt()` of a negative (RR11 Beyond 2).
- Extract that boundary expression into a named helper so the condition is
  directly unit-testable — the state is not reachable through the public API
  (see the work log's reachability probe), so a seam is what gives the guard
  a test with teeth.
- Validate the numeric `sd` argument at `R/axes_reliability.R:886`: refuse
  non-finite (`NA`, `NaN`, `±Inf`) and non-positive values, which today are
  accepted and propagate into the reported SEm.
- Roxygen: `@param sd` states the refusal; the `@details` boundary sentence
  (`:473-475`) names the new condition. Regenerate `man/` via `document()`.

**Out:**
- The `sd = "raw"` branch's computed axis SD (`:881-884`). It is guarded
  upstream — zero-variance items and a non-positive-definite item matrix are
  both already refused — so it has no reachable path to a non-finite or
  non-positive SD. If one is ever found it folds into the milestone that finds
  it, per the ROADMAP's continuous-refactors row.
- The other two items parked on that same candidate row: the
  `cpm_analytic_se` Hessian recomputation and the D-003 pole-snap cosmetic.
  They stay on the row, untouched.
- A `NEWS.md` bullet. `axes_reliability()` is new in 2.0.0 and has never
  shipped, so this documents no change a user can observe across versions;
  it folds into the existing feature bullet if it needs saying at all
  (the M7 T1 precedent for fixes to code that never shipped).
- A `DECISIONS.md` entry. D-001 bars new features; this hardens guards on an
  unreleased function, so nothing is superseded and D-030/D-031's narrow
  supersessions are not extended. Reasoning recorded here instead (plan gate,
  2026-07-26).

## Acceptance criteria

- [ ] AC1 — A fit with `xi1 >= 1` is flagged `boundary`, returns `NA`
      reliability and SEm, and raises the boundary warning; no bare
      `NaNs produced` warning escapes `axes_reliability()` on any accepted
      input. The extracted boundary helper returns `TRUE` for `xi1 >= 1` and
      is unchanged for every case the shipped expression already caught
      (`xi1 <= 0`, negative `xi2`/`zeta1`/`eps`, and the ζ1-dropped path where
      `zeta1` is `NULL`).
- [ ] AC2 — Numeric `sd` refuses `NA`, `NaN`, `Inf`, `-Inf`, `-1`, `0`, and a
      length-2 vector with one bad element, each with an error naming `sd`;
      `"std"`, `"raw"`, a positive scalar, and a positive length-2 vector all
      still return the same values they do today.
- [ ] AC3 — Both guards verified by mutation, not by eye: deleting the
      `xi1 >= 1` disjunct and, separately, deleting the `sd` validation each
      redden at least one named test. Recorded in the work log with the test
      names (LESSONS.md, "a guard has teeth only if you break the guarded line
      and see it fail").
- [ ] AC4 — `document()` leaves no uncommitted diff, and
      `devtools::check(manual = TRUE)` is 0 errors / 0 warnings / 0 notes with
      `checking PDF version of manual ... OK` verified present in the log by
      name — `devtools::check()` defaults to `manual = FALSE` (LESSONS.md, M7
      as sharpened by M57).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T1, T2, T4
- AC4 → T3, T4

## Tasks

- [x] **T1** — Test first: assert the extracted boundary helper is `TRUE` at
      `xi1 = 1` and `xi1 = 1.001` and unchanged across the shipped cases, and
      that `axes_reliability()` emits no `NaNs produced` warning. Then extract
      the boundary expression from `R/axes_reliability.R:843` into a helper,
      add the `xi1 >= 1` disjunct, and extend the warning text at `:845-849` to
      name the new condition. The `>=` is deliberate and symmetric with the
      shipped `xi1 <= 0`: at exactly 1 the Spearman-Brown reliability is
      exactly 1 and SEm exactly 0, which requires zero item-error variance —
      a degenerate solution, not a usable one.
- [x] **T2** — Test first: each refused `sd` value from AC2 errors, each
      accepted one is unchanged. Then add the validation at
      `R/axes_reliability.R:886`. This is the fourth recurrence of the
      M32/M35 `!is.finite()` lesson, so guard with `is.finite()`, never
      `is.na()`.
- [x] **T3** — Roxygen for `@param sd` and the `@details` boundary sentence;
      `document()`; confirm `man/` regenerated, never hand-edited.
- [ ] **T4** — Run the mutation checks for AC3, then the profile's verify slot
      and the full `check(manual = TRUE)`, grepping the log for the PDF-manual
      line by name rather than trusting `Status: OK`.

## Work log

- 2026-07-26: created by /milestone-plan. Promoted from the ROADMAP's continuous-refactors candidate row, which absorbed RR11 Beyond 2 on 2026-07-26.
- 2026-07-26: plan-gate investigation, three probes run in R against the dev tree. (1) `sd` accepts `-1`/`Inf`/`NA`/`NaN` and reports SEm `-0.4764406`/`Inf`/`NA`/`NaN` — a second never-NaN gap the candidate row did not name, folded in at Jeff's gate choice. (2) ξ1 > 1 looks unreachable through the public API: an engineered cormat implying ξ1 = 1.15, ξ2 = .20 has min eigenvalue exactly −0.35 (= its implied negative eps) and is refused by the existing positive-definite gate — evidence, not proof, since the finite-sample fit is approximate, so the guard is still owed and its test needs a seam. (3) The NaN threshold is exactly ξ1 > 1 (rel 0.999937 → 1.000000 → 1.000062, SEm 0.0079 → 0 → NaN), and R's bare `NaNs produced` warning escapes today because `suppressWarnings()` wraps only the fit.
- 2026-07-26: plan gate — four decisions (Jeff). Both gaps in one milestone; ξ1 ≥ 1 folded into `boundary` rather than made a hard error or silently floored; `sd` refuses non-finite AND non-positive; no D-entry, since D-001 bars new features and this hardens an unreleased function (D-030/D-031 not extended).
- 2026-07-26: started (/milestone-implement). Branch `m62-axes-reliability-never-nan` cut from master at 57f5c009; no dependencies to verify. Status planned→in-progress.
- 2026-07-26: T1 done. Boundary expression extracted to `axes_is_boundary(xi1, xi2, zeta1, eps)` with the `xi1 >= 1` disjunct added; the caller now reads the seam and its warning names "an axes variance outside (0, 1)". `zeta1`'s NULL-ness replaces the separate `fit_zeta1` flag — one source of truth for whether the component was fitted, and behaviorally identical on both paths (enumerated in the test rather than assumed, per the M60 lesson on generalizing a gate). Three tests added: the predicate across the new and every shipped case incl. both ζ1-dropped branches; a swept property test that no admitted ξ1 yields a non-finite or non-positive SEm across item_n ∈ {2, 2.5, 26/3, 16, 32}; and an end-to-end NA-not-NaN test via `local_mocked_bindings(axes_is_boundary=)`, the seam pattern `axes_converged` already uses. **AC3 first half: mutation-verified** — deleting the `xi1 >= 1` disjunct reddens `test-axes-reliability.R:463` and `:464`; code restored and re-run green. Two defects in my own tests caught before the fix landed, both in the wrapper rather than the claim: a `...`-forwarding helper collided on `xi1` ("matched by multiple actual arguments"), and the sweep called the predicate on a length-7 vector, which `||` rejects in R >= 4.3. `devtools::test()`: 0 failures, 3910 passing, 0 skipped; the 4 warnings are the pre-existing test-ci_accuracy.R diagnostics.
- 2026-07-26: T2 done. Numeric `sd` now refuses non-finite and non-positive values with a message naming the received value; `"std"`, `"raw"`, a positive scalar and a positive length-2 vector are untouched and asserted to return exactly what they returned before. Guarded with `is.finite()`, never `is.na()` — the fourth recurrence of the M32/M35 lesson, and the two `Inf` cases are what pin that choice. Plain logical `NA` was already refused by the existing `stopifnot(is.numeric(sd))`, so the new guard covers `NA_real_`/`NaN`. **AC3 second half: mutation-verified** — deleting the guard block reddens 8 assertions in `test-axes-reliability.R`; restored and re-run green. `devtools::test()`: 0 failures, 3922 passing, 0 skipped, same 4 pre-existing warnings.
- 2026-07-26: T3 done. `@param sd` states the finite-and-positive requirement; the `@details` boundary sentence now names both sides of the axes-variance bracket and says why each is unusable, in the vignette-prose register rather than by naming ξ1. `document()` regenerated `man/axes_reliability.Rd` only, never hand-edited. Its three `cpm_gradient` link warnings were checked against a stashed clean tree and occur there identically — pre-existing, not introduced here. `test-rd-latex-safe.R` passes on the regenerated Rd (the guard M7 repaired), so the new prose introduces no LaTeX-hostile character. `devtools::test()`: 0 failures, 3922 passing, 0 skipped.

## Decisions

## Review
