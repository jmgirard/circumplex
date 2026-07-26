# M62: Close `axes_reliability()`'s two never-NaN gaps — ξ1 ≥ 1 and an unvalidated `sd`

- **Status:** review
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

- [x] AC1 — A fit with `xi1 >= 1` is flagged `boundary`, returns `NA`
      reliability and SEm, and raises the boundary warning; no bare
      `NaNs produced` warning escapes `axes_reliability()` on any accepted
      input. The extracted boundary helper returns `TRUE` for `xi1 >= 1` and
      is unchanged for every case the shipped expression already caught
      (`xi1 <= 0`, negative `xi2`/`zeta1`/`eps`, and the ζ1-dropped path where
      `zeta1` is `NULL`).
- [x] AC2 — Numeric `sd` refuses `NA`, `NaN`, `Inf`, `-Inf`, `-1`, `0`, and a
      length-2 vector with one bad element, each with an error naming `sd`;
      `"std"`, `"raw"`, a positive scalar, and a positive length-2 vector all
      still return the same values they do today.
- [x] AC3 — Both guards verified by mutation, not by eye: deleting the
      `xi1 >= 1` disjunct and, separately, deleting the `sd` validation each
      redden at least one named test. Recorded in the work log with the test
      names (LESSONS.md, "a guard has teeth only if you break the guarded line
      and see it fail").
- [x] AC4 — `document()` leaves no uncommitted diff, and
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
- [x] **T4** — Run the mutation checks for AC3, then the profile's verify slot
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
- 2026-07-26: T4 done; status in-progress→review. Both mutation checks were run at their own tasks and are recorded there (T1: 2 assertions redden; T2: 8), so T4 carried only the full check. `devtools::check(manual = TRUE)`: **Status OK, 0 errors / 0 warnings / 0 notes**, 8m25s. The two steps this repo has learned to verify by name rather than trusting `Status: OK` are both present in the log: `checking PDF version of manual ... OK` and `checking re-building of vignette outputs ... [45s/49s] OK`. Branch is 4 commits over 3 files; PR not opened here — the PR URL is review's slot per the section-ownership table.
- 2026-07-26: deviation logged — the `r-package` profile's test-doctrine slot says new user-facing conditions use `cli::cli_abort()`, but `cli` is not in Imports and `R/` contains zero `cli_abort()` calls; adopting it would be a unilateral dependency change, which the tracking rules forbid. The new `sd` refusal matches the file's own `stop(..., call. = FALSE)` idiom instead.
- 2026-07-26: reviewed (/milestone-review). PR #88 opened as draft; three fresh-context lenses + a scorer. Three findings: F3 (88) and F2 (84) actioned and fixed on the branch, F1 (74) logged with its two textual over-claims fixed alongside on the M7 precedent. AC1-AC4 all verified with fresh evidence (Review section); four mutations run. `check(manual = TRUE)` 0/0/0 on the reviewed tip.

## Decisions

## Review

Reviewed 2026-07-26. PR [#88](https://github.com/jmgirard/circumplex/pull/88).

### Acceptance-criterion evidence

- **AC1 — ξ1 ≥ 1 flagged, no bare NaN warning, shipped cases unchanged.** Four
  tests, all green on the reviewed tip: the predicate across the new and every
  shipped case incl. both ζ1-dropped branches (11 assertions), the swept
  never-NaN property over item_n ∈ {2, 2.5, 26/3, 16, 32} (5), the boundary
  branch reporting NA rather than NaN end to end (4), and — added at review, see
  F3 — the `NaNs produced` clause asserted on *unmocked* calls (3). Teeth shown
  by mutation, not by eye: forcing `axis_sem()` to take the square root of a
  negative reddens the new AC1 test (fail=1). Verified that the paired
  `is.na`/`is.nan` assertions are not redundant: `is.na(NaN)` is `TRUE`, so the
  `is.nan` half is what discriminates.
- **AC2 — `sd` refusals and unchanged acceptances.** 12 assertions green: each
  of `-1`, `0`, `Inf`, `-Inf`, `NA_real_`, `NaN` and two length-2 vectors with
  one bad element errors on "must be finite and positive", while `"std"`,
  `"raw"`, a positive scalar and a positive length-2 vector return exactly what
  they returned before (asserted against the `"std"` result at 1e-10). Logical
  `NA` was already refused by the pre-existing `is.numeric()` check.
- **AC3 — both guards mutation-verified.** Re-run fresh at review, per test:
  deleting the `xi1 >= 1` disjunct → predicate test fail=2, every other M62 test
  green; deleting the `sd` guard block → `sd` test fail=8, every other M62 test
  green. Each mutation reddens only its own guard's test, which is the point.
  Two further mutations run for the review fixes are recorded under F1/F3.
- **AC4 — docs and full check.** `document()` leaves no diff.
  `devtools::check(manual = TRUE)` on the reviewed tip: **Status OK, 0 errors /
  0 warnings / 0 notes**, 6m01s. Both steps this repo has learned to verify by
  name rather than trusting `Status: OK` are present in the log:
  `checking PDF version of manual ... OK` and
  `checking re-building of vignette outputs ... [37s/39s] OK` — the latter
  exercising the vignette edit made for F2. Tests inside check: 215s, OK.

### Consistency gate

`cairn_validate` exit 0, all 16 checks PASS. 47 advisory `work-log format`
warnings, every one on a hard-wrapped pre-implement entry in M7 — history, which
IP4 forbids editing; not touched. Toolchain slot: `document()` no diff;
`man/`/`NAMESPACE` regenerated never hand-edited; README.md in sync;
`pkgdown::check_pkgdown()` no problems; no new top-level files; full check clean.
NEWS.md needs no entry and gets none — `axes_reliability()` is new in 2.0.0 and
has never shipped, so nothing changes across released versions, and the existing
2.0.0 bullet's generic "a boundary fit returns `NA` reliability rather than a
clipped value" (`NEWS.md:171-173`) stays true under the widened definition.
No milestone numbers leak into user-facing text (grepped NEWS, README,
cran-comments, vignettes).

### Independent review — three lenses

- **[O] diff-bug (Opus):** 3 findings. Also cleared, with reasons, the three
  risks it was pointed at: the `!is.null(zeta1)` substitution is exactly
  equivalent at the only call site; the closed `xi1 >= 1` bound cannot reject a
  legitimate fit (every fit is on a unit-diagonal metric, so `xi1 = 1` forces
  `eps_i = 0`, and lavaan's `(N-1)/N` rescaling puts the sum further below 1);
  and no path bypasses the `sd` guard.
- **[S] blame-history (Sonnet):** no silent undoing. Confirmed the M61
  `fit_zeta1` flag was introduced only to avoid `logical(0)` inside `||`, which
  `!is.null(zeta1)` handles identically; that nothing depended on the old
  warning string; and that the test diff is purely additive with no numeric
  literal, tolerance, or existing assertion changed.
- **[S] prior-PR-comments (Sonnet):** no regression of any prior-review point.
  Read the archived `## Review` sections of M53–M55 and M59–M61 plus RR09/RR10/
  RR11; the GitHub inline-comment probe returned `[]`, so no per-PR walk. Noted
  the change is the faithful implementation of RR11 Beyond 2 and extends RR09's
  BC11 rather than contradicting it.

### Findings actioned (score >= 80)

**F3 (88) — the test claiming AC1's NaN-warning clause could not fail.**
`test-axes-reliability.R` titled a test "raises no bare NaN warning" while
mocking `axes_is_boundary` to `TRUE`, which routes to a literal
`sem <- c(NA_real_, NA_real_)`; the arithmetic that could raise the warning
never ran, and both assertions were implied by that literal. Confirmed by
mutation: with the `xi1 >= 1` disjunct deleted that test stayed green while its
sibling reddened. AC1's second clause therefore had no assertion anywhere.
**Fixed:** the mocked test is retitled to what it actually proves (the boundary
branch reports NA rather than NaN) with a comment saying what it does not probe,
and a new test asserts the clause on *unmocked* calls — capturing warnings via
`withCallingHandlers` on both an ordinary fit and a real boundary fit (the BC11
seed), requiring the boundary warning present and no `NaN` warning.
**Verified by mutation:** making `axis_sem()` compute `sqrt(-1 - rel)` reddens
the new test.

**F2 (84) — two user-facing descriptions left behind by the widened boundary.**
The diff updated the `warning()` text and the roxygen `@details` but not
`vignettes/axes-reliability.Rmd:156-158`, which *enumerates* the disjuncts and
was complete before the diff, nor `R/axes_reliability_oop.R:86`, whose printed
note names "non-positive axes variance" as the cause — after M62 it could state
the opposite one. (The print string was already inaccurate for the
negative-variance disjuncts, so only the worsening is new.)
**Fixed both:** the vignette now states the `(0, 1)` interval and why each end is
unusable; the print note names the *class* of solution and points at the
components table rather than guessing a cause. Confirmed no test or vignette
depended on the old strings before changing them.

### Findings logged, not actioned (score < 80)

- **F1 (74) — the closed bound does not deliver its own "never a bare zero"
  rationale.** Because the guard tests `xi1` rather than the derived `rel`,
  floating-point rounding admits an `xi1` within ~1e-15 of 1 for which the SB
  ratio rounds to exactly 1 and SEm is exactly 0 (verified: `item_n` 26/3 and 32
  at `xi1 = 1 - 1e-15`). **No NaN at any of these** — the Goal and AC1 hold, and
  a bare zero is outside what they promise, which is why this scored below the
  bar. Two textual over-claims **fixed anyway**, on the M7 precedent, because
  they sat in the exact lines F3's fix rewrote: the guard comment no longer
  claims to prevent "a bare zero" and now records what the bound does not claim,
  and the sweep test asserts `!is.nan(sem) & sem >= 0` with `1 - 1e-14` and
  `1 - 1e-15` added to its grid. **Verified by mutation:** reverting that
  assertion to `sem > 0` now reddens the test, so the added grid point has
  teeth. **Not done:** moving the guard onto `rel`, which would close the float
  gap at the cost of a per-axis predicate — a design change beyond this
  milestone, recorded in the code comment for whoever next touches it.
