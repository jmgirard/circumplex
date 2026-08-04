# M71: Refuse an infinite fitted diagonal in `axes_scaling_factor()`

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m71-scaled-fit-infinite-diagonal`

## Goal

`axes_scaling_factor()` refuses a fitted `sigma` carrying a `+Inf` diagonal
entry with a named-reason NA, instead of computing a scaling factor from the
corrupted correlation matrix `cov2cor()` returns for it.

## Scope

**In:** the positive-infinity hole in the diagonal guard at
`R/axes_scaled_fit.R:126` — a `+Inf` entry fails `<= 0`, `cov2cor()` maps its
row/column to zeros with a unit diagonal, `solve()` inverts that happily and
`all(is.finite(si))` accepts, so a factor is computed and reported as a
corrected statistic (measured live at the M70 review, F5 at 82; at the octant
probe with `sigma[4, 4] <- Inf` the function returns `scale = 0.9579017`,
`baseline = 0.8777185`, `reason = NULL`, no warning). The new refusal reports
`reason = "infinite_diagonal"`. Also in: the two reason-string enumeration
comments in `R/axes_reliability.R` (`fit_scaling_failed` gains the new literal;
`se_correction_failed` is corrected — it names two literals where its helper
contains four, a staleness that predates this milestone).

**Out:** the sibling surface `axes_corrected_se()`. The M71 criteria audit
measured `+Inf` at all 24 diagonal positions of the probe and found it already
refuses at every one (`reason = "unidentified"`, all three vectors NA, warned),
because it prices the raw `sigma` before normalizing — so it computes no wrong
number and carries only a misleading label. Relabelling it was declined at this
plan gate; it stays a `candidate` ROADMAP row, promoted on a user reporting the
label or on evidence the two surfaces must agree.
Out: reason-code parity between the two surfaces generally → declined at M70's
gate and unchanged here. Out: the `NA`/`NaN` and finite-nonpositive routes,
which are correct today → pinned by AC2, not modified. Out: any upstream check
on lavaan's fitted matrix in `axes_reliability()` → the guard stays at the
helper's own contract boundary.

## Acceptance criteria

- [ ] AC1 — At the octant probe with one `+Inf` diagonal entry,
      `axes_scaling_factor()` returns `scale` and `baseline` both `NA_real_`,
      `reason` identical to `"infinite_diagonal"`, and warns; the regression
      test asserting this is mutation-verified — the new guard line is reverted,
      the test observed red, and the line restored.
- [ ] AC2 — The four pre-existing refusal routes on this surface are unchanged:
      a `-Inf`, a `0`, an `NA` and a `NaN` diagonal entry each still yield
      `reason == "singular"`. Evidence: `tests/testthat/test-axes-scaled-fit.R`
      asserts all four explicitly (the `-Inf` cell is added by this milestone;
      the other three exist at `:1229-1254`) and the file runs green with no
      edit to any pre-existing reason-string expectation.
- [ ] AC3 — The `fit_scaling_failed` and `se_correction_failed` comments in
      `R/axes_reliability.R` each name exactly the reason literals *contained
      in* their helper's source, enumerated by a grep over the `na_out(` call
      sites and the bare string returns of `R/axes_scaled_fit.R` and of
      `R/axes_corrected_se.R` (the latter including `axes_se_pricing()`, whose
      strings `axes_corrected_se()` forwards through `na_out(raw)`/`na_out(std)`
      rather than as literals).
- [ ] AC4 — The header comment at `R/axes_scaled_fit.R:114-119` no longer
      describes the `+Inf` case as an open candidate: `grep -n "ROADMAP
      candidate" R/axes_scaled_fit.R` returns nothing, and the block instead
      states which entries the two guards refuse and which fall through.
- [ ] AC5 — `devtools::test()` clean over the full suite, and
      `devtools::check(args = "--no-manual")` at 0 errors / 0 warnings / 0 notes.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T1, T2
- AC3 → T4
- AC4 → T2
- AC5 → T5

## Tasks

- [x] T1 — Tests first, in `tests/testthat/test-axes-scaled-fit.R` beside the
      existing diagonal cases at `:1229-1254`: a `+Inf` case expecting
      `"infinite_diagonal"` (fails against current code — it returns
      `reason = NULL`), and a `-Inf` case expecting `"singular"` (passes today;
      it is the route the fix must not disturb, and no test covers it).
- [x] T2 — Add the guard immediately *after* the `<= 0` line at
      `R/axes_scaled_fit.R:126`, testing positive infinity only, so `-Inf` keeps
      its existing route and `NA`/`NaN` keep falling through
      (`is.infinite()` is `FALSE` on both, so no `na.rm` is needed). Rewrite the
      `:103-125` comment block for the shipped behavior (AC4).
- [x] T3 — Mutation-verify T2's guard: revert the line, confirm T1's `+Inf`
      test goes red, restore, confirm green.
- [x] T4 — Correct the two enumeration comments in `R/axes_reliability.R`
      (`:1871` gains `"nonpositive_diagonal"` and `"indefinite"`; `:1892` gains
      `"infinite_diagonal"`), and update the enumeration guard at
      `tests/testthat/test-axes-corrected-se.R:1181-1185` if it pins the set.
- [x] T5 — Full `devtools::test()` and `devtools::check(args = "--no-manual")`;
      no roxygen change is expected (`man/axes_reliability.Rd:80` documents the
      field as "a string naming why", not an enumeration) — re-document only if
      that proves wrong.

## Work log

- 2026-08-04: created by /milestone-plan.
- 2026-08-04: criteria audit ([O], fresh context) returned four findings. Two were fixed here: the draft's "refuse a non-finite diagonal" would have captured `-Inf` and silently changed its reason on a route no test covered, so the guard is narrowed to positive infinity, placed after the `<= 0` test, and AC2 gains an explicit `-Inf` assertion; and AC3's domain was narrowed from the literals a helper *can return* (a reachability claim grep cannot settle — `"indefinite"` is recorded at `R/axes_corrected_se.R:185-198` as never having fired in 3822 draws) to the literals its source *contains*, which the named grep enumerates exactly. Two went to the gate as questions: the sibling surface's scope, and the reason literal. The audit also corrected the draft's claim that both helpers forward `axes_se_pricing()` strings — only `axes_corrected_se()` calls it.
- 2026-08-04: plan gate chose refusing positive infinity only, after the `<= 0` guard, over a `!is.finite()` guard ahead of it, because the latter relabels the `-Inf`, `NA` and `NaN` routes that are correct today; falsified by evidence that a caller needs one reason for every non-finite entry.
- 2026-08-04: plan gate chose `"infinite_diagonal"` over reusing `"singular"` and over `"nonfinite_diagonal"`, because an infinite variance is not a singular matrix and a non-finite label would be false for the `NA`/`NaN` entries that keep reporting `"singular"`; falsified by a user or a downstream consumer needing the two infinite and missing cases to share one code.
- 2026-08-04: plan gate chose leaving `axes_corrected_se()`'s label alone over relabelling both surfaces, because it already refuses and warns so no user is misled about a *result*, and relabelling reopens the cross-surface parity question M70 declined; falsified by a user reporting the `"unidentified"` label, or by any need for the two surfaces to agree.
- 2026-08-04: T1 tests written and confirmed red before the fix — the `+Inf` case fails 4 assertions (`reason` NULL, `scale`/`baseline` returned as numbers), the `-Inf` control passes, showing it already takes the `<= 0` guard. T1 stays unticked until T2 turns it green, so no task is checked off against a red suite.
- 2026-08-04: T2/T3 done — the guard `if (any(is.infinite(diag(sigma)))) return(na_out("infinite_diagonal"))` sits immediately after the `<= 0` line at `R/axes_scaled_fit.R:145`, so `-Inf` keeps its `"singular"` route and `NA`/`NaN` keep falling through (`is.infinite()` is FALSE on both, no `na.rm` needed). `test-axes-scaled-fit.R` green; mutation-verified by deleting the guard line (the same 4 assertions redden), restoring, and re-running green. The `:103-125` comment block is rewritten as a three-door table naming which entry each guard refuses, and no longer calls the `+Inf` case an open candidate — the phrase AC4 greps for is gone from the file.
- 2026-08-04: T4 done — AC3's grep run over both helpers gives `axes_scaled_fit.R` = {df_mismatch, baseline_df_mismatch, singular, unidentified, indefinite, infinite_diagonal} and `axes_corrected_se.R` = {nonpositive_diagonal} direct plus {singular, unidentified, indefinite} forwarded from `axes_se_pricing()`. Both `details` comments now match: `:1871` gained the two literals it had been missing since M69, `:1892` gained `"infinite_diagonal"`, and each says it enumerates what the source CONTAINS rather than what a user has been shown (`"indefinite"` has never fired). The enumeration guard at `test-axes-corrected-se.R:1181-1185` needed no change — M71 adds no literal to the file it pins.
- 2026-08-04: full `devtools::test()` green — FAIL 0 | WARN 4 | SKIP 0 | PASS 5788. The four warnings are pre-existing and not from this branch: `test-axes-scaled-fit.R` alone reports WARN 0 over 671 passes, and both new inputs return before `cov2cor()`, so neither can raise an uncaught warning.
- 2026-08-04: T5 done — `devtools::check(args = "--no-manual")` Status OK, 0 errors / 0 warnings / 0 notes (14m 25s, circumplex 2.0.0). No roxygen change was needed: `man/axes_reliability.Rd:80` documents the field as "a string naming why" and never enumerates the literals, so the corrected enumerations are code comments only. All tasks done; status in-progress→review.

## Decisions

## Review
