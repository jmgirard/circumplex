# M70: Self-describing scaled fit output for `axes_reliability()`

**Status:** done (2026-08-04, PR #96 https://github.com/jmgirard/circumplex/pull/96)

**Goal:** Let a reader of an `axes_reliability()` object locate its fit
statistics on the calibration curve, and against lavaan's own fit measures,
without recomputing anything.

**Outcome:** `details` gained `n_moments` (p* = p(p+1)/2) and `baseline`, the
independence model's unscaled chisq and df; `details$n` is documented as the N
the fit was priced at, distinct from `n_total`/`n_complete`. Roxygen, the
`summary()` note, the vignette and NEWS now name the scaled statistics as
lavaan's `*.scaled` definitions, not `*.robust`, and state that a plain ML fit
yields no `*.robust` measure at all. Fixed `axes_scaling_factor()`'s NA-unsafe
nonpositive-diagonal guard (`na.rm = TRUE`), graduating the row M69 left. No
reported statistic moved.

**Decisions:** Reason-code parity with `axes_corrected_se()` on a finite
nonpositive diagonal declined as user-visible output. `details$n` documented
rather than aliased — two fields holding one number can later disagree.

**Review:** 19 findings, 6 actioned at >= 80, all fixed: a false "reproduce the
cfi" claim on three surfaces (five inputs, not three), misleading `cfi.robust`
advice, a comment overclaiming a non-finite guarantee `+Inf` defeats, an AC2
guard skipping always under `R CMD check`. One amendment return on AC1.
