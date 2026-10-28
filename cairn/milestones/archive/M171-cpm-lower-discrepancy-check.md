# M171: Flag a lower discrepancy that extra starts find in cpm_fit()

**Status:** done (2026-10-28, PR #205 https://github.com/jmgirard/circumplex/pull/205)

**Goal:** After the engine's fit, `cpm_fit()` runs M170's fixed set of extra starts and reports a discrepancy more than 0.01 chi-square units lower than the fit, without changing the estimate, as D-072 decided.

**Outcome:** `cpm_fit()` gains `check_lower = TRUE`. `cpm_lower_starts()` ports M170's W starts: p + 1 + 3(p - 1) + 3 choose(p - 1, 2) + p, built from the engine's clipped parameter vector. `cpm_lower_check()` fires on a raw gap > 0.01 T units and finishes the lowest point with the weight polish and canonicalization. `details$lower_check` records checked, found, gap, df, order_changed, more_wall, failed_starts, n_starts and error.
The check runs only for the quasi-circumplex model with unit scaling, on accepted fits, before the bootstrap, and uses no RNG. `cpm_lower_check_safely()` keeps an error in the check from aborting the fit. A note in `cpm_diagnostic_lines()`, printed by `print()` and `summary()`, gives the gap, the lower df when it differs, the order and the communality change, or says the check could not finish.
It adds no boundary marker, and the analytic caution is unchanged. `ssm_ci_accuracy()`'s internal fit skips the check. The help section "Which solution is reported" was revised, along with DESIGN.md's D-071 section, the cpm-boundary-fits vignette and NEWS.
`devel/m171-lower-check-oracle.R`: 0 mismatches against M170's W on 925 regenerated fits (boundary N = 250, interior N = 500), on M170's machine. Three heavy CPM test files turn the check off with a file-scoped mock (the suite had grown by 77 s of CPU); the two snapshot tests keep the real check.

**Decisions:** none. The plan gate set an argument on by default, the default model only, a threshold of 0.01 with the gap printed, and no marker.

**Review:** Pass 1 returned before the lenses ran (review return 1: AC6 no-note tests, AC7 cost wording; fixed under T8). Pass 2 was a four-lens fan-out with 29 findings: 21 fixed, 7 rejected, 1 follow-up. Among the fixes: fail closed on an error or when every start fails, accepted fits only, the df phrase kept whole, small gaps printed with four decimals, the record's fields documented, the help sentence revised as D-072 asked, and tests that fail if the threshold or the logit clip is broken. The follow-up is the row "Repoint stale line cites into cpm_fit.R".
A claim audit read 75 claims and corrected 6. check(manual = TRUE) was clean, and CI was green on 6 checks. Hygiene: the row "Flag a lower optimum in CPM fits" graduated; M167's row was pruned for retention; two lessons were captured (an Edit-tool escape trap with `is_flag(NA)`, and plants under load_all with file-level mocks); M104 and M106 were evicted by rank; used M171 was marked on six lessons (M7 family, M108 family, M116, M167, M169, M170).
