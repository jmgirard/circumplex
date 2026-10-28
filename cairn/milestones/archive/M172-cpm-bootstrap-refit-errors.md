# M172: Exclude a bootstrap refit that errors in cpm_fit()

**Status:** done (2026-10-28, PR #206 https://github.com/jmgirard/circumplex/pull/206)

**Goal:** A bootstrap refit that stops with an error in `cpm_fit()` drops its resample with a counted warning instead of ending the call.

**Outcome:** `cpm_boot_refit()` holds one bootstrap refit attempt: `tryCatch(error = )` around `cpm_optimize_one()` and `cpm_gradient()`, and acceptance `is.finite(gnorm) && is.finite(F) && gnorm <= 1e-6 * max(1, abs(F))`. `cpm_bootstrap()` restarts a failed attempt once, never one that errored, and adds a still-failed resample to `n_nonconvergent`, which the unchanged exclusion warning, the `print()` note and `details$boots_nonconvergent` report. Every refit erroring gives `NA` intervals and the all-excluded warning. On error-free fits results match master at two seeds (`identical(..., ignore.environment = TRUE)`).
The `cpm_fit()` help says errored refits are excluded and counted; the marker lists on the `cpm_fit()` and `summary.circumplex_cpm()` pages use the five `cpm_marker_labels()` values, tied by a test reading `man/` or `tools::Rd_db()`. 62 line-number cites into `R/cpm_fit.R` and `R/cpm_oop.R` across `cairn/references`, `devel/`, `R/axes_reliability.R` and a test file now name functions. NEWS entry under "Minor improvements and fixes".
Tests in test-cpm_api.R mock refit faults through `local_refit_fault()`: errors at four call sites in two forms, a NaN norm, an infinite F, and every refit erroring.

**Decisions:** none. The plan gate counted errors as nonconvergent and kept the warning text.

**Review:** The full criteria audit returned 12 findings, all disposed. Review return 1 (AC1): a refit with infinite gradient norm and F was accepted; fixed with a probe that failed first. Two four-lens passes gave 35 findings: 15 fixed, 4 sent to candidate rows, 15 rejected (1 edge: `options(warn = 2)`), 1 noted. Claim audits read 49 and 21 claims, corrected 2 and 4. check() clean; CI green on 6 checks.
Hygiene: rows "Catch errors in CPM bootstrap refits" and "Repoint stale line cites into cpm_fit.R" graduated; rows "Harden CPM refit acceptance and errors" and "Raise the testthat floor to 3.1.7" filed; M169's row pruned for retention; one lesson captured (comparison traps), the M112 lesson evicted by rank; used M172 marked on the M7-family and M171 load_all lessons.
