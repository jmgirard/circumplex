# M169: State which solution the CPM estimate is on the cpm_fit() help page

**Status:** done (2026-10-27, PR #203 https://github.com/jmgirard/circumplex/pull/203)

**Goal:** The `cpm_fit()` help page says which solution the CPM estimate is, and that a configuration with a lower discrepancy can exist, as D-071 decided.

**Outcome:** `?cpm_fit` section "Which solution is reported": the start battery per model (free-angle: theory angles, mirror, four 15/30-degree offset
patterns; fixed-angle: one start and three communality-index rescalings; plus the unit solution under free scaling), the boundary-harmonic polish,
the estimate as the ML solution in the basin those starts reach, a lower-discrepancy configuration that the reproduction check and the near-tied
note cannot signal, and a larger chi-square there (p smaller, RMSEA and CFI no better at the same df). Tests in `test-cpm_boundary.R`: a pinned
sample (replicate 30 of the M167 boundary truth, N = 250, seed 21261736) where the accepted fit carries no warning or marker and an HI
communality-wall start reaches 1.96 T units lower (`skip_on_cran()`, platform basin drift); unskipped start-battery size per variant and
fit-index direction at equal df. NEWS Documentation entry. DESIGN.md D-071 section corrected (no "small N" limit; unit seed named);
DESCRIPTION Config/roxygen2/version 8.1.1.

**Decisions:** none.

**Review:** four-lens fan-out, 34 findings: 17 fixed on the branch (df qualifier on the index claim, reproduction check glossed, communality
index wording, polish described without thresholds, test comments and header, tests for claims (a) and (d), planted defects in the code under
test, DESIGN unit seed, stale "small N" in LESSONS), 16 rejected, 1 follow-up (whether `summary()`'s analytic caution should fire on a
diagnosed lower basin) added to "State and settle the CPM engine estimate" part (ii). check(manual = TRUE) 0/0/0; CI green on 6 checks.
Hygiene: that row narrowed to part (ii) and its M169 token removed; one lesson captured; used M169 marked on the M90, M108-family, M116 and
M167 profile lessons (the M108-family line shortened to fit 450 bytes).
