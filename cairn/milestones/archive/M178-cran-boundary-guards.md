# M178: Run the boundary bug guards on CRAN

**Status:** done (2026-10-30, PR #212 https://github.com/jmgirard/circumplex/pull/212)

**Goal:** Five guards against past boundary and order bugs run under CRAN's own check, where `skip_on_cran()` hides them today.

**Outcome:** `skip_on_cran()` removed in place from 13 blocks: `ssm_analyze_long()` occasion order (2, `test-ssm_analyze_long.R`), the SEM group contrast near ±180° and the relabel flip (2, `test-ssm_sem_groups.R`), the trajectory seam battery (6, `test-ssm_trajectory.R`), free-scaling CPM pole recovery (2, `test-cpm_boundary.R`) and the `angle_dist()` half-turn atom (`test-utils.R`). Two bounds changed: the relabel displacement bound 1e-3° → 1° (measured 1.14e-4°, defect 141°), and the half-turn block's `atan2` loop now asserts the (-pi, pi] contract next to the half-turn instead of a bit-exact pi. The comment above the M168 test in `test-lavaan-cfi-helper.R` now says the R-CMD-check and test-coverage jobs run `skip_on_cran()` blocks, since `r-lib/actions/setup-r` exports `NOT_CRAN=true`. `cairn/boundary-coverage.md` re-cited for the two edited files, with an M178 audit note. No `R/` or `src/` change. CRAN-mode cost: 2.86 s summed locally; `R CMD check --as-cran` on the tarball with `NOT_CRAN` unset: Status OK, SKIP 619 → 606.

**Decisions:** D-076 (these guards run on CRAN, the lavaan-based ones included). Plan gate chose in-place skip removal over CRAN-only copies.

**Review:** Criteria audit at plan (full mode): 12 findings, all fixed. Claim audit: 4 claims, 1 corrected. Four-lens fan-out, one pass, 11 findings: 3 fixed (platform-robust `atan2` check, 1° relabel bound, comment scope), 2 to the new parked row "Tidy three test comments and checks", 4 rejected (ggplot2 exposure and the `:463` cite false, D-063 premise false, stale absorbed row planned). Hygiene graduated the "CRAN-dark boundary invariants" row, pruned M175's terminal row, captured one lesson and evicted the 2026-09-25 (M155) lesson by rank.
