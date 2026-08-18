# M94: Print the fired-marker list on the bootstrap path

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP5
- **Branch/PR:** —

## Goal

Make `summary()` on a bootstrap-CI `circumplex_cpm` fit print the fired
boundary/weak-identification marker list with its own validation limit stated,
where today only the analytic caution ever names the marker vocabulary.

## Scope

Surface tier: **user-facing** — the deliverable is printed console output, a
help page, and vignette prose, all reaching package users.

**In:**
- A descriptive marker line in `summary()`'s `# Diagnostics` section on
  `ci_method = "bootstrap"` fits, at every N, naming the fired
  `cpm_marker_labels()` labels and stating that the markers were validated as
  interval predictors on the analytic path only (`R/cpm_oop.R:218-222` is the
  insertion region).
- Byte-identity fences on the analytic path and `print()` (the D-010
  coverage-validated caution at `R/cpm_oop.R:234-267` is untouched).
- The `summary.circumplex_cpm` roxygen and the vignette locus paragraph
  (`vignettes/evaluating-circumplex-structure.Rmd:169-172`) updated, with the
  guard test extended.

**Out:**
- Any analytic-path or `print()` output change → none; the analytic caution's
  thresholds and wording stay under D-010.
- Reworking the vignette's boundary demo around the jz2017 bootstrap fit →
  stays as shipped by M92; revisit only on teaching evidence (gate choice
  2026-08-18).
- Marker-conditioned coverage measurement for bootstrap intervals → the
  boundary-regime interval improvement candidate row.
- NEWS.md's historical M92 note → historical record, never updated.

## Acceptance criteria

- [ ] AC1: On a `circumplex_cpm` fit with `ci_method = "bootstrap"`,
      `summary()` prints in its `# Diagnostics` section one line naming every
      fired marker by its `cpm_marker_labels()` label, at every sample size
      (no N gate). Tested on two bootstrap fits running unskipped on CI and
      CRAN (small `boots`): one with N < 2000 whose asserted fired set has
      ≥2 markers (exercising the join) and one with N ≥ 2000 firing exactly
      one; each test asserts the fired set via `cpm_boundary_markers()`
      first, then asserts the marker line itself (not the whole section) with
      its fixed label prefix, naming each fired label and no unfired one,
      using only code-composed phrases (the prefix and `cpm_marker_labels()`
      values, never data-borne strings such as scale names).
- [ ] AC2: The marker line states its own validation limit: its fixed text —
      pinned exactly by test — says the markers were validated as interval
      predictors on the analytic path only and were not studied on the
      bootstrap path; the marker line contains none of "mis-cover", "near a
      parameter boundary", "no effect", "does not affect" (asserted on the
      line, not the whole output). On a bootstrap fit whose fired set the
      test asserts is `character(0)`, `summary()` prints no marker line.
- [ ] AC3: The analytic path and `print()` are unchanged: analytic-path
      `summary()` output is byte-identical to the merge-base commit's output
      for four fits — clean N ≥ 2000, marker-firing N ≥ 2000, N < 2000, and
      free-scaling N ≥ 2000 — with the capture regenerable at review from the
      merge-base commit (procedure recorded in the test file); on an analytic
      marker-firing fit the fired-label set appears exactly once in
      `summary()` output; `print()` output on a bootstrap marker-firing fit
      is byte-identical to merge-base.
- [ ] AC4: The three surfaces this milestone updates agree with the new
      behavior: (a) the `summary.circumplex_cpm` roxygen describes the
      bootstrap marker line and `devtools::document()` runs warning-free;
      (b) the vignette locus paragraph (anchor "`summary()` prints that list
      when", kept in the rewrite) states the new behavior; (c) the guard test
      `tests/testthat/test-cpm_boundary_vignette.R` gains an assertion on
      that paragraph pinning the corrected claim and failing on the retired
      "rather than the list" wording, skipping only where the package
      installs without vignettes. A supplementary `grep -rni "marker"` sweep
      over `R/`, `man/`, `vignettes/`, `tests/testthat/` is reviewed for
      further printing-locus claims and its result recorded in the work log
      (non-load-bearing).
- [ ] AC5: The r-package profile's verify commands are clean:
      `devtools::test()` all passing and
      `devtools::check(args = "--no-manual")` at 0 errors / 0 warnings /
      0 notes.

## Coverage

- AC1 → T2, T3
- AC2 → T2, T3
- AC3 → T1, T3
- AC4 → T4
- AC5 → T5

## Tasks

- [ ] **T1** — Capture the merge-base baselines and write the byte-identity
      fences (four analytic `summary()` fits + bootstrap `print()`),
      recording the regeneration procedure in the test file.
- [ ] **T2** — Write the bootstrap marker-line tests (red first): the two
      CI-runnable fits with fired-set assertions, marker-line prefix/label
      assertions, the exact-text pin, the negative-phrase assertions, and the
      empty-set absence case.
- [ ] **T3** — Implement the marker line in `summary.circumplex_cpm`
      (bootstrap branch, after the diagnostics block at
      `R/cpm_oop.R:218-222`); T1/T2 green.
- [ ] **T4** — Update the roxygen (`R/cpm_oop.R:138-152`) and `document()`;
      rewrite the vignette locus paragraph keeping its anchor; add the
      guard-test assertion; run the supplementary grep sweep and log its
      result.
- [ ] **T5** — Full profile verify: `devtools::test()` and
      `devtools::check(args = "--no-manual")` clean.

## Work log

- 2026-08-18: created by /milestone-plan from the M92-residue candidate row; its promotion condition ("evidence users miss the markers there") had not fired — Jeff overrode the parking at the plan gate by selecting the candidate.
- 2026-08-18: criteria audit ran in full mode ([O] fresh-context reader, two passes): round 1 found AC4's enumerating grep provably missed the vignette sentence being fixed and AC2/AC3 instrument-bound; round 2 found AC2's ban on the token "coverage" colliding with the limit statement it requires, plus missing N-axis and free-scaling probes; all repairs applied to the committed wording.
- 2026-08-18: plan gate chose a caveat-carrying marker line over a bare list because the record validates the markers on the analytic path only (GP2/GP5); falsified by a bootstrap-path marker-coverage measurement showing the caveat wrong or misleading.
- 2026-08-18: plan gate chose the bootstrap-summary()-only locus over all-paths or print() printing because it matches the candidate exactly and leaves D-010's coverage-validated caution untouched; falsified by evidence that analytic sub-2000 or print() readers miss the marker vocabulary.
- 2026-08-18: plan gate chose a minimal vignette correction over reworking the demo around the jz2017 bootstrap fit because the candidate scopes this as a printed-output change, not teaching; falsified by evidence readers misread the boundary section after the change.

## Decisions

## Review
