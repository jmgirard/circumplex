---
name: statistical-validation
description: Numerically validate circumplex estimation code against independent references and angular boundary cases. Use after any change to ssm_* statistical functions or src/*.cpp, or when the user asks to verify statistical correctness.
---

# statistical-validation

Independent numerical validation of the SSM estimation pipeline. The unit
tests check that behavior didn't change; this skill checks that behavior is
RIGHT, by comparing against implementations that share no code with the
package. Run it from a throwaway R script (scratchpad), not from tests.

## Reference checks (run all that touch the changed code)

1. **SSM parameters vs OLS.** For equally spaced angles, the closed-form
   estimator must match `lm(scores ~ cos(rad) + sin(rad))` to ~1e-10:
   elevation = intercept, x/y = slopes, amplitude = sqrt(x²+y²),
   displacement = atan2(y, x) mod 360, Fit = summary(fit)$r.squared.
   Use several random score vectors, not just one.

2. **Circular quantiles vs rotation invariance.** For any bootstrap
   distribution of angles, adding a constant rotation to all replicates must
   rotate the CI endpoints by exactly that constant (mod 360). Test with
   distributions concentrated near 0°/360° and near 180°.

3. **Contrast displacement.** `angle_dist(a, b)` must equal the shortest
   signed rotation: check (10°, 350°) → +20°, (350°, 10°) → −20°,
   (179°, −179°) → −2°, and symmetry `angle_dist(a,b) == -angle_dist(b,a)`.

4. **C++ helpers vs base R.** `col_means` vs `colMeans(na.rm=TRUE)`;
   `pairwise_r` vs `cor(use="pairwise.complete.obs")`; `mean_scores`/
   `corr_scores` vs manual `tapply`/split computation, with and without NAs.

5. **End-to-end sanity on jz2017.** Point estimates from `ssm_analyze()` must
   equal directly computed values: group means → `ssm_parameters()`;
   correlations → `ssm_parameters()`. Bootstrap CIs must bracket the point
   estimates.

## Mandatory boundary cases

- Profile peaking exactly at 0°/360° (scores = cos(rad)): displacement is the
  documented boundary value, amplitude = 1, fit = 1.
- Bootstrap CI straddling 0°/360°: lci > uci in degrees is expected; circle
  plot must still draw a tight arc.
- Contrast near ±180°: estimate must lie inside its CI on the same branch.
- Flat profile (constant scores): documented degenerate behavior, no silent
  garbage.
- Unequal angle spacing: results will differ from OLS — confirm the difference
  is the documented estimator choice, not a new bug (compare against the
  2/n Σ formulas computed by hand).

## Reporting

State pass/fail per check with the observed max discrepancy. Any failure
blocks the change until explained: either the new code is wrong, or the
reference comparison is invalid — say which and why. If a check reveals a
pre-existing (not newly introduced) defect, record it instead of silently
fixing it in passing: as a task in MILESTONES.md if it belongs to the active
milestone, otherwise as a plain bullet under the appropriate ROADMAP.md
milestone (a future intention, not a checkbox).
