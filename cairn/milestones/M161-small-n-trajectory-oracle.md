# M161: Small-N coverage oracle for trajectory intervals

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3, IP4
- **Resolves:** —
- **Surface tier:** internal — a `devel/` simulation oracle, its record and a decision entry, with no export, help page or behavior change
- **Branch/PR:** —

## Goal

Measure the coverage of the `ssm_trajectory()` intervals from `coef` and `vcov` at small numbers of persons, and select by a pre-registered coverage gate the small-sample draw rule that M162 ships.

## Scope

**In:** a seeded simulation oracle in `devel/` modeled on the M27 oracle, its committed results and record, the comparison of candidate draw rules, and the decision entry that names the rule. The candidate rules come from the Fable review that the plan gate asked for, plus the two rules named in AC2.

**Out:** any change to `R/`, `man/`, the vignettes or NEWS.md. The new argument, its help text, the vignette's Section 7 and the NEWS entry land in M162. Draws from a Bayesian fit (`draws =`) are not corrected; they already carry the variance-component uncertainty. A shared-intercept cell stays in the "Longitudinal deferrals" candidate row.

## Acceptance criteria

- [ ] AC1: `devel/m161-small-n-oracle.R` simulates replicates from the model family that the M27 oracle's header names, at waves 0 to 4. It fits each replicate by REML with the glmmTMB call that `ssm_growth_formula("glmmTMB")` returns. It runs 1000 replicates in each cell of a grid. The grid crosses the M27 oracle's `pole` and `xycor` truths with N in {10, 20, 30, 50, 200} persons. For each truth it adds missing-wave cells at N = 20 and N = 30, with 20% of person-waves deleted at random. Each cell is seeded by its level index.
- [ ] AC2: The script's header fixes the candidate rules, the gated waves and the selection gate before the full run. The candidates include the shipped normal draws, multivariate t draws with nu = N - 1, and each rule that the driving review names. The gated d(t) waves are fixed from each truth's amplitude, never from a rule's certification rate. Coverage counts every replicate, and a failed or non-positive-definite fit counts as not covering. A rule passes when two conditions hold in every complete-data cell with N in {20, 30, 50}. Its mean 95% interval coverage over waves lies in [.93, .97] for each of e, x, y and gated d. No single wave of these falls below .91.
- [ ] AC3: `devel/m161-small-n-results.rds` and `devel/m161-small-n-oracle.md` report the coverage of the e, x, y, a and d intervals for every rule, cell and wave. They also report the certification rate, the Monte Carlo standard error of each coverage, and the count of failed or non-positive-definite fits. The N = 10 cells and the missing-wave cells are reported without a gate. The record names the rules that passed the AC2 gate.
- [ ] AC4: In both complete-data N = 200 cells, the shipped rule's mean d(t) coverage over the gated waves lies in [.93, .97].
- [ ] AC5: A D-entry in `cairn/DECISIONS.md` names the rule that M162 ships and cites the record. It is chosen among the passing rules other than the shipped normal draws. When more than one passes, it is the rule with the narrowest median d(t) arc width, `(d_uci - d_lci) %% 360`, taken over replicates and gated waves and pooled over the gated cells. When none passes, the entry records that none passed.

## Coverage

- AC1 → T2, T3
- AC2 → T1, T2
- AC3 → T3, T4
- AC4 → T3, T4
- AC5 → T4

## Tasks

- [ ] T1: Ingest the driving review's report through `/milestone-brief`. Set the `Driving RR:` slot. Fold each candidate rule and each binding criterion into AC2 and the tasks under the amendment protocol, with a work-log line either way.
- [ ] T2: Write `devel/m161-small-n-oracle.R`. Reuse the M27 truths and generator from `devel/m27-coverage-oracle.R`. Write the header's pre-registration (candidate rules, gated waves, gate, seeds disjoint from the M27 and M21 offsets) before any full run. Add a smoke mode (`M161_SMOKE=1`, 25 replicates per cell) that checks the machinery only. Each candidate rule draws from the same fit, so the rules share replicates.
- [ ] T3: Run the full grid in the background and commit `devel/m161-small-n-results.rds`. Record the run time and the R, glmmTMB and package versions in a work-log line.
- [ ] T4: Write `devel/m161-small-n-oracle.md` in the M27 record's shape: verdict table, reading, design iterations and oracle types. Append the D-entry that AC5 names. If no rule other than the shipped one passes, M162's goal is wrong, and its next step is `/milestone-plan`.

## Work log

- 2026-10-18: created by /milestone-plan. tier check: milestone — a statistical method choice with a design decision and a coverage oracle, over more than one sitting.
- 2026-10-18: absorbs the candidate row "Small-sample correction for trajectory draws" in full, split as M161 (oracle and rule) and M162 (the argument, docs and NEWS). feature series: M161 -> M162.
- 2026-10-18: collision check: D-060 and D-064 (no fitter, engine-agnostic helper) hold, since the rule works on `coef` and `vcov` with no engine call. M155-D1's shape-dependent caution is extended in M162, not reopened. The "Longitudinal deferrals" and "Scaled-statistic residues" rows are distinct. Inbox sweep: one open issue (#180, CI red), no overlap; no open PRs.
- 2026-10-18: question set: Fable review of the rule design before the oracle runs — yes, a brief is written after the plan commit. Smallest gated N — 20, with N = 10 reported. Missing waves — reported cells at N = 20 and 30, not gated.
- 2026-10-18: criteria audit (reduced mode for M161, full for M162, fresh Opus reader) returned ten findings, all fixed: replicate-level denominator with failures counted as misses; gate on the mean over waves plus a per-wave floor, to absorb about 160 checks; gated waves fixed from the truth; the N = 200 control in the same form; the width tie-break's domain named. The M162 findings are in its work log.
- 2026-10-18: plan gate chose an opt-in argument over a new default because the helper cannot know N from `coef` and `vcov`; falsified by a rule whose degrees of freedom are computable from `coef` and `vcov` alone.
- 2026-10-18: plan gate chose a simulation gate among named candidates over adopting a published Kenward-Roger or Satterthwaite rule because glmmTMB offers neither and nlme's DF is 2095 for every coefficient here; falsified by an engine-agnostic published rule for this model that the oracle shows nominal.
- 2026-10-18: lessons bearing on this scope: run the oracle under `load_all()` (M21 family); commit exact seeds per level index; a frozen measurement needs a gate (M108 family).

## Decisions

## Review
