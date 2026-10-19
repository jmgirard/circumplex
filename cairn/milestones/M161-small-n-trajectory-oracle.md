# M161: Small-N coverage oracle for trajectory intervals

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** RR25 (advisory)
- **Principles touched:** IP1, IP3, IP4
- **Resolves:** —
- **Surface tier:** internal — a `devel/` simulation oracle, its record and a decision entry, with no export, help page or behavior change
- **Branch/PR:** m161-small-n-trajectory-oracle

## Goal

Measure the coverage of the `ssm_trajectory()` intervals from `coef` and `vcov` at small numbers of persons, and select by a pre-registered coverage gate the small-sample draw rule that M162 ships.

## Scope

**In:** a seeded simulation oracle in `devel/` modeled on the M27 oracle, its committed results and record, the comparison of candidate draw rules, and the decision entry that names the rule. The candidate rules come from the Fable review that the plan gate asked for, plus the two rules named in AC2.

**Out:** any change to `R/`, `man/`, the vignettes or NEWS.md. The new argument, its help text, the vignette's Section 7 and the NEWS entry land in M162. Draws from a Bayesian fit (`draws =`) are not corrected; they already carry the variance-component uncertainty. A shared-intercept cell stays in the "Longitudinal deferrals" candidate row.

## Acceptance criteria

- [ ] AC1: `devel/m161-small-n-oracle.R` simulates replicates from the model family that the M27 oracle's header names, at waves 0 to 4. It fits each replicate by REML with the glmmTMB call that `ssm_growth_formula("glmmTMB")` returns. It runs 1000 replicates in each cell of a grid. The grid crosses the M27 oracle's `pole` and `xycor` truths with N in {10, 20, 30, 50, 200} persons. For each truth it adds missing-wave cells at N = 20 and N = 30, with 20% of person-waves deleted at random. Each cell is seeded by its level index.
- [ ] AC2: The script's header fixes the candidate rules, the gated waves and the selection gate before the full run. The candidates include the shipped normal draws, multivariate t draws with nu = N - 1, and each rule that the driving review names. All five waves are gated for d(t), because the true a(t) is at least .59 at every wave of both truths. Coverage counts every replicate, and a failed or non-positive-definite fit counts as not covering. A rule passes when two conditions hold in every complete-data cell with N in {20, 30, 50}. Its mean 95% interval coverage over waves lies in [.93, .97] for each of e, x, y and gated d. No single wave of these falls below .91.
- [ ] AC3: `devel/m161-small-n-results.rds` and `devel/m161-small-n-oracle.md` report the coverage of the e, x, y, a and d intervals for every rule, cell and wave. They also report the certification rate, the Monte Carlo standard error of each coverage, and the count of failed or non-positive-definite fits. For every gated cell they report the coverage among converged, positive-definite fits beside the coverage over all replicates. The N = 10 cells and the missing-wave cells are reported without a gate. The record names the rules that passed the AC2 gate.
- [ ] AC4: In both complete-data N = 200 cells, the shipped rule's mean d(t) coverage over the gated waves lies in [.93, .97].
- [ ] AC5: A D-entry in `cairn/DECISIONS.md` names the rule that M162 ships and cites the record. It is chosen among the passing rules, other than the shipped normal draws, that are computable from `coef`, `vcov`, `times` and one numeric argument with a stated lower bound. When more than one passes, it is the rule with the narrowest median d(t) arc width, `(d_uci - d_lci) %% 360`, taken over replicates and gated waves and pooled over the gated cells. When none passes, the entry records that none passed.

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
- 2026-10-18: criteria re-audit after the question set (reduced mode, same fresh reader) returned three findings, all fixed: AC2 now gates all five waves with the amplitude floor that justifies it (true a(t) at least .591 in `pole` and .598 in `xycor`, read from `devel/m27-coverage-oracle.R:227-276`); AC3 reports coverage among converged fits beside the all-replicates figure, so a gate miss from fit failures is visible; AC5 limits the chosen rule to one computable from `coef`, `vcov`, `times` and one bounded numeric argument, the form M162 can ship. A review rule outside that form comes back through an amendment.
- 2026-10-18: lessons bearing on this scope: run the oracle under `load_all()` (M21 family); commit exact seeds per level index; a frozen measurement needs a gate (M108 family).
- 2026-10-18: implement started on branch m161-small-n-trajectory-oracle, cut from master at 1464b6d4.
- 2026-10-18: T1: drafted RB25 (`cairn/reviews/RB25-small-n-trajectory-draws.md`), advisory, eight questions. Its background records the author's unverified Satterthwaite estimate that the shipped normal draws reach about .936 to .945 marginal coverage at N = 20, which would let them pass AC2's gate. First escalation of this mechanism (sweep of `cairn/reviews/` found none).
- 2026-10-18: blocked on RB25.
- 2026-10-18: RR25 returned by a Fable subagent (user-approved spawn) and ingested; recorded as M161-D1 to M161-D3 below. Triage: apply recs 1, 2, 5, 8 in M161, and rec 4's non-finite `vcov` check in `fit_joint()`; rec 3 (calibration selection with N = 10 in the domain) goes to the user at the amendment stop, beside a derivation-selects alternative; rec 4's boundary-rate ceiling is not applied while N = 10 stays ungated (measured N = 20 rate 0% to 1.2%); recs 6, 7 (help and vignette part) and 9 are routed to M162's work log; recs 10 to 12 rejected for the reasons RR25 gives. The `tbar` agreement check from "Beyond the brief" is a candidate row. No standing D-entry is contradicted (rec 12 upholds the plan gate's opt-in).
- 2026-10-18: unblocked; status back to in-progress.
- 2026-10-18: re-audit: AC2 (reduced) — the header-timing promise bound a recording act, now a commit-order fact; the header list named a renamed "gate", now the band plus AC3's targets and tolerance.
- 2026-10-18: re-audit: AC3 (reduced) — the closed-form check could not tell R1 from R0 at N of 20 or more (miss .015 against a .024 tolerance), so a paired R1 minus R0 check was added; R0 and R1 targets, the 3.5 SE tolerance and conditioning on converged fits were confirmed; a target miss made the AC6 fallback unreachable, so AC6 no longer conditions on AC3.
- 2026-10-18: re-audit: AC4 (reduced) — converged-fit coverage was required only at gated N while AC3 needs N = 200 and a finite `vcov`; the failure counts are now disjoint and ordered, and the paired-difference domain is named.
- 2026-10-18: re-audit: AC6 (reduced) — "no rule passed" was false when R0 or R3a passes the band; the df = N - 1 derivation omitted its no-covariate and interior-estimate conditions and the first-order caveat for d(t); all fixed in the draft.
- 2026-10-18: re-audit: AC2 (reduced) — the commit-order fact did not show the header unchanged at the results commit; a no-diff condition on the header block was added.
- 2026-10-18: re-audit: AC3 (reduced) — nothing; the paired-difference target and its standard error were confirmed.
- 2026-10-18: re-audit: AC4 (reduced) — a non-finite `vcov` counted as a failed fit left the third failure count always zero, so T2 now gives it a distinct status; paired differences now name both bases and their standard errors.
- 2026-10-18: re-audit: AC6 (reduced) — the R1-fails branch still required a shipping rule, so the entry now names none then, and T4's fallback follows; the derivation's conditions now name the random-intercept, single-linear-time model.
- 2026-10-18: second re-audit on AC2, AC4 and AC6 reached the repeated-churn stop; the amendment (draft in the session scratchpad, quoted at the chip) goes to the user with the RR25 rec 3 alternative.

## Decisions

- M161-D1 (2026-10-18, RR25 §1, §2, §6): the premise holds but is smaller than assumed. On complete balanced data REML equals the ANOVA fit, and at the mean design time (wave 2) the pivot of `e`, `x`, `y` and every linear combination is exactly t with N - 1 df under every truth. The shipped normal draws cover .918, .935, .940 and .944 at N = 10, 20, 30 and 50 there, and slightly more at the outer waves. AC2's band as planned does not separate them from a corrected rule at N = 20 to 50, and AC5's width criterion would choose a `vcov` inflation by N / (N - 1), which undercorrects (.931 at N = 10, centre wave).
- M161-D2 (2026-10-18, RR25 §3, §4): the candidate set is R0 (shipped normal draws), R1 (multivariate t, one chi-square divisor shared by all coefficients, nu = N - 1) and R3a (normal draws on `vcov` times N / (N - 1)) as a plausible-but-wrong control. Not gated: independent t marginals (no closed form; correlation attenuated .931 at nu = 9), level-specific or variance-matched inflations, design-based df rules (need T as well as N; at most 3.4% narrower than R1 at N = 20), and nu = N - 2 or N - 3 (overcorrect). No rule's df are computable from `coef` and `vcov` alone, so the opt-in argument stands.
- M161-D3 (2026-10-18, RR25 §5, §8): glmmTMB's REML `vcov` includes a variance-component term that vanishes on complete balanced data and adds 0.01% to 0.42% to the SE on 20%-missing cells, so the missing-wave cells are read like the complete ones. At N = 10 the fit reaches the boundary of the 3 by 3 random-effect covariance in 14% to 34% of replicates, so N = 10 coverage counting failures as misses reads .59 to .75 for every rule. `vcov.glmmTMB` can return an all-`NA` matrix with `pdHess` TRUE, which the oracle's `fit_joint()` must count as a failed fit.

## Review
