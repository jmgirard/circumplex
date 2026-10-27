# M170: Measure the CPM engine's missed optima and settle which solution it reports

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3, GP7
- **Resolves:** —
- **Surface tier:** internal — a devel study and decision records, with no change to package code, help pages or vignettes
- **Branch/PR:** m170-cpm-engine-estimate-study

## Goal

Measure how often and by how much a lower discrepancy than `cpm_engine()`'s estimate exists, and compare the engine's estimate with two alternatives, so that a decision rule fixed before the run settles which solution the estimate should be.

## Scope

**In:** a seeded study `devel/m170-engine-estimate.R` (helpers beside it) with a pre-registration block, a timing pilot, a full run, a record `devel/m170-engine-estimate.md` and a results file `devel/m170-engine-estimate-results.rds`. A Fable review of the pre-registration before the full run. Four real or published fits. A D-entry with the decision, and DESIGN.md's D-071 section pointing at it.

Definitions. E is `cpm_engine()` as shipped. W is E plus a fixed deterministic set of extra starts: a communality-wall start per scale, an all-communality-level start, and starts with each free angle moved by 90, -90 and 180 degrees. W keeps the lowest discrepancy. H is the lowest discrepancy over W's starts plus 2K seeded random starts around E's estimate and 2K around the theory start. H_K uses only the first K starts for each centre. W and H run the same boundary polish as E. Cells: M167's two truths (boundary, interior; p = 8, communality .75, m = 3, variant A, unit scaling) at raw-data N = 250, 500, 1000 and correlation-matrix N = 2000, 5000, with M167's data generators and fresh seeds.

**Out:**
- Shipping any change to `cpm_engine()`, `cpm_fit()`, `print()`, `summary()` or the bootstrap goes to the ship-step candidate row that AC6 writes, when the decision changes behavior. That row also carries the formal T calibration re-run for the shipped estimator, the CIRCUM/CircE oracle re-check, the bootstrap warm-start change, and bootstrap coverage of an adopted alternative. "Catch errors in CPM bootstrap refits" joins it if that step opens `cpm_bootstrap()`.
- The help-page statement is M169.
- Non-normal data, other truths and instruments, variants B to D, and the free scaling family stay outside the measured envelope. The D-entry's scope names them, as D-068 did.
- Profile-likelihood intervals stay in the "Boundary-regime interval improvement" row.

## Acceptance criteria

- [ ] AC1: A pre-registration block at the top of `devel/m170-engine-estimate.R` fixes these items: the truths, cells and seeds, including the seeds of the AC5 fits; the replicate count rule from the timing pilot; the definitions of E, W and H, with K; the threshold for a lower discrepancy (0.01 in T units, T = (N - 1) times the discrepancy); the saturation rule; every metric that AC3 and AC4 name; and the decision rule. The saturation rule compares H_K with H, and compares H with an independent search family (another spread or centre, fixed in the block) on a stated subsample per cell. The decision rule is an ordered if/else chain over named metrics. It ends in a default branch and selects one of five options: keep E unchanged; keep E and add a diagnostic for a lower optimum outside the theoretical configuration; report W; report H; or report E with the lowest discrepancy found beside it. It selects W or H only where the saturation verdict passes in the cells it relies on and AC4's figures favor that estimator. Otherwise it selects among the three options that keep E. The block states how it breaks a tie between the diagnostic and the report-beside option under GP4. (RB tripwire: no-oracle)
- [ ] AC2: For each cell, `devel/m170-engine-estimate.md` reports the share of accepted E fits where H_K and where H find a discrepancy lower than E's by more than the threshold. The figures are computed from `devel/m170-engine-estimate-results.rds`. The record also reports the saturation rule's verdict. In a cell whose verdict is a fail, H's shares are labelled as lower bounds in the record and in the D-entry. H's AC4 figures are labelled everywhere as describing H, not the global minimum.
- [ ] AC3: For each cell and for each of W and H, the record reports these figures from the same results file, over accepted E fits. (a) The share with a discrepancy lower than E's by more than the threshold. (b) The median and largest gap in T units among those fits. (c) The share of those whose lower point has more scales with a communality above .995 than E's. (d) The share of those whose cyclic angle order, after `cpm_canonicalize()`, differs from E's. (e) W's median CPU time per fit on an unloaded core, beside E's.
- [ ] AC4: For each cell and for each of E, W and H, the record reports these figures from the same results file, over accepted E fits. (a) The bias and RMSE of each free angle (the signed circular difference from the truth), each communality and each weight. (b) Per parameter, the share of fits with a defined analytic standard error, and the 95% Wald coverage on those fits. (c) The share of fits whose T exceeds the .95 chi-square quantile at the degrees of freedom of the model as fitted at that point.
- [ ] AC5: The record reports four real or published fits, each at the settings its oracle test or vignette uses (m, angles and model). The fits are: `jz2017`'s eight octant scales on the raw-data path; the matrices from `cpm_oracle_voc()` and `cpm_oracle_verbal()` in `tests/testthat/helper-cpm-oracles.R`; and the simulated `sim` sample that the `cpm-boundary-fits` vignette fits as `demo`. For each, the record gives E's T and estimates. For W and H it gives the gap in T units. Where the gap exceeds the threshold, it also gives their estimates and any change in angle order.
- [ ] AC6: A new entry in `cairn/DECISIONS.md` records which of the five options the estimate takes, as the output of AC1's decision rule on the committed results. It gives the rationale, the scope (truths, cells, normal data, variant A, unit scaling) and the record files its figures come from. It names the commit that added the pre-registration block and says whether any full-run results file existed at that commit. It states its relation to D-071: confirms, annotates or supersedes. DESIGN.md's section "CPM engine estimate: what it is (D-071)" names the new entry. If the option is not "keep E unchanged", `cairn/ROADMAP.md` gains a candidate row for shipping it. The row lists each RR27 Q6 item that applies to the chosen option, and gives a reason for each one it omits.
- [ ] AC7: `git diff <default-branch>...HEAD --stat` lists no path under `R/`, `src/`, `man/`, `tests/` or `vignettes/`, and no change to `DESCRIPTION` or `NEWS.md`.

## Coverage

- AC1 → T2, T3
- AC2 → T1, T5, T7
- AC3 → T1, T4, T5, T7
- AC4 → T5, T7
- AC5 → T6, T7
- AC6 → T8
- AC7 → T8

## Tasks

- [x] T1: Write the helpers for E, W, H and the metrics on M167's generators (`devel/m167-interval-oracle.R`). Wrap each `nlminb()` call so that a start that throws is skipped (RR27 beyond-brief 3). Check that H reaches each of RR27's six boundary N = 250 lower points, and that W's starts are the same on every run.
- [x] T2: Write the pre-registration block with the decision rule, and commit it before any full run.
- [x] T3: Write RB28 through `/milestone-brief` on the block and its decision rule. Apply the RR's accepted changes and list them under a corrections heading in the block, before the pilot.
- [x] T4: Run a 10-fit timing pilot per cell on an unloaded machine. Set the replicate count by the block's rule so that the full run fits 12 CPU-hours.
- [ ] T5: Run the full study on the 16-worker machine when the user is plugged in. Commit the results file.
- [ ] T6: Fit the four AC5 cases at their oracle and vignette settings with the block's seeds.
- [ ] T7: Write summary functions that read the results file, and write the record from their output.
- [ ] T8: Apply the decision rule. Write the D-entry, point DESIGN.md at it, and add the ship-step candidate row when AC6 calls for it. Check AC7's diff.

## Work log

- 2026-10-26: created by /milestone-plan from the candidate row "State and settle the CPM engine estimate", part (ii). Part (i) is M169.
- 2026-10-26: tier check: milestone — part (ii) is a design decision about the estimator.
- 2026-10-26: idea vet: one concern, that the row's "a saturating search" cannot be certified, because RR27 Q1 found the search still falling at 150 restarts per centre. "Adopt the lowest" can only mean the lowest found by a fixed bounded search, and a shipped version must be deterministic (IP4). Answered at the question set: a bounded study with a saturation check, where a failed check limits the decision to options that keep E.
- 2026-10-26: question set: estimator study — bounded study with a saturation check (recommended), chosen.
- 2026-10-26: question set: Fable review of the pre-registration before the full run — granted (T3, one RB/RR round).
- 2026-10-26: question set: CPU budget for the full run — about 12 CPU-hours, granted.
- 2026-10-26: question set: coverage — Wald coverage for E, W and H. Bootstrap coverage of an adopted alternative moves to the ship-step row. The user asked to plan without implementing yet, and to run the simulation when plugged in (T5).
- 2026-10-26: lessons used: 2026-10-26 (M167) on theory-centred starts missing Heywood-wall basins. 2026-10-26 (M167) on fixing a stop rule with numeric gates before a fix. 2026-08-16 (M90) on computing figures from the committed construction.
- 2026-10-26: criteria audit (full mode, fresh Opus reader): 10 findings on M170, all accepted. The decision rule may pick W or H only where saturation passes (D-071 Reopens). It is an ordered if/else chain with a default branch and a GP4 tie-break. The commit-order promise moved into the D-entry. Saturation now also uses an independent search family. Only H's shares are lower bounds. The Heywood tolerance (.995), the order rule (after `cpm_canonicalize()`), the unloaded-core timing, the AC4 domain (accepted E fits) and df as fitted are now stated. AC5 fixes each fit's settings and seeds. AC6 lists only the RR27 Q6 items that apply. AC7: no finding.
- 2026-10-26: plan gate chose Wald coverage for the alternatives over bootstrap coverage, because a bootstrap of W or H needs a full search per resample (tens of CPU-hours). Falsified by a decision that adopts W or H, which then needs bootstrap coverage in the ship step.
- 2026-10-26: plan gate chose one study-and-decision milestone with the ship step left as a conditional candidate row over planning the ship step now, because its content depends on which option the rule selects. Falsified by a decision rule whose result is known before the run.
- 2026-10-27: implement started; branch m170-cpm-engine-estimate-study cut from master at 445994d9.
- 2026-10-27: implementation choice: the all-communality-level start sets every scale's communality to .99 at E's angles and weights (the plan left the level open).
- 2026-10-27: implementation choice: W and H replace E only when their refit lies more than 1e-8 below E's F (the engine's reproduction tolerance); removed weights are floored at 1e-4 to place E's estimate in the full layout as a start.
- 2026-10-27: implementation choice: the saturation rule's independent family is uniform configurations (angles uniform on the circle, communalities uniform on [.3, .99], weight logits standard normal), `m170_uniform_starts()`.
- 2026-10-27: T1 done: `devel/m170-engine-estimate-helpers.R` and `devel/m170-engine-estimate-checks.R`, 4 checks pass. At K = 100, H gaps on RR27's six cases 0.81, 0.65, 3.31, 3.05, 1.96, 1.16 T units; W alone gives 0 on replicates 28 and 31, so check 1 can fail. H costs about 5.5 CPU-s per fit at K = 100 (6 workers); replicates 26 and 28 still vary with the seed at K = 100. devtools::test() clean.
- 2026-10-27: T2 done: `devel/m170-engine-estimate.R` with its pre-registration block (K = 100; saturation S1 doubling share <= .02 and S2 400 uniform starts on 50 fits, at most 1 lower; decision rule branches 0 to 6, default the diagnostic, GP4 tie-break toward it; replicate rule R in [100, 500] under 12 CPU-hours with a 1.2 margin). Committed before any pilot or full run; no results file exists at this commit. A smoke run (4 replicates, K = 5) ran end to end and was not kept. The W and H discrepancies are now read after their polish, as E's is.
- 2026-10-27: blocked on RB28 (`cairn/reviews/RB28-cpm-engine-estimate-prereg.md`): Fable review of the pre-registration block and decision rule, nine questions, the ninth on readings the block leaves open; second escalation on the engine estimate, so question 8 weighs not running the study. Committed on the milestone branch, not main, so the brief travels with the block it reviews.
- 2026-10-27: RR28 ingested (Fable, spawned after the gate). Recommendations 1 to 12 applied and 13 applied after its check; 14 and 15 not taken; 16 to 20 rejections accepted (M170-D1 to D3). Block CORRECTIONS lists all 13 changes. Status back to in-progress.
- 2026-10-27: rec 13 check (pre-pilot, M167's seeds, 18 accepted N = 250 fits): H with both random families around g0 (sd .7 and 1.2) reached a lower point than the old design (E at .7, g0 at .7) on 1 fit, by 0.087 T units, and the old design on none; adopted by the rule fixed before the check ("adopt if it is lower on more fits"). No W start was infinite on any of the 18 fits after rec 1.
- 2026-10-27: T3 done: RB28/RR28 round complete, corrections applied before the pilot. Checks: 4 pass (check 2 now asserts all 101 W starts finite on boundary replicate 6, whose E has a communality at exactly 1, and check 3 asserts that fit is in its domain). Smoke run end to end; W now reaches the verbal matrix's lower point (3.158 T units, RR27 3.16); `demo` is now the vignette's sample (T_E 2.229).
- 2026-10-27: verify slot after T3: devtools::test() clean (0 failures).
- 2026-10-27: T4 done: timing pilot at a65a356e (clean tree), one worker, 10 replicates per cell, 14.4 min wall, `devel/m170-engine-estimate-pilot.rds`. Median CPU per replicate 3.6 to 5.5 s, per family run 3.0 to 4.6 s; E 0.04 to 0.07 s, W 0.68 to 1.07 s. Replicate rule: R = 500, projected 8.80 CPU-hours.

## Decisions

- M170-D1 (2026-10-27, RR28): the study runs, amended (RR28 Q8). Not running would leave the question on RR27's uncommitted survey, with no detection rate for any deterministic probe, so no diagnostic could be specified. The amended study costs the same 12 CPU-hours.
- M170-D2 (2026-10-27, RR28): RR28's answers as applied. W gets 101 deterministic starts, adding swaps, swaps with the wall, and wall starts around g0, because the interior-truth lower points are two-scale swaps that single-angle shifts cannot reach (Q1). H is a bounded reference search, never the global minimum. Its random starts are both around g0 (Q2, rec 13). Saturation is expected to fail at N <= 1000 and at interior N = 2000, which leaves branches 3 to 6 (Q3). favors() splits into no_worse in every cell and improves in at least one (Q4). Branch 5 reads a 95% lower bound on W's median gap (Q5, Q7). The default stays the diagnostic, and the tie-break names IP3 beside GP4 (Q5). "Report H" means a later search shown to reach the same point whatever its seed (Q6). W's detection power is reported beside any diagnostic (Q6).
- M170-D3 (2026-10-27, RR28): not taken. Rec 14 would drop the saturation requirement from branch 1 for W. It disagrees with AC1's wording, its practical effect is nil because favors(W) is expected to fail on its own, and amending a criterion for no effect is not worth the amendment. Rec 15 would apply one Wilson allowance to every threshold, but the share thresholds are read far from their bars (RR28 Q5, Q7), so point estimates stay. Rejections accepted: a "keep E" default (16), removing branch 2 (17), more all-communality levels (18), not running the study (19), raising K to chase saturation (20).

## Review
