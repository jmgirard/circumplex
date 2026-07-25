# M57: ΔCFI secondary invariance criterion for `ssm_sem()`

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m57-dcfi-invariance-criterion`

## Goal

Add the Cheung–Rensvold (2002) ΔCFI as a labeled, reported-only secondary invariance criterion in `ssm_sem()`'s ladder — scope-gated to its validated envelope (two-group, plain ML) — and author the source note it owes; Δχ² stays the sole gating verdict statistic.

## Scope

**In:** compute `dcfi` per adjacent ladder rung and surface a scope-gated Cheung–Rensvold flag against the −.01 cutoff, per the operational rule in `devel/cr2002-transcription.md` (not the source's contradicted p. 251 sentence). Author `cairn/references/cheung2002.md` and correct the now-stale "uncited" prose it falsifies. Teach the criterion + full scope caveats in roxygen, NEWS, and the SEM vignette.

**Out:** ΔGamma-hat / ΔMcDonald's NCI (the CR trio's other members; their cutoffs are transcribed but need own wiring — candidate row if wanted). A *validated* flag for robust CFI or >2 groups → would need new simulation CR2002 never ran; this milestone declines to flag there rather than inventing a cutoff. Gating on ΔCFI → rejected by design (Δχ² sole gate; spec §12.2).

## Acceptance criteria

- [ ] AC1: the invariance-ladder table gains a `dcfi` column = CFI(rung) − CFI(previous rung), differenced from the CFI the table already displays (plain `cfi` under ML, `cfi.robust` under MLR); NA for configural (no predecessor) and the strict-tier vacuous metric rung. Value matches a hand-computed difference on a fixture.
- [ ] AC2: a labeled retain/reject flag against −.01 (direction per the transcription: ΔCFI **< −.01 → reject**) prints **only when the fit is two-group AND plain ML**; under a robust estimator or >2 groups the ΔCFI value prints with an explicit "cutoff not validated for this configuration (robust CFI / >2 groups); descriptive only" note and no binary verdict. The Cheung–Rensvold attribution + scope label (`α = .01, two-group ML simulation scope`) always accompanies the value.
- [ ] AC3: ΔCFI is reported-only — `comparable`, the gating verdict string, and the estimation-fit selection are byte-identical with and without the ΔCFI machinery, proven on a fixture where the ΔCFI flag and the Δχ² gate would disagree.
- [ ] AC4: `cairn/references/cheung2002.md` exists (Provenance block copied from an existing page; channel = the 2026-07-07 full-text transcription; `Traces to` the `R/ssm_sem.R` lines that now cite it); `INDEX.md` moves cheung2002 to a committed page; the stale INDEX "no shipped code … cites it" line and the design-doc §12.2 "unexercised offer" framing are corrected (M56 stale-prose lesson).
- [ ] AC5: roxygen (`ssm_sem()`), a NEWS entry (exported print output change), and a short precise SEM-vignette paragraph teach the criterion with its full scope caveats (two groups, ML, normality, Type-I only, not validated for robust indices), attributing the corrected direction to the transcription note; no prose implies validation for robust CFI or >2 groups.
- [ ] AC6: `devtools::test()` clean; `devtools::document()` run; `devtools::check(args = "--no-manual")` clean (0/0/0).

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T2
- AC4 → T3
- AC5 → T4
- AC6 → T5

## Tasks

- [x] T1: tests first (`tests/testthat/test-ssm_sem.R`) — (a) `dcfi` = hand-computed CFI difference + NA rows; (b) two-group ML: ΔCFI ∈ (−.01, 0] retains, < −.01 rejects; (c) scope-gate: MLR fit and a 3-group fit print value + note, no binary flag; (d) reported-only: gate/verdict/fit identical with ΔCFI present vs absent on a disagreement fixture.
- [x] T2: implement `dcfi` in `sem_fit_ladder` ([R/ssm_sem.R:776](R/ssm_sem.R:776)) and the scope-gated flag; wire into the table print ([R/ssm_sem.R:1604](R/ssm_sem.R:1604)). No touch to `comparable`/verdict/`fit_est` selection.
- [x] T3: author `cairn/references/cheung2002.md`; add the inline `# Cheung & Rensvold (2002)` citation at the implementing lines; update `INDEX.md`; correct the stale INDEX + `devel/m5-sem-design.md` §12.2 prose.
- [ ] T4: roxygen on `ssm_sem()` + NEWS.md entry + SEM-vignette paragraph.
- [ ] T5: D-entry (D-027) recording the decision; `devtools::document()`; `devtools::test()`; `devtools::check(args = "--no-manual")`.

## Work log

- 2026-07-24: T1+T2 done, committed together — a tests-first pair cannot be green apart, so both tick at one checkpoint. Minor amendment: the ΔCFI tests live in `tests/testthat/test-ssm_sem_groups.R` (where every ladder test lives), not `test-ssm_sem.R` as T1 wrote — the criterion is a multi-group ladder feature.
- 2026-07-24: T1 fixtures — a second-harmonic-perturbed two-group population whose `eps` sets the population CFI drop while n sets Δχ², so the two criteria can be made to DISAGREE (eps = 0.12, n = 2000/group: ΔCFI −.0022 → "retain", Δχ² p = .0004 → reject). Oracles: invariant (dcfi == diff of the table's own cfi) + live (both rungs refitted outside the ladder, lavaan's own CFI differenced, agreeing to 1e-8) + a deterministic helper pin of the ≥ −.01 boundary.
- 2026-07-24: T2 — `sem_dcfi_cutoff`/`sem_dcfi_flag`/`sem_dcfi_note` added; `sem_fit_ladder` gains `dcfi`/`cr` columns and a `dcfi_scope` record carried into the returned `invariance` list; `print.circumplex_ssm_sem` gains the two columns and the attribution block. Full suite 0 failures / 3220 pass (4 pre-existing CPM-Hessian warnings in test-ci_accuracy.R). Print output rendered and read in all four configurations (in-scope retain, in-scope reject, MLR, 3-group).
- 2026-07-24: T3 — `cairn/references/cheung2002.md` authored. Not carried over from the 2026-07-07 transcription: every anchored page (233, 248–251) was re-read against the born-digital PDF text layer this session (M40 lesson — authoring a page is a fresh extraction). The two channels agree on every value; two typographic diffs recorded (the article prints "–0.01", and the general-criterion sentence spans pp. 250–251). Table 5's ΔCFI 1% column (−.0085 … −.0039, all negative, means ≈ 0) is the decisive internal evidence that the p. 251 sentence has the direction backwards — a stronger direction oracle than the transcription's reasoning alone. No DOI is printed anywhere in the 23-page PDF, so none is asserted.
- 2026-07-24: T3 stale-prose sweep (M56 lesson) grepped the OLD assertion's keywords, not just the touched lines: corrected in place the INDEX "owes no page / unexercised offer" block, `devel/m5-sem-design.md` §6.2's TBT paragraph and §12.2 item 2, and — found by the sweep, beyond AC4's named two — `cairn/references/wendt2019.md`'s "sources are not on the shelf" claim, which was already wrong for cheung2002 and gurtman2003 when written. `cairn/milestones/archive/M41-*.md` carries the same falsified claim and is left byte-untouched (history, IP4); `devel/cr2002-transcription.md` likewise (ROADMAP's byte-untouched-until-M7 constraint). `cairn_validate`: all checks pass, `references index<->disk` PASS.
- 2026-07-23: created by /milestone-plan. Promotes the "ΔCFI secondary invariance criterion" candidate (M41 surface; `devel/m5-sem-design.md` §12.2 item 2; transcription `devel/cr2002-transcription.md` landed 2026-07-07; T4 shipped without it). Q1 (robust-CFI scope) resolved to the scope-gated hybrid; Q2 → docs reach roxygen+NEWS+vignette; Q3 → rides v2.0.0, no M7 gate. No RB tripwire (numeric part is a subtraction; transcription is the value/direction oracle). No scope-supersession D-entry owed — completes M5's already-bundled `ssm_sem`, not a new feature family.

## Decisions

- 2026-07-24 (implement question gate): the ΔCFI surface. (a) The printed ladder gains a `dcfi` column plus, in scope, a `cr` retain/reject column, with one attribution + scope block beneath the table. (b) `invariance$table` stores `cr` as well as `dcfi`, so a programmatic caller gets the criterion applied rather than re-deriving the −.01 cutoff and the scope rule. (c) The scope gate keys on **the statistic actually differenced** — two groups AND no `cfi.robust`/`cfi.scaled` in `fitMeasures()` — not on `estimator == "ML"`. Keying on the statistic means a robust index can never be labeled against a normal-theory cutoff, and it correctly admits `estimator = "ML", se = "robust.huber.white"`, where the CFI is normal-theory (CFI does not read the standard errors).

## Review
