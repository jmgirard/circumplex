# M64: FIML on items for `axes_reliability()` — the estimator-metric question

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m64-axes-reliability-fiml-review` / [PR #90](https://github.com/jmgirard/circumplex/pull/90)

## Goal

Settle under independent Fable review whether `axes_reliability()` can honestly
offer full-information maximum likelihood on item data, and record the verdict,
so the build is either cleared with binding criteria or dropped on stated
grounds.

## Scope

**In:**
- `devel/m64-fiml-probe.R`: the plan-gate probe, seed-pinned and committed, so
  RB12's reviewer and any later build can re-run its four findings.
- RB12 (`/milestone-brief`) on the one question this session cannot settle:
  whether available-case z-standardization puts this model on the correlation
  metric under incomplete item data, given that the *complete*-data fit already
  departs from a unit implied diagonal.
- RR12 ingestion per the brief protocol, every recommendation dispositioned.
- The GO/NO-GO `DECISIONS.md` entry. On GO it also narrowly supersedes D-001
  for this feature and takes up D-026's last deferral — the D-025/D-030/D-031/
  D-032 template — citing the measured deletion cost as the use case D-026
  asked for. No plan-time scope-admission entry is authored: M64 ships no
  feature, and whether the build rides v2.0.0 is the GO entry's to decide.

**Out:**
- Any package code, test, or NEWS entry — docs-only. The build is planned
  post-GO from RR12, as M54 was from RR09; on NO-GO it is dropped at AC5.
- Deciding the build's design. The three positions fixed at this plan gate
  (one-stage FIML through `sem_fit_cfa`; N–B and `sd = "raw"` reported
  unavailable-with-reason; a synthetic evidence bar carrying a non-MCAR cell)
  enter RB12 as positions for review to confirm or overturn, not as settled.
- Block membership on the `circumplex_instrument` class → stays on the
  axes-reliability candidate row. The quasi-circumplex refusal → untouched,
  standing on RR09 §4.

## Acceptance criteria

- [x] **AC1** `devel/m64-fiml-probe.R` reproduces all four plan-gate findings
      from a clean `Rscript` session, seed-pinned, printing each figure RB12
      quotes: the saturated mean structure (24 free intercepts, df unchanged at
      273), the complete-data implied-diagonal departure (max |v − 1| = 0.046),
      one-stage vs two-stage agreement stated comparatively — the routes' ξ1
      differing by under 5% of ξ1's own SE at 2/5/10% per-item MCAR (measured
      0.9 / 3.6 / 2.8%), which is the discriminating bound since materially
      disagreeing routes would sit about one SE apart — and the deletion-cost
      shares (p = 64 → 0.53 / 0.27 / 0.038 complete cases at 1 / 2 / 5%).
- [ ] **AC2** `cairn/reviews/archive/RB12-*.md` is self-contained: it states the
      question, all four AC1 findings with the command producing each, and the
      three verdict questions — the metric, what the reported components mean
      under FIML, and whether the FIML-consistent correlation matrix is the
      right input for the OLS cross-check (`axes_ols_shadow()`) and the
      positive-definiteness refusal. Of the three design positions fixed at
      this gate it states **two** as the milestone's (N–B and `sd = "raw"`
      unavailable-with-reason; the synthetic bar's non-MCAR cell) and puts the
      third — one-stage versus two-stage — as an **open** question, left
      deliberately unanchored, which is why RR12 §4's one-stage answer is
      independent evidence rather than an echo.
- [x] **AC3** `cairn/reviews/archive/RR12-*.md` is committed and ingested per
      `/milestone-brief`'s protocol, with every recommendation carrying a
      disposition (apply / consider / reject) and a reason in this file's
      Decisions section.
- [x] **AC4** A `cairn/DECISIONS.md` entry records GO or NO-GO. On GO it names
      the binding criteria the build ingests verbatim, supersedes D-001 narrowly
      for this feature, and takes up D-026's FIML deferral citing the measured
      use case. On NO-GO it records the refusal and its rationale.
- [x] **AC5** `cairn/ROADMAP.md` reflects the verdict: on GO a build candidate
      row naming `Driving RR: RR12`; on NO-GO the FIML item retired with a
      pointer to AC4's entry. Either way FIML leaves the axes-reliability
      extensions row with its lineage noted.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T4

## Tasks

- [x] **T1** — Lift the three plan-gate scratchpad probes into
      `devel/m64-fiml-probe.R`: one section per finding, seed-pinned, each
      printing the figure AC1 quotes. Fix probe 1's dead `pe$free` filter
      (`parameterEstimates()` has no `free` column, so the intercept count read
      0 — the LESSONS (f) family) by counting `coef()` names.
- [x] **T2** — Author RB12 via `/milestone-brief` from T1's output, carrying the
      four findings, the three fixed positions, and the verdict questions in
      AC2. Note for the brief: `ssm_sem()` already exports
      `missing = c("listwise", "fiml")` (`R/ssm_sem.R:1303`), so the argument
      spelling is precedent, not a proposal.
- [x] **T3** — Ingest RR12: disposition every recommendation, and record the
      binding criteria verbatim for the build to ingest.
- [x] **T4** — Author the GO/NO-GO D-entry; update the ROADMAP per AC5 and
      graduate FIML off the axes-reliability extensions row.

## Work log

- 2026-07-26: created by /milestone-plan. Jeff chose escalation over a direct build at the gate, so this milestone produces the verdict and the build is planned from RR12 (the M53→M54 shape). D-026's FIML clause is a *deferral*, not a rejection (tracking-rules: "not now" is a ROADMAP fact), so planning this needs no supersession; the scope-admission + deferral takeup belong to the GO entry at T4. Investigation dissolved two of the candidate row's three worries: lavaan frees all 24 item intercepts itself under `missing = "ml"` (npar 27→51, df unchanged at 273), so the mean structure is saturated and imposes nothing; and the unit-variance concern is not FIML-specific — the complete-data fit's implied per-item variances already depart from 1 by up to 4.6%, against 4.8–6.5% under FIML, with the reported components still summing to 0.9994. What survives is the metric question RB12 asks.
- 2026-07-26: started (/milestone-implement). Branch `m64-axes-reliability-fiml-review` cut from master at `ec420791`; no dependencies to check. Status planned→in-progress. No pre-implementation question gate: the plan fixed the three design positions as RB12 inputs, and the Fable spawn is gated per-instance inside /milestone-brief at T2.
- 2026-07-26: T1 done. `devel/m64-fiml-probe.R` reproduces all four findings seed-pinned — F1 deletion shares, F1b the shipped refusal, F2 the saturated mean structure, F3 the implied-diagonal departure, F4 route agreement. Fixed the scratchpad probe's dead `pe$free` filter by counting `coef()` names instead: `parameterEstimates()` has no `free` column, so the intercept count printed 0 where the true count is 24 (LESSONS (f), the probe's own syntax lying). The committed script prints 12 complete cases at 15% MCAR where the scratchpad printed 16 — a different missingness seed, and no criterion quotes that figure.
- 2026-07-26: AC1 AMENDED (gated) — its one-vs-two-stage agreement bound read 5e-4 and the committed script measures 6.2e-4, so the criterion would have failed review as written. Replaced with a comparative bound at Jeff's gate choice: the routes' ξ1 differ by under 5% of ξ1's own SE (measured 0.9 / 3.6 / 2.8%), ~14× headroom to the one-SE disagreement it must rule out. The M59/M61 tolerance lesson recurring inside a plan that had cited it.

- 2026-07-26: status in-progress→**blocked** on RB12 (`cairn/reviews/RB12-axes-reliability-fiml-metric.md`), nine questions: the metric under FIML, the complete-data implied-diagonal departure, the saturated mean structure's effect on ξ1's SE, one-stage vs two-stage, the OLS-shadow/PD-refusal input, N–B and `sd = "raw"`, the FIML refusal set and the `"Complete N:"` label, the evidence bar, GO/NO-GO. Committed on the milestone branch rather than master: M64's whole deliverable is this RB/RR pair, so splitting it off would fragment the branch T1 already sits on.

- 2026-07-26: T2+T3 done. RB12 spawned to Fable at Jeff's gate; RR12 returned **GO under BC1–BC16** and is ingested — eight decisions recorded above, every recommendation triaged, pair archived. The review overturned the plan's standardization (available-case `scale()` is MAR-dishonest, measured ≈1 SE) while confirming one-stage FIML, closed question 2 without a spin-out, and strengthened the N–B ruling. One RR12 evidence note corrected against an independent re-run (M64-D6). Status blocked→in-progress.

- 2026-07-26: minor amendment — AC2/AC3 now name `cairn/reviews/archive/`, since the brief protocol archives the RB/RR pair at ingestion and the plan-time paths would have pointed review at files that no longer sit there. No change to what either criterion verifies.

- 2026-07-26: T4 done. **D-033 = GO** appended to `cairn/DECISIONS.md`: narrow D-001 supersession, takes up D-026's last deferral citing the measured deletion cost, carries the metric holding (saturated-FIML standardization, never available-case `scale()`), and fixes BC1–BC16 as binding on the build. ROADMAP gains the build candidate row (`Driving RR: RR12`, with M64-D8's replicate-cost note) and a post-build follow-ons row (planned-missingness designs; lavaan `missing = "two.stage"`); the extensions row's FIML lineage now records the GO and that D-026's deferral list is empty. M7 deliberately does not gain a dependency, on D-030's reading.

- 2026-07-26: status in-progress→review (/milestone-implement). T1–T4 done; the branch is four commits over five files and touches **no package surface** (`R/`, `tests/`, `man/`, `vignettes/`, `NAMESPACE`, `DESCRIPTION` and `NEWS.md` all untouched), so the profile's verify slot has nothing to run and there is no NEWS entry to owe — M64 is docs-only by design and the user-visible change ships with the build.

- 2026-07-26: status review→**in-progress** (/milestone-review). **AC2 fails as written**: RB12 states two of the three design positions the Scope fixed — Q6 and Q8 name theirs as the milestone's, but one-stage FIML appears nowhere as a fixed position, Q4 having asked the route openly. RB12 is archived history (IP4), so the criterion cannot be satisfied by editing it and is not reinterpreted; it needs a gated amendment. AC1/AC3/AC4/AC5 verified and ticked. Gate otherwise clean: `cairn_validate` exit 0 (15 PASS), `check(manual = TRUE)` Status OK 0/0/0 with both step lines verified by name, `document()` no diff, pkgdown clean. Four findings actioned (F6 87, F7 85, F2 82, F1 80), three fixed anyway below the bar (F4 63, F5 78, F3 68), one logged (F8 65). First return; thrash count 1 of 3.

- 2026-07-26: review findings applied. F1: the available-case `scale()` check is added to `devel/m64-fiml-probe.R` as a new F5 section, which makes RB12's claim auditable — though the measurement shows RB12's quoted `|mean| <= 6e-17` is itself seed-specific and does not reproduce (7.76e-17 / 7.62e-17 / 7.9e-17 at 2/5/10%; its SD bound does reproduce), the M59/M61 lesson recurring for the second time in this milestone. F3/F4/F5 corrected by appended **D-034** (annotates D-033, changes no decision) and locally by M64-D9; the ROADMAP build row's `sd = "raw"` contract corrected in place. F8 left standing under IP4, recorded in D-034.
- 2026-07-26: corrections to two earlier work-log lines in this file, appended rather than edited (D-045/IP4 — work logs are history). **Review F6:** the 2026-07-26 status→review line says "four commits over five files"; the branch was **six** files at that point (`DECISIONS.md`, `ROADMAP.md`, this file, RB12, RR12, `devel/m64-fiml-probe.R`) — the same commit that wrote the line is what added the sixth. **Review F7:** the AC1 amendment line says "~14× headroom"; the correct figures are **20×** for the 5%-of-an-SE bound and **~28×** for the largest measurement (3.6%), against the one-SE disagreement the bound rules out. No arithmetic on the stated numbers yields 14×, and the amendment's justification is stronger than it claimed, not weaker.

- 2026-07-26: **AC2 AMENDED (gated)** — the criterion required RB12 to state all three design positions fixed at this gate and RB12 states two, which failed review (F2). Amended at Jeff's gate choice (option A of three) to describe the artifact accurately: two positions stated as the milestone's, the one-stage/two-stage question left deliberately open, and the reason that framing is a property rather than a gap — RR12 §4's one-stage answer is independent evidence, not an echo of ours. RB12 is archived history so the artifact was never a candidate for editing. Second gated amendment in this milestone (AC1 at T1, AC2 here); both were criteria written more tightly than the evidence supported.
- 2026-07-26: status in-progress→review (/milestone-implement), second pass. All eight review findings dispositioned: F1 fixed in the probe, F3/F4/F5 by D-034 + M64-D9 + the in-place ROADMAP correction, F6/F7 by appended work-log corrections, F2 by this AC2 amendment, F8 left standing under IP4 and recorded. Still no package surface touched.

## Decisions

- **M64-D1 (2026-07-26, RR12 §1) — the metric is FIML, not available-case.** RR12 confirms one-stage FIML and **overturns the standardization** the plan submitted with it: available-case `scale()` is MCAR-honest but MAR-dishonest, because the standardized columns carry `k_i·k_j·ρ_ij` and the model has no free off-diagonal per-item parameter to absorb it. Measured under mechanism M2: +0.0167 above the FIML-metric estimate (paired SE 0.0006), ≈1 SE at N = 600, while the two metric-correct routes agree to +0.0008. The build standardizes by saturated-FIML (EM) moments with a `sqrt(N_used/(N_used − 1))` convention that reproduces `scale()` exactly on complete data.
- **M64-D2 (2026-07-26, RR12 §4) — one-stage only.** Two-stage R̂ is internal machinery; its SEs and χ² never surface. `sample.nobs = N_total` overstates information, and no scalar effective N repairs it (the loss is parameter- and pattern-specific). F4's near-equal SEs are an artifact of mild missingness and must not be cited as validating the convention (RR12 B-2).
- **M64-D3 (2026-07-26, RR12 §2) — the complete-data diagonal departure is not a defect.** Expected restricted-ML behavior, verified at the stationarity condition (weighted diagonal 8.0e-07 against a raw 0.0448) and vanishing on the population (2.95e-13, ξ1 recovered exactly). No correction milestone; M64's question 2 is closed rather than spun out.
- **M64-D4 (2026-07-26, RR12 §6) — N–B and `sd = "raw"` stay unavailable-with-reason,** and the brief's premise is corrected in the strengthening direction: `cronbach_alpha()` runs on **raw** scores, so an R̂ reconstruction would silently swap covariance alpha for standardized alpha under the same column name — the swap RR09 rec. 4 exists to prevent.
- **M64-D5 (2026-07-26) — BC1–BC16 bind the BUILD, not M64.** M64 ships no code, so its header keeps `Driving RR: —`; the archived RR12 `## Binding criteria` section is the single authority, and the build sets `Driving RR: RR12` and ingests all sixteen verbatim. T3's "record for the build to ingest" is discharged by that pointer, not by copying the criteria here — a second copy is a drift vector, and `cairn_validate`'s string-compare exists to keep one.
- **M64-D6 (2026-07-26) — correction to RR12's V-F evidence note, verified independently.** RR12 says lavaan fabricates a never-jointly-observed moment "silently … no error and no warning". It does fabricate it (independently reproduced: r(1,4) = 0 against a population 0.3475), but lavaan 0.6.21 **does** warn ("some pairwise combinations have zero coverage … use `lavInspect(fit, \"coverage\")`"). The finding stands and BC7(iii) is unchanged, because `axes_reliability()` fits inside `suppressWarnings()` (`R/axes_reliability.R:1069`), making it silent *in this function*. Two consequences for the build: use `lavInspect(fit, "coverage")` as the coverage source for BC7(iii)/BC8 rather than hand-rolling one, and do not restate RR12's "no warning" wording. RR12 itself is history and stays unedited (IP4).
- **M64-D7 (2026-07-26) — recommendation triage.** Apply: recs 1–7 (all sixteen BCs, via the build). Consider, each routed: fold the M1/M2 reviewer probes into a committed probe → the build, whose BC11/BC12 tests must implement them anyway (a devel/ duplicate would be a second record; RR12 states both mechanisms with seeds inline, so its evidence is already reproducible); a soft minimum-coverage warning threshold → build's discretion, BC8 already binds *reporting* it; one doc sentence on the finite-sample diagonal departure → build's discretion; planned-missingness support (B-4) and lavaan `missing = "two.stage"` → one candidate row at T4. Reject: all four of rec. 9's rejections stand as RR12 states them, and none contradicts a standing entry — D-026's equal-errors rejection generalizes to the determined-errors constraint, and RR09 BC13 is upheld (R̂ is a saturated FIML estimate, not a pairwise matrix).
- **M64-D8 (2026-07-26) — cost flagged for the build, not a deviation.** BC10 and BC13 ask ≥ 200 replicates per cell, i.e. several hundred FIML fits on 24 items. That will not fit an ordinary `devtools::test()` run; the build must solve it (a `devel/` oracle run with committed results, or `skip_on_cran`), and raise a "Deviations from RR12" row only if it cannot meet the replicate count at all.

- **M64-D9 (2026-07-26, M64 review) — corrections to M64-D1 and M64-D4; see D-034.** M64-D1's "overturns the standardization the plan submitted with it" is wrong (review F3): the Scope named that standardization as the OPEN question, and all three positions this milestone actually fixed were confirmed by RR12 §4/§6/§8. Read M64-D1 as "RR12 answered M64's open question and thereby ruled out the mechanism the shipped path uses". M64-D4's "unavailable-with-reason" wrongly covers both withheld quantities (review F4): BC9 makes `nb_reliability` an NA-with-reason and `sd = "raw"` a hard informative error. Both entries stay as written — the Decisions section is append-only — and D-034 carries the full corrections plus the two items IP4 leaves standing in RB12 (its non-reproducible mean bound, its non-verbatim transcript).

## Review

Reviewed 2026-07-26. PR [#90](https://github.com/jmgirard/circumplex/pull/90).
**Outcome: RETURNED to `in-progress` — AC2 fails as written.** First return for
this milestone (thrash count 1 of 3).

### Acceptance-criterion evidence

- **[x] AC1 — the probe reproduces all four named figures.** Fresh
  `Rscript devel/m64-fiml-probe.R` from a clean session: F2 prints
  `npar 27 df 273 free intercepts 0` (listwise) against
  `npar 51 df 273 free intercepts 24` (FIML) — the saturated mean structure;
  F3 prints `max |v - 1| = 0.0456` on complete data → the criterion's 0.046;
  F4 prints route agreement 0.9% / 3.6% / 2.8% of ξ1's SE at 2/5/10% MCAR, all
  under the amended 5% bound; F1 prints `p = 64 items 0.526 0.274 0.038` at
  1/2/5%. Deterministic — the [O] lens ran it twice byte-identical, and the
  seeds are exact integers. **Scoped correctly:** AC1's colon enumerates
  exactly these four, so RB12's separate `|mean| ≤ 6e-17` figure — which the
  script does *not* compute (finding F1) — is outside this criterion. The
  scorer flagged the diff lens's wider reading and is right.
- **[ ] AC2 — FAILS. RB12 states two of the three fixed design positions.**
  Verified by literal grep: RB12 states position 2 as the milestone's ("The
  milestone's position is to report both as unavailable-with-reason", Q6) and
  position 3 ("The milestone's synthetic bar is…", Q8), but nowhere states
  position 1 (one-stage FIML through `sem_fit_cfa`) as a fixed position — its
  Q4 asks "which route is the defensible default?" openly, and `one-stage`
  occurs in RB12 only in the pasted probe table, its legend, and a
  hypothetical clause. Found independently by this review and by the [O] lens
  (F2, scored 82). The criterion's other clauses hold: RB12 states the
  question, pastes all four AC1 findings with `Rscript devel/m64-fiml-probe.R`
  as the producing command, and its Q1/Q2/Q5/Q9 carry all three
  verdict-must-decide items. **Not reinterpreted and not patchable
  review-side:** RB12 now sits in `cairn/reviews/archive/`, which IP4 forbids
  editing, so the criterion must be amended through
  `/milestone-implement` step 6 and re-reviewed.
- **[x] AC3 — RR12 committed and ingested.** `git ls-files` confirms
  `cairn/reviews/archive/RR12-axes-reliability-fiml-metric.md` tracked; 9
  numbered recommendations, dispositioned 7 Apply / 1 Consider / 1 Reject in
  RR12 and each routed in M64-D7 (recs 1–7 apply via the build; all five of
  rec. 8's items individually routed; all four of rec. 9's rejections
  recorded). 8 milestone-local Decisions entries; 16 BCs located.
- **[x] AC4 — D-033 records GO with all four required elements.** Present by
  literal grep: `Decision: GO`; the narrow supersession clause ("insofar as
  it"); `BC1–BC16` named as binding with `Driving RR: RR12`; D-026's deferral
  taken up citing the measured use case (the verbatim refusal string
  `(12) must exceed the number of items (24)`); and `BC13 is upheld, not
  superseded`. Ticked against the elements the criterion names. **Three
  factual defects inside the same entry are actioned separately** (F3, F4, F5)
  and require an appended correcting entry, since D-033 is append-only
  history.
- **[x] AC5 — ROADMAP reflects the verdict.** Build candidate row present
  naming `Driving RR: RR12`; the post-build follow-ons row present; the
  extensions row's FIML lineage records `M64` and `D-033 = GO`; M64's status
  row reads `review`. File at 35 lines against a <60 cap.

### Consistency gate

**Universal:** `cairn_validate` exit 0 — all 15 PASS checks green, including
`coverage complete`, `binding criteria`, `roadmap<->disk orphans` and
`weight caps`. 47 advisory `work-log format` warnings, every one on M7's
pre-existing hard-wrapped history, which IP4 forbids editing. No `DESIGN.md`
principle touched (this repo carries no IP/GP block), so `cairn_impact` is a
clean skip.

**Toolchain (`r-package` profile):** `devtools::check(manual = TRUE)`
**Status OK, 0 errors / 0 warnings / 0 notes**, with
`checking PDF version of manual ... OK` and
`checking re-building of vignette outputs ... OK` both verified present **by
name** rather than inferred from the summary line (the M7/M57 lesson).
`devtools::document()` produces no diff — `man/`, `NAMESPACE` and both
`RcppExports` files clean. `pkgdown::check_pkgdown()`: no problems found.
README untouched and in sync. No new top-level files. **No NEWS entry is
owed:** the branch modifies no package surface at all —
`git diff --name-only origin/master..HEAD` matches nothing under `R/`,
`tests/`, `man/`, `src/`, `data/`, `vignettes/`, `NAMESPACE`, `DESCRIPTION` or
`NEWS.md` — so the profile's `verify` slot had nothing to run and the
user-visible change ships with the build.

### Independent review — three lenses

- **[O] diff-bug (Opus):** 8 findings, listed below. Also independently
  re-derived RR12's V-B identities (ξ1 diff 5.55e-17 against RR12's 5.6e-17;
  `lavCor` vs `cor` 8.88e-16 against 8.9e-16), confirmed every D-033 and
  ROADMAP figure against its source, and confirmed the probe measures what
  each comment claims with no lying filter, pattern, or range.
- **[S] blame-history (Sonnet):** no silent undoing, no unacknowledged
  contradiction. Verified D-033's supersession is genuinely narrow on the
  D-008/D-018/D-025/D-030/D-031/D-032 template, that RR09 BC13 is upheld
  rather than superseded, and that the rewritten candidate row dropped
  nothing — every element of the old row survives or was deliberately
  graduated.
- **[S] prior-PR-comments (Sonnet):** **no prior-review evidence.** Read every
  `## Review` record in M53, M54, M59–M63 and all of `LESSONS.md`; the gated
  GitHub probe returned `[]`, so the PR-thread walk was correctly skipped.
  Found no regression of any prior finding, and noted the diff explicitly
  applies three recorded lessons (the M57/M59 probe-syntax family at T1, the
  M59/M61 tolerance family at AC1's amendment, the M53/M63 pointer-not-copy
  discipline at M64-D5).

### Findings actioned (score ≥ 80)

- **F6 (87) — the closing work-log line undercounts the branch.** It reads
  "four commits over five files"; `git diff --stat origin/master..HEAD` shows
  six (`DECISIONS.md`, `ROADMAP.md`, the milestone file, RB12, RR12, the probe
  script). The line was written in the T4 commit, which is itself what added
  the sixth. Fix by appending a correcting work-log line — work logs are
  history.
- **F7 (85) — the gated amendment's justification misstates its own margin.**
  "~14× headroom" follows from neither figure in the sentence: a 5%-of-an-SE
  bound is 20× headroom to the one-SE disagreement it rules out, and the
  largest measurement (3.6%) is ~28×. A later audit of why AC1 was relaxed
  cannot reconstruct 14×. Fix by appended correction.
- **F2 (82) — AC2 fails as written.** See the AC2 evidence line. Fix by a
  gated AC2 amendment at `/milestone-implement` step 6; RB12 is archived
  history and cannot be edited to satisfy it.
- **F1 (80) — RB12 asserts the probe reproduces a figure the probe never
  computes.** RB12 claims it "reproduces every figure quoted in this brief"
  and quotes "|mean| ≤ 6e-17, |SD − 1| ≤ 9e-16" for available-case `scale()`;
  that check was in a plan-gate scratchpad probe and did not survive
  consolidation into the committed script. Does **not** fail AC1 (see above).
  Fix by adding the check to `devel/m64-fiml-probe.R`, which makes the
  archived claim true rather than editing history.

### Findings fixed anyway below the actioned bar, with reason

Three findings scored under 80 but sit in durable records a future build
session reads first, and each costs one sentence to correct — the M62
precedent for fixing below the bar rather than logging and leaving a trap.

- **F4 (63) — `sd = "raw"` is softened from a hard error to
  "unavailable-with-reason".** RR12's §6 Ruling and BC9 set two *different*
  contracts: `nb_reliability` becomes NA with an accumulated reason, while
  `sd = "raw"` must be "refused with an informative error". M64-D4 and the
  ROADMAP build row lump them. A planner reading the ROADMAP row rather than
  BC9 implements a silent NA, which BC9's verbatim diff then flags as a
  deviation. The ROADMAP is current knowledge and is corrected in place; the
  milestone-local entry takes an appended correction. (RR12's own §6 bold
  summary uses the loose phrase, so the drift is inherited — but the ruling,
  not the summary, is what binds.)
- **F5 (78) — wrong line cited for the vignette quote.** D-033 attributes
  "address the missingness before interpreting the estimate" to
  `vignettes/axes-reliability.Rmd:156`; the text is on 157. It points a build
  session at the wrong line of the very paragraph BC16 requires rewriting.
  The M43/M57 off-by-one anchor family, recurring.
- **F3 (68) — the durable record claims a reversal that did not happen.**
  D-033 says the metric holding "overturns the position M64 put to review" and
  M64-D1 says it "overturns the standardization the plan submitted with it".
  But M64's own Scope names available-case standardization as "the one
  question this session cannot settle" — an open question, not a position —
  and RR12 §1 answers it rather than reversing anything. Meanwhile all three
  positions M64 *did* fix were confirmed (RR12 §4, §6, §8; §8 augmented the
  bar rather than overturning it). A future session auditing which of M64's
  positions survived review gets exactly the wrong answer. Corrected by the
  appended entry.

### Findings logged, not actioned (score < 80)

- **F8 (65) — RB12's pasted probe transcript is not verbatim.** It omits the
  `axes_reliability(): 600 complete case(s) used.` message the script emits
  inside F3, while including the analogous F1b message. Not actioned: RB12 is
  archived history and IP4 forbids editing it, no cited figure is affected,
  and the appended correction records the discrepancy for anyone diffing the
  block against a fresh run.
