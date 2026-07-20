# M43: Source notes for the structure criteria and the validity source

- **Status:** review
- **Priority:** normal
- **Depends on:** M40
- **Principles touched:** —
- **Branch/PR:** `m43-reference-notes-structure` · https://github.com/jmgirard/circumplex/pull/69

## Goal

Author source notes for the circumplex-structure criteria `R/fit_structure.R`
implements and for the substantive validity source the SEM vignette cites.

## Scope

**In:** Two source notes, following the page conventions M40 establishes.

- `acton2004.md` — Acton & Revelle (2004), *Evaluation of ten psychometric
  criteria for circumplex structure*, *MPR Online* 9(1), shelf
  `sources/acton2004.pdf`. The four criteria the package actually implements
  (Fisher, Gap, VT2, Rotation), their printed formulas and cutoffs, and the
  deviation-scoring guidance the repo cites at p. 9
  (`R/fit_structure.R:722`).
- `wendt2019.md` — Wendt et al. (2019), *The latent structure of
  interpersonal problems*, *J Abnorm Psychol* 128(8) 823–839, shelf
  `sources/wendt2019.pdf`. The four claims
  `vignettes/sem-based-ssm-analysis.Rmd` makes from it: the latent-structure
  framing (`:44`), the general–agency correlation of roughly −.3 (`:114`),
  RMSEA between .075 and .111 (`:368`), and the three-factor model (`:394`).

**Two discrepancies the pages must carry rather than resolve.**
`R/fit_structure.R:332-333` already records that Acton & Revelle's Eq. 6 **as
printed** uses the communalities h², while their prose describes vector
lengths √h²; the page records the printed form and the repo's resolution
**separately**, so the two stay distinguishable from the page alone. And
`devel/m5-wendt-discrepancies.md` is an existing read-only design record of
paper-internal inconsistencies; T5 reconciles against it and records what
disagrees rather than silently settling it.

The repo's two-channel transcription protocol applies to both pages: a visual
page read and an independent `pdftotext -layout` extraction, diffed on every
load-bearing numeral, with between-channel discrepancies recorded rather than
silently resolved.

**Out:** The fit-index benchmark pair → M41. Browne (1992) and Browne (1982)
→ M42. **Package code changes of any kind**, including any correction to the
Eq. 6 handling — if the note finds one is warranted, that is its own
milestone. Editing the `devel/` transcriptions and design records → after M7
archives (ROADMAP candidate row). Acton & Revelle (2002), shelved as
`acton2002.pdf` — owes no page, cited only as other authors' citation of
prior work (`INDEX.md`, 2026-07-19).

## Acceptance criteria

- [x] `cairn/references/acton2004.md` exists carrying every template section,
      with each implemented criterion's formula and cutoff page-, equation-,
      or table-anchored.
- [x] The page records the Eq. 6 discrepancy **as the paper prints it** and
      records the repo's resolution at `R/fit_structure.R:332-333` separately,
      such that a reader can tell the printed form from the shipped form from
      the page alone.
- [x] The page records the cutoff calibration basis — Acton & Revelle
      calibrated at nv = 64/128 variables (`R/fit_structure.R:233`) — with its
      own anchor, so a reader can see what the shipped cutoffs were
      calibrated at and at what scale count the repo applies them.
- [x] `cairn/references/wendt2019.md` exists carrying every template section,
      with each of the four vignette-cited claims quoted or table-anchored,
      and the reconciliation against `devel/m5-wendt-discrepancies.md`
      recorded — any disagreement stated, never silently resolved.
- [x] Each page's `Extraction:` status is one physical line stating its real
      per-channel standing — never a verification a channel did not perform
      (M40) — and each page's `Traces to` names the specific citing lines
      listed in Scope, verified against the files.
- [x] `INDEX.md` carries one line per new page with the **filename** as link
      text (M40); `cairn_validate` reports `references index<->disk` PASS with
      no `references staleness` WARN; `git diff --stat devel/` is empty, no
      file outside `cairn/` is modified, and each written page's tail bytes
      are checked for leaked tool-call scaffolding (M34).

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T3
- AC4 → T1, T4, T5
- AC5 → T2, T4, T6
- AC6 → T6

## Tasks

- [x] T1. Re-verify `acton2004.pdf` and `wendt2019.pdf` on the shelf and stamp
      the observation — the shelf is live and moved twice during M40, so
      confirm rather than trust this plan's inventory.
- [x] T2. Author `acton2004.md` — the four implemented criteria, their
      formulas and cutoffs, two channels.
- [x] T3. Record the Eq. 6 printed-vs-prose discrepancy and the nv = 64/128
      calibration basis, cross-checked against `R/fit_structure.R:96,233,332-333`.
- [x] T4. Author `wendt2019.md` — the four vignette-cited claims, two
      channels.
- [x] T5. Reconcile against `devel/m5-wendt-discrepancies.md` (read-only) and
      record what disagrees.
- [x] T6. `Traces to` sections written against the actual citing lines;
      `INDEX.md` entries; `cairn_validate` clean; tail-byte and untouched-tree
      checks.

## Work log

- 2026-07-19: created by /milestone-plan, splitting M41 at the re-size gate its own `Out:` clause called for. Carries the two remaining relied-on sources. Both verified present on the shelf at plan time by first-page read — `acton2004.pdf` = *MPR Online* 9(1), `wendt2019.pdf` = *J Abnorm Psychol* 128(8) 823–839 — observed 2026-07-19. Acton & Revelle is the heavier of the two: `R/fit_structure.R` implements four of its criteria and already records one printed-vs-prose discrepancy in its own comments, which AC2 makes the page carry rather than launder. Wendt is lighter — four vignette claims — but comes with an existing read-only design record of paper-internal inconsistencies that T5 must reconcile against.

- 2026-07-19: T1 — both shelf PDFs re-verified independently of the plan's inventory. `acton2004.pdf` = *MPR Online* 9(1), 27 pp., pagination 1:1 (running heads on PDF pp. 3/5/9/13 read 3/5/9/13); `wendt2019.pdf` = *J Abnorm Psychol* 128(8) 823–839, 17 pp., PDF p. n = printed p. 822+n. Neither is an OCR scan (`pdfinfo` Producer: Acrobat Distiller from Word, and Adobe LiveCycle/XPP), so the M42-D1 trap — text layer derived from the page image, corroborating nothing — does not apply and the two channels are genuinely independent here — observed 2026-07-19.
- 2026-07-19: implementation question gate held; all three recommendations taken. (1) `acton2004.md` records A&R's published nv = 64/128 cutoffs alongside the shipped re-derived nv = 8 constants, so the re-derivation's sanity gate stays checkable from the page. (2) RANDALL's sources (Hubert & Arabie 1987; Tracey 1997) back shipped code, are unshelved, and have no page — recorded on the page as a non-attribution and raised as a ROADMAP candidate at review rather than expanding M43. (3) `acton2004.md` reconciles against `devel/ar2004-transcription.md` read-only, symmetric with T5's treatment of Wendt.

- 2026-07-19: T2/T3 — `acton2004.md` authored. Two-channel coverage on every equation (pp. 5–7, 10), every published cutoff (pp. 17, 19), and Table 2 (p. 15); prose anchors rest on the text channel, stated as such. **The text channel returns an empty line for every display equation** (Eq. 6 extracts as nothing), so the equations rest on the image channel — a silent-dropout mode, not a disagreement. Eq. 6 recorded both ways per AC2: printed `X_v = Σ_f φ_fv²` (communality) vs. the p. 6 prose describing vector lengths √h², with the repo's `sd(h)/mean(h)` resolution recorded separately. Gap/VT2/RT verified as implemented exactly as printed — RT's `sum((rl2[,1]-rl2[,2])^2/2)` **is** Eq. 9 at nf = 2. AC3 satisfied and sharpened: the published cutoffs are nv = 64/128 (p. 8 design), and **A&R announce an nv = 8/16/32 follow-up twice (pp. 10, 18) but never report its results**, which is why the repo re-derived; a side-by-side published-vs-shipped table is on the page, with the raw Gap "almost certainly" cutoff moving .01 → .35.
- 2026-07-19: T2/T3 reconciliation against `devel/ar2004-transcription.md` (read-only, no `devel/` file touched): agreement on every point compared, including all nine published-cutoff rows. Its two recorded paper-internal errata independently confirmed (VT2 24.6 in Table 2 vs 24.5 in p. 18 prose; p. 20's MT paragraph saying "an RT value less than .06"). **A third erratum found that the prior record missed:** p. 20 gives MT's deviation-scoring effect as F(1,192) = 1,265.5, η² = .11, but Table 2 gives MT's IS × Dev cell as 1,262.5 (.11) — η² agrees, so the table is coherent and the prose digit is corrupt, plausibly contaminated by RT's genuine 1,265.6 one row up. Immaterial to the package; recorded for completeness.

- 2026-07-19: T4/T5 — `wendt2019.md` authored; all four vignette claims verified against the source and all four hold. RMSEA .075–.111 quoted verbatim from p. 830 and correctly scoped to CFA-PC (the fixed-spacing, fixed-communality model); the four g–agency correlations (−.283/−.292/−.267/−.324, mean −.2915) quoted verbatim from p. 831. The `:394` claim is accurate and if anything **conservative** — "competitive with categorical and hybrid alternatives" understates p. 832's "superior validity … No evidence … for the incremental validity of categorical or hybrid approaches". One qualifier recorded: the "unit-cosine" specificity rests on the online supplement (R Code S25), which is **not on the shelf**, so it is second-hand on this page. The image channel was load-bearing here — `pdftotext` mis-renders this PDF's operators through the font encoding (`=`→`⫽`, `−`→`⫺`, `<`→`⬍`), so every numeral was confirmed against the rendered page.
- 2026-07-19: T5 reconciliation against `devel/m5-wendt-discrepancies.md` (read-only): agreement on everything load-bearing, including its §7 correction, which is confirmed verbatim — the "relaxing the restrictions improves fit without sacrificing validity" claim is Wendt et al.'s **citation of prior work** (p. 829, crediting Acton & Revelle 2002 and Gurtman & Pincus 2000), while their own p. 832 finding is the opposite. **One disagreement recorded rather than settled:** its §1 gives the g–communion range as "−.034 to +.142", but p. 831 prints Sample 4 r = −.115, so the true range is −.115 to +.142 — the record's lower bound is Sample 1's value, not the minimum. Immaterial (nothing uses the communion correlation; §1's argument rests on the agency values, which are right) and not corrected, since `devel/` is out of scope.

- 2026-07-19: T6 — both `INDEX.md` entries added with the **filename** as link text (M40); its owed-a-page ledger note updated to discharge M43's two sources and to record that RANDALL's two sources are relied on but unshelved. ROADMAP candidate row added for those two per the gate decision (search-first sweep found no overlap in candidates, DECISIONS, or the archive). `cairn_validate`: all 15 checks PASS including `references index<->disk` and `coverage complete`, `references staleness` OK with no WARN. Two advisory WARNs both pre-existing and not M43's — the ROADMAP hygiene stamp at 2,568 chars (line 4; replaced rather than appended at review, which clears it) and 47 wrapped work-log lines in M7. `git diff --stat master..HEAD -- devel/` empty; every file touched on the branch is under `cairn/`; tail bytes and a whole-file scan of all three written pages show no leaked tool-call scaffolding (M34).
- 2026-07-19: all tasks complete; status → review. `devtools::test()` under NOT_CRAN=true: 3082 PASS, 0 FAIL, 0 SKIP, 4 pre-existing WARN — the exact M42 baseline, as an empty package diff predicts. No package file, test, or vignette was changed, so the profile's `verify` slot has no code surface to exercise — the milestone is documentation-only by its own `Out:` clause, and `cairn_validate` plus the untouched-tree checks are the applicable gate.

## Decisions

- 2026-07-19: **correction, superseding the T2/T3 entry above.** That entry states in bold that "the text channel returns an empty line for every display equation (Eq. 6 extracts as nothing)". That is **false** and is not edited here because work logs are history (IP4/D-045). `pdftotext -layout` does extract every display equation; it scatters each across several physical output lines, leaving the line that carries the `(6)` marker bare. The implementing probe grepped only that marker line and inferred absence — the same "green because it never looked" failure LESSONS records for M31/M7/M39. Caught by the review's diff-bug lens, reproduced before acting. Consequence is favourable, not adverse: both channels genuinely cover the equations, so the two-channel claim is stronger than the page first said, not weaker. `acton2004.md` corrected in place (it is current knowledge, not history).

## Review

**Reviewed 2026-07-19.** PR #69. Branch cut from and compared against `origin/master`, which had not moved (0 commits behind). Diff: 5 files, +608/−15, every file under `cairn/`.

### Acceptance-criteria evidence (fresh, by command)

- **AC1 — `acton2004.md` template-complete, criteria anchored.** All seven template sections present, exactly one each (grep for `# `, `**Provenance.**`, `**Citation.**`, `**Role.**`, `## Extracted values`, `## Traces to`, `## Open questions`). All four implemented criteria carry an equation *and* page anchor in the "four implemented criteria, as printed" table — Gap Eq. (2) p. 5, Fisher Eq. (6) p. 6, VT2 Eq. (8) p. 7, RT Eq. (9) p. 7 — and every published cutoff carries a page anchor (pp. 17, 19, 20) in the cutoff table.
- **AC2 — Eq. 6 recorded both ways, separately.** The page carries a dedicated section quoting the printed equation (`X_v = Σ_f φ_fv²`, the communality) and the p. 6 prose (vector lengths `√h²`) as separate blocks, then records the repo's shipped resolution separately again. **Amended at review:** the page originally cited only `R/fit_structure.R:95-103` for the repo's resolution, while AC2 names `:332-333`; rather than read the criterion charitably, the page now cites *both* code locations and states that the explanation is duplicated in two places a corrector must keep in sync. `sed -n '332,333p'` confirms those lines carry the printed-vs-prose split verbatim.
- **AC3 — calibration basis anchored.** `sed -n '233p' R/fit_structure.R` returns "Acton & Revelle's published cutoffs were calibrated at nv = 64/128 variables", matching the page's own p. 8 design anchor (the 2 × 2 × 2 × 2 × 3 × 2 × **2 (64 vs 128 variables)** factorial). The page additionally establishes *why* no published nv = 8 value exists — A&R announce the 8/16/32 follow-up at pp. 10 and 18 and never report its results — and carries a published-vs-shipped comparison table. **All 24 shipped constants independently re-verified at review** by reading `circumplex:::structure_cutoffs[["8"]]` from the loaded package (not the source text); every value matches the page's table.
- **AC4 — `wendt2019.md` template-complete, four claims anchored, reconciliation recorded.** All seven template sections present, one each. Each of the four vignette claims is quoted verbatim with a page anchor (RMSEA and fit indices p. 830; g–agency and g–communion correlations p. 831) and summarized in a claims-checked table with a verdict per claim. The reconciliation against `devel/m5-wendt-discrepancies.md` records agreement and **one disagreement stated rather than settled**: its §1 gives the g–communion range as "−.034 to +.142" where p. 831 prints Sample 4 `r = −.115`.
- **AC5 — `Extraction:` status and `Traces to` verified.** `awk` confirms exactly one `Extraction:` line per page, each a single physical line (acton2004.md line 8, 501 chars; wendt2019.md line 8, 448 chars), each carrying a verification verb and an `— observed` date, and each naming what the channels did *not* cover. All 20 distinct `path:LINE` citations across both pages checked in-range by script; the Scope-named anchors spot-verified by content (`R/fit_structure.R:722` = the p. 9 deviation-scoring citation; vignette `:44`, `:114`, `:368`, `:394` = the four Wendt claims).
- **AC6 — index, validation, untouched tree.** Both `INDEX.md` entries use the **filename** as link text (M40's mutation-caught trap). `cairn_validate`: 15/15 checks PASS including `references index<->disk` and `coverage complete`; `references staleness` OK with **no WARN**. `git diff --stat origin/master..HEAD -- devel/` empty. No file outside `cairn/` modified, in the diff or the working tree. Whole-file scaffolding scan of both pages plus `INDEX.md` clean (M34).

### Consistency gate

- `cairn_validate` exit 0; 15/15 PASS. Two advisory WARNs, both pre-existing and neither M43's: the ROADMAP hygiene stamp at 2,568 chars (line 4 — replaced, not appended, in this pass, which clears it) and 47 wrapped work-log lines in M7.
- No `DESIGN.md` principle changed (`Principles touched: —`), so `cairn_impact` is skipped by rule.
- Profile `r-package` `consistency-gate`: `devtools::document()` produces no diff (`git status` on `man/`, `NAMESPACE`, `R/` clean). No generated file hand-edited; no `README.Rmd`, export, or top-level file touched. **No `NEWS.md` entry** — correct, not an omission: M43 changes no package file and has no user-visible surface, and the profile scopes the NEWS requirement to user-visible changes.
- `devtools::test()` (NOT_CRAN=true): 3082 PASS / 0 FAIL / 0 SKIP / 4 pre-existing WARN — the exact M42 baseline, as an empty package diff predicts.
- `devtools::check(args = "--no-manual")`: **0 errors, 0 warnings, 0 notes**, 5m 6s.

### Independent review fan-out

Three fresh-context lenses plus a separate scorer. The `[S]` blame-history lens returned **zero findings** — it independently confirmed nothing was lost in the `INDEX.md` ledger rewrite (byte-diffed against master), that both pages' absence claims survive a *formula* grep rather than a citation grep (the M41 lesson), and that the page's account of the cutoff re-derivation matches `data-raw/structure-test-cutoffs.R` and its commit. The `[S]` prior-PR lens returned **zero findings** and is the known clean no-op here (PRs #66/#67/#68 carry zero review comments; LESSONS records this lens has no evidential weight in this repo). The `[O]` diff-bug lens returned five findings, all independently reproduced by the reviewing session before acting.

**Actioned (scored ≥ 80):**

- **F1 (95) — fixed. A false statement about the page's own evidence.** The page asserted three times that `pdftotext -layout` "silently drops this paper's display equations" and that "Eq. 6 extracts as nothing at all". **This is false.** The command extracts every display equation; `-layout` scatters each across several physical output lines, leaving the line carrying the `(6)` marker bare. The implementing probe grepped only that marker line and inferred absence — the same "green because it never looked" failure LESSONS already records for M31, M7, and M39. Reproduced at review before acting. The correction *strengthens* the provenance: both channels genuinely cover the equations, so the equations are two-channel, not image-only. Page corrected in place (current knowledge); the work-log entry that repeated the claim is superseded by an appended correction, never edited (IP4/D-045).
- **F3 (85) — fixed.** `acton2004.md` carried an undated absence claim about another repo file's state ("Second independent human re-read: pending (Jeff)… carries no human attestation"), which the "Standing facts vs. dated observations" rule requires be stamped. `wendt2019.md` handled the identical situation correctly, so the two pages disagreed on one rule. Now stamped `— observed 2026-07-19`.
- **F5 (88) — fixed.** Interleaved code-span and bold delimiters (`` **`1,262.5** (.11)`** ``) garbled the very numeral the third erratum turns on. Rewritten as `` `1,262.5` with η² `(.11)` ``.
- **F4 (80) — fixed.** The page quoted "strongly recommended in every case" and anchored it to p. 19. That word order is p. 21's (the MT section); p. 19 (VT2) reads "In every case, deviation scoring is strongly recommended." Substance was right, the verbatim string was mis-anchored. Both wordings and both pages now recorded.

**Logged, below threshold:**

- **F2 (68) — fixed anyway, recorded as a deviation.** The `Traces to` list omitted `R/fit_structure.R:332-338` after the review-time AC2 amendment cited it in the prose. The scorer put it at 68 because Scope's literal citing-line list for this page names only `:722`, leaving room to read AC5 as already satisfied. Fixed regardless — the gap was *introduced by this review's own amendment*, the fix is one entry, and leaving the prose insisting two copies be kept in sync while the corrector's map named only one would be self-defeating. Actioning a sub-threshold finding is a deviation from the ≥ 80 rule and is logged here rather than passed off as routine.

No finding required a code change; the milestone remains documentation-only.
