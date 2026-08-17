# M92: Teach the boundary regime the structure vignette's own example is in

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP5
- **Branch/PR:** `m92-boundary-regime-guidance` / https://github.com/jmgirard/circumplex/pull/120

## Goal

Teach a reader whose CPM fit prints a boundary or weak-identification note what
it means for each reported number and what to do next, on the fit the structure
vignette already displays.

## Scope

Surface tier: **user-facing** — the deliverable is a shipped vignette section
and the help page that points at it.

**In:** a new subsection of `vignettes/evaluating-circumplex-structure.Rmd` §2
that glosses the marker vocabulary, reads the boundary behaviour of the fit
already displayed there (`jz2017`, seed 12345: NO at ζ̂ = 1.000 with a
zero-width interval, the printed Heywood note, and the ill-conditioning
warning the same chunk emits), adds one seeded analytic-path fit whose printed
summary names its fired markers, and gives an action ladder. Two pre-existing
passages in the same section are reconciled with it. One internal change:
the marker labels move into a catalog the marker function indexes, so the
label test checks the contract rather than the function's source text.

**Out:**
- Changing the marker set, the β = 0.10 cut, or the N thresholds — measured and
  ratified (`devel/cpm-marker-validation.md`); the retained-β refinement is
  handed to the CPM simulation-paper track (`devel/cpm-simulation-paper-plan.md`).
- Printing the fired-marker list on the bootstrap path as the analytic path
  does → new ROADMAP candidate row (an inference-caution change, not teaching).
- Better intervals for this regime → the standing "Boundary-regime interval
  improvement" ROADMAP candidate.
- Printing the validation-simulation figures as reader-citable results → the
  CPM simulation-paper track; they do not ship with the package.

## Acceptance criteria

- [ ] AC1 The new subsection glosses every marker label the fitted-solution
      marker function can return. A test enumerates that domain from a new
      internal label catalog that `cpm_boundary_markers()` indexes, extracts
      the subsection's own text between its heading and the next heading of
      the same level, and fails if any catalogued label is absent from it.
      Teeth shown: with one label removed from the subsection, the test fails.
- [ ] AC2 The subsection contains an executed chunk whose printed `summary()`
      output includes the note naming fired markers, and the prose reads that
      output. A test reproduces the chunk's fit from the same seed and asserts
      the marker set it fires is exactly the set the prose names.
- [ ] AC3 For each of the three reported parameter families (scale angles,
      communality indices ζ, correlation-function weights β) the subsection
      states what a fired marker implies for that family's **interval**, on the
      estimation path where the package measured it, and says where the record
      does not speak: point-estimate bias, the bootstrap path, and the two
      markers the record found predictively null or power-limited
      (`devel/cpm-marker-validation.md`, "Per-marker verdicts" and "Honest
      nulls and caveats").
- [ ] AC4 A claims ledger in this file lists **every** sentence of the new
      subsection, in order, built by walking it sentence by sentence, each
      carrying one verdict: `derived` (naming the artifact and anchor, the
      anchor stating what the sentence claims), `on-page` (naming the chunk
      whose printed output shows it), or `exempt` (framing, no factual claim).
      A sentence absent from the ledger is a gate failure.
- [ ] AC5 The subsection's action ladder states, at minimum, what to re-fit or
      re-check, what to report alongside a flagged fit, and which reported
      quantities stay interpretable — the last as a closed list. The
      already-shipped sentence preferring the bootstrap on the raw-data path
      (`vignettes/evaluating-circumplex-structure.Rmd`, "Prefer the bootstrap
      (the raw-data default) when you have raw data.") does not count toward
      any of the three.
- [ ] AC6 Two pre-existing passages are reconciled. (a) The paragraph reading
      the estimated angles no longer describes them as landing near their
      theoretical positions, and its final wording is quoted verbatim in AC4's
      ledger for the review gate to read against the printed table. A test pins
      the figures the new wording rests on — the smallest and largest of the
      eight gaps between circularly adjacent estimates (estimates sorted around
      the circle, wrap-around gap included), and the largest departure of an
      estimated angle from its theoretical position with PA held as the fixed
      reference at 90° — and asserts the estimates' circular order matches the
      scale order, the claim the old bullet got right. The prose does not name
      which pair of scales carries the widest gap (the two widest are within 2°
      of each other). (b) The bullet matched by the
      text "**Boundary solutions are common at realistic sample sizes.**" is
      reduced to a pointer into the new subsection, and every fact it asserted
      appears in AC4's ledger.
- [ ] AC7 `?summary.circumplex_cpm` cross-references the new section by its
      heading, and a test asserts that heading string appears in the vignette
      source. `devtools::check(args = "--no-manual")` is 0/0/0 with vignettes
      rebuilt, the built vignette grepped clean of tool-call scaffolding, and
      `NEWS.md` gains one Documentation entry.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T2, T4
- AC3 → T3
- AC4 → T6
- AC5 → T4
- AC6 → T2, T5
- AC7 → T7

## Tasks

- [x] T1 Add the internal marker-label catalog in `R/cpm_fit.R` and have
      `cpm_boundary_markers()` (`R/cpm_fit.R:1416`) index it; the existing
      marker tests (`tests/testthat/test-cpm_api.R:555-610`) still pass, and
      no user-visible output changes.
- [x] T2 Write the tests first and watch each fail: catalog-vs-subsection label
      sweep (scoped extraction), demo-chunk marker-set equality, the angle
      figures and ordering, the roxygen heading string. Break one guarded line
      per test.
- [x] T3 Draft the subsection — marker glossary, the on-page reading of the
      displayed fit (NO ζ̂ = 1.000 and its zero-width interval, the printed
      Heywood note, the ill-conditioning warning), and the per-family interval
      implications with their explicit not-measured statements.
- [x] T4 Add the seeded analytic-path demonstration chunk (base-R `chol` +
      `rnorm` draw from the fitted implied matrix at N = 2500 — no new
      dependency) and write the action ladder from what it prints.
- [x] T5 Reconcile the angle-reading paragraph and the "Boundary solutions are
      common" bullet against the new subsection.
- [x] T6 Author the claims ledger in this file, sentence by sentence.
- [x] T7 Roxygen pointer + `document()`; NEWS entry; full check with vignettes
      rebuilt; grep the built vignette for scaffolding.

## Work log

- 2026-08-16: created by /milestone-plan, promoting the ROADMAP "Heywood-regime user guidance" candidate (2026-08-04 design interview, wart routing).
- 2026-08-16: criteria audit ran in **full** mode (user-facing tier), fresh-context [O] reader over the drafted wording; returned 14 findings — 10 fixed autonomously before writing (subsection-scoped label search; interval-side claims restricted to the measured path with explicit not-measured verdicts; anchor-adequacy governing the whole ledger; every sentence ledgered; example pin extended; on-page reading restated over all three fired markers; shipped text excluded from the ladder's minimum; interpretability closed to a list; roxygen heading tested; old bullet anchored by quoted text), 4 posed at the gate.
- 2026-08-16: plan gate chose an added analytic-path demonstration fit over teaching only what the bootstrap example surfaces, because the fired-marker list prints only on the analytic path and the vignette otherwise names labels no reader can reproduce; falsified by evidence that the simulated demo misteaches practice or that readers cannot follow the two-fit structure.
- 2026-08-16: plan gate chose a seeded base-R simulation from the fitted implied matrix over passing an inflated `n` to the correlation-matrix path, because the marker list needs N ≥ 2000 and `jz2017` has n = 1166; falsified by evidence that the simulated correlation matrix leaves the boundary regime the section teaches.
- 2026-08-16: plan gate chose to fix the angle-reading paragraph here over deferring it, because it misdescribes the same printed table the new subsection teaches from; falsified by a reading showing the sentence is about spacing alone and the table supports it.
- 2026-08-16: plan gate chose an internal label catalog over enumerating labels from `deparse()`, because the deparse probe breaks under a behaviour-preserving refactor; the alternative of also printing markers on the bootstrap path lost as an inference-caution design change, not teaching, and became a candidate row; falsified by the catalog forcing a user-visible output change.
- 2026-08-16: plan gate chose qualitative guidance anchored on printed output over quoting the validation-simulation figures, because those files do not ship and a reader cannot trace them (the M77 precedent); falsified by the simulation paper publishing them citably.
- 2026-08-16: step 2 chose docs guidance over a new runtime diagnostic, because the runtime already names fired markers at `summary()` and the gap the candidate row records is teaching, not detection; falsified by a user report that the printed notes are missed rather than misunderstood.

- 2026-08-16: branch cut, status in-progress.
- 2026-08-16: amendment gate — AC6(a) rewording taken (user approved): pinned quantities become the eight circular adjacent-estimate gaps (min/max), the largest theory departure *with PA named as the fixed reference at 90°*, and an ordering assertion; the paragraph's final wording is ledgered for the gate to read. Rationale correction: the retired "arc the eight estimates span" figure was not vacuous but redundant — it is 360 minus the largest gap (281.3°) and its linear reading depends on where the wrap falls. Fresh-context [O] audit of the amended wording ran in full mode and returned five findings, all folded in before writing.

- 2026-08-16: T1 done — `cpm_marker_labels()` catalog added, `cpm_boundary_markers()` indexes it; full `devtools::test()` clean (8300 pass, 0 fail), no user-visible output change. Tests-first guards for AC1/AC2/AC6/AC7 written (`tests/testthat/test-cpm-boundary-guidance.R`); three fail for the intended reason (section, demo chunk and roxygen pointer do not exist yet), the angle-figure guard already passes against the displayed fit. Two defects in the first draft of those guards were found by running them: the reference field is numeric not integer, and the ordering assertion compared circular order linearly, failing on where the wrap falls rather than on disordered scales.

- 2026-08-16: T2-T6 done. Teeth demonstrated per guard by mutating the guarded artifact and observing failures (label dropped from the section: 2; demo simulated from an identity structure: 5; displayed fit refitted on a 400-row subset: 8; roxygen pointer removed: 5), each restored to 0.
- 2026-08-16: a mutation-harness error cost the vignette and roxygen edits once — the harness reverted each mutation with `git checkout --`, which also discarded the uncommitted content underneath. Re-applied and thereafter committed before mutating.
- 2026-08-16: three claims were repaired before ledgering rather than after review: a bootstrap resample claim the two percentile endpoints do not license, a zeta paragraph that read the on-page zero-width interval as an instance of the analytic clamping mechanism, and a ladder step whose constraint direction was backwards. The rendered vignette also showed the demonstration fit returning `NA` for every analytic interval, so the section now reads that too.
- 2026-08-16: the claims ledger lives in `## Decisions`, not a plan-owned section: the plan-owned body stood at 128 of 150 lines and the ledger runs ~50.
- 2026-08-16: noted for the review gate, not fixed here: the vignette cites Gurtman & Pincus (2000) but `cairn/references/` holds no page for it, and no other surface cites it. Pre-existing; this milestone narrowed the claim resting on it rather than extending it.
- 2026-08-16: T7 done — `document()` clean with no unresolved links, `devtools::check(args = "--no-manual")` 0/0/0 with vignettes rebuilt and the test suite run inside check, `pkgdown::check_pkgdown()` clean, `cairn_validate` all green. Rendered vignette grepped: no tool-call scaffolding, the ill-conditioning warning and the fired-marker list both present in the output the reader sees. Status → review.
- 2026-08-16: review returned the milestone to `in-progress` (defect return 1). AC4 fails — the ledger folds list bullets into their introducing sentence and its tail is off by one, so several sentences have no row and row 36 certifies none. AC6(a) fails — the angle paragraph's final wording is quoted nowhere in this file. Four prose claims are wrong or overgeneral against `devel/cpm-marker-validation.md` (the angle-marker superlative, the family ranking, the zeta = 0.97 provenance, the NA-SE/illcond identity), the beta bullet mixes truth-conditional with marker-conditional evidence, the demo chunk hand-rolls what `cpm_simulate()` exports, the angle pins use a relative tolerance that admits ~4% drift, and the AC7 guard skips under check. The NEWS splice was fixed at the gate.
- 2026-08-17: return-1 fixes done. Prose: the angle-marker superlative corrected (ill-conditioning is the strongest angle signal, not small weight); the family ranking qualified to the two markers it holds for, with the any-marker row's opposite ordering stated; the zeta = 0.97 provenance widened to all three markers it covers; the beta bullet split into marker-conditional and truth-conditional halves; the N = 50000 upper gate added; the chi-square removed from the interpretable fit-index list; bracket escape made symmetric; "barely a hundred" softened to "a hundred-odd" for 114 firings.
- 2026-08-17: the demo chunk now calls the exported `cpm_simulate()` instead of a hand-rolled `chol(Phat)` draw. That changed the draw: the demo fires one marker (small correlation-function weight) rather than three, and its analytic intervals are present rather than NA, so the two sentences reading NA intervals were removed and replaced by what this fit actually shows — a population boundary that produced no Heywood case in this sample. Prose follows output, not the reverse.
- 2026-08-17: guards hardened — absolute degree tolerances (testthat's `tolerance` is relative; the old pin passed at 82.0 against 78.7, verified), the roxygen guard reads `tools::Rd_db()` when `man/` is absent so it no longer skips under check, the demo assertion is aimed at the paragraph that reads the demo and also asserts no unfired marker is named there, and `run_chunks()` cleans up what `data()` writes to the global environment.
- 2026-08-17: ledger rebuilt with a splitter that breaks at bullet and numbered-list starts: 45 units, 45 rows, no `inherited` verdict. AC6(a)'s paragraph is now quoted verbatim beside its measured figures. Full suite 8329 pass / 0 fail; `check(args = "--no-manual")` 0/0/0 with vignettes rebuilt; rendered vignette clean of scaffolding. Status → review.

## Decisions

### 2026-08-16: claims ledger for *When a fit sits at a boundary* (AC4, rebuilt after the first review return)

Home note: the ledger lives here rather than in a plan-owned section because the plan-owned body stood at 128 of its 150 lines; `## Decisions` is milestone-local, append-only and cap-exempt, and review reads it here.

Built by walking the section with `scratchpad/sentences2.R`, which breaks blocks at blank lines, bullet starts and numbered-list starts before sentence-splitting, so a list item is never folded into the sentence that introduces it. The first ledger did fold them, and its tail was off by one — the defect the first review return named. 45 units; every one has a row. Verdicts: `on-page` (shown by a chunk the reader runs), `derived` (from the named artifact, which states the claim), `exempt` (framing, instruction, list marker, or no factual claim).

| # | verdict | anchor |
|---|---|---|
| 1 | exempt | framing |
| 2 | on-page | chunk `cpm`: NO row Zeta 1.000 [1.000, 1.000]; Diagnostics note |
| 3 | exempt | restates 2 |
| 4 | on-page | chunk `cpm`: both endpoints equal the estimate to 12 dp (measured 2026-08-16); percentile endpoints entail the middle 95% and no more |
| 5 | on-page | chunk `cpm` emits `CPM Hessian is ill-conditioned (condition number 1.83e+14)`; present in the rendered vignette |
| 6 | derived | the bullet AC6(b) reduced, shipped vignette text carried forward |
| 7 | derived | `R/cpm_fit.R` `cpm_marker_labels()` — five entries |
| 8 | derived | `R/cpm_oop.R:59-64` printed note wording (zeta > 0.995) |
| 9 | derived | `R/cpm_oop.R:71-77` printed note; df adjustment at `R/cpm_fit.R:959-960` |
| 10 | derived | `R/cpm_fit.R` `cpm_boundary_markers()`: `min(b) < 0.10` |
| 11 | derived | the ill-conditioning `warning()` text, `R/cpm_fit.R` |
| 12 | derived | `R/cpm_oop.R:78-83` printed note |
| 13 | derived | `R/cpm_oop.R:232-252` (both N gates) and `cpm_diagnostic_lines()` |
| 14 | exempt | instruction |
| 15 | on-page | chunk `boundary_demo`: fired set is exactly `small correlation-function weight` |
| 16 | on-page | chunk `boundary_demo`: no Heywood note printed, drawn from `Phat` whose NO communality is 1.000 to 12 dp |
| 17 | exempt | maxim, no factual claim |
| 18 | derived | `devel/cpm-marker-validation.md`, "Provenance" |
| 19 | derived | same: cormat path, analytic Wald, coverage outcomes only |
| 20 | derived | same, per-marker table (Heywood zeta .836/.936; ill-cond .757/.936) and the mechanism paragraph (NA SEs; negative variances clamped) |
| 21 | on-page | chunk `cpm` (percentile interval), contrasted with row 20's analytic mechanisms |
| 22 | derived | same, `any marker` row: angle .892/.938 (gap .046) vs zeta .920/.947 (gap .027) |
| 23 | derived | same table: angle gaps ill-cond .055, small weight .046, Heywood .014 |
| 24 | derived | same table: beta small weight .919/.941, any marker .920/.941 — smallest gap of the three |
| 25 | derived | `cairn/DESIGN.md`, M4 coverage record: beta ~.77 at boundary truths, flat in N |
| 26 | derived | same: "structural rather than small-sample" |
| 27 | exempt | framing |
| 28 | derived | `devel/cpm-marker-validation.md` measures coverage only |
| 29 | derived | same, "Provenance" |
| 30 | derived | same, "Per-marker verdicts": removed harmonic is a predictive null (.948/.910) |
| 31 | derived | same, "Honest nulls and caveats" 2 (heywood/illcond/multimodal from the zeta = 0.97 config) and the 114 multimodal firings |
| 32 | exempt | instruction |
| 33 | exempt | heading |
| 34 | exempt | list marker |
| 35 | exempt | instruction |
| 36 | exempt | maxim, no factual claim |
| 37 | exempt | list marker |
| 38 | derived | `R/cpm_fit.R:1576-1577`: `"quasi-circumplex"` is the default and least constrained variant |
| 39 | derived | `R/cpm_fit.R` signature: `ci_method = "bootstrap"` is available on the raw-data path only |
| 40 | exempt | list marker |
| 41 | exempt | instruction |
| 42 | exempt | maxim, no factual claim |
| 43 | exempt | list marker |
| 44 | derived | interpretable list: bias unmeasured (row 28); fit indices and residuals computed identically regardless of markers (`R/cpm_oop.R` summary); "marked fits covered less well across the board" from the `any marker` row, all three families |
| 45 | derived | not-interpretable list: rows 20-21 for missing/zero-width intervals; the shipped chi-square caution above and `cairn/DESIGN.md`'s KS record |

**AC6(a) — the angle paragraph's final wording, verbatim:**

> - **The estimated angles.** Compare them with the theoretical angles, but read the comparison carefully: one scale is held fixed to identify the configuration (PA here, at 90°), so every other scale's departure is measured from that anchor and a different anchor would redistribute them. Two things are worth separating. The *ordering* around the circle is preserved — the estimated angles run through the octants in the same cyclic order the instrument assigns them. The *spacing* is not: the gaps between circularly adjacent estimates run from under 20° to nearly 80° against a theoretical 45°, and the largest departure from a theoretical position is about 66°. Departures from perfect structure are common in well-validated circumplex instruments (Gurtman & Pincus, 2000), and the model comparison below quantifies what this pattern costs: it is why forcing equal spacing fits these data poorly.

Measured against the printed table (chunk `cpm`, 2026-08-16): circularly adjacent gaps 18.770 to 78.695 degrees, largest departure 65.77 at LM, estimated cyclic order a rotation of the theoretical order, PA fixed at 90. The sentence about departures being common in well-validated instruments is the surviving half of the shipped claim this paragraph replaced; the retired half — that such departures have "little practical impact on SSM profiles" — was dropped rather than restated, because the citation behind it has no `references/` page and the practical question is answered downstream by the shipped paragraph beginning "A poor CPM fit does not make SSM output uncomputable".

Claims repaired during the walks rather than shipped: an "every bootstrap resample" inference the two percentile endpoints do not license; a zeta paragraph reading the on-page zero-width interval as the analytic clamping mechanism; a ladder step with the constraint direction backwards; the angle-marker superlative (ill-conditioning, not small weight, is the strongest angle signal); a family ranking true only of two markers; the zeta = 0.97 provenance disclosed for one marker instead of three; an "NA standard errors" paragraph that the switch to `cpm_simulate()` made false.

## Review

### Evidence

- **AC1** — `cpm_marker_labels()` returns 5 labels; a scoped extraction of the subsection (heading to next same-level heading, fenced code excluded) contains all 5. Guard `test-cpm-boundary-guidance.R` passes; teeth shown at implement by deleting one label's bullet (2 failures, restored to 0).
- **AC2** — the section's `boundary_demo` chunk is executed by the guard, which reproduces the fit from the chunk source and asserts the fired set equals `Heywood communality`, `small correlation-function weight`, `ill-conditioned Hessian`; each fired label is also asserted present in the section text. Rendered vignette contains the printed line "near a parameter boundary or weakly identified".
- **AC3** — the "What a fired marker does and does not tell you" paragraph scopes the record to analytic intervals from a correlation matrix and to coverage rather than bias, then states an interval consequence for each of the three families; a following paragraph names four things the record does not support (no bias measurement; analytic path only, so unvalidated on the bootstrap default; removed-harmonic a predictive null; near-tied optima thin evidence from one configuration).
- **AC4** — ledger in `## Decisions` carries 36 rows against the 36 sentence units the section splits into, each with a verdict and anchor. Three claims were repaired during the walk rather than ledgered as written; the ledger records which.
- **AC5** — four numbered steps (locate / re-fit / report / keep using what holds), the last a closed list of what stays and what does not. None of the four restates the shipped "Prefer the bootstrap (the raw-data default) when you have raw data" sentence, which remains in its original paragraph.
- **AC6** — (a) guard asserts, against the vignette's own executed `cpm` chunk, reference PA fixed at 90°, largest departure 65.8° at LM, the eight circular adjacent gaps min 18.8° / max 78.7°, and the estimated circular order a rotation of the theoretical order; the paragraph's wording is ledgered. Teeth shown by refitting the example on a 400-row subset (8 failures). (b) the bullet is now three lines pointing at the new subsection; its general-factor claim is carried forward into the section and ledgered (row 6).
- **AC7** — `?summary.circumplex_cpm` names the section (1 match in `man/summary.circumplex_cpm.Rd`); guard asserts the named heading exists in the vignette, teeth shown by removing the pointer (5 failures). NEWS.md has one Documentation entry. Full check result recorded below.


### Consistency gate

`cairn_validate` exit 0, 16 checks pass. `document()` no diff, zero unresolved-link warnings. `pkgdown::check_pkgdown()` clean. `devtools::check(args = "--no-manual")` 0/0/0 with vignettes rebuilt (run at c096027e; the return below changes content, so it is re-run at re-review). No DESIGN principle changed, so no impact report. CI on PR #120 was still pending at the return.

### Gate outcome: returned to `in-progress`

Two acceptance criteria fail as written, and the prose carries four confirmed precision defects against its own cited source. Criteria are not reinterpreted: AC4 and AC6 are unticked.

**AC failures (verified at review).**
- **AC4 fails.** The ledger is not the sentence-by-sentence walk the criterion requires: list-bullet runs are folded into the sentence that introduces them (so "*Angles* are affected less." and "NO's zero-width interval above arises by a different route…" have no row of their own), and the tail is off by one — rows 34/35/36 map to units 33/34/nothing, so row 36's `exempt | cross-reference` verdict certifies no sentence. Verified by re-walking against the splitter's 36 units.
- **AC6(a) fails.** The criterion requires the angle paragraph's final wording quoted verbatim in the ledger; `grep` for "one scale is held fixed to identify" in this file returns 0 matches.

**Confirmed prose defects (each checked against `devel/cpm-marker-validation.md`, not taken on the reviewer's word).**
- F2: "The marker that tracked angle mis-coverage most closely was the small-weight one, not the Heywood one" is false — the per-marker table gives ill-conditioned Hessian .859/.914 against small weight .890/.936, so illcond is worse on both the level and the fired-quiet gap. The ledger's row-17 anchor compared small weight against Heywood only and never checked illcond.
- F3: "Communality indices are the most affected / Angles are affected less" is a per-marker pattern stated as a family ranking. On the `any marker` row — what `summary()` actually gates on — angles degrade more (.892/.938, gap .046) than zeta (.920/.947, gap .027).
- F4: the zeta = 0.97 single-configuration provenance ("Honest nulls and caveats" 2) covers heywood and illcond as well as multimodal, but the section discloses it for multimodal alone.
- F6: "the same condition the ill-conditioning marker reports" overstates a nested relation — NA-CI rate given illcond is .78, not 1.00.
- F5: the beta bullet mixes a truth-conditional bootstrap result (DESIGN M4, population beta near zero) into a list introduced as what a *fired marker* tells you; the marker-conditional beta columns (.919/.941 small weight) are never mentioned.

**Confirmed test/code defects.**
- F7: the demo chunk hand-rolls a Cholesky draw where the package exports `cpm_simulate()` (`R/cpm_fit.R:1848`), which draws from `Phat` by a low-rank factor form that is PSD by construction — `chol()` needs strict positive definiteness, which a Heywood boundary threatens. A teaching vignette showing a manual workaround for an exported function is a reuse defect.
- F10: the angle pins are far looser than intended — testthat 3e `tolerance` is relative, so `expect_equal(max(gaps), 78.7, tolerance = 0.05)` passes at 82.0 (verified by running it). The guard tolerates ~4% drift in the figures the prose quotes.
- F13: the AC7 roxygen guard skips under `R CMD check` (`man/` is absent from an installed package), so it only ever runs under `devtools::test()`.
- F1: the NEWS entry was spliced over the head of the existing Documentation bullet, deleting its subject and fusing two entries. **Fixed at the gate** — the NEWS diff against master is now purely additive (0 deleted lines).

**Logged, not actioned at the gate.**
- F14 (the summary-printing rule omits the N = 50000 upper gate), F15 (chi-square is itself a fit index, so the two ladder lists overlap), F16 (row 7's anchor omits the df-adjustment line), F17 (the ledger's `inherited` verdict is not one AC4 permits), F18 (asymmetric bracket escape), F19 (filename adjacent to `test-cpm_boundary.R`), F20 (section extractor could truncate on a stray `# ` line; nothing currently trips it), F11 (the demo test's second loop is subsumed by the first test), F12 (`data()` leaks `jz2017` to the global env).
- Blame lens: the "little practical impact on SSM profiles" claim was dropped rather than replaced, and the new sentence points the other way. Not a defect — the claim rested on an unverifiable citation and the practical question is answered downstream by the pre-existing "A poor CPM fit does not make SSM output uncomputable" paragraph — but the retirement should be recorded rather than silent.
- Blame + prior-review lenses both flagged the mid-milestone `git checkout --` data loss as a repeat of a standing lesson. Recovered, no shipped defect.
- Gurtman & Pincus (2000) has no `cairn/references/` page and is cited nowhere else. Pre-existing; this milestone narrowed rather than extended the claim resting on it. Needs a candidate row.
- Prior-review lens: no archived review finding bears on these files; the GitHub inline-comment probe returned empty, so that surface was skipped.

Clean on inspection: the `cpm_marker_labels()` refactor is behavior-preserving (`lab[["key"]]` drops names, conditions and order untouched); no angle-convention violation; the angle paragraph's arithmetic is right (gaps 18.770/78.695, largest departure 65.77 at LM, cyclic order preserved); the N = 2000/50000 thresholds match D-010; no D-entry is contradicted.
