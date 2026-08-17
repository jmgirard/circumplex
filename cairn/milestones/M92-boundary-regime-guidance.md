# M92: Teach the boundary regime the structure vignette's own example is in

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP5
- **Branch/PR:** `m92-boundary-regime-guidance`

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
- [ ] T7 Roxygen pointer + `document()`; NEWS entry; full check with vignettes
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

## Decisions

### 2026-08-16: claims ledger for *When a fit sits at a boundary* (AC4)

Home note: the ledger lives here rather than in a plan-owned section because
the plan-owned body stood at 128 of its 150 lines; `## Decisions` is
milestone-local, append-only and cap-exempt, and review reads it here.

Built by walking the subsection's prose sentence by sentence with
`scratchpad/sentences.R` (splits the section's non-chunk lines on sentence
boundaries; 36 units, list-bullet runs counted with the sentence that
introduces them). Verdicts: `on-page` = shown by a chunk the reader runs;
`derived` = from the named artifact, which states the claim; `inherited` =
carried forward from the bullet AC6(b) reduced, not newly composed;
`exempt` = framing, instruction, or no factual claim.

| # | verdict | anchor |
|---|---|---|
| 1 | exempt | framing |
| 2 | on-page | chunk `cpm` output: NO row, Zeta 1.000 [1.000, 1.000]; Diagnostics note |
| 3 | exempt | restates 2 |
| 4 | on-page | chunk `cpm`: both endpoints equal the estimate to 12 dp (measured 2026-08-16), which entails the middle 95% of retained resamples sat there |
| 5 | on-page | chunk `cpm` emits `CPM Hessian is ill-conditioned (condition number 1.83e+14)` |
| 6 | inherited | the reduced bullet's general-factor claim, shipped text |
| 7 | derived | `R/cpm_fit.R` `cpm_marker_labels()` / `cpm_boundary_markers()`; gloss wording checked against the printed notes in `R/cpm_oop.R:50-97` and the ill-conditioning `warning()` |
| 8 | derived | `R/cpm_oop.R:232-252` (analytic branch, N thresholds) and `cpm_diagnostic_lines()` (bootstrap path prints notes, not the list) |
| 9 | exempt | instruction |
| 10 | on-page | chunk `boundary_demo` draws from `cpm$matrices$Phat`, whose NO communality is 1.000 to 12 dp |
| 11 | exempt | scope disclaimer |
| 12 | on-page | chunk `boundary_demo` output: every Angle_lci/uci and Zeta_lci/uci is `NA` |
| 13 | derived | `devel/cpm-marker-validation.md`: SEs come back NA exactly when `solve()` rejects the finite-difference Hessian as computationally singular |
| 14 | derived | `devel/cpm-marker-validation.md`, "Provenance" and "Headline results" |
| 15 | derived | same, "Per-marker conditional coverage" mechanism paragraph (NA SEs from singular Hessian; negative variances clamped to zero) |
| 16 | derived | same, per-marker table: Heywood zeta .836 fired / .936 quiet; ill-cond .757 / .936 |
| 17 | derived | same table (small weight angle .890 / .936 vs Heywood .899 / .913); beta half from `cairn/DESIGN.md`, M4 coverage record (~.77, flat in N at boundary truths) |
| 18 | derived | `cairn/DESIGN.md`, M4 record: "structural rather than small-sample" |
| 19 | exempt | framing |
| 20 | derived | `devel/cpm-marker-validation.md` measures coverage only; no bias outcome |
| 21 | derived | same, "Provenance": all fits on the literal `cormat` path, analytic Wald |
| 22 | derived | same, "Per-marker verdicts": removed harmonic is a predictive null |
| 23 | derived | same, "Honest nulls and caveats" 2, and 114 firings all in the zeta = 0.97 config |
| 24 | exempt | heading + list marker |
| 25 | exempt | instruction |
| 26 | exempt | maxim, no factual claim |
| 27 | exempt | list marker |
| 28 | derived | `R/cpm_fit.R:1576-1577` — `"quasi-circumplex"` is the default and least constrained of the four variants |
| 29 | exempt | instruction (correlation-matrix path has analytic intervals only, already stated in the shipped paragraph above) |
| 30 | exempt | list marker |
| 31 | exempt | instruction |
| 32 | exempt | maxim, no factual claim |
| 33 | exempt | list marker |
| 34 | derived | interpretable list: point estimates unmeasured for bias (row 20); fit indices and residuals unchanged by a boundary (`R/cpm_oop.R` summary computes them identically); marked-vs-unmarked coverage gap from the per-marker table |
| 35 | derived | not-interpretable list: missing or zero-width intervals (row 15); chi-square at field N from the shipped caution above and `cairn/DESIGN.md`'s KS record |
| 36 | exempt | cross-reference |

Two claims were repaired during the walk rather than ledgered as written.
"Every bootstrap resample landed on the same boundary" was an inference the
output does not license — two percentile endpoints entail only the middle 95% —
and now says that. And the zeta paragraph originally read NO's zero-width
interval as an instance of the analytic clamping mechanism; it is a percentile
interval, a different route to the same absence, and the text now says so.
Ladder item 2 was also rewritten: it had the constraint direction backwards,
advising a comparison against *more* constrained variants while describing a
constraint being relaxed.

## Review
